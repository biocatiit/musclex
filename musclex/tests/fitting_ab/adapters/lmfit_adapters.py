"""
lmfit-based adapters.

* :class:`LmfitBaselineAdapter` reproduces the *current* call shape exactly
  (``Model.fit`` with default ``leastsq`` and no weights). Replaying every
  captured case through it should yield results numerically identical to the
  reference, which gives us the framework self-test.

* :class:`LmfitTRFAdapter` switches to ``method='least_squares'`` (Trust
  Region Reflective). This handles bounds natively (no internal tan/arctan
  remapping) and tends to be both more accurate at the boundaries and a
  little faster.

* :class:`LmfitPoissonWeightedAdapter` keeps ``leastsq`` but adds
  ``weights = 1 / sqrt(max(y, 1))`` to approximate Poisson noise (the
  natural model for photon counting histograms).
"""

from __future__ import annotations

import time
from typing import Dict, Optional, Tuple

import numpy as np
from lmfit import Model, Parameters

try:
    from sklearn.metrics import r2_score
except ImportError:  # pragma: no cover - sklearn is a hard musclex dep

    def r2_score(y_true, y_pred):  # type: ignore[no-redef]
        ss_res = float(np.sum((np.asarray(y_true) - np.asarray(y_pred)) ** 2))
        ss_tot = float(np.sum((np.asarray(y_true) - np.mean(y_true)) ** 2))
        return 1.0 - ss_res / ss_tot if ss_tot > 0 else 0.0


from .base import FitCase, FitInputs, FitResult, FitterAdapter, ParamSpec

# --------------------------------------------------------------------------- #
# Model functions
# --------------------------------------------------------------------------- #
#
# We import the *exact* model functions used by ProjectionProcessor so the
# baseline adapter is bitwise-equivalent to production code.


def _get_model_func(model_kind: str):
    # Imported lazily to avoid pulling in fabio/sklearn at framework import time.
    if model_kind in ("gmm", "standard"):
        try:
            from musclex.modules.ProjectionProcessor import (
                layerlineModel,
                layerlineModelGMM,
            )
        except ImportError:  # for the dev coverage path
            from modules.ProjectionProcessor import (  # type: ignore[no-redef]
                layerlineModel,
                layerlineModelGMM,
            )
        return layerlineModelGMM if model_kind == "gmm" else layerlineModel

    if model_kind == "cardiac":
        try:
            from musclex.modules.EquatorImage import cardiacFit
        except ImportError:  # for the dev coverage path
            from modules.EquatorImage import cardiacFit  # type: ignore[no-redef]
        return cardiacFit

    raise ValueError(f"Unknown model_kind: {model_kind!r}")


# --------------------------------------------------------------------------- #
# Helpers shared across lmfit adapters
# --------------------------------------------------------------------------- #


def _maybe_perturb_init(
    spec: ParamSpec,
    perturb: Optional[float],
    rng: np.random.Generator,
) -> float:
    """Bound-aware perturbation of a free parameter's initial value.

    Two regimes:

    * **Bounded** params (both ``min`` and ``max`` finite, with positive
      width): perturb additively by ``perturb * (max - min) / 2``.
      This treats ``perturb`` as "fraction of half the search range" and
      avoids the multiplicative-overshoot trap where, e.g., a tightly
      bounded position parameter (init=546, bounds=[543, 547]) would be
      thrown to 626 by a ``init * (1 ± 0.15)`` perturbation, then
      clipped to a boundary, putting the optimizer in an active-constraint
      degenerate state from step 0.

    * **Unbounded / one-sided** params: fall back to multiplicative
      ``init * (1 ± perturb)`` which is the natural interpretation for
      amplitude/scale-like quantities. Initial value of zero is handled
      with an additive ``± perturb`` to avoid a no-op.
    """
    if perturb is None or perturb <= 0:
        return float(spec.init)

    init = float(spec.init)
    has_min = spec.min is not None and np.isfinite(float(spec.min))
    has_max = spec.max is not None and np.isfinite(float(spec.max))

    if has_min and has_max:
        lo = float(spec.min)
        hi = float(spec.max)
        if hi > lo:
            half_width = 0.5 * (hi - lo)
            return init + rng.uniform(-perturb, +perturb) * half_width

    if abs(init) > 1e-12:
        factor = 1.0 + rng.uniform(-perturb, +perturb)
        return init * factor
    return init + rng.uniform(-perturb, +perturb)


def _build_lmfit_parameters(
    inputs: FitInputs,
    perturb: Optional[float],
    rng: np.random.Generator,
) -> Parameters:
    """Construct an lmfit ``Parameters`` set from a :class:`FitInputs`."""
    params = Parameters()

    # Free params
    for name, spec in inputs.free_params.items():
        init = _maybe_perturb_init(spec, perturb, rng)
        # Clamp perturbed init back into bounds, otherwise lmfit raises.
        lo, hi = spec.to_lmfit_min(), spec.to_lmfit_max()
        if init < lo:
            init = lo
        if init > hi:
            init = hi
        params.add(name, value=init, min=lo, max=hi)

    # vary=False params (user-locked)
    for name, val in inputs.fixed_params.items():
        params.add(name, value=float(val), vary=False)

    return params


def _independent_kwargs(inputs: FitInputs) -> Dict[str, object]:
    """Return the kwargs that are passed straight to the model func."""
    kwargs = dict(inputs.independent_vars)
    kwargs["x"] = inputs.x
    return kwargs


def _independent_var_names(inputs: FitInputs) -> Tuple[str, ...]:
    """Names that should be declared as ``independent_vars`` to lmfit."""
    names = set(inputs.independent_vars.keys())
    names.add("x")
    return tuple(names)


def _compute_r2(y, predicted) -> Optional[float]:
    """True coefficient of determination (1.0 = perfect fit)."""
    try:
        return float(r2_score(y, predicted))
    except Exception:  # pragma: no cover
        return None


_ABORTED_HINTS = ("aborted", "exceeded", "maxfev", "max_nfev", "max iter")


def _is_aborted(result) -> bool:
    """Detect "ran out of budget" rather than "converged" outcomes.

    lmfit reports ``success=False`` for several distinct conditions and
    occasionally fills in bogus ``chisqr`` values when the underlying
    optimizer was terminated by an evaluation cap. We sniff the message
    plus the success flag to identify these.
    """
    msg = str(getattr(result, "message", "") or "").lower()
    if any(hint in msg for hint in _ABORTED_HINTS):
        return True
    return bool(not getattr(result, "success", True))


def _manual_chi2(y: np.ndarray, predicted: np.ndarray, weights) -> Optional[float]:
    """Sum of squared (optionally weighted) residuals.

    Always recomputed from the final parameter values so it survives
    lmfit's "fit aborted" reporting bug, where ``result.chisqr`` is
    populated with bogus values like ``1e-250``.
    """
    if predicted is None:
        return None
    try:
        residuals = np.asarray(y, dtype=np.float64) - np.asarray(
            predicted, dtype=np.float64
        )
        if weights is not None:
            residuals = residuals * np.asarray(weights, dtype=np.float64)
        if not np.all(np.isfinite(residuals)):
            return None
        return float(np.sum(residuals**2))
    except Exception:  # pragma: no cover
        return None


# --------------------------------------------------------------------------- #
# Adapters
# --------------------------------------------------------------------------- #


class _LmfitAdapterBase(FitterAdapter):
    """Shared scaffolding for all lmfit-based adapters."""

    #: lmfit method name passed via ``Model.fit(method=...)``.
    fit_method: str = "leastsq"
    #: Whether to apply Poisson sqrt weights ``1/sqrt(max(y, 1))``.
    use_poisson_weights: bool = False
    #: Extra kwargs forwarded to the underlying minimizer (e.g.
    #: ``{"x_scale": "jac"}`` for scipy ``least_squares``). Empty dict
    #: means "use lmfit/scipy defaults".
    fit_kws: Dict[str, object] = {}

    def __init__(self, *, seed: int = 0):
        self._rng = np.random.default_rng(seed)

    def _resolve_model_func(self, model_kind: str):
        """Return the model callable to wrap in ``lmfit.Model``.

        Override in subclasses to substitute a different implementation of
        the inner Gaussian / Voigt evaluation (e.g. numba, cython).  The
        default calls the module-level :func:`_get_model_func` which imports
        the production function from ``ProjectionProcessor``.
        """
        return _get_model_func(model_kind)

    def fit(self, case: FitCase, *, perturb_init: Optional[float] = None) -> FitResult:
        inputs = case.inputs

        params = _build_lmfit_parameters(inputs, perturb_init, self._rng)
        indep_kwargs = _independent_kwargs(inputs)
        indep_names = _independent_var_names(inputs)

        model_func = self._resolve_model_func(inputs.model_kind)
        model = Model(
            model_func,
            nan_policy="propagate",
            independent_vars=indep_names,
        )

        weights = None
        if self.use_poisson_weights:
            weights = 1.0 / np.sqrt(np.maximum(inputs.y, 1.0))
        elif inputs.weights is not None:
            weights = np.asarray(inputs.weights, dtype=np.float64)

        t0 = time.perf_counter()
        try:
            extra_fit_kws = dict(self.fit_kws) if self.fit_kws else None
            result = model.fit(
                inputs.y,
                params=params,
                method=self.fit_method,
                weights=weights,
                verbose=False,
                fit_kws=extra_fit_kws,
                **indep_kwargs,
            )
            message = getattr(result, "message", "")
        except Exception as exc:  # noqa: BLE001
            elapsed = time.perf_counter() - t0
            return FitResult(
                success=False,
                values={},
                stderr={},
                n_eval=None,
                n_iter=None,
                elapsed_s=elapsed,
                converged=False,
                chi2=None,
                redchi=None,
                r2=None,
                message=f"{type(exc).__name__}: {exc}",
                aborted=False,
                raw=None,
            )
        elapsed = time.perf_counter() - t0

        # Collect outputs
        values = dict(result.values)
        stderr: Dict[str, Optional[float]] = {}
        for name in inputs.free_params:
            p = result.params.get(name)
            stderr[name] = None if p is None or p.stderr is None else float(p.stderr)

        # Recompute chi2 / r2 from the final parameter values so we don't
        # inherit lmfit's reporting quirks (notably the ``chisqr ~ 1e-250``
        # bug seen on aborted least_squares runs).
        predicted = None
        chi2: Optional[float] = None
        r2: Optional[float] = None
        try:
            predicted = model_func(**{**indep_kwargs, **values})
            r2 = _compute_r2(inputs.y, predicted)
            chi2 = _manual_chi2(inputs.y, predicted, weights)
        except Exception:  # pragma: no cover
            pass

        # Reduced chi2: chi2 / (ndata - nvarys), guard against zero/neg dof.
        redchi: Optional[float] = None
        try:
            ndata = int(np.asarray(inputs.y).size)
            nvarys = int(getattr(result, "nvarys", len(inputs.free_params)))
            dof = ndata - nvarys
            if chi2 is not None and dof > 0:
                redchi = float(chi2) / float(dof)
        except Exception:  # pragma: no cover
            pass

        aborted = _is_aborted(result)
        converged = bool(getattr(result, "success", True)) and not aborted

        return FitResult(
            success=converged,
            values=values,
            stderr=stderr,
            n_eval=int(getattr(result, "nfev", 0) or 0) or None,
            n_iter=None,  # lmfit doesn't expose iter count uniformly
            elapsed_s=elapsed,
            converged=converged,
            chi2=chi2,
            redchi=redchi,
            r2=r2,
            message=str(message or ""),
            aborted=aborted,
            raw=result,
        )


class LmfitBaselineAdapter(_LmfitAdapterBase):
    """Faithful reproduction of the current production fit call."""

    name = "lmfit-baseline-leastsq"
    fit_method = "leastsq"
    use_poisson_weights = False


class LmfitTRFAdapter(_LmfitAdapterBase):
    """lmfit + ``method='least_squares'`` (scipy TRF, native bounds)."""

    name = "lmfit-trf"
    fit_method = "least_squares"
    use_poisson_weights = False


class LmfitTRFJacAdapter(_LmfitAdapterBase):
    """TRF with Jacobian-based parameter scaling ``x_scale='jac'``.

    Recommended by scipy docs for ill-scaled problems where free
    parameters span many orders of magnitude (e.g. EquatorImage where
    ``area`` parameters are ~1e6 while ``sigma`` parameters are ~10).
    Without this, TRF's relative-step ``xtol`` convergence check trips
    almost immediately and the optimizer stops before the large-scale
    parameters have moved meaningfully.
    """

    name = "lmfit-trf-jac"
    fit_method = "least_squares"
    use_poisson_weights = False
    fit_kws = {"x_scale": "jac"}


class LmfitPoissonWeightedAdapter(_LmfitAdapterBase):
    """leastsq + Poisson sqrt weights ``1/sqrt(max(y, 1))``."""

    name = "lmfit-poisson"
    fit_method = "leastsq"
    use_poisson_weights = True


class LmfitTRFPoissonAdapter(_LmfitAdapterBase):
    """TRF + Poisson sqrt weights (combine both improvements)."""

    name = "lmfit-trf-poisson"
    fit_method = "least_squares"
    use_poisson_weights = True


# --------------------------------------------------------------------------- #
# Model-variant adapters  (speedup comparison: lmfit-objects vs numpy vs
# numba vs cython — all using TRF so the optimizer is a constant)
# --------------------------------------------------------------------------- #


def _make_variant_resolver(import_path: str):
    """Factory: return a ``_resolve_model_func`` that imports from *import_path*."""

    def _resolve(self, model_kind: str):  # noqa: ANN001
        import importlib  # noqa: PLC0415

        mod = importlib.import_module(import_path)
        if model_kind == "gmm":
            return mod.layerlineModelGMM
        if model_kind == "standard":
            return mod.layerlineModel
        # Non-PT model kinds (e.g. "cardiac") fall back to the production import.
        return _get_model_func(model_kind)

    return _resolve


class LmfitTRFLmfitObjectsAdapter(_LmfitAdapterBase):
    """TRF + baseline *before* speedup: new lmfit model object on every eval.

    This is the "old production code" reference point.  Use it to measure
    the overhead of ``GaussianModel().eval()`` / ``VoigtModel().eval()``
    object construction per optimizer call.
    """

    name = "lmfit-trf-lmfit-objects"
    fit_method = "least_squares"

    _resolve_model_func = _make_variant_resolver(
        "musclex.tests.fitting_ab.model_variants.model_lmfit_objects"
    )


class LmfitTRFNumpyAdapter(_LmfitAdapterBase):
    """TRF + direct NumPy Gaussian / wofz Voigt (no lmfit model objects).

    The proposed speedup: identical maths, no Python object construction
    overhead per residual evaluation.
    """

    name = "lmfit-trf-numpy"
    fit_method = "least_squares"

    _resolve_model_func = _make_variant_resolver(
        "musclex.tests.fitting_ab.model_variants.model_numpy"
    )


class LmfitTRFNumbaAdapter(_LmfitAdapterBase):
    """TRF + Numba-JIT Gaussian (Voigt falls back to NumPy/wofz).

    ``numba`` must be installed.  JIT warm-up happens at import time of
    ``model_variants.model_numba`` so the first fit does not bear JIT cost.
    If ``numba`` is absent the module silently falls back to NumPy and logs
    a warning.
    """

    name = "lmfit-trf-numba"
    fit_method = "least_squares"

    _resolve_model_func = _make_variant_resolver(
        "musclex.tests.fitting_ab.model_variants.model_numba"
    )


class LmfitTRFCythonAdapter(_LmfitAdapterBase):
    """TRF + Cython-compiled Gaussian (Voigt falls back to NumPy/wofz).

    The Cython extension must be compiled before use::

        cd musclex/tests/fitting_ab/model_variants/model_cython
        python setup.py build_ext --inplace

    If the extension is absent a descriptive ``ImportError`` is raised when
    the first ``fit()`` call is made.
    """

    name = "lmfit-trf-cython"
    fit_method = "least_squares"

    _resolve_model_func = _make_variant_resolver(
        "musclex.tests.fitting_ab.model_variants.model_cython.model_cython"
    )


# --------------------------------------------------------------------------- #
# Hull-range slicing adapter
# --------------------------------------------------------------------------- #

#: Extra pixels kept on each side of the hull window to avoid clipping peak
#: tails.  Small enough that it doesn't reduce the speed gain meaningfully.
_HULL_SLICE_MARGIN = 15


def _slice_to_hull_range(inputs: FitInputs, hull_range) -> FitInputs:
    """Return a copy of *inputs* trimmed to the outer hull-range window.

    Slices the array to ``[centerX - hull_end - margin, centerX + hull_end +
    margin]``, removing the outer zero-padded region.  The inner zero-padded
    region (from center to hull_start) is **retained** — see
    :func:`_double_slice_to_hull_range` for a more aggressive variant that
    removes those zeros as well.

    ``centerX`` is kept as-is; the ``x`` values retain their original absolute
    pixel coordinates.
    """
    hull_start, hull_end = float(hull_range[0]), float(hull_range[1])
    centerX = float(inputs.independent_vars.get("centerX", 0.0))

    n = len(inputs.x)
    lo = max(0, int(centerX - hull_end) - _HULL_SLICE_MARGIN)
    hi = min(n, int(centerX + hull_end) + _HULL_SLICE_MARGIN + 1)

    if lo == 0 and hi == n:
        return inputs  # nothing to trim

    x_sl = inputs.x[lo:hi]
    y_sl = inputs.y[lo:hi]
    w_sl = inputs.weights[lo:hi] if inputs.weights is not None else None

    from dataclasses import replace  # noqa: PLC0415

    return replace(inputs, x=x_sl, y=y_sl, weights=w_sl)


def _double_slice_to_hull_range(inputs: FitInputs, hull_range) -> FitInputs:
    """Return a copy of *inputs* keeping ONLY the two active signal windows.

    When ``bgsub == 1`` the histogram has non-zero values only in the two
    rings ``[centerX ± hull_start, centerX ± hull_end]``.  The region
    between ``-hull_start`` and ``+hull_start`` (near the beam centre) is
    also zero-padded, not just the outer tail.

    This function concatenates the two narrow windows:

        left window:  ``x[ centerX - hull_end - M : centerX - hull_start + M ]``
        right window: ``x[ centerX + hull_start - M : centerX + hull_end + M ]``

    The concatenated ``x`` array retains absolute pixel coordinates so
    ``centerX`` and peak positions in the model function require no
    adjustment.  lmfit treats the combined array as a flat list of
    (x_i, y_i) pairs, which is mathematically valid.

    For typical real cases where ``hull_start ≈ hull_end * 0.9`` this
    reduces the effective array length from ``~2 * hull_end`` (single-slice)
    to ``~2 * (hull_end - hull_start + 2 * M)``, yielding an additional
    **5–20×** reduction in ``exp()`` evaluations.
    """
    hull_start, hull_end = float(hull_range[0]), float(hull_range[1])
    centerX = float(inputs.independent_vars.get("centerX", 0.0))
    n = len(inputs.x)
    M = _HULL_SLICE_MARGIN

    # Left window: from (centerX - hull_end) to (centerX - hull_start)
    l_lo = max(0, int(centerX - hull_end) - M)
    l_hi = min(n, int(centerX - hull_start) + M + 1)

    # Right window: from (centerX + hull_start) to (centerX + hull_end)
    r_lo = max(0, int(centerX + hull_start) - M)
    r_hi = min(n, int(centerX + hull_end) + M + 1)

    # Fall back to single-slice if windows overlap or hull_start ~ 0
    if l_hi >= r_lo or l_lo >= l_hi or r_lo >= r_hi:
        return _slice_to_hull_range(inputs, hull_range)

    x_cat = np.concatenate([inputs.x[l_lo:l_hi], inputs.x[r_lo:r_hi]])
    y_cat = np.concatenate([inputs.y[l_lo:l_hi], inputs.y[r_lo:r_hi]])
    w_cat = None
    if inputs.weights is not None:
        w_cat = np.concatenate(
            [
                inputs.weights[l_lo:l_hi],
                inputs.weights[r_lo:r_hi],
            ]
        )

    from dataclasses import replace  # noqa: PLC0415

    return replace(inputs, x=x_cat, y=y_cat, weights=w_cat)


def _recompute_metrics_on_full_array(
    result: FitResult,
    original_inputs: FitInputs,
) -> FitResult:
    """Recompute chi² and R² of *result* against the **full** original array.

    When a slice adapter fits on a reduced array, the reported chi² / R² are
    computed on that smaller array.  Zero-padded regions (removed by slicing)
    are trivially fitted — the model evaluates to ~0 there — so excluding them
    makes R² look *lower* than the baseline even when the fitted parameters are
    identical.

    This helper evaluates the model on the full ``original_inputs.x`` /
    ``original_inputs.y`` and recomputes the metrics so results are
    comparable across adapters.
    """
    if not result.values:
        return result  # nothing to recompute (failed fit)

    model_func = _get_model_func(original_inputs.model_kind)
    indep_kwargs = dict(original_inputs.independent_vars)
    indep_kwargs["x"] = original_inputs.x

    try:
        predicted_full = model_func(**{**indep_kwargs, **result.values})
        r2_full = _compute_r2(original_inputs.y, predicted_full)
        chi2_full = _manual_chi2(
            original_inputs.y, predicted_full, original_inputs.weights
        )
        redchi_full: Optional[float] = None
        try:
            ndata = int(np.asarray(original_inputs.y).size)
            nvarys = len(original_inputs.free_params)
            dof = ndata - nvarys
            if chi2_full is not None and dof > 0:
                redchi_full = float(chi2_full) / float(dof)
        except Exception:  # pragma: no cover
            pass
    except Exception:  # pragma: no cover
        return result

    from dataclasses import replace as _replace  # noqa: PLC0415

    return _replace(result, r2=r2_full, chi2=chi2_full, redchi=redchi_full)


class LmfitTRFHullDoubleSliceAdapter(_LmfitAdapterBase):
    """TRF + two-window slicing that removes BOTH the outer AND inner zeros.

    For ``bgsub == 1`` cases, non-zero signal exists only in the two rings
    ``[centerX ± hull_start, centerX ± hull_end]``.  This adapter passes
    only those two windows to the optimizer, concatenated into a single flat
    array with the original absolute ``x`` coordinates preserved.

    Compared with :class:`LmfitTRFHullSliceAdapter` (single outer-cut), this
    removes the large zero-padded meridian region and gives a stronger
    speedup when ``hull_start`` is large relative to ``hull_end``.  In the
    captured real cases ``hull_start / hull_end ≈ 0.85–0.96``, so the inner
    zeros account for 85–96 % of the sliced window — double slicing can
    reduce the effective array by 5–20× vs single slicing.

    R² and chi² are recomputed on the **full original array** after fitting
    so metrics are comparable with the baseline TRF adapter.
    """

    name = "lmfit-trf-hull-double-slice"
    fit_method = "least_squares"

    def fit(self, case: FitCase, *, perturb_init: Optional[float] = None) -> FitResult:
        meta = case.meta
        if (
            getattr(meta, "bgsub", -1) == 1
            and getattr(meta, "hull_range", None) is not None
        ):
            sliced_inputs = _double_slice_to_hull_range(case.inputs, meta.hull_range)
            from dataclasses import replace as _replace  # noqa: PLC0415

            sliced_case = _replace(case, inputs=sliced_inputs)
            result = super().fit(sliced_case, perturb_init=perturb_init)
            return _recompute_metrics_on_full_array(result, case.inputs)
        return super().fit(case, perturb_init=perturb_init)


class LmfitTRFHullSliceAdapter(_LmfitAdapterBase):
    """TRF + hull-range array slicing for bgsub=1 (convex hull) cases.

    When the case metadata indicates ``bgsub == 1`` and a ``hull_range`` is
    recorded, the histogram and x-array are trimmed to the active signal
    window before fitting.  The zero-padded exterior (produced by
    :func:`convexHull`) contributes almost nothing to the loss function but
    forces :func:`_gaussian_eval` to evaluate ``np.exp()`` for every wasted
    pixel on every optimizer iteration.

    For ``bgsub != 1`` or when ``hull_range`` is absent the adapter falls
    back to standard full-array fitting.

    Expected speedup: **2–4×** relative to unsliced fitting (in addition
    to the baseline lmfit-objects → numpy gain), depending on the ratio
    ``hull_window / array_length``.
    """

    name = "lmfit-trf-hull-slice"
    fit_method = "least_squares"

    def fit(self, case: FitCase, *, perturb_init: Optional[float] = None) -> FitResult:
        meta = case.meta
        if (
            getattr(meta, "bgsub", -1) == 1
            and getattr(meta, "hull_range", None) is not None
        ):
            sliced_inputs = _slice_to_hull_range(case.inputs, meta.hull_range)
            from dataclasses import replace as _replace  # noqa: PLC0415

            sliced_case = _replace(case, inputs=sliced_inputs)
            result = super().fit(sliced_case, perturb_init=perturb_init)
            return _recompute_metrics_on_full_array(result, case.inputs)
        return super().fit(case, perturb_init=perturb_init)


# --------------------------------------------------------------------------- #
# Vectorised batch adapters (L3 / L4)
# --------------------------------------------------------------------------- #


class LmfitTRFNumpyVectorizedAdapter(_LmfitAdapterBase):
    """TRF + NumPy batch vectorisation (L3, control group).

    All K Gaussian peaks are accumulated in a single ``np.exp()`` call on a
    ``(K, N)`` matrix, replacing the per-peak loop from :class:`LmfitTRFNumpyAdapter`.
    No JIT — pure NumPy.  Acts as the control group to isolate the Numba
    overhead from the vectorisation gain.
    """

    name = "lmfit-trf-numpy-vectorized"
    fit_method = "least_squares"

    _resolve_model_func = _make_variant_resolver(
        "musclex.tests.fitting_ab.model_variants.model_numpy_vectorized"
    )


class LmfitTRFNumbaVectorizedAdapter(_LmfitAdapterBase):
    """TRF + Numba-JIT batch kernel (L4 — fastest single-fit variant).

    The residual function passes all K peak centers/amplitudes to a single
    ``@numba.njit(cache=True, fastmath=True)`` kernel that fuses the loop over
    peaks and the loop over array points into one pass, eliminating temporary
    ``(K, N)`` matrix allocations and crossing the Python/numba boundary once
    per residual evaluation instead of K times (as in :class:`LmfitTRFNumbaAdapter`).

    Warmup is performed at module import time of ``model_numba_vectorized``
    (``cache=True`` means disk-caching after the first run).

    ``numba`` is a hard dependency in the project's ``.deb`` packaging so no
    fallback logic is included.
    """

    name = "lmfit-trf-numba-vectorized"
    fit_method = "least_squares"

    _resolve_model_func = _make_variant_resolver(
        "musclex.tests.fitting_ab.model_variants.model_numba_vectorized"
    )


# --------------------------------------------------------------------------- #
# Analytical-Jacobian adapter (direct scipy, no lmfit wrapper overhead)
# --------------------------------------------------------------------------- #


class ScipyAnalyticJacAdapter(FitterAdapter):
    """Direct ``scipy.optimize.least_squares`` with an analytical Jacobian.

    This adapter bypasses lmfit entirely and calls scipy directly with an
    analytical Jacobian for all Gaussian peak parameters.  Compared with
    :class:`LmfitTRFNumpyAdapter` it removes two bottlenecks:

    1. **lmfit residual wrapper overhead** — ``__residual`` does
       ``from_internal`` bound remapping, ``update_constraints()``, and
       nan-policy checks on *every single function evaluation* (including each
       of the ``n_free`` finite-difference Jacobian columns).
    2. **Finite-difference Jacobian** — replaced by closed-form derivatives of
       the Gaussian model, which require O(n_peaks) array operations instead
       of O(n_free) complete model evaluations.

    Non-peak free parameters (background gaussians when bgsub==0) still use
    central finite differences so the adapter handles all model kinds.

    The adapter is primarily validated against :class:`LmfitTRFNumpyAdapter`
    on the captured real cases; see
    ``tests/fitting_ab/scripts/validate_analytic_jac.py``.
    """

    name = "scipy-analytic-jac"

    def __init__(self, *, seed: int = 0, max_nfev: Optional[int] = None):
        self._rng = np.random.default_rng(seed)
        self._max_nfev = max_nfev  # None → scipy default (100 × n_free)

    def fit(self, case: FitCase, *, perturb_init: Optional[float] = None) -> FitResult:
        from scipy.optimize import least_squares as _ls
        from musclex.tests.fitting_ab.model_variants.model_analytic_jac import (
            make_residual_and_jac,
        )

        inputs = case.inputs
        free_names = list(inputs.free_params.keys())
        fixed = dict(inputs.fixed_params)

        # Build x0, bounds
        x0 = np.array(
            [
                float(_maybe_perturb_init(spec, perturb_init, self._rng))
                for spec in inputs.free_params.values()
            ],
            dtype=np.float64,
        )
        lo = np.array([s.to_lmfit_min() for s in inputs.free_params.values()])
        hi = np.array([s.to_lmfit_max() for s in inputs.free_params.values()])

        # Clamp x0 into bounds
        x0 = np.clip(x0, lo, hi)

        residual_fn, jac_fn, _ = make_residual_and_jac(inputs, free_names, x0)

        t0 = time.perf_counter()
        try:
            result = _ls(
                residual_fn,
                x0,
                jac=jac_fn,
                bounds=(lo, hi),
                method="trf",
                max_nfev=self._max_nfev,
            )
        except Exception as exc:
            elapsed = time.perf_counter() - t0
            return FitResult(
                success=False,
                values={},
                stderr={},
                n_eval=None,
                n_iter=None,
                elapsed_s=elapsed,
                converged=False,
                chi2=None,
                redchi=None,
                r2=None,
                message=f"{type(exc).__name__}: {exc}",
            )
        elapsed = time.perf_counter() - t0

        # Map result vector back to named dict
        values: Dict[str, float] = dict(zip(free_names, result.x.tolist()))
        # Add fixed params so values is complete
        values.update(fixed)
        # Add independent vars that happen to be params (e.g. bg when bgsub==1)
        for k, v in inputs.independent_vars.items():
            if k != "x":
                values.setdefault(k, float(v))

        # Covariance → stderr (from Jacobian at solution, same as scipy convention)
        stderr: Dict[str, Optional[float]] = {n: None for n in free_names}
        try:
            # result.jac is the (m, n) Jacobian at the solution
            J = result.jac
            JtJ = J.T @ J
            cov = np.linalg.pinv(JtJ)
            # Scale by residual variance
            m, n = J.shape
            if m > n:
                s2 = np.sum(result.fun**2) / (m - n)
                cov = cov * s2
            for i, name in enumerate(free_names):
                v = float(cov[i, i])
                stderr[name] = float(np.sqrt(v)) if v > 0 else None
        except Exception:
            pass

        # Post-fit metrics
        from musclex.tests.fitting_ab.model_variants.model_numpy import (
            layerlineModelGMM,
            layerlineModel,
        )

        model_fn = layerlineModelGMM if inputs.model_kind == "gmm" else layerlineModel
        indep_kw = dict(inputs.independent_vars)
        indep_kw["x"] = inputs.x

        predicted = None
        chi2: Optional[float] = None
        r2: Optional[float] = None
        redchi: Optional[float] = None
        try:
            predicted = model_fn(**{**indep_kw, **values})
            r2 = _compute_r2(inputs.y, predicted)
            chi2 = _manual_chi2(inputs.y, predicted, inputs.weights)
            ndata = len(inputs.y)
            nvarys = len(free_names)
            dof = ndata - nvarys
            if chi2 is not None and dof > 0:
                redchi = chi2 / dof
        except Exception:
            pass

        # scipy.OptimizeResult.status codes:
        #   -1  improper inputs
        #    0  max nfev reached
        #    1–4 convergence criteria
        converged = bool(result.success) and result.status > 0
        aborted = result.status == 0

        return FitResult(
            success=converged,
            values=values,
            stderr=stderr,
            n_eval=int(result.nfev),
            n_iter=None,
            elapsed_s=elapsed,
            converged=converged,
            chi2=chi2,
            redchi=redchi,
            r2=r2,
            message=result.message,
            aborted=aborted,
            raw=result,
        )
