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
        residuals = np.asarray(y, dtype=np.float64) - np.asarray(predicted, dtype=np.float64)
        if weights is not None:
            residuals = residuals * np.asarray(weights, dtype=np.float64)
        if not np.all(np.isfinite(residuals)):
            return None
        return float(np.sum(residuals ** 2))
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

    def fit(self, case: FitCase, *, perturb_init: Optional[float] = None) -> FitResult:
        inputs = case.inputs

        params = _build_lmfit_parameters(inputs, perturb_init, self._rng)
        indep_kwargs = _independent_kwargs(inputs)
        indep_names = _independent_var_names(inputs)

        model_func = _get_model_func(inputs.model_kind)
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
            stderr[name] = (None if p is None or p.stderr is None else float(p.stderr))

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
