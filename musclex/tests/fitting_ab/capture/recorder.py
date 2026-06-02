"""
Recorder hooked into :func:`ProjectionProcessor.fitModel`.

Activation
----------
Set ``MUSCLEX_CAPTURE_FITS=1`` to enable. Optional env vars:

* ``MUSCLEX_CAPTURE_DIR`` -- where pickle files are written
  (default: ``<repo>/musclex/tests/fitting_ab/fit_cases``).
* ``MUSCLEX_CAPTURE_TAG`` -- a string folded into each ``case_id`` so
  successive captures don't clobber each other.

Behaviour when the env flag is off
----------------------------------
:func:`maybe_record_fit` returns immediately. The cost is one ``os.environ``
lookup, no allocation.
"""

from __future__ import annotations

import os
import platform
import sys
import threading
import time
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Mapping, Optional

import numpy as np

from ..adapters.base import (
    SCHEMA_VERSION,
    CaseMeta,
    FitCase,
    FitInputs,
    ParamSpec,
    ReferenceResult,
    save_case,
)


# --------------------------------------------------------------------------- #
# Default location
# --------------------------------------------------------------------------- #


DEFAULT_CAPTURE_DIR = Path(__file__).resolve().parent.parent / "fit_cases"


# --------------------------------------------------------------------------- #
# Env helpers
# --------------------------------------------------------------------------- #


def is_capture_enabled() -> bool:
    """Cheap env-flag check; safe to call from hot loops."""
    val = os.environ.get("MUSCLEX_CAPTURE_FITS", "").strip().lower()
    return val not in ("", "0", "false", "no", "off")


def _capture_dir() -> Path:
    raw = os.environ.get("MUSCLEX_CAPTURE_DIR")
    if raw:
        return Path(raw).expanduser().resolve()
    return DEFAULT_CAPTURE_DIR


def _capture_tag() -> str:
    return os.environ.get("MUSCLEX_CAPTURE_TAG", "default")


# --------------------------------------------------------------------------- #
# Per-process counter (so case_ids are stable & sortable)
# --------------------------------------------------------------------------- #


_counter_lock = threading.Lock()
_counter = 0


def _next_index() -> int:
    global _counter
    with _counter_lock:
        _counter += 1
        return _counter


# --------------------------------------------------------------------------- #
# Build FitCase from runtime objects
# --------------------------------------------------------------------------- #


def _build_inputs(
    *,
    x: np.ndarray,
    y: np.ndarray,
    params: Any,                  # lmfit.Parameters
    int_vars: Mapping[str, Any],
    model_kind: str,
    has_voigt: bool,
) -> FitInputs:
    free: Dict[str, ParamSpec] = {}
    fixed: Dict[str, float] = {}

    for name, p in params.items():
        if p.vary:
            free[name] = ParamSpec(
                init=float(p.value),
                min=(None if not np.isfinite(p.min) else float(p.min)),
                max=(None if not np.isfinite(p.max) else float(p.max)),
            )
        else:
            fixed[name] = float(p.value)

    independent_vars: Dict[str, Any] = {}
    for name, val in int_vars.items():
        if name == "x":
            continue
        independent_vars[name] = (
            np.asarray(val, dtype=np.float64).copy()
            if isinstance(val, np.ndarray)
            else val
        )

    return FitInputs(
        x=np.asarray(x, dtype=np.float64).copy(),
        y=np.asarray(y, dtype=np.float64).copy(),
        weights=None,
        model_kind=model_kind,
        has_voigt=bool(has_voigt),
        free_params=free,
        independent_vars=independent_vars,
        fixed_params=fixed,
    )


def _build_meta(
    *,
    case_id: str,
    box_name: str,
    box: Any,
    processor: Any,
    n_peaks: int,
    has_voigt: bool,
) -> CaseMeta:
    invocation = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "python_version": sys.version.split()[0],
        "platform": platform.platform(),
    }
    try:
        from musclex import __version__ as musclex_version
        invocation["musclex_version"] = musclex_version
    except Exception:  # pragma: no cover
        pass
    try:
        import lmfit
        invocation["lmfit_version"] = lmfit.__version__
    except Exception:  # pragma: no cover
        pass
    invocation["numpy_version"] = np.__version__

    tags = [
        f"n_peaks={n_peaks}",
        f"gmm={getattr(box, 'use_common_sigma', False)}",
        f"merid_bg={getattr(box, 'merid_bg', False)}",
        f"voigt={has_voigt}",
        f"bgsub={getattr(box, 'bgsub', None)}",
    ]
    if getattr(box, 'fixed_center', None):
        tags.append("has_fixed_center")
    if getattr(box, 'fixed_sigma', None):
        tags.append("has_fixed_sigma")
    if getattr(box, 'fixed_amplitude', None):
        tags.append("has_fixed_amp")
    if getattr(box, 'fixed_common_sigma', None) is not None:
        tags.append("has_fixed_common_sigma")

    source_image = None
    try:
        source_image = getattr(processor, "filename", None) or getattr(
            getattr(processor, "state", None), "image_name", None
        )
    except Exception:  # pragma: no cover
        source_image = None

    return CaseMeta(
        case_id=case_id,
        box_name=str(box_name),
        box_type=str(getattr(box, "type", "")),
        bgsub=int(getattr(box, "bgsub", 0)),
        use_common_sigma=bool(getattr(box, "use_common_sigma", False)),
        merid_bg=bool(getattr(box, "merid_bg", False)),
        peaks_seed=[float(p) for p in (getattr(box, "peaks", None) or [])],
        hull_range=(
            tuple(float(v) for v in box.hull_range)  # type: ignore[arg-type]
            if getattr(box, "hull_range", None) is not None
            else None
        ),
        peak_tolerance=float(getattr(box, "peak_tolerance", 2.0)),
        sigma_tolerance=float(getattr(box, "sigma_tolerance", 100.0)),
        source_image=str(source_image) if source_image else None,
        invocation=invocation,
        difficulty_tags=tags,
    )


def _build_reference(
    result: Any,
    free_param_names,
    elapsed_s: float,
    adapter_label: str = "lmfit-baseline-leastsq",
) -> ReferenceResult:
    values: Dict[str, float] = {}
    stderr: Dict[str, Optional[float]] = {}

    if hasattr(result, "values"):
        for k, v in result.values.items():
            try:
                values[k] = float(v)
            except (TypeError, ValueError):
                continue

    for name in free_param_names:
        p = result.params.get(name) if hasattr(result, "params") else None
        stderr[name] = (
            None if p is None or p.stderr is None else float(p.stderr)
        )

    # Recompute chi2 / r2 from actual residuals so we don't inherit lmfit's
    # "fit aborted" reporting quirk (where ``result.chisqr`` is sometimes
    # populated with a bogus near-zero number). Falls back to lmfit's
    # numbers if best_fit isn't available for some reason.
    chi2: Optional[float] = None
    r2: Optional[float] = None
    redchi: Optional[float] = None
    try:
        if hasattr(result, "best_fit") and hasattr(result, "data"):
            data = np.asarray(result.data, dtype=np.float64)
            best_fit = np.asarray(result.best_fit, dtype=np.float64)
            if np.all(np.isfinite(best_fit)):
                residuals = data - best_fit
                chi2 = float(np.sum(residuals ** 2))
                ss_tot = float(np.sum((data - np.mean(data)) ** 2))
                if ss_tot > 0:
                    r2 = 1.0 - chi2 / ss_tot
                ndata = data.size
                nvarys = int(getattr(result, "nvarys", len(free_param_names)))
                dof = ndata - nvarys
                if dof > 0:
                    redchi = chi2 / dof
    except Exception:  # pragma: no cover
        chi2 = r2 = redchi = None

    if chi2 is None:
        chi2 = float(result.chisqr) if getattr(result, "chisqr", None) is not None else None
        redchi = float(result.redchi) if getattr(result, "redchi", None) is not None else None

    return ReferenceResult(
        adapter=adapter_label,
        success=bool(getattr(result, "success", True)),
        values=values,
        stderr=stderr,
        chi2=chi2,
        redchi=redchi,
        r2=r2,
        n_eval=int(getattr(result, "nfev", 0) or 0) or None,
        elapsed_s=float(elapsed_s),
        message=str(getattr(result, "message", "")),
    )


# --------------------------------------------------------------------------- #
# Public hook
# --------------------------------------------------------------------------- #


def _build_equator_meta(
    *,
    case_id: str,
    image_name: str,
    int_vars: Mapping[str, Any],
    free_param_count: int,
    extra_tags: Optional[list] = None,
) -> CaseMeta:
    invocation = {
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "python_version": sys.version.split()[0],
        "platform": platform.platform(),
    }
    try:
        from musclex import __version__ as musclex_version
        invocation["musclex_version"] = musclex_version
    except Exception:  # pragma: no cover
        pass
    try:
        import lmfit
        invocation["lmfit_version"] = lmfit.__version__
    except Exception:  # pragma: no cover
        pass
    invocation["numpy_version"] = np.__version__

    model_kind_str = str(int_vars.get("model", "Gaussian"))
    is_skeletal = bool(int_vars.get("isSkeletal", False))
    is_extra_peak = bool(int_vars.get("isExtraPeak", False))

    tags = [
        f"model={model_kind_str}",
        f"isSkeletal={is_skeletal}",
        f"isExtraPeak={is_extra_peak}",
        f"n_free={free_param_count}",
    ]
    if extra_tags:
        tags.extend(extra_tags)

    return CaseMeta(
        case_id=case_id,
        box_name="equator",
        box_type="equator",
        bgsub=0,
        use_common_sigma=False,
        merid_bg=False,
        peaks_seed=[],
        hull_range=None,
        peak_tolerance=0.0,
        sigma_tolerance=0.0,
        source_image=image_name,
        invocation=invocation,
        difficulty_tags=tags,
    )


def maybe_record_equator_fit(
    *,
    image_name: str,
    x: np.ndarray,
    y: np.ndarray,
    params: Any,                          # lmfit.Parameters used in fit
    int_vars: Mapping[str, Any],
    result: Any,                          # lmfit ModelResult
    elapsed_s: float,
    extra_tags: Optional[list] = None,
) -> Optional[Path]:
    """Persist a single equator (cardiac) fit invocation to disk.

    Mirrors :func:`maybe_record_fit` but for ``EquatorImage.cardiacFit``.
    No-op when capture is disabled.
    """
    if not is_capture_enabled():
        return None

    try:
        has_voigt = str(int_vars.get("model", "")).lower() == "voigt"

        idx = _next_index()
        # short suffix from image basename for traceability
        from os.path import basename, splitext
        img_short = splitext(basename(image_name or "unknown"))[0][:20]
        case_id = (
            f"{_capture_tag()}_eq_{img_short}_"
            f"{idx:05d}_"
            f"{uuid.uuid4().hex[:6]}"
        )

        inputs = _build_inputs(
            x=x, y=y, params=params, int_vars=int_vars,
            model_kind="cardiac", has_voigt=has_voigt,
        )
        meta = _build_equator_meta(
            case_id=case_id,
            image_name=image_name,
            int_vars=int_vars,
            free_param_count=len(inputs.free_params),
            extra_tags=extra_tags,
        )
        reference = _build_reference(
            result, list(inputs.free_params.keys()), elapsed_s,
        )

        case = FitCase(
            schema_version=SCHEMA_VERSION,
            meta=meta,
            inputs=inputs,
            reference=reference,
            optional={},
        )

        out_dir = _capture_dir()
        out_path = out_dir / f"{case_id}.pkl"
        save_case(case, out_path)
        return out_path
    except Exception as exc:  # pragma: no cover
        try:
            sys.stderr.write(
                f"[fitting_ab] equator capture failed for image="
                f"{image_name!r}: {type(exc).__name__}: {exc}\n"
            )
        except Exception:
            pass
        return None


def maybe_record_fit(
    *,
    box_name: str,
    box: Any,
    processor: Any,
    x: np.ndarray,
    y: np.ndarray,
    params: Any,                          # lmfit.Parameters used in fit
    int_vars: Mapping[str, Any],
    use_gmm: bool,
    result: Any,                          # lmfit ModelResult
    elapsed_s: float,
) -> Optional[Path]:
    """Persist a single fit invocation to disk if capture is enabled.

    Returns the path of the written pickle (or ``None`` if disabled).

    Failures are swallowed (capture must never break a real run).
    """
    if not is_capture_enabled():
        return None

    try:
        # Detect Voigt usage from int_vars or params (gamma_i in kwargs)
        has_voigt = any(k.startswith("gamma") for k in params.keys())

        n_peaks = len(getattr(box, "peaks", []) or [])
        idx = _next_index()
        case_id = (
            f"{_capture_tag()}_{box_name}_"
            f"{idx:05d}_"
            f"{uuid.uuid4().hex[:6]}"
        )

        inputs = _build_inputs(
            x=x, y=y, params=params, int_vars=int_vars,
            model_kind=("gmm" if use_gmm else "standard"),
            has_voigt=has_voigt,
        )
        meta = _build_meta(
            case_id=case_id, box_name=box_name, box=box,
            processor=processor, n_peaks=n_peaks, has_voigt=has_voigt,
        )
        reference = _build_reference(
            result, list(inputs.free_params.keys()), elapsed_s,
        )

        case = FitCase(
            schema_version=SCHEMA_VERSION,
            meta=meta,
            inputs=inputs,
            reference=reference,
            optional={},
        )

        out_dir = _capture_dir()
        out_path = out_dir / f"{case_id}.pkl"
        save_case(case, out_path)
        return out_path
    except Exception as exc:  # pragma: no cover
        # Capture must be best-effort; never raise into the fit path.
        try:
            sys.stderr.write(
                f"[fitting_ab] capture failed for box={box_name!r}: "
                f"{type(exc).__name__}: {exc}\n"
            )
        except Exception:
            pass
        return None
