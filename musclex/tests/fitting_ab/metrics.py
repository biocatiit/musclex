"""
Metrics that quantify how a candidate adapter compares to the reference.

We deliberately keep this small and explicit; the runner calls the public
helpers and lets pandas do the heavy lifting downstream.
"""

from __future__ import annotations

from typing import Dict, Iterable, Optional

import numpy as np

from .adapters.base import FitCase, FitResult


# --------------------------------------------------------------------------- #
# Public API
# --------------------------------------------------------------------------- #


def per_param_diff(
    result: FitResult,
    case: FitCase,
    *,
    only_free: bool = True,
) -> Dict[str, float]:
    """Return a dict ``{param_name: candidate - reference}``.

    Parameters
    ----------
    result
        Adapter output.
    case
        The replayed fixture (must have ``case.reference`` set).
    only_free
        If ``True`` (default) only compare free parameters. Fixed parameters
        always match by definition.
    """
    if case.reference is None:
        return {}
    ref = case.reference.values
    cand = result.values

    if only_free:
        names: Iterable[str] = case.inputs.free_params.keys()
    else:
        names = sorted(set(ref) | set(cand))

    diffs: Dict[str, float] = {}
    for name in names:
        if name in ref and name in cand:
            diffs[name] = float(cand[name]) - float(ref[name])
    return diffs


def position_max_abs_diff(result: FitResult, case: FitCase) -> Optional[float]:
    """Maximum absolute deviation of any peak position ``p_i`` (pixels)."""
    if case.reference is None:
        return None
    diffs = []
    for name in case.inputs.free_params:
        if not name.startswith("p_"):
            continue
        ref_v = case.reference.values.get(name)
        cand_v = result.values.get(name)
        if ref_v is None or cand_v is None:
            continue
        diffs.append(abs(float(cand_v) - float(ref_v)))
    return max(diffs) if diffs else None


def relative_amplitude_diff(result: FitResult, case: FitCase) -> Optional[float]:
    """Maximum relative amplitude difference, ignoring near-zero references."""
    if case.reference is None:
        return None
    rels = []
    for name in case.inputs.free_params:
        if not name.startswith("amplitude"):
            continue
        ref_v = case.reference.values.get(name)
        cand_v = result.values.get(name)
        if ref_v is None or cand_v is None:
            continue
        if abs(float(ref_v)) < 1e-9:
            continue  # avoid divide-by-zero / meaningless ratio
        rels.append(abs((float(cand_v) - float(ref_v)) / float(ref_v)))
    return max(rels) if rels else None


def chi2_ratio(result: FitResult, case: FitCase) -> Optional[float]:
    """``chi2_candidate / chi2_reference``.

    < 1.0 means candidate found a better-fitting (lower chi2) optimum.
    """
    if case.reference is None or case.reference.chi2 is None or result.chi2 is None:
        return None
    if case.reference.chi2 <= 0:
        return None
    return float(result.chi2) / float(case.reference.chi2)


def speed_ratio(result: FitResult, case: FitCase) -> Optional[float]:
    """``elapsed_candidate / elapsed_reference``.

    < 1.0 means candidate is faster.
    Note: reference time was captured during a *production run* and includes
    its own warmup overhead; for fair comparison use the runner's repeated
    measurements rather than a single ratio.
    """
    if case.reference is None or case.reference.elapsed_s is None:
        return None
    if case.reference.elapsed_s <= 0:
        return None
    return result.elapsed_s / float(case.reference.elapsed_s)


def summarize(result: FitResult, case: FitCase) -> Dict[str, Optional[float]]:
    """One-row summary used by the runner to populate the result DataFrame."""
    return {
        "success": bool(result.success),
        "converged": bool(result.converged),
        "aborted": bool(result.aborted),
        "elapsed_s": float(result.elapsed_s),
        "n_eval": result.n_eval,
        "chi2": result.chi2,
        "redchi": result.redchi,
        "r2": result.r2,
        "ref_chi2": (case.reference.chi2 if case.reference else None),
        "ref_r2": (case.reference.r2 if case.reference else None),
        "ref_elapsed_s": (case.reference.elapsed_s if case.reference else None),
        "chi2_ratio": chi2_ratio(result, case),
        "speed_ratio": speed_ratio(result, case),
        "p_max_abs_diff": position_max_abs_diff(result, case),
        "amp_max_rel_diff": relative_amplitude_diff(result, case),
    }
