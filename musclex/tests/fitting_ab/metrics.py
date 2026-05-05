"""
Metrics that quantify how a candidate adapter compares to the reference.

We deliberately keep this small and explicit; the runner calls the public
helpers and lets pandas do the heavy lifting downstream.
"""

from __future__ import annotations

from typing import Dict, Iterable, Iterator, List, Mapping, Optional

import numpy as np

from .adapters.base import FitCase, FitResult


# --------------------------------------------------------------------------- #
# Canonicalization
# --------------------------------------------------------------------------- #
#
# Several MuscleX models are *symmetric* under particular parameter
# permutations: swapping two components leaves the predicted curve (and
# therefore chi^2) unchanged but moves the parameter values. The two
# known degeneracies are:
#
# 1. PT meridian background and meridian peak share the same center; the
#    pair ``(center_sigma1, center_amplitude1)`` and ``(center_sigma2,
#    center_amplitude2)`` are interchangeable. Convention is
#    ``sigma1 >= sigma2`` (background broader). ``ProjectionProcessor``
#    canonicalizes after every fit (see comment near line 783), but that
#    only normalizes the *primary* result; recorded reference values from
#    earlier captures may not be canonicalized, and other adapters might
#    settle on the swapped optimum.
#
# 2. The ``p_i`` peak positions in ``layerlineModel(GMM)`` are addressed
#    by lmfit kwargs ``p_0``, ``p_1``, ... but the model only sums them.
#    If two trials initialize the optimizer with peak ordering swapped,
#    the fitted ``p_0`` may correspond to two different physical peaks
#    across trials, polluting any per-parameter mean/std.
#
# Without canonicalization, naive ``param mean +/- std`` across trials
# would report bimodal distributions that look like an unstable adapter
# but are really just a labeling artifact. We sort symmetric blocks
# canonically here so the consistency table reflects *physical* stability.

def canonicalize_values(values: Mapping[str, float]) -> Dict[str, float]:
    """Return a copy of ``values`` with known symmetric blocks sorted.

    Currently handles two block types:
    1. ``center_sigma1 >= center_sigma2`` (PT meridian pair),
       swapping the matching ``center_amplitude1/2`` together.
    2. ``p_0 < p_1 < ... < p_n`` (PT free peak positions),
       swapping the matching ``sigma_i`` / ``amplitude_i`` /
       ``gamma_i`` to follow the new index order.

    Unknown / non-symmetric parameters pass through untouched. Safe to
    call on any value dict; missing keys just skip the relevant rule.
    """
    out: Dict[str, float] = dict(values)

    s1 = out.get("center_sigma1")
    s2 = out.get("center_sigma2")
    if s1 is not None and s2 is not None and float(s1) < float(s2):
        out["center_sigma1"], out["center_sigma2"] = out["center_sigma2"], out["center_sigma1"]
        a1 = out.get("center_amplitude1")
        a2 = out.get("center_amplitude2")
        if a1 is not None and a2 is not None:
            out["center_amplitude1"], out["center_amplitude2"] = a2, a1

    peak_indices: List[int] = []
    i = 0
    while f"p_{i}" in out:
        peak_indices.append(i)
        i += 1

    if len(peak_indices) >= 2:
        positions = [(out[f"p_{i}"], i) for i in peak_indices]
        sorted_positions = sorted(positions, key=lambda t: float(t[0]))
        if [orig_i for _, orig_i in sorted_positions] != peak_indices:
            companion_prefixes = ("p_", "amplitude", "sigma", "gamma")
            new_block: Dict[str, float] = {}
            for new_i, (_, old_i) in enumerate(sorted_positions):
                for prefix in companion_prefixes:
                    old_key = f"{prefix}{old_i}"
                    if old_key in out:
                        new_block[f"{prefix}{new_i}"] = out[old_key]
            for prefix in companion_prefixes:
                for old_i in peak_indices:
                    out.pop(f"{prefix}{old_i}", None)
            out.update(new_block)

    return out


# --------------------------------------------------------------------------- #
# Public API
# --------------------------------------------------------------------------- #


def per_param_diff(
    result: FitResult,
    case: FitCase,
    *,
    only_free: bool = True,
    canonicalize: bool = True,
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
    canonicalize
        If ``True`` (default) sort symmetric blocks (see
        :func:`canonicalize_values`) before diffing so that label swaps
        do not show up as parameter drift.
    """
    if case.reference is None:
        return {}
    ref = canonicalize_values(case.reference.values) if canonicalize else case.reference.values
    cand = canonicalize_values(result.values) if canonicalize else result.values

    if only_free:
        names: Iterable[str] = case.inputs.free_params.keys()
    else:
        names = sorted(set(ref) | set(cand))

    diffs: Dict[str, float] = {}
    for name in names:
        if name in ref and name in cand:
            diffs[name] = float(cand[name]) - float(ref[name])
    return diffs


def per_param_rows(
    result: FitResult,
    case: FitCase,
    *,
    only_free: bool = True,
    canonicalize: bool = True,
) -> Iterator[Dict[str, object]]:
    """Yield one dict per parameter, suitable for a long-format DataFrame.

    Used by the runner to build the consistency table: for every fit, we
    record each free parameter's reference value, candidate value, absolute
    diff, and relative diff. The runner then aggregates ``(adapter, case,
    param_name)`` groups across trials to produce per-parameter mean / std.
    """
    if case.reference is None:
        return
    ref = canonicalize_values(case.reference.values) if canonicalize else case.reference.values
    cand = canonicalize_values(result.values) if canonicalize else result.values

    if only_free:
        names: Iterable[str] = case.inputs.free_params.keys()
    else:
        names = sorted(set(ref) | set(cand))

    for name in names:
        if name not in ref or name not in cand:
            continue
        ref_v = float(ref[name])
        cand_v = float(cand[name])
        if abs(ref_v) > 1e-9:
            rel = (cand_v - ref_v) / ref_v
        else:
            rel = float("nan")
        yield {
            "param_name": name,
            "ref_value": ref_v,
            "cand_value": cand_v,
            "diff": cand_v - ref_v,
            "abs_diff": abs(cand_v - ref_v),
            "rel_diff": rel,
        }


def position_max_abs_diff(result: FitResult, case: FitCase) -> Optional[float]:
    """Maximum absolute deviation of any peak position ``p_i`` (pixels).

    Both reference and candidate are canonicalized first (see
    :func:`canonicalize_values`) so that two adapters which find the same
    set of peak positions but in a different label order do not falsely
    register a large diff.
    """
    if case.reference is None:
        return None
    ref = canonicalize_values(case.reference.values)
    cand = canonicalize_values(result.values)
    diffs = []
    for name in case.inputs.free_params:
        if not name.startswith("p_"):
            continue
        ref_v = ref.get(name)
        cand_v = cand.get(name)
        if ref_v is None or cand_v is None:
            continue
        diffs.append(abs(float(cand_v) - float(ref_v)))
    return max(diffs) if diffs else None


def relative_amplitude_diff(result: FitResult, case: FitCase) -> Optional[float]:
    """Maximum relative amplitude difference, ignoring near-zero references."""
    if case.reference is None:
        return None
    ref = canonicalize_values(case.reference.values)
    cand = canonicalize_values(result.values)
    rels = []
    for name in case.inputs.free_params:
        if not name.startswith("amplitude"):
            continue
        ref_v = ref.get(name)
        cand_v = cand.get(name)
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
