"""
Diagnose why ``LmfitTRFAdapter`` fails on the m6 case under init perturbation.

Strategy:

* Load the captured m6 case.
* For ``n_seeds`` independent perturbations of size ``perturb``, run TRF.
* Print summary lines for every fit, full diagnostic dump for any failure
  (success=False or aborted at the lmfit nfev cap).

Run with::

    python -m musclex.tests.fitting_ab.diagnose_m6_trf_failure
"""

from __future__ import annotations

import argparse
import sys
import time
from dataclasses import asdict
from pathlib import Path
from typing import List

import numpy as np
from lmfit import Model, Parameters

from musclex.modules.ProjectionProcessor import (
    layerlineModel,
    layerlineModelGMM,
)
from .adapters.base import FitCase, FitInputs, ParamSpec, load_case


def _build_perturbed_params(inputs: FitInputs, perturb: float, rng: np.random.Generator):
    params = Parameters()
    perturbed_log = []
    for name, spec in inputs.free_params.items():
        factor = 1.0 + rng.uniform(-perturb, +perturb)
        init = float(spec.init) * factor
        lo = -np.inf if spec.min is None else float(spec.min)
        hi = np.inf if spec.max is None else float(spec.max)
        clipped_init = init
        if init < lo:
            clipped_init = lo
        if init > hi:
            clipped_init = hi
        params.add(name, value=clipped_init, min=lo, max=hi)
        perturbed_log.append((name, float(spec.init), init, clipped_init, lo, hi))
    for name, val in inputs.fixed_params.items():
        params.add(name, value=float(val), vary=False)
    return params, perturbed_log


def _model_for(inputs: FitInputs):
    if inputs.model_kind == "gmm":
        return layerlineModelGMM
    return layerlineModel


def _format_params(params: Parameters) -> str:
    rows = []
    for k, p in params.items():
        rows.append(
            f"  {k:24s} = {p.value:14.6g}  bounds=[{p.min:>10.4g}, {p.max:<10.4g}]  "
            f"vary={p.vary}"
        )
    return "\n".join(rows)


def _residual_stats(y: np.ndarray, predicted: np.ndarray) -> str:
    if predicted is None or np.any(~np.isfinite(predicted)):
        n_bad = int(np.sum(~np.isfinite(predicted))) if predicted is not None else -1
        return f"predicted has {n_bad} non-finite values"
    res = y - predicted
    return (
        f"residual: mean={res.mean():.4g} median={np.median(res):.4g} "
        f"std={res.std():.4g} max|res|={np.max(np.abs(res)):.4g}  "
        f"y stats: mean={y.mean():.4g} std={y.std():.4g}"
    )


def diagnose(case: FitCase, *, n_seeds: int = 30, perturb: float = 0.15):
    inputs = case.inputs
    model_func = _model_for(inputs)

    print(f"=== Case: {case.meta.case_id} ===")
    print(
        f"n_peaks={len(case.meta.peaks_seed)}, "
        f"model_kind={inputs.model_kind}, n_free={len(inputs.free_params)}"
    )
    print(f"y.shape={inputs.y.shape}, y range=[{inputs.y.min():.3g}, {inputs.y.max():.3g}]")
    print(f"reference chi2={case.reference.chi2:.6g}, r2={case.reference.r2:.6f}")
    print()

    indep_kwargs = dict(inputs.independent_vars)
    indep_kwargs["x"] = inputs.x
    indep_names = tuple({"x", *inputs.independent_vars.keys()})

    failures: List[dict] = []

    for seed in range(n_seeds):
        rng = np.random.default_rng(seed)
        params, plog = _build_perturbed_params(inputs, perturb, rng)

        model = Model(
            model_func,
            nan_policy="propagate",
            independent_vars=indep_names,
        )

        t0 = time.perf_counter()
        try:
            result = model.fit(
                inputs.y,
                params=params,
                method="least_squares",
                weights=None,
                verbose=False,
                **indep_kwargs,
            )
            err = None
        except Exception as exc:  # noqa: BLE001
            elapsed = time.perf_counter() - t0
            print(
                f"seed={seed:3d}  HARD FAILURE elapsed={elapsed:.2f}s "
                f"err={type(exc).__name__}: {exc}"
            )
            failures.append({
                "seed": seed, "error": f"{type(exc).__name__}: {exc}",
                "perturbed": plog,
            })
            continue
        elapsed = time.perf_counter() - t0

        ok = bool(getattr(result, "success", True))
        nfev = int(getattr(result, "nfev", 0) or 0)
        chi2 = float(result.chisqr) if result.chisqr is not None else float("nan")
        msg = str(getattr(result, "message", "")).strip()

        # Re-evaluate predicted curve to inspect for NaN/inf
        predicted = result.best_fit
        ratio = (chi2 / case.reference.chi2) if case.reference.chi2 else float("nan")
        flag = "OK"
        if not ok:
            flag = "FAIL"
        elif chi2 < 1e-100:
            flag = "DEGENERATE"
        elif ratio > 2.0:
            flag = "WORSE"
        print(
            f"seed={seed:3d}  {flag:10s} elapsed={elapsed:6.2f}s nfev={nfev:6d}  "
            f"chi2={chi2:.4g} ratio={ratio:.4g}  msg={msg[:60]}"
        )

        if flag in ("FAIL", "DEGENERATE"):
            failures.append({
                "seed": seed,
                "elapsed": elapsed, "nfev": nfev, "chi2": chi2, "ratio": ratio,
                "message": msg,
                "perturbed": plog,
                "fitted_values": dict(result.values),
                "residual_summary": _residual_stats(inputs.y, predicted),
            })

    print()
    print("=" * 80)
    print(f"Total failures/degenerate: {len(failures)} out of {n_seeds}")
    print("=" * 80)
    for i, f in enumerate(failures):
        print(f"\n--- Failure #{i+1}  seed={f['seed']} ---")
        for k in ("elapsed", "nfev", "chi2", "ratio", "message", "error"):
            if k in f:
                print(f"  {k}: {f[k]}")
        print("  perturbed init (init -> perturbed -> clipped, bounds):")
        for name, orig, perturbed, clipped, lo, hi in f["perturbed"]:
            note = " <CLIPPED>" if perturbed != clipped else ""
            print(
                f"    {name:24s}  {orig:14.6g} -> {perturbed:14.6g} "
                f"-> {clipped:14.6g}  [{lo:>10.4g}, {hi:<10.4g}]{note}"
            )
        if "fitted_values" in f:
            print("  fitted final values:")
            for k, v in f["fitted_values"].items():
                # Highlight extreme values
                tag = ""
                try:
                    if not np.isfinite(v):
                        tag = " !!!NON-FINITE!!!"
                    elif abs(v) > 1e10:
                        tag = " !!!HUGE!!!"
                    elif abs(v) < 1e-30:
                        tag = " (essentially zero)"
                except Exception:
                    pass
                print(f"    {k:24s} = {v}{tag}")
        if "residual_summary" in f:
            print(f"  {f['residual_summary']}")


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--case",
        default="/tmp/full_capture/full_run_m6_00004_409e1b.pkl",
        help="Path to the captured FitCase pickle",
    )
    parser.add_argument("--seeds", type=int, default=30, help="Number of seeds to test")
    parser.add_argument("--perturb", type=float, default=0.15, help="Perturbation amplitude")
    args = parser.parse_args(argv)

    case = load_case(args.case)
    diagnose(case, n_seeds=args.seeds, perturb=args.perturb)


if __name__ == "__main__":
    main()
