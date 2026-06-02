#!/usr/bin/env python3
"""
Validate the ScipyAnalyticJacAdapter against LmfitTRFNumpyAdapter (L1 reference)
on all captured real cases in real_cases/.

For each case the script reports:
  - Speed: median elapsed time (5 reps each)
  - Convergence: success/fail flags for both adapters
  - Parameter agreement: max |Δp_i| and max |Δamp_i| across all peaks
  - Quality: R² for both adapters

Usage
-----
    cd /path/to/musclex
    MUSCLEX_CAPTURE_FITS=0 python3.10 -m musclex.tests.fitting_ab.scripts.validate_analytic_jac
or directly:
    python3.10 musclex/tests/fitting_ab/scripts/validate_analytic_jac.py

Output columns
--------------
  file        : pkl filename
  model/len/pk: model kind, array length, number of peaks
  L1_ms       : median fit time for LmfitTRFNumpyAdapter
  AJ_ms       : median fit time for ScipyAnalyticJacAdapter
  speedup     : L1_ms / AJ_ms
  L1_ok       : L1 converged?
  AJ_ok       : AJ converged?
  max_dp      : max |p_i(AJ) - p_i(L1)| across all peak positions (pixels)
  max_damp%   : max |Δamp_i / amp_i(L1)| × 100  across all amplitudes
  L1_r2 / AJ_r2 : R² of both solutions
"""
from __future__ import annotations

import glob
import os
import sys
import time
from pathlib import Path

import numpy as np

# ── path setup ────────────────────────────────────────────────────────────────
_ROOT = Path(__file__).resolve().parents[4]   # repo root
if str(_ROOT) not in sys.path:
    sys.path.insert(0, str(_ROOT))

os.environ.setdefault("MUSCLEX_CAPTURE_FITS", "0")
os.environ.setdefault("MUSCLEX_SKIP_ENV_CHECK", "1")

from musclex.tests.fitting_ab.adapters import (
    LmfitTRFNumpyAdapter,
    ScipyAnalyticJacAdapter,
)
from musclex.tests.fitting_ab.adapters.base import load_case

# ── locate real cases ─────────────────────────────────────────────────────────
_CASES_DIR = Path(__file__).parent.parent / "real_cases"
pkl_files = sorted(_CASES_DIR.glob("*.pkl"))

if not pkl_files:
    print(f"No .pkl files found in {_CASES_DIR}. Run MuscleX with MUSCLEX_CAPTURE_FITS=1 first.")
    sys.exit(1)

# ── adapters ──────────────────────────────────────────────────────────────────
REPS = 5
ref_ad = LmfitTRFNumpyAdapter(seed=0)
aj_ad  = ScipyAnalyticJacAdapter(seed=0)

# ── helpers ───────────────────────────────────────────────────────────────────

def timed_fit(adapter, case, reps):
    """Return (median_ms, last_FitResult)."""
    times = []
    result = None
    for _ in range(reps):
        t0 = time.perf_counter()
        result = adapter.fit(case)
        times.append(time.perf_counter() - t0)
    return float(np.median(times)) * 1000, result


def param_diff(r_ref, r_cand, free_names):
    """Return max |Δp_i| and max rel |Δamp_i| for peak params."""
    p_keys   = [k for k in free_names if k.startswith('p_')]
    amp_keys = [k for k in free_names if k.startswith('amplitude')]
    if not r_ref.values or not r_cand.values:
        return float('nan'), float('nan')
    max_dp = max(
        abs(r_cand.values.get(k, 0) - r_ref.values.get(k, 0))
        for k in p_keys
    ) if p_keys else float('nan')
    max_da = max(
        abs(r_cand.values.get(k, 0) - r_ref.values.get(k, 0))
        / max(abs(r_ref.values.get(k, 1.0)), 1e-6) * 100
        for k in amp_keys
    ) if amp_keys else float('nan')
    return max_dp, max_da


# ── main loop ─────────────────────────────────────────────────────────────────

rows = []
HEADER = (
    f"{'file':<36} {'mod':<5} {'len':>5} {'pk':>3}"
    f" | {'L1_ms':>7} {'AJ_ms':>7} {'spdup':>6}"
    f" | {'L1':>3} {'AJ':>3}"
    f" | {'max_dp':>7} {'damp%':>7}"
    f" | {'L1_r2':>6} {'AJ_r2':>6}"
)
SEP = "-" * len(HEADER)

print(HEADER)
print(SEP)

speedups, ok_both, ok_l1_only, fail_both = [], [], [], []

for pkl in pkl_files:
    case = load_case(str(pkl))
    inp  = case.inputs
    free_names = list(inp.free_params.keys())
    n_peaks = len([k for k in free_names if k.startswith('p_')])

    l1_ms,  r_l1 = timed_fit(ref_ad, case, REPS)
    aj_ms,  r_aj = timed_fit(aj_ad,  case, REPS)

    speedup = l1_ms / aj_ms if aj_ms > 0 else float('nan')
    max_dp, max_da = param_diff(r_l1, r_aj, free_names)

    l1_ok = "✓" if r_l1.success else "✗"
    aj_ok = "✓" if r_aj.success else "✗"

    l1_r2 = f"{r_l1.r2:.4f}" if r_l1.r2 is not None else "  N/A"
    aj_r2 = f"{r_aj.r2:.4f}" if r_aj.r2 is not None else "  N/A"

    dp_str = f"{max_dp:.3f}" if np.isfinite(max_dp) else "  N/A"
    da_str = f"{max_da:.2f}" if np.isfinite(max_da) else "  N/A"

    fname = pkl.name
    print(
        f"{fname:<36} {inp.model_kind[:3]:<5} {len(inp.x):>5} {n_peaks:>3}"
        f" | {l1_ms:>7.1f} {aj_ms:>7.1f} {speedup:>6.2f}x"
        f" | {l1_ok:>3} {aj_ok:>3}"
        f" | {dp_str:>7} {da_str:>7}"
        f" | {l1_r2:>6} {aj_r2:>6}"
    )
    sys.stdout.flush()

    rows.append(dict(
        fname=fname, model=inp.model_kind, len_x=len(inp.x),
        n_peaks=n_peaks, l1_ms=l1_ms, aj_ms=aj_ms, speedup=speedup,
        l1_ok=r_l1.success, aj_ok=r_aj.success,
        max_dp=max_dp, max_da=max_da,
        l1_r2=r_l1.r2, aj_r2=r_aj.r2,
    ))

    if r_l1.success and r_aj.success:
        ok_both.append(speedup)
    elif r_l1.success and not r_aj.success:
        ok_l1_only.append(fname)
    elif not r_l1.success and not r_aj.success:
        fail_both.append(fname)

# ── summary ───────────────────────────────────────────────────────────────────
print()
print("=" * len(HEADER))
print(f"Cases where both converge   : {len(ok_both)}")
if ok_both:
    print(f"  Speedup  median={np.median(ok_both):.2f}x  "
          f"min={min(ok_both):.2f}x  max={max(ok_both):.2f}x")

# Parameter agreement among both-converged cases
ok_rows = [r for r in rows if r['l1_ok'] and r['aj_ok']]
if ok_rows:
    valid_dp = [r['max_dp'] for r in ok_rows if np.isfinite(r['max_dp'])]
    valid_da = [r['max_da'] for r in ok_rows if np.isfinite(r['max_da'])]
    if valid_dp:
        print(f"  max |Δp_i| (peak pos)      : median={np.median(valid_dp):.3f} px  "
              f"max={max(valid_dp):.3f} px")
    if valid_da:
        print(f"  max |Δamp_i/amp_i| (%)      : median={np.median(valid_da):.2f}%  "
              f"max={max(valid_da):.2f}%")

if ok_l1_only:
    print(f"\nL1 converged but AJ failed  : {len(ok_l1_only)}")
    for f in ok_l1_only:
        print(f"  {f}")

fail_aj_only = [r['fname'] for r in rows if not r['l1_ok'] and r['aj_ok']]
if fail_aj_only:
    print(f"\nAJ converged but L1 failed  : {len(fail_aj_only)}")
    for f in fail_aj_only:
        print(f"  {f}")

if fail_both:
    print(f"\nBoth failed                 : {len(fail_both)}")
    for f in fail_both:
        print(f"  {f}")

print()
print("CONCLUSION:")
good_dp = [r for r in ok_rows if np.isfinite(r['max_dp']) and r['max_dp'] < 0.5]
print(f"  Peak position agrees to <0.5 px: {len(good_dp)}/{len(ok_rows)} both-converged cases")
fast = [r for r in ok_rows if r['speedup'] > 2.0]
print(f"  Speedup > 2×:  {len(fast)}/{len(ok_rows)} cases")
