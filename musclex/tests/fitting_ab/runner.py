"""
A/B runner: replay a directory of captured :class:`FitCase` pickles through
one or more :class:`FitterAdapter` implementations and produce a long-format
``pandas.DataFrame`` of per-(case, adapter, trial) results.

Usage (programmatic)
--------------------
>>> from musclex.tests.fitting_ab.runner import run_ab, load_cases_from_dir
>>> from musclex.tests.fitting_ab.adapters import (
...     LmfitBaselineAdapter, LmfitTRFAdapter,
... )
>>> cases = load_cases_from_dir('/tmp/musclex_fit_cases')
>>> df = run_ab(
...     adapters=[LmfitBaselineAdapter(), LmfitTRFAdapter()],
...     cases=cases,
...     n_repeats=3,
... )

Usage (CLI)
-----------
>>> python -m musclex.tests.fitting_ab.runner \
...     --cases /tmp/musclex_fit_cases \
...     --adapters lmfit-baseline-leastsq lmfit-trf \
...     --report /tmp/ab_report.csv \
...     --n-repeats 3
"""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path
from typing import Iterable, List, Optional, Sequence, Union

import numpy as np

try:
    import pandas as pd
except ImportError:  # pragma: no cover - pandas is a hard musclex dep
    pd = None  # type: ignore[assignment]

from .adapters import (
    ADAPTER_REGISTRY,
    FitterAdapter,
    get_adapter,
    load_case,
)
from .adapters.base import FitCase
from .metrics import summarize


# --------------------------------------------------------------------------- #
# Loading
# --------------------------------------------------------------------------- #


def load_cases_from_dir(path: Union[str, Path], *, pattern: str = "*.pkl") -> List[FitCase]:
    """Load every ``*.pkl`` file from ``path`` (sorted by name)."""
    p = Path(path).expanduser().resolve()
    if not p.is_dir():
        raise NotADirectoryError(p)
    files = sorted(p.glob(pattern))
    cases: List[FitCase] = []
    for f in files:
        try:
            cases.append(load_case(f))
        except Exception as exc:  # noqa: BLE001
            sys.stderr.write(f"[runner] skipping {f.name}: {exc}\n")
    return cases


# --------------------------------------------------------------------------- #
# Core run
# --------------------------------------------------------------------------- #


def run_ab(
    adapters: Sequence[FitterAdapter],
    cases: Sequence[FitCase],
    *,
    n_repeats: int = 1,
    perturb_init: Optional[float] = None,
    progress: bool = True,
):
    """Run every ``adapter x case x trial`` combo and return a DataFrame.

    Returns a pandas DataFrame with one row per fit invocation.
    """
    if pd is None:
        raise RuntimeError("pandas is required for run_ab()")

    rows = []
    total = len(adapters) * len(cases) * max(1, n_repeats)
    done = 0
    t_start = time.perf_counter()

    for case in cases:
        for adapter in adapters:
            for trial in range(max(1, n_repeats)):
                row = {
                    "case_id": case.meta.case_id,
                    "box_name": case.meta.box_name,
                    "n_peaks": len(case.meta.peaks_seed),
                    "model_kind": case.inputs.model_kind,
                    "has_voigt": case.inputs.has_voigt,
                    "use_common_sigma": case.meta.use_common_sigma,
                    "bgsub": case.meta.bgsub,
                    "n_free": case.n_free(),
                    "tags": ",".join(case.meta.difficulty_tags),
                    "adapter": adapter.name,
                    "trial": trial,
                    "perturb_init": perturb_init or 0.0,
                }
                try:
                    fit_result = adapter.fit(case, perturb_init=perturb_init)
                except Exception as exc:  # noqa: BLE001
                    row.update({
                        "success": False,
                        "converged": False,
                        "aborted": False,
                        "elapsed_s": float("nan"),
                        "n_eval": None,
                        "chi2": None, "redchi": None, "r2": None,
                        "ref_chi2": (case.reference.chi2 if case.reference else None),
                        "ref_r2": (case.reference.r2 if case.reference else None),
                        "ref_elapsed_s": (
                            case.reference.elapsed_s if case.reference else None
                        ),
                        "chi2_ratio": None, "speed_ratio": None,
                        "p_max_abs_diff": None, "amp_max_rel_diff": None,
                        "error": f"{type(exc).__name__}: {exc}",
                    })
                else:
                    row.update(summarize(fit_result, case))
                    row["error"] = ""
                rows.append(row)

                done += 1
                if progress and (done % 25 == 0 or done == total):
                    elapsed = time.perf_counter() - t_start
                    rate = done / elapsed if elapsed > 0 else 0
                    sys.stderr.write(
                        f"[runner] {done}/{total} fits done "
                        f"({rate:.1f} fits/s, elapsed {elapsed:.1f}s)\n"
                    )

    df = pd.DataFrame(rows)
    return df


# --------------------------------------------------------------------------- #
# Top-level summary helper
# --------------------------------------------------------------------------- #


def summarize_dataframe(df) -> "pd.DataFrame":  # type: ignore[name-defined]
    """Aggregate ``run_ab`` output into a per-adapter summary table."""
    if pd is None:
        raise RuntimeError("pandas is required")
    if df.empty:
        return df

    grouped = df.groupby("adapter")
    summary = grouped.agg(
        n_fits=("case_id", "count"),
        success_rate=("success", "mean"),
        aborted_rate=("aborted", "mean") if "aborted" in df.columns else ("success", "mean"),
        elapsed_median=("elapsed_s", "median"),
        elapsed_mean=("elapsed_s", "mean"),
        speed_ratio_median=("speed_ratio", "median"),
        chi2_ratio_median=("chi2_ratio", "median"),
        p_max_diff_median=("p_max_abs_diff", "median"),
        p_max_diff_p95=(
            "p_max_abs_diff",
            lambda s: s.dropna().quantile(0.95) if not s.dropna().empty else float("nan"),
        ),
        amp_diff_median=("amp_max_rel_diff", "median"),
    )
    return summary.sort_values("elapsed_median")


# --------------------------------------------------------------------------- #
# CLI entry point
# --------------------------------------------------------------------------- #


def _parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument(
        "--cases", required=True,
        help="Directory containing FitCase pickle files (*.pkl).",
    )
    p.add_argument(
        "--adapters", nargs="+", default=list(ADAPTER_REGISTRY),
        help=f"Adapter names to compare. Available: {sorted(ADAPTER_REGISTRY)}",
    )
    p.add_argument(
        "--report", default=None,
        help="Where to write the long-format CSV. Default: stdout.",
    )
    p.add_argument(
        "--summary", default=None,
        help="Optional: write the per-adapter summary CSV to this path.",
    )
    p.add_argument(
        "--n-repeats", type=int, default=1,
        help="Number of trials per (adapter, case). Default: 1.",
    )
    p.add_argument(
        "--perturb-init", type=float, default=None,
        help=(
            "If set, multiply each free init by a random factor in "
            "[1-perturb, 1+perturb] (clamped to bounds). Use for "
            "robustness experiments."
        ),
    )
    p.add_argument(
        "--limit", type=int, default=None,
        help="Optional: cap the number of cases (for quick smoke runs).",
    )
    return p.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> int:
    if pd is None:
        sys.stderr.write("pandas is required for the runner CLI\n")
        return 2

    args = _parse_args(argv)

    cases = load_cases_from_dir(args.cases)
    if args.limit:
        cases = cases[: args.limit]
    if not cases:
        sys.stderr.write(f"No cases found under {args.cases}\n")
        return 1

    adapters: List[FitterAdapter] = []
    for name in args.adapters:
        try:
            adapters.append(get_adapter(name))
        except KeyError as exc:
            sys.stderr.write(f"{exc}\n")
            return 2

    sys.stderr.write(
        f"[runner] {len(cases)} cases x {len(adapters)} adapters x "
        f"{args.n_repeats} trials = "
        f"{len(cases) * len(adapters) * args.n_repeats} fits\n"
    )

    df = run_ab(
        adapters=adapters,
        cases=cases,
        n_repeats=args.n_repeats,
        perturb_init=args.perturb_init,
    )

    if args.report:
        Path(args.report).parent.mkdir(parents=True, exist_ok=True)
        df.to_csv(args.report, index=False)
        sys.stderr.write(f"[runner] wrote report -> {args.report}\n")
    else:
        df.to_csv(sys.stdout, index=False)

    summary = summarize_dataframe(df)
    if args.summary:
        Path(args.summary).parent.mkdir(parents=True, exist_ok=True)
        summary.to_csv(args.summary)
        sys.stderr.write(f"[runner] wrote summary -> {args.summary}\n")
    sys.stderr.write("\n=== Per-adapter summary (median elapsed) ===\n")
    sys.stderr.write(summary.to_string())
    sys.stderr.write("\n")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
