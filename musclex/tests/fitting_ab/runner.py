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
...     --report ab_report.csv \
...     --n-repeats 3
...
... # Outputs go to musclex/tests/fitting_ab/ab_report.csv by default.
... # Pass --out-dir /some/other/dir to change the output location.
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
from .metrics import per_param_rows, summarize


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
    collect_param_rows: bool = False,
):
    """Run every ``adapter x case x trial`` combo and return a DataFrame.

    Returns
    -------
    df : pandas.DataFrame
        Long-format, one row per fit invocation.
    param_df : pandas.DataFrame, optional
        Returned only when ``collect_param_rows`` is True. Long-format,
        one row per (fit invocation, free parameter), with columns
        ``adapter, case_id, trial, perturb_init, param_name, ref_value,
        cand_value, diff, abs_diff, rel_diff``. Used to build the
        consistency table.
    """
    if pd is None:
        raise RuntimeError("pandas is required for run_ab()")

    rows = []
    param_rows: List[dict] = [] if collect_param_rows else []
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
                    if collect_param_rows and case.reference is not None:
                        ctx = {
                            "adapter": adapter.name,
                            "case_id": case.meta.case_id,
                            "box_name": case.meta.box_name,
                            "trial": trial,
                            "perturb_init": perturb_init or 0.0,
                        }
                        for prow in per_param_rows(fit_result, case):
                            param_rows.append({**ctx, **prow})
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
    if collect_param_rows:
        param_df = pd.DataFrame(param_rows)
        return df, param_df
    return df


# --------------------------------------------------------------------------- #
# Top-level summary helper
# --------------------------------------------------------------------------- #


def _iqr(s) -> float:
    """Interquartile range (P75 - P25). Robust dispersion estimator for
    right-skewed distributions like wall-clock time, where ``std`` is
    dominated by the long tail of scheduler / GC jitter.
    """
    s = s.dropna()
    if s.empty:
        return float("nan")
    return float(s.quantile(0.75) - s.quantile(0.25))


def _p95(s) -> float:
    s = s.dropna()
    return float(s.quantile(0.95)) if not s.empty else float("nan")


def summarize_dataframe(df) -> "pd.DataFrame":  # type: ignore[name-defined]
    """Aggregate ``run_ab`` output into a per-adapter summary table.

    Notes
    -----
    * **Time dispersion**: we report both ``elapsed_std`` and ``elapsed_iqr``.
      ``std`` is what users typically request, but wall-clock time is
      right-skewed (occasional GC / scheduler spikes), so ``IQR`` is what we
      recommend for ranking. Both are kept so the reader can decide.
    * **Error dispersion**: aggregated on ``chi2_ratio`` (per-case
      normalized) rather than absolute ``chi2``. Raw chi-square varies by
      orders of magnitude across cases (peak intensity scale), so a global
      mean / std on ``chi2`` would be dominated by whichever case happens
      to have the brightest data, not by adapter behavior.
    * **Deterministic replay caveat**: when no perturbation is applied, all
      trials of a given (adapter, case) feed identical inputs to the
      optimizer and must produce identical outputs (modulo timer noise).
      In that mode ``chi2_ratio_std`` and similar columns will read 0 or
      ``NaN`` and only the timing columns carry signal.
    """
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
        elapsed_std=("elapsed_s", "std"),
        elapsed_iqr=("elapsed_s", _iqr),

        speed_ratio_median=("speed_ratio", "median"),

        chi2_ratio_median=("chi2_ratio", "median"),
        chi2_ratio_mean=("chi2_ratio", "mean"),
        chi2_ratio_std=("chi2_ratio", "std"),

        r2_median=("r2", "median"),
        r2_mean=("r2", "mean"),
        r2_std=("r2", "std"),

        p_max_diff_median=("p_max_abs_diff", "median"),
        p_max_diff_p95=("p_max_abs_diff", _p95),
        amp_diff_median=("amp_max_rel_diff", "median"),
    )
    return summary.sort_values("elapsed_median")


def summarize_consistency(param_df) -> "pd.DataFrame":  # type: ignore[name-defined]
    """Per-(adapter, case, parameter) mean / std of the fitted value across trials.

    This is the table that answers "how stable is each model parameter
    when the optimizer is run repeatedly with perturbed initial values?".

    The input ``param_df`` comes from :func:`run_ab` with
    ``collect_param_rows=True``. Both reference and candidate values fed
    into ``param_df`` are already canonicalized in
    :func:`metrics.per_param_rows`, so symmetric label swaps (PT
    ``center_sigma1<->2``, peak ordering) do not show up as spurious
    instability here.

    Caveats
    -------
    * ``cand_std`` and ``abs_diff_std`` will be 0 (or NaN) if all trials
      use byte-identical inputs (deterministic replay). Run with
      ``perturb_init > 0`` and ``n_repeats > 1`` to see real dispersion.
    * ``cand_std`` is the trial-wise std of the *fitted* value, which is
      what most users mean by "parameter stability".
    """
    if pd is None:
        raise RuntimeError("pandas is required")
    if param_df is None or param_df.empty:
        return param_df

    grouped = param_df.groupby(["adapter", "case_id", "param_name"])
    summary = grouped.agg(
        n_trials=("cand_value", "count"),
        ref_value=("ref_value", "first"),
        cand_mean=("cand_value", "mean"),
        cand_std=("cand_value", "std"),
        cand_min=("cand_value", "min"),
        cand_max=("cand_value", "max"),
        diff_mean=("diff", "mean"),
        diff_std=("diff", "std"),
        abs_diff_mean=("abs_diff", "mean"),
        abs_diff_max=("abs_diff", "max"),
        rel_diff_mean=("rel_diff", "mean"),
        rel_diff_std=("rel_diff", "std"),
    ).reset_index()
    return summary


def summarize_per_case(df) -> "pd.DataFrame":  # type: ignore[name-defined]
    """Per-(adapter, case) error/time mean ± std.

    This is the table that lets you see, for each individual fit case, how
    much an adapter wobbles across trials. Only meaningful when ``df``
    contains multiple trials per (adapter, case) — typically a perturbation
    sweep with ``--n-repeats > 1`` and ``--perturb-init`` set.
    """
    if pd is None:
        raise RuntimeError("pandas is required")
    if df.empty:
        return df

    grouped = df.groupby(["adapter", "case_id"])
    return grouped.agg(
        n_trials=("trial", "count"),
        success_rate=("success", "mean"),
        elapsed_mean=("elapsed_s", "mean"),
        elapsed_std=("elapsed_s", "std"),
        elapsed_median=("elapsed_s", "median"),
        elapsed_iqr=("elapsed_s", _iqr),
        chi2_mean=("chi2", "mean"),
        chi2_std=("chi2", "std"),
        chi2_ratio_mean=("chi2_ratio", "mean"),
        chi2_ratio_std=("chi2_ratio", "std"),
        r2_mean=("r2", "mean"),
        r2_std=("r2", "std"),
        p_max_diff_mean=("p_max_abs_diff", "mean"),
        p_max_diff_std=("p_max_abs_diff", "std"),
    ).reset_index()


# --------------------------------------------------------------------------- #
# CLI entry point
# --------------------------------------------------------------------------- #


_FITTING_AB_DIR = Path(__file__).parent


def _resolve_out(path: Optional[str], out_dir: Path) -> Optional[Path]:
    """Resolve *path* relative to *out_dir* when it is a bare filename."""
    if path is None:
        return None
    p = Path(path)
    if not p.is_absolute():
        p = out_dir / p
    return p


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
        "--out-dir", default=str(_FITTING_AB_DIR),
        help=(
            "Base directory for output CSVs when relative paths are given. "
            f"Default: {_FITTING_AB_DIR}"
        ),
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
        "--per-case", default=None,
        help=(
            "Optional: write per-(adapter, case) mean/std CSV to this path. "
            "Useful when error / time vary case-by-case."
        ),
    )
    p.add_argument(
        "--consistency", default=None,
        help=(
            "Optional: write per-(adapter, case, param) consistency CSV "
            "(mean / std of each fitted parameter across trials). Only "
            "meaningful with --perturb-init > 0 and --n-repeats > 1."
        ),
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
    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    report_path = _resolve_out(args.report, out_dir)
    summary_path = _resolve_out(args.summary, out_dir)
    per_case_path = _resolve_out(args.per_case, out_dir)
    consistency_path = _resolve_out(args.consistency, out_dir)

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

    want_param_rows = bool(consistency_path)
    if want_param_rows:
        df, param_df = run_ab(
            adapters=adapters,
            cases=cases,
            n_repeats=args.n_repeats,
            perturb_init=args.perturb_init,
            collect_param_rows=True,
        )
    else:
        df = run_ab(
            adapters=adapters,
            cases=cases,
            n_repeats=args.n_repeats,
            perturb_init=args.perturb_init,
        )
        param_df = None

    if report_path:
        report_path.parent.mkdir(parents=True, exist_ok=True)
        df.to_csv(report_path, index=False)
        sys.stderr.write(f"[runner] wrote report -> {report_path}\n")
    else:
        df.to_csv(sys.stdout, index=False)

    summary = summarize_dataframe(df)
    if summary_path:
        summary_path.parent.mkdir(parents=True, exist_ok=True)
        summary.to_csv(summary_path)
        sys.stderr.write(f"[runner] wrote summary -> {summary_path}\n")

    if per_case_path:
        per_case = summarize_per_case(df)
        per_case_path.parent.mkdir(parents=True, exist_ok=True)
        per_case.to_csv(per_case_path, index=False)
        sys.stderr.write(f"[runner] wrote per-case summary -> {per_case_path}\n")

    if consistency_path:
        consistency = summarize_consistency(param_df)
        consistency_path.parent.mkdir(parents=True, exist_ok=True)
        consistency.to_csv(consistency_path, index=False)
        sys.stderr.write(f"[runner] wrote consistency table -> {consistency_path}\n")
        if (args.perturb_init or 0) == 0:
            sys.stderr.write(
                "[runner] note: --perturb-init was not set, so consistency "
                "stds will be ~0 (deterministic replay).\n"
            )

    sys.stderr.write("\n=== Per-adapter summary (median elapsed) ===\n")
    sys.stderr.write(summary.to_string())
    sys.stderr.write("\n")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
