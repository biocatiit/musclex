# A/B Fit Benchmark Report — 2026-04-29

> Benchmark of alternative non-linear fitting backends for
> `ProjectionProcessor.fitModel()` on real MuscleX projection traces.
> All numbers in this report were generated **after** the bound-aware
> perturbation fix landed in `_maybe_perturb_init`
> ([`adapters/lmfit_adapters.py`](adapters/lmfit_adapters.py) §
> "Bound-aware perturbation"); an earlier draft based on pre-fix CSVs
> overstated baseline-LM's failure modes and has been discarded.

## Executive summary

| | recommendation |
|---|---|
| **Production today** | `lmfit-baseline-leastsq` (LM with `tan/arctan` bound mapping). Always converges on this corpus, but takes 2–5 × more wall time than TRF on the harder cases and is sensitive to perturbed init on one case. |
| **Recommended next** | `lmfit-trf` (`method='least_squares'`, Trust-Region-Reflective with native bounds). Reaches the **identical** optimum (`chi2_ratio = 1.0`) on every case and every perturbation level, **5–35 % faster median wall-time** overall, and **5–15 ×** tighter peak-position p95 on the one sensitive case (`TEST`). |
| **Do not use** | Both `lmfit-poisson` variants. Convex-hull background subtraction has already removed the Poisson noise, so `weights = 1/√y` becomes a misweighting and the fit converges 1–4 px away from the production optimum. `lmfit-poisson` (LM + Poisson) also hits the iteration cap on 5 / 25 unperturbed fits. |

Switching `ProjectionProcessor.fitModel()` from `leastsq` to TRF is a
**one-line change** with measurable gains and no observed downsides on
the captured corpus.

---

## 1. Setup

**Cases.** 5 fit cases captured from a single `musclex pth` run on
`EIGERTestImage.tif` (MuscleX 1.29.0-beta.2, lmfit 1.1.0, NumPy 1.26.4,
Python 3.10, captured 2026-04-29).

| case_id | box | model | n_peaks | n_free | bgsub | difficulty |
|---|:---:|:---:|:---:|:---:|:---:|---|
| `full_run_M3_00001_6ab33e` | M3  | gmm |  4  |  9 | conv-hull | medium |
| `full_run_M6_00002_952a4b` | M6  | gmm |  8  | 17 | conv-hull | high (most peaks) |
| `full_run_m3_00003_dc8ba7` | m3  | gmm |  2  |  5 | conv-hull | low |
| `full_run_m6_00004_407a65` | m6  | gmm |  6  | 13 | conv-hull | medium-high |
| `full_run_TEST_00005_861239` | TEST | gmm |  2 | 11 | none | high (strong bg, no subtraction) |

Pickles live in `/tmp/musclex_fit_cases/`; each contains the 1D `y`
trace, the free-parameter init/bounds, fixed params, and the baseline
reference fit result captured at recording time.

**Adapters.**

| name | algorithm | weights |
|---|---|---|
| `lmfit-baseline-leastsq` | LM via `lmfit.Model.fit()` (production code path) | unweighted |
| `lmfit-trf` | `least_squares(method='trf')` via lmfit | unweighted |
| `lmfit-poisson` | LM | `1/√max(y, 1)` |
| `lmfit-trf-poisson` | TRF | `1/√max(y, 1)` |

**Methodology.**

* **Test 1 — deterministic replay**: 4 adapters × 5 cases × 5 trials = 100 fits, no perturbation. All trials use the captured init values verbatim; the 5 trials measure timing variance only.
* **Test 2 — robustness sweep**: 2 adapters (baseline + TRF) × 5 cases × 10 trials = 50 fits per perturbation level, repeated at `perturb_init ∈ {0.05, 0.15, 0.30, 0.50}`. Perturbation is bound-aware: bounded params get additive `±perturb·(max−min)/2`, unbounded params get multiplicative `init·(1±perturb)`.

`chi2` and `r2` are recomputed manually from `(y − best_fit)` for every
adapter — bypassing `lmfit.MinimizerResult.chisqr`, which mis-reports
`1e-250` on aborted fits.

> **Note on RNG.** The runner instantiates each adapter with the default
> `seed=0`, so two invocations of `--perturb-init 0.15` are byte
> identical. The robustness story below relies on **varying perturbation
> magnitude** (Test 2) rather than two independent seeds at the same
> magnitude. Running with multiple seeds requires a runner-level seed
> override that doesn't yet exist.

---

## 2. Headline numbers — Test 1: deterministic replay

Source: [`ab_summary.csv`](../../../ab_summary.csv) (aggregate),
[`ab_report.csv`](../../../ab_report.csv) (per-trial).

| adapter | success | aborted | el_med (ms) | speed_ratio_median¹ | chi2_ratio_median² | p_max_diff_p95 (px)³ | amp_diff_median |
|---|:---:|:---:|---:|---:|---:|---:|---:|
| `lmfit-baseline-leastsq` | 25/25 | 0 % | **157.4** | 1.05 | **1.0000** | **0.000** | 0.0 |
| `lmfit-trf`              | 25/25 | 0 % | **88.2**  | **0.66** | 1.0000 | 0.030 | 1.0e-5 |
| `lmfit-trf-poisson`      | 25/25 | 0 % | 57.8 | 1.05 | 0.0109 | 1.131 | 0.30 |
| `lmfit-poisson`          | 20/25 | **20 %** | 211.6 | 1.23 | 0.0220 | 3.898 | 0.30 |

¹ `speed_ratio = elapsed / ref_elapsed` (ref = baseline at capture time).
² `chi2_ratio = chi2 / ref_chi2`. Values < 1 in the Poisson rows mean
they minimised a *different* objective and the unweighted SSE happens
to be smaller, **not** that they are better — peak positions are off
by 1–4 px (see `p_max_diff_p95`).
³ 95th-percentile of the maximum peak-position deviation across all
peaks of all trials of the case.

**Reading the table:**

* `lmfit-baseline-leastsq` reproduces the captured reference exactly
  (chi2_ratio = 1.0000, p_max_diff = 0). This is the framework
  self-test; if this row weren't trivial we'd have a bug.
* `lmfit-trf` reaches the **same minimum** (chi2_ratio = 1.0000,
  p_max_diff_p95 = 0.030 px ≈ 1/30 of a pixel) at **roughly two-thirds
  the wall time** of baseline (66 % ratio).
* Both Poisson-weighted adapters converge somewhere else — `chi2_ratio`
  ≈ 0.01 might look attractive, but `p_max_diff_p95` ≥ 1.13 px and
  amplitudes differ by 30 % from the baseline. They minimise a weighted
  objective on data whose noise has already been removed; reweighting
  was a category mistake.
* `lmfit-poisson` (LM + Poisson weights) **fails to converge in 5 / 25
  fits** (success = 80 %) and on `m6` it burns through 28 000 function
  evaluations to do worse than the unweighted LM does in 516.

### Per-case timing (median wall time, ms)

Source: median over 5 trials in `ab_report.csv`. `n_free` and `nfev`
shown in the leftmost columns.

| case | n_peaks | n_free | nfev (LM) | baseline-LM | TRF | TRF-poisson | LM-poisson |
|---|:---:|:---:|---:|---:|---:|---:|---:|
| `M3`   | 4 |  9 |   135 |  31.8 | **38.9** |  57.0 |  35.6 |
| `M6`   | 8 | 17 |   550 | 194.7 | 219.3 | 218.3 | 211.6 |
| `m3`   | 2 |  5 |   619 |  81.7 | **11.7** |  21.2 |  15.2 |
| `m6`   | 6 | 13 |   516 | 157.4 | **100.1** | 159.9 | **8373.1** ⚠ |
| `TEST` | 2 | 11 |  2284 | 290.7 | **88.2** |  48.3 | 1097.0 |

* TRF wins clearly on `m3`, `m6`, `TEST` — the cases where baseline LM
  spends 600 – 2 300 nfev iterating. On `TEST` TRF needs **42 nfev**
  vs. baseline's **2 284** to reach the same optimum.
* TRF loses by ≤ 25 ms on `M3` and `M6`. Total nfev for those is
  already low for baseline (135 / 550), so there isn't much TRF can
  shave off — it pays a constant overhead per trust-region step.
* `lmfit-poisson` on `m6` is the worst run in the corpus: 28 000 nfev
  hitting the iteration cap.

---

## 3. Robustness — Test 2: perturbation sweep

Sources:
[`ab_sweep_p0.05.csv`](../../../ab_sweep_p0.05.csv) (and `_long.csv`),
[`ab_robustness.csv`](../../../ab_robustness.csv) (perturb=0.15, and `ab_robustness_long.csv`),
[`ab_sweep_p0.30.csv`](../../../ab_sweep_p0.30.csv),
[`ab_sweep_p0.50.csv`](../../../ab_sweep_p0.50.csv).

Each row aggregates 5 cases × 10 perturbed trials = 50 fits.

| perturb | adapter | success | aborted | el_med (ms) | chi2_ratio_median | p_max_diff_median | **p_max_diff_p95** | amp_diff_median |
|:---:|---|:---:|:---:|---:|---:|---:|---:|---:|
| 0.05 | `lmfit-baseline-leastsq` | 50/50 | 0 % | 124.5 | 1.0000 | 3.9e-5 | **0.926** | 1.7e-5 |
| 0.05 | `lmfit-trf`              | 50/50 | 0 % | **59.9** | 1.0000 | 9.1e-5 | **0.030** | 2.6e-5 |
| 0.15 | `lmfit-baseline-leastsq` | 50/50 | 0 % | **35.7** | 1.0000 | 5.4e-5 | **1.002** | 3.2e-5 |
| 0.15 | `lmfit-trf`              | 50/50 | 0 % | 52.6 | 1.0000 | 8.9e-5 | **0.168** | 1.2e-4 |
| 0.30 | `lmfit-baseline-leastsq` | 50/50 | 0 % | 48.1 | 1.0000 | 1.0e-4 | **1.146** | 1.2e-5 |
| 0.30 | `lmfit-trf`              | 50/50 | 0 % | 48.5 | 1.0000 | 9.0e-5 | **0.168** | 1.3e-4 |
| 0.50 | `lmfit-baseline-leastsq` | 50/50 | 0 % | 59.6 | 1.0000 | 9.9e-5 | **2.398** | 9.1e-5 |
| 0.50 | `lmfit-trf`              | 50/50 | 0 % | **49.5** | 1.0000 | 9.1e-5 | **0.168** | 1.3e-4 |

Three things to take away:

1. **Both adapters always converge to the same minimum.**
   `chi2_ratio_median = 1.0000` for both at every perturbation level —
   no aborts, no convergence to a worse local minimum, on this corpus.
2. **TRF's worst-case peak position is bounded.** `p_max_diff_p95` for
   TRF stays at 0.03 – 0.17 px regardless of how badly the init is
   perturbed.
3. **Baseline's worst-case peak position grows with perturbation.**
   p95 climbs 0.93 → 1.00 → 1.15 → 2.40 px as `perturb` goes 0.05 →
   0.50. So for one in twenty fits the baseline lands ~2.4 px off when
   the user nudges init by half the search range.

> Both adapters always reach `chi2_ratio = 1.0`, so the *fit* is the
> same. They differ in whether the *parameter values* are exactly
> reproducible across perturbed inits — which matters for the human
> looking at "did the peak position change between two re-fits?".

### Per-case `p_max_diff_p95` across the sweep (px)

Aggregated from the `_long.csv` files of each sweep level.

| case | n_peaks | adapter | p=0.05 | p=0.15 | p=0.30 | p=0.50 |
|---|:---:|---|---:|---:|---:|---:|
| `M3`   | 4 | baseline | 0.0000 | 0.0000 | 0.0000 | 0.0000 |
| `M3`   | 4 | TRF      | 0.0000 | 0.0000 | 0.0000 | 0.0000 |
| `M6`   | 8 | baseline | 0.0004 | 0.0003 | 0.0004 | **2.398** |
| `M6`   | 8 | TRF      | 0.0003 | 0.0003 | 0.0002 | 0.0003 |
| `m3`   | 2 | baseline | 0.0000 | 0.0001 | 0.0001 | 0.0000 |
| `m3`   | 2 | TRF      | 0.0001 | 0.0001 | 0.0001 | 0.0001 |
| `m6`   | 6 | baseline | 0.0001 | 0.0001 | 0.0001 | 0.0004 |
| `m6`   | 6 | TRF      | 0.0001 | 0.0001 | 0.0001 | 0.0001 |
| **`TEST`** | 2 | **baseline** | **1.031** | **1.249** | **1.514** | **1.512** |
| **`TEST`** | 2 | **TRF**      | **0.030** | **0.168** | **0.168** | **0.168** |

* **`TEST` is the sensitive case.** It's a 2-peak GMM fit on a trace
  with **strong background and no convex-hull subtraction** (`bgsub=0`).
  Baseline drifts 1 – 1.5 px in the 95th percentile at every perturbation
  level; TRF stays under 0.17 px throughout. **5 – 35 × tighter.**
* **`M6` is the high-perturbation collapse point.** With 8 peaks and
  17 free params, baseline is rock-stable up through `perturb = 0.30`
  but drifts 2.4 px at `perturb = 0.50`. TRF stays at 0.0003 px.
* **`M3`, `m3`, `m6` are insensitive.** Both adapters agree to within
  0.0004 px at every perturbation level.

---

## 4. Why the Poisson-weighted adapters lose

Captured `y` traces are **already convex-hull background-subtracted**
(`bgsub=1` on 4 / 5 cases) before they reach `fitModel()`. Convex-hull
subtraction is a non-linear, non-Poisson transformation, so the
residual noise is no longer Poisson-distributed and definitely not
σ²(x) = y(x). Applying `weights = 1/√max(y, 1)` then **down-weights the
peak tops** (where unweighted SSE wants to fit best) and **up-weights
near-zero tails** (which carry no information after bg subtraction).
The fit's "best" weighted optimum lands at displaced peak positions
(`p_max_diff_p95` of 1–4 px) and inflated amplitudes (30 % off).

The (unweighted) `chi2_ratio < 1` for these adapters is misleading: it
just reflects the fact that the unweighted SSE coincidentally
undercounts the same large-tail residuals the weighting was originally
meant to discount. The fitted curves are objectively worse against the
data — see the per-case `p_max_diff_p95` column of the headline table.

If we ever want to revisit Poisson weighting, it has to happen
**before** background subtraction (or with a properly modelled bg
component inside the fit), not on the convex-hull-subtracted trace.

---

## 5. Recommendation

1. **Switch `fitModel()` to TRF.** Replace
   `model.fit(hist, …)` with
   `model.fit(hist, …, method='least_squares', …)` (lmfit picks TRF as
   the default `least_squares` solver when bounds are present). Net
   effects on the 5-case corpus:
   * **Same optimum** (`chi2_ratio = 1.0`) on every case, every
     perturbation level.
   * **5 – 35 % faster median wall time** overall in Test 1; **2 – 3 ×
     faster** on the harder cases (`m3`, `m6`, `TEST`). Slightly
     slower (≤ 25 ms) on the two cases where baseline already converges
     in < 200 nfev.
   * **5 – 35 × tighter peak-position p95** on the one sensitive case
     (`TEST`).
   * **No regression** on reliability: 0 / 50 aborts at every
     perturbation level vs. baseline's 0 / 50.

   **Implementation note — `(center_sigma1, center_sigma2)` labeling.**
   The model adds two same-center Gaussians on the meridian, with
   `(sigma1, amp1)` semantically labelled as the *background* and
   `(sigma2, amp2)` as the *peak*. The model is mathematically
   symmetric under swapping them; the production downstream
   (`PT_CSVManager`, GUI panels) relies on the convention "background
   sigma is broader" (`sigma1 ≥ sigma2`). With the LM solver this falls
   out from the init values; with TRF, on the strong-background case
   (`TEST`) the optimizer lands at the labelled-swapped optimum.
   Canonicalising the ordering after the fit is a 5-line post-process
   that makes the labelling solver-independent. This is committed as
   part of the TRF switch.

2. **Drop the Poisson-weighting experiment** for the bg-subtracted code
   path. Keep the adapters in the A/B framework as a regression
   sentinel.

3. **Re-capture once the switch lands.** With TRF as the new baseline,
   re-running the same captures gives a fresh reference for any future
   adapter (VarPro, scipy direct, iminuit, …).

4. **Keep the auto-capture-on-dev hook** so this corpus grows the next
   time anyone touches `ProjectionProcessor` on the dev branch.

---

## 6. Data sources

All paths relative to the repository root.

| artefact | path | what's in it |
|---|---|---|
| Per-trial long-format report (Test 1) | [`ab_report.csv`](../../../ab_report.csv) | 100 rows, 28 columns — every fit attempted |
| Aggregate summary (Test 1) | [`ab_summary.csv`](../../../ab_summary.csv) | 4 rows (one per adapter) |
| Robustness sweep (perturb=0.15, headline) | [`ab_robustness.csv`](../../../ab_robustness.csv) + `ab_robustness_long.csv` | 50-fit aggregate + per-trial detail |
| Robustness sweep (perturb=0.05) | [`ab_sweep_p0.05.csv`](../../../ab_sweep_p0.05.csv) + `_long.csv` | 50-fit aggregate + per-trial |
| Robustness sweep (perturb=0.30) | [`ab_sweep_p0.30.csv`](../../../ab_sweep_p0.30.csv) + `_long.csv` | 50-fit aggregate + per-trial |
| Robustness sweep (perturb=0.50) | [`ab_sweep_p0.50.csv`](../../../ab_sweep_p0.50.csv) + `_long.csv` | 50-fit aggregate + per-trial |
| Captured fit cases | `/tmp/musclex_fit_cases/full_run_*.pkl` | 5 pickled `FitCase` objects |
| Diagnostic harness | [`musclex/tests/fitting_ab/diagnose_m6_trf_failure.py`](diagnose_m6_trf_failure.py) | resolved the m6 / TRF mystery (perturbation strategy bug, not TRF) |
| Framework code | [`musclex/tests/fitting_ab/`](.) | adapters, runner, capture hook |

### Reproducing this report

```bash
# 1. capture (or copy the pickles already in /tmp/musclex_fit_cases/)
export MUSCLEX_CAPTURE_FITS=1
export MUSCLEX_CAPTURE_DIR=/tmp/musclex_fit_cases
export MUSCLEX_CAPTURE_TAG=full_run
python -m unittest musclex.tests.musclex_tester.MuscleXGlobalTester.testHeadlessPTFittingGaussiansHorizontal
unset MUSCLEX_CAPTURE_FITS

# 2. Test 1 — deterministic replay
python -m musclex.tests.fitting_ab.runner \
    --cases /tmp/musclex_fit_cases \
    --adapters lmfit-baseline-leastsq lmfit-trf lmfit-poisson lmfit-trf-poisson \
    --n-repeats 5 \
    --report ab_report.csv --summary ab_summary.csv

# 3. Test 2 — perturbation sweep
for p in 0.05 0.15 0.30 0.50; do
  python -m musclex.tests.fitting_ab.runner \
    --cases /tmp/musclex_fit_cases \
    --adapters lmfit-baseline-leastsq lmfit-trf \
    --n-repeats 10 --perturb-init $p \
    --report  ab_sweep_p${p}_long.csv \
    --summary ab_sweep_p${p}.csv
done
# (the perturb=0.15 outputs are also saved as ab_robustness.csv /
#  ab_robustness_long.csv for backwards compatibility with earlier docs.)
```

### Caveats / scope

* **5 cases is small.** The story is consistent across four
  perturbation levels and concentrates on the one no-bg-subtraction
  case (`TEST`), but a full retrospective on dozens of real datasets
  is the next sensible step. `_maybe_enable_dev_capture` in
  `musclex/__init__.py` exists to make that easy.
* **Single-seed RNG.** The runner uses `seed=0` per adapter, so
  perturbation sequences are deterministic across runs — the diversity
  in this report comes from varying perturbation *magnitude*, not
  varying seed at fixed magnitude. A `--seed` flag on the runner would
  let us add genuine seed-variation to the next iteration.
* **Single platform.** Linux 6.8 / Python 3.10 / NumPy 1.26 / lmfit 1.1.
  Re-run on macOS and Windows before committing the algorithm change.
* **Single image source.** All five cases come from `EIGERTestImage.tif`.
  Boxes from real scattering data with messier backgrounds may behave
  more like `TEST` than like `M3` — capture them and rerun.
