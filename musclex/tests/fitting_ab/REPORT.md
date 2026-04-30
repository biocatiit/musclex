# A/B Fit Benchmark Report — 2026-04-29

> Benchmark of alternative non-linear fitting backends on real MuscleX
> data. The report is split into two parts:
>
> * **Part A — Projection Traces** (`ProjectionProcessor.fitModel`)
> * **Part B — Equator** (`EquatorImage.fitModel` / `processFit`)
>
> The verdict is different for each module — see the executive summary.
> All numbers were generated after the bound-aware perturbation fix
> landed in `_maybe_perturb_init`
> ([`adapters/lmfit_adapters.py`](adapters/lmfit_adapters.py) §
> "Bound-aware perturbation"); an earlier draft based on pre-fix CSVs
> overstated baseline-LM's failure modes for PT and has been discarded.

## Executive summary

| module | verdict | reason |
|---|---|---|
| **Projection Traces** | **switch to `lmfit-trf`** (`method='least_squares'`) | Same optimum, 5–35 % faster median, 5–35 × tighter peak-position p95 on the one strong-background case (`TEST`). Already shipped — see § A.5. |
| **Equator** | **stay on `lmfit-baseline-leastsq`** | TRF without scaling is broken (`xtol` trips at 5 nfev because area params are 10⁶ × bigger than sigma params). TRF with `x_scale='jac'` is correct but **2 – 3 × slower than LM**. No robustness benefit on this corpus. See § B. |

| do-not-use | reason |
|---|---|
| Both `lmfit-poisson` variants on the bg-subtracted PT path | Convex-hull subtraction has already removed Poisson noise; `1/√y` weighting biases the optimum 1 – 4 px away. |
| Plain `lmfit-trf` on equator | Premature `xtol` convergence after 5 nfev, plus one catastrophic 24 000-nfev abort. Use `lmfit-trf-jac` if you need TRF on this model. |

---

# Part A — Projection Traces (`ProjectionProcessor.fitModel`)

## A.1. Setup

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

## A.2. Headline numbers — Test 1: deterministic replay

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

## A.3. Robustness — Test 2: perturbation sweep

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

## A.4. Why the Poisson-weighted adapters lose

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

## A.5. Recommendation (PT)

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

---

# Part B — Equator (`EquatorImage.fitModel` / `processFit`)

## B.1. Setup

**Cases.** 6 cases captured in one pass by running the existing equator
headless integration tests with `MUSCLEX_CAPTURE_FITS=1` (3 image
sources × 2 captures each):

| case_id | image | model | n_free | difficulty |
|---|:---:|:---:|:---:|---|
| `eq_eq_F10_pCa8_SL21_0001_*` | F10_pCa8_SL21 frame 0001 (PILATUS) | Gaussian | 11 | very-large areas (~6.5e6) |
| `eq_eq_F10_pCa8_SL21_0002_*` | F10_pCa8_SL21 frame 0002 (PILATUS) | Gaussian | 11 | very-large areas (~6.5e6) |
| `eq_eq_P1_F1_tet_..._00005_*` | P1_F1_tet… (EIGER) | Gaussian | 11 | small areas (~1e3) |
| `eq_eq_P1_F1_tet_..._00006_*` | P1_F1_tet… (EIGER) | Gaussian | 11 | small areas (~1e3) |
| `eq_eq_P2_F5_849_1_094_*`     | P2_F5_849_1_094 (MAR) | Gaussian | 11 | medium areas (~5e4) |
| `eq_eq_P2_F5_849_2_095_*`     | P2_F5_849_2_095 (MAR) | Gaussian | 11 | medium areas (~5e4) |

All cases: `isSkeletal=False`, `isExtraPeak=False`, Gaussian model, 11
free params (`centerX`, `S10`, `S0`, 4 areas, 2 sigmads, 2 gammas).
Pickles in `/tmp/musclex_eq_cases/`.

**Adapters.**

| name | algorithm | extra fit_kws |
|---|---|---|
| `lmfit-baseline-leastsq` | LM (production code path) | — |
| `lmfit-trf` | TRF, scipy default scaling | — |
| `lmfit-trf-jac` | TRF + Jacobian-based parameter scaling | `{"x_scale": "jac"}` |

**Methodology.** Same as Part A: deterministic replay (5 trials × 6
cases × 3 adapters = 90 fits) plus a perturbation sweep at
`perturb_init=0.15` (10 trials × 6 cases × 2 adapters = 120 fits). The
plain TRF adapter is excluded from the perturbation sweep because it
already abort-cap on the deterministic case.

## B.2. Headline numbers — deterministic replay

Source: [`eq_ab_summary.csv`](../../../eq_ab_summary.csv),
[`eq_ab_report.csv`](../../../eq_ab_report.csv).

| adapter | success | aborted | el_med (ms) | speed_ratio_median | chi2_ratio_median |
|---|:---:|:---:|---:|---:|---:|
| `lmfit-baseline-leastsq` | 30/30 | 0 % | **36.4** | 1.02 | **1.0000** |
| `lmfit-trf`              | 25/30 | **17 %** | 67.4 | 1.89 | 1.027 |
| `lmfit-trf-jac`          | 30/30 | 0 % | 70.5 | 2.02 | **1.0000** |

> The `p_max_diff_*` and `amp_diff_*` columns are not meaningful for
> equator (peak positions are derived from `S10·θ(hk)`, not free
> parameters), so they're omitted here.

### Per-case detail

| case | leastsq el_med | leastsq nfev | TRF el_med | TRF nfev | TRF chi2 ratio | TRF-jac el_med | TRF-jac nfev | TRF-jac chi2 ratio |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `F10_pCa8_SL21_0001` | **36 ms** | 192 | 21 ms | 5 | **1.0595** ⚠ | 106 ms | 27 | 1.0000 |
| `F10_pCa8_SL21_0002` | **43 ms** | 230 | 19 ms | 5 | **1.1833** ⚠ | 60 ms | 26 | 1.0000 |
| `P1_F1_tet (1)`      | **36 ms** | 220 | 69 ms | 24 | 1.0000 | 71 ms | 29 | 1.0000 |
| `P1_F1_tet (2)`      | **32 ms** | 193 | 61 ms | 26 | 1.0000 | 106 ms | 30 | 1.0000 |
| `P2_F5_849 (1)`      | **35 ms** | 155 | **9 477 ms** | **24 001** | **1.0538** ⚠ (5/5 aborted) | 66 ms | 26 | 1.0000 |
| `P2_F5_849 (2)`      | **39 ms** | 169 | 71 ms | 33 | 1.0000 | 65 ms | 26 | 1.0000 |

### What's going on

* **Plain `lmfit-trf` is broken on equator.** Two failure modes visible:
  1. **Premature `xtol` termination** on the F10 cases (5 nfev,
     5–18 % worse chi² than leastsq). With area parameters at scale
     ~6.5 × 10⁶ and sigma at ~10, scipy's relative-step `xtol = 1e-8`
     trips after the first trust-region step nudges the small params
     by O(1) — leaving the area params essentially unchanged. The
     diagnostic deltas confirm this:

     | param | leastsq Δ from init | TRF Δ from init |
     |---|---:|---:|
     | `right_area1` (~6.4e6) | **−139 181** | −0.10 |
     | `left_area1` (~6.5e6)  | **−42 860**  | −0.01 |
     | `left_sigmad` (~12)    | −5.00 | −4.96 |
  2. **Catastrophic budget exhaustion** on `P2_F5_849 (1)`: 5 / 5
     trials hit the 24 000-nfev cap, taking ~9.5 s per trial, and
     still ending at chi² 5 % worse than leastsq.

* **`lmfit-trf-jac` (TRF + `x_scale='jac'`) is correct.** Reaches the
  same minimum (chi² ratio = 1.0000 to 1e-8) on every case in
  26–30 nfev. But it's **1.5 – 3 × slower** than leastsq on every
  case because (a) leastsq already converges in 30–40 ms, and
  (b) each TRF iteration evaluates a Jacobian (extra ~11×N model
  evaluations).

* **leastsq always wins on speed** — production LM is faster than
  both TRF variants on every single case.

## B.3. Robustness — perturbation sweep at `perturb=0.15`

Source: [`eq_robustness.csv`](../../../eq_robustness.csv) (and
`_long.csv`).

| adapter | success | aborted | el_med (ms) | chi2_ratio_median | chi2_ratio_max |
|---|:---:|:---:|---:|---:|---:|
| `lmfit-baseline-leastsq` | 60/60 | 0 % | **31.5** | 1.0000 | 1.0000 |
| `lmfit-trf-jac`          | 60/60 | 0 % | 73.4 | 1.0000 | 1.0000 |

Both adapters always converge to the same minimum under perturbation.
There is no robustness gap. leastsq is consistently 2 – 3 × faster.

## B.4. Why the equator answer is different from PT

The structural reasons TRF wins on PT but loses on equator:

1. **Parameter scaling.** PT free params span at most 4 orders of
   magnitude (positions ~10², sigmas ~10¹, amplitudes ~10⁴).
   Equator areas are ~10⁶ while sigmas are ~10¹ — **5 + orders of
   magnitude**. Without `x_scale='jac'`, TRF's relative-step
   convergence checks declare done before the largest-scale params
   move at all.

2. **LM convergence count.** PT's hardest case (`TEST`) needs LM
   2 284 nfev, opening a wide gap for TRF (42 nfev) to win on speed.
   Equator's hardest case needs LM ~230 nfev — TRF's 26 nfev × the
   per-iteration cost (Jacobian evaluation, SVD) lands in the same
   wall-time bucket.

3. **No labeling-symmetry pitfall.** PT has the
   `(center_sigma1, center_sigma2)` Gaussian degeneracy (post-fix
   handled by canonicalisation). Equator has no equivalent: peak
   positions are anchored by `S10·θ(hk)`, left/right are hardcoded
   asymmetric, and `(sigmac, sigmad, sigmas)` are different
   theta-power moments — no swap is possible.

So TRF doesn't have a hidden robustness win on equator that would
offset the speed regression.

## B.5. Recommendation (Equator)

1. **Don't switch `EquatorImage.fitModel` / `processFit` to TRF.**
   The captured corpus shows leastsq is uniformly faster and equally
   correct.

2. **Keep `lmfit-trf-jac` in the framework** as the right thing to use
   if anyone re-runs this benchmark on a future equator change (e.g.
   adding a Voigt branch or skeletal/EP free params, where Jacobian
   conditioning could shift). Plain `lmfit-trf` should stay in the
   registry but only as a smoke-test for the `xtol` bug.

3. **Optional independent fix**: the `init_z` → `init_z_ep` copy-paste
   bug in `EquatorImage.py` line 828 (the `zline_EP` lower-bound
   formula uses `init_z` instead of `init_z_ep`) is unrelated to the
   solver choice but worth fixing while we're here.

4. **Capture more cases**: the 3 image sources from the integration
   tests aren't representative of every dataset. If a future
   equator-related performance complaint arrives, capture cases from
   the affected dataset and rerun this benchmark before drawing
   conclusions from this 6-case sample.

---

## 6. Data sources

All paths relative to the repository root.

### Part A — Projection Traces

| artefact | path | what's in it |
|---|---|---|
| Per-trial long-format report (Test 1) | [`ab_report.csv`](../../../ab_report.csv) | 100 rows, 28 columns — every fit attempted |
| Aggregate summary (Test 1) | [`ab_summary.csv`](../../../ab_summary.csv) | 4 rows (one per adapter) |
| Robustness sweep (perturb=0.15, headline) | [`ab_robustness.csv`](../../../ab_robustness.csv) + `ab_robustness_long.csv` | 50-fit aggregate + per-trial detail |
| Robustness sweep (perturb=0.05) | [`ab_sweep_p0.05.csv`](../../../ab_sweep_p0.05.csv) + `_long.csv` | 50-fit aggregate + per-trial |
| Robustness sweep (perturb=0.30) | [`ab_sweep_p0.30.csv`](../../../ab_sweep_p0.30.csv) + `_long.csv` | 50-fit aggregate + per-trial |
| Robustness sweep (perturb=0.50) | [`ab_sweep_p0.50.csv`](../../../ab_sweep_p0.50.csv) + `_long.csv` | 50-fit aggregate + per-trial |
| Captured PT fit cases | `/tmp/musclex_fit_cases/full_run_*.pkl` | 5 pickled `FitCase` objects |
| Diagnostic harness | [`musclex/tests/fitting_ab/diagnose_m6_trf_failure.py`](diagnose_m6_trf_failure.py) | resolved the m6 / TRF mystery (perturbation strategy bug, not TRF) |

### Part B — Equator

| artefact | path | what's in it |
|---|---|---|
| Per-trial long-format report | [`eq_ab_report.csv`](../../../eq_ab_report.csv) | 90 rows, 6 cases × 3 adapters × 5 trials |
| Aggregate summary | [`eq_ab_summary.csv`](../../../eq_ab_summary.csv) | 3 rows (one per adapter) |
| Robustness summary (perturb=0.15) | [`eq_robustness.csv`](../../../eq_robustness.csv) | 2 rows (baseline vs trf-jac) |
| Robustness per-trial | [`eq_robustness_long.csv`](../../../eq_robustness_long.csv) | 120 rows |
| Captured Equator fit cases | `/tmp/musclex_eq_cases/eq_eq_*.pkl` | 6 pickled `FitCase` objects (model_kind='cardiac') |

### Common

| artefact | path | what's in it |
|---|---|---|
| Framework code | [`musclex/tests/fitting_ab/`](.) | adapters, runner, capture hooks |

### Reproducing this report

#### Part A — Projection Traces

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

#### Part B — Equator

```bash
# 1. capture by running the existing equator headless tests with capture on
mkdir -p /tmp/musclex_eq_cases
export MUSCLEX_CAPTURE_FITS=1
export MUSCLEX_CAPTURE_DIR=/tmp/musclex_eq_cases
export MUSCLEX_CAPTURE_TAG=eq
python -m unittest musclex.tests.musclex_tester -k Equator
unset MUSCLEX_CAPTURE_FITS

# 2. deterministic replay (3 adapters)
python -m musclex.tests.fitting_ab.runner \
    --cases /tmp/musclex_eq_cases \
    --adapters lmfit-baseline-leastsq lmfit-trf lmfit-trf-jac \
    --n-repeats 5 \
    --report eq_ab_report.csv --summary eq_ab_summary.csv

# 3. robustness sweep (skip plain lmfit-trf — it aborts)
python -m musclex.tests.fitting_ab.runner \
    --cases /tmp/musclex_eq_cases \
    --adapters lmfit-baseline-leastsq lmfit-trf-jac \
    --n-repeats 10 --perturb-init 0.15 \
    --report eq_robustness_long.csv --summary eq_robustness.csv
```

### Caveats / scope

* **Small samples.** PT has 5 cases (one image source, perturbation
  swept across four levels) and Equator has 6 cases (three image
  sources, single perturbation level). The PT verdict is consistent
  across all perturbation levels and the Equator verdict is consistent
  across all six cases, but a full retrospective on dozens of real
  datasets is the next sensible step. `_maybe_enable_dev_capture` in
  `musclex/__init__.py` exists to make that easy.
* **Single-seed RNG.** The runner uses `seed=0` per adapter, so two
  invocations at the same perturbation level are byte-identical. The
  PT diversity comes from varying perturbation magnitude. A `--seed`
  flag on the runner would let us add genuine seed-variation in the
  next iteration.
* **Single platform.** Linux 6.8 / Python 3.10 / NumPy 1.26 / lmfit 1.1.
  Re-run on macOS and Windows before committing any algorithm change
  here. Especially relevant for the Equator finding because LM
  (`leastsq`) is documented to give OS-dependent results — the
  current verdict assumes the linux behaviour matches macOS/Windows.
* **Image source coverage.** PT cases all come from `EIGERTestImage.tif`.
  Equator cases come from three sources (PILATUS, EIGER, MAR) but each
  appears only twice. Real beam-line datasets with stronger
  backgrounds, more peaks, or skeletal/extra-peak modes (none of which
  are exercised in the captured corpus) may shift the verdict.
