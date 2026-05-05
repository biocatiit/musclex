# Tasks Report — Fitting Benchmark Metrics

> Focused deliverable report covering exactly the three asks:
>
> 1. Repeat the fitting several times and compute **mean ± std of fitting error**.
> 2. Compute the **mean ± std of the fitting time**.
> 3. A separate table — **consistency** under small perturbations: **mean and std of the model parameters**.
>
> All numbers below are from real captured fit cases run through the
> A/B framework. Two MuscleX modules were instrumented:
> **Projection Traces** (`ProjectionProcessor.fitModel`) and
> **Equator** (`EquatorImage.fitModel` / `processFit`). Reproduction
> command at the end.

---

## Setup


|                            | Projection Traces                     | Equator                                    |
| -------------------------- | ------------------------------------- | ------------------------------------------ |
| captured cases             | 5 (from `EIGERTestImage.tif`)         | 6 (from PILATUS / EIGER / MAR test images) |
| adapters compared          | `lmfit-baseline-leastsq`, `lmfit-trf` | `lmfit-baseline-leastsq`, `lmfit-trf-jac`  |
| trials per (adapter, case) | 10                                    | 10                                         |
| perturbation magnitude     | 15 % bound-aware                      | 15 % bound-aware                           |
| total fits                 | 5 × 2 × 10 = **100**                  | 6 × 2 × 10 = **120**                       |


Each trial uses the captured initial values, perturbed by a
bound-aware random factor. Bounded params get additive perturbation
(`init + uniform(−p, +p) · (max−min)/2`); unbounded params get
multiplicative (`init · (1 ± p)`).

---

## Task ① — Mean ± std of fitting error

For every (adapter, case) the fit was repeated and the resulting fit
error aggregated. Two error metrics are reported:

- `**chi²_ratio`** = `chi² / chi²_baseline_at_capture`
— normalized so cases with very different intensity scales can be
averaged together. `1.0` = found the same minimum as baseline.
- `**r²**` = coefficient of determination — bounded `[0, 1]`,
directly comparable across cases.

### Headline (perturb = 15 %)


| module            | adapter                  | n_fits | success_rate | **chi²_ratio mean ± std** | **r² mean ± std** |
| ----------------- | ------------------------ | ------ | ------------ | ------------------------- | ----------------- |
| Projection Traces | `lmfit-baseline-leastsq` | 50     | 1.00         | **3.012 ± 7.609**         | **0.885 ± 0.271** |
| Projection Traces | `lmfit-trf`              | 50     | 1.00         | **1.006 ± 0.018**         | **0.955 ± 0.069** |
| Equator           | `lmfit-baseline-leastsq` | 60     | 1.00         | **1.000 ± 2.0e-9**        | **0.971 ± 0.018** |
| Equator           | `lmfit-trf-jac`          | 60     | 1.00         | **1.000 ± 8.3e-9**        | **0.971 ± 0.018** |


**Sources**:
`[ab_sweep_p0.15.csv](ab_sweep_p0.15.csv)` (PT, per-adapter aggregate)
· `[ab_sweep_p0.15_long.csv](ab_sweep_p0.15_long.csv)` (PT, per-trial)
· `[eq_robustness.csv](eq_robustness.csv)` (Equator, per-adapter aggregate)
· `[eq_robustness_long.csv](eq_robustness_long.csv)` (Equator, per-trial)
· `[ab_sweep_p0.15_per_case.csv](ab_sweep_p0.15_per_case.csv)` and
`[eq_robustness_per_case.csv](eq_robustness_per_case.csv)`
break the same numbers down by case if a per-case mean ± std is needed.

> **Why ratio not raw chi²?** Raw `chi²` varies by orders of magnitude
> across cases (it depends on intensity scale), so a global mean of raw
> chi² would be dominated by whichever case has the brightest data.
> Raw `chi²_mean / std` per case is available in the `_per_case.csv`
> files.

### Mapping to the GUI's "Fitting Error" field

Both modules display a `**Fitting Error`** number after each fit. In
both cases it is defined as `1 − R²`, so the `r²` column above maps
directly to whatever the GUI shows.

**Definition in code:**


| module            | code                                                                            | location                                     |
| ----------------- | ------------------------------------------------------------------------------- | -------------------------------------------- |
| Projection Traces | `result_dict['error'] = 1. - r2_score(hist, predicted)`                         | `musclex/modules/ProjectionProcessor.py:862` |
| Equator           | `fit_result['fiterror'] = 1. - r2_score(cardiacFit(**fit_result), histNdarray)` | `musclex/modules/EquatorImage.py:914, 1084`  |


**Conversion to GUI numbers:**

```
GUI Fitting Error mean = 1 − r²_mean
GUI Fitting Error std  =     r²_std        (1 − X has the same std as X)
```

**Headline expressed as the GUI sees it (perturb = 15 %):**


| module            | adapter                  | **Fitting Error mean ± std** (GUI form) | vs. typical threshold (0.2 on Equator) |
| ----------------- | ------------------------ | --------------------------------------- | -------------------------------------- |
| Projection Traces | `lmfit-baseline-leastsq` | **0.115 ± 0.271**                       | mean+std ≈ 0.39 — TEST case dominates  |
| Projection Traces | `lmfit-trf`              | **0.045 ± 0.069**                       | mean+std ≈ 0.11 — well-behaved         |
| Equator           | `lmfit-baseline-leastsq` | **≈ 0.029 ± 0.018**                     | well below threshold                   |
| Equator           | `lmfit-trf-jac`          | **≈ 0.029 ± 0.018**                     | well below threshold                   |


> **Equator subtlety — `r2_score` argument order.**
> The Equator code calls `r2_score(prediction, data)`, but
> `sklearn.metrics.r2_score`'s signature is `r2_score(y_true, y_pred)`,
> so the arguments are swapped. The numerator
> (`Σ(prediction − data)²`) is unaffected, but the denominator becomes
> `var(prediction)` instead of `var(data)`. For "good" fits these are
> nearly equal, so the GUI's `fiterror` ≈ `1 − r²` reported here. For
> badly diverging fits the two values can differ noticeably. The PT
> code uses the standard order, so PT is **exactly** `1 − r²` from
> this report, with no caveat.

---

## Task ② — Mean ± std of fitting time

Wall-clock seconds per fit, aggregated across the same trials.
Both `mean ± std` (requested) and `median ± IQR` (more robust against
GC / scheduler spikes that make wall-time right-skewed) are reported.

### Headline (perturb = 15 %)


| module            | adapter                  | n_fits | **time mean ± std (s)** | median (s) | IQR (s) |
| ----------------- | ------------------------ | ------ | ----------------------- | ---------- | ------- |
| Projection Traces | `lmfit-baseline-leastsq` | 50     | **0.106 ± 0.102**       | 0.032      | 0.174   |
| Projection Traces | `lmfit-trf`              | 50     | **0.062 ± 0.057**       | 0.035      | 0.046   |
| Equator           | `lmfit-baseline-leastsq` | 60     | **0.0312 ± 0.0056**     | 0.030      | 0.008   |
| Equator           | `lmfit-trf-jac`          | 60     | **0.0513 ± 0.0340**     | 0.037      | 0.015   |


**Sources**: same files as Task ①. Time and error mean ± std live
side-by-side in the per-adapter summary CSVs.

> **Why also IQR?** Wall-clock time is right-skewed (long tail from
> occasional GC / scheduler hiccups), so `std` overstates how jittery
> the adapter actually is. `IQR = P75 − P25` is the recommended
> dispersion estimator for ranking. Both are reported.

---

## Task ③ — Consistency table: per-parameter mean ± std under perturbation

A separate, dedicated table is produced for each module: one row per
**(adapter, case, parameter)** with the mean and std of the **fitted
parameter value** across the 10 perturbed trials. This is the
"per-parameter stability" view, distinct from the aggregated error /
time tables above.

Each row contains:


| column                                          | meaning                                                      |
| ----------------------------------------------- | ------------------------------------------------------------ |
| `n_trials`                                      | how many trials were aggregated                              |
| `ref_value`                                     | captured baseline value (frozen)                             |
| `cand_mean`, `cand_std`, `cand_min`, `cand_max` | distribution of the **fitted** value across perturbed trials |
| `diff_mean`, `diff_std`                         | mean / std of `(candidate − reference)`                      |
| `abs_diff_mean`, `abs_diff_max`                 | absolute deviation, mean and worst                           |
| `rel_diff_mean`, `rel_diff_std`                 | relative deviation `(candidate − reference) / reference`     |


Both `ref_value` and the candidate values are **canonicalised** before
aggregation (PT meridian Gaussian pair `(sigma1, sigma2)` is sorted,
peak positions `p_0..p_n` are reordered ascending) so a label
permutation at the optimum does not show up as fake parameter drift.

### Files


| module                           | rows | path                                                                        |
| -------------------------------- | ---- | --------------------------------------------------------------------------- |
| Projection Traces (perturb=0.15) | 110  | `[ab_sweep_p0.15_consistency.csv](ab_sweep_p0.15_consistency.csv)` |
| Projection Traces (perturb=0.05) | 110  | `[ab_sweep_p0.05_consistency.csv](ab_sweep_p0.05_consistency.csv)` |
| Projection Traces (perturb=0.30) | 110  | `[ab_sweep_p0.30_consistency.csv](ab_sweep_p0.30_consistency.csv)` |
| Projection Traces (perturb=0.50) | 110  | `[ab_sweep_p0.50_consistency.csv](ab_sweep_p0.50_consistency.csv)` |
| Equator (perturb=0.15)           | 132  | `[eq_robustness_consistency.csv](eq_robustness_consistency.csv)`   |


110 rows = 5 cases × (variable free params per case, summing to 55) × 2 adapters.
132 rows = 6 cases × 11 free params × 2 adapters.

### Headline excerpt — most-unstable parameters

For each adapter, the 5 parameters with the highest CV (`cand_std / |ref|`). Source:
`[ab_sweep_p0.15_consistency.csv](ab_sweep_p0.15_consistency.csv)`
and `[eq_robustness_consistency.csv](eq_robustness_consistency.csv)`.

#### Projection Traces — `lmfit-baseline-leastsq`


| case | param               | ref   | cand_mean | cand_std  | CV (%) |
| ---- | ------------------- | ----- | --------- | --------- | ------ |
| TEST | `center_sigma1`     | 29.66 | 333.7     | **264.3** | 891    |
| TEST | `center_amplitude2` | 751.5 | 3 111     | **3 160** | 421    |
| TEST | `center_sigma2`     | 13.82 | 43.20     | **51.09** | 370    |
| TEST | `center_amplitude1` | 1 931 | 6 139     | **6 724** | 348    |
| TEST | `bg_sigma`          | 340.4 | 531.7     | **451.5** | 133    |


#### Projection Traces — `lmfit-trf`


| case | param               | ref    | cand_mean | cand_std  | CV (%) |
| ---- | ------------------- | ------ | --------- | --------- | ------ |
| TEST | `center_sigma1`     | 29.66  | 196.6     | **176.0** | 593    |
| TEST | `center_amplitude2` | 751.5  | 1 651     | **947.7** | 126    |
| TEST | `center_amplitude1` | 1 931  | 3 647     | **2 326** | 120    |
| TEST | `center_sigma2`     | 13.82  | 18.31     | **4.73**  | 34     |
| TEST | `bg_amplitude`      | 16 107 | 14 097    | **2 575** | 16     |


#### Equator — `lmfit-baseline-leastsq`


| case          | param | ref     | cand_mean | cand_std   | CV (%) |
| ------------- | ----- | ------- | --------- | ---------- | ------ |
| P1_F1_tet (1) | `S0`  | 8.8e-4  | 4.5e-5    | **9.5e-4** | 109    |
| P1_F1_tet (2) | `S0`  | -8.4e-4 | -2.4e-4   | **8.2e-4** | 99     |
| F10_…0002     | `S0`  | 9.1e-4  | 1.4e-4    | **8.8e-4** | 97     |
| P2_F5_849 (1) | `S0`  | -9.3e-4 | 2.8e-4    | **8.7e-4** | 93     |
| P2_F5_849 (2) | `S0`  | -8.0e-4 | 8.1e-5    | **7.4e-4** | 93     |


#### Equator — `lmfit-trf-jac`


| case          | param         | ref     | cand_mean | cand_std   | CV (%) |
| ------------- | ------------- | ------- | --------- | ---------- | ------ |
| P1_F1_tet (2) | `S0`          | -8.4e-4 | 1.5e-5    | **6.7e-4** | 81     |
| F10_…0002     | `S0`          | 9.1e-4  | -6.0e-5   | **3.8e-4** | 42     |
| F10_…0001     | `S0`          | -1.0e-3 | 5.1e-5    | **3.5e-4** | 35     |
| P1_F1_tet (1) | `S0`          | 8.8e-4  | -3.6e-5   | **2.5e-4** | 28     |
| P1_F1_tet (1) | `right_gamma` | 9.61    | 10.26     | **2.47**   | 26     |


> **Reading the consistency table**: `cand_std` is the literal
> standard deviation of the fitted parameter value across the 10
> trials. **Every parameter not shown** has `cand_std / |ref| < 1e-4`
> (sub-noise stability). The full 110- and 132-row tables in the CSVs
> have one row per fitted parameter for every (adapter, case).

---

## Reproduction

```bash
# requires captured fit cases (already present in /tmp/musclex_fit_cases
# and /tmp/musclex_eq_cases on this host; see REPORT.md §6 for capture
# instructions)

# Projection Traces — perturb = 0.15 (10 trials × 5 cases × 2 adapters = 100 fits)
python -m musclex.tests.fitting_ab.runner \
    --cases /tmp/musclex_fit_cases \
    --adapters lmfit-baseline-leastsq lmfit-trf \
    --n-repeats 10 --perturb-init 0.15 \
    --report      ab_sweep_p0.15_long.csv \
    --summary     ab_sweep_p0.15.csv \
    --per-case    ab_sweep_p0.15_per_case.csv \
    --consistency ab_sweep_p0.15_consistency.csv

# Equator — perturb = 0.15 (10 trials × 6 cases × 2 adapters = 120 fits)
python -m musclex.tests.fitting_ab.runner \
    --cases /tmp/musclex_eq_cases \
    --adapters lmfit-baseline-leastsq lmfit-trf-jac \
    --n-repeats 10 --perturb-init 0.15 \
    --report      eq_robustness_long.csv \
    --summary     eq_robustness.csv \
    --per-case    eq_robustness_per_case.csv \
    --consistency eq_robustness_consistency.csv
```

Each run takes ~30 seconds. Per-perturbation-level outputs at
`p ∈ {0.05, 0.15, 0.30, 0.50}` are also already on disk for PT — see
`[REPORT.md](REPORT.md)` §6 for the full sweep and the four-question
metrics framework that the broader analysis uses.

---

## Files produced for these three tasks


| task                             | file                                                                        | shape                                                                                                  |
| -------------------------------- | --------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| ① + ② (PT)                       | `[ab_sweep_p0.15.csv](ab_sweep_p0.15.csv)`                         | 2 rows (one per adapter), with `chi2_ratio_mean/std`, `r2_mean/std`, `elapsed_mean/std`, `elapsed_iqr` |
| ① + ② (Equator)                  | `[eq_robustness.csv](eq_robustness.csv)`                           | 2 rows, same columns                                                                                   |
| ① + ② per-trial detail (PT)      | `[ab_sweep_p0.15_long.csv](ab_sweep_p0.15_long.csv)`               | 100 rows                                                                                               |
| ① + ② per-trial detail (Equator) | `[eq_robustness_long.csv](eq_robustness_long.csv)`                 | 120 rows                                                                                               |
| ① + ② per-case detail (PT)       | `[ab_sweep_p0.15_per_case.csv](ab_sweep_p0.15_per_case.csv)`       | 10 rows (5 cases × 2 adapters)                                                                         |
| ① + ② per-case detail (Equator)  | `[eq_robustness_per_case.csv](eq_robustness_per_case.csv)`         | 12 rows (6 cases × 2 adapters)                                                                         |
| ③ consistency (PT)               | `[ab_sweep_p0.15_consistency.csv](ab_sweep_p0.15_consistency.csv)` | 110 rows (one per (adapter, case, free param))                                                         |
| ③ consistency (Equator)          | `[eq_robustness_consistency.csv](eq_robustness_consistency.csv)`   | 132 rows                                                                                               |


