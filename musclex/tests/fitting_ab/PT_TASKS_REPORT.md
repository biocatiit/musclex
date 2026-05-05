# Projection Traces Fitting Assessment Report

> PT-only report for `ProjectionProcessor.fitModel`.
>
> This report is ordered exactly as requested:
>
> 1. **Fitting assessment**
>    - repeat fitting several times and compute mean plus standard deviation of fitting error;
>    - compute mean plus standard deviation of fitting time;
>    - separate consistency table: small perturbations, mean plus standard deviation of model parameters.
> 2. **Other assessment**
>    - deterministic replay;
>    - robustness across perturbation levels;
>    - peak-position stability;
>    - Poisson-weighted adapters;
>    - final recommendation.

---

## Dataset And Test Setup

| item | value |
|---|---|
| module | Projection Traces, `ProjectionProcessor.fitModel` |
| captured cases | 5 |
| source image | `EIGERTestImage.tif` |
| fit case directory | `/tmp/musclex_fit_cases/` |
| main adapters | `lmfit-baseline-leastsq`, `lmfit-trf` |
| deterministic adapters | `lmfit-baseline-leastsq`, `lmfit-trf`, `lmfit-poisson`, `lmfit-trf-poisson` |
| perturbation levels | `0.05`, `0.15`, `0.30`, `0.50` |
| trials per `(adapter, case)` in perturbation runs | 10 |

Captured PT cases:

| case | box | model | n_peaks | n_free | note |
|---|---:|---:|---:|---:|---|
| `full_run_M3_00001_6ab33e` | M3 | gmm | 4 | 9 | medium |
| `full_run_M6_00002_952a4b` | M6 | gmm | 8 | 17 | most peaks |
| `full_run_m3_00003_dc8ba7` | m3 | gmm | 2 | 5 | low |
| `full_run_m6_00004_407a65` | m6 | gmm | 6 | 13 | medium-high |
| `full_run_TEST_00005_861239` | TEST | gmm | 2 | 11 | strong background, no subtraction |

---

# Fitting Assessment

## Task 1 — Mean Plus Standard Deviation Of Fitting Error

### What "Fitting Error" Means In PT

The PT GUI reports:

```text
Fitting Error = 1 - R²
```

The PT processor computes it directly:

```text
result_dict['error'] = 1. - r2_score(hist, predicted)
```

So in this report:

```text
Fitting Error mean = 1 - r2_mean
Fitting Error std  =     r2_std
```

`chi²_ratio` is also reported because it is the optimizer-side error:

```text
chi²_ratio = chi²_candidate / chi²_reference
```

`chi²_ratio = 1.0` means the adapter found the same minimum as the captured baseline. `Fitting Error` is the GUI-facing quality number.

### Main Result: Perturbation = 15%

Source: [`ab_sweep_p0.15.csv`](ab_sweep_p0.15.csv).

| adapter | n_fits | success | chi²_ratio mean ± std | R² mean ± std | GUI Fitting Error mean ± std |
|---|---:|---:|---:|---:|---:|
| `lmfit-baseline-leastsq` | 50 | 100% | 3.012 ± 7.609 | 0.885 ± 0.271 | **0.115 ± 0.271** |
| `lmfit-trf` | 50 | 100% | **1.006 ± 0.018** | **0.955 ± 0.069** | **0.045 ± 0.069** |

Interpretation:

- `lmfit-trf` has a much tighter fitting-error distribution under perturbed starts.
- Baseline's error spread is dominated by the `TEST` case.
- Both adapters have median `chi²_ratio = 1.0`, but the mean/std show the tail: baseline sometimes lands in a worse basin under perturbation.

### All Perturbation Levels

Sources:
[`ab_sweep_p0.05.csv`](ab_sweep_p0.05.csv),
[`ab_sweep_p0.15.csv`](ab_sweep_p0.15.csv),
[`ab_sweep_p0.30.csv`](ab_sweep_p0.30.csv),
[`ab_sweep_p0.50.csv`](ab_sweep_p0.50.csv).

| perturb | adapter | n_fits | chi²_ratio mean ± std | R² mean ± std | GUI Fitting Error mean ± std |
|---:|---|---:|---:|---:|---:|
| 0.05 | `lmfit-baseline-leastsq` | 50 | 2.243 ± 6.149 | 0.912 ± 0.223 | 0.088 ± 0.223 |
| 0.05 | `lmfit-trf` | 50 | **1.001 ± 0.009** | **0.955 ± 0.069** | **0.045 ± 0.069** |
| 0.15 | `lmfit-baseline-leastsq` | 50 | 3.012 ± 7.609 | 0.885 ± 0.271 | 0.115 ± 0.271 |
| 0.15 | `lmfit-trf` | 50 | **1.006 ± 0.018** | **0.955 ± 0.069** | **0.045 ± 0.069** |
| 0.30 | `lmfit-baseline-leastsq` | 50 | 1.919 ± 3.476 | 0.923 ± 0.137 | 0.077 ± 0.137 |
| 0.30 | `lmfit-trf` | 50 | **1.011 ± 0.024** | **0.955 ± 0.069** | **0.045 ± 0.069** |
| 0.50 | `lmfit-baseline-leastsq` | 50 | 2.486 ± 4.617 | 0.904 ± 0.172 | 0.096 ± 0.172 |
| 0.50 | `lmfit-trf` | 50 | **1.011 ± 0.024** | **0.955 ± 0.069** | **0.045 ± 0.069** |

Key point:

- Across every perturbation level, TRF keeps `chi²_ratio` close to 1.0 with small std.
- Baseline keeps a median of 1.0 but has a wide tail; mean and std expose that tail.

### Per-Case Detail At Perturbation = 15%

Source: [`ab_sweep_p0.15_per_case.csv`](ab_sweep_p0.15_per_case.csv).

| adapter | case | chi²_ratio mean ± std | R² mean ± std | GUI Fitting Error mean ± std |
|---|---|---:|---:|---:|
| baseline | M3 | 1.000 ± 6.8e-11 | 0.9978 ± 1.5e-13 | 0.0022 ± 1.5e-13 |
| baseline | M6 | 1.000 ± 1.6e-9 | 0.9960 ± 6.5e-12 | 0.0040 ± 6.5e-12 |
| baseline | TEST | **11.058 ± 15.010** | **0.615 ± 0.522** | **0.385 ± 0.522** |
| baseline | m3 | 1.000 ± 5.0e-9 | 0.8209 ± 8.9e-10 | 0.1791 ± 8.9e-10 |
| baseline | m6 | 1.000 ± 9.4e-9 | 0.9967 ± 3.1e-11 | 0.0033 ± 3.1e-11 |
| TRF | M3 | 1.000 ± 4.0e-11 | 0.9978 ± 9.0e-14 | 0.0022 ± 9.0e-14 |
| TRF | M6 | 1.000 ± 1.1e-8 | 0.9960 ± 4.5e-11 | 0.0040 ± 4.5e-11 |
| TRF | TEST | **1.030 ± 0.032** | **0.964 ± 0.001** | **0.036 ± 0.001** |
| TRF | m3 | 1.000 ± 6.3e-11 | 0.8209 ± 1.1e-11 | 0.1791 ± 1.1e-11 |
| TRF | m6 | 1.000 ± 7.3e-11 | 0.9967 ± 2.4e-13 | 0.0033 ± 2.4e-13 |

The only hard case is `TEST`. TRF keeps that case's GUI fitting error near 0.036, while baseline jumps to 0.385 on average with high variance.

---

## Task 2 — Mean Plus Standard Deviation Of Fitting Time

Times are wall-clock seconds per fit. `mean ± std` is reported because it was requested. `median` and `IQR` are also included because wall-clock timing is right-skewed by scheduler and GC noise.

### Main Result: Perturbation = 15%

Source: [`ab_sweep_p0.15.csv`](ab_sweep_p0.15.csv).

| adapter | n_fits | time mean ± std (s) | median (s) | IQR (s) |
|---|---:|---:|---:|---:|
| `lmfit-baseline-leastsq` | 50 | 0.106 ± 0.102 | 0.032 | 0.174 |
| `lmfit-trf` | 50 | **0.062 ± 0.057** | 0.035 | **0.046** |

Interpretation:

- TRF is faster on average at the 15% perturbation level.
- TRF also has a much smaller IQR, so its runtime is less jittery.

### All Perturbation Levels

| perturb | adapter | time mean ± std (s) | median (s) | IQR (s) |
|---:|---|---:|---:|---:|
| 0.05 | `lmfit-baseline-leastsq` | 0.164 ± 0.344 | 0.121 | 0.160 |
| 0.05 | `lmfit-trf` | **0.059 ± 0.050** | **0.038** | **0.043** |
| 0.15 | `lmfit-baseline-leastsq` | 0.106 ± 0.102 | **0.032** | 0.174 |
| 0.15 | `lmfit-trf` | **0.062 ± 0.057** | 0.035 | **0.046** |
| 0.30 | `lmfit-baseline-leastsq` | 0.096 ± 0.091 | 0.042 | 0.144 |
| 0.30 | `lmfit-trf` | **0.060 ± 0.055** | **0.032** | **0.053** |
| 0.50 | `lmfit-baseline-leastsq` | 0.111 ± 0.154 | 0.041 | 0.152 |
| 0.50 | `lmfit-trf` | **0.079 ± 0.147** | **0.032** | **0.043** |

### Per-Case Time Detail At Perturbation = 15%

Source: [`ab_sweep_p0.15_per_case.csv`](ab_sweep_p0.15_per_case.csv).

| adapter | case | time mean ± std (s) |
|---|---|---:|
| baseline | M3 | 0.028 ± 0.004 |
| baseline | M6 | 0.205 ± 0.022 |
| baseline | TEST | 0.062 ± 0.107 |
| baseline | m3 | 0.023 ± 0.005 |
| baseline | m6 | 0.211 ± 0.069 |
| TRF | M3 | 0.027 ± 0.002 |
| TRF | M6 | 0.166 ± 0.024 |
| TRF | TEST | 0.036 ± 0.007 |
| TRF | m3 | 0.012 ± 0.000 |
| TRF | m6 | 0.068 ± 0.005 |

TRF is faster on the hard cases (`TEST`, `m3`, `m6`) and comparable on `M3` / `M6`.

---

## Task 3 — Consistency Table: Parameter Mean Plus Standard Deviation Under Small Perturbations

The consistency table is separate from the error/time summaries. It has one row per:

```text
(adapter, case_id, param_name)
```

For each fitted parameter it reports:

| column | meaning |
|---|---|
| `n_trials` | number of perturbed trials |
| `ref_value` | captured baseline reference |
| `cand_mean` | mean fitted value across trials |
| `cand_std` | std of fitted value across trials |
| `cand_min`, `cand_max` | min/max fitted values |
| `diff_mean`, `diff_std` | mean/std of `candidate - reference` |
| `abs_diff_mean`, `abs_diff_max` | absolute deviation from reference |
| `rel_diff_mean`, `rel_diff_std` | relative deviation from reference |

Canonicalization is applied before aggregation:

- meridian Gaussian pair `(center_sigma1, center_sigma2)` is sorted by the convention `sigma1 >= sigma2`;
- free peak positions `p_0..p_n` are sorted by position so a label swap does not look like parameter drift.

### Consistency Files

| perturb | path | rows |
|---:|---|---:|
| 0.05 | [`ab_sweep_p0.05_consistency.csv`](ab_sweep_p0.05_consistency.csv) | 110 |
| 0.15 | [`ab_sweep_p0.15_consistency.csv`](ab_sweep_p0.15_consistency.csv) | 110 |
| 0.30 | [`ab_sweep_p0.30_consistency.csv`](ab_sweep_p0.30_consistency.csv) | 110 |
| 0.50 | [`ab_sweep_p0.50_consistency.csv`](ab_sweep_p0.50_consistency.csv) | 110 |

110 rows = 2 adapters × 5 cases × all free parameters in each case.

### Headline: Most Unstable Parameters At Perturbation = 15%

Source: [`ab_sweep_p0.15_consistency.csv`](ab_sweep_p0.15_consistency.csv).

#### `lmfit-baseline-leastsq`

| case | param | n_trials | ref | cand_mean | cand_std | CV (%) | abs_diff_max |
|---|---|---:|---:|---:|---:|---:|---:|
| TEST | `center_sigma1` | 10 | 29.66 | 333.70 | **264.29** | 891 | 813.44 |
| TEST | `center_amplitude2` | 10 | 751.50 | 3,110.96 | **3,160.33** | 421 | 8,839.90 |
| TEST | `center_sigma2` | 10 | 13.82 | 43.20 | **51.09** | 370 | 135.09 |
| TEST | `center_amplitude1` | 10 | 1,931.37 | 6,139.15 | **6,723.65** | 348 | 14,176.01 |
| TEST | `bg_sigma` | 10 | 340.39 | 531.67 | **451.54** | 133 | 875.92 |

#### `lmfit-trf`

| case | param | n_trials | ref | cand_mean | cand_std | CV (%) | abs_diff_max |
|---|---|---:|---:|---:|---:|---:|---:|
| TEST | `center_sigma1` | 10 | 29.66 | 196.62 | **176.00** | 593 | 334.02 |
| TEST | `center_amplitude2` | 10 | 751.50 | 1,650.52 | **947.70** | 126 | 1,798.09 |
| TEST | `center_amplitude1` | 10 | 1,931.37 | 3,646.90 | **2,325.52** | 120 | 5,621.81 |
| TEST | `center_sigma2` | 10 | 13.82 | 18.31 | **4.73** | 34 | 8.98 |
| TEST | `bg_amplitude` | 10 | 16,107.45 | 14,096.61 | **2,575.05** | 16 | 6,212.45 |

### Peak Position Consistency At Perturbation = 15%

Peak position parameters (`p_i`) are the main physical outputs. Their standard deviations are much smaller under TRF on the hard `TEST` case.

Source: [`ab_sweep_p0.15_consistency.csv`](ab_sweep_p0.15_consistency.csv).

| adapter | case | param | ref | cand_mean | cand_std | abs_diff_max |
|---|---|---|---:|---:|---:|---:|
| baseline | TEST | `p_0` | -146.71 | -146.14 | **0.457** | 1.331 |
| baseline | TEST | `p_1` | 144.75 | 145.00 | **0.511** | 1.249 |
| TRF | TEST | `p_0` | -146.71 | -146.65 | **0.090** | 0.142 |
| TRF | TEST | `p_1` | 144.75 | 144.66 | **0.088** | 0.168 |

Interpretation:

- Baseline peak positions wobble by about 0.46–0.51 px std on `TEST`.
- TRF reduces that to about 0.09 px std.
- This is the clearest per-parameter evidence that TRF is more stable for PT under perturbed starts.

---

# Other Assessment

## Deterministic Replay, No Perturbation

This run repeats the same captured init values 5 times per `(adapter, case)`.
It mostly measures timing and confirms whether each adapter reproduces the reference solution.

Source:
[`ab_summary.csv`](ab_summary.csv),
[`ab_report.csv`](ab_report.csv).

| adapter | n_fits | success | aborted | time mean ± std (s) | median (s) | chi²_ratio median | p_max_diff_p95 (px) | amp_diff_median |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `lmfit-baseline-leastsq` | 25 | 100% | 0% | 0.144 ± 0.088 | 0.152 | 1.000 | 0.000 | 0.000 |
| `lmfit-trf` | 25 | 100% | 0% | **0.061 ± 0.046** | **0.058** | 1.000 | 0.030 | 0.000006 |
| `lmfit-trf-poisson` | 25 | 100% | 0% | 0.068 ± 0.047 | 0.038 | 0.0109 | 1.131 | 0.303 |
| `lmfit-poisson` | 25 | 80% | 20% | 1.849 ± 3.145 | 0.201 | 0.0221 | 3.898 | 0.303 |

Interpretation:

- TRF reaches the same optimum as baseline and is faster in deterministic replay.
- Poisson-weighted variants do not preserve the same physical solution; peak positions drift by 1–4 px.
- `lmfit-poisson` also aborts in 20% of deterministic replay fits.

## Robustness Across Perturbation Levels

Peak-position p95 is the tail-risk metric: it asks how far the worst 5% of runs drift in peak position.

| perturb | adapter | success | p_max_diff_median (px) | p_max_diff_p95 (px) |
|---:|---|---:|---:|---:|
| 0.05 | baseline | 100% | 0.00004 | 0.926 |
| 0.05 | TRF | 100% | 0.00009 | **0.030** |
| 0.15 | baseline | 100% | 0.00005 | 1.002 |
| 0.15 | TRF | 100% | 0.00009 | **0.168** |
| 0.30 | baseline | 100% | 0.00010 | 1.146 |
| 0.30 | TRF | 100% | 0.00009 | **0.168** |
| 0.50 | baseline | 100% | 0.00010 | 2.398 |
| 0.50 | TRF | 100% | 0.00009 | **0.168** |

TRF keeps the peak-position p95 bounded below 0.17 px even at 50% perturbation. Baseline grows to 2.40 px at the highest perturbation level.

## Case-Level Finding

The robustness gap is concentrated in one case:

| case | finding |
|---|---|
| `M3` | both adapters stable |
| `M6` | both adapters stable; TRF slightly faster in perturbation runs |
| `m3` | both adapters stable; TRF much faster |
| `m6` | both adapters stable; TRF faster |
| `TEST` | baseline unstable under perturbation; TRF substantially more stable |

The `TEST` case has strong background and no background subtraction, which exposes the meridian Gaussian degeneracy and makes baseline LM sensitive to the starting point.

## Why Poisson-Weighted Adapters Are Not Recommended

The Poisson-weighted adapters apply `weights = 1 / sqrt(max(y, 1))`.
That is inappropriate here because most captured PT traces are already convex-hull background-subtracted.
After background subtraction, the residual noise is no longer Poisson-distributed.

Observed result:

- `lmfit-trf-poisson`: success 100%, but `p_max_diff_p95 = 1.131 px`, amplitude median diff 30%.
- `lmfit-poisson`: success 80%, abort rate 20%, `p_max_diff_p95 = 3.898 px`, amplitude median diff 30%.

Even when the unweighted `chi²_ratio` looks low, these adapters are fitting a different objective and moving the physical peak positions. They should not be used for the current PT background-subtracted path.

## Final PT Recommendation

For `ProjectionProcessor.fitModel`, prefer:

```text
lmfit-trf  =>  lmfit Model.fit(..., method='least_squares')
```

Reason:

- same optimum as production baseline in deterministic replay;
- lower mean and std of fitting error under perturbed starts;
- lower mean fitting time and smaller timing IQR in perturbation tests;
- better per-parameter consistency, especially peak positions on `TEST`;
- peak-position p95 remains bounded across perturbation levels;
- no reliability regression: 100% success, 0% aborted in all PT perturbation runs.

Keep `lmfit-baseline-leastsq` as the reference adapter in the A/B framework and keep Poisson adapters only as regression sentinels, not as production candidates for the current bg-subtracted PT path.

---

## Reproduction Commands

```bash
# deterministic replay
python -m musclex.tests.fitting_ab.runner \
    --cases /tmp/musclex_fit_cases \
    --adapters lmfit-baseline-leastsq lmfit-trf lmfit-poisson lmfit-trf-poisson \
    --n-repeats 5 \
    --report ab_report.csv \
    --summary ab_summary.csv \
    --per-case ab_per_case.csv

# perturbation sweep and consistency tables
for p in 0.05 0.15 0.30 0.50; do
  python -m musclex.tests.fitting_ab.runner \
      --cases /tmp/musclex_fit_cases \
      --adapters lmfit-baseline-leastsq lmfit-trf \
      --n-repeats 10 --perturb-init $p \
      --report      ab_sweep_p${p}_long.csv \
      --summary     ab_sweep_p${p}.csv \
      --per-case    ab_sweep_p${p}_per_case.csv \
      --consistency ab_sweep_p${p}_consistency.csv
done
```

## Data Files Used

| category | files |
|---|---|
| deterministic replay | [`ab_report.csv`](ab_report.csv), [`ab_summary.csv`](ab_summary.csv), [`ab_per_case.csv`](ab_per_case.csv) |
| perturbation p=0.05 | [`ab_sweep_p0.05.csv`](ab_sweep_p0.05.csv), [`ab_sweep_p0.05_long.csv`](ab_sweep_p0.05_long.csv), [`ab_sweep_p0.05_per_case.csv`](ab_sweep_p0.05_per_case.csv), [`ab_sweep_p0.05_consistency.csv`](ab_sweep_p0.05_consistency.csv) |
| perturbation p=0.15 | [`ab_sweep_p0.15.csv`](ab_sweep_p0.15.csv), [`ab_sweep_p0.15_long.csv`](ab_sweep_p0.15_long.csv), [`ab_sweep_p0.15_per_case.csv`](ab_sweep_p0.15_per_case.csv), [`ab_sweep_p0.15_consistency.csv`](ab_sweep_p0.15_consistency.csv) |
| perturbation p=0.30 | [`ab_sweep_p0.30.csv`](ab_sweep_p0.30.csv), [`ab_sweep_p0.30_long.csv`](ab_sweep_p0.30_long.csv), [`ab_sweep_p0.30_per_case.csv`](ab_sweep_p0.30_per_case.csv), [`ab_sweep_p0.30_consistency.csv`](ab_sweep_p0.30_consistency.csv) |
| perturbation p=0.50 | [`ab_sweep_p0.50.csv`](ab_sweep_p0.50.csv), [`ab_sweep_p0.50_long.csv`](ab_sweep_p0.50_long.csv), [`ab_sweep_p0.50_per_case.csv`](ab_sweep_p0.50_per_case.csv), [`ab_sweep_p0.50_consistency.csv`](ab_sweep_p0.50_consistency.csv) |
