# Projection Processor (PT) Fitting Optimization — Index Report

This is a chronological index of the benchmark and optimization reports generated during the A/B testing and performance tuning of the `ProjectionProcessor.fitModel` (and partially `EquatorImage.fitModel`) routines.

### 1. [A/B Fit Benchmark Report (Initial Baseline)](REPORT.md)
* **File**: `REPORT.md`
* **Summary**: The initial benchmark comparing alternative non-linear fitting backends on real MuscleX data. It evaluated the original Levenberg-Marquardt (`leastsq`) against Trust-Region Reflective (`trf`), identifying that `trf` handles parameter bounds natively and provides tighter peak-position consistency under perturbed initialization without the distortion of LM's internal tan/arctan remapping.

### 2. [Tasks Report — Fitting Benchmark Metrics (PT + Equator)](TASKS_REPORT.md)
* **File**: `TASKS_REPORT.md`
* **Summary**: A deliverable focusing specifically on three core metrics across both Projection Traces and Equator:
  1. Mean ± standard deviation of fitting error (R² or chi-square based).
  2. Mean ± standard deviation of fitting time.
  3. Model parameter consistency (mean ± std) when subjected to 15% random initialization perturbations.

### 3. [Projection Traces Fitting Assessment Report (PT Only)](PT_TASKS_REPORT.md)
* **File**: `PT_TASKS_REPORT.md`
* **Summary**: A refined version of the Tasks Report restricted entirely to the Projection Traces (`ProjectionProcessor.py`) module. It includes the core metrics (time, error, consistency) and additional assessments like deterministic replay, robustness across perturbation levels, peak-position stability, and experiments with Poisson-weighted adapters.

### 4. [PT Hull-Range Array Slicing Report](PT_HULL_SLICE_REPORT.md)
* **File**: `PT_HULL_SLICE_REPORT.md`
* **Summary**: Evaluates a data-reduction optimization for `bgsub=1` (convex hull background subtraction) mode. By slicing the `x` and `y` arrays to only the active signal windows around the beam center (dropping padded zeros), it reduced the array size fed to the optimizer, providing a noticeable speedup without affecting the mathematical outcome of the fit.

### 5. [PT Model-Function Speedup Report (Initial L2 Numba/Cython)](PT_SPEEDUP_REPORT.md)
* **File**: `PT_SPEEDUP_REPORT.md`
* **Summary**: Investigates speeding up the core Gaussian/Voigt evaluation functions (`_gaussian_eval`). It compares baseline NumPy (L1) against simple Numba JIT (L2) and Cython approaches. The key finding was a bottleneck analysis: the model function itself only accounts for ~2% of the total fit time, meaning optimizing just the individual evaluations yields marginal end-to-end gains.

### 6. [PT Vectorised-Batch Report: L3 (NumPy) and L4 (Numba)](PT_VECTORISED_BATCH_REPORT.md)
* **File**: `PT_VECTORISED_BATCH_REPORT.md`
* **Summary**: A deeper optimization of the model function replacing loops over `_gaussian_eval` with batched evaluations. It compares L3 (a single vectorized NumPy `np.exp()` call) against L4 (a fused `@numba.njit` loop). It concludes that L3 provides a safe, modest speedup (~1.15–1.43× on the evaluation step), while L4 with `fastmath=True` introduces numerical instability and divergence for "standard" (non-GMM) models. L3 was subsequently deployed to production.

### 7. [PT Analytical Jacobian Report](PT_ANALYTIC_JAC_REPORT.md)
* **File**: `PT_ANALYTIC_JAC_REPORT.md`
* **Summary**: Tackles the actual 98% bottleneck: the optimizer's finite-difference Jacobian estimation overhead. By supplying an exact analytical Jacobian for Gaussian peaks and calling `scipy.optimize.least_squares` directly (bypassing `lmfit` wrapper overhead), it demonstrated massive speedups (median ~4-7×, peaking >40×). It established safety guardrails, concluding that the Analytical Jacobian is highly stable and performant for GMM models with ≥10 peaks, but should fall back to standard `lmfit` for edge cases or divergent scenarios (optimality > 50).