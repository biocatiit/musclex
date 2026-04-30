# Fitting A/B Test Framework

Compare alternative non-linear fitting backends (lmfit/leastsq, lmfit/TRF,
scipy, VarPro, iminuit, ...) against the current production baseline on
**real fit inputs** captured during normal MuscleX runs.

The framework has three pieces:

```
[ProjectionProcessor.fitModel]  capture/  →  fit_cases/*.pkl  →  runner  →  report.csv
       (instrumented)              ↑                              ↑
                                   |                              |
                              env-flag hook              FitterAdapter strategies
```

A "case" = one self-contained replayable fit fixture (data, init, bounds,
fixed params, baseline reference result). Adapters all implement the same
`FitterAdapter` interface, so swapping backends is one line of config.

---

## Quick start (60 seconds)

```bash
# 1) capture some fits
export MUSCLEX_CAPTURE_FITS=1
export MUSCLEX_CAPTURE_DIR=~/captures/today
python -m unittest musclex.tests.musclex_tester.MuscleXGlobalTester.testHeadlessPTFittingGaussiansHorizontal
unset MUSCLEX_CAPTURE_FITS

# 2) run A/B against the captured cases
python -m musclex.tests.fitting_ab.runner \
    --cases ~/captures/today \
    --adapters lmfit-baseline-leastsq lmfit-trf \
    --n-repeats 5 \
    --report ab.csv \
    --summary ab_summary.csv
```

Look at `ab_summary.csv` for the per-adapter table (median elapsed,
chi² ratio, peak position p95, etc).

---

## 1. Capturing fit cases

Every `ProjectionProcessor.fitModel()` call is hooked. The hook is a
no-op (one `os.environ` lookup) when capture is off, so it never affects
production runs.

A captured `.pkl` contains: histogram (`y`), independent variables,
free-param init/bounds, fixed params, and a frozen reference of the
baseline `model.fit` result (chi², peak positions, stderr, n_eval, ...).
**No raw images are stored**, only the 1D projection that was already
fed into the optimizer.

### 1a. Automatic on dev builds

If you're on the `dev` git branch (`__version__` ends with `.dev0`),
capture is **on by default**. At `import musclex` time you'll see:

```
[musclex] dev build: A/B fit capture ENABLED -> /home/<you>/.musclex/fit_captures/2026-04-29
[musclex]   disable with: export MUSCLEX_CAPTURE_FITS=0
```

Defaults that get filled in (each can be overridden):

| env var | default on dev |
|---|---|
| `MUSCLEX_CAPTURE_FITS` | `1` |
| `MUSCLEX_CAPTURE_DIR`  | `~/.musclex/fit_captures/<YYYY-MM-DD>/` |
| `MUSCLEX_CAPTURE_TAG`  | `dev` |

The auto-enable is **skipped** under test runners (pytest, `python -m
unittest`) so CI won't silently litter your home directory.

### 1b. Explicit via environment variables

Works on any branch:

```bash
export MUSCLEX_CAPTURE_FITS=1
export MUSCLEX_CAPTURE_DIR=/path/to/cases    # optional
export MUSCLEX_CAPTURE_TAG=mysession          # folded into case_id
```

To opt out on a dev build:

```bash
export MUSCLEX_CAPTURE_FITS=0
```

### 1c. Run sources that trigger captures

Anything that ends up calling `ProjectionProcessor.fitModel()`:

| run mode | command | notes |
|---|---|---|
| **GUI initial fit** | `musclex pt` → load image → click "Fit" | one pkl per box |
| **GUI GMM editor refit** | "Edit GMM Parameters" dialog → "Refit" | only the active box re-fits |
| **headless** | `musclex pth -i input.tif -s ptsettings.json` | full batch |
| **unittest** | `python -m unittest musclex.tests.musclex_tester.MuscleXGlobalTester.testHeadlessPT*` | a few PT cases |

The GMM editor is the highest-value source: users tweak fixed params,
bounds, peak counts, and toggle Equal Variance — exactly the corner cases
where adapters diverge.

### 1d. Production / large captures

100 frames × 6 boxes ≈ 600 pkls (~5–20 MB total). Easily merge multiple
sessions:

```bash
mkdir ~/all_captures
cp ~/captures/*/*.pkl ~/all_captures/
ls ~/all_captures | wc -l
```

---

## 2. Running A/B comparisons

```bash
python -m musclex.tests.fitting_ab.runner \
    --cases <pkl_dir> \
    --adapters <name1> <name2> ... \
    --n-repeats <N> \
    [--perturb-init <fraction>] \
    [--limit <max_cases>] \
    --report <long.csv> \
    --summary <pivot.csv>
```

### Adapters

Built-in adapter names (more can be added — see § 5):

| name | what changes vs production |
|---|---|
| `lmfit-baseline-leastsq` | exact production behaviour (LM + tan/arctan bound mapping) |
| `lmfit-trf` | `method='least_squares'` — TRF with native bounds |
| `lmfit-trf-jac` | TRF + `fit_kws={'x_scale': 'jac'}` — Jacobian-based parameter scaling. Use this when free params span many orders of magnitude (e.g. EquatorImage, where areas are ~10⁶ and sigmas are ~10¹) — without it, scipy's relative-step `xtol` can trip after 5 nfev. |
| `lmfit-poisson` | leastsq + Poisson `weights = 1/sqrt(max(y, 1))` |
| `lmfit-trf-poisson` | TRF + Poisson weights |

### `--n-repeats`

Each (adapter, case) pair runs `n` times. With deterministic init this
just measures timing variance; with `--perturb-init` it also probes
robustness.

### `--perturb-init`

Bound-aware perturbation. For each free parameter:

* **bounded** params (both min/max finite): additive,
  `init + uniform(-p, +p) * (max-min)/2`
* **unbounded / one-sided**: multiplicative `init * (1 ± p)`

A unit `p=1.0` means "anywhere across the search range" for bounded
params, "±100% relative" for unbounded ones.

### Examples

Default 5× replay, no perturbation:

```bash
python -m musclex.tests.fitting_ab.runner \
    --cases ~/captures \
    --adapters lmfit-baseline-leastsq lmfit-trf \
    --n-repeats 5
```

Robustness sweep:

```bash
for p in 0.0 0.05 0.15 0.30 0.50 1.00; do
  python -m musclex.tests.fitting_ab.runner \
    --cases ~/captures \
    --adapters lmfit-baseline-leastsq lmfit-trf \
    --n-repeats 10 --perturb-init $p \
    --summary "sweep_${p}.csv"
done
```

Quick smoke test on a subset:

```bash
python -m musclex.tests.fitting_ab.runner \
    --cases ~/captures \
    --adapters lmfit-trf \
    --limit 20 \
    --n-repeats 1
```

---

## 3. Reading the report

### Long-format report (`--report`)

One row per (adapter, case, trial). Key columns:

| column | meaning |
|---|---|
| `case_id`, `box_name`, `n_peaks`, `bgsub`, `tags` | case identification |
| `adapter`, `trial`, `perturb_init` | run config |
| `success`, `converged`, `aborted` | success=converged∧¬aborted; `aborted=True` when fit hit max nfev |
| `elapsed_s`, `n_eval` | wall time and function evaluations |
| `chi2`, `redchi`, `r2` | unweighted Σresidual², χ²/dof, coefficient of determination |
| `ref_chi2`, `ref_r2`, `ref_elapsed_s` | baseline reference (frozen at capture time) |
| `chi2_ratio` | `chi2 / ref_chi2`. **=1.0** ⇒ same optimum, **>2** ⇒ likely worse local minimum |
| `speed_ratio` | `elapsed_s / ref_elapsed_s` |
| `p_max_abs_diff` | max(\|p_i - p_i_ref\|) over all peak positions, in pixels |
| `amp_max_rel_diff` | max relative amplitude diff |
| `error` | exception message if the fit raised |

### Summary table (`--summary`)

One row per adapter, aggregated:

| column | meaning |
|---|---|
| `n_fits` | total fits run |
| `success_rate` | fraction that converged without aborting |
| `aborted_rate` | fraction that hit nfev/iter cap |
| `elapsed_median`, `elapsed_mean` | timing |
| `speed_ratio_median` | median of per-fit `speed_ratio` |
| `chi2_ratio_median` | central tendency of fit quality vs reference |
| `p_max_diff_median` | typical peak position deviation |
| `p_max_diff_p95` | **worst-5%** peak deviation — the real robustness number |
| `amp_diff_median` | typical amplitude deviation |

> Always look at `p_max_diff_p95` *and* the median. The median can hide
> tail behaviour where one in twenty fits goes wildly wrong.

### Interpreting at a glance

| signal | interpretation |
|---|---|
| `chi2_ratio_median ≈ 1.0`, `p_max_diff_median < 1e-3` | adapter finds the same optimum |
| `chi2_ratio_median ≈ 1.0`, `p_max_diff_p95 ≈ 0` | adapter is very stable |
| `chi2_ratio_median > 2` | adapter often converges to a worse local min |
| `aborted_rate > 0` | adapter hits its iteration cap on some cases |

---

## 4. Diagnosing a single failure

When a particular case behaves weirdly, the diagnostic script perturbs it
many times and dumps full state for any failure:

```bash
python -m musclex.tests.fitting_ab.diagnose_m6_trf_failure \
    --case /tmp/captures/some_case.pkl \
    --seeds 30 \
    --perturb 0.15
```

Output per failure includes:

* the perturbed init values (and which were clipped to bounds),
* the final fitted params,
* residual mean / std / max — useful for spotting numerical degeneracy
  (e.g. all positions pinned to the boundary, sigma collapsed to 1, etc).

This is how the original "TRF aborted on m6 trial 5" mystery got solved
(answer: the perturbation strategy was wrong, not TRF).

---

## 5. Adding a new adapter

Subclass `FitterAdapter` (or `_LmfitAdapterBase` if you stay inside
lmfit). Required interface:

```python
from musclex.tests.fitting_ab.adapters import FitterAdapter, FitCase, FitResult

class ScipyDirectAdapter(FitterAdapter):
    name = "scipy-direct-trf"

    def fit(self, case: FitCase, *, perturb_init=None) -> FitResult:
        ...
        return FitResult(
            success=...,
            values=...,
            stderr=...,
            n_eval=..., n_iter=..., elapsed_s=...,
            converged=..., chi2=..., redchi=..., r2=...,
            message="", aborted=False, raw=...,
        )
```

Then register it:

```python
# in adapters/__init__.py
from .scipy_direct import ScipyDirectAdapter
ADAPTER_REGISTRY[ScipyDirectAdapter.name] = ScipyDirectAdapter
```

Smoke tests in `tests/test_smoke.py` automatically exercise every entry
in `ADAPTER_REGISTRY` against a synthetic case, so a new adapter gets a
free correctness check.

---

## 6. Schema (`FitCase` pickle layout)

```
FitCase
 ├── schema_version: int                # currently 1
 ├── meta: CaseMeta
 │    ├── case_id: str                  # tag_box_index_uuid
 │    ├── box_name, box_type, bgsub
 │    ├── use_common_sigma, merid_bg
 │    ├── peaks_seed, hull_range
 │    ├── peak_tolerance, sigma_tolerance
 │    ├── source_image
 │    ├── invocation: dict              # versions, timestamps
 │    └── difficulty_tags: list[str]    # n_peaks=4, gmm=True, ...
 │
 ├── inputs: FitInputs
 │    ├── x: np.ndarray                 # float64
 │    ├── y: np.ndarray                 # float64
 │    ├── weights: np.ndarray | None    # reserved
 │    ├── model_kind: 'standard'|'gmm'
 │    ├── has_voigt: bool
 │    ├── free_params: dict[str, ParamSpec]
 │    │      # {init, min, max} ; None means unbounded
 │    ├── independent_vars: dict        # passed straight to model func
 │    └── fixed_params: dict[str, float] # vary=False params
 │
 ├── reference: ReferenceResult          # baseline frozen at capture
 │    ├── values, stderr
 │    ├── chi2, redchi, r2
 │    └── n_eval, elapsed_s, message
 │
 └── optional: dict                      # forward compatibility
```

Pure NumPy / Python types — never pickled lmfit objects, so a `FitCase`
remains valid even if lmfit is removed.

---

## 7. Tests

```bash
python -m unittest musclex.tests.fitting_ab.tests.test_smoke -v
```

Eleven tests cover:

* pickle round-trip
* every registered adapter on a synthetic 2-peak case
* baseline replay = reference (framework self-test)
* smart perturbation: bounded params stay in range, unbounded use multiplicative
* `_is_aborted` / `_manual_chi2` helpers

---

## 8. Architecture

```
musclex/tests/fitting_ab/
├── __init__.py                      # module entry
├── adapters/
│   ├── base.py                      # FitCase, FitInputs, FitResult, FitterAdapter
│   ├── lmfit_adapters.py            # 4 lmfit variants
│   └── __init__.py                  # ADAPTER_REGISTRY, get_adapter()
├── capture/
│   ├── recorder.py                  # env-flag hook installed by ProjectionProcessor
│   └── __init__.py
├── metrics.py                       # diff / ratio computations
├── runner.py                        # CLI + run_ab() programmatic API
├── diagnose_m6_trf_failure.py       # focused failure investigator
├── tests/
│   └── test_smoke.py
├── fit_cases/                       # default capture dir (only used when env unset)
└── README.md                        # this file
```

The hook in `ProjectionProcessor.py` is ~15 lines, lazily imported and
zero-cost when capture is off.

---

## 9. Common gotchas

1. **Multiplicative perturbation kills bounded params.** Always use the
   provided `--perturb-init`; it's bound-aware. A naive `init * (1±0.15)`
   on a position with bounds `[543, 547]` overshoots to 626 and starts
   the optimizer from the active boundary, which manufactures fake
   "failures" you'll spend a day chasing.
2. **lmfit's `chisqr` is wrong on aborted fits** (it can read 1e-250).
   Use the `chi2` field from `FitResult` (recomputed manually from
   residuals); reference recompute in the recorder is also fixed.
3. **Environment variables are read at import time on the GUI side.**
   Changing `MUSCLEX_CAPTURE_TAG` after `musclex pt` is already running
   has no effect — restart the GUI for a new tag.
4. **The runner uses one shared RNG per adapter.** Trial `k` of case `i`
   is *not* independent of trial `k` of case `j`; if you need true
   independence, run multiple separate `--n-repeats=1` invocations.
5. **`elapsed_s` includes lmfit / scipy import overhead on the first
   call.** Always set `--n-repeats >= 3` and look at the median, not the
   mean.

---

## 10. Findings to date

The dated benchmark report — including data-grounded tables, citation
of every source CSV, and the recommendation — lives in
[`REPORT.md`](REPORT.md). It now covers two modules with **opposite
verdicts**:

### Projection Traces — switch to TRF

5 captured cases, perturbation sweep at `p ∈ {0.05, 0.15, 0.30, 0.50}`:

* **`lmfit-trf` reaches the same optimum as the production baseline**
  (`chi2_ratio_median = 1.0000`) on every case, every perturbation
  level. Both adapters always converge.
* **TRF is faster on the harder cases** (Test 1 medians: `m3` 11.7 vs
  81.7 ms, `m6` 100 vs 157 ms, `TEST` 88 vs 290 ms; ~5–35 % faster
  overall).
* **TRF's peak-position p95 is bounded** at 0.03 – 0.17 px regardless
  of perturbation. Baseline's p95 grows to 2.4 px at `p = 0.50`,
  concentrated on the one strong-background case (`TEST`).
* **Both Poisson-weighted adapters are wrong** — convex-hull bg
  subtraction removed the Poisson noise; `1/√y` biases the optimum
  1 – 4 px away.
* **Production change shipped** in `ProjectionProcessor.fitModel()`.

### Equator — stay on `leastsq`

6 captured cases (3 image sources, 2 captures each), perturbation
sweep at `p = 0.15`:

* **Plain `lmfit-trf` is broken** — premature `xtol` termination at
  5 nfev (because area params are 10⁶ × bigger than sigma params), 17 %
  aborted, and one catastrophic 24 000-nfev abort.
* **`lmfit-trf-jac` (TRF + `x_scale='jac'`) is correct** — same
  minimum as `leastsq`, 0 aborts. But **2 – 3 × slower** than `leastsq`
  on every case.
* **`leastsq` wins**: 100 % success, 30–40 ms median, no robustness
  gap vs `trf-jac`. Production stays on `leastsq`.

See `REPORT.md` for the full numbers, per-case breakdown, and
reproduction commands.
