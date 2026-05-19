# Quadrant Folder (QF) Refactoring and Bug Fixes Summary

## Objective
The primary goal of this work was to ensure 100% consistency between the **GUI** and **Headless (command-line)** modes of the Quadrant Folder (QF) module, specifically focusing on background subtraction settings and evaluation metrics.

## 1. Export Fields Addition

Two new columns were added to `qf_results/summary.csv` by updating `QF_CSVManager` and `QuadrantFolder.evaluateResult`.

### `bgSum` — Background Sum
* **Where it is calculated**: in `QuadrantFolder.evaluateResult()`, immediately after the loss is computed.
* **How**: `self.info['result_bg']['intensity'] = np.sum(bg)`, where `bg` is the background image array (the BG-subtracted complement, reconstructed from `resultBg` in `imgCache`).
* **Where it is written**: `QF_CSVManager.writeNewData()` reads `quadFold.info['result_bg']['intensity']` and writes it under the column name `bgSum`.

### `symmetry` — Fold Symmetry (Normalised)
* **Where it is calculated**: also in `QuadrantFolder.evaluateResult()`, right after `bgSum`.
* **How**: calls `_compute_fold_symmetry(img, center, rotation)` from the new utility module `musclex/utils/fold_symmetry.py`. This function:
  1. Applies the same translate-then-rotate affine transform that `transformImage` uses.
  2. Splits the result into 4 quadrants and flips them to a common (top-left) orientation.
  3. Pads quadrants with NaN for fair overlap handling.
  4. Computes per-pixel `nanstd` across the 4 quadrants at every position with ≥ 2 valid samples → `fold_std_sum`.
  5. Normalises by `N_fg × μ_fg` (total foreground signal determined via Otsu thresholding) → `fold_std_norm`. This makes the score dimensionless and exposure-independent.
* The resulting `fold_std_norm` value is stored as `self.info['result_bg']['symmetry']`.
* **Why a separate utility module**: `fold_symmetry.py` lives under `musclex.utils` (not `musclex.ui`) so the headless pipeline can call it without importing Qt.
* **Where it is written**: `QF_CSVManager.writeNewData()` reads `quadFold.info['result_bg']['symmetry']` and writes it as the `symmetry` column.

### `QF_CSVManager` column definition
```python
self.colnames = [
    'Filename', 'centerX', 'centerY', 'rotationAngle',
    'backgroundMethod', 'backgroundConfigName',
    'parameters', 'downsampled',
    'loss', 'bgSum', 'symmetry'
]
```

## 2. GUI Settings Import (Load Settings) & Schema Binding

### The Problem
The GUI and headless paths were using different parameters in practice. In headless mode, the user provides a `qfsettings.json` file. In the GUI, parameters are set through widgets by hand. There was no way to import a JSON file into the GUI, so reproducibility between the two modes was impossible to guarantee.

### What We Built

#### `musclex/utils/qf_settings_bindings.py` — Qt-free declarative schema
A new module (intentionally Qt-free so tests can import it without a display) that declares three binding tables — the inverse of `getFlags()`:

| Table | Type | How the value is applied |
|---|---|---|
| `QF_SPINBOX_BINDINGS` | 58 `(json_key, widget_attr)` pairs | `widget.setValue(loaded[k])` |
| `QF_COMBO_TEXT_BINDINGS` | 5 entries with a transform function | `widget.setCurrentIndex(findText(xform(value)))` |
| `QF_CHECKBOX_BINDINGS` | 4 entries incl. `persist_evaluation_baseline` | `widget.setChecked(bool(loaded[k]))` |

Additionally:
- `QF_SPECIAL_KEYS` — keys handled by custom inline logic (nested dicts, lists, paired widgets).
- `QF_SKIP_KEYS` — per-image / runtime state and legacy calibration keys that should be accepted in the JSON but never applied to the UI.
- `classify_qf_setting_key(key)` — returns `spinbox / combo / checkbox / special / skip / unknown`. Any `unknown` result means the test suite fails fast.

#### `QuadrantFoldingGUI.loadSettings()` — the actual import function
Added a **Load Settings** button to the GUI that calls `loadSettings()`. It:
1. Opens a file picker pointing to `musclex/settings/`.
2. Parses the JSON and classifies every key; prints a warning for `unknown` keys but proceeds.
3. Calls `_apply_loaded_qf_settings(loaded)` which loops through all three binding tables with `blockSignals(True)` to avoid triggering N reprocess passes mid-load.
4. Handles all special-case keys inline: `optimize` flag, `methods` list, `steps` text, `background_configurations`, paired `fixed_roi_w/h`, `mean_metric_values`/`metric_weights` (via `_sync_metric_and_synthetic_widgets_from_info`), and `evaluation_baseline`/`synthetic_*` spinboxes (applied directly, see Section 6).
5. Invalidates three layers of cached state before reprocessing (see Section 3 for details).
6. Calls `_push_session_bg_eval_settings_to_info()` to flush the just-loaded widget values into `quadFold.info` before `processImage()` is called.

#### `testQFSettingsBindingsSchema` — schema invariant test
A unit test in `musclex_tester.py` that loads the baseline `qfsettings.json` for MARimages, EIGERimages, and PILATUSimages and asserts that every key is classified as something other than `unknown`. This ensures that any new key added to `getFlags()` / `saveSettings()` without a corresponding load binding is caught immediately.

## 3. Cache Invalidation and State Leakage Fixes
* **`bgSum` = 0.0 Bug**: Fixed an issue where the background sum would be exported as 0.0 after importing settings in the GUI. This was caused by stale background calculation caches (e.g., `BgSubFold`) not being cleared, causing subsequent calculations to be skipped. Deep cache invalidation logic was added to `loadSettings`.
* **`backgroundMethod` Loss**: Fixed an issue where the background method unexpectedly reverted to `None` when switching images. This was resolved by differentiating between Python's `None` and the string `'None'`.
* **KeyError Crashes**: Fixed multiple `KeyError: 'BgSubFold'` crashes in methods like `saveBackground()` caused by accessing non-existent cache keys.

## 4. Fingerprint Drift and Precision Truncation
* **Loss Value Fluctuations**: Addressed an issue where repeatedly switching images caused minor variations in the `loss` value. The root cause was the GUI's `QDoubleSpinBox` (which retains 2 decimal places) truncating high-precision floating-point numbers dynamically calculated for fields like `evaluation_baseline`.
* **Fix**: This truncation caused the `processing_fingerprint` hash to mismatch, triggering a slow-path recalculation. We added these dynamically calculated fields to `_NON_FINGERPRINT_KEYS` to prevent fingerprint drift and added regression tests.

## 5. Cross-Image State Contamination (Leakage)
* **Sentinel (`0.0`) Mechanism Failure**: The backend logic (`_ensure_synthetic_gaussian_params`) relies on `synthetic_sigma_x/y <= 0.0` as a sentinel signal to dynamically calculate Gaussian parameters for new images.
* **Fix**: When switching images in the GUI, parameters from the previous image remained in the SpinBoxes and overwrote the new image's `info`, causing the new image to use incorrect old parameters (leading to wrong `loss` calculations). We fixed this by ensuring sentinel values are respected during image transitions.

## 6. Rebase Conflicts and Session-Global Architecture Adaptation
* During a `git rebase`, we encountered a **Session-Global** UI state management refactor introduced by another developer (e.g., `_push_session_bg_eval_settings_to_info` and `persist_evaluation_baseline`).
* This new architecture conflicted with our Load Settings logic and Sentinel mechanism (SpinBox minimum values were hardcoded, preventing `0.0` from being entered).
* **Final Fixes**:
  1. Registered the newly added `persist_evaluation_baseline` into the Schema.
  2. Modified `_apply_loaded_qf_settings` to directly assign values to Widgets.
  3. **Critical Fix**: Restored the `min_val` of Widgets like `amplitudeSpnBx` and `sigmaXSpnBx` to `0.0`, allowing Sentinel values to survive. The actual minimum value safety limits are now enforced by the backend using `max(MIN_SYNTHETIC_*, ...)`.

## Current Status and Results
Following these fixes, our comparison tests (`_compareToGuiBaselineIfPresent`) have **all passed**. 
This means: **For the EIGER and PILATUS datasets, given the same `qfsettings.json`, the GUI results and Headless results are now 100% pixel/value consistent.**

The only remaining known state is that due to minor underlying algorithmic changes introduced during the rebase, the currently generated `loss` values have a deviation of ~`1e-4` compared to the old baseline CSVs in the `testResults` directory (though `bgSum` and `symmetry` are identical). If the new values are confirmed to be correct, the baseline files in `testResults` simply need to be updated.