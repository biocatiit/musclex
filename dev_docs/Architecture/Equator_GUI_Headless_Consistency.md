# Equator (EQ) GUI–Headless Consistency Refactor

## Objective

Ensure that the **GUI** and **Headless (command-line)** modes of the Equator
module produce numerically identical results when given the same settings and
calibration, and that settings can be freely exported from the GUI, committed
alongside test images, and replayed by the headless runner without any manual
editing.

---

## 1. Load Settings Feature

### Problem

Before this work there was no way to import a `eqsettings.json` file into the
GUI. Parameters had to be set by hand through widgets. This made it impossible
to guarantee that a GUI run and a headless run used exactly the same parameters.

### What was built

#### `EquatorWindow.loadSettings()` — the import entry point

A **"Load Settings…"** menu action (Ctrl+L) was added to `EquatorWindow`. It:

1. Opens a file picker defaulting to `musclex/settings/`.
2. Reads and JSON-parses the file; warns on unknown keys but continues.
3. Calls `_apply_loaded_eq_settings(loaded)` to push all values into the
   appropriate widgets, with `blockSignals(True)` active so only one
   reprocess pass fires at the end.
4. Wipes the per-image EquatorImage cache (`fit_results`, `peaks`, `hist`,
   `hulls`, `tmp_peaks`, `int_area`) before calling `processImage()`, so the
   freshly-loaded parameters actually take effect instead of being shadowed by
   a stale cache.

#### `EquatorWindow.saveSettings()` — the export entry point

Saves the current widget state to a JSON file. Calibration-derived fields
(`lambda_sdd`, `detector`, `calib_center`, `silverB`, `radius`, `type`,
`sdd`, `pixel_size`, `lambda`) are explicitly stripped before writing so the
exported file is portable and does not encode environment-specific calibration.

---

## 2. Declarative Binding Schema (`musclex/utils/eq_settings_bindings.py`)

A new **Qt-free** module that is the inverse of `EquatorWindow.getSettings()`
and `EQ_FittingTab.getFittingSettings()`. It declares exactly how every JSON
key maps to a widget, so `loadSettings()` can be driven by a table rather than
ad-hoc code. Being Qt-free lets unit tests import it without a display.

### Binding categories

| Table / Set | Keys covered | How applied |
|---|---|---|
| `EQ_SPINBOX_BINDINGS` | `nPeaks`, `mask_thres` | `widget.setValue(v)` |
| `EQ_COMBO_TEXT_BINDINGS` | `model` | `widget.setCurrentText(str(v))` |
| `EQ_COMBO_INDEX_BINDINGS` | `orientation_model` | `widget.setCurrentIndex(int(v))` |
| `EQ_CHECKBOX_BINDINGS` | `isSkeletal`, `isExtraPeak`, `90rotation`, `find_oritation`, `inpaint` | `widget.setChecked(bool(v))` |
| `EQ_SPARSE_FIX_BINDINGS` | `fix_k`, `fixed_rmin`, `fixed_rmax` | key present → tick checkbox + set spinbox; absent → untick |
| `EQ_FITTING_FIX_SUFFIXES` | `left/right_fix_sigmac/d/s/gamma` + all `*_z*` / `*_EP` variants | Handled by `EQ_FittingTab.applyLoadedSettings()` |
| `EQ_FITTING_VAL_SUFFIXES` | `left/right_sigmac/d/s/gamma` (unfixed value hints) | Set spinbox only, leave checkbox unchecked |
| `EQ_SPECIAL_KEYS` | `fixed_int_area` | Custom inline logic (tuple → two spinboxes + checkbox) |
| `EQ_SKIP_KEYS` | All calibration fields + `blank_mask` | Silently ignored on import |

### Sparse-fix schema

EQ persists "fix this parameter" choices as **presence of the key**:

```
"left_fix_sigmac": 1.0   →  tick fixSigmaC checkbox, set spinbox to 1.0
(key absent)             →  leave fixSigmaC unchecked
```

When a parameter's fix-checkbox is **not** ticked, `getFittingSettings()`
still writes the current spinbox value under the plain key (e.g.
`"left_sigmad": 3.124`). `applyLoadedSettings()` restores this as an initial
guess: spinbox value is set, checkbox stays unchecked.

### `classify_eq_setting_key(key)`

Returns one of `spinbox / combo_text / combo_index / checkbox / sparse_fix /
fitting_fix / fitting_val / special / skip / unknown`. Any `unknown` result
means the schema test fails immediately, surfacing drift between
`getSettings()` and the binding tables.

---

## 3. Fitting Tab Support (`EQ_FittingTab.applyLoadedSettings`)

`EQ_FittingTab.applyLoadedSettings(settings)` is the inverse of
`getFittingSettings()`. It handles both variants per parameter:

```
left_fix_sigmad present  →  check fixSigmaD checkbox + sigmaDSpinBx.setValue(v)
left_sigmad present      →  sigmaDSpinBx.setValue(v) only (checkbox stays off)
neither present          →  fixSigmaD.setChecked(False), spinbox disabled
```

All widget changes happen inside a `syncUI = True` block so intermediate
states do not fire cascading slot calls.

---

## 4. Calibration Architecture Fix

### The problem (before this work)

`eqsettings.json` contained calibration-derived fields (`lambda_sdd`,
`calib_center`, `detector`, …). The GUI derived these from
`<dataset>/settings/calibration.info` at runtime via `getFlags()`, while
headless mode swallowed the entire JSON including these fields via
`info.update(settings)`. This created two sources of truth:

- If the user had calibrated in the GUI, the JSON carried their calibration.
- If they then moved the dataset to another machine (different calibration),
  the JSON values would silently override the local calibration.
- If they saved settings without calibration, `d10` would be absent in the
  GUI but present (stale) in headless runs from the old JSON.

### The fix (three-part)

#### `SettingsManager.derive_processing_calibration()`

A new method on `SettingsManager` (pure Python, no Qt) that reads
`<dataset>/settings/calibration.info` and applies the same formula that
`EquatorWindow.getFlags()` uses:

```python
# type == "img"
lambda_sdd = silverB * radius

# type == "cont"
lambda_sdd = lambda_ * sdd / pixel_size
```

Returns `{}` when the file does not exist or its `settings` dict is empty
(user never ran calibration). `detector` is forwarded when present.

#### `EquatorWindow.saveSettings()` — strip on export

All calibration-related keys are popped from the settings dict before
writing, so the exported `eqsettings.json` never contains them.

#### `EquatorWindowh.getSettings()` — strip + inject on import

1. Any residual calibration keys in the JSON are removed (backward-compat with
   legacy files; a status message is printed).
2. `SettingsManager(dir_path).derive_processing_calibration()` is called and
   the result is merged into `settings`.

**Net effect**: if the user never ran calibration, `settings` lacks
`lambda_sdd` → `EquatorImage` skips `d10 = lambda_sdd / S10` → CSV shows
`d10 = -`. This matches what the GUI would show in the same uncalibrated state.

### Ordering constraint in `_apply_loaded_eq_settings`

`model`, `isSkeletal`, and `isExtraPeak` must be applied **before** fitting-tab
parameters because those checkboxes gate widget visibility inside
`EQ_FittingTab`. Applying `left_fix_gammaz` before `isSkeletal` would set a
hidden widget that is then immediately overwritten when the tab re-renders.

---

## 5. Test Coverage

### `testEQSettingsBindingsSchema`

Loads the baseline `eqsettings.json` for all three test datasets
(MARimages, EIGERimages, PILATUSimages) and asserts that every key is
classified as non-`unknown` by `classify_eq_setting_key()`.

Fails fast when:
- A new key is added to `getSettings()` / `getFittingSettings()` without a
  corresponding load binding, OR
- A key is removed from the code but remains in a committed JSON file.

### `testDeriveProcessingCalibration`

A contract test with 6 cases locking `SettingsManager.derive_processing_calibration()` against known inputs and expected outputs:

| Case | Input | Expected output |
|---|---|---|
| No settings dir | `SettingsManager('')` | `{}` |
| Missing `calibration.info` | dir exists, file absent | `{}` |
| Empty `settings` dict | file present, `settings={}` | `{}` |
| `type=img` with detector | `silverB=5.838, radius=884, detector=…` | `{lambda_sdd: 5165.592, detector: …}` |
| `type=cont` no detector | `lambda=1.5, sdd=200, pixel_size=0.075` | `{lambda_sdd: 4000.0}` |
| `type=img` missing `radius` | only `silverB` present | `{}` (safe degradation) |

Drift in this test means the GUI and headless calibration formulas have
diverged, which would corrupt `d10` and downstream fit metrics silently.

### `testHeadless{Mar,Eiger,Pilatus}Equator` — headless vs GUI comparison

After running headless EQ processing, `_compareToGuiBaselineIfPresent` is
called for all three datasets. It compares `eq_results/summary.csv` (long
format, one row per peak) against `eq_results_gui/summary.csv` (produced by
a GUI run with Load Settings). The check is a no-op when the GUI baseline
file is absent, so it does not break CI when run without GUI baselines.

---

## 6. Files Changed

| File | Change |
|---|---|
| `musclex/ui/EquatorWindow.py` | Added `loadSettings()`, `_apply_loaded_eq_settings()`, modified `saveSettings()` to strip calibration fields; added "Load Settings…" menu action (Ctrl+L) |
| `musclex/ui/EQ_FittingTab.py` | Added `applyLoadedSettings(settings)` handling both sparse-fix and value-hint variants |
| `musclex/utils/eq_settings_bindings.py` | **New file** — Qt-free declarative binding tables and `classify_eq_setting_key()` |
| `musclex/utils/settings_manager.py` | Added `PROCESSING_CALIBRATION_KEYS` class constant and `derive_processing_calibration()` method |
| `musclex/headless/EquatorWindowh.py` | Rewrote `getSettings()` to strip calibration fields from JSON and inject derived values from `calibration.info` |
| `musclex/tests/musclex_tester.py` | Added `testEQSettingsBindingsSchema`, `testDeriveProcessingCalibration`; added GUI baseline comparison hook to all three EQ headless tests; extended `_compareToGuiBaselineIfPresent` with `rtol_override`/`atol_override` params |
| `musclex/tests/testImages/*/eqsettings.json` | Regenerated from GUI after calibration refactor — calibration fields removed |
| `musclex/tests/testResults/*/eq_results/summary{,2}.csv` | Updated to match output of new settings |
| `musclex/tests/testImages/*/eq_results_gui/summary.csv` | New files — GUI baselines for headless vs GUI comparison |

---

## 7. Invariants to Maintain

1. **Every key that `getSettings()` / `getFittingSettings()` writes must
   appear in `eq_settings_bindings.py`** in a non-`skip` category.
   Enforced by `testEQSettingsBindingsSchema`.

2. **`eqsettings.json` must never contain calibration fields.**
   Enforced by `saveSettings()` stripping them on export and by the schema
   test classifying them as `skip` (not `unknown`).

3. **`derive_processing_calibration()` must mirror `EquatorWindow.getFlags()`
   exactly.** Enforced by `testDeriveProcessingCalibration`.

4. **Fitting-tab state (`isSkeletal`, `isExtraPeak`, `model`) must be applied
   before per-side fitting parameters** during `_apply_loaded_eq_settings()`.
   Required for widget visibility to be correct when spinbox values are written.
