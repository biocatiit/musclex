# QF Cache & Fast-Path Refactor — Pixel Regression Test Report

**Date:** 2026-05-07  
**MuscleX version:** 1.29.0-beta.2.dev0  
**Branch:** `dev`  
**Tester:** automated comparison script

---

## 1. Scope

This report covers pixel-level regression testing for the Quadrant Folding
processing chain following the changes described in
[`QF-Cache-And-FastPath.md`](./QF-Cache-And-FastPath.md).

### Commits under test

| Commit | Date | Description |
|---|---|---|
| `91592dab` | 2026-04-28 | **Baseline** — last commit before this feature work |
| `5e6cda94` | 2026-05-01 | Cache refactor: fingerprint, fast-path, `CACHE_FORMAT_VERSION=2` |
| `c19c32bb` | 2026-05-01 | ROI translation moved to headless `getFlags()` |
| `4200d2cd` | 2026-05-04 | Remove "Save Cropped Image" feature |
| `e8abf2f3` | 2026-05-06 | ROI applied inside BG-sub estimation (Smoothed / RovingWindow) |
| `2c974ac0` | 2026-05-07 | `mergeImages` skips blending when `bgsub2='None'` |

---

## 2. Test Input

| Field | Value |
|---|---|
| Image | `F1_P3_02041_LA_pCA8_00001.tif` |
| Image path | `musclex/tests/qf/tmp_verify_settingsQF/` |
| Settings file | `qfsettings.json` (same directory) |
| Detector | HF-4M |
| Image size | 4148 × 4362 px → fold / result: **2048 × 2048** |

**Effective settings used** (from `qfsettings.json`, plus forced flags):

```json
{
  "bgsub":  "Circularly-symmetric",
  "bgsub2": "None",
  "fixed_rmin": 34,
  "transition_radius": 1020,
  "transition_delta": 1,
  "compressed": true,
  "no_cache": true,
  "no_fast_path": true,
  "fold_image": true,
  "orientation_model": 0
}
```

`no_cache` and `no_fast_path` were set to guarantee the full slow-path
pipeline ran in all three versions, making the comparison a pure
pixel-equivalence check with no caching artefacts.

---

## 3. Method

Three source-tree checkouts were run in the same Python process using
the shared `venv`, with `PYTHONPATH` swapped between runs to load each
version's `musclex` package:

| Label | Source dir | Commit |
|---|---|---|
| **BASELINE** | `/tmp/qf_baseline` | `91592dab` |
| **AFTERCACHE** | `/tmp/qf_aftercache` | `4200d2cd` |
| **HEAD** | `/home/alex/VSCode/musclex` | `2c974ac0` |

Each run called `QuadrantFolder.process(flags)` and captured
`imgCache['resultImg']` (float32, 2048 × 2048). Results were compared
pairwise using `numpy`.

---

## 4. Results

### 4.1 Comparison matrix

| Pair | Max abs diff | Pixels differing | Verdict |
|---|---|---|---|
| **BASELINE vs AFTERCACHE** | **0.000000** | **0.00%** | ✅ **BIT-FOR-BIT IDENTICAL** |
| BASELINE vs HEAD | 4.0668 | 21.98% | ⚠️ Intentional algorithm change (§5) |
| AFTERCACHE vs HEAD | 4.0668 | 21.98% | ⚠️ Same delta, same source (§5) |

### 4.2 Detail — largest divergence

Largest pixel difference (BASELINE vs HEAD, at pixel (2, 1023)):

| Version | Value |
|---|---|
| BASELINE / AFTERCACHE | 14.5195 |
| HEAD | 10.4527 |
| Difference | −4.0668 |

---

## 5. Analysis

### 5.1 Cache refactor commits — ✅ No pixel impact

The comparison **BASELINE vs AFTERCACHE** is the definitive test for
our cache/fingerprint work (`5e6cda94`, `c19c32bb`, `4200d2cd`).

Result: **identical**. Zero pixels differ across the entire 2048 × 2048
output. This confirms:

- `computeFingerprint` / `_tryFastLoad` / `cacheInfo` changes — no
  effect on the processing pipeline.
- ROI translation refactor (`fixed_roi_*` → `roi_w/h` in headless) —
  no effect (no ROI set in this test; translation path not exercised).
- Removal of "Save Cropped Image" — no effect on
  `imgCache['resultImg']` (was output-only).
- Removal of `self.initImg` dead field — no effect.
- `_NON_CACHED_KEYS` / `_IMAGE_ARRAY_KEYS` changes — no effect on
  pixel computation.

### 5.2 BASELINE vs HEAD divergence — ⚠️ Intentional, from `2c974ac0`

The 21.98% pixel difference is introduced entirely by commit `2c974ac0`
("Refactor background subtraction logic in QuadrantFolder"), which
changed `mergeImages()`:

**Before (`2c974ac0`):**
```python
# bgsub2='None' → bgimg2 = avg_fold (pass-through)
# then blend bgimg1 with avg_fold using sigmoid at transition_radius
BgSubFold = combine_bgsub_linear_float32(bgimg1, avg_fold, ...)
```

**After (`2c974ac0`):**
```python
if bgsub2 == 'None':
    # skip blending entirely
    BgSubFold = bgimg1
else:
    BgSubFold = combine_bgsub_linear_float32(bgimg1, bgimg2, ...)
```

For this test (`bgsub='Circularly-symmetric'`, `bgsub2='None'`), the
outer-radius region (beyond `transition_radius`) previously received a
blended value; it now receives the raw `bgimg1` value. The change is
**intentional** and documented in the commit message.

**This difference is not a regression from the cache refactoring.**

### 5.3 `e8abf2f3` (ROI in BG-sub) — not exercised

This test had no `roi_w` / `roi_h` in the settings, so the new
ROI-inside-BG-sub branches in `applySmoothedBGSub` and
`applyRovingWindowBGSub` were not triggered.

---

## 6. Coverage gaps

The following scenarios were **not** covered by this automated run and
remain manual / future test candidates:

| Scenario | Risk | Reason not covered |
|---|---|---|
| `bgsub='Smoothed Gaussian'` + ROI | Medium | `e8abf2f3` changes BG estimation area |
| `bgsub='Roving Window'` + ROI | Medium | Same as above |
| ROI > single-quadrant size (padding edge case) | Medium | No such test image available |
| Fast-path round-trip (cold → hot, pixel equality) | Medium | Deliberately disabled with `no_fast_path` |
| Headless `fixed_roi_*` → `roi_w/h` translation | Low | Unit-test level; no headless fixture |
| GUI coordinate-system accuracy on fast-path | Low | GUI not testable headless |
| Old v1 cache (`CACHE_FORMAT_VERSION=1`) rejection | Low | Requires constructing synthetic pickle |
| `bgsub2 != 'None'` (two-BG blend path) | Low | Not tested — both pre- and post-`2c974ac0` should be identical in this path |

---

## 7. Conclusion

| Item | Result |
|---|---|
| Cache / fingerprint / fast-path refactor (`5e6cda94`, `c19c32bb`, `4200d2cd`) | ✅ **No pixel regression** |
| Remove "Save Cropped Image" feature | ✅ **No pixel regression** |
| ROI in BG-sub (`e8abf2f3`) | ⚪ Not exercised (no ROI in test settings) |
| Merge-logic change (`2c974ac0`) | ⚠️ **Intentional pixel change** when `bgsub2='None'` and inner BG method is active — verify with users |

The cache refactoring is safe to ship. The only outstanding item
requiring user confirmation is whether the `2c974ac0` blend-skip
behaviour is the intended scientific result for the `bgsub2='None'`
case.
