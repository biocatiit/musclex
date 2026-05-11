# Equator Gap Interpolation Mechanisms

Equator has two distinct mechanisms for handling gaps in the projection histogram (such as those caused by detector gaps or user-defined masks). While both use interpolation, they operate at different stages of the processing pipeline, use different algorithms, and serve different purposes.

## 1. Convex-Hull Post-Interpolation (Automatic)

This is the default, automatic handling for ignored columns in Equator. It operates **after** convex-hull background subtraction.

### Purpose
To prevent masked columns or detector gaps from creating artificial zero-drops (hard cuts) in the background-subtracted peak profile, which would severely distort the Gaussian/Voigt peak fitting.

### How it works
1. **Raw Histogram:** The raw projection histogram is computed normally.
2. **Ignore Detection:** Any column in the `img_area` containing a pixel at or below the mask threshold (e.g., `-1.0` from a mask or detector gap) is flagged as `ignore`.
3. **Background Subtraction:** `convexHull(ignore=...)` is called. The `ignore` array prevents the background baseline (the "rubber band") from dipping into the artificial valleys caused by the gaps.
4. **Post-Interpolation:** The `convexHull` function returns a background-subtracted profile where the ignored columns are forced to `0`. Equator's `_interpolateIgnoredHullValues` then intercepts this profile and replaces those `0`s with a **linear interpolation** between the nearest valid (non-ignored) neighbors on either side of the gap.

### Characteristics
- **Trigger:** Automatic, whenever `img_area[:, i] <= mask_thres` is met.
- **Algorithm:** Linear interpolation.
- **Stage:** Applied to the background-subtracted profile (the final signal used for peak fitting).
- **Effectiveness:** Highly effective for both physical detector gaps and user-drawn masks (`mask.tif`), because it relies on pixel-level sentinel values (`-1.0`) rather than column sums.

---

## 2. "Interpolate Gaps" Checkbox (Manual)

This is an optional feature enabled by the user via the "Interpolate Gaps" checkbox in the Fitting tab. It operates **before** convex-hull background subtraction.

### Purpose
To estimate and reconstruct missing data in the raw projection histogram before any background estimation occurs.

### How it works
1. **Raw Histogram:** The raw projection histogram is computed.
2. **Gap Detection:** The algorithm looks for columns where the **total sum is less than 0** (`hist < 0`). Users can also manually define gap ranges using the "Add Gaps" button.
3. **Pre-Interpolation:** 
   - The non-gap regions of the raw histogram are smoothed using a Savitzky-Golay filter.
   - The smoothed curve is sampled at regular intervals.
   - A B-spline curve is fitted to these samples.
   - The gap regions in the raw histogram are replaced with values from the B-spline curve.
4. **Background Subtraction:** `convexHull` is called on this repaired histogram. Because the gaps are assumed to be fixed, the `ignore` array is set to `None` (convex hull treats the repaired histogram as continuous valid data).

### Characteristics
- **Trigger:** Manual (user must check "Interpolate Gaps").
- **Algorithm:** Savitzky-Golay smoothing + B-spline interpolation.
- **Stage:** Applied to the raw projection histogram (before background subtraction).
- **Effectiveness:** Works well for physical detector gaps (where the entire column is `-1.0`, making the sum negative) or manually specified gaps. 
- **Limitation with User Masks:** It often fails to automatically detect user-drawn masks (`mask.tif`). A user mask typically only covers part of a column; the sum of the remaining valid pixels usually keeps the column total positive (`hist > 0`), so the algorithm does not recognize it as a gap.

---

## Summary Comparison

| Feature | Convex-Hull Post-Interpolation | "Interpolate Gaps" Checkbox |
| :--- | :--- | :--- |
| **Trigger** | Automatic (pixel `<= mask_thres`) | Manual (Checkbox + `hist < 0` or manual gaps) |
| **Pipeline Stage** | **After** convex-hull background subtraction | **Before** convex-hull background subtraction |
| **Target Data** | Background-subtracted profile | Raw projection histogram |
| **Algorithm** | Linear interpolation | Savitzky-Golay + B-spline |
| **Convex Hull `ignore`** | Active (protects background estimate) | Disabled (`ignore=None`) |
| **Handles `mask.tif`?** | Yes (pixel-level detection) | Poorly (column sum is usually positive) |
