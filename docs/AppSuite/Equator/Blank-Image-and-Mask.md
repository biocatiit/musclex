# Empty Cell Image and Mask

For information on configuring the empty cell image and mask dialog (selecting an image, scale factor, threshold masking, drawn mask, saving), see [Common Settings — Empty Cell Image and Mask](../Common-Settings.md#empty-cell-image-and-mask).

---

## How Masked Pixels Are Treated in Equator

Understanding the full effect of enabling the mask requires tracing the data through two stages: preprocessing and analysis.

### Stage 1 — Preprocessing (pixel values)

When **Apply Mask** is checked, the program loads `mask.tif` before any analysis begins. In this binary image, `0` means "masked" and `1` means "keep". Every pixel where `mask == 0` is overwritten with a sentinel value of **−1.0** in the working image array. The raw file on disk is never modified.

If empty cell subtraction is also enabled, it is applied first; the mask sentinel assignment happens afterward.

> After blank subtraction, pixel values that become slightly negative (e.g., due to noise) are clipped to 0 before the mask is applied. Sentinel −1.0 therefore remains distinguishable from any legitimate near-zero intensity.

### Stage 2 — Analysis (histogram and fitting)

The equatorial analysis works on a **column projection histogram**: for each x-column of the image, the pixel intensities within the integration area (box height) are summed to produce one value. Masked columns — those containing at least one pixel with a value ≤ −0.01 — are flagged as **ignored** and handled as follows:

- **Projection histogram** — ignored columns contribute their actual (sentinel-containing) sum but are excluded from the convex hull background estimation. The convex hull algorithm receives an `ignore` array and uses it to prevent the background baseline from dipping into the artificial gap created by the −1.0 sentinels.
- **Peak fitting** — values in ignored columns within the active hull range are replaced by linear interpolation between the nearest non-ignored neighbors on each side, so the fitted curve is continuous across the gap rather than forced to zero.
- **Integration area trimming** — rows at the top or bottom of the integration box whose entire row sum is ≤ −0.01 are also excluded before the projection is computed.

The net result is that detector gaps, dead pixels, and any other manually masked regions are cleanly bypassed without biasing the background estimate or creating spurious dips in the diffraction profile.

### Equator Convex-Hull Interpolation

Equator uses a local interpolation pass after convex-hull background subtraction.

The processing order is:

1. Build the raw projection histogram from the integration area.
2. Detect ignored columns from the rotated working image. A column is ignored if any pixel in that column within the active integration area is at or below the mask threshold.
3. Run convex-hull background subtraction with the `ignore` array. During background estimation, ignored columns are lifted out of the way so the background baseline does not follow artificial valleys caused by detector gaps or mask sentinels.
4. After convex-hull subtraction, fill ignored output samples by linear interpolation between the nearest non-ignored neighbors on the already background-subtracted curve.

This interpolation is applied only inside the active hull range (`rmin` to `rmax`). Values outside that range remain zero-padded as before.

This behavior is intentionally scoped to Equator. The shared `convexHull()` helper still has its original default behavior for other modules. Equator keeps using `convexHull(ignore=...)` to protect the background estimate, then repairs the ignored samples in the returned background-subtracted profile so peak fitting sees a continuous curve instead of a hard zero cut.

### Optional: Inpainting

If **inpainting** is enabled (a separate option in the processing panel), masked pixels are filled by the `pyFAI` inpainting algorithm before the analysis runs. Inpainting estimates each sentinel pixel's value by interpolating from surrounding valid pixels. This can be useful for narrow gaps where projection-based interpolation is insufficient.
