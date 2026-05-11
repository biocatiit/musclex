# Empty Cell Image and Mask

This dialog provides two independent settings: **Empty Cell Image** (formerly called "blank image") and **Mask**. Both are configured through the **Apply Empty Cell Image and Mask** panel in the processing workspace. Each has its own button to open its respective dialog, and each can be independently enabled or disabled via a checkbox once its settings have been saved.

## Empty Cell Image

An empty cell image (also called a blank image) is a diffraction pattern recorded without a sample — only the sample holder, solvent, or beam path. A diffraction pattern from a muscle is the sum of the muscle signal and this empty cell background. Subtracting the empty cell image isolates the muscle diffraction alone, which is important for accurate peak fitting and intensity ratios.

### Opening the Dialog

Click **Set Empty Cell Image** in the processing workspace panel. This opens the Empty Cell Subtraction dialog.

### Selecting an Image

Click **Select Empty Cell Image** to browse and select a single empty cell image file. Any format supported by `fabio` is accepted (e.g., `.tif`, `.edf`, `.cbf`). Once loaded, a status indicator turns green confirming the image has been loaded.

> **Note:** Earlier versions of the software supported selecting multiple images that were averaged together. The current version accepts a single empty cell image file.

### Scale Factor

The **Empty Cell Image Scale** spin box (range 0–1000, default 1.0) scales the empty cell image before subtraction. Because the empty cell exposure may differ from the sample exposure in terms of beam intensity or collection time, you can use this factor to match the two. Setting it below 1.0 reduces the subtraction amount; above 1.0 increases it.

The difference image updates live as you change the scale.

### Compare Panel

Three radio buttons let you switch what is displayed in the viewer:

| Option | Description |
|---|---|
| **Difference Image (Original − Empty Cell)** | Shows the result of the subtraction at the current scale factor. This is the image that will actually be processed. |
| **Original Image** | Shows the raw sample image with no subtraction. |
| **Empty Cell Image** | Shows the empty cell image scaled by the current factor. |

The viewer also provides full intensity and display controls (vmin/vmax, log scale, colormap) via the Display Options panel on the right.

### Saving

Click **Save** to write the configuration to `blank_image_settings.json` in the settings folder. The **Apply Empty Cell Image** checkbox in the main panel will become enabled. Uncheck it at any time to temporarily disable subtraction without deleting the configuration.

---

## Mask

A mask is a binary image that marks which pixels should be ignored during processing. Masked pixels are excluded from histogram projection, peak fitting, and all downstream calculations. The mask dialog offers three complementary masking methods that can be combined.

### Opening the Dialog

Click **Set Mask** in the processing workspace panel. This opens the Set Image Mask dialog.

### Mask Options

All three methods live in the **Mask Options** group. Their combined result is previewed as a color overlay on the image in real time.

#### 1. Drawn Mask

Click **Draw Mask** to launch the `pyFAI-drawmask` tool. This opens an external interactive drawing window where you can paint arbitrary regions to mask using geometric tools (polygon, rectangle, brush, etc.).

After closing `pyFAI-drawmask`, the mask is loaded automatically. The status line below the checkbox confirms whether a drawn mask file is available. Enable the **Drawn Mask** checkbox to include it in the combined mask.

> Drawn mask regions are shown as a **red** overlay.

#### 2. Low Mask Threshold

Enable the **Low Mask Threshold** checkbox to mask all pixels whose intensity is **below** the specified value. The default threshold is −0.01, which is appropriate for masking detector gaps or dead pixels that have been assigned a negative sentinel value.

The threshold spin box range is −50 to 10 000.

Optionally enable **Enable Mask Dilation** and choose a kernel size (3×3, 5×5, or 7×7) to expand the low-threshold mask outward by one pass of morphological erosion. This is useful for catching border pixels adjacent to sensor gaps that may be partially affected.

> Low-threshold masked regions are shown as a **green** overlay.

#### 3. High Mask Threshold

Enable the **High Mask Threshold** checkbox to mask all pixels whose intensity is **above** the specified value. The default is 64 000, which targets saturated pixels on typical 16-bit detectors.

The same optional dilation controls (3×3 / 5×5 / 7×7 kernel) are available.

> High-threshold masked regions are shown as a **blue** overlay.

### Color Legend

| Color | Meaning |
|---|---|
| Green | Low Mask Threshold |
| Blue | High Mask Threshold |
| Red | Drawn Mask |
| Purple | Rmin / Rmax mask (applied by the main processing window) |

### Saving

Click **Save** to write the mask configuration. The following files are created in the settings folder:

- `mask_config.json` — stores the threshold values and dilation kernel sizes.
- `drawn-mask.edf` — the raw output from `pyFAI-drawmask` (if a drawn mask was created).
- `mask.tif` — the final combined binary mask (all active methods merged). This is the file that is actually applied during processing.

If all three mask methods are disabled when you click Save, all mask files are removed and the mask is cleared entirely.

The **Apply Mask** checkbox in the main panel will become enabled once a `mask.tif` exists. Uncheck it to temporarily disable the mask without deleting the saved settings.

---

## How Masked Pixels Are Treated During Processing

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

---

## Applying Settings During Processing

In the processing workspace, the **Apply Empty Cell Image and Mask** panel shows both checkboxes:

- **Apply Empty Cell Image** — enabled when `blank_image_settings.json` exists.
- **Apply Mask** — enabled when `mask.tif` exists.

Either or both can be checked/unchecked independently at any time. The program re-processes the current image immediately when a checkbox state changes.

## Persistent Storage

All files are saved under the `settings/` subdirectory that the program creates next to the image directory being processed. When you reopen the same directory, the saved empty cell image path, scale factor, and mask configuration are loaded automatically and the checkboxes are restored to their last saved state.
