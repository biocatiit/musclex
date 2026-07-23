# How it works

When an image is selected, the program immediately processes it automatically. The resulting image is saved as `[image_name]_folded.tif` (or `[image_name]_folded_compressed.tif` when **Save Compressed Image** is enabled) in the `qf_results` folder under the output directory.

## Prerequisites

Before the pipeline runs, the input image and processing geometry are resolved:

**Empty cell subtraction and masking** — if configured, the blank image is subtracted and invalid pixels are marked with a sentinel value of −1 by `ImageData`. This happens before any QF-specific processing. See [Common Settings — Empty Cell Image and Mask](../Common-Settings.md#empty-cell-image-and-mask).

**Diffraction center and rotation angle** — resolved from user input, calibration, cached settings, or automatic detection, and always resolved even on a cached reload. See [Center and rotation](#center-and-rotation) below.

**Calibration geometry** — optional intensity corrections use the sample-to-detector distance and detector pixel size from Calibration Settings. QF converts this geometry to SDD in pixels. If it cannot resolve a positive value, the requested corrections are skipped and the uncorrected image continues through the pipeline.

**Caching** — if the image has already been processed with the same settings, the program reloads the saved result from disk instead of reprocessing. Center, rotation, calibration geometry, and intensity-correction choices participate in cache handling, so changing them invalidates dependent results. To force reprocessing, delete the `qf_cache` folder in the output directory before opening the image, or use the folder/batch processing control.

## Center and rotation

Every run needs a diffraction center — the beam position, where the scattering vector is zero — and a rotation angle that brings the equatorial axis onto the horizontal axis the pipeline assumes. These can come from the user or be detected automatically; the resolved values drive the [transform step](#2-transform-image) and the on-screen overlays.

### Where the values come from

For each image the center and rotation are taken, in order of precedence, from:

- **User input** — values set with the Set Center / Set Rotation tools (Quick Center and Rotation Angle, Chords, Perpendiculars, Manual, Set Angle Interactively / Manually) or copied across images with Apply Center / Apply Rotation. A user-set value is flagged as manual and preserved across cache reloads until Restore Auto is used.
- **Calibration** — a fitted calibrant ring supplies the center (and the pixel-to-nm scale); a fixed center can also be pinned independently of the calibrant.
- **Cache** — values saved from a previous run, reloaded for speed and reproducibility.
- **Automatic detection** — used when none of the above applies.

See [Common Settings — Diffraction Center and Rotation](../Common-Settings.md#diffraction-center-and-rotation) for the full tool descriptions and [How to use — Step 3](Quadrant-Folding--How-to-use.md#step-3-set-center-and-rotation-on-a-reference-image) for the workflow.

### Automatic detection

The center is found by converting the image to 8-bit, Gaussian-smoothing it, thresholding, and fitting an ellipse to the largest contour; the ellipse center is the diffraction center (falling back to image moments if no ellipse fits). The rotation angle is seeded from that same ellipse and refined by a two-pass azimuthal integration (pyFAI): a 1° scan to find the dominant meridional peak, then a 0.1° scan around it. The orientation algorithm is configurable — Max Intensity, GMM, or the Herman factor over [0, π/2] or [0, π] — via **Set Auto Orientation**, which can also lock the whole folder to the most common (mode) angle.

### Refinement

**Refine Center** and **Refine Rotation** improve an existing center and angle rather than finding them from scratch, by maximizing the symmetry between the four quadrants. Both minimize a **fold-symmetry loss**: the image is centered, rotated, and folded, and the loss is the summed per-pixel standard deviation across the overlapping quadrants divided by the total foreground signal (foreground separated from background by Otsu thresholding), which makes it dimensionless and exposure-independent. A lower loss means the quadrants agree more closely.

Refine Center runs up to three stages in sequence, each refining the previous result: ECC image **registration** of opposite quadrants, **gradient** minimization of the loss, then a coarse-to-fine local **search** over center and angle. Refine Rotation runs the angle-only search with the center held fixed. Masked pixels (beam stop, dead regions) are excluded so they do not bias the score. For the button-level workflow, see [How to use — Step 3](Quadrant-Folding--How-to-use.md#step-3-set-center-and-rotation-on-a-reference-image).

## Processing pipeline

### 1. Apply intensity corrections (optional)

When **Solid Angle** or **Polarization** is enabled, QF constructs a multiplicative correction from the calibrated SDD in pixels and each pixel's radius and azimuth about the diffraction center. The correction is applied to the original image before it is centered, rotated, or folded.

- **Solid Angle** divides by the flat-detector relative solid angle, proportional to `cos(2θ)³`.
- **Polarization** divides by the Thomson-scattering polarization factor for the selected **Unpolarized**, **Horizontal**, or **Vertical** incident-beam model.

The correction status records which operations were applied and the SDD used. If calibration geometry is unavailable, QF records a skipped status and passes the original intensities to the next step unchanged.

### 2. Transform image

The diffraction center is generally not at the pixel center of the image. If folding were done directly, the four quadrants would have unequal sizes and the folded image would be cropped.

![-](../../images/QF/incorrect_centerize3.png)

To fix this, the program applies a single composite affine transform that translates the diffraction center to the image center and rotates the image by the resolved rotation angle, both in one `warpAffine` call. After the transform the center is at `(width/2, height/2)` and the equatorial axis is horizontal.

![-](../../images/QF/centerize3.png)

### 3. Calculate average quadrant fold

The transformed image is split into four quadrants about the (now-centered) diffraction center. Each quadrant is flipped to the same orientation as the top-left quadrant.

![-](../../images/QF/split_folds.png)

The four quadrants are then averaged pixel by pixel. Pixels with intensity ≤ −1 (detector gaps, masked regions) are excluded from the average at each position. Any quadrants the user has excluded via **Ignore Folds** are also skipped. The result is a single quarter-image (`avg_fold`) with improved signal-to-noise.

![-](../../images/QF/avg_fold2.png)

### 4. Calculate R-min and R-max

R-min and R-max define the radial range used for background subtraction.

A 1D azimuthal integration histogram is computed from `avg_fold` using pyFAI. The backstop shadow appears as the first peak in this histogram. R-min is set to that peak position + 5 pixels. R-max is set to the detector edge − 20 pixels. Both can be overridden manually.

![-](../../images/QF/rmin.png)

### 5. Create analysis mask (used for Background Subtraction)

A composite mask is built from five components, combined by element-wise multiplication:

- **R-min / R-max annulus** — excludes pixels inside R-min (backstop region) and outside R-max (detector edge).
- **Equator band** — a horizontal strip centered on the equator, sized automatically from the FWHM of the meridional projection and the M1 layer-line position.
- **Equatorial peaks** — small box exclusions placed on the equator at the positions of the N most prominent equatorial peaks (and their mirror positions), sized from the peaks' average FWHM.
- **Beam exclusion** — a small circular exclusion at the center to remove residual direct beam.
- **Layer lines** — horizontal band exclusions at the expected layer-line positions (multiples of M1), sized from the meridional peak FWHM.

This mask is used by the background optimizer to evaluate subtraction quality on the diffraction features rather than on background regions.

For GUI workflow (manual, transition, and automated modes), saved configurations, and batch processing, see [Background Subtraction](Quadrant-Folding--Background-Subtraction.md).

### 6. Create synthetic data (used for Background Subtraction)

A grid of 2D Gaussian blobs is generated and added to `avg_fold` to produce `avg_fold_with_syn`. The grid spacing and blob dimensions are derived from the I<sub>1,0</sub> and M1 peak positions detected in the pattern. Three density presets are available (sparse / medium / dense).

This synthetic data gives the background optimizer a known signal to protect: a good background subtraction should preserve the synthetic peaks while removing diffuse background.

### 7. Smooth fold (optional; used for Background Subtraction)

If **Smooth Image** is enabled, a guided filter (radius = 7, ε = 1) is applied to `avg_fold` before background search. This reduces pixel-level noise without blurring diffraction features. The smoothing only affects the resulting background, since the smooth estimated background is subtracted from the original average fold `avg_fold`.

### 8. Downsample (optional; used for Background Subtraction)

If a downsample factor > 1 is set, `avg_fold` is resized with area interpolation before the background search. R-min, R-max, and the center coordinates are scaled accordingly. The full-resolution fold is used for the final subtraction; downsampling only speeds up the optimizer and makes the resulting background smoother.

### 9. Search and apply background subtraction (optional)

The program selects a background subtraction method (either the one chosen by the user, or the best-scoring method found by the background optimizer) and applies it to `avg_fold` to produce `BgSubFold`. By default, no background subtraction is selected. 

The same method is applied in parallel to the synthetic data for optimizer scoring.

Six algorithms are available:

#### Circularly Symmetric

![-](../../images/QF/cs.png)

Parameters: pixel range (percentages), radial bin size (pixels), spline smoothing factor.

For each radial bin, pixels are sorted by intensity and the mean of the lowest N% is taken as the background value at that radius. The per-bin values are fitted with a smoothing spline and the resulting 1D curve is spread through 360° to produce a 2D background image.

Source: [CCP13 FibreFix](http://www.diamond.ac.uk/Beamlines/Soft-Condensed-Matter/small-angle/SAXS-Software/CCP13/FibreFix/FibreFix.html).

#### 2D Convex Hull

![-](../../images/QF/ch.png)

Parameters: R-min, angle bin size (default 1°).

A 1D radial intensity profile is computed by integrating over each angular bin (1°) across the 90° quadrant. Each profile is smoothed, a 1D convex hull (shrink-wrap) is applied using R-min as the start point, and the hull points are interpolated with the pchip algorithm. The resulting vectors are smoothed and neighborhood-averaged to form a 2D background surface, which is then Gaussian-filtered before subtraction.

![-](../../images/QF/2dconvex_img.png)

#### Roving Window

![-](../../images/QF/rw.png)

Parameters: R-min, window size, window separation, pixel range (percentages), smoothing factor, tension factor.

A window of the specified size is moved across the data. Within each window position, pixels are sorted and those in the user-selected intensity range are averaged to estimate the background at the window center. A smoothing spline under tension fills the gaps between window centers.

Source: [CCP13 FibreFix](http://www.diamond.ac.uk/Beamlines/Soft-Condensed-Matter/small-angle/SAXS-Software/CCP13/FibreFix/FibreFix.html).

#### White Top-Hat

![-](../../images/QF/wth.png)

Parameter: kernel size.

The white top-hat transform `f − f∘b` extracts features smaller than the structuring element `b`. Features that are smaller than the disk kernel and brighter than their surroundings (i.e. diffraction peaks) are retained; the slowly varying background is removed. The kernel is a disk from scikit-image; the transform is applied with `skimage.morphology.white_tophat`.

![-](../../images/QF/tophat_img.png)

#### Smoothed — Gaussian and Boxcar

Parameters: R-min, number of cycles, FWHM (Gaussian) or box size (Boxcar).

![-](../../images/QF/sg.png)

![-](../../images/QF/sb.png)

Based on Ivanova and Makowski (Acta Cryst. A54, 626–631, 1998). The image is repeatedly smoothed (Gaussian or boxcar filter) and the smoothed version is subtracted as the background estimate. Multiple cycles progressively refine the estimate.

#### Background Subtraction Modes

There are three modes for background subtraction: **Manual Setting**, **Transition**, and **Automated Processing**.

#### Manual Setting
In this mode, the user can select one background subtraction method and set the parameters for the background subtraction manually.
#### Transition
In this mode, the user can select one background subtraction method for the inner radii and another background subtraction method for the outer radii, and set the parameters for the two methods. The two background images are then merged at the **transition radius** and **transition delta**, as described in [Section 10. Merge images](#10-merge-images-transition-mode-only). Typically, this approach is applied for 2D Convex Hull on the inner radii, which handles the equatorial background well, and another method for the outer radii, since 2D Convex Hull produces artifacts at high angles.
#### Automated Processing
In this mode, the program will search for the best parameters for the background subtraction using the automated processing. The search for the optimal method is performed using a compound **loss** built from metrics that reflect pattern features: NMSE of synthetic signal, oversubtraction of synthetic signal, baseline residuals, connected negative pixels, and background smoothness. Automated processing also includes the "Advanced Configuration" feature, which allows the user to set the parameters for the optimization target and create background configurations to be reused for subsequent images. Saved **configurations** let you apply the best parameter set per image when processing a folder. See [Background Subtraction (GUI)](Quadrant-Folding--Background-Subtraction.md) for the full interactive workflow.

Generally speaking, the “White Top-Hat” and the “Smoothed-Gaussian” algorithms work well at large radii from the center and, at low radii, some other algorithm will work better than others, depending on the type of muscle generating the X-ray pattern.

### 10. Merge images (transition mode only)

If **Transition** mode is active, a second background subtraction method is applied to `avg_fold` independently (the outer method), producing a second background image (`BgFold_out`) alongside the inner one (`BgFold_in`).

The two **background images** are then linearly blended into a single merged background using `transition_radius` and `transition_delta`: the inner background dominates near the center, the outer dominates at large radii, and a linear ramp interpolates between them over a band of width `transition_delta` centered on `transition_radius`. The merged background is then subtracted from `avg_fold` to produce the final `BgSubFold`.

![-](../../images/QF/transition.png)

The guideline is to set the transition radius just outside the M3 meridional peak.

### 11. Generate result image

The background-subtracted quarter-image `BgSubFold` is expanded to a full 2D pattern by tiling the quarter and its three flipped copies (horizontal flip → top-right, vertical flip → bottom-left, both flips → bottom-right). The resulting full image is then post-processed: NaN values are zeroed; if **Rotate 90** is enabled the image is rotated 90°; if an ROI is set, the image is cropped symmetrically about the center.

![-](../../images/QF/result.png)

### 12. Evaluate result

Several quality metrics are computed and written to `summary.csv`:

- **symmetry** — normalized fold-symmetry score (`fold_std_norm`) computed on the pre-transform image by `_compute_fold_symmetry`. Lower means the four quadrants are more consistent.
- **bgSum** (stored internally as `intensity`) — `np.sum` of the estimated background image. Used for batch consistency checks and normalization.
- **loss** — weighted combination of five metrics used to evaluate background subtraction quality: MSE between the result image and the synthetic-data ground truth (under the analysis mask), share of negative pixels around synthetic peaks, share of result pixels below the radial baseline, share of pixels in connected negative regions, and a smoothness penalty (sum of vertical gradients). Each metric is normalized by its running mean before being weighted. Lower loss is better.
