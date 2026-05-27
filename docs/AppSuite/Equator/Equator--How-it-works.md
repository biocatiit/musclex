# How it works

When an image is selected, Equator applies any configured calibration, center/rotation, empty cell image, and mask settings, then runs the processing pipeline described below. If the image has already been processed with the same MuscleX version and the same preprocessing settings, Equator loads cached results from `eq_cache` and skips recalculation.

Before Equator-specific processing begins, the shared workspace resolves the diffraction center and rotation angle from automatic detection, calibration, manual overrides, or cached settings. See [Common Settings — Diffraction Center and Rotation](../Common-Settings.md#diffraction-center-and-rotation) for details.

## Processing pipeline

Equator processes the image in the following steps:

### 1. Calculate R-min

R-min is calculated using the shared algorithm. See [Common Settings — R-min](../Common-Settings.md#r-min) for details.

### 2. Calculate Box Width

The working image is rotated using the resolved rotation angle, and the area inside R-min is removed. If R-max is set, the area outside R-max is also removed. During rotation the image is expanded as needed so it is not cropped; both square and non-square images are handled.

![-](../../images/BM/boxwidth_1.png)

The box width is initially set to R-min × 1.5. To refine this, the program computes a horizontal histogram of the rotated image, fits a convex hull to estimate the background, and selects the integrated area as the span between the start and end points of the histogram.

![-](../../images/BM/boxwidth_2.png)

### 3. Get Intensity Histogram

With the integrated area determined, the program produces a 1-D intensity histogram by summing columns of the rotated image within that area. If a blank image and mask are configured, the original image is subtracted by the blank image before rotation.

![-](../../images/BM/hist.png)

### 4. Apply Convex Hull to Intensity Histogram

The histogram is split into left and right halves. A convex hull background is estimated independently for each half, using R-min as the starting point, and subtracted to yield a background-free pattern.

![-](../../images/BM/convexhull.png)

During histogram production, columns with values below the mask threshold are treated as ignored. Masked detector gaps are protected during convex-hull background estimation and interpolated before fitting. See [Empty Cell Image and Mask](Blank-Image-and-Mask.md) for details.

### 5. Find Diffraction Peaks

Local maxima are located in both the left and right background-subtracted histograms. All candidate peak positions are recorded. Noisy images may produce more candidates than true diffraction peaks; these are filtered in the next step.

![-](../../images/BM/find_peaks.png)

### 6. Manage Diffraction Peaks

The candidate peak list is validated and corrected before fitting. False positives (peaks from noise) and false negatives (missed reflections) are resolved by searching for the first pair of symmetric peaks to establish S<sub>10</sub> — the center-to-first-peak distance on the equator. All remaining peak positions are then calculated from S<sub>10</sub> and the lattice angles θ(h,k). The user specifies how many peaks per side to fit (default and minimum is 2).

### 7. Fit Model

The background-subtracted histogram is fit using the selected model (Gaussian or Voigtian) with initial parameters derived from S<sub>10</sub> and the integrated reflection areas. The fit returns the area of each peak, S<sub>10</sub>, σ<sub>D</sub>, σ<sub>S</sub>, γ, and I<sub>11</sub>/I<sub>10</sub>.

When only 4 reflections are visible (I<sub>11</sub> and I<sub>10</sub> on each side), the Voigt model is overdetermined. In this case, σ<sub>S</sub> or γ should be fixed to avoid degenerate fits.

## Partial re-runs on manual parameter changes

When a parameter is changed manually, the pipeline does not restart from step 1. Instead, it resumes from the first step that depends on the changed parameter. For example, if Box Width is set manually, the pipeline re-runs from step 3 (Get Intensity Histogram) onward, since the center, rotation angle, and R-min are unaffected.
