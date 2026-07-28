# Background Subtraction

Quadrant Folding estimates and removes diffuse background scattering from the quadrant-folded image. Several algorithms from the [CCP13 FibreFix](http://www.diamond.ac.uk/Beamlines/Soft-Condensed-Matter/small-angle/SAXS-Software/CCP13/FibreFix/FibreFix.html) suite are available, along with 2D convex hull and white top-hat methods. Methods can be used alone, combined across radius (inner/outer merge), or chosen automatically using a quantitative **loss** score. A separate parametric fitting mode models the background as an explicit 2D function instead of estimating it numerically.

For the algorithmic details of each method, see [How it works — Search and apply background subtraction](Quadrant-Folding--How-it-works.md#9-search-and-apply-background-subtraction-optional). This page documents the **GUI workflow** (Results tab and **Background Subtraction Settings** dialog). Two companion pages cover the two areas that require the most parameter-level detail:

- [Optimization Settings](Quadrant-Folding--Optimization-Settings.md) — the **Advanced Configuration** dialog used by Automated Processing: image settings, evaluation masks, metric weights, results, and batch configuration.
- [Background Fitting](Quadrant-Folding--Background-Fitting.md) — the parametric (iterative 2D) equator + general background model and its fitting window.

## Introduction

Muscle diffraction patterns carry a diffuse background whose structure varies with radius and angle. The choice of subtraction method therefore depends on what you need from the analysis — display, peak fitting, or downstream normalization — and on which part of the pattern matters most.

In principle, background should be removed consistently across the entire folded image. In practice, no single algorithm performs equally well at all radii: some methods preserve low-angle equatorial features while others handle the high-angle region more reliably. **2D Convexhull** and **2D Iterative Parametric Fitting** are currently the strongest options for revealing equatorial structure near the beam. At larger radii, where meridional reflections and layer lines dominate, **White-top-hats** and **Smoothed-Gaussian** typically give cleaner results; **Automated Processing** can search among these methods and select parameters using the compound **loss** metric.

To obtain plausible background removal over the full pattern with the non-parametric methods, use **Manual Setting | Transition**, which fits an inner method at small radii and an outer method at large radii, then merges the two estimates at a **Transition Radius**. As an alternative, [parametric (iterative 2D) background fitting](Quadrant-Folding--Background-Fitting.md) models the equatorial streak and the general background as explicit 2D functions and subtracts them, optionally before a non-parametric method runs on the residual.

The integrated intensity of the estimated background image can be used to normalize measured diffraction-feature intensities across a series. Select **Manual Setting | One Method**, set R-min and R-max to exclude unwanted regions, and apply the same method and parameters to every frame. `summary.csv` records the integrated background sum between R-min and R-max for each frame.



See [Examples](#examples) for more details and the [Use cases](#use-cases) section for specific scenarios.

## Use cases

| Use case | Approach |
|---|---|
| I want to reveal equatorial structure near the beam. | [Manual Setting \| One Method](#manual-setting--one-method) with **2D Convexhull**, or [parametric background fitting](Quadrant-Folding--Background-Fitting.md) which is significantly more time consuming but provides a smoother background. |
| I want to normalize measured intensities across a series of frames using the background level. | [Manual Setting \| One Method](#manual-setting--one-method) with fixed **R-min**/**R-max** applied uniformly to every frame; read the integrated background sum from the summary csv. The method may be set manually (e.g. 2D Convex Hull) or be the one chosen after [Automated Processing](#automated-processing). |
| I need a clean background across the whole pattern, including both equatorial and high-angle meridional features relatively fast (e.g. for visualization). | [Manual Setting \| Transition](#manual-setting--transition): **2D Convexhull** for the inner radii, **White-top-hats** or **Smoothed-Gaussian** for the outer radii, blended at the **Transition Radius** |
| I need a clean background across the whole pattern, including both equatorial and high-angle meridional features. I want a smooth analytic background and can wait for parametric fitting. | [Parametric (iterative 2D) background fitting](Quadrant-Folding--Background-Fitting.md), optionally followed by a non-parametric method on the residual (**Subtract fitted before non-parametric**). |
| I'm working at high angles, where meridional layer reflections and lines dominate. | **White-top-hats** or **Smoothed-Gaussian**, applied manually or via [Automated Processing](#automated-processing). |
| I don't know which method or parameters suit this dataset. | [Automated Processing](#automated-processing) — click **Apply Default Optimization** for a good starting point, then refine in [Optimization Settings](Quadrant-Folding--Optimization-Settings.md). For [parametric fitting](Quadrant-Folding--Background-Fitting.md), set the general component's **Component 2** to `auto` so the fit tries all available kernels and keeps the best one. |
| I need to batch-process a folder with visually different images (e.g. mixed muscle types). | Automated Processing: build a few [saved configurations](Quadrant-Folding--Optimization-Settings.md#step-3-batch-processing) and let **Choose best configuration for images automatically** pick per image. |
| I need to reproduce a GUI-tuned result from the command line / a script. | [Headless mode](#headless-mode) with the saved `qfsettings.json`. |
<!-- | Automated results look good on most images but poor on a few outliers. | Enable **Automatically create new configurations for outlier images** in [Optimization Settings — batch processing](Quadrant-Folding--Optimization-Settings.md#step-3-batch-processing), or assign configurations manually. | -->


## Where to find the controls

Background subtraction is configured in the **Results** tab:

<img src="../../images/QF/background_sub_panel.png" alt="img" width="400"> 

**Background Subtraction** panel (collapsible section on the right) — quick access to mode, method parameters, apply buttons, and current configuration summary.

## Non-parametric Subtraction Processing Options


Under **Non-parametric Background Subtraction**, use the **Options** dropdown in the Results tab to choose how background is applied.

<img src="../../images/QF/background_sub_non_param.png" alt="img" width="400">

| Option | Description |
|--------|-------------|
| **Manual Setting \| One Method** | A single subtraction method on the full pattern (inner region). Method and parameters are set in the panel; click **Apply Selected Subtraction Settings** to reprocess. |
| **Manual Setting \| Transition** | One method for radii below the transition radius (inner) and another for radii above (outer). Results are merged using **Transition Radius** and **Transition Delta**.|
| **Automated Processing** | Parameter search and/or selection from saved configurations using the compound **loss** metric. Use **Apply Default Optimization** on the current image, then tune settings in **Advanced Configuration**. |



### Manual Setting \| One Method

1. **Subtraction Method** — Select the background subtraction method to use.
2. **Method-specific parameters** — Set the method-specific parameters.
3. **Apply Selected Subtraction Settings** — Click to apply the selected subtraction settings.

Set the method and parameters to use for the background subtraction.

### Manual Setting \| Transition

1. **Inner background** — Method and parameters for radii inside the transition region (e.g. 2D Convexhull).
2. **Outer background** — Separate method and parameters for larger radii (e.g. Smoothed-Gaussian).
3. **Transition Radius** — Radius where inner and outer estimates are blended.
4. **Transition Delta** — Width of the linear blend zone.
5. **Show Transition Radius and Delta** — Overlay transition circles on the folded image.

Set the transition radius just outside the M3 meridional peak when possible.

## Automated Processing
**Apply Default Optimization** runs this search with default methods on the current image and can add a **Default Optimization** entry to the configuration table to be applied to subsequent images.
**Advanced Configuration** adjusts the optimization target and lets you add reusable configurations to the configuration table.

<img src="../../images/QF/bg_sub_automated.png" alt="Automated Processing controls" width="400">

## Recommended workflow using Automated Processing

### Step 0: Apply Default Optimization

Click **Apply Default Optimization** to search **White-top-hats** and **Smoothed-Gaussian** using the default loss weights. QF selects the best method and parameters and can add a **Default Optimization** entry to the configuration table. Review the result and a representative next image before processing the folder or selected batch. If the result is unsatisfactory, tune the search in **Advanced Configuration**.

The **Background Subtraction Settings** dialog is organized in three steps:

### Step 1: Adjust image settings and process

- **R-min/R-max** — Define the radial range used for masking and fitting. R-min excludes the backstop; R-max limits the outer edge of the pattern. Use **Manual R-min/max** to set values on the image, **Show R-min/max** to overlay circles, and **Persist R-min/max** to reuse values when switching images.
- **Image Processing** — **Downsample** (1, 2, or 4) speeds optimization and smooths the background; **Smooth Image** optionally smooths the folded image before subtraction using an edge-preserving smoothing algorithm (OpenCV's guided filter). By default, the image is downsampled by 2 and the background is smoothed.
- **Subtraction** — In the dialog, choose **Manual** or **Automated** processing mode:
  - **Manual**: Select **Subtraction Method** and method-specific parameters (same methods as in the Results panel).
  - **Automated**: Multi-select **BG Subtraction Methods**, set **Step Sizes** (comma-separated schedule, e.g. `100, 50, 25, 10, 5, 3, 1`), **Max Iterations** per parameter, and **Early Stop Loss Threshold**.
- **Evaluation Masks** - Adjust the evaluation masks to restrict scoring to physically meaningful regions:
  - **Equator Height** and **Equator Center Radius** — mask the equator and central beam.
  - **Layer line spacing** and **Layer line width** — mask Bragg layer lines so they do not dominate the loss.

:::{warning}
Automatic detection of **Equator Height**, **Equator Center Radius**, **Layer line spacing**, and **Layer line width** is not implemented yet. Set these values manually in **Evaluation Masks** for each dataset.
:::

To view the evaluation masks, click **Show** in the Results tab to inspect **Subtracted**, **Background**, **Folded**, **Evaluation Mask**, **Synthetic Signal**, or **Synthetic Mask**.
- **Metric Settings** - Adjust the relative importance of each metric and the normalization means. The weights should roughly add up to 1. The weights may depend on the dataset and are the most important settings to adjust to get the best results. Leave the default values for the first run and adjust after reviewing the results. The normalization means are hidden by default; double-click the metric table header to show/hide means. Usually, they don't need to be adjusted.

<img src="../../images/QF/metric_setting.png" alt="IMG" width="400">

- **Additional Settings**:
  - **Evaluation Baseline** sets the near-zero threshold; **Persist evaluation baseline** keeps it when changing images. **Evaluation Baseline** allows to adjust the near-zero threshold for the calculation of **Fraction of Non Near-Zero Baseline Pixels**. This is the threshold below which pixels are considered to be part of the background. Change this value if the noise level doesn't match the calculated value.
  - **Synthetic** amplitude and sigmas (and **Sampling Frequency**) define the reference pattern used in MSE and oversubtraction metrics.


<img src="../../images/QF/additional_setting.png" alt="IMG" width="600">


- Click **Apply Selected Subtraction Settings** (dialog or Results panel) to run on the current image. During automated optimization the button becomes **Stop Optimization** which will stop the optimization and return the previous best performing method and parameters. This is useful when you want to tune the settings and rerun the optimization.

### Step 2: Review results

After processing, the **Results** section shows **Loss** and a table of metrics:

| Metric | Meaning | Purpose |
|--------|---------|---------|
| **Normalized MSE of Synthetic Signal** | Normalized mean squared error between the subtracted image and a synthetic meridional reference, inside the evaluation mask. | Measures preservation of the synthetic signal. Lower is better. |
| **Fraction of Synthetic Oversubtraction** | Share of masked pixels where subtraction went below the synthetic reference. | Measure the amount of synthetic signal that is removed. Lower is better. High when the synthetic signal is removed significantly. |
| **Fraction of Non Near-Zero Baseline Pixels** | Share of masked pixels still above the evaluation baseline after subtraction. | Measure the amount of background that is not removed. Lower is better. High when the background is not removed significantly. |
| **Fraction of Negative Connected Pixels** | Share of connected negative regions (oversubtraction artifacts). | Measure the amount of oversubtraction artifacts. Lower is better. High when the oversubtraction artifacts are significant. |
| **Smoothness Metric** | Penalty for roughness in the estimated background. | Measure the smoothness of the estimated background. High when the background is rough. |

**Compound loss** is a weighted sum of normalized metrics. Adjust **Metric Settings** (weights and normalization means; double-click the metric table header to show/hide means).

- **Save result metrics to csv** - Save the result metrics to a csv file. This is useful for further analysis. This will save the result metrics to a csv file in the `qf_results/bg` folder with name `background_metrics.csv`.

<img src="../../images/QF/results_table_2.png" alt="IMG" width="600">

### Step 3: Batch processing

1. When satisfied with settings on a representative image, click **Add Background Configuration** to save the selected method and parameters under a configuration name. Loss is also added for information. Configurations are stored in `qf_cache/background_cache.json` for the folder.
2. Repeat for other parameter sets if needed (e.g. different muscle types).
3. Under **Folder Processing**:
   - **Choose best configuration for images automatically** — For each image in the folder, evaluate all saved configurations and apply the one with lowest loss (typical for heterogeneous datasets). This is the recommended option for most datasets.
   - **Optimize each image** — Run the full configured optimization independently for every image instead of selecting from saved configurations. This is slower but useful when images vary too much for a small configuration set.
   - **Manually assign configurations to images** — Open the assignment dialog to map specific configuration names to filenames (disabled while auto-select is on). This is useful for datasets with a clear separation between different types of images.
4. Click **Process Current Folder** in the dialog for the current folder, or use the navigator's **Process Batch Folder(s)** after selecting multiple folders.

The **Current Configuration** summary in both the dialog and Results panel shows the active method, parameters, and loss after each run.

## Subtraction methods and parameters

Six methods are available (plus **None**). Visible parameters depend on the selected method.

### Circularly-symmetric

- **Pixel Range** (min–max %): lowest-intensity pixels averaged per radial bin (e.g. 0–25% = lowest quarter).
- **Radial Bin** (pixels)
- **Smoothing factor** (spline smoothing)

### 2D Convexhull

- **Step Degree** (angular bin size for radial histograms)
- **R-min** (and **R-max** when set)

### Roving Window

- **Window Size** (X, Y) and **Window Separation** (X, Y)
- **Pixel Range** (%)
- **Smoothing factor** and **Tension factor**

### White-top-hats

- **Top-hat Disk Size**

### Smoothed-Gaussian

- **Gaussian FWHM**
- **Number of Cycles**

### Smoothed-BoxCar

- **Box Car Size** (X, Y)
- **Number of Cycles**

<img src="../../images/QF/bg_sub_transition.png" alt="img" width="800">

## Parametric (iterative 2D) background fitting

Parametric fitting models the diffuse background as an explicit 2D function and subtracts it, in contrast to the non-parametric methods above (Smoothed-Gaussian/Boxcar, 2D Convex Hull, white-top-hats, etc.), which estimate the background numerically. It targets the two structures that dominate a folded muscle pattern: the anisotropic **equatorial streak** near the beam and the broad, near-isotropic **general background**. Use it when a smooth analytic background that does not follow the peaks is preferable. It can be combined with a non-parametric method: after applying a parametric fit, enable **Subtract fitted before non-parametric** so a non-parametric method runs on top of the fitted residual.

Fitting runs in the **Iterative 2D Background Fitting** window, opened from the **Parametric Background Fitting** panel in the Results tab (or from the Background Subtraction Settings dialog). It operates on the quadrant-folded image; on apply, the residual (image minus fitted background) replaces the current result.

<img src="../../images/QF/bg_sub_param_fit.png" alt="img" width="200">


See [Background Fitting](Quadrant-Folding--Background-Fitting.md) for the model, the fitting procedure, mask parameters, and the full settings reference.

## Examples

### Example 1: High Angle Features with Automated Processing - Intact Mouse Skeletal Muscle - Faint pattern & faint background

The default weights are used for the optimization.

![-](../../images/QF/example1_2.png)

### Example 2: Whole Pattern Background Subtraction with Transition - 2D Convexhull for Inner and Smoothed Gaussian for Outer - Skinned Pig Cardiac Muscle - High intensity background

This example shows the use of transition mode to remove background on the whole pattern. This is preferable for visualization of patterns where the equatorial features are important. The transition radius and delta are shown in yellow and red.

![-](../../images/QF/transition.png)

## Headless mode

Background subtraction parameters can be set in `qfsettings.json`. Important keys include `bgsub`, `bgsub_out`, `transition_radius`, `transition_delta`, `fixed_rmin`, `fixed_rmax`, method-specific fields (`fwhm`, `cycles`, `win_size_x`, `win_sep_x`, etc.), and `bg_options` (0 = one method, 1 = transition, 2 = automated). For automated batch behavior, also pass saved configurations and flags such as `choose_configurations_auto` as produced by saving settings from the GUI (**File → Save current settings**). See [Optimization Settings](Quadrant-Folding--Optimization-Settings.md) for the settings behind the automated search, and [Background Fitting — Headless mode](Quadrant-Folding--Background-Fitting.md#headless-mode) for the `bgfit_*` keys used by parametric fitting.

See [How to use — Headless Mode](Quadrant-Folding--How-to-use.md#headless-mode) for the general headless workflow.

## Related topics

- [How it works](Quadrant-Folding--How-it-works.md) — Full processing pipeline including merge and result image generation.
- [Optimization Settings](Quadrant-Folding--Optimization-Settings.md) — Advanced Configuration dialog: image settings, evaluation masks, metric weights, results, and batch configuration.
- [Background Fitting](Quadrant-Folding--Background-Fitting.md) — Parametric (iterative 2D) equator + general background model.
- [Common Settings — Empty Cell Image and Mask](../Common-Settings.md#empty-cell-image-and-mask) — Empty cell correction before folding.
