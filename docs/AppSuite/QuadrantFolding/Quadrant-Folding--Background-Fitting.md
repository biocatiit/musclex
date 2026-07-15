# Background Fitting (Parametric, Iterative 2D)

Parametric fitting models the diffuse background as an explicit 2D function and subtracts it, in contrast to the non-parametric methods (CCP13, convex hull, top-hat) documented on [Background Subtraction](Quadrant-Folding--Background-Subtraction.md), which estimate the background numerically. It targets the two structures that dominate a folded muscle pattern: the anisotropic **equatorial streak** near the beam and the broad, near-isotropic **general background**. Use it when the equatorial streak leaks into the low-angle equatorial reflections, or when a smooth analytic background that does not follow the peaks is preferable.

Fitting runs in the **Iterative 2D Background Fitting** window, opened from the **Parametric Background Fitting** panel in the Results tab (or from the Background Subtraction Settings dialog). It operates on the quadrant-folded image; on apply, the residual (image minus fitted background) replaces the current result.

## Model

The fitted background is the sum of an equator component and a general component.

**Equator component** — an elliptical model of the equatorial streak, fit over an equatorial band with the beam and the equatorial Bragg peaks masked out. It resolves into up to three lobed sub-peaks (equatorial streak, lamellar, fibrillar), each with an amplitude, a radial position and width, an angular position and width, and a shape exponent.

**General component** — an isotropic, slightly elongated background of the form `exponential + component 2 + constant baseline`:

- **Component 1** is a fixed exponential.
- **Component 2** is selectable: `lorentzian`, `powerlaw`, `stretched`, or `auto`. With `auto`, the first round tries all three kernels and keeps the one with the best anti-oversubtraction score, then pins it for the remaining rounds.
- **Component 3** is a constant baseline.

## How the fit runs

Fitting is alternating block-coordinate descent. An optional **step 0** seeds the first equator fit with a rough projection background (a per-sector cone-model fit reconstructed by 2D convex hull). Each round then alternates:

1. Fit the equator to (image − current general background).
2. Fit the general background to (image − current equator).

The general fit penalizes oversubtraction (going below the data). After the configured number of rounds, the fit selects the best iteration: the one with the fewest oversubtracted pixels among the rounds whose equator formed the expected two lobes. If no round forms two lobes, the fit falls back to the least-oversubtracted round and warns; inspect the fitted equator before applying.

Fitting is done on a center-crop of size `2 x fit size` and optionally downsampled for speed; the fitted background is reconstructed at full resolution.

## Masks

The fit uses a general mask and an equator mask, built from the same evaluation-mask parameters as the [non-parametric optimizer](Quadrant-Folding--Optimization-Settings.md#step-1-adjust-image-settings-and-process) and mirrored into the fitting window under **Mask Parameters**. Editing a parameter here updates the matching Background Subtraction control and rebuilds the masks; select **General mask** or **Equator mask** in the View dropdown to inspect them before running.

- **Equator Band Width** — half-height of the equatorial band kept for the equator fit.
- **Beam Center Radius** — radius of the central beam removed from both masks.
- **Layer line spacing (M1)** and **Layer line width** — mask the Bragg layer lines so they do not bias the general fit.
- **Rmax** — outer radius of the rmin..rmax annulus used for the masks and for every oversubtraction measurement.
- **Equator peaks** and **Equator peak width** — number and width of equatorial Bragg peaks detected and removed from the equator fit (local to this window). Set **Equator peaks** to 0 to mask none.

## Additional settings

Under **Additional Settings** (collapsed by default):

- **Number of rounds** — equator/general alternating rounds.
- **Equator max fit iters** / **General max fit iters** — least-squares evaluation limits per stage.
- **Fit size (rmax\*)** — fit radius; the image is cropped to twice this value. Lower is faster, as long as it still covers the data. Defaults to 0.8 × rmax.
- **Downsample** — downsample factor during fitting.
- **Use step-0 projection background** — seed the first equator fit as described above.
- **General reduction** / **Equator reduction** — scale each fitted background down by this fraction before subtracting, to guard against oversubtraction. The general reduction is always applied (default 5%). Editing a reduction after a fit rebuilds the background and residual with no re-fit.
- **Auto-reduce** — increase both reductions on top of the fixed values until the oversubtracted-pixel fraction stops improving. After a fit the reduction spinboxes show the values actually used.

## Views and fitted parameters

The View dropdown shows **Original**, **Fitted background (equator+general)**, **Equator component**, **General component**, **Residual (background removed)**, **Equator / Meridian profiles**, and the two mask overlays. Before a fit, only the mask overlays are available. The **Fitted parameters** panel reports the selected iteration, the component-2 kernel, the oversubtracted-pixel fraction over rmin..rmax, the reductions used, and the full equator and general parameter values.

## Applying and running for a folder

- **Apply (use residual) & Close** replaces the current result with the residual and enables **Subtract fitted before non-parametric**, so a non-parametric method can then run on top of the fitted residual.
- **Run Fitting with current setting and apply** (Parametric Background Fitting panel) runs the fit headlessly with the current settings and applies it in one click, without opening the window.
- **Fit background for each image in folder** fits and subtracts the parametric background for every image during folder processing, using the current fitting parameters, instead of reusing a single applied fit.

Every applied fit is saved to `qf_results/bg_fit_params/`: the equator, general, and residual images as TIFFs, and the fit parameters as `<image>_bgfit_params.npz`. Fit parameters are also cached per image, so reopening the window on a processed image reloads the applied fit without re-running it.

## Headless mode

Parametric (iterative 2D) fitting is controlled by `bgfit_*` flags mirroring the fitting window: `bgfit_comp2`, `bgfit_iters`, `bgfit_eq_max_nfev`, `bgfit_gen_max_nfev`, `bgfit_fit_size` (the fit radius; doubled internally for the crop), `bgfit_downsample`, `bgfit_use_step0`, `bgfit_general_reduction`, `bgfit_equator_reduction`, and `bgfit_auto_reduce`. These are written when settings are saved from the GUI and are used for per-image fitting during batch runs when **Fit background for each image in folder** is enabled.

See [How to use — Headless Mode](Quadrant-Folding--How-to-use.html#headless-mode) for the general headless workflow.

## Related topics

- [Background Subtraction](Quadrant-Folding--Background-Subtraction.md) — Processing options, manual and transition modes, and the subtraction method reference.
- [Optimization Settings](Quadrant-Folding--Optimization-Settings.md) — Advanced Configuration dialog, including the evaluation-mask parameters shared with this fitting window.
- [How it works](Quadrant-Folding--How-it-works.html) — Full processing pipeline including merge and result image generation.
