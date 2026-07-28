# Version-2.1.0

Release Date: July 2026

Version 2.1.0 expands calibration and multi-folder processing, introduces
parametric background fitting for Quadrant Folding, and improves alignment and
Projection Traces workflows.

---

## New Features

### Calibration and Geometry Refinement

- Calibration images can now identify detector models from FabIO metadata or
  image dimensions and use the matching PyFAI detector metadata to populate
  pixel size.
- Manual detector selection remains available when automatic identification is
  unavailable or needs to be overridden.
- Beam energy is supported in calibration settings and is saved with the
  calibration metadata.
- Existing calibration information can be imported into the calibration
  dialog.
- Quadrant Folding adds calibration-refinement controls for refining geometry
  from the current diffraction image.
- Alignment tools now share and persist the authoritative geometry settings,
  including geometry updates made during batch alignment.

### Quadrant Folding – Parametric Background Fitting

- Added an **Iterative 2D Background Fitting** workflow for modeling and
  subtracting diffuse background from quadrant-folded images.
- The fitted background combines an anisotropic equatorial component with a
  smooth general component. General-component kernels include Lorentzian,
  power-law, stretched, and automatic selection modes.
- Alternating fitting rounds select the result with the best
  anti-oversubtraction behavior.
- Dedicated mask controls exclude the beam center, layer lines, equatorial
  peaks, and regions outside the selected radial range.
- Preview modes display the fitted equator, general background, combined
  background, residual, profiles, and fitting masks before the result is
  applied.
- General and equatorial reduction controls help protect diffraction features
  from oversubtraction; an automatic reduction option is also available.
- Applied fits and parameters are saved per image and restored from cache in
  later sessions.
- A one-click action can fit and apply the background with the current
  settings, and batch processing can fit each image independently.
- Parametric fitting can be followed by the existing non-parametric background
  subtraction methods for a two-stage workflow.

### Quadrant Folding – Intensity Corrections

- Added optional solid-angle correction before quadrant folding.
- Added optional X-ray polarization correction with selectable incident-beam
  polarization modes.
- The applied correction state is recorded with the processing results.

### Multi-Folder Batch Processing

- Added an ordered batch-folder selection dialog for choosing multiple dataset
  folders under a parent directory.
- Selected folders can be reordered before processing.
- Quadrant Folding and Projection Traces can process the selected folder batch
  while preserving the correct output path for each source folder.
- Quadrant Folding alignment can inspect images across the selected batch
  folders and display their source folder in the alignment table.

### Projection Traces

- Manual peak selection can mirror selected peak positions around the center,
  reducing repetitive selection for symmetric patterns.
- Images can be marked as rejected from either the image or fitting view.
- Rejection state is synchronized across tabs, persisted with the image state,
  and written to the CSV output.
- Projection geometry now has an explicit signature so cached results are
  invalidated when relevant geometry changes.

---

## Improvements

### Quadrant Folding

- Background-fitting parameters, masks, radial limits, and applied-fit state
  are restored more consistently when changing images or reopening a dataset.
- Background configurations are invalidated when a newly applied fit changes
  the input used by non-parametric subtraction.
- Automated fitting can detect initial M1 layer-line spacing and equator height.
- The selected component-2 kernel is reflected in the fitted result.
- Background optimization now gives a clear completion message and preserves
  the last valid result when processing is stopped.
- Background metrics include additional diagnostic information for synthetic,
  baseline, and mask settings.
- Calibration-center handling and alignment-folder display were corrected.

### Alignment and Batch Workflows

- Symmetry checking is enabled by default in the shared image-alignment
  workflow and in the Add Intensities applications.
- Alignment tables refresh after geometry refinement.
- Output-directory associations can be applied for the current operation or
  persisted for later sessions.
- Parent output directories are created automatically when processing
  multi-folder batches.

### Equator

- Results for the current image refresh immediately after processing or
  calibration refinement.
- Original Equator settings metadata is included in summary output.

### User Interface

- Calibration terminology now uses **Calibrant ring d-spacing** in place of
  the material-specific **Silver Behenate** label.
- Background-subtraction controls, labels, units, help buttons, and
  documentation links were reorganized for a clearer workflow.
- Processing-completion and cancellation handling was improved.

---

## Bug Fixes

- Fixed background-sum calculation during Quadrant Folding processing.
- Fixed stale cached Quadrant Folding results when slow-path processing inputs
  change.
- Fixed selected-folder output paths in Equator and Projection Traces.
- Fixed image discovery in multi-folder processing by using absolute paths.
- Fixed DDF Processor downsampling affected by true division and improved its
  reading animation and empty-range handling.
- Fixed loading and persistence of background-fitting masks and radial limits.
- Fixed several empty-image and missing-result edge cases in background result
  displays.

---

## Documentation and Testing

- Added complete documentation for parametric background fitting and advanced
  background-optimization settings.
- Updated the Quadrant Folding background-subtraction workflow, screenshots,
  labels, and cross-references.
- Updated Sphinx/MyST configuration so embedded HTML images render correctly.
- Added pull-request tests covering the MuscleX summary suite, Python syntax,
  unit tests, GUI regressions, and fitting-adapter smoke tests.
- Added regression tests for calibration settings and refinement, batch-folder
  selection, output-directory behavior, alignment geometry, cache
  invalidation, Projection Traces rejection, and mirrored peak selection.

```eval_rst
.. note:: MuscleX 2.1.0 release packages are built with Python 3.10.
```
