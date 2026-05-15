# Version-1.30.0

Release Date : May 2026

## Major Features

### Quadrant Folding – Background Subtraction Overhaul
- New `BackgroundSubtractionDialog`: dedicated popup containing all background subtraction controls, separating them from the main QF window for a cleaner workflow
- New `background_search.py` engine: automated parameter search for background subtraction with optimization cache (`optimization_cache.py`) to reuse best configurations across images
- Manual background assignment dialog (`ManualBackgroundAssignmentDialog`) for per-image overrides
- Background configurations table in the dialog: stores multiple named configurations; best match is auto-selected during batch processing
- Fold symmetry scoring (`fold_symmetry.py`): quantitative metric for evaluating quadrant-fold quality; integrated into `QuadrantFolder` processing and exported to CSV
- Layer-line mask support added to QF processing
- ROI (Region of Interest) support in `QuadrantFolder` with ROI translation for headless processing
- "Save Cropped Image" option removed; only full-size folded images are saved
- QF settings bindings (`qf_settings_bindings.py`) and defaults (`qf_defaults.py`) centralized for consistent configuration management
- Caching refactored: proper invalidation of background subtraction states; images removed from info dict to reduce cache size

### Quadrant Folding – Image Alignment Detection
- New `QFAlignmentDialog`: non-modal dialog reusing the AISE `ImageAlignmentWidget` for detecting misaligned images within a QF dataset
- "Detect Image Alignment" button added to `QuadrantFoldingGUI`; dialog broadcasts changes back to the main window via signal
- Fold symmetry score surfaced in the alignment table with configurable normalized threshold

### Projection Traces – Fitting Performance
- Analytical Jacobian fast-path for GMM fitting (`scipy.optimize.least_squares`): significant speedup for cases with ≥ 10 peaks
- Vectorised L3 batch Gaussian evaluation applied to production `layerlineModel`
- Hull-range slicing: fit arrays are cropped to the hull range before fitting, reducing unnecessary computation
- `ProjectionProcessor` refactored to use `least_squares` (TRF) solver; Gaussian and Voigt model evaluations restructured

### X-Ray Viewer – New Tools
- **Box Intensity Stats tool** (`BoxStatsTool`): multi-shot rectangular ROI tool for computing region intensity statistics; boxes are editable and numbered; accessible via a dedicated popup panel
- **Radius tool** (`RadiusTool`): interactive radial selection on the image
- Unit selection for calibrated distance display (nm or q-space)
- Center marker overlay on the image
- Tab switching dynamically repositions navigation controls
- Status bar formatting and coordinate display further optimized
- Quadrant-folded image detection added to `XRayViewer`

### Total Diffraction Intensity – Refactored
- Renamed from `TotalDisplayIntensity` to `TotalDiffractionIntensity` across the entire project
- Rebuilt UI around `ProcessingWorkspace` for consistency with other modules
- Radial range selection added via `RadiusTool`
- Status bar enhancements

### Equator – Settings Bindings
- New `eq_settings_bindings.py`: centralized classification and loading/saving of Equator settings
- Settings are now loaded from and bound to `<dataset>/settings/calibration.info`; headless path also sources calibration from the same file
- New `EQ_FittingTab`: dedicated fitting settings tab extracted from the main Equator window
- Equator cache size reduced: images removed from the info dict

### Other Improvements
- **Headless**: `EquatorWindowh`, `QuadrantFoldingh`, `ProjectionTracesh` updated to use `SettingsManager` for manual center and rotation; output directory validation added
- **CPU limiting**: headless Equator batch processing can be bounded by worker count
- **EquatorImage**: interpolation for ignored hull values; mask threshold extended to allow negative values; improved gap interpolation mechanisms
- **Image Blank Dialog**: integrated `ImageViewerWidget` for preview
- **Tooltips**: comprehensive tooltips added across AISE, AIME, QF, PT, navigation controls, image mask tool, and center/rotation settings widgets
- **Cache**: `ImageData` reduced in size by not storing raw images; cache relative-path fixes

### Code Cleanup
- `TotalDisplayIntensity.py` replaced by `TotalDiffractionIntensity.py`
- Background subtraction functions removed from `QuadrantFoldingGUI` and centralized in `BackgroundSubtractionDialog` and `background_search.py`

---

```eval_rst
.. note:: Version 1.30.0 is a major feature release. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: ~175 commits since v1.29.0
