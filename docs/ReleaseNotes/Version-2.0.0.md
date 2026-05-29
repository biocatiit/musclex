# Version-2.0.0

Release Date : May 2026

This is a major release that consolidates all development since v1.27.5. Versions 1.28.0, 1.29.0, and 1.30.0 were beta milestones leading to this release.

---

## New Features

### Add Intensities Single Experiment (AISE) – Major Overhaul
- Complete rewrite with a new, workflow-oriented UI
- Built-in workflow guide dialog to walk users through the alignment and summation process
- Image alignment table (`ImageAlignmentTable`) with columns for center, rotation, image difference, and transform status; misaligned rows highlighted in red
- Auto-detection of misaligned images with configurable distance and image-difference thresholds
- Per-image transform flag: apply (or skip) affine correction to individual images before summation
- Auto-rotation and auto-center modes; propagate global base correction to subsequent images
- Global settings dialog for managing the reference (base) image center and rotation
- Blank image support applied during summation; CSV output of per-image intensity statistics
- Multiprocessing support for geometry calculations; start/stop toggle on the "Sum Images" button
- Center overlay visualization in the image viewer

### Add Intensities Multiple Experiments (AIME) – New Module
- New window for combining images across multiple experiment directories
- Multiple experiment folders can be added at once ("Add Folder" / "Clear List" buttons)
- Cartesian row-mapping strategy groups images by frame index across experiments
- Tab-based experiment management with per-tab table views
- Base name input dialog for naming output files; default base name appends `_folded` when source filenames contain `folded`
- Workflow guide dialog accessible via a "Workflow Guide" button; can be suppressed for future sessions

### Quadrant Folding – Background Subtraction Overhaul
- New `BackgroundSubtractionDialog`: dedicated popup for all background subtraction controls
- New `background_search.py` engine: automated parameter search with optimization cache (`optimization_cache.py`) to reuse best configurations across images
- Manual background assignment dialog (`ManualBackgroundAssignmentDialog`) for per-image overrides
- Background configurations table: stores multiple named configurations; best match auto-selected during batch processing
- Optimization timeout feature: cap per-image search time to prevent runaway computations
- Synthetic Gaussian data generation for quantitative evaluation of background subtraction quality
- Full evaluation metric suite: MSE against synthetic data, equator metrics, symmetry score, and composite loss; metrics exported to `background_metrics.csv`
- Evaluation baseline persistence: optionally fix a reference baseline across the dataset
- "Optimize each image" mode: run parameter search independently per image
- Layer-line mask support added to QF processing
- ROI (Region of Interest) support with ROI translation for headless processing
- "Save Cropped Image" option removed; only full-size folded images are saved

### Quadrant Folding – Image Alignment Detection
- New `QFAlignmentDialog`: non-modal dialog reusing the AISE `ImageAlignmentWidget` for detecting misaligned images within a QF dataset
- "Detect Image Alignment" button added to `QuadrantFoldingGUI`
- Fold symmetry scoring (`fold_symmetry.py`): quantitative metric exported to CSV and surfaced in the alignment table

### Quadrant Folding – Batch Processing
- Batch processing rewritten to use `ProcessPoolExecutor` for true parallel execution
- New `process_one_qf_image` headless function in `mp_executor.py` for GUI-free batch processing
- User-initiated stop: "Stop" button cleanly cancels in-flight tasks with an indeterminate progress dialog
- Progress bar logic refactored to use `taskManager` statistics as the single source of truth

### Projection Traces – Fitting Performance
- Analytical Jacobian fast-path for GMM fitting (`scipy.optimize.least_squares`): significant speedup for ≥ 10 peaks
- Vectorised L3 batch Gaussian evaluation applied to production `layerlineModel`
- Hull-range slicing: fit arrays are cropped to the hull range before fitting
- `ProjectionProcessor` refactored to use `least_squares` (TRF) solver

### X-Ray Viewer – New Tools
- **Box Intensity Stats tool** (`BoxStatsTool`): multi-shot rectangular ROI tool for region intensity statistics; boxes are editable and numbered
- **Radius tool** (`RadiusTool`): interactive radial selection on the image
- Unit selection for calibrated distance display (nm or q-space)
- Center marker overlay on the image
- "Export Graph Data to Text File..." (Ctrl+Shift+E): saves current graph profile as a two-column ASCII file
- `XRayViewerGUI` integrated with `ProcessingWorkspace` for consistent file management, display options, navigation, and calibration

### Total Diffraction Intensity – Refactored
- Renamed from `TotalDisplayIntensity` to `TotalDiffractionIntensity` across the project
- Rebuilt UI around `ProcessingWorkspace` for consistency with other modules
- Radial range selection added via `RadiusTool`

### Equator – Settings Bindings
- New `eq_settings_bindings.py`: centralized loading/saving of Equator settings
- Settings loaded from and bound to `<dataset>/settings/calibration.info`; headless path uses the same file
- New `EQ_FittingTab`: dedicated fitting settings tab
- Inpainting option: fill invalid/masked pixels using pyFAI before processing
- Process pool uses `spawn` context to avoid Qt fork-safety issues on Linux

### Read/Write Directory Separation
- New `DirectoryContext` utility resolves the effective output directory at runtime
- New `AssociationStore` persists per-user input→output directory mappings in `~/.musclex/directory_associations.json`
- New `OutputDirDialog` widget; "Change Output Directory" option added to all major modules
- Headless modules now validate and respect output directory settings

### Architecture
- New `SettingsManager`: centralized, Qt-free manager for geometry, auto-geometry cache, blank/mask settings
- `FileManager` extended with `load_from_sources()` and `load_from_directories()`
- Cache file naming uses relative paths; `ImageData` no longer stores raw images to reduce cache size
- `configparser` replaced with `QSettings` for user preferences across launcher and UI components
- "Fusion" style applied across all modules for consistent UI on all platforms
- matplotlib backend updated from `Qt5Agg` to `QtAgg`; `canvas.draw()` replaced with `canvas.draw_idle()`

### Startup – PySide6 Environment Check
- Detects at startup if PySide6 is loaded from outside the active Python prefix
- Shows a Tkinter error dialog with resolution steps; exits before crashing in C++
- Bypass with `MUSCLEX_SKIP_ENV_CHECK=1`

---

## Improvements

- **Projection Traces – sub-pixel support**: hull range inputs use `QDoubleSpinBox` (0.5 step); center calculations no longer rounded to integers; `center_log.csv` records full-precision center and per-box `centerX` values
- **QF CSV**: `summary.csv` extended with `version`, `date`, `blank_enabled`, `mask_enabled`, `blank_weight`, `detector`, `lambda_sdd`; ROI columns standardized; processing flags included
- **QF headless consistency**: `QuadrantFoldingh` applies blank/mask preprocessing from `SettingsManager`, matching GUI behavior
- **X-Ray Viewer – status bar**: d-spacing (nm) shown instead of q-values when calibration available; distance measurements show calibrated Δq / d equivalents
- **X-Ray Viewer – PNG export**: off-screen rendering; overlays excluded; playback returns to starting image after completion
- **Status bar**: cursor coordinates and radial distance (r_px) shown across all modules
- **Display options**: `gray_r` (reversed grayscale) added as a colormap option
- **EquatorImage**: interpolation for ignored hull values; mask threshold allows negative values
- **Headless**: `EquatorWindowh`, `QuadrantFoldingh`, `ProjectionTracesh` use `SettingsManager` for center/rotation; CPU worker count limitable
- **Image Blank Dialog**: integrated `ImageViewerWidget` for preview
- **Tooltips**: added across AISE, AIME, QF, PT, navigation controls, image mask tool, and center/rotation settings widgets
- **Draw Mask**: cross-environment launcher (`drawmask_launcher.py`) handles venv, pip, and PyInstaller-frozen `.deb`; PyInstaller hooks for `pyFAI` and `silx` collect all required resources
- **macOS installer**: `.dmg` builds restored; `multiprocessing.freeze_support()` added for frozen apps
- **Launcher – environment check**: version mismatch messages clarified; output label changed from "Test results" to "Log excerpt"

---

## Documentation Overhaul
- All major module docs refreshed: Equator, Quadrant Folding, Projection Traces, X-Ray Viewer, Add Intensities, Scanning Diffraction, Total Diffracted Intensity
- New dedicated **Quadrant Folding – Background Subtraction** reference page
- New **Common Settings** page consolidating shared configuration across modules
- Utilities section reorganised; DDF Processor and Convert Rectangle Image to Square Images entries added
- Deprecated/outdated doc files removed

---

## Code Cleanup
- Removed deprecated `AddIntensitiesMultExp.py`, `AddIntensitiesExp.py`, `AddIntensitiesSingleExp_bak.py`, `CalibrationDialog.py`, `TotalDisplayIntensity.py`
- Removed deprecated `getBlankImageAndMask` / `getMaskOnly` from `file_manager.py`
- Background subtraction functions removed from `QuadrantFoldingGUI` and centralized in `BackgroundSubtractionDialog`

---

```eval_rst
.. note:: Version 2.0.0 is a major release. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: ~400 commits since v1.27.5
