# Version-1.28.0

Release Date : February 2026

## Major Features

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
- Cartesian row-mapping strategy groups images by frame index across experiments
- Tab-based experiment management with per-tab table views
- Base name input dialog for naming output files
- Shared helper functions with AISE for geometry computation and image summation
- Workflow guide dialog (shared with AISE) accessible via a "Workflow Guide" button; can be suppressed for future sessions

### Equator Window – Refactored
- `EquatorImage` now accepts an `ImageData` container instead of raw arrays, consolidating blank/mask application and center/rotation resolution
- Inpainting option added: fill invalid/masked pixels using pyFAI before processing
- QF (Quadrant Fold) checkbox added to the right panel
- Process pool now uses `spawn` context to avoid Qt fork-safety issues on Linux
- Improved caching mechanism and preprocessing handling
- Minimum window size set; various UI cleanups

### Architecture
- New `SettingsManager` utility: centralized, Qt-free manager for `settings/` directory (manual geometry, auto-geometry cache)
- `FileManager` extended with `load_from_sources()` and `load_from_directories()` for loading from multiple directories; common parent directory used as `dir_path`
- Cache file naming now uses relative paths; cache file version updated

### Other Improvements
- `ProjectionTracesGUI`, `QuadrantFoldingGUI`, `XRayViewerGUI`: early return on invalid HDF5 frame range to prevent crashes
- `ProjectionTracesGUI`, `QuadrantFoldingGUI`: folder selection button removed from image navigator (not currently used)
- `ProcessingWorkspace`: improved message box parent handling; display updates streamlined
- **Status bar enhancements**: cursor coordinates and radial distance (r_px) from center displayed in the status bar across all modules
- **Calibrated q-coordinates**: `ProjectionTracesGUI` and `XRayViewerGUI` status bar now shows qX, qY, qR (reciprocal-space coordinates) when calibration is available, replacing the deprecated `inverseNmFromCenter` function
- **macOS installer**: macOS `.dmg` builds restored and included in releases; `multiprocessing.freeze_support()` added for compatibility with frozen apps

---

```eval_rst
.. note:: Version 1.28.0 is a major feature release. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: ~130 commits since v1.27.5
