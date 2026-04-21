# Version-1.29.0

Release Date : April 2026

## Key Changes

### New Features

#### Read/Write Directory Separation
A major architectural change allowing output files (results, CSV, cache) to be written to a separate directory from the read-only input data:
- New `DirectoryContext` utility resolves the effective output directory at runtime
- New `AssociationStore` persists per-user input→output directory mappings in `~/.musclex/directory_associations.json`
- New `OutputDirDialog` widget lets users confirm or change the output directory, with silent reuse of previously stored associations when the directory is writable
- "Change Output Directory" option added to all major modules: AISE, AIME, Diffraction Centroids, DI Batch, Equator, Projection Traces, Quadrant Folding, X-Ray Viewer

#### Projection Traces – Sub-pixel Support
- Hull range inputs and center calculations in `ProjectionBoxTab` now support floating-point values instead of being restricted to integers; spinboxes changed from `QSpinBox` to `QDoubleSpinBox` (0.5 step, 1 decimal place)

#### X-Ray Viewer – Graph Data Export
- New "Export Graph Data to Text File..." menu action (Ctrl+Shift+E) saves the current graph profile as a two-column ASCII file (pixel index, intensity), with header metadata including image name, MuscleX version, and acquisition mode

#### Startup – PySide6 Environment Check
- MuscleX now detects at startup if PySide6 is loaded from outside the active Python prefix (a common cause of ABI-mismatch segfaults on keyboard input)
- Shows a clear error dialog (via Tkinter, independent of Qt) with step-by-step instructions for resolving the conflict; exits early instead of crashing later in C++
- Can be bypassed with `MUSCLEX_SKIP_ENV_CHECK=1`

#### AIME – Multi-folder Input
- Users can now add multiple experiment folders (instead of selecting one at a time)
- "Add Folder" / "Clear List" buttons replace the previous single-folder browser
- Duplicate folders are silently ignored; source population logic refactored accordingly

### Improvements
- **X-Ray Viewer – PNG export**: switched to off-screen rendering for cleaner exports; status message no longer printed after export
- **X-Ray Viewer – measurement overlay**: all measurement lines, patches, and text are cleared when the distance measurement box is confirmed
- **X-Ray Viewer – output directory**: CSV manager is reset automatically when the output directory changes
- **X-Ray Viewer – status bar coordinates**: cursor coordinates now show d-spacing (nm) instead of q-values when calibration is available; radial distance (px) also shown
- **X-Ray Viewer – distance measurement**: measured distances show both pixel count and calibrated Δq / d-spacing equivalents when calibration is available
- **UI consistency**: "Fusion" style applied across all modules for a consistent look on all platforms
- **matplotlib backend**: updated from `Qt5Agg` to `QtAgg` and `FigureCanvasQTAgg` import updated accordingly for PySide6 compatibility; `canvas.draw()` calls replaced with `canvas.draw_idle()` for improved UI responsiveness
- **Display options**: `gray_r` (reversed grayscale) added as a colormap option
- **Blank/mask handling**: all modules now use `SettingsManager` methods for loading blank images and masks, replacing direct `file_manager` calls; logic is centralized and consistent
- **User preferences**: launcher and UI components migrated from `configparser` to `QSettings` for managing preferences; `CollapsibleRightPanel` saves its visibility state via `QSettings`
- **SettingsManager**: extended to manage blank and mask settings in addition to geometry
- **Projection Traces – center log**: `center_log.csv` written alongside `summary.csv`, recording full-precision image center and per-box `centerX` values

### Code Cleanup
- Removed deprecated `AddIntensitiesMultExp.py`, `AddIntensitiesSingleExp.py` (old versions), `AddIntensitiesExp.py`, `AddIntensitiesSingleExp_bak.py`, and `CalibrationDialog.py`
- Removed deprecated `getBlankImageAndMask` / `getMaskOnly` functions from `file_manager.py`

---

```eval_rst
.. note:: Version 1.29.0 is a feature release. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: ~33 commits since v1.28.0
