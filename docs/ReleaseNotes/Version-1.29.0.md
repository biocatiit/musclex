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

#### AIME – Multi-folder Input
- Users can now add multiple experiment folders (instead of selecting one at a time)
- "Add Folder" / "Clear List" buttons replace the previous single-folder browser
- Duplicate folders are silently ignored; source population logic refactored accordingly

### Improvements
- **X-Ray Viewer – PNG export**: switched to off-screen rendering for cleaner exports; status message no longer printed after export
- **X-Ray Viewer – measurement overlay**: all measurement lines, patches, and text are cleared when the distance measurement box is confirmed
- **X-Ray Viewer – output directory**: CSV manager is reset automatically when the output directory changes
- **Display options**: `gray_r` (reversed grayscale) added as a colormap option
- **Blank/mask handling**: all modules now use `SettingsManager` methods for loading blank images and masks, replacing direct `file_manager` calls; logic is centralized and consistent
- **User preferences**: launcher and UI components migrated from `configparser` to `QSettings` for managing preferences; `CollapsibleRightPanel` saves its visibility state via `QSettings`
- **SettingsManager**: extended to manage blank and mask settings in addition to geometry

### Code Cleanup
- Removed deprecated `AddIntensitiesMultExp.py`, `AddIntensitiesSingleExp.py` (old versions), `AddIntensitiesExp.py`, `AddIntensitiesSingleExp_bak.py`, and `CalibrationDialog.py`
- Removed deprecated `getBlankImageAndMask` / `getMaskOnly` functions from `file_manager.py`

---

```eval_rst
.. note:: Version 1.29.0 is a feature release. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: 19 commits since v1.28.0
