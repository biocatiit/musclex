# Version-1.26.0

Release Date : XX/XX/2025

## Major Features

### Image Viewer Architecture Refactoring
- **New Widget System**: Introduced reusable UI component architecture
  - `ImageViewerWidget`: Enhanced image display with integrated tool support
  - `DisplayOptionsPanel`: Centralized display controls for brightness, contrast, and zoom
  - `DoubleZoomWidget` & `ZoomHandler`: Advanced zoom functionality with dual-view capability
  - `NavigationControls`: Unified navigation widget across all GUIs (EquatorWindow, QuadrantFoldingGUI, XRayViewerGUI)

- **Interactive Tool System**: New `musclex/ui/tools/` package with modular tools
  - `CenterRotateTool`: Quick center and rotation angle setting simultaneously
  - `ChordsCenterTool` & `PerpendicularsCenterTool`: Advanced geometric center-finding methods
  - `RotationTool`: Dedicated rotation adjustment with visual feedback
  - `ZoomRectangleTool`: Interactive rectangular zoom regions
  - `ToolManager`: Coordinated tool interaction and mode switching

### Multiprocessing & Performance
- **Parallel Image Processing**: New multiprocessing support for batch operations
  - Significantly improved processing speed in EquatorWindow
  - Task management system with cancellation support
  - Progress tracking for HDF5 and folder processing
  - Stop process functionality with real-time feedback

- **Enhanced File Management**: Major FileManager improvements
  - Full HDF5 file processing support across all GUIs
  - Cached directory scanning with disk caching for faster loading
  - Asynchronous directory scanning with provisional file selection
  - Two-phase loading (synchronous + background) for improved responsiveness
  - Better memory management with automatic cleanup

### Quadrant Folding Enhancements
- **New Interactive Dialogs**:
  - `SetCentDialog`: Manual center setting with live image preview and zoom support
  - `SetAngleDialog`: Interactive rotation angle adjustment
  - `ImageBlankDialog`: Blank image subtraction with preview
  - `ImageMaskDialog`: Advanced image masking functionality

- **Improved Center & Rotation Management**:
  - Refactored center handling with configuration fingerprinting
  - Cache validation system to ensure consistency
  - "Quick Center and Rotation Angle" tool for efficient calibration
  - Center and rotation mode indicators for better visibility
  - Restore auto center/rotation with option to apply to current image or all images
  - Preserved manual settings during cache operations

## UI/UX Improvements

### Visual Enhancements
- Bold fonts for group boxes across all GUIs for better visual hierarchy
- Green labels for improved visibility of important information
- Enhanced button styling and consistent appearance
- Improved status bar with file indices and processing information

### Progress & Feedback
- HDF5 processing progress display in all GUIs
- Enhanced progress bar formatting with detailed status
- Real-time task completion indicators
- Confirmation dialog when closing windows with running tasks
- "Play Current Folder" button for clearer batch processing control

### User Controls
- Persist intensity value feature across processing sessions
- Removed range restrictions on QDoubleSpinBox components (allow any value)
- Allow negative input for rotation angle dialog
- Better navigation button text for clarity
- Window stays on top hint for critical dialogs

## Bug Fixes


### Data & Processing
- Fixed pandas.errors.InvalidIndexError by removing duplicate "Meridian Sigma" column


### UI Issues
- Fixed double zoom handling in multiple dialogs
- Fixed musclex.desktop launcher to properly start GUI

## Code Quality & Architecture

### Refactoring
- Reorganized file structure: `ui_widgets` → `ui/widgets` following Python conventions
- Removed obsolete code and unused attributes (centerizeImage, center_before_rotation, etc.)
- Removed redundant center-related code from info dictionary
- Simplified blank image and mask handling logic
- Better separation of concerns across modules

### Configuration & Cache Management
- Configuration fingerprinting for reliable cache validation
- Automatic cache invalidation when settings change
- Improved state management in QuadrantFolder
- Better handling of manual vs automatic settings

## Development & Build

### Documentation
- Added comprehensive guide for building Linux DEB packages (`dev_docs/DevGuide/build_linux_deb.md`)
- Updated environment tester with latest Python and package versions

### Packaging
- Updated `musclex_linux.spec` with sklearn data files and PySide6 hidden imports
- Updated `musclex_mac.spec` with sklearn data files collection
- Fixed musclex.desktop to launch GUI properly
- Added VSCode launch configuration for debugging

### Project Maintenance
- Updated `.gitignore` to include venv, .idea, temp_files, docs/_build directories
- Removed musclex.egg-info folder from version control
- Version bumped to 1.26.0 in meta.yaml and __init__.py

## Testing & Environment

```eval_rst
.. note:: Version tested on Python 3.10 and Python 3.8 - on Ubuntu 20.04 and 22.04, macOS 12.6, Windows 10 and Windows 11.
```

## Known Issues & Future Work

- Additional documentation needed for background removal algorithms
- Continue performance optimization for very large HDF5 files

---

**Total Changes**: 59 files changed, 10,174 insertions(+), 4,294 deletions

