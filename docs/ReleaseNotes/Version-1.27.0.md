# Version-1.27.0 (Beta)

Release Date : February 2026

## Major Features

### Projection Traces - GMM Fitting Support
- **Gaussian Mixture Model (GMM) Fitting**: Revolutionary new fitting approach for complex peak patterns
  - `GMMParameterEditorDialog`: Interactive parameter editor with real-time preview
  - Support for multiple peaks with common sigma (shared width)
  - Per-parameter bounds management for precise control
  - Fixed parameter support to constrain specific values
  - Peak and sigma tolerance settings for robust fitting
  - Transactional rollback capability for safe parameter exploration
  
- **Enhanced Peak Management**:
  - Peak mirroring functionality based on cache status
  - `EditPeakDetailsDialog`: Edit individual peak parameters with tolerance control
  - Peak labeling visualization for better identification
  - Peak bounds synchronization across the fitting process
  - Automatic peak position recalculation to prevent drift
  
- **Improved Box Processing**:
  - Three-stage processing pipeline (histogram → peak detection → fitting)
  - Box-centric configuration format using JSON
  - `ProcessingBox` and `ProcessingState` dataclasses for structured data management
  - Image-level cache utilization for faster reprocessing
  - Hull range management with auto-zoom functionality
  - Overlay dragging and edge manipulation for interactive adjustments

### Manual Calibration Enhancements
- **Advanced Optimization Methods**:
  - `ManualCalibrationDialog`: Comprehensive calibration interface
  - Differential evolution for global optimization (escapes local minima)
  - MAD-based (Median Absolute Deviation) outlier rejection for robust fitting
  - Multi-start optimization for improved convergence
  - Objective-based refinement with configurable parameters
  
- **Calibration Workflow Improvements**:
  - Point refinement for sub-pixel accuracy
  - CSV export of optimization history for analysis
  - Enhanced results display with detailed metrics

### Architecture Refactoring

#### New Base Classes
- **`BaseGUI`**: Minimal foundation class for all GUI components
  - Standard image tab creation using template method pattern
  - Floating widget positioning
  - Unified initialization flow
  - Status bar integration via navigator attribute
  
- **`ImageNavigatorWidget`**: Integrated image viewing and file management component
  - Combines ImageViewerWidget with FileManager for unified file browsing
  - Image display with tools and interactions
  - Navigation controls (next/prev image, next/prev file)
  - File path display with auto-update
  - Background directory scanning with progress tracking
  - Scan completion tracking for accurate file counts
  
- **`ProcessingWorkspace`**: Complete workspace for image processing modules (QF, PT, EQ)
  - Uses ImageNavigatorWidget for integrated image viewing and file management
  - Display panel integration with collapsible settings
  - Calibration settings management (center, rotation, blank/mask)
  - Settings persistence to JSON files
  - Delayed notifications for improved UI responsiveness

#### Data Management
- **`ImageData` Container**: Structured image data management
  - Center and rotation handling
  - Quadrant folding state
  - Blank/mask image references
  - Factory method for consistent creation

### Quadrant Folding Improvements

#### Terminology Update
- **"Blank Image" → "Empty Cell Image"**: More accurate terminology throughout the application

#### Enhanced UI Components
- **`ImageSettingsPanel`**: Centralized settings management
  - Center and rotation settings delegation
  - Coordinate handling improvements
  - Settings persistence across images
  
- **`BlankMaskSettingsWidget`**: Dedicated widget for empty cell and mask configuration
  
- **Improved Dialogs**:
  - `SetCentDialog`: Now uses `ImageViewerWidget` for better image display
  - `SetAngleDialog`: Integrated into `ImageSettingsPanel`
  - Support for negative rotation angles
  
#### Center and Rotation Management
- **Configuration Fingerprinting**: Automatic cache validation
  - Cache invalidation when settings change
  - Preserved manual settings during cache operations
  - "Quick Center and Rotation Angle" tool for simultaneous setting
  
- **Mode Indicators**: Visual feedback for center and rotation modes
  - Auto vs. manual mode display
  - Apply current center/rotation to subsequent images
  - Restore auto center/rotation with granular control (current image or all)

#### Collapsible UI
- `CollapsibleGroupBox` and `CollapsibleRightPanel` for better space utilization
- Streamlined quadrant options integration
- Optimized result tab layout

### X-Ray Viewer Enhancements
- **Slice Box Functionality**: Enhanced slice visualization
- **Refactored Layout**: Improved status bar handling
- **Intensity Range Handling**: Better control over display ranges
- **Fitting Options Tab**: Enabled after histogram update

## Performance & Multiprocessing

### Parallel Processing
- **EquatorWindow Multiprocessing**: Significant speed improvements
  - `ProcessPoolExecutor` for concurrent image processing
  - Worker initialization with proper context (macOS compatibility)
  - Task cancellation support with real-time feedback
  - Stop process dialog with progress indication
  - Batch completion metrics and timing
  
- **Memory Management**:
  - Automatic cleanup after batch processing
  - Release of large image data after UI updates
  - Optional memory usage monitoring
  - Better resource management

### File Manager Enhancements
- **Cached Directory Scanning**: Disk caching for faster loading
  - Multiprocessing for I/O-bound tasks
  - Two-phase loading (synchronous + background)
  - Provisional file selection during scanning
  - Asynchronous directory scanning with status updates
  
- **HDF5 Support Improvements**:
  - Full HDF5 file processing across all GUIs
  - Frame count tracking instead of loading all images
  - Master file preference when both master and data files exist
  - Progress tracking for HDF5 processing
  - Proper handling of HDF5 frame ranges

## UI/UX Improvements

### Navigation
- **`NavigationControls` Widget**: Reusable navigation component
  - Consistent button naming and styling
  - Mode-aware navigation (folder vs. H5)
  - "Play Current Folder" vs. "Process H5 File" clarity
  - Single instance across tabs for consistency

### Display Enhancements
- **`DisplayOptionsPanel`**: Centralized display controls
  - Intensity value validation to prevent errors
  - Persist intensity values across sessions
  - Brightness, contrast, and zoom controls
  
- **`ImageViewerWidget`**: Enhanced image display
  - Integrated tool support
  - Built-in zoom handling (mouse wheel, zoom rectangle)
  - Panning support
  - Axis management improvements

### Zoom Functionality
- **`DoubleZoomWidget`**: Advanced dual-view zoom
  - Dynamic crop radius adjustment
  - Improved intensity normalization
  - Red dot cursor tracking fix
  - Subpixel-level accuracy for calibration

### Status and Progress
- HDF5 processing progress display in all GUIs
- Enhanced progress bar formatting with detailed status
- File indices in status bar
- Scan completion indicators
- Confirmation dialog when closing with running tasks

## Bug Fixes

### Data Processing
- Fixed pandas.errors.InvalidIndexError by removing duplicate "Meridian Sigma" column
- Fixed center coordinate handling in multiple GUIs
- Fixed blank/mask handling in QuadrantFolder
- Clipped image values to [0, 255] range to prevent underflow/overflow
- Fixed pixel threshold check for invalid pixels (≤ threshold)
- Fixed zoom rectangle removal in ProjectionBoxTab

### UI Issues
- Fixed double zoom red dot not following mouse
- Fixed zoom functionality in SetCentDialog
- Fixed center display updates in QuadrantFoldingGUI and ImageSettingsPanel
- Fixed background processing in QuadrantFoldingGUI
- Fixed progress bar calculation in multiple GUIs
- Fixed file manager name errors
- Fixed notification handling when opening images

### Processing Issues
- Fixed rotation handling to use original coordinates
- Fixed image size consistency after rotation
- Fixed cache handling for quadrant folded images
- Fixed geometric center display for quadrant folded images
- Removed axis inversion issues

## Code Quality & Architecture

### Refactoring
- **Component Architecture**: New modular GUI architecture with clear separation of concerns
  - BaseGUI provides unified initialization framework
  - ImageNavigatorWidget handles file management and navigation
  - ProcessingWorkspace integrates image processing workflows
  
- Reorganized file structure following Python conventions (`ui_widgets` → `ui/widgets`)
- Removed obsolete methods and unused attributes
- Streamlined blank image and mask handling
- Better separation of concerns across modules
- Removed redundant center-related code
- Deprecated unused methods (getRotatedImage, centerizeImage, movePeaks)

### State Management
- Configuration fingerprinting for cache validation
- Improved state management in QuadrantFolder
- Better handling of manual vs automatic settings
- Cache persistence for processing state
- Transactional updates with rollback support

### Code Cleanup
- Removed debug print statements
- Removed commented-out code
- Standardized naming conventions
- Improved error handling and logging
- Better resource cleanup

## Development & Build

### Documentation
- Added comprehensive architecture SVG diagrams (v1.25-1.27)
- GitHub Actions Build & Release Workflow documentation
- Manual calibration circle-band objective documentation
- Performance optimization and parallel processing guides
- Build guides for Linux DEB packages

### Build System
- **GitHub Actions Workflow**: Automated multi-platform builds
  - Linux, macOS, and Windows support
  - Multi-architecture builds
  - Artifact management and release creation
  - DMG creation for macOS with quarantine attribute removal
  - Debian package handling
  
- **Packaging Updates**:
  - Inno Setup installer script for Windows
  - Advanced Installer project (MuscleX-1.26.0.aip)
  - Windows icon support (AppIcon.ico)
  - Updated spec files with sklearn data and PySide6 imports
  - Source distribution creation in workflow

### Development Tools
- VSCode launch configurations for debugging
  - Multiple GUI modes
  - Headless mode support
  - Various functionality testing configs
  
- Updated `.gitignore`:
  - venv, .idea directories
  - temp_files, docs/_build
  - test_guide directory
  - test.log files

## Testing

### New Test Cases
- `PT_FittingGaussians_Horizontal`: GMM fitting validation
- `EIGER_PT_Convex_Hull_Vertical`: EIGER detector support
- `MAR_PT_Convex_Hull_Vertical`: MAR detector convex hull tests
- Updated test suite for new box configuration format
- Headless test enhancements for background processing

### Test Infrastructure
- Updated environment tester with latest package versions
- Improved test result validation
- Better test image organization
- Enhanced test utilities

## Known Issues & Future Work

- Additional documentation needed for GMM fitting workflows
- Continue performance optimization for very large HDF5 files
- Further UI refinements based on user feedback
- Extended testing on additional detector types

---

```eval_rst
.. note:: Version 1.27.0 is currently in **Beta**. This version has been tested on Python 3.8 and 3.10 on Ubuntu 20.04 and 22.04, macOS 12.6+, Windows 10 and Windows 11.
```

**Total Changes**: 346+ commits since v1.26.1

**Key Contributors**: Development team continues to enhance MuscleX with focus on advanced fitting algorithms, performance optimization, and improved user experience.
