# Version-1.27.1

Release Date : February 2026

## New Features

### Image Processing Workflow
- **Comments Functionality**: Added ability to add and manage comments in the image processing workflow
  - Integrated comment management in ProjectionTracesGUI
  - CSV export support for comments data
  - Comment persistence across processing sessions

- **Reject Functionality**: New capability to reject images in the processing workflow
  - Mark images as rejected during review
  - Filter and manage rejected images
  - Export reject status in CSV files

### X-Ray Viewer Enhancements
- **Enhanced Distance Measurement**: Improved distance measurement functionality
  - Better mouse interaction handling
  - More precise measurement tools
  - Improved visual feedback for measurements

- **Mouse Interaction Improvements**: Enhanced mouse handling across ImageViewerWidget
  - Better cursor tracking and responsiveness
  - Improved interaction for measurement and annotation tools

## Improvements

### UI/UX Enhancements
- **ProjectionBoxTab Layout**: Refactored to use CollapsibleRightPanel for improved space utilization
  - More compact options panel
  - Better organization of processing options
  - Improved workflow efficiency

### Widget Refactoring
- **DoubleZoomWidget Simplification**: Streamlined state management
  - Removed DoubleZoomState class in favor of simple boolean flag
  - Enhanced code clarity and maintainability
  - Direct state management implementation

- **Code Cleanup**: Removed unused widget classes
  - Deleted UIWidget class dependency
  - Removed unused ZoomInWidget class
  - Removed unused ZoomHandler class
  - Cleaned up DoubleZoomGUI import references
  - Improved overall code maintainability

## Build & Development

### GitHub Actions Workflows
- **Conda Package Automation**: Complete GitHub Actions workflow for Conda package management
  - Automated building for all platforms (linux-64, win-64, osx-64, osx-arm64)
  - Separate build and upload workflows for better control
  - Artifact management and version tracking
  - Integration with Anaconda Cloud (biocat_IIT)

### Dependencies
- **OpenCV Update**: Switched to py-opencv with minimum version requirement
  - Better compatibility across platforms
  - More maintainable dependency management

### Documentation
- **Enhanced Release Process Documentation**: Updated GitHub Actions workflow documentation
  - Detailed instructions for Conda package building and uploading
  - Prerequisites and testing procedures
  - Comprehensive workflow usage guide

- **Manual Calibration Documentation**: Updated with differential evolution optimization details

- **Installation Instructions**: Refreshed installation guides for Windows, macOS, and Linux

## Bug Fixes
- Fixed mouse tracking issues in DoubleZoomWidget
- Improved state management consistency across widgets
- Enhanced stability of image processing workflow

## Code Quality
- Removed dependencies on obsolete widget classes
- Simplified widget architecture
- Improved code organization and maintainability
- Better separation of concerns

---

```eval_rst
.. note:: Version 1.27.1 is a patch release with enhancements and bug fixes. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: 20 commits since v1.27.0

**Key Contributors**: Development team continues to improve MuscleX with focus on workflow enhancements, code quality, and build automation.
