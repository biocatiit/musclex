# Version-1.27.1

Release Date : February 2026

## Key Changes

### New Features
- **Comments Functionality**: Add and manage comments in image processing workflow (ProjectionTracesGUI)
- **Reject Functionality**: Mark and filter rejected images during processing
- **Enhanced Distance Measurement**: Improved distance measurement tools in X-Ray Viewer with better mouse interaction

### Improvements
- Refactored ProjectionBoxTab to use CollapsibleRightPanel for better layout
- Simplified DoubleZoomWidget state management 
- Updated OpenCV dependency to py-opencv with minimum version requirement

### Build & Development
- Added GitHub Actions workflows for automated Conda package building and uploading (all platforms)
- Updated documentation for release process, manual calibration, and installation instructions

### Code Cleanup
- Removed unused widget classes (UIWidget, ZoomInWidget, ZoomHandler)
- Improved code organization and maintainability

---

```eval_rst
.. note:: Version 1.27.1 is a patch release with enhancements and bug fixes. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: 20 commits since v1.27.0
