# Version-1.27.3

Release Date : February 2026

## Key Changes

### Improvements
- **X-Ray Viewer Status Bar**: Enhanced status message display
  - Full path shown in tooltip; elided text in status bar for better fit
  - Resize event handler resets status bar when window is resized
  - Safer attribute access to prevent potential errors
- **GMM Parameter Editor (Projection Traces)**: Refit workflow improvements
  - Refit completion message is now non-modal—users can interact with the application while the message is shown
  - Prevents overlapping dialogs when multiple refit messages would appear
  - Folder cache is refreshed automatically after a successful refit, improving data consistency

---

```eval_rst
.. note:: Version 1.27.3 is a patch release with enhancements and bug fixes. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: 6 commits since v1.27.2
