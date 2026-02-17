# Version-1.27.2

Release Date : February 2026

## Key Changes

### New Features
- **X-Ray Viewer PNG Export**: Export the current view as PNG with default filename; overlays are removed when exporting for a clean image output

### Bug Fixes
- **Persist Intensity**: Fixed bug where unchecking "persist intensity" did not apply to the next image—the previous intensity setting was still used
- **Process Current Folder Missing Tasks**: Fixed QuadrantFoldingGUI bug where "Process Current Folder" could miss some tasks. Changes include:
  - Enhanced error handling and retry logic for failed processing tasks
  - Detailed error signals for retries with statistics tracking (success/failure counts)
  - Retry phase for failed tasks with improved user feedback
  - Processing summaries and error reporting in the GUI
  - New `writeProcessingLog` method for detailed processing summaries and error logs
  - Refactored `tasks_done.txt` writing: removed `qf_lock`, streamlined logic, safer main-thread handling
  - Use try-finally to ensure locks are released reliably
  - Removed unnecessary lock management
  - QuadrantFolder: Improved cache handling with error logging for cache write failures; processing continues even when caching encounters issues
  - Separate dictionaries for first-attempt and save errors for better diagnostics

---

```eval_rst
.. note:: Version 1.27.2 is a patch release with enhancements and bug fixes. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: 14 commits since v1.27.1
