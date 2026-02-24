# Version-1.27.4

Release Date : February 2026

## Key Changes

### Bug Fixes
- **Projection Traces - Parameter Bounds**: Fixed parameter bounds handling in GMM fitting to use a consistent empty state instead of copying from proc_box, preventing issues with  parameter bounds
- **Blank/Mask Settings**: Fixed cache invalidation when blank or mask settings are changed via the settings dialog—changes are now reflected immediately without requiring a full reload

### Improvements
- **Documentation**: Added sphinx-copybutton extension for copy-to-clipboard buttons on code blocks
- **Installation**: Added one-line install command for Linux DEB packages that removes existing versions and installs the latest release

---

```eval_rst
.. note:: Version 1.27.4 is a patch release with enhancements and bug fixes. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: 7 commits since v1.27.3
