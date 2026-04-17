# Version-1.27.6

Release Date : April 2026

## Key Changes

### Improvements
- **Consistent UI style for conda**: "Fusion" application style applied across all modules (Equator, Projection Traces, Quadrant Folding, Diffraction Centroids, X-Ray Viewer, Add Intensities, and the main launcher) for a uniform look and feel across platforms

### Build & Packaging
- Added `.condaignore` to exclude build artifacts, docs, and development files from Conda packages
- Set `include_recipe: False` in `meta.yaml` to omit the build recipe from the Conda package

---

```eval_rst
.. note:: Version 1.27.6 is a patch release. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: 2 commits since v1.27.5
