# Version-1.27.5

Release Date : February 2026

## Key Changes

### Bug Fixes
- **Projection Traces - CSV Sync**: CSV data (summary.csv) is now written after peak details updates and after refit completion, keeping exported data in sync with the processing cache

- **Projection Traces - Peak Data**: Refactored peak data handling in PT_CSVManager for correct right/left peak indexing; average centroid and Gaussian area calculations now use proper mirrored indices。

### Improvements
- **ProjectionProcessor**:  centerX parameter is no longer for fitting.

---

```eval_rst
.. note:: Version 1.27.5 is a patch release with enhancements and bug fixes. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: 5 commits since v1.27.4
