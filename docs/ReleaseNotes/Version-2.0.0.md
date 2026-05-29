# Version-2.0.0

Release Date : May 2026

## Major Features

### Quadrant Folding – Synthetic Evaluation & Optimization Overhaul
- Synthetic Gaussian data generation for quantitative evaluation of background subtraction quality
- Full evaluation metric suite: MSE against synthetic data, equator metrics, symmetry score, and composite loss; metrics exported to `background_metrics.csv`
- Evaluation baseline persistence: optionally fix a reference baseline image across the dataset for consistent comparisons
- "Optimize each image" mode: run the background parameter search independently per image rather than reusing the folder-level best configuration
- Optimization timeout feature: cap the per-image search time to prevent runaway computations
- Background search utilities extended with `process_file_with_timeout`, full `evaluate_loss` pipeline, and improved parameter range handling
- Synthetic parameters and evaluation baseline stored in the processing cache for reproducibility
- `persist_evaluation_baseline` and `optimize_each_image` checkboxes added to the QF settings bindings

### Quadrant Folding – CSV and Headless Consistency
- `QF_CSVManager` extended with versioning and timestamp columns (`version`, `date`) in `summary.csv`
- New columns in `summary.csv`: `blank_enabled`, `mask_enabled`, `blank_weight`, `detector`, `lambda_sdd`—sourced from `SettingsManager`/`ImageData` so GUI and headless report identical values
- ROI preferences (`fixed_roi_w`, `fixed_roi_h`) mapped to canonical CSV columns
- Headless `QuadrantFoldingh` now applies blank and mask preprocessing settings from `SettingsManager`, matching GUI behavior
- `processing_flags` propagated from `QuadrantFolder` through to `QF_CSVManager`
- Background metrics CSV upserted into the processing info for headless consistency
- Transition background parameters added to evaluation baseline handling

### Documentation Overhaul
- All major module docs refreshed: Equator, Quadrant Folding, Projection Traces, X-Ray Viewer, Add Intensities, Scanning Diffraction, Total Diffracted Intensity
- New dedicated **Quadrant Folding – Background Subtraction** reference page with manual and automated methods, settings descriptions, and worked examples
- Utilities section reorganised; DDF Processor and Convert Rectangle Image to Square Images entries added
- Deprecated/outdated doc files removed; broken links updated to `.md` extension
- Common Settings page expanded to cover calibration workflow

---

```eval_rst
.. note:: Version 2.0.0 is a major release. This version has been tested on Python 3.10 on Ubuntu 22.04.
```

**Total Changes**: ~60 commits since v1.30.0
