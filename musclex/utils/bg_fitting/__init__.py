"""Vendored numeric helpers for iterative 2D background fitting.

Currently holds the 1D per-sector cone-background model used by the step-0
projection pass. Copied from the bg-optimization research repo
(``src/bg_removal/gen_bg_fit/fit_sector_var_model.py``); depends only on
numpy/scipy so it is self-contained.
"""

from .cone_model import ConeModelConfig, FitResult, model_eval, fit_cone_background

__all__ = ["ConeModelConfig", "FitResult", "model_eval", "fit_cone_background"]
