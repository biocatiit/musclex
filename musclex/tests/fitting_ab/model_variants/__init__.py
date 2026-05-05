"""
Self-contained model-function variants for A/B speed benchmarking.

Each sub-module exports the same public API::

    layerlineModel(x, centerX, bg_line, bg_sigma, bg_amplitude,
                   center_sigma1, center_amplitude1,
                   center_sigma2, center_amplitude2, **kwargs)

    layerlineModelGMM(x, centerX, bg_line, bg_sigma, bg_amplitude,
                      center_sigma1, center_amplitude1,
                      center_sigma2, center_amplitude2,
                      common_sigma, **kwargs)

The function signatures are identical to those in
``musclex.modules.ProjectionProcessor`` so they can be dropped into any
``lmfit.Model`` wrapper.  The only difference between variants is the
implementation of the innermost ``_gaussian_eval`` / ``_voigt_eval``
helpers.

Variants
--------
model_lmfit_objects
    Baseline / "before" state: creates a new ``lmfit.models.GaussianModel``
    or ``VoigtModel`` object on *every* call (the original production code).
model_numpy
    Direct NumPy formulas — no object construction overhead.
    Voigt uses ``scipy.special.wofz`` directly.
model_numba
    Gaussian inner loop compiled with ``@numba.njit(cache=True,
    fastmath=True)``.  Voigt falls back to NumPy/wofz (numba cannot JIT
    ``wofz``).  JIT is triggered at import time via a warm-up call so the
    first actual fit is not penalised.
model_cython
    Same Gaussian formula compiled as a Cython C extension.
    **Requires a manual build step** — see
    ``model_variants/model_cython/README.md``.
    If the extension is not compiled the module raises a descriptive
    ``ImportError`` instead of a generic crash.
"""
