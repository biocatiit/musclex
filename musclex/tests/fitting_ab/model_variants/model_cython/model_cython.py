"""
Variant: cython  (C-compiled Gaussian kernel)

Wraps the ``_eval_cy`` Cython extension and exposes the same
``layerlineModel`` / ``layerlineModelGMM`` API as the other variants.

The extension must be compiled before use::

    cd musclex/tests/fitting_ab/model_variants/model_cython
    python setup.py build_ext --inplace

If the ``.so`` / ``.pyd`` file is absent an ``ImportError`` is raised with
a descriptive message explaining how to build it.
"""

from __future__ import annotations

import os
import sys

import numpy as np
from scipy.special import wofz

_SQRT_2PI = float(np.sqrt(2.0 * np.pi))
_SQRT2 = float(np.sqrt(2.0))

# ── Try to import the compiled Cython extension ──────────────────────────────
_THIS_DIR = os.path.dirname(os.path.abspath(__file__))
if _THIS_DIR not in sys.path:
    sys.path.insert(0, _THIS_DIR)

try:
    from _eval_cy import gaussian_eval_cy  # type: ignore[import]

    _CYTHON_AVAILABLE = True
except ImportError as _cy_err:
    _CYTHON_AVAILABLE = False
    _cy_err_msg = (
        "Cython extension '_eval_cy' not found. "
        "Build it first:\n\n"
        "    cd musclex/tests/fitting_ab/model_variants/model_cython\n"
        "    python setup.py build_ext --inplace\n\n"
        f"Original error: {_cy_err}"
    )


def _gaussian_eval(x, amplitude, center, sigma):
    if not _CYTHON_AVAILABLE:
        raise ImportError(_cy_err_msg)
    return gaussian_eval_cy(
        np.ascontiguousarray(x, dtype=np.float64),
        float(amplitude),
        float(center),
        float(sigma),
    )


def _voigt_eval(x, amplitude, center, sigma, gamma):
    """Voigt: no Cython version; falls back to NumPy/wofz."""
    z = ((x - center) + 1j * gamma) / (sigma * _SQRT2)
    return amplitude * np.real(wofz(z)) / (sigma * _SQRT_2PI)


# Build model functions using the shared backbone
from musclex.tests.fitting_ab.model_variants._backbone import (
    build_model_functions,
)  # noqa: E402

layerlineModel, layerlineModelGMM = build_model_functions(_gaussian_eval, _voigt_eval)
