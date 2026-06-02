"""
Variant: numpy  (direct NumPy / wofz — no lmfit object construction)

``_gaussian_eval`` evaluates the area-normalised Gaussian analytically.
``_voigt_eval`` calls ``scipy.special.wofz`` (Faddeeva function) directly.

This is the simplest non-trivial speedup: identical maths, zero Python
object overhead per call.
"""
from __future__ import annotations

import numpy as np
from scipy.special import wofz

from ._backbone import build_model_functions

_SQRT_2PI = np.sqrt(2.0 * np.pi)
_SQRT2 = np.sqrt(2.0)


def _gaussian_eval(x, amplitude, center, sigma):
    z = (x - center) / sigma
    return (amplitude / (sigma * _SQRT_2PI)) * np.exp(-0.5 * z * z)


def _voigt_eval(x, amplitude, center, sigma, gamma):
    z = ((x - center) + 1j * gamma) / (sigma * _SQRT2)
    return amplitude * np.real(wofz(z)) / (sigma * _SQRT_2PI)


layerlineModel, layerlineModelGMM = build_model_functions(_gaussian_eval, _voigt_eval)
