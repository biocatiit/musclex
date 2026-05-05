"""
Variant: numba  (JIT-compiled Gaussian inner loop)

The Gaussian evaluation loop is compiled with ``@numba.njit(cache=True,
fastmath=True)``.  ``fastmath=True`` allows LLVM to reorder and approximate
floating-point operations for speed (safe for a smooth Gaussian curve).

Voigt falls back to ``scipy.special.wofz`` because numba cannot JIT-compile
that function.  If your dataset has no Voigt peaks the Voigt branch is never
reached and you get the full numba benefit.

JIT compilation is triggered **at import time** via a warm-up call on a
3-element dummy array.  This ensures the first real fit is not penalised by
JIT latency (~100–300 ms cold start).

Requirements
------------
``numba`` must be installed::

    pip install numba
    # or: conda install numba
"""
from __future__ import annotations

import numpy as np
from scipy.special import wofz

from ._backbone import build_model_functions

_SQRT_2PI = float(np.sqrt(2.0 * np.pi))
_SQRT2 = float(np.sqrt(2.0))
# Hard-coded constant inside the JIT kernel avoids dependency on a Python-level
# module attribute, which numba can't always resolve in nopython mode.
_SQRT_2PI_SCALAR: float = _SQRT_2PI

try:
    import numba  # noqa: PLC0415

    @numba.njit(cache=True, fastmath=True)
    def _gaussian_eval_inner(x: np.ndarray, amplitude: float, center: float, sigma: float) -> np.ndarray:
        """Numba-compiled Gaussian kernel (area-normalised, lmfit convention)."""
        factor = amplitude / (sigma * 2.5066282746310002)  # sqrt(2*pi) inlined
        n = x.shape[0]
        result = np.empty(n, dtype=np.float64)
        for i in range(n):
            dz = (x[i] - center) / sigma
            result[i] = factor * np.exp(-0.5 * dz * dz)
        return result

    def _gaussian_eval(x, amplitude, center, sigma):
        return _gaussian_eval_inner(
            np.ascontiguousarray(x, dtype=np.float64),
            float(amplitude), float(center), float(sigma),
        )

    # ── warm-up: trigger JIT compilation now, not during the first fit ──────
    _gaussian_eval_inner(np.array([0.0, 1.0, 2.0], dtype=np.float64), 1.0, 1.0, 1.0)

    _NUMBA_AVAILABLE = True

except ImportError:  # pragma: no cover
    # numba not installed — silently fall back to NumPy so the adapter still
    # works; the adapter will log a warning.
    def _gaussian_eval(x, amplitude, center, sigma):  # type: ignore[misc]
        z = (x - center) / sigma
        return (amplitude / (sigma * _SQRT_2PI)) * np.exp(-0.5 * z * z)

    _NUMBA_AVAILABLE = False


def _voigt_eval(x, amplitude, center, sigma, gamma):
    """Voigt: numba cannot JIT wofz, so this falls back to NumPy."""
    z = ((x - center) + 1j * gamma) / (sigma * _SQRT2)
    return amplitude * np.real(wofz(z)) / (sigma * _SQRT_2PI)


layerlineModel, layerlineModelGMM = build_model_functions(_gaussian_eval, _voigt_eval)
