"""
Variant: numba-vectorized  (L4 — @njit batch kernel)

Combines two optimisations:

1. **Batch accumulation** (same as ``model_numpy_vectorized``):
   peak parameters are collected first, then all Gaussian peaks are
   evaluated together rather than one call per peak.

2. **Numba JIT compilation**: the inner ``(K, N)`` computation is
   compiled with ``@numba.njit(cache=True, fastmath=True)``.  This
   fuses the loops into a single pass, eliminates temporary (K, N)
   matrix allocations, and takes advantage of LLVM auto-vectorisation
   without SVML — unlike a Cython scalar loop, numba uses its own
   bundled LLVM so it doesn't depend on system libsvml.

Why this is different from ``model_numba.py`` (L2)
----------------------------------------------------
L2 called ``@njit`` on the *single-peak* leaf function.  Each call
still crossed the Python/numba boundary once per peak — 14 calls for
14 peaks.  The JIT speedup per call was real but modest; the call
overhead was paid 14 times.

L4 (this file) calls the ``@njit`` kernel *once* with arrays of all K
peak centers/amplitudes.  The boundary crossing cost is paid exactly
once, and the JIT-compiled inner loop fuses all K peaks into a single
pass over the N-element ``x`` array, writing the running sum directly
into the output buffer.

Voigt fallback
--------------
``scipy.special.wofz`` is not JIT-compilable.  Voigt peaks use the
NumPy path (same as ``model_numpy_vectorized``).  In practice PT data
are Gaussian-only, so this is a no-op.

Warm-up
-------
JIT compilation is triggered at **module import time** via dummy calls
to both kernels.  ``cache=True`` persists compiled code to disk so
subsequent imports are instant (≈ 0 ms overhead after first run).
"""
from __future__ import annotations

import numpy as np
import numba
from scipy.special import wofz

_SQRT_2PI        = np.sqrt(2.0 * np.pi)
_SQRT_2PI_SCALAR = float(_SQRT_2PI)   # inlined in njit kernel
_SQRT2           = np.sqrt(2.0)


# ── Background helpers (single-peak, unchanged from L1) ─────────────────────

def _gaussian_eval(x, amplitude, center, sigma):
    z = (x - center) / sigma
    return (amplitude / (sigma * _SQRT_2PI)) * np.exp(-0.5 * z * z)


def _voigt_eval(x, amplitude, center, sigma, gamma):
    z = ((x - center) + 1j * gamma) / (sigma * _SQRT2)
    return amplitude * np.real(wofz(z)) / (sigma * _SQRT_2PI)


# ── Numba JIT kernels ────────────────────────────────────────────────────────

@numba.njit(cache=True, fastmath=True)
def _gmm_peaks_kernel(x, centers, amps, sigma):
    """Sum of K GMM Gaussian peaks (shared sigma) over N points.

    Compiled loop fuses K peaks into a single pass over x — no
    intermediate (K, N) matrix is allocated.

    Parameters
    ----------
    x       : (N,) float64
    centers : (K,) float64
    amps    : (K,) float64  — integrated area (lmfit convention)
    sigma   : float64       — shared sigma

    Returns
    -------
    (N,) float64
    """
    sqrt_2pi = 2.5066282746310002   # sqrt(2*pi) inlined
    n = x.shape[0]
    k = centers.shape[0]
    result = np.zeros(n, dtype=np.float64)
    inv_sig = 1.0 / sigma
    base_factor = inv_sig / sqrt_2pi
    for i in range(k):
        c = centers[i]
        a = amps[i] * base_factor
        for j in range(n):
            dz = (x[j] - c) * inv_sig
            result[j] += a * np.exp(-0.5 * dz * dz)
    return result


@numba.njit(cache=True, fastmath=True)
def _std_peaks_kernel(x, centers, amps, sigmas):
    """Sum of K peaks with independent sigmas (standard model).

    Parameters
    ----------
    x       : (N,) float64
    centers : (K,) float64
    amps    : (K,) float64
    sigmas  : (K,) float64 — independent sigma per peak

    Returns
    -------
    (N,) float64
    """
    sqrt_2pi = 2.5066282746310002
    n = x.shape[0]
    k = centers.shape[0]
    result = np.zeros(n, dtype=np.float64)
    for i in range(k):
        c   = centers[i]
        s   = sigmas[i]
        inv_s = 1.0 / s
        a   = amps[i] * inv_s / sqrt_2pi
        for j in range(n):
            dz = (x[j] - c) * inv_s
            result[j] += a * np.exp(-0.5 * dz * dz)
    return result


# ── Warm-up: trigger JIT at import time ─────────────────────────────────────
# cache=True means this only compiles once; subsequent imports load from disk.

_WU_X = np.array([0.0, 1.0, 2.0], dtype=np.float64)
_gmm_peaks_kernel(_WU_X, np.array([1.5], dtype=np.float64),
                  np.array([1.0], dtype=np.float64), 1.0)
_std_peaks_kernel(_WU_X, np.array([1.5], dtype=np.float64),
                  np.array([1.0], dtype=np.float64),
                  np.array([1.0], dtype=np.float64))
del _WU_X


# ── Model functions ──────────────────────────────────────────────────────────

def layerlineModelGMM(
    x, centerX, bg_line, bg_sigma, bg_amplitude,
    center_sigma1, center_amplitude1,
    center_sigma2, center_amplitude2,
    common_sigma, **kwargs,
):
    """GMM model (shared sigma) with numba batch kernel."""
    result = (
        _gaussian_eval(x, bg_amplitude, centerX, bg_sigma) + bg_line
        + _gaussian_eval(x, center_amplitude1, centerX, center_sigma1)
        + _gaussian_eval(x, center_amplitude2, centerX, center_sigma2)
    )

    g_centers, g_amps = [], []
    voigt_peaks = []
    i = 0
    while f"p_{i}" in kwargs:
        p   = kwargs[f"p_{i}"]
        amp = kwargs[f"amplitude{i}"]
        if f"gamma{i}" in kwargs:
            voigt_peaks.append((centerX + p, amp, kwargs[f"gamma{i}"]))
        else:
            g_centers.append(centerX + p)
            g_amps.append(amp)
        i += 1

    if g_centers:
        result += _gmm_peaks_kernel(
            np.ascontiguousarray(x, dtype=np.float64),
            np.asarray(g_centers, dtype=np.float64),
            np.asarray(g_amps,    dtype=np.float64),
            float(common_sigma),
        )

    # Voigt: numba can't JIT wofz — NumPy fallback
    for c, a, g in voigt_peaks:
        result += _voigt_eval(x, a, c, common_sigma, g)

    return result


def layerlineModel(
    x, centerX, bg_line, bg_sigma, bg_amplitude,
    center_sigma1, center_amplitude1,
    center_sigma2, center_amplitude2, **kwargs,
):
    """Standard model (independent sigma per peak) with numba batch kernel."""
    result = (
        _gaussian_eval(x, bg_amplitude, centerX, bg_sigma) + bg_line
        + _gaussian_eval(x, center_amplitude1, centerX, center_sigma1)
        + _gaussian_eval(x, center_amplitude2, centerX, center_sigma2)
    )

    g_centers, g_amps, g_sigmas = [], [], []
    voigt_peaks = []
    i = 0
    while f"p_{i}" in kwargs:
        p     = kwargs[f"p_{i}"]
        amp   = kwargs[f"amplitude{i}"]
        sigma = kwargs[f"sigma{i}"]
        if f"gamma{i}" in kwargs:
            voigt_peaks.append((centerX + p, amp, sigma, kwargs[f"gamma{i}"]))
        else:
            g_centers.append(centerX + p)
            g_amps.append(amp)
            g_sigmas.append(sigma)
        i += 1

    if g_centers:
        result += _std_peaks_kernel(
            np.ascontiguousarray(x, dtype=np.float64),
            np.asarray(g_centers, dtype=np.float64),
            np.asarray(g_amps,    dtype=np.float64),
            np.asarray(g_sigmas,  dtype=np.float64),
        )

    for c, a, s, g in voigt_peaks:
        result += _voigt_eval(x, a, c, s, g)

    return result
