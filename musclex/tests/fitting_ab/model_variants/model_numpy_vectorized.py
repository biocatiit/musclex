"""
Variant: numpy-vectorized  (L3 — batch NumPy, no per-peak function calls)

Instead of calling ``_gaussian_eval`` once per peak inside a ``while`` loop,
all Gaussian peaks are accumulated in a single batched expression:

    z     = (x[None, :] - centers[:, None]) / sigmas[:, None]   # (K, N)
    total = ((amps / (sigmas * sqrt2pi))[:, None] * exp(-0.5*z²)).sum(0)

This replaces K separate ``np.exp(N)`` calls with one ``np.exp(K*N)`` call,
eliminating K-1 rounds of Python dispatch, NumPy buffer allocation, and
loop-startup overhead.

Differences from ``model_numpy.py`` (L1)
-----------------------------------------
* L1 still calls ``_gaussian_eval`` (a scalar-sigma function) K times.
* L3 (this file) collects all peak parameters first, then evaluates in one
  matrix expression.

Voigt peaks fall back to a loop because broadcasting the complex Faddeeva
function across a (K, N) grid is memory-intensive and rarely faster than
iterating, given how few Voigt peaks appear in practice.
"""
from __future__ import annotations

import numpy as np
from scipy.special import wofz

_SQRT_2PI = np.sqrt(2.0 * np.pi)
_SQRT2    = np.sqrt(2.0)


# ── background helpers (unchanged — single-peak, rarely called) ─────────────

def _gaussian_eval(x, amplitude, center, sigma):
    z = (x - center) / sigma
    return (amplitude / (sigma * _SQRT_2PI)) * np.exp(-0.5 * z * z)


def _voigt_eval(x, amplitude, center, sigma, gamma):
    z = ((x - center) + 1j * gamma) / (sigma * _SQRT2)
    return amplitude * np.real(wofz(z)) / (sigma * _SQRT_2PI)


# ── vectorised Gaussian batch ────────────────────────────────────────────────

def _gaussian_batch(x, centers, amps, sigmas):
    """Evaluate the sum of K Gaussians over N points in one exp() call.

    Parameters
    ----------
    x       : (N,) array — evaluation grid (original pixel coordinates)
    centers : (K,) array — peak centres
    amps    : (K,) array — peak amplitudes (integrated area, lmfit convention)
    sigmas  : (K,) array — peak sigmas (may all be equal for GMM mode)

    Returns
    -------
    (N,) array — sum of all K Gaussian contributions
    """
    # z shape: (K, N)
    z = (x[np.newaxis, :] - centers[:, np.newaxis]) / sigmas[:, np.newaxis]
    # prefactors shape: (K, 1) for broadcasting
    prefactors = (amps / (sigmas * _SQRT_2PI))[:, np.newaxis]
    return (prefactors * np.exp(-0.5 * z * z)).sum(axis=0)


# ── model functions ──────────────────────────────────────────────────────────

def layerlineModelGMM(
    x, centerX, bg_line, bg_sigma, bg_amplitude,
    center_sigma1, center_amplitude1,
    center_sigma2, center_amplitude2,
    common_sigma, **kwargs,
):
    """GMM model (shared sigma) with vectorised peak accumulation."""
    # Background + meridian (3 single Gaussians, cheap)
    result = (
        _gaussian_eval(x, bg_amplitude, centerX, bg_sigma) + bg_line
        + _gaussian_eval(x, center_amplitude1, centerX, center_sigma1)
        + _gaussian_eval(x, center_amplitude2, centerX, center_sigma2)
    )

    # Collect peak parameters in one pass
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

    # Gaussian batch — single exp() call for all K_g peaks
    if g_centers:
        n_g = len(g_centers)
        result += _gaussian_batch(
            x,
            np.asarray(g_centers, dtype=np.float64),
            np.asarray(g_amps,    dtype=np.float64),
            np.full(n_g, common_sigma, dtype=np.float64),
        )

    # Voigt (rare in PT data): loop fallback is fine
    for c, a, g in voigt_peaks:
        result += _voigt_eval(x, a, c, common_sigma, g)

    return result


def layerlineModel(
    x, centerX, bg_line, bg_sigma, bg_amplitude,
    center_sigma1, center_amplitude1,
    center_sigma2, center_amplitude2, **kwargs,
):
    """Standard model (independent sigma per peak) with vectorised accumulation."""
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
        result += _gaussian_batch(
            x,
            np.asarray(g_centers, dtype=np.float64),
            np.asarray(g_amps,    dtype=np.float64),
            np.asarray(g_sigmas,  dtype=np.float64),
        )

    for c, a, s, g in voigt_peaks:
        result += _voigt_eval(x, a, c, s, g)

    return result
