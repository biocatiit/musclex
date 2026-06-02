"""
Analytical Jacobian helpers for GMM and standard Gaussian models.

For a Gaussian peak contributing ``G_i`` to the residual vector:

    G_i(x) = A_i / (σ_i √2π) × exp(-0.5 z_i²),   z_i = (x - c_i) / σ_i
    c_i     = centerX + p_i          (GMM / standard)

Partial derivatives of the **model** (not the negative residual) w.r.t. the
free parameters:

    ∂f/∂A_i     =  G_i / A_i
    ∂f/∂p_i     =  G_i × z_i / σ_i      (sign from c_i = centerX + p_i)
    ∂f/∂σ_i     =  G_i × (z_i² - 1) / σ_i       (standard: independent σ)
    ∂f/∂σ_com   =  Σ_i G_i × (z_i² - 1) / σ_com  (GMM: shared σ)

Background terms (bg_line, bg_amplitude×gaussian, center_amplitude1/2×gaussian)
are all fixed when bgsub==1 and live in independent_vars.  When they are free
(bgsub!=1, not common in captured data) the adapter falls back to finite
differences for those columns — they are cheap and rarely used.

The Jacobian matrix returned has shape (n_data, n_free) and represents
``∂residual/∂params = ∂model/∂params`` (scipy least_squares minimises
``‖residual(p)‖² = ‖model(p) - y_data‖²``; sign cancels in J^T J).
"""
from __future__ import annotations

import numpy as np

_SQRT_2PI = np.sqrt(2.0 * np.pi)


# ── parameter layout helpers ─────────────────────────────────────────────────

def _build_peak_index(free_names: list[str], model_kind: str) -> dict:
    """Return metadata describing where peak params sit in *free_names*.

    Returns
    -------
    dict with keys:
        'sigma_idx'    : int | None  — index of 'common_sigma' (GMM) or None
        'peaks'        : list of dicts, each with keys:
                           'p_idx'     : int   — index of 'p_i'
                           'amp_idx'   : int   — index of 'amplitudei'
                           'sigma_idx' : int|None — index of 'sigmai' (standard)
        'analytic_cols': set[int] — column indices covered by analytic Jac
    """
    sigma_idx = free_names.index('common_sigma') if 'common_sigma' in free_names else None

    peaks = []
    i = 0
    while f'p_{i}' in free_names:
        entry: dict = {
            'p_idx':   free_names.index(f'p_{i}'),
            'amp_idx': free_names.index(f'amplitude{i}'),
            'sigma_idx': None,
        }
        if f'sigma{i}' in free_names:
            entry['sigma_idx'] = free_names.index(f'sigma{i}')
        peaks.append(entry)
        i += 1

    analytic_cols: set[int] = set()
    for pk in peaks:
        analytic_cols.add(pk['p_idx'])
        analytic_cols.add(pk['amp_idx'])
        if pk['sigma_idx'] is not None:
            analytic_cols.add(pk['sigma_idx'])
    if sigma_idx is not None:
        analytic_cols.add(sigma_idx)

    return {
        'sigma_idx': sigma_idx,
        'peaks': peaks,
        'analytic_cols': analytic_cols,
    }


# ── core Jacobian computation ────────────────────────────────────────────────

def compute_jacobian(
    p: np.ndarray,
    x: np.ndarray,
    centerX: float,
    free_names: list[str],
    peak_index: dict,
    *,
    model_kind: str,
) -> np.ndarray:
    """Return the (n_data, n_free) analytical Jacobian of the model.

    Only covers peak parameters (p_i, amplitude_i, sigma_i / common_sigma).
    Non-peak free params (background gaussians when bgsub==0) are left as
    zeros here; the caller uses finite differences for those columns.

    Parameters
    ----------
    p           : current parameter vector, shape (n_free,)
    x           : data grid, shape (n_data,)
    centerX     : beam-centre pixel coordinate (fixed)
    free_names  : ordered list of free parameter names
    peak_index  : output of :func:`_build_peak_index`
    model_kind  : 'gmm' or 'standard'
    """
    n_data = len(x)
    n_free = len(free_names)
    J = np.zeros((n_data, n_free), dtype=np.float64)

    peaks = peak_index['peaks']
    sigma_idx = peak_index['sigma_idx']         # None for standard model

    # GMM: all peaks share one sigma
    if model_kind == 'gmm' and sigma_idx is not None:
        sigma = p[sigma_idx]
        inv_sig = 1.0 / sigma
        prefactor = inv_sig / _SQRT_2PI
        dsigma_col = np.zeros(n_data)

        for pk in peaks:
            center = centerX + p[pk['p_idx']]
            amp    = p[pk['amp_idx']]
            z = (x - center) * inv_sig
            G = amp * prefactor * np.exp(-0.5 * z * z)

            J[:, pk['amp_idx']] = G / amp                  # ∂/∂amp_i
            J[:, pk['p_idx']]   = G * z * inv_sig          # ∂/∂p_i
            dsigma_col         += G * (z * z - 1.0) * inv_sig  # ∂/∂σ_common

        J[:, sigma_idx] = dsigma_col

    else:
        # Standard: each peak has its own sigma
        for pk in peaks:
            s_idx  = pk['sigma_idx']
            if s_idx is None:
                # sigma not free for this peak — skip
                continue
            sigma  = p[s_idx]
            inv_sig = 1.0 / sigma
            prefactor = inv_sig / _SQRT_2PI

            center = centerX + p[pk['p_idx']]
            amp    = p[pk['amp_idx']]
            z = (x - center) * inv_sig
            G = amp * prefactor * np.exp(-0.5 * z * z)

            J[:, pk['amp_idx']] = G / amp
            J[:, pk['p_idx']]   = G * z * inv_sig
            J[:, s_idx]         = G * (z * z - 1.0) * inv_sig

    return J


def make_residual_and_jac(
    inputs,          # FitInputs
    free_names: list[str],
    x0: np.ndarray,
    *,
    fd_step: float = 1e-7,
) -> tuple:
    """Return ``(residual_fn, jac_fn, peak_index)`` ready for scipy.

    The Jacobian uses analytical derivatives for all peak params and
    falls back to central finite differences for any remaining free
    params (e.g. background gaussians when bgsub==0).

    Parameters
    ----------
    inputs      : FitInputs
    free_names  : ordered list matching the x0 / bounds vectors
    x0          : initial parameter vector (used only to detect near-zero params)
    fd_step     : relative step for fallback FD columns

    Returns
    -------
    residual_fn : callable(p) → (n_data,) residuals
    jac_fn      : callable(p) → (n_data, n_free) Jacobian
    peak_index  : the parsed peak layout dict (useful for debugging)
    """
    from musclex.tests.fitting_ab.model_variants.model_numpy import (
        layerlineModelGMM,
        layerlineModel,
    )
    model_fn = layerlineModelGMM if inputs.model_kind == 'gmm' else layerlineModel

    x_arr    = inputs.x
    y_arr    = inputs.y
    indep    = dict(inputs.independent_vars)
    centerX  = float(indep.get('centerX', 0.0))
    mk       = inputs.model_kind

    peak_index = _build_peak_index(free_names, mk)
    analytic_cols = peak_index['analytic_cols']
    fd_cols = [j for j in range(len(free_names)) if j not in analytic_cols]

    def residual_fn(p: np.ndarray) -> np.ndarray:
        kw = dict(zip(free_names, p))
        kw.update(indep)
        kw['x'] = x_arr
        return model_fn(**kw) - y_arr

    def jac_fn(p: np.ndarray) -> np.ndarray:
        J = compute_jacobian(p, x_arr, centerX, free_names, peak_index, model_kind=mk)

        # Fill non-peak columns via central FD
        if fd_cols:
            r0 = residual_fn(p)
            for j in fd_cols:
                step = max(abs(p[j]) * fd_step, fd_step)
                p_hi = p.copy(); p_hi[j] += step
                p_lo = p.copy(); p_lo[j] -= step
                J[:, j] = (residual_fn(p_hi) - residual_fn(p_lo)) / (2.0 * step)

        return J

    return residual_fn, jac_fn, peak_index
