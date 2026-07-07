"""1D per-sector cone-background model (Chebyshev polynomial + optional
physical tails) used by the step-0 projection background pass.

Vendored verbatim from the bg-optimization research repo
(``src/bg_removal/gen_bg_fit/fit_sector_var_model.py``). Depends only on
numpy/scipy, so it is self-contained.
"""

import numpy as np
from dataclasses import dataclass
from typing import Tuple, Dict, Optional, Sequence
from scipy.optimize import least_squares
from scipy.special import j1  # First order Bessel function

# -----------------------------
# Utilities
# -----------------------------
def normalize_q(q: np.ndarray, qmin: float, qmax: float) -> np.ndarray:
    """Map q in [qmin, qmax] to q_tilde in [-1, 1]."""
    return 2 * (q - qmin) / (qmax - qmin) - 1

def chebyshev_design(q_tilde: np.ndarray, m: int) -> np.ndarray:
    """
    Build Chebyshev T_j(q_tilde) columns for j=0..m.
    Shape: (N, m+1)
    """
    # Use recurrence: T0=1, T1=q~, T_{j+1}=2 q~ T_j - T_{j-1}
    N = q_tilde.shape[0]
    X = np.zeros((N, m+1), dtype=float)
    X[:,0] = 1.0
    if m >= 1:
        X[:,1] = q_tilde
        for j in range(1, m):
            X[:, j+1] = 2*q_tilde*X[:, j] - X[:, j-1]
    return X

def anscombe(x: np.ndarray) -> np.ndarray:
    """Anscombe variance stabilizing transform for Poisson-like data."""
    return 2.0 * np.sqrt(np.maximum(x, 0.0) + 3.0/8.0)

# -----------------------------
# Model definition
# -----------------------------
@dataclass
class ConeModelConfig:
    m: int = 4                      # Chebyshev degree
    use_upturn: bool = True
    use_exp_decay: bool = True
    use_exp_decay_second: bool = False
    use_power_law: bool = False
    use_gaussian: bool = False
    use_lorentzian: bool = False
    use_bessel_j1: bool = False
    use_pearson_vii: bool = False
    use_modified_lorentzian: bool = False
    # Reasonable defaults; they will be optimized starting from here
    p_init: float = 2.0             # exponent for low-q upturn
    q0_init: float = 0.03           # Å^{-1}, soft cutoff to avoid singularity
    d_init: float = 1.5             # Å, decay rate for high-q tail

@dataclass
class FitResult:
    coeffs: np.ndarray              # [a0..am, (A_low, q0, p), (A_hi, d)] subset depending on config
    success: bool
    message: str
    qmin: float
    qmax: float
    config: ConeModelConfig
    # diagnostics
    rmse: float
    dof: int

def model_eval(q: np.ndarray, params: np.ndarray, qmin: float, qmax: float, cfg: ConeModelConfig, nonneg: bool=True) -> np.ndarray:
    """
    Evaluate B_cone(q) for given params.
    Param vector layout:
      base = [a0..am]
      if use_exp_decay: append [A_hi, d]
      if use_upturn:  append [A_low, q0, p]
      
    """
    idx = 0
    a = params[idx:idx+cfg.m+1]; idx += cfg.m+1
    q_tilde = normalize_q(q, qmin, qmax)
    X = chebyshev_design(q_tilde, cfg.m)
    y = X @ a

    if cfg.use_exp_decay:
        A_hi, d = params[idx:idx+2]; idx += 2
        y = y + A_hi * np.exp(-d * q)
    if cfg.use_exp_decay_second:
        A_hi2, d2 = params[idx:idx+2]; idx += 2
        y = y + A_hi2 * np.exp(-d2 * q)
    if cfg.use_upturn:
        A_low, q0, p = params[idx:idx+3]; idx += 3
        y = y + A_low * (q**2 + q0**2) ** (-0.5 * p)
    if cfg.use_power_law:
        A_pl, q0, p_pl = params[idx:idx+3]; idx += 3
        y = y + A_pl * (q + q0) ** (-p_pl)
    if cfg.use_gaussian:
        A_g, mu_g, sigma_g = params[idx:idx+3]; idx += 3
        y = y + A_g * np.exp(-0.5 * ((q - mu_g)/sigma_g)**2)
    if cfg.use_lorentzian:
        A_l, mu_l, gamma_l = params[idx:idx+3]; idx += 3
        y = y + A_l * (gamma_l / ((q - mu_l)**2 + gamma_l**2))
    if cfg.use_bessel_j1:
        A_j1, j1_q0, j1_scale = params[idx:idx+3]; idx += 3
        # Use J1 Bessel function with scaling parameter
        y = y + A_j1 * j1(j1_scale * q) / (j1_scale * (q + j1_q0))
    if cfg.use_pearson_vii:
        A_vii, q0_vii, w_vii = params[idx:idx+3]; idx += 3
        m = 2
        k = 4.0 * (2.0**(1.0/m) - 1.0) / (w_vii**2)
        y = y + A_vii * (1.0 / (1.0 + k * (q - q0_vii)**2)**m)
    if cfg.use_modified_lorentzian:
        A_ml, q0_ml, width, m = params[idx:idx+4]; idx += 4
        k = 4.0 * (2.0**(1.0/m) - 1.0) / (width**2)
        y = y + A_ml * (1.0 / (1.0 + k * (q - q0_ml)**2)**m)
    if nonneg:
        y = np.maximum(y, 0.0)
    return y  # optional non-negativity

def initial_params(y: np.ndarray, q: np.ndarray, w: np.ndarray, cfg: ConeModelConfig, qmin: float, qmax: float) -> np.ndarray:
    """
    Get a robust linear init for [a0..am] by solving weighted least squares
    without the edge terms, then append edge-term inits.
    """
    q_tilde = normalize_q(q, qmin, qmax)
    X = chebyshev_design(q_tilde, cfg.m)
    # Weighted least squares for a_j
    W = np.sqrt(w)
    a, *_ = np.linalg.lstsq(X * W[:, None], y * W, rcond=None)
    params = [*a]

    if cfg.use_exp_decay:
        params += [max(0.0, 0.9*np.nanmax(y)), max(1e-3, cfg.d_init)]
        # print("Initial params after exp decay:", params)
    if cfg.use_exp_decay_second:
        params += [max(0.0, 0.9*np.nanmax(y)), max(1e-3, cfg.d_init)]
        # print("Initial params after exp decay second:", params)
    if cfg.use_upturn:
        params += [max(0.0, 0.1*np.nanmax(y)), max(1e-3, cfg.q0_init), np.clip(cfg.p_init, 0.5, 4.0)]
        # print("Initial params after upturn:", params)
    if cfg.use_power_law:
        params += [max(0.0, 0.1*np.nanmax(y)), max(1e-3, cfg.q0_init), np.clip(cfg.p_init, 0.5, 4.0)]
        # print("Initial params after power law:", params)
    if cfg.use_gaussian:
        params += [max(0.0, 0.9*np.nanmax(y)), 0.1*(qmax - qmin) + qmin, 0.05*(qmax - qmin)]
        # print("Initial params after gaussian:", params)
    if cfg.use_lorentzian:
        params += [max(0.0, 0.9*np.nanmax(y)), 0.1*(qmax - qmin) + qmin, 0.05*(qmax - qmin)]
        # print("Initial params after lorentzian:", params)
    if cfg.use_bessel_j1:
        params += [max(0.0, 0.9*np.nanmax(y)), max(1e-3, cfg.q0_init), 2.0 / (qmax - qmin)]  # A_j1, j1_q0, j1_scale
        # print("Initial params after bessel j1:", params)
    if cfg.use_pearson_vii:
        params += [max(0.0, 0.9*np.nanmax(y)), 0.1*(qmax - qmin) + qmin, 0.05*(qmax - qmin)]
    if cfg.use_modified_lorentzian:
        params += [max(0.0, 0.9*np.nanmax(y)), 0.1*(qmax - qmin) + qmin, 0.05*(qmax - qmin), 2]
    return np.array(params, dtype=float)

def bounds_for_params(cfg: ConeModelConfig, m: int) -> Tuple[np.ndarray, np.ndarray]:
    """
    Build parameter bounds. Keep polynomials unbounded; constrain physical extras.
    """
    lower = [-np.inf]*(m+1)
    upper = [ np.inf]*(m+1)
    if cfg.use_exp_decay:
        lower += [0.0, 1e-4]          # A_hi >=0, d > 0
        upper += [np.inf, np.inf]
    if cfg.use_exp_decay_second:
        lower += [0.0, 1e-4]          # A_hi2 >=0, d2 > 0
        upper += [np.inf, np.inf]
    if cfg.use_upturn:
        lower += [0.0, 1e-4, 0.5]     # A_low >=0, q0>0,  0.5 <= p <= 4
        upper += [np.inf, 0.1, 4.0]
    if cfg.use_power_law:
        lower += [0.0, 1e-4, 0.5]     # A_pl >=0, q0>0,  0.5 <= p <= 4
        upper += [np.inf, 0.1, 4.0]
    if cfg.use_gaussian:
        lower += [0.0, 0, 1e-4]    # A_g >=0, mu_g >= 0, sigma_g > 0
        upper += [np.inf, 0.5, 1.0]
    if cfg.use_lorentzian:
        lower += [0.0, 0, 1e-4]    # A_l >=0, mu_l >= 0, gamma_l > 0
        upper += [np.inf, 0.9, 1.0]
    if cfg.use_bessel_j1:
        lower += [0.0, -0.5, 0.1]        # A_j1 >=0, j1_q0 >= 0, j1_scale > 0
        upper += [np.inf, 0.5, 100.0]
    if cfg.use_pearson_vii:
        lower += [0.0, 0, 1e-4]    # A_vii >=0, x0_vii >= 0, w_vii > 0
        upper += [np.inf, 300.0, 300.0]
    if cfg.use_modified_lorentzian: 
        lower += [0.0, 0, 1e-4, 1.0]    # A_ml >=0, x0_ml >= 0, width > 0, m >= 1
        upper += [np.inf, 300.0, 300.0, 10.0]
    return np.array(lower), np.array(upper)

def fit_cone_background(q: np.ndarray,
                        I: np.ndarray,
                        w: Optional[np.ndarray]=None,
                        cfg: ConeModelConfig=ConeModelConfig(),
                        transform_anscombe: bool=True,
                        q_range: Optional[Tuple[float,float]]=None,
                        asymmetric_weight: float=1.0,
                        loss = 'linear'
                       ) -> FitResult:
    """
    Fit B_cone(q) on samples (q, I) from one angular cone/sector.
    - q, I: 1D arrays of equal length (per-pixel samples already selected by cone and mask)
    - w: optional weights (e.g., all ones). If None, all ones are used.
    - transform_anscombe: apply Anscombe transform before fitting for robustness
    - q_range: optional (qmin, qmax) to define normalization; if None, taken from data percentiles [2%, 98%]
    """
    q = np.asarray(q).ravel()
    I = np.asarray(I).ravel()
    if w is None:
        w = np.ones_like(I, dtype=float)
    else:
        w = np.asarray(w).ravel()

    # Filter finite and positive weights
    m = np.isfinite(q) & np.isfinite(I) & np.isfinite(w) & (w > 0)
    q = q[m]; I = I[m]; w = w[m]

    # Optional variance stabilization
    y = anscombe(I) if transform_anscombe else I

    # q-range for normalization
    if q_range is None:
        qmin = float(np.quantile(q, 0.02))
        qmax = float(np.quantile(q, 0.98))
        if qmax <= qmin:
            qmin, qmax = float(np.min(q)), float(np.max(q))
    else:
        qmin, qmax = q_range

    # Initial parameters
    p0 = initial_params(y, q, w, cfg, qmin, qmax)

    # Bounds
    lb, ub = bounds_for_params(cfg, cfg.m)

    # Residuals with robust loss (Huber)
    # def residuals(p):
    #     yhat = model_eval(q, p, qmin, qmax, cfg)
    #     return np.sqrt(w) * (y - yhat)
    
    # Custom asymmetric residuals function
    def residuals(p):
        yhat = model_eval(q, p, qmin, qmax, cfg)
        raw_residuals = y - yhat
        
        # Apply asymmetric weighting
        weighted_residuals = np.where(raw_residuals < 0, 
                                    asymmetric_weight * raw_residuals, 
                                    raw_residuals)
        
        return np.sqrt(w) * weighted_residuals

    # res = least_squares(residuals, p0, bounds=(lb, ub), loss='huber', f_scale=1.345, max_nfev=5000)
    # res = least_squares(residuals, p0, bounds=(lb, ub), loss='soft_l1', max_nfev=5000)
    # res = least_squares(residuals, p0, bounds=(lb, ub), loss='cauchy', max_nfev=5000)
    # print(f"Initial parameters p0: {p0}")
    # print(f"Lower bounds lb: {lb}")
    # print(f"Upper bounds ub: {ub}")
    res = least_squares(residuals, p0, bounds=(lb, ub), loss=loss, max_nfev=5000)



    yhat = model_eval(q, res.x, qmin, qmax, cfg)
    rmse = float(np.sqrt(np.average((y - yhat)**2, weights=w)))
    dof = max(1, q.size - res.x.size)

    return FitResult(
        coeffs=res.x,
        success=res.success,
        message=res.message,
        qmin=qmin,
        qmax=qmax,
        config=cfg,
        rmse=rmse,
        dof=dof
    )


def evaluate_on_grid(fr: FitResult, q_grid: np.ndarray) -> np.ndarray:
    """Evaluate a fitted model on a given q grid (returns in the same space as fitted y, i.e., transformed if used)."""
    return model_eval(q_grid, fr.coeffs, fr.qmin, fr.qmax, fr.config)