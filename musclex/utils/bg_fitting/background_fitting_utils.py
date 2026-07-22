"""Helper functions for iterative 2D background fitting.

Headless (no file I/O, no matplotlib) support code for
:mod:`musclex.utils.background_fitting`:

* image geometry -- center crop to a fit size and reconstruct model
  backgrounds at the original full resolution;
* wedge-based initialization of the general-background exponential;
* goodness-of-fit / oversubtraction metrics and best-iteration selection;
* the pyFAI angular-sector radial projections used by the step-0 pass.

Most of this is ported from the bg-optimization research scripts
(``evaluate_models.py`` and ``fit_two_stage_iterative.py``); the masks and
1D projections used elsewhere in the GUI come from musclex itself
(``QuadrantFolder.createMask`` / ``utils.bg_search.background_search``).
"""

from __future__ import annotations

import numpy as np
from skimage.transform import resize

from .elliptical_saxs_fitter import EllipticalSAXSFitter
from ..image_processor import distance


# --------------------------------------------------------------------------- #
# Image geometry: crop to a fit size, reconstruct at full resolution
# --------------------------------------------------------------------------- #
def center_crop(image, crop_size):
    """Center-crop (or center-pad) ``image`` to ``crop_size`` = (h, w).

    Larger images are cropped around the center; smaller ones are zero-padded
    so the original center stays at the new center. This keeps the pixel scale
    and center unchanged, so a model fit on the crop can be reconstructed on a
    differently sized (e.g. full-resolution) canvas with the same parameters.
    """
    image = np.asarray(image)
    th, tw = int(crop_size[0]), int(crop_size[1])
    h, w = image.shape[:2]

    out = np.zeros((th, tw), dtype=image.dtype)

    # source crop window (centered)
    sy0 = max((h - th) // 2, 0)
    sx0 = max((w - tw) // 2, 0)
    sy1 = min(sy0 + th, h)
    sx1 = min(sx0 + tw, w)

    # destination window (centered)
    dy0 = max((th - h) // 2, 0)
    dx0 = max((tw - w) // 2, 0)
    dy1 = dy0 + (sy1 - sy0)
    dx1 = dx0 + (sx1 - sx0)

    out[dy0:dy1, dx0:dx1] = image[sy0:sy1, sx0:sx1]
    return out


def crop_to_fit(img, fit_size, mask=None):
    """Center-crop ``img`` (and optionally ``mask``) to a square fit window.

    ``fit_size`` is the target side length used for fitting (e.g. 2000). The
    side is clamped to the image and made even. The original image shape is
    returned so backgrounds can later be reconstructed at full resolution.

    Returns ``(img_fit, full_shape)`` or ``(img_fit, mask_fit, full_shape)``.
    """
    full_shape = img.shape
    side = min(int(fit_size), img.shape[0], img.shape[1])
    side -= side % 2
    img_fit = center_crop(img, (side, side))
    if mask is None:
        return img_fit, full_shape
    mask_fit = center_crop(np.asarray(mask).astype(np.float32), (side, side)) > 0.5
    return img_fit, mask_fit, full_shape


# def _downsample(img, ds, aa=True):
#     """Downsample an image by an integer factor (no-op when ds <= 1)."""
#     if ds <= 1:
#         return img
#     return resize(img, (img.shape[0] // ds, img.shape[1] // ds), anti_aliasing=aa)


def upsample_to_full(bg, full_shape):
    """Resize a (quadrant/cropped) background up to ``full_shape``."""
    if tuple(bg.shape) == tuple(full_shape):
        return bg
    return resize(bg, full_shape, anti_aliasing=False)


# --------------------------------------------------------------------------- #
# Model reconstruction (evaluate a fitted model onto a full-size canvas)
# --------------------------------------------------------------------------- #
def reconstruct_equator(params, shape, ds, max_norm, keep_baseline):
    """Reconstruct the elliptical equator streak model onto ``shape``.

    ``params`` are in the (downsampled) fit coordinate scale; the model is
    evaluated on a ``shape // ds`` grid then resized to ``shape`` so the fit
    performed on a cropped/downsampled strip is painted onto the full canvas.
    When ``keep_baseline`` is False the flat baseline (params[1]) is zeroed so
    only the streak is subtracted.
    """
    p = np.asarray(params, dtype=float).copy()
    if not keep_baseline:
        p[1] = 0.0
    dummy = np.zeros((shape[0] // ds, shape[1] // ds))
    f = EllipticalSAXSFitter(
        dummy, mask=None, model="elliptical", n_peaks=1, downsample_factor=1
    )
    e = f.compute_intensity(p) * max_norm
    return resize(e, shape, anti_aliasing=False)


def reconstruct_general(params, shape, comp2, ds, max_norm):
    """Reconstruct the circular exp+comp2+baseline general background onto ``shape``."""
    dummy = np.zeros((shape[0] // ds, shape[1] // ds))
    f = EllipticalSAXSFitter(
        dummy,
        mask=None,
        model="circular_exponential",
        K=0,
        n_peaks=1,
        downsample_factor=1,
        comp1="exponential",
        comp2=comp2,
        comp3=None,
        aspect=True,
    )
    g = f.compute_intensity(params) * max_norm
    return resize(g, shape, anti_aliasing=False)


# --------------------------------------------------------------------------- #
# Loss-mode kwargs for the fitter
# --------------------------------------------------------------------------- #
def loss_mode_kwargs(mode, neg_weight, pos_weight):
    """Map a loss-mode name to keyword args for ``EllipticalSAXSFitter.fit``.

    ``linear_asym`` penalizes model>data (oversubtraction) ``neg_weight`` times
    harder than under-fit; ``log`` fits log-residuals with no asymmetry.
    """
    if mode == "linear_asym":
        return dict(
            loss="linear", log_space=False, neg_weight=neg_weight, pos_weight=pos_weight
        )
    if mode == "log":
        return dict(loss="linear", log_space=True, neg_weight=1.0, pos_weight=1.0)
    raise ValueError(f"unknown loss mode {mode!r}")


# --------------------------------------------------------------------------- #
# Wedge radial profile + general-background initialization
# --------------------------------------------------------------------------- #
def wedge_radial_profile(pattern, mask, x0, y0, amin, amax, dr=1.0):
    """Azimuthally averaged radial profile over the diagonal wedge
    ``amin <= |angle from equator| <= amax`` (folded into all 4 quadrants).

    This sector avoids the equatorial streak (0 deg) and the meridional peaks
    (90 deg), giving a clean view of the isotropic background used to seed the
    exponential. Returns ``(r_centers, profile)`` or ``(None, None)``.
    """
    ny, nx = pattern.shape
    yy, xx = np.mgrid[0:ny, 0:nx]
    xc = xx - x0
    yc = yy - y0
    R = np.hypot(xc, yc)
    ang = np.degrees(np.arctan2(np.abs(yc), np.abs(xc)))
    wedge = (ang >= amin) & (ang <= amax)
    valid = wedge & mask & np.isfinite(pattern)
    if valid.sum() < 10:
        return None, None
    r_max = int(R[valid].max()) + 1
    edges = np.arange(0, r_max + dr, dr)
    r_centers = 0.5 * (edges[:-1] + edges[1:])
    n_bins = len(r_centers)
    idx = np.clip(np.digitize(R, edges) - 1, 0, n_bins - 1)[valid]
    vals = pattern[valid].astype(float)
    s = np.bincount(idx, weights=vals, minlength=n_bins)
    c = np.bincount(idx, minlength=n_bins)
    prof = np.where(c > 0, s / c, np.nan)
    return r_centers, prof


def init_from_wedge(r, prof, rmin):
    """Estimate (baseline, amp, decay_length) from the wedge profile.

    baseline = robust floor of the outer profile; amp = inner value above
    baseline; decay = slope of log(prof-baseline) vs r. Returns a dict or None.
    """
    good = np.isfinite(prof) & (r >= rmin)
    rg, pg = r[good], prof[good]
    if rg.size < 8:
        return None

    n_tail = max(3, int(0.2 * rg.size))
    baseline = float(np.nanmedian(pg[-n_tail:]))
    baseline = max(min(baseline, float(np.nanpercentile(pg, 2))), 0.0)

    n_head = max(2, int(0.05 * rg.size))
    amp = float(np.nanmedian(pg[:n_head]) - baseline)
    amp = max(amp, 1e-6)

    above = pg - baseline
    decay_ok = above > 0.02 * amp
    decay = None
    if decay_ok.sum() >= 5:
        rr = rg[decay_ok]
        ll = np.log(above[decay_ok])
        slope = np.polyfit(rr, ll, 1)[0]
        if slope < 0:
            decay = float(-1.0 / slope)
    if decay is None or not np.isfinite(decay):
        decay = float(max(0.25 * (rg.max() - rg.min()), 1.0))
    decay = float(np.clip(decay, 1.0, 0.5 * rg.max()))

    return {"baseline": baseline, "amp": amp, "decay": decay}


def inject_initial_params(base_params, init, aspect=True):
    """Override baseline / exp-amp / exp-scale of the circular_exponential
    initial-parameter vector with the wedge estimates.

    Layout (aspect=True, K=0): [baseline, rmin, e, a0_amp, a0_scl_raw, <comp2...>]
    """
    p = np.asarray(base_params, dtype=float).copy()
    p[0] = init["baseline"]
    amp_idx = 3 if aspect else 2
    scl_idx = amp_idx + 1
    p[amp_idx] = init["amp"]
    d0 = init["decay"]
    p[scl_idx] = np.log(np.expm1(d0)) if d0 > 1 else d0  # softplus^-1(d0)
    return p


# --------------------------------------------------------------------------- #
# Metrics
# --------------------------------------------------------------------------- #
def compute_metrics(data, model, mask, n_params):
    """Goodness-of-fit + oversubtraction metrics on masked pixels.

    Residual = data - model; residual < 0 means the model sits above the data
    (oversubtraction).
    """
    v = mask & np.isfinite(data) & np.isfinite(model)
    d = data[v].astype(float)
    m = model[v].astype(float)
    n = d.size
    res = d - m
    out = {"n_pixels": int(n)}
    if n < 5:
        return out

    ss_res = float(np.sum(res**2))
    ss_tot = float(np.sum((d - d.mean()) ** 2)) + 1e-12
    out["r2"] = 1.0 - ss_res / ss_tot
    out["rmse"] = float(np.sqrt(ss_res / n))
    med = float(np.median(np.abs(d))) + 1e-12
    out["nrmse_pct"] = 100.0 * out["rmse"] / med
    out["mae"] = float(np.mean(np.abs(res)))
    out["median_abs_res"] = float(np.median(np.abs(res)))

    sigma2 = np.maximum(d, 1.0)
    dof = max(n - n_params, 1)
    out["chi2_red"] = float(np.sum(res**2 / sigma2) / dof)

    pos = (d > 0) & (m > 0)
    if pos.sum() > 5:
        lr = np.log(d[pos]) - np.log(m[pos])
        out["logres_rms"] = float(np.sqrt(np.mean(lr**2)))
        out["logres_bias"] = float(np.mean(lr))
    else:
        out["logres_rms"] = np.nan
        out["logres_bias"] = np.nan

    aic = n * np.log(ss_res / n + 1e-30) + 2 * n_params
    bic = n * np.log(ss_res / n + 1e-30) + n_params * np.log(n)
    out["aic"] = float(aic)
    out["bic"] = float(bic)

    over = res < 0
    out["oversub_frac"] = float(np.mean(over))
    out["oversub_flux_frac"] = float(np.sum(-res[over]) / (np.sum(d) + 1e-12))
    out["oversub_max_norm"] = float((-res[over]).max() / med) if over.any() else 0.0
    out["oversub_mean_norm"] = float((-res[over]).mean() / med) if over.any() else 0.0
    out["n_params"] = int(n_params)
    return out


def negative_stats(residual, valid):
    """Negative-pixel / oversubtraction stats over the real-data region ``valid``."""
    d = residual[valid].astype(float)
    n = d.size
    neg = d < 0
    pos_sum = float(np.sum(d[d > 0])) + 1e-12
    return {
        "n_valid": int(n),
        "n_negative": int(neg.sum()),
        "frac_negative": float(neg.mean()) if n else float("nan"),
        "min": float(d.min()) if n else float("nan"),
        "neg_sum": float(-d[neg].sum()) if neg.any() else 0.0,
        "neg_mean": float(d[neg].mean()) if neg.any() else 0.0,
        "oversub_flux_frac": float(-d[neg].sum() / pos_sum) if neg.any() else 0.0,
    }


def decision_score(metrics):
    """Anti-oversubtraction decision score (lower = better).

    Oversubtracted flux dominates, then the spread of oversubtraction, then fit
    quality as a tie-break.
    """
    ofl = metrics.get("oversub_flux_frac", np.nan)
    ofr = metrics.get("oversub_frac", np.nan)
    chi = metrics.get("chi2_red", np.nan)
    if not np.isfinite(ofl):
        return float("inf")
    return float(
        ofl * 100.0
        + 0.5 * (ofr if np.isfinite(ofr) else 1.0)
        + 0.1 * np.log(chi if (np.isfinite(chi) and chi > 0) else 1e6)
    )


def equator_lobes_ok(eq_params, max_v0_dev):
    """Decide whether the fitted equator forms the proper compact two-lobe streak.

    eq_params layout: [A, baseline, amp, u0, uw, v0, vw, m].
    Returns (ok: bool, info: dict).
    """
    p = np.asarray(eq_params, dtype=float)
    A = float(p[0])
    amp = float(p[2])
    u0 = float(p[3])
    uw = float(p[4])
    v0 = float(p[5])
    vw = float(p[6])
    v_mod = abs(v0) % 180.0
    v0_dev = min(v_mod, 180.0 - v_mod)
    x_peak = float(np.hypot(max(u0, 0.0), A))
    sep = 2.0 * x_peak
    ok = bool(amp > 0 and A < 10 and vw >= 2 and v0_dev <= max_v0_dev)
    info = {
        "A": A,
        "amp": amp,
        "u0": u0,
        "uw": uw,
        "v0": v0,
        "v0_dev": v0_dev,
        "x_peak": x_peak,
        "sep": sep,
        "ok": ok,
    }
    return ok, info


def auto_reduce(frac_fn, max_reduction=0.25, step=0.005, min_improve=0.001):
    """Smallest reduction fraction that captures most of the oversubtraction
    benefit: increase the cut until the negative-pixel fraction stops improving.

    Returns (best_r, history) where history is a list of (r, frac_negative).
    """
    r = 0.0
    prev = float(frac_fn(0.0))
    history = [(0.0, prev)]
    while r < max_reduction - 1e-9:
        r_next = min(r + step, max_reduction)
        frac = float(frac_fn(r_next))
        history.append((r_next, frac))
        if (prev - frac) <= min_improve:
            break
        r, prev = r_next, frac
    return r, history


def select_best_iteration(iter_records):
    """Pick the best iteration: fewest oversubtracted pixels after the fixed
    general reduction, among iterations whose equator forms two lobes; tie-break
    by general fit cost. Falls back to the least-bad iteration if none form two
    lobes. The auto reduction (and the final full reduction) are applied to the
    winner afterwards, not during selection.

    Returns (best_record, fallback: bool).
    """

    def key(rec):
        return (rec["neg_red"]["n_negative"], rec["gen_cost"])

    valid = [r for r in iter_records if r["lobe_ok"]]
    if valid:
        return min(valid, key=key), False
    return min(iter_records, key=key), True


# --------------------------------------------------------------------------- #
# Angular-sector radial projections (pyFAI) -- used by the step-0 pass
# --------------------------------------------------------------------------- #
def get_radial_projections(
    copy_img, copy_img_masked, rmin=100, rmax=950, step=1, ignore_equator=0, srange=None
):
    """Radial intensity profiles integrated over angular sectors (bottom-left
    quadrant, 180-270 deg), using pyFAI azimuthal integration.

    ``srange`` is an optional list of ``(start_deg, end_deg)`` sectors; when
    None, sectors of width ``step`` spanning 180-270 deg are used. Returns
    ``(projections_x, projections_y, projections_y_full)`` -- the masked-image
    profiles, and the full-image profiles (peaks included). Ported from the
    bg-optimization research repo (``src/data/dimg_projections.py``).
    """
    from pyFAI.method_registry import IntegrationMethod
    from pyFAI.azimuthalIntegrator import AzimuthalIntegrator

    center = [copy_img.shape[1] - 1, copy_img.shape[0] - 1]
    npt_rad = int(distance(center, (0, 0)))
    ai = AzimuthalIntegrator(detector="agilent_titan")
    ai.setFit2D(100, center[0], center[1])
    hist_x = list(np.arange(rmin, rmax + 1))

    projections_y = []
    projections_y_full = []
    projections_x = []

    integration_method = IntegrationMethod.select_one_available(
        "csr", dim=1, default="csr", degradable=True
    )

    if srange is not None:
        sectors = list(srange)
    else:
        step = 1 if step not in [0.5, 1, 2, 3, 5, 9, 10, 15, 18] else step
        sectors = []
        for deg in np.arange(180, 270 + step - ignore_equator, step):
            if deg == 180:
                sectors.append((180, 180 + step / 2))
            elif deg >= 270:
                sectors.append((270 - step / 2, 270))
            else:
                sectors.append((deg - step / 2, deg + step / 2))

    for start_deg, end_deg in sectors:
        _, I = ai.integrate1d(
            copy_img_masked,
            npt_rad,
            unit="r_mm",
            method=integration_method,
            azimuth_range=(start_deg, end_deg),
            correctSolidAngle=False,
        )
        hist_y = I[int(rmin) : int(rmax + 1)]
        hist_y = list(np.concatenate((hist_y, np.zeros(len(hist_x) - len(hist_y)))))
        projections_y.append(hist_y)
        projections_x.append(hist_x)

        _, I = ai.integrate1d(
            copy_img,
            npt_rad,
            unit="r_mm",
            method=integration_method,
            azimuth_range=(start_deg, end_deg),
            correctSolidAngle=False,
        )
        hist_y = I[int(rmin) : int(rmax + 1)]
        hist_y = list(np.concatenate((hist_y, np.zeros(len(hist_x) - len(hist_y)))))
        projections_y_full.append(hist_y)

    return projections_x, projections_y, projections_y_full
