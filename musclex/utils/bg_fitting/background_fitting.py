"""Iterative two-stage 2D background fitting for quadrant-folded SAXS patterns.

Headless fitting API (no Qt, no file I/O unless explicitly requested, no
plotting) used by :mod:`musclex.ui.BackgroundFittingDialog`.

Pipeline
--------
``step 0``  optional rough projection background ``B0`` (per-sector cone-model
            fit -> 2D convex-hull reconstruction). Seeds the first equator fit.
``stage A`` EQUATOR -- anisotropic equatorial streak (``elliptical`` model).
``stage B`` GENERAL -- isotropic, slightly elongated background
            ``exponential + comp2 + baseline`` (``circular_exponential``),
            wedge-initialized, ``linear_asym`` loss (oversubtraction penalized).

After step 0 it is alternating block-coordinate descent::

    E_k = fit_equator(img - G_{k-1})     (G_0 = B0)
    G_k = fit_general(img - E_k)

with ``comp2='auto'`` trying lorentzian/powerlaw/stretched on the first round,
keeping the kernel with the best anti-oversubtraction score and pinning it for
the remaining rounds. The best iteration (fewest oversubtracted pixels, among
two-lobe-valid equators) is selected.

Images are cropped to ``fit_size`` (e.g. 2000x2000) for fitting; the fitted
backgrounds are reconstructed at the original full resolution.

Masks come from the caller (``QuadrantFolder.createMask``); the step-0 2D
reconstruction and radial projections reuse musclex's own helpers.
"""

from __future__ import annotations

import contextlib
import os
from dataclasses import dataclass, field
from typing import Optional, Callable

import numpy as np
from skimage.transform import resize

from .elliptical_saxs_fitter import EllipticalSAXSFitter
from .cone_model import ConeModelConfig, model_eval, fit_cone_background
from . import background_fitting_utils as bfu

# --------------------------------------------------------------------------- #
# Configuration
# --------------------------------------------------------------------------- #
GEN_COMP2_CANDIDATES = ["lorentzian", "powerlaw", "stretched"]


@dataclass
class FitConfig:
    """Parameters controlling the two-stage iterative fit.

    Defaults mirror ``fit_two_stage_iterative.py``. The GUI overrides ``comp2``,
    ``iters``, ``fit_size`` and ``use_step0``; the rest are sensible defaults.
    """

    # top-level
    comp2: str = "auto"  # 'auto' | 'lorentzian' | 'powerlaw' | 'stretched'
    iters: int = 3
    fit_size: int = 2000
    use_step0: bool = True

    # geometry / preprocessing
    downsample_factor: int = 2
    eq_cut: int = 300  # equatorial strip height for the equator fit
    gen_hwidth: int = 2000  # strip height for the general-background fit
    rmin_mask: int = 30
    wedge_min: float = 25.0
    wedge_max: float = 75.0

    # fit control
    eq_max_nfev: int = 1000
    gen_max_nfev: int = 600
    eq_neg_weight: float = 20.0
    gen_neg_weight: float = 10.0
    pos_weight: float = 1.0
    equator_keep_baseline: bool = False

    # post-fit safety reductions (guard against oversubtraction). The baseline
    # reduction is always applied; the equator reduction defaults to none. With
    # auto_reduce both fractions are tuned upward on top of these fixed values
    # until the oversubtracted-pixel fraction stops improving (see bfu.auto_reduce).
    baseline_reduction: float = 0.25
    equator_reduction: float = 0.0
    auto_reduce: bool = False
    reduction_step_baseline: float = 0.005
    reduction_max_baseline: float = 0.25
    reduction_min_improve_baseline: float = 0.001
    reduction_step_equator: float = 0.005
    reduction_max_equator: float = 0.25
    reduction_min_improve_equator: float = 0.001

    # two-lobe gate
    eq_max_v0_dev: float = 5.0

    # step-0 (rough projection background)
    step0_m: int = 3
    step0_step: int = 10
    step0_srange: tuple = ((220, 255), (255, 265), (265, 270))
    step0_cone: ConeModelConfig = field(
        default_factory=lambda: ConeModelConfig(
            m=3, use_upturn=False, use_exp_decay=False, use_power_law=True
        )
    )

    quiet: bool = True


# --------------------------------------------------------------------------- #
# small utilities
# --------------------------------------------------------------------------- #
@contextlib.contextmanager
def maybe_quiet(quiet):
    """Silence the chatty fitter prints unless quiet is False."""
    if not quiet:
        yield
        return
    with open(os.devnull, "w") as devnull, contextlib.redirect_stdout(devnull):
        yield


def _downsample(img, ds, aa=True):
    """Downsample an image by an integer factor (no-op when ds <= 1)."""
    if ds <= 1:
        return img
    return resize(img, (img.shape[0] // ds, img.shape[1] // ds), anti_aliasing=aa)


# --------------------------------------------------------------------------- #
# Step 0: rough projection-based general background (per-sector cone fit)
# --------------------------------------------------------------------------- #
def run_step0_projection_bg(
    img,
    mask,
    rmin,
    rmax,
    cfg=None,
    save_params=False,
    save_img=False,
    save_dir=None,
    name="image",
):
    """Rough first-pass general background from angular-sector radial profiles.

    ``img`` is the full centered folded image; ``mask`` is a background mask
    (True = use). The bottom quadrant's sector radial profiles are fit with the
    cone model, reconstructed into a 2D background (convex-hull painter) and
    mirrored to full size.

    Nothing is written unless ``save_params`` / ``save_img`` is set (then
    ``save_dir`` must be given). Returns ``(background_full, residual, info)``.
    """
    from ...modules import QF_utilities as qfu
    from ..bg_search.background_search import makeFullImage
    from ..image_processor import weighted_neighborhood_average

    if cfg is None:
        cfg = FitConfig()
    cone_cfg = cfg.step0_cone
    step = cfg.step0_step
    srange = [tuple(s) for s in cfg.step0_srange]

    img = np.asarray(img, dtype=float)
    h, w = img.shape
    h2, w2 = h // 2, w // 2

    # bottom-left quadrant with the beam at its bottom-right corner (matches the
    # radial-projection center convention below)
    quad = img[h2:, :w2]
    quad = np.asarray(quad, dtype=float)
    qmask = np.asarray(mask, dtype=bool)[h2:, :w2]
    quad_masked = quad.copy()
    quad_masked[~qmask] = np.nan

    rmax = int(rmax if rmax is not None else int(0.9 * min(h, w) / 2))
    rmin = int(rmin)

    px, py, _ = bfu.get_radial_projections(
        quad, quad_masked, rmin=rmin, rmax=rmax, step=step, srange=srange
    )

    # fit each sector's radial profile with the cone model
    proj_pred = []
    for xs, ys in zip(px, py):
        xs = np.asarray(xs, dtype=float)
        ys = np.asarray(ys, dtype=float)
        m = (ys != 0) & np.isfinite(ys)
        if m.sum() < 30:
            # too few points -- fall back to a flat zero curve for this sector
            proj_pred.append(np.zeros_like(xs))
            continue
        x_fit = xs[m]
        y_fit = ys[m]
        x_max = float(np.max(x_fit)) or 1.0
        y_max = float(np.max(y_fit)) or 1.0
        try:
            with maybe_quiet(cfg.quiet):
                fr = fit_cone_background(
                    (x_fit / x_max).reshape(-1, 1),
                    y_fit / y_max,
                    cfg=cone_cfg,
                    transform_anscombe=False,
                    loss="huber",
                    asymmetric_weight=2.0,
                )
            y_pred = (
                model_eval(xs / x_max, fr.coeffs, fr.qmin, fr.qmax, fr.config) * y_max
            )
        except Exception:  # noqa: BLE001 -- rough pass; skip a bad sector
            y_pred = np.zeros_like(xs)
        proj_pred.append(np.asarray(y_pred, dtype="float32"))

    proj_pred = np.array(proj_pred, dtype="float32")

    # pad to the 10 angular bins make2DConvexhullBG2 expects at step=10 by
    # repeating the first (equatorial) sector curve
    if step == 10 and proj_pred.shape[0] < 10:
        first = proj_pred[:1]
        pad = np.repeat(first, 10 - proj_pred.shape[0], axis=0)
        proj_pred = np.concatenate([pad, proj_pred], axis=0)
    if proj_pred.shape[0] > 2:
        proj_pred = weighted_neighborhood_average(proj_pred, weights=[0.25, 0.5, 0.25])

    center = (quad.shape[1] - 1, quad.shape[0] - 1)
    dbg = qfu.make2DConvexhullBG2(
        proj_pred, quad.shape[1], quad.shape[0], center[0], center[1], rmin, rmax, step
    )
    bg_full = makeFullImage(dbg)
    if bg_full.shape != img.shape:
        bg_full = bfu.upsample_to_full(bg_full, img.shape)
    residual = img - bg_full

    if (save_params or save_img) and save_dir:
        from PIL import Image

        os.makedirs(save_dir, exist_ok=True)
        if save_img:
            Image.fromarray(bg_full.astype(np.float32)).save(
                os.path.join(save_dir, f"{name}_step0_background.tif")
            )
            Image.fromarray(residual.astype(np.float32)).save(
                os.path.join(save_dir, f"{name}_step0_residual.tif")
            )
        if save_params:
            np.savez(
                os.path.join(save_dir, f"{name}_step0_params.npz"),
                projections=proj_pred,
                rmin=rmin,
                rmax=rmax,
                step=step,
            )

    info = {"rmin": rmin, "rmax": rmax, "n_sectors": len(px)}
    return bg_full, residual, info


# --------------------------------------------------------------------------- #
# Stage A: equator (elliptical streak)
# --------------------------------------------------------------------------- #
def fit_equator(target, mask, cfg, quiet=True):
    """Fit the equatorial streak on ``target``.

    Returns ``(equator_full, params, max_norm, info)`` with ``equator_full``
    reconstructed at ``target``'s resolution.
    """
    h = target.shape[0]
    hc = h // 2
    cut = min(cfg.eq_cut, h)
    sl = slice(hc - cut // 2, hc + cut // 2)
    strip = target[sl, :].astype(float)
    mstrip = mask[sl, :].astype(bool)

    pos = mstrip & np.isfinite(strip) & (strip > 0)
    max_norm = float(np.nanmax(strip[pos])) if pos.any() else 1.0
    if not np.isfinite(max_norm) or max_norm <= 0:
        max_norm = 1.0
    pattern = strip / max_norm

    ds = cfg.downsample_factor
    pattern_ds = _downsample(pattern, ds, aa=True)
    mask_ds = (
        _downsample(mstrip.astype(float), ds, aa=False) > 0.5 if ds > 1 else mstrip
    )

    with maybe_quiet(quiet):
        fitter = EllipticalSAXSFitter(
            pattern_ds, mask=mask_ds, model="elliptical", n_peaks=1, downsample_factor=1
        )
        result = fitter.fit(
            n_peaks=1,
            loss="linear",
            neg_weight=cfg.eq_neg_weight,
            pos_weight=cfg.pos_weight,
            max_nfev=cfg.eq_max_nfev,
            model="elliptical",
        )
        params = fitter.fitted_params.copy()
        equator_full = bfu.reconstruct_equator(
            params, target.shape, ds, max_norm, cfg.equator_keep_baseline
        )
        equator_model = bfu.reconstruct_equator(
            params, target.shape, ds, max_norm, keep_baseline=True
        )
    eq_metrics = bfu.compute_metrics(strip, equator_model[sl, :], mstrip, len(params))
    info = {
        "success": bool(getattr(result, "success", False)),
        "cost": float(getattr(result, "cost", np.nan)),
        "metrics": eq_metrics,
    }
    return equator_full, params, max_norm, info


# --------------------------------------------------------------------------- #
# Stage B: general background (circular exp + comp2 + baseline)
# --------------------------------------------------------------------------- #
def fit_general(target, mask, cfg, comp2, quiet=True):
    """Fit the general background on ``target`` with linear_asym + wedge init.

    Returns ``(general_full, params, max_norm, wedge, cost)``.
    """
    h = target.shape[0]
    hc = h // 2
    hw = min(cfg.gen_hwidth, h)
    hw -= hw % 2
    sl = slice(hc - hw // 2, hc + hw // 2)
    strip = target[sl, :].astype(float)
    mstrip = mask[sl, :].astype(bool)

    pos = mstrip & np.isfinite(strip) & (strip > 0)
    max_norm = float(np.nanmax(strip[pos])) if pos.any() else 1.0
    if not np.isfinite(max_norm) or max_norm <= 0:
        max_norm = 1.0
    pattern = strip / max_norm

    ds = cfg.downsample_factor
    pattern_ds = _downsample(pattern, ds, aa=True)
    mask_ds = (
        _downsample(mstrip.astype(float), ds, aa=False) > 0.5 if ds > 1 else mstrip
    )

    # wedge initialization (25-75 deg) for baseline + exponential
    y0 = (pattern_ds.shape[0] - 1) / 2.0
    x0 = (pattern_ds.shape[1] - 1) / 2.0
    r, prof = bfu.wedge_radial_profile(
        pattern_ds, mask_ds, x0, y0, cfg.wedge_min, cfg.wedge_max
    )
    init = (
        bfu.init_from_wedge(r, prof, max(cfg.rmin_mask // ds, 1))
        if r is not None
        else None
    )

    with maybe_quiet(quiet):
        fitter = EllipticalSAXSFitter(
            pattern_ds,
            mask=mask_ds,
            model="circular_exponential",
            K=0,
            n_peaks=1,
            downsample_factor=1,
            comp1="exponential",
            comp2=comp2,
            comp3=None,
            aspect=True,
        )
        base = fitter.get_initial_params(
            n_peaks=1, K=0, model="circular_exponential", verbose=False
        )
        if init is not None:
            base = bfu.inject_initial_params(base, init, aspect=True)
        kw = bfu.loss_mode_kwargs("linear_asym", cfg.gen_neg_weight, cfg.pos_weight)
        result = fitter.fit(
            initial_params=base,
            n_peaks=1,
            K=0,
            max_nfev=cfg.gen_max_nfev,
            model="circular_exponential",
            comp1="exponential",
            comp2=comp2,
            comp3=None,
            aspect=True,
            **kw,
        )
        params = fitter.fitted_params.copy()

    cost = float(getattr(result, "cost", np.nan))
    general_full = bfu.reconstruct_general(params, target.shape, comp2, ds, max_norm)
    return general_full, params, max_norm, (r, prof, init), cost


def fit_general_best(target, mask, cfg, quiet=True, comp2=None):
    """Fit the general background trying each candidate comp2 kernel and keep the
    winner by anti-oversubtraction score.

    ``comp2`` selects the kernel(s): ``'auto'`` tries every candidate, a specific
    kernel name pins it, and ``None`` falls back to ``cfg.comp2``.

    Returns ``(best, candidates)``.
    """
    comp2 = cfg.comp2 if comp2 is None else comp2
    candidates = GEN_COMP2_CANDIDATES if comp2 == "auto" else [comp2]
    h = target.shape[0]
    hc = h // 2
    hw = min(cfg.gen_hwidth, h)
    hw -= hw % 2
    sl = slice(hc - hw // 2, hc + hw // 2)
    cands = []
    for comp2 in candidates:
        try:
            general, params, norm, wedge, cost = fit_general(
                target, mask, cfg, comp2, quiet
            )
        except Exception as e:  # noqa: BLE001
            print(f"    [comp2={comp2} FAIL] {e}")
            continue
        metrics = bfu.compute_metrics(
            target[sl, :], general[sl, :], mask[sl, :], len(params)
        )
        score = bfu.decision_score(metrics)
        cands.append(
            {
                "comp2": comp2,
                "general": general,
                "params": params,
                "norm": norm,
                "score": score,
                "cost": cost,
                "metrics": metrics,
                "wedge": wedge,
            }
        )
    if not cands:
        raise RuntimeError("all comp2 candidates failed for the general fit")
    best = min(cands, key=lambda c: c["score"])
    return best, cands


# --------------------------------------------------------------------------- #
# Post-fit reduced reconstruction
# --------------------------------------------------------------------------- #
def reduce_backgrounds(
    img,
    eq_params,
    gen_params,
    eq_norm,
    gen_norm,
    comp2,
    ds,
    eq_red,
    bl_red,
    equator_keep_baseline=False,
):
    """Reconstruct the reduced equator/general backgrounds and residual.

    Rebuilds the full-resolution backgrounds from the fitted parameters with the
    given equator/baseline reduction *fractions* applied (the equator is scaled
    by ``1 - eq_red``; the general baseline parameter is cut by ``1 - bl_red``
    before reconstruction). Returns ``(equator_full, general_full, residual)``.

    This is the same reconstruction the fit driver performs for its chosen
    reductions, factored out so the UI can recompute cheaply when the user edits
    the reductions after a fit without re-running the fit.
    """
    full_shape = img.shape
    equator_full = bfu.reconstruct_equator(
        eq_params, full_shape, ds, eq_norm, equator_keep_baseline
    )
    equator_full = equator_full * (1.0 - float(eq_red))
    gen_params_red = np.asarray(gen_params, dtype=float).copy()
    gen_params_red[0] *= 1.0 - float(bl_red)
    general_full = bfu.reconstruct_general(
        gen_params_red, full_shape, comp2, ds, gen_norm
    )
    residual_full = img - equator_full - general_full
    return equator_full, general_full, residual_full


# --------------------------------------------------------------------------- #
# Main driver
# --------------------------------------------------------------------------- #
def two_stage_iterative_fit(
    img,
    general_mask,
    equator_mask=None,
    rmin=30,
    rmax=None,
    cfg=None,
    progress_cb=None,
    rminrmax_mask=None,
):
    """Run the two-stage iterative background fit.

    Parameters
    ----------
    img : 2D array
        Full-resolution centered folded image.
    general_mask : 2D bool array
        Background mask for the general fit (True = use). Same shape as ``img``.
    equator_mask : 2D bool array, optional
        Mask for the equator streak fit (keeps the equatorial region). Defaults
        to ``general_mask`` if not given.
    rmin, rmax : int
        Radial mask limits (full-res px). ``rmax`` defaults to ~0.9*side/2.
    cfg : FitConfig
    progress_cb : callable(stage: str, frac: float), optional
        Called with progress updates for a GUI.
    rminrmax_mask : 2D bool array, optional
        The rmin..rmax annulus (True = inside the ring), same shape as ``img``,
        as built by ``QuadrantFolder._create_rminrmax_mask``. When given, every
        oversubtraction metric (iteration selection, reduction sweeps and the
        reported final metric) is restricted to this ring so all optimization
        and reporting share the same region. Falls back to a synthetic annulus
        computed from ``rmin``/``rmax`` about the crop center if not provided.

    Returns
    -------
    dict with keys: ``equator``, ``general``, ``residual`` (full-res),
    ``equator_params``, ``general_params``, ``comp2``, ``best_iter``,
    ``fallback``, ``rounds`` (per-iteration records), ``step0`` (bg or None),
    ``max_norm`` for reconstruction, and ``full_shape``.
    """
    if cfg is None:
        cfg = FitConfig()

    def report(stage, frac):
        if progress_cb is not None:
            progress_cb(stage, float(frac))

    img = np.asarray(img, dtype=float)
    full_shape = img.shape

    # crop to fit size (backgrounds are reconstructed at full resolution below)
    img_fit, gmask_fit, _ = bfu.crop_to_fit(img, cfg.fit_size, general_mask)
    if equator_mask is None:
        emask_fit = gmask_fit
    else:
        _, emask_fit, _ = bfu.crop_to_fit(img, cfg.fit_size, equator_mask)

    side = img_fit.shape[0]
    if rmax is None:
        rmax = int(0.9 * side / 2)

    # rmin..rmax annulus in the fit-crop frame. Prefer QF's own
    # _create_rminrmax_mask (passed in) so the metric region matches the mask
    # used for fitting; otherwise synthesize the ring about the crop center
    # (the crop is centered, so the beam center sits at side/2).
    if rminrmax_mask is not None:
        _, ring_fit, _ = bfu.crop_to_fit(img, cfg.fit_size, rminrmax_mask)
    else:
        _cy = _cx = side / 2.0
        _yy, _xx = np.ogrid[:side, :side]
        _rr = np.hypot(_yy - _cy, _xx - _cx)
        ring_fit = (_rr >= rmin) & (_rr <= rmax)

    # Region for ALL oversubtraction metrics (iteration selection, reduction
    # sweeps, and the reported final metric): real positive data inside the
    # rmin..rmax ring, so optimization and reporting share the same region.
    valid = np.isfinite(img_fit) & (img_fit > 0) & ring_fit

    # ---- step 0: rough projection background seeds the first equator fit ----
    report("Step 0: Projection-based Background", 0.0)
    step0_bg = None
    if cfg.use_step0:
        try:
            step0_bg_fit, _, _ = run_step0_projection_bg(
                img_fit, gmask_fit, rmin, rmax, cfg=cfg
            )
            general = step0_bg_fit.copy()
            step0_bg = bfu.upsample_to_full(step0_bg_fit, full_shape)
        except Exception as e:  # noqa: BLE001
            print(f"  [step0] failed ({e}); proceeding without it.")
            general = np.zeros_like(img_fit)
    else:
        general = np.zeros_like(img_fit)

    base = img_fit
    iter_records = []
    # With comp2='auto' the first iteration tries every candidate kernel and
    # picks the best by anti-oversubtraction score; that winner is then pinned
    # for all remaining iterations (rather than re-selecting each round).
    active_comp2 = cfg.comp2
    for it in range(1, cfg.iters + 1):
        report(f"Step {it}. Fit Equator", 0.9 * (it * 2 - 1) / max(cfg.iters * 2, 1))
        # Stage A: equator on img - current general background
        eq_target = base - general
        equator, eq_params, eq_norm, eq_info = fit_equator(
            eq_target, emask_fit, cfg, cfg.quiet
        )
        # Stage B: general background on img - current equator (best comp2)
        report(f"Step {it}. Fit General", 0.9 * (it * 2) / max(cfg.iters * 2, 1))
        gen_target = base - equator
        gen_best, _ = fit_general_best(
            gen_target, gmask_fit, cfg, cfg.quiet, comp2=active_comp2
        )
        general = gen_best["general"]
        # Lock in the winning kernel after the first auto selection.
        active_comp2 = gen_best["comp2"]

        residual = base - equator - general
        gen_params_red = np.asarray(gen_best["params"], dtype=float).copy()
        gen_params_red[0] *= 1.0 - cfg.baseline_reduction
        general_red = bfu.reconstruct_general(
            gen_params_red,
            img_fit.shape,
            gen_best["comp2"],
            cfg.downsample_factor,
            gen_best["norm"],
        )
        residual_red = base - equator - general_red

        neg = bfu.negative_stats(residual, valid)
        neg_red = bfu.negative_stats(residual_red, valid)
        lobe_ok, lobe_info = bfu.equator_lobes_ok(eq_params, cfg.eq_max_v0_dev)
        iter_records.append(
            {
                "round": it,
                "equator": equator,
                "general": general,
                "eq_params": eq_params,
                "gen_params": gen_best["params"],
                "gen_norm": gen_best["norm"],
                "eq_norm": eq_norm,
                "comp2": gen_best["comp2"],
                "gen_cost": gen_best["cost"],
                "gen_score": gen_best["score"],
                "gen_metrics": gen_best["metrics"],
                "eq_cost": eq_info["cost"],
                "eq_metrics": eq_info["metrics"],
                "neg": neg,
                "neg_red": neg_red,
                "lobe_ok": lobe_ok,
                "lobe": lobe_info,
            }
        )

    # ---- pick the best iteration ----
    best, fallback = bfu.select_best_iteration(iter_records)
    ds = cfg.downsample_factor
    comp2 = best["comp2"]
    eq_params = np.asarray(best["eq_params"], dtype=float)
    gen_params = np.asarray(best["gen_params"], dtype=float)
    eq_norm = best["eq_norm"]
    gen_norm = best["gen_norm"]

    # ---- resolve the post-fit safety reductions (fit-size domain, fast) ----
    # The final background is the *reduced* version: the general baseline is cut
    # by cfg.baseline_reduction (always) and the equator by cfg.equator_reduction
    # (default 0). With cfg.auto_reduce each cut is tuned upward on top of its
    # fixed fraction until the oversubtracted-pixel fraction stops improving. The
    # sweep runs on the fit-size crop where `valid` is defined, then the chosen
    # fractions are applied to the full-resolution reconstruction.
    report("Reduce", 0.95)
    with maybe_quiet(cfg.quiet):
        eq_fit = bfu.reconstruct_equator(
            eq_params, img_fit.shape, ds, eq_norm, cfg.equator_keep_baseline
        )

    def _gen_fit(bl_frac):
        gp = gen_params.copy()
        gp[0] *= 1.0 - bl_frac
        with maybe_quiet(cfg.quiet):
            return bfu.reconstruct_general(gp, img_fit.shape, comp2, ds, gen_norm)

    gen_fit_full_baseline = _gen_fit(0.0)

    # equator reduction: fixed fraction, then optional auto sweep on top
    eq_red_fixed = float(cfg.equator_reduction)
    eq_fit_fixed = eq_fit * (1.0 - eq_red_fixed)
    eq_red_auto = 0.0
    if cfg.auto_reduce:

        def eq_frac(r):
            return bfu.negative_stats(
                base - eq_fit_fixed * (1.0 - r) - gen_fit_full_baseline, valid
            )["frac_negative"]

        eq_red_auto, _ = bfu.auto_reduce(
            eq_frac,
            cfg.reduction_max_equator,
            cfg.reduction_step_equator,
            cfg.reduction_min_improve_equator,
        )
    eq_red = 1.0 - (1.0 - eq_red_fixed) * (1.0 - eq_red_auto)
    eq_fit_final = eq_fit * (1.0 - eq_red)

    # baseline reduction: fixed fraction (always), then optional auto sweep on top
    bl_red_fixed = float(cfg.baseline_reduction)
    bl_red_auto = 0.0
    if cfg.auto_reduce:

        def bl_frac(r):
            total = 1.0 - (1.0 - bl_red_fixed) * (1.0 - r)
            return bfu.negative_stats(base - eq_fit_final - _gen_fit(total), valid)[
                "frac_negative"
            ]

        bl_red_auto, _ = bfu.auto_reduce(
            bl_frac,
            cfg.reduction_max_baseline,
            cfg.reduction_step_baseline,
            cfg.reduction_min_improve_baseline,
        )
    bl_red = 1.0 - (1.0 - bl_red_fixed) * (1.0 - bl_red_auto)

    # oversubtraction of the FINAL (reduced) residual, for reporting. Uses the
    # same rmin..rmax `valid` region as the optimization metrics above.
    final_neg = bfu.negative_stats(base - eq_fit_final - _gen_fit(bl_red), valid)

    # ---- reconstruct the chosen (reduced) backgrounds at FULL resolution ----
    with maybe_quiet(cfg.quiet):
        equator_full, general_full, residual_full = reduce_backgrounds(
            img,
            eq_params,
            gen_params,
            eq_norm,
            gen_norm,
            comp2,
            ds,
            eq_red,
            bl_red,
            cfg.equator_keep_baseline,
        )

    report("Done", 1.0)
    return {
        "equator": equator_full,
        "general": general_full,
        "residual": residual_full,
        "equator_params": best["eq_params"],
        "general_params": best["gen_params"],
        "comp2": comp2,
        "best_iter": best["round"],
        "fallback": fallback,
        "rounds": iter_records,
        "step0": step0_bg,
        "full_shape": full_shape,
        # reconstruction inputs, so the reduced backgrounds can be rebuilt
        # (reduce_backgrounds) when the reductions change without re-fitting.
        "eq_norm": eq_norm,
        "gen_norm": gen_norm,
        "downsample_factor": ds,
        "equator_keep_baseline": cfg.equator_keep_baseline,
        "equator_reduction": eq_red,
        "baseline_reduction": bl_red,
        "oversub_frac": final_neg["frac_negative"],
        "oversub_flux_frac": final_neg["oversub_flux_frac"],
        "n_negative": final_neg["n_negative"],
        "n_valid": final_neg["n_valid"],
    }
