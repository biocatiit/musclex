"""Fold-symmetry scoring used by both QuadrantFolder and the image-alignment UI.

This module lives under ``musclex.utils`` (instead of ``musclex.ui.widgets``)
so the core/headless QuadrantFolder pipeline can call it without dragging in
PySide6 — importing anything under ``musclex.ui.widgets`` runs that package's
``__init__.py`` which transitively loads Qt.

All heavy dependencies (cv2/numpy) are imported lazily inside the function so
``import musclex.utils.fold_symmetry`` stays cheap.
"""

import traceback


def _compute_fold_symmetry(img, center, rotation):
    """Compute per-quadrant symmetry scores for *img*.

    Mirrors the transform used by ``QuadrantFolder.transformImage`` and the
    quadrant slicing in ``QuadrantFolder.calculateAvgFold``: the image is
    centred, rotated, then split into top-left / top-right / bottom-left /
    bottom-right quadrants flipped to a common orientation. Quadrants are
    padded with NaN so partial overlaps don't pollute the std calculation.

    Returns a dict with two complementary scores:

    ``fold_std_sum``
        Sum of per-pixel std-deviation across all valid positions (>= 2
        quadrant samples). Lower is more symmetric. Not normalised, so it
        scales with image area and exposure.

    ``fold_std_norm``
        ``fold_std_sum`` divided by the total foreground signal
        ``N_fg x mu_fg`` (professor's proposal). The foreground mask is
        derived from the average-quadrant image using Otsu thresholding,
        making it dimensionless and comparable across different exposures.
        ``None`` when the foreground cannot be determined reliably
        (fewer than 100 pixels above threshold, or zero signal).

    @param img: 2-D numpy array
    @param center: (cx, cy) effective center in original image coordinates
    @param rotation: rotation angle in degrees, applied around the image center
    @returns dict ``{'fold_std_sum': float | None, 'fold_std_norm': float | None}``
    """
    _null = {"fold_std_sum": None, "fold_std_norm": None}
    if img is None or center is None:
        return _null
    try:
        import cv2 as _cv2
        import numpy as _np

        h, w = img.shape[:2]
        cx_orig, cy_orig = float(center[0]), float(center[1])

        # Build the same composite (translate-then-rotate) matrix QuadrantFolder
        # uses in ``transformImage``. Doing both steps with a single warpAffine
        # avoids the NaN-border bleed that two sequential warps would cause
        # (each interpolation pass spreads NaNs by ~1 pixel because any tap
        # touching a NaN produces NaN), keeping the valid region — and thus
        # the fold-std score — aligned with what QF actually folds.
        tx = (w / 2.0) - cx_orig
        ty = (h / 2.0) - cy_orig
        M1 = _np.array(
            [[1.0, 0.0, tx], [0.0, 1.0, ty], [0.0, 0.0, 1.0]], dtype=_np.float64
        )
        if rotation:
            M2 = _cv2.getRotationMatrix2D((w / 2.0, h / 2.0), float(rotation), 1.0)
            M2_3x3 = _np.vstack([M2, [0.0, 0.0, 1.0]])
            M_total = (M2_3x3 @ M1)[:2, :].astype(_np.float32)
        else:
            M_total = M1[:2, :].astype(_np.float32)
        img_t = _cv2.warpAffine(
            img.astype(_np.float32),
            M_total,
            (w, h),
            flags=_cv2.INTER_LINEAR,
            borderValue=_np.nan,
        )

        cx, cy = w // 2, h // 2
        fold_w = max(cx, w - cx)
        fold_h = max(cy, h - cy)
        if fold_w <= 0 or fold_h <= 0:
            return _null

        # Slice 4 quadrants exactly as calculateAvgFold does, flipping each to a
        # common orientation (top-left frame).
        tl = img_t[max(cy - fold_h, 0) : cy, max(cx - fold_w, 0) : cx]
        tr = img_t[max(cy - fold_h, 0) : cy, cx : cx + fold_w]
        bl = img_t[cy : cy + fold_h, max(cx - fold_w, 0) : cx]
        br = img_t[cy : cy + fold_h, cx : cx + fold_w]
        tr = _cv2.flip(tr, 1) if tr.size else tr
        bl = _cv2.flip(bl, 0) if bl.size else bl
        br = _cv2.flip(_cv2.flip(br, 1), 0) if br.size else br

        # Pad each quadrant to (fold_h, fold_w) with NaN so missing samples
        # do not inflate / suppress the std.
        stack = _np.full((4, fold_h, fold_w), _np.nan, dtype=_np.float32)
        for i, q in enumerate((tl, tr, bl, br)):
            if q is None or q.size == 0:
                continue
            qh, qw = q.shape[:2]
            stack[i, -qh:, -qw:] = q

        # Treat raw "invalid pixel" sentinels (from masked pixels in the
        # original image) the same as NaN so they do not enter the std.
        try:
            from musclex.modules.QuadrantFolder import INVALID_PIXEL_THRESHOLD

            stack[stack <= INVALID_PIXEL_THRESHOLD] = _np.nan
        except Exception:
            pass

        valid = ~_np.isnan(stack)
        counts = valid.sum(axis=0)
        keep = counts >= 2
        if not _np.any(keep):
            return {"fold_std_sum": 0.0, "fold_std_norm": None}

        # Silence numpy's "Degrees of freedom <= 0" / "All-NaN slice" warnings
        # for positions that have <2 valid samples; we mask those out via *keep*
        # immediately afterwards anyway.
        import warnings as _warnings

        with _np.errstate(invalid="ignore"), _warnings.catch_warnings():
            _warnings.simplefilter("ignore", RuntimeWarning)
            per_pixel_std = _np.nanstd(stack, axis=0, ddof=0)
        per_pixel_std = _np.where(keep, per_pixel_std, 0.0)
        fold_std_sum = float(_np.nansum(per_pixel_std))

        # --- Normalised score (professor's proposal) ---
        # avg_quad[i,j] is the mean of the 4 quadrant values at position (i,j).
        # Otsu thresholding on the finite+positive values of this 2-D map
        # separates foreground (diffraction signal) from background.
        # The denominator N_fg x mu_fg = Sigma I_fg is the total foreground signal,
        # making the score dimensionless and exposure-independent.
        fold_std_norm = None
        with _np.errstate(invalid="ignore"), _warnings.catch_warnings():
            _warnings.simplefilter("ignore", RuntimeWarning)
            avg_quad = _np.nanmean(stack, axis=0)  # shape (fold_h, fold_w)

        finite_pos = avg_quad[_np.isfinite(avg_quad) & (avg_quad > 0)]
        if finite_pos.size >= 100:
            # cv2.threshold requires uint8/float32; scale to [0, 255] for Otsu
            aq_min, aq_max = float(finite_pos.min()), float(finite_pos.max())
            if aq_max > aq_min:
                # cv2 Otsu requires uint8; scale [0, 255] then cast.
                scaled_f = (avg_quad - aq_min) / (aq_max - aq_min) * 255.0
                scaled_u8 = (
                    _np.nan_to_num(scaled_f, nan=0.0).clip(0, 255).astype(_np.uint8)
                )
                thresh_scaled, _ = _cv2.threshold(
                    scaled_u8, 0, 255, _cv2.THRESH_BINARY + _cv2.THRESH_OTSU
                )
                # Convert threshold back to original intensity units
                threshold = aq_min + thresh_scaled / 255.0 * (aq_max - aq_min)
                fg_mask = (
                    (avg_quad > threshold) & _np.isfinite(avg_quad) & (avg_quad > 0)
                )
                n_fg = int(fg_mask.sum())
                if n_fg >= 100:
                    total_fg_signal = float(avg_quad[fg_mask].sum())
                    if total_fg_signal > 0:
                        fold_std_norm = fold_std_sum / total_fg_signal

        return {"fold_std_sum": fold_std_sum, "fold_std_norm": fold_std_norm}
    except Exception:
        traceback.print_exc()
        return _null
