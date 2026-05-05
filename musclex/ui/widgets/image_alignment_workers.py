"""Subprocess worker functions used by :class:`ImageAlignmentWidget`.

These functions live at module top-level (no class scope, no closures) so that
``concurrent.futures.ProcessPoolExecutor`` can pickle them across the
process boundary. They are deliberately self-contained: every dependency is
imported lazily inside the function body so the worker process only loads
heavy libraries (cv2, numpy, fabio, pyFAI) when actually invoked.

Scope: image-alignment batch detection only. Workers serving other features
(AddIntensitiesSingleExp / AddIntensitiesMultipleExp grouping) live in
``musclex.ui.add_intensities_common``.
"""

import traceback


def _compute_image_diff(args):
    """Subprocess worker: aligned mean-absolute-difference between two images.

    Both images are warped onto the *base* (center, rotation) before differencing
    so the score reflects residual mismatch after geometric alignment, rather
    than raw pixel offset. Difference is averaged inside a 100-pixel radius
    around the base center to focus on the diffraction region.

    @param args: ``(dir_path, img_name_a, img_name_b, spec_a, spec_b,
                   center_a, rotation_a, center_b, rotation_b,
                   base_center, base_rotation, pair_index)``
    @returns dict ``{'pair_index', 'diff', 'error'}``
    """
    (dir_path, img_name_a, img_name_b,
     spec_a, spec_b,
     center_a, rotation_a,
     center_b, rotation_b,
     base_center, base_rotation,
     pair_index) = args
    try:
        import cv2 as _cv2
        import numpy as _np
        from musclex.utils.file_manager import load_image_via_spec

        def _transform_img(img, center, rotation, b_center, b_rotation):
            h, w = img.shape[:2]
            if center is not None and b_center is not None:
                tx = b_center[0] - center[0]
                ty = b_center[1] - center[1]
                if tx != 0 or ty != 0:
                    M = _np.float32([[1, 0, tx], [0, 1, ty]])
                    img = _cv2.warpAffine(img, M, (w, h))
            deviation = (rotation or 0) - (b_rotation or 0)
            if deviation != 0 and b_center is not None:
                M2 = _cv2.getRotationMatrix2D(tuple(b_center), deviation, 1)
                img = _cv2.warpAffine(img, M2, (w, h))
            return img.astype(_np.float32)

        img_a = load_image_via_spec(dir_path, img_name_a, spec_a).astype(_np.float32)
        img_b = load_image_via_spec(dir_path, img_name_b, spec_b).astype(_np.float32)
        ta = _transform_img(img_a, center_a, rotation_a, base_center, base_rotation)
        tb = _transform_img(img_b, center_b, rotation_b, base_center, base_rotation)
        h, w = ta.shape[:2]
        if base_center is not None:
            cy, cx = base_center[1], base_center[0]
        else:
            cy, cx = h / 2.0, w / 2.0
        ys, xs = _np.ogrid[:h, :w]
        mask = (xs - cx) ** 2 + (ys - cy) ** 2 <= 100 ** 2
        absdiff = _np.abs(ta - tb)
        diff = float(_np.mean(absdiff[mask]) if mask.any() else _np.mean(absdiff))
        return {'pair_index': pair_index, 'diff': diff, 'error': None}
    except Exception as e:
        traceback.print_exc()
        return {'pair_index': pair_index, 'diff': None, 'error': str(e)}


def _compute_geometry(args):
    """Subprocess worker: load image and run auto center/rotation detection.

    ``manual_center`` / ``manual_rotation`` are intentionally not forwarded to
    :class:`ImageData` so detection always runs on the raw image. The caller
    stores the result in the auto-geometry cache; manual values are applied
    separately when computing the *effective* (final) center/rotation.

    @param args: ``(dir_path, img_name, loader_spec, manual_center,
                   manual_rotation, orientation_model)``
    @returns dict ``{'img_name', 'center', 'rotation', 'error'}``
    """
    dir_path, img_name, loader_spec, manual_center, manual_rotation, orientation_model = args
    try:
        from musclex.utils.file_manager import load_image_via_spec
        from musclex.utils.image_data import ImageData

        img = load_image_via_spec(dir_path, img_name, loader_spec)
        image_data = ImageData(
            img=img, img_path=dir_path, img_name=img_name,
            center=None, rotation=None,
            orientation_model=orientation_model,
        )
        return {
            'img_name': img_name,
            'center': image_data.center,
            'rotation': image_data.rotation,
            'error': None,
        }
    except Exception as e:
        traceback.print_exc()
        return {'img_name': img_name, 'center': None, 'rotation': None, 'error': str(e)}


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
        ``N_fg × μ_fg`` (professor's proposal). The foreground mask is
        derived from the average-quadrant image using Otsu thresholding,
        making it dimensionless and comparable across different exposures.
        ``None`` when the foreground cannot be determined reliably
        (fewer than 100 pixels above threshold, or zero signal).

    @param img: 2-D numpy array
    @param center: (cx, cy) effective center in original image coordinates
    @param rotation: rotation angle in degrees, applied around the image center
    @returns dict ``{'fold_std_sum': float | None, 'fold_std_norm': float | None}``
    """
    _null = {'fold_std_sum': None, 'fold_std_norm': None}
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
        M1 = _np.array([[1.0, 0.0, tx],
                        [0.0, 1.0, ty],
                        [0.0, 0.0, 1.0]], dtype=_np.float64)
        if rotation:
            M2 = _cv2.getRotationMatrix2D((w / 2.0, h / 2.0), float(rotation), 1.0)
            M2_3x3 = _np.vstack([M2, [0.0, 0.0, 1.0]])
            M_total = (M2_3x3 @ M1)[:2, :].astype(_np.float32)
        else:
            M_total = M1[:2, :].astype(_np.float32)
        img_t = _cv2.warpAffine(
            img.astype(_np.float32), M_total, (w, h),
            flags=_cv2.INTER_LINEAR, borderValue=_np.nan,
        )

        cx, cy = w // 2, h // 2
        fold_w = max(cx, w - cx)
        fold_h = max(cy, h - cy)
        if fold_w <= 0 or fold_h <= 0:
            return _null

        # Slice 4 quadrants exactly as calculateAvgFold does, flipping each to a
        # common orientation (top-left frame).
        tl = img_t[max(cy - fold_h, 0):cy, max(cx - fold_w, 0):cx]
        tr = img_t[max(cy - fold_h, 0):cy, cx:cx + fold_w]
        bl = img_t[cy:cy + fold_h, max(cx - fold_w, 0):cx]
        br = img_t[cy:cy + fold_h, cx:cx + fold_w]
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
            return {'fold_std_sum': 0.0, 'fold_std_norm': None}

        # Silence numpy's "Degrees of freedom <= 0" / "All-NaN slice" warnings
        # for positions that have <2 valid samples; we mask those out via *keep*
        # immediately afterwards anyway.
        import warnings as _warnings
        with _np.errstate(invalid='ignore'), _warnings.catch_warnings():
            _warnings.simplefilter('ignore', RuntimeWarning)
            per_pixel_std = _np.nanstd(stack, axis=0, ddof=0)
        per_pixel_std = _np.where(keep, per_pixel_std, 0.0)
        fold_std_sum = float(_np.nansum(per_pixel_std))

        # --- Normalised score (professor's proposal) ---
        # avg_quad[i,j] is the mean of the 4 quadrant values at position (i,j).
        # Otsu thresholding on the finite+positive values of this 2-D map
        # separates foreground (diffraction signal) from background.
        # The denominator N_fg × μ_fg = Σ I_fg is the total foreground signal,
        # making the score dimensionless and exposure-independent.
        fold_std_norm = None
        with _np.errstate(invalid='ignore'), _warnings.catch_warnings():
            _warnings.simplefilter('ignore', RuntimeWarning)
            avg_quad = _np.nanmean(stack, axis=0)  # shape (fold_h, fold_w)

        finite_pos = avg_quad[_np.isfinite(avg_quad) & (avg_quad > 0)]
        if finite_pos.size >= 100:
            # cv2.threshold requires uint8/float32; scale to [0, 255] for Otsu
            aq_min, aq_max = float(finite_pos.min()), float(finite_pos.max())
            if aq_max > aq_min:
                # cv2 Otsu requires uint8; scale [0, 255] then cast.
                scaled_f = (avg_quad - aq_min) / (aq_max - aq_min) * 255.0
                scaled_u8 = _np.nan_to_num(scaled_f, nan=0.0).clip(0, 255).astype(_np.uint8)
                thresh_scaled, _ = _cv2.threshold(
                    scaled_u8, 0, 255, _cv2.THRESH_BINARY + _cv2.THRESH_OTSU)
                # Convert threshold back to original intensity units
                threshold = aq_min + thresh_scaled / 255.0 * (aq_max - aq_min)
                fg_mask = (avg_quad > threshold) & _np.isfinite(avg_quad) & (avg_quad > 0)
                n_fg = int(fg_mask.sum())
                if n_fg >= 100:
                    total_fg_signal = float(avg_quad[fg_mask].sum())
                    if total_fg_signal > 0:
                        fold_std_norm = fold_std_sum / total_fg_signal

        return {'fold_std_sum': fold_std_sum, 'fold_std_norm': fold_std_norm}
    except Exception:
        traceback.print_exc()
        return _null


def _compute_geometry_with_symmetry(args):
    """Subprocess worker: compute auto geometry and (optionally) fold symmetry.

    Behaves like :func:`_compute_geometry` and additionally returns
    ``fold_std_sum`` when ``do_symmetry`` is True. The symmetry calculation is
    done with the *effective* center/rotation (manual settings preferred,
    falling back to the freshly-detected auto values), so the score reflects
    what folding will actually see.

    @param args: ``(dir_path, img_name, loader_spec, manual_center,
                   manual_rotation, orientation_model, do_symmetry)``
    @returns dict ``{'img_name', 'center', 'rotation', 'fold_std_sum',
                     'fold_std_norm', 'error'}``
    """
    (dir_path, img_name, loader_spec,
     manual_center, manual_rotation,
     orientation_model, do_symmetry) = args
    try:
        from musclex.utils.file_manager import load_image_via_spec
        from musclex.utils.image_data import ImageData

        img = load_image_via_spec(dir_path, img_name, loader_spec)
        image_data = ImageData(
            img=img, img_path=dir_path, img_name=img_name,
            center=None, rotation=None,
            orientation_model=orientation_model,
        )
        auto_center = image_data.center
        auto_rotation = image_data.rotation

        fold_std_sum = None
        fold_std_norm = None
        if do_symmetry:
            eff_center = manual_center if manual_center is not None else auto_center
            eff_rotation = (manual_rotation
                            if manual_rotation is not None else auto_rotation)
            sym = _compute_fold_symmetry(img, eff_center, eff_rotation)
            fold_std_sum = sym['fold_std_sum']
            fold_std_norm = sym['fold_std_norm']

        return {
            'img_name': img_name,
            'center': auto_center,
            'rotation': auto_rotation,
            'fold_std_sum': fold_std_sum,
            'fold_std_norm': fold_std_norm,
            'error': None,
        }
    except Exception as e:
        traceback.print_exc()
        return {
            'img_name': img_name,
            'center': None,
            'rotation': None,
            'fold_std_sum': None,
            'fold_std_norm': None,
            'error': str(e),
        }
