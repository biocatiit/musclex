#!/usr/bin/env python3
"""Serial coarse-to-fine symmetry search for MuscleX calibration refinement."""

from __future__ import annotations

import math
import time
import cv2
import numpy as np

try:
    from musclex.utils.fold_symmetry import _compute_fold_symmetry
except Exception:
    from utils import _compute_fold_symmetry

# ------------------------------------------------------------
# Sampling utilities
# ------------------------------------------------------------


def bilinear_sample(image: np.ndarray, x: np.ndarray, y: np.ndarray, fill_value=0.0):
    h, w = image.shape

    x0 = np.floor(x).astype(np.int64)
    y0 = np.floor(y).astype(np.int64)
    x1 = x0 + 1
    y1 = y0 + 1

    valid = (x0 >= 0) & (x1 < w) & (y0 >= 0) & (y1 < h)

    x0c = np.clip(x0, 0, w - 1)
    x1c = np.clip(x1, 0, w - 1)
    y0c = np.clip(y0, 0, h - 1)
    y1c = np.clip(y1, 0, h - 1)

    Ia = image[y0c, x0c]
    Ib = image[y0c, x1c]
    Ic = image[y1c, x0c]
    Id = image[y1c, x1c]

    wa = (x1 - x) * (y1 - y)
    wb = (x - x0) * (y1 - y)
    wc = (x1 - x) * (y - y0)
    wd = (x - x0) * (y - y0)

    out = wa * Ia + wb * Ib + wc * Ic + wd * Id
    out = np.where(valid, out, fill_value)

    return out, valid


def nearest_sample(mask: np.ndarray, x: np.ndarray, y: np.ndarray, fill_value=1):
    h, w = mask.shape

    xi = np.rint(x).astype(np.int64)
    yi = np.rint(y).astype(np.int64)

    valid = (xi >= 0) & (xi < w) & (yi >= 0) & (yi < h)

    xic = np.clip(xi, 0, w - 1)
    yic = np.clip(yi, 0, h - 1)

    out = mask[yic, xic]
    out = np.where(valid, out, fill_value)

    return out.astype(np.uint8), valid


# ------------------------------------------------------------
# Geometry and folding
# ------------------------------------------------------------


def make_canonical_grid(width: int, height: int, radius_x=None, radius_y=None):
    if radius_x is None:
        radius_x = width // 2

    if radius_y is None:
        radius_y = height // 2

    ux = np.arange(radius_x, dtype=np.float64)
    uy = np.arange(radius_y, dtype=np.float64)

    UY, UX = np.meshgrid(uy, ux, indexing="ij")
    return UX, UY


def quadrant_coordinates(UX, UY, center_x, center_y, angle_deg, sx, sy):
    theta = math.radians(angle_deg)
    cos_t = math.cos(theta)
    sin_t = math.sin(theta)

    x_rel = sx * UX
    y_rel = sy * UY

    x = center_x + cos_t * x_rel - sin_t * y_rel
    y = center_y + sin_t * x_rel + cos_t * y_rel

    return x, y


def extract_quadrants(image, mask, center_x, center_y, angle_deg, UX, UY):
    signs = [
        (+1, +1),
        (+1, -1),
        (-1, +1),
        (-1, -1),
    ]

    vals = []
    weights = []

    for sx, sy in signs:
        x, y = quadrant_coordinates(
            UX,
            UY,
            center_x=center_x,
            center_y=center_y,
            angle_deg=angle_deg,
            sx=sx,
            sy=sy,
        )

        v, in_bounds_img = bilinear_sample(image, x, y, fill_value=0.0)
        m, in_bounds_mask = nearest_sample(mask, x, y, fill_value=1)

        valid = in_bounds_img & in_bounds_mask & (m == 0)

        vals.append(v)
        weights.append(valid.astype(np.float64))

    return np.stack(vals, axis=0), np.stack(weights, axis=0)


def _image_with_mask_sentinel(image: np.ndarray, mask: np.ndarray) -> np.ndarray:
    """Return *image* with masked-out pixels set to a sentinel ``_compute_fold_symmetry``
    treats as invalid (``<= INVALID_PIXEL_THRESHOLD = -1``). When the mask has no
    invalid pixels, the original array is returned without copying."""
    if mask is None or not mask.any():
        return image
    out = image.astype(np.float64, copy=True)
    out[mask != 0] = -1.0
    return out


def _local_fold_std_norm(folded, var, count, min_valid):
    """Region-restricted analogue of ``_compute_fold_symmetry``'s ``fold_std_norm``.

    Operates directly on the already-extracted quadrant stack (the
    ``radius_x x radius_y`` folding grid) instead of warping/folding the whole
    image, so the symmetry score is measured over exactly the region the
    search samples. Returns ``None`` when the foreground cannot be determined
    reliably (mirrors the whole-image version).

    ``folded`` is the per-pixel mean across valid quadrants (the "average
    quadrant"), ``var`` the per-pixel population variance across valid
    quadrants (ddof=0), and ``count`` the number of valid quadrant samples per
    pixel.
    """
    keep = count >= max(2, min_valid)
    if not np.any(keep):
        return None

    per_pixel_std = np.sqrt(np.maximum(var, 0.0))
    fold_std_sum = float(np.sum(per_pixel_std[keep]))

    # Foreground = diffraction signal in the averaged quadrant, separated from
    # background with Otsu. Denominator = total foreground signal, making the
    # score dimensionless / exposure-independent (same as the whole-image one).
    finite_pos = folded[keep & (folded > 0)]
    if finite_pos.size < 100:
        return None

    aq_min = float(finite_pos.min())
    aq_max = float(finite_pos.max())
    if aq_max <= aq_min:
        return None

    scaled = (folded - aq_min) / (aq_max - aq_min) * 255.0
    scaled_u8 = np.nan_to_num(scaled, nan=0.0).clip(0, 255).astype(np.uint8)
    thresh_scaled, _ = cv2.threshold(
        scaled_u8, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU
    )
    threshold = aq_min + thresh_scaled / 255.0 * (aq_max - aq_min)

    fg = (folded > threshold) & keep & (folded > 0)
    if int(np.sum(fg)) < 100:
        return None

    total_fg_signal = float(np.sum(folded[fg]))
    if total_fg_signal <= 0:
        return None

    return fold_std_sum / total_fg_signal


def _whole_image_fold_std_norm(image, mask, center_x, center_y, angle_deg):
    """Whole-image ``fold_std_norm`` score, ignoring ``radius_x``/``radius_y``.

    Used only for the *reported* before/after loss (see ``main``): folding the
    entire frame keeps those numbers comparable across runs with different
    crop sizes and with the other refinement scripts, even though the search
    itself is driven by the radius-restricted score from
    ``_local_fold_std_norm``.
    """
    scores = _compute_fold_symmetry(
        _image_with_mask_sentinel(image, mask),
        (center_x, center_y),
        angle_deg,
    )
    norm = scores.get("fold_std_norm")
    return float(norm) if norm is not None else float("inf")


def fold_and_loss(
    image,
    mask,
    center_x,
    center_y,
    angle_deg,
    UX,
    UY,
    min_valid=2,
    *,
    loss_mode: str = "var_mean",
):
    vals, weights = extract_quadrants(
        image=image,
        mask=mask,
        center_x=center_x,
        center_y=center_y,
        angle_deg=angle_deg,
        UX=UX,
        UY=UY,
    )

    count = np.sum(weights, axis=0)
    weighted_sum = np.sum(weights * vals, axis=0)

    folded = np.zeros_like(weighted_sum)
    valid_any = count > 0
    folded[valid_any] = weighted_sum[valid_any] / count[valid_any]

    diff2 = weights * (vals - folded[None, :, :]) ** 2

    var = np.zeros_like(folded)
    var[valid_any] = np.sum(diff2, axis=0)[valid_any] / count[valid_any]

    valid_loss = count >= min_valid

    if not np.any(valid_loss):
        residual = np.sqrt(np.maximum(var, 0.0))
        return folded, count, residual, float("inf")

    if loss_mode == "var_mean":
        loss = float(np.mean(var[valid_loss]))

    elif loss_mode == "fold_std_norm":
        # Drive the search from the radius_x x radius_y folding region — the
        # same quadrants the search samples — rather than warping/folding the
        # whole image; see `_whole_image_fold_std_norm` for the whole-image
        # score used for the final reported loss in `main`.
        norm = _local_fold_std_norm(folded, var, count, min_valid)
        loss = float(norm) if norm is not None else float("inf")

    else:
        raise ValueError(f"Unknown loss_mode: {loss_mode!r}")

    residual = np.sqrt(np.maximum(var, 0.0))

    return folded, count, residual, loss


def transform_pattern_to_canonical(
    image: np.ndarray,
    mask: np.ndarray,
    center_x: float,
    center_y: float,
    angle_deg: float,
    fill_value: float = 0.0,
) -> np.ndarray:
    """Warp *image* so the symmetry center sits at the image center with zero rotation.

    For each output pixel, inverse-map through rotation then translation (same
    convention as Musclex ``QuadrantFolder.transformImage`` / ``fold_symmetry``).
    """
    h, w = image.shape
    cx_out = w / 2.0
    cy_out = h / 2.0

    y_out, x_out = np.mgrid[0:h, 0:w]
    xo = x_out.astype(np.float64) - cx_out
    yo = y_out.astype(np.float64) - cy_out

    theta = math.radians(-angle_deg)
    cos_t = math.cos(theta)
    sin_t = math.sin(theta)

    x_rel = cos_t * xo - sin_t * yo
    y_rel = sin_t * xo + cos_t * yo

    x_in = center_x + x_rel
    y_in = center_y + y_rel

    out, valid = bilinear_sample(image, x_in, y_in, fill_value=fill_value)
    m, _ = nearest_sample(mask, x_in, y_in, fill_value=1)
    out = np.where(valid & (m == 0), out, fill_value)
    return out


# ------------------------------------------------------------
# Grid-search utilities
# ------------------------------------------------------------


def parse_levels(levels_str):
    levels = []

    for part in levels_str.split(","):
        vals = part.strip().split(":")
        if len(vals) != 4:
            raise ValueError(
                "Each level must have format center_range:center_step:angle_range:angle_step"
            )

        cr, cs, ar, astep = map(float, vals)

        if cr < 0 or ar < 0 or cs <= 0 or astep <= 0:
            raise ValueError("Ranges must be nonnegative and steps must be positive.")

        levels.append(
            {
                "center_range": cr,
                "center_step": cs,
                "angle_range": ar,
                "angle_step": astep,
            }
        )

    return levels


def parse_angle_only_levels(levels_str):
    """Parse angle-only levels format: 'angle_range:angle_step,...'

    Returns the same dict structure as ``parse_levels`` but with
    ``center_range=0`` so the center is never moved during the search.
    """
    levels = []
    for part in levels_str.split(","):
        vals = part.strip().split(":")
        if len(vals) != 2:
            raise ValueError(
                "Each angle-only level must have format angle_range:angle_step"
            )
        ar, astep = map(float, vals)
        if ar < 0 or astep <= 0:
            raise ValueError("Range must be nonnegative and step must be positive.")
        levels.append(
            {
                "center_range": 0.0,
                "center_step": 1.0,
                "angle_range": ar,
                "angle_step": astep,
            }
        )
    return levels


def make_offsets(search_range, step):
    n = int(round((2.0 * search_range) / step))
    vals = -search_range + step * np.arange(n + 1, dtype=np.float64)
    vals[np.argmin(np.abs(vals))] = 0.0
    return vals


ALL_EVAL_FIELDNAMES = [
    "global_eval_index",
    "level",
    "level_eval_index",
    "level_start_center_x",
    "level_start_center_y",
    "level_start_angle_deg",
    "dx",
    "dy",
    "dtheta_deg",
    "candidate_center_x",
    "candidate_center_y",
    "candidate_angle_deg",
    "candidate_loss",
    "is_new_best",
    "best_so_far_center_x",
    "best_so_far_center_y",
    "best_so_far_angle_deg",
    "best_so_far_loss",
]


def coarse_to_fine_search(
    image,
    mask,
    init_center,
    init_angle_deg,
    UX,
    UY,
    levels,
    min_valid=2,
    loss_mode: str = "var_mean",
    verbose=True,
):
    best_center_x, best_center_y = init_center
    best_angle = init_angle_deg

    history = []
    all_eval_rows = []

    global_eval_index = 0
    total_evals = 0

    t0 = time.perf_counter()

    _, _, _, best_loss = fold_and_loss(
        image,
        mask,
        best_center_x,
        best_center_y,
        best_angle,
        UX,
        UY,
        min_valid=min_valid,
        loss_mode=loss_mode,
    )

    all_eval_rows.append(
        {
            "global_eval_index": global_eval_index,
            "level": 0,
            "level_eval_index": 0,
            "level_start_center_x": best_center_x,
            "level_start_center_y": best_center_y,
            "level_start_angle_deg": best_angle,
            "dx": 0.0,
            "dy": 0.0,
            "dtheta_deg": 0.0,
            "candidate_center_x": best_center_x,
            "candidate_center_y": best_center_y,
            "candidate_angle_deg": best_angle,
            "candidate_loss": best_loss,
            "is_new_best": 1,
            "best_so_far_center_x": best_center_x,
            "best_so_far_center_y": best_center_y,
            "best_so_far_angle_deg": best_angle,
            "best_so_far_loss": best_loss,
        }
    )

    if verbose:
        print(f"Initial loss: {best_loss:.8g}")

    for level_index, level in enumerate(levels, start=1):
        cr = level["center_range"]
        cs = level["center_step"]
        ar = level["angle_range"]
        astep = level["angle_step"]

        dxs = make_offsets(cr, cs)
        dys = make_offsets(cr, cs)
        dts = make_offsets(ar, astep)

        level_start_center_x = best_center_x
        level_start_center_y = best_center_y
        level_start_angle = best_angle

        level_best = {
            "loss": best_loss,
            "center_x": best_center_x,
            "center_y": best_center_y,
            "angle_deg": best_angle,
            "dx_from_level_start": 0.0,
            "dy_from_level_start": 0.0,
            "dtheta_from_level_start": 0.0,
        }

        level_t0 = time.perf_counter()
        level_eval_index = 0

        for dtheta in dts:
            cand_angle = level_start_angle + dtheta

            for dy in dys:
                cand_y = level_start_center_y + dy

                for dx in dxs:
                    cand_x = level_start_center_x + dx

                    _, _, _, loss = fold_and_loss(
                        image,
                        mask,
                        cand_x,
                        cand_y,
                        cand_angle,
                        UX,
                        UY,
                        min_valid=min_valid,
                        loss_mode=loss_mode,
                    )

                    global_eval_index += 1
                    level_eval_index += 1
                    total_evals += 1

                    is_new_best = loss < best_loss

                    if is_new_best:
                        best_center_x = float(cand_x)
                        best_center_y = float(cand_y)
                        best_angle = float(cand_angle)
                        best_loss = float(loss)

                    all_eval_rows.append(
                        {
                            "global_eval_index": global_eval_index,
                            "level": level_index,
                            "level_eval_index": level_eval_index,
                            "level_start_center_x": level_start_center_x,
                            "level_start_center_y": level_start_center_y,
                            "level_start_angle_deg": level_start_angle,
                            "dx": float(dx),
                            "dy": float(dy),
                            "dtheta_deg": float(dtheta),
                            "candidate_center_x": float(cand_x),
                            "candidate_center_y": float(cand_y),
                            "candidate_angle_deg": float(cand_angle),
                            "candidate_loss": float(loss),
                            "is_new_best": int(is_new_best),
                            "best_so_far_center_x": best_center_x,
                            "best_so_far_center_y": best_center_y,
                            "best_so_far_angle_deg": best_angle,
                            "best_so_far_loss": best_loss,
                        }
                    )

                    if loss < level_best["loss"]:
                        level_best = {
                            "loss": float(loss),
                            "center_x": float(cand_x),
                            "center_y": float(cand_y),
                            "angle_deg": float(cand_angle),
                            "dx_from_level_start": float(dx),
                            "dy_from_level_start": float(dy),
                            "dtheta_from_level_start": float(dtheta),
                        }

        level_time = time.perf_counter() - level_t0

        history.append(
            {
                "level": level_index,
                "center_range_pixels": cr,
                "center_step_pixels": cs,
                "angle_range_degrees": ar,
                "angle_step_degrees": astep,
                "num_dx": len(dxs),
                "num_dy": len(dys),
                "num_dtheta": len(dts),
                "num_evaluations": level_eval_index,
                "runtime_seconds": level_time,
                "best": {
                    "loss": best_loss,
                    "center_x": best_center_x,
                    "center_y": best_center_y,
                    "angle_deg": best_angle,
                },
                "level_best": level_best,
            }
        )

        if verbose:
            print(
                f"Level {level_index}: "
                f"evals={level_eval_index}, "
                f"time={level_time:.2f}s, "
                f"best_loss={best_loss:.8g}, "
                f"center=({best_center_x:.4f}, {best_center_y:.4f}), "
                f"angle={best_angle:.4f}"
            )

    total_time = time.perf_counter() - t0

    return {
        "center_x": float(best_center_x),
        "center_y": float(best_center_y),
        "angle_deg": float(best_angle),
        "loss": float(best_loss),
        "history": history,
        "all_evaluations": all_eval_rows,
        "total_evaluations": total_evals,
        "total_runtime_seconds": total_time,
    }
