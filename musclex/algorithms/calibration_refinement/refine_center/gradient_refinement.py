#!/usr/bin/env python3
"""Serial gradient-based symmetry refinement for MuscleX calibration refinement."""

from __future__ import annotations

import math
import time
import cv2
import numpy as np
from scipy.optimize import minimize

try:
    from musclex.utils.fold_symmetry import _compute_fold_symmetry
except Exception:
    from utils import _compute_fold_symmetry

# Sampling
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

QUADRANTS = [
    ("++", +1, +1),
    ("+-", +1, -1),
    ("-+", -1, +1),
    ("--", -1, -1),
]


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
    gradient optimization samples. Returns ``None`` when the foreground cannot
    be determined reliably (mirrors the whole-image version).

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
    crop sizes and with the other refinement scripts, even though the
    optimization itself is driven by the radius-restricted score from
    ``_local_fold_std_norm``.
    """
    scores = _compute_fold_symmetry(
        _image_with_mask_sentinel(image, mask),
        (center_x, center_y),
        angle_deg,
    )
    norm = scores.get("fold_std_norm")
    return float(norm) if norm is not None else float("inf")


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
    values = []
    weights = []

    for _, sx, sy in QUADRANTS:
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

        values.append(v)
        weights.append(valid.astype(np.float64))

    return np.stack(values, axis=0), np.stack(weights, axis=0)


def fold_and_loss(
    image,
    mask,
    center_x,
    center_y,
    angle_deg,
    UX,
    UY,
    min_valid=2,
    robust=None,
    huber_delta=0.02,
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
        return folded, count, np.sqrt(np.maximum(var, 0.0)), float("inf")

    if loss_mode == "fold_std_norm":
        # Drive the optimization from the radius_x x radius_y folding region —
        # the same quadrants sampled above — rather than warping/folding the
        # whole image; see `_whole_image_fold_std_norm` for the whole-image
        # score used for the final reported loss in `main`.
        norm = _local_fold_std_norm(folded, var, count, min_valid)
        loss = float(norm) if norm is not None else float("inf")

    elif robust is None:
        loss = float(np.mean(var[valid_loss]))

    elif robust == "huber":
        residual = np.sqrt(np.maximum(var[valid_loss], 0.0))
        d = float(huber_delta)
        abs_r = np.abs(residual)
        penalty = np.where(
            abs_r <= d,
            0.5 * residual * residual,
            d * (abs_r - 0.5 * d),
        )
        loss = float(np.mean(penalty))

    else:
        raise ValueError(f"Unknown robust option: {robust}")

    residual_img = np.sqrt(np.maximum(var, 0.0))
    return folded, count, residual_img, loss


def transform_pattern_to_canonical(
    image: np.ndarray,
    mask: np.ndarray,
    center_x: float,
    center_y: float,
    angle_deg: float,
    fill_value: float = 0.0,
) -> np.ndarray:
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
# Initialization
# ------------------------------------------------------------


def read_initial_estimate_from_report(path: str):
    with open(path, "r", encoding="utf-8") as f:
        report = json.load(f)

    if "refined_estimate" not in report:
        raise ValueError("init report does not contain refined_estimate")

    refined = report["refined_estimate"]
    center = refined.get("center_xy", None)
    angle = refined.get("angle_degrees", None)

    if center is None or angle is None:
        raise ValueError("refined_estimate must contain center_xy and angle_degrees")

    return [float(center[0]), float(center[1])], float(angle)


# ------------------------------------------------------------
# Optimization
# ------------------------------------------------------------

EVAL_FIELDNAMES = [
    "eval_index",
    "candidate_center_x",
    "candidate_center_y",
    "candidate_angle_deg",
    "candidate_loss",
    "candidate_objective",
    "is_new_best",
    "best_so_far_center_x",
    "best_so_far_center_y",
    "best_so_far_angle_deg",
    "best_so_far_loss",
    "best_so_far_objective",
]


def optimize_symmetry_gradient(
    image,
    mask,
    init_center,
    init_angle_deg,
    UX,
    UY,
    max_center_step=2.0,
    max_angle_step=1.0,
    center_scale=1.0,
    angle_scale=1.0,
    min_valid=2,
    robust=None,
    huber_delta=0.02,
    loss_mode: str = "var_mean",
    reg_center=0.0,
    reg_angle=0.0,
    maxiter=100,
    maxfun=500,
    finite_diff_rel_step=None,
    verbose=True,
):
    init_cx, init_cy = init_center

    z_bounds = [
        (-max_center_step / center_scale, +max_center_step / center_scale),
        (-max_center_step / center_scale, +max_center_step / center_scale),
        (-max_angle_step / angle_scale, +max_angle_step / angle_scale),
    ]

    rows = []
    eval_counter = {"n": 0}

    best = {
        "z": np.array([0.0, 0.0, 0.0], dtype=np.float64),
        "loss": float("inf"),
        "objective": float("inf"),
    }

    def z_to_params(z):
        cx = init_cx + center_scale * float(z[0])
        cy = init_cy + center_scale * float(z[1])
        angle = init_angle_deg + angle_scale * float(z[2])
        return cx, cy, angle

    def compute_objective(z):
        cx, cy, angle = z_to_params(z)

        _, _, _, loss = fold_and_loss(
            image=image,
            mask=mask,
            center_x=cx,
            center_y=cy,
            angle_deg=angle,
            UX=UX,
            UY=UY,
            min_valid=min_valid,
            robust=robust,
            huber_delta=huber_delta,
            loss_mode=loss_mode,
        )

        dcx = cx - init_cx
        dcy = cy - init_cy
        dtheta = angle - init_angle_deg

        objective = loss
        objective += reg_center * (dcx * dcx + dcy * dcy)
        objective += reg_angle * (dtheta * dtheta)

        return float(objective), float(loss)

    def objective_wrapper(z):
        objective, loss = compute_objective(z)

        eval_counter["n"] += 1

        cx, cy, angle = z_to_params(z)

        is_new_best = objective < best["objective"]

        if is_new_best:
            best["z"] = np.array(z, dtype=np.float64)
            best["loss"] = loss
            best["objective"] = objective

        bx, by, bangle = z_to_params(best["z"])

        rows.append(
            {
                "eval_index": eval_counter["n"],
                "candidate_center_x": cx,
                "candidate_center_y": cy,
                "candidate_angle_deg": angle,
                "candidate_loss": loss,
                "candidate_objective": objective,
                "is_new_best": int(is_new_best),
                "best_so_far_center_x": bx,
                "best_so_far_center_y": by,
                "best_so_far_angle_deg": bangle,
                "best_so_far_loss": best["loss"],
                "best_so_far_objective": best["objective"],
            }
        )

        return objective

    t0 = time.perf_counter()

    minimize_kwargs = {
        "fun": objective_wrapper,
        "x0": np.array([0.0, 0.0, 0.0], dtype=np.float64),
        "method": "L-BFGS-B",
        "bounds": z_bounds,
        "jac": "2-point",
        "options": {
            "maxiter": int(maxiter),
            "maxfun": int(maxfun),
            "ftol": 1e-12,
            "gtol": 1e-8,
            "disp": False,
        },
    }

    if finite_diff_rel_step is not None:
        minimize_kwargs["options"]["finite_diff_rel_step"] = finite_diff_rel_step

    result = minimize(**minimize_kwargs)

    runtime = time.perf_counter() - t0

    final_z = best["z"]
    final_cx, final_cy, final_angle = z_to_params(final_z)
    final_objective, final_loss = compute_objective(final_z)

    if verbose:
        print("Gradient optimization:")
        print(f"  success: {result.success}")
        print(f"  message: {result.message}")
        print(f"  evaluations: {eval_counter['n']}")
        print(f"  best loss: {final_loss:.8g}")
        print(f"  best objective: {final_objective:.8g}")
        print(f"  best center: ({final_cx:.6f}, {final_cy:.6f})")
        print(f"  best angle: {final_angle:.6f}")

    return {
        "center_x": float(final_cx),
        "center_y": float(final_cy),
        "angle_deg": float(final_angle),
        "loss": float(final_loss),
        "objective": float(final_objective),
        "rows": rows,
        "runtime_seconds": runtime,
        "optimizer_result": {
            "success": bool(result.success),
            "status": int(result.status),
            "message": str(result.message),
            "nit": int(result.nit),
            "nfev": int(result.nfev),
            "fun": float(result.fun),
            "x_scaled": [float(v) for v in result.x],
            "best_x_scaled": [float(v) for v in final_z],
        },
    }
