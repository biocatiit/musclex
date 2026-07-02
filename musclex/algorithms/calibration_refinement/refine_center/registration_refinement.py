#!/usr/bin/env python3
"""Serial ECC registration refinement for MuscleX calibration refinement."""

from __future__ import annotations

import math
import time
import cv2
import matplotlib.pyplot as plt
import numpy as np

try:
    from musclex.utils.fold_symmetry import _compute_fold_symmetry
except Exception:
    from utils import _compute_fold_symmetry

try:
    from scipy.optimize import least_squares

    SCIPY_AVAILABLE = True
except Exception:
    SCIPY_AVAILABLE = False

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
    values = {}
    weights = {}

    for qname, sx, sy in QUADRANTS:
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

        values[qname] = v.astype(np.float32)
        weights[qname] = valid.astype(np.float32)

    return values, weights


def _local_fold_std_norm(folded, var, count, min_valid):
    """Region-restricted analogue of ``_compute_fold_symmetry``'s ``fold_std_norm``.

    Operates directly on the already-extracted quadrant stack (the
    ``radius_x × radius_y`` folding grid) instead of warping/folding the whole
    image, so the symmetry score is measured over exactly the region the ECC
    registration sees. Returns ``None`` when the foreground cannot be
    determined reliably (mirrors the whole-image version).

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
    entire frame — the same thing ``search-refinement-real.py`` does — keeps
    those numbers comparable across runs with different crop sizes, even
    though the search/registration itself is driven by the radius-restricted
    score from ``_local_fold_std_norm``.
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
    values, weights = extract_quadrants(
        image,
        mask,
        center_x=center_x,
        center_y=center_y,
        angle_deg=angle_deg,
        UX=UX,
        UY=UY,
    )

    vals = np.stack([values[q] for q, _, _ in QUADRANTS], axis=0)
    wts = np.stack([weights[q] for q, _, _ in QUADRANTS], axis=0)

    count = np.sum(wts, axis=0)
    weighted_sum = np.sum(wts * vals, axis=0)

    folded = np.zeros_like(weighted_sum)
    valid_any = count > 0
    folded[valid_any] = weighted_sum[valid_any] / count[valid_any]

    diff2 = wts * (vals - folded[None, :, :]) ** 2

    var = np.zeros_like(folded)
    var[valid_any] = np.sum(diff2, axis=0)[valid_any] / count[valid_any]

    valid_loss = count >= min_valid

    if not np.any(valid_loss):
        residual = np.sqrt(np.maximum(var, 0.0))
        return folded, count, residual, float("inf")

    if loss_mode == "var_mean":
        loss = float(np.mean(var[valid_loss]))

    elif loss_mode in ("fold_std_norm", "local_fold_std_norm"):
        # Both modes drive the search/registration from the radius_x x radius_y
        # folding region — the same quadrants ECC registers — rather than
        # warping/folding the whole image. (`fold_std_norm` differs only in
        # what gets written to the final report; see `_whole_image_fold_std_norm`
        # and its use in `main`.)
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
# Error metrics
# ------------------------------------------------------------


def wrap_angle_deg(angle):
    return (angle + 180.0) % 360.0 - 180.0


def read_initial_estimate_from_report(path: str):
    """Read ``refined_estimate`` (center_xy, angle_degrees) from a prior report.

    Coordinates are expected in original (full-resolution) image scale.
    """
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
# ECC registration
# ------------------------------------------------------------


def preprocess_for_ecc(img, weight, blur_sigma=1.0):
    img = img.astype(np.float32)
    w = (weight > 0).astype(np.uint8)

    out = img.copy()
    out[w == 0] = 0.0

    if blur_sigma > 0:
        k = int(max(3, 2 * round(3 * blur_sigma) + 1))
        # Normalized ("mask-aware") Gaussian blur: smoothing the masked image and
        # the mask separately, then dividing, prevents the zero-filled invalid
        # region from bleeding into valid pixels and creating a false gradient
        # ring at the mask boundary that ECC would otherwise lock onto.
        wf = w.astype(np.float32)
        blurred_vals = cv2.GaussianBlur(out * wf, (k, k), blur_sigma)
        blurred_wts = cv2.GaussianBlur(wf, (k, k), blur_sigma)
        out = np.where(blurred_wts > 1e-6, blurred_vals / blurred_wts, 0.0).astype(
            np.float32
        )
        out[w == 0] = 0.0

    valid = w > 0
    if np.any(valid):
        mean = float(np.mean(out[valid]))
        std = float(np.std(out[valid]))
        if std < 1e-6:
            std = 1.0
        out = (out - mean) / std
        out[w == 0] = 0.0

    return out.astype(np.float32), (w * 255).astype(np.uint8)


def warp_with_ecc_matrix(moving_img, moving_w, warp_matrix, out_shape):
    h, w = out_shape

    moved = cv2.warpAffine(
        moving_img.astype(np.float32),
        warp_matrix,
        (w, h),
        flags=cv2.INTER_LINEAR + cv2.WARP_INVERSE_MAP,
        borderMode=cv2.BORDER_CONSTANT,
        borderValue=0,
    )

    moved_w = cv2.warpAffine(
        moving_w.astype(np.float32),
        warp_matrix,
        (w, h),
        flags=cv2.INTER_NEAREST + cv2.WARP_INVERSE_MAP,
        borderMode=cv2.BORDER_CONSTANT,
        borderValue=0,
    )

    moved_w = (moved_w > 0.5).astype(np.float32)
    return moved, moved_w


def registration_loss_after_warp(
    moving_img, moving_w, ref_img, ref_w, warp_matrix, min_valid=100
):
    moved, moved_w = warp_with_ecc_matrix(
        moving_img, moving_w, warp_matrix, ref_img.shape
    )

    valid = (moved_w > 0) & (ref_w > 0)
    n_valid = int(np.count_nonzero(valid))

    if n_valid < min_valid:
        return float("inf"), n_valid

    diff = moved[valid] - ref_img[valid]
    loss = float(np.mean(diff * diff))
    return loss, n_valid


def rigid_params_from_2x3(M):
    a = float(M[0, 0])
    b = float(M[1, 0])
    angle = math.degrees(math.atan2(b, a))
    dx = float(M[0, 2])
    dy = float(M[1, 2])
    return dx, dy, angle


def save_mask_debug_figure(
    ref_img: np.ndarray,
    ref_ecc: np.ndarray,
    ref_mask: np.ndarray,
    mov_img: np.ndarray,
    mov_ecc: np.ndarray,
    mov_mask: np.ndarray,
    combined_mask: np.ndarray,
    pass_index: int,
    qname: str,
    debug_dir: Path,
):
    """Save a 2×3 diagnostic figure for one (pass, quadrant) ECC registration.

    Row 0: raw quadrant images with each quadrant's own mask overlaid in red.
    Row 1: z-scored ECC inputs with the combined mask overlaid; colour-coded
           mask comparison showing where the reference and moving masks differ.
    """
    debug_dir.mkdir(parents=True, exist_ok=True)

    def _imshow_with_invalid_overlay(ax, img, mask_u8, cmap, title):
        display = np.asarray(img, dtype=np.float32)
        finite = np.isfinite(display)
        vmin = float(np.nanmin(display[finite])) if np.any(finite) else 0.0
        vmax = float(np.nanmax(display[finite])) if np.any(finite) else 1.0
        if vmax <= vmin:
            vmax = vmin + 1.0
        ax.imshow(
            display,
            cmap=cmap,
            vmin=vmin,
            vmax=vmax,
            origin="upper",
            interpolation="nearest",
        )
        # Semi-transparent red over every invalid (masked-out) pixel.
        invalid = (mask_u8 == 0).astype(np.float32)
        rgba = np.zeros((*invalid.shape, 4), dtype=np.float32)
        rgba[..., 0] = 1.0
        rgba[..., 3] = invalid * 0.55
        ax.imshow(rgba, origin="upper", interpolation="nearest")
        ax.set_title(title, fontsize=8)
        ax.axis("off")

    fig, axes = plt.subplots(2, 3, figsize=(14, 9))
    fig.suptitle(
        f"Mask diagnostics — pass {pass_index}, quadrant {qname}\n"
        f"Red overlay = excluded pixels",
        fontsize=11,
    )

    _imshow_with_invalid_overlay(
        axes[0, 0],
        ref_img,
        ref_mask,
        "gray",
        "ref image (raw)\nexcluded by ref_mask",
    )
    _imshow_with_invalid_overlay(
        axes[0, 1],
        mov_img,
        mov_mask,
        "gray",
        f"mov image [{qname}] (raw)\nexcluded by mov_mask",
    )

    n_valid = int(np.count_nonzero(combined_mask))
    n_total = int(combined_mask.size)
    axes[0, 2].imshow(
        combined_mask,
        cmap="gray",
        vmin=0,
        vmax=255,
        origin="upper",
        interpolation="nearest",
    )
    axes[0, 2].set_title(
        f"combined_mask (white = valid)\n{n_valid}/{n_total} = {100*n_valid/n_total:.1f}% valid",
        fontsize=8,
    )
    axes[0, 2].axis("off")

    _imshow_with_invalid_overlay(
        axes[1, 0],
        ref_ecc,
        combined_mask,
        "RdBu_r",
        "ref_ecc (z-scored)\nexcluded by combined_mask",
    )
    _imshow_with_invalid_overlay(
        axes[1, 1],
        mov_ecc,
        combined_mask,
        "RdBu_r",
        "mov_ecc (z-scored)\nexcluded by combined_mask",
    )

    # Colour-coded mask comparison:
    #   white  = both valid           (passes ECC)
    #   red    = ref invalid only     (ref masks out, mov doesn't)
    #   blue   = mov invalid only     (mov masks out, ref doesn't)
    #   dark   = both invalid
    ref_valid = ref_mask > 0
    mov_valid = mov_mask > 0
    rgb = np.zeros((*ref_mask.shape, 3), dtype=np.float32)
    rgb[ref_valid & mov_valid] = [1.0, 1.0, 1.0]
    rgb[~ref_valid & mov_valid] = [1.0, 0.2, 0.2]
    rgb[ref_valid & ~mov_valid] = [0.2, 0.4, 1.0]
    rgb[~ref_valid & ~mov_valid] = [0.15, 0.15, 0.15]
    axes[1, 2].imshow(rgb, origin="upper", interpolation="nearest")
    axes[1, 2].set_title(
        "mask comparison\nwhite=both valid | red=ref only | blue=mov only | grey=both invalid",
        fontsize=8,
    )
    axes[1, 2].axis("off")

    plt.tight_layout()
    safe_qname = qname.replace("+", "p").replace("-", "m")
    out_path = debug_dir / f"mask_debug_pass{pass_index:02d}_{safe_qname}.png"
    plt.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close(fig)


def save_ecc_input_debug_images(
    ref_ecc: np.ndarray,
    mov_ecc: np.ndarray,
    combined_mask: np.ndarray,
    pass_index: int,
    qname: str,
    debug_dir: Path,
):
    """Save ref_ecc / mov_ecc / combined_mask as standalone PNGs for ECC inspection."""
    debug_dir.mkdir(parents=True, exist_ok=True)
    safe_qname = qname.replace("+", "p").replace("-", "m")
    prefix = f"pass{pass_index:02d}_{safe_qname}"

    def _ecc_to_bgr(ecc_img: np.ndarray) -> np.ndarray:
        valid = combined_mask > 0
        arr = np.asarray(ecc_img, dtype=np.float32)
        if np.any(valid):
            scale = max(float(np.max(np.abs(arr[valid]))), 1e-6)
        else:
            scale = 1.0
        norm = np.clip(arr / scale, -1.0, 1.0)
        gray = ((norm + 1.0) * 0.5 * 255.0).astype(np.uint8)
        gray[~valid] = 0
        return cv2.applyColorMap(gray, cv2.COLORMAP_TWILIGHT)

    cv2.imwrite(str(debug_dir / f"ecc_ref_{prefix}.png"), _ecc_to_bgr(ref_ecc))
    cv2.imwrite(str(debug_dir / f"ecc_mov_{prefix}.png"), _ecc_to_bgr(mov_ecc))
    cv2.imwrite(str(debug_dir / f"ecc_combined_mask_{prefix}.png"), combined_mask)


def ecc_register_quadrant_to_reference(
    moving_img,
    moving_w,
    ref_img,
    ref_w,
    pass_index,
    qname,
    number_of_iterations=100,
    termination_eps=1e-6,
    blur_sigma=1.0,
    min_valid=100,
    erosion_px: int = 3,
    erode_mask: bool = True,
    reseed: bool = True,
    debug_dir: "Path | None" = None,
):
    t0 = time.perf_counter()

    ref_ecc, ref_mask = preprocess_for_ecc(ref_img, ref_w, blur_sigma=blur_sigma)
    mov_ecc, mov_mask = preprocess_for_ecc(moving_img, moving_w, blur_sigma=blur_sigma)

    # ECC only accepts a single (template-space) inputMask, so use the
    # intersection of the reference and moving masks. This keeps masked-out
    # pixels of *either* quadrant from contributing to the ECC objective.

    combined_mask = cv2.bitwise_and(ref_mask, mov_mask)

    # Erode the combined mask to exclude pixels adjacent to gap boundaries.
    # Gap edges create large image gradients that ECC can lock onto instead
    # of the diffraction pattern, causing convergence to wrong local optima.
    erode_kernel = None
    if erode_mask and erosion_px > 0:
        k = 2 * erosion_px + 1
        erode_kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (k, k))
        combined_mask = cv2.erode(combined_mask, erode_kernel)

    # Re-z-score both images over the shared valid region so both inputs
    # have the same normalisation basis when the masks differ between quadrants.
    combined_valid = combined_mask > 0
    if np.any(combined_valid):
        for ecc_img in [ref_ecc, mov_ecc]:
            vals = ecc_img[combined_valid]
            m = float(np.mean(vals))
            s = float(np.std(vals))
            if s < 1e-6:
                s = 1.0
            ecc_img -= m
            ecc_img /= s
            ecc_img[~combined_valid] = 0.0

    if debug_dir is not None:
        save_mask_debug_figure(
            ref_img=ref_img,
            ref_ecc=ref_ecc,
            ref_mask=ref_mask,
            mov_img=moving_img,
            mov_ecc=mov_ecc,
            mov_mask=mov_mask,
            combined_mask=combined_mask,
            pass_index=pass_index,
            qname=qname,
            debug_dir=debug_dir,
        )
        save_ecc_input_debug_images(
            ref_ecc=ref_ecc,
            mov_ecc=mov_ecc,
            combined_mask=combined_mask,
            pass_index=pass_index,
            qname=qname,
            debug_dir=debug_dir,
        )

    n_ref_valid = int(np.count_nonzero(combined_mask))
    if n_ref_valid < min_valid:
        runtime = time.perf_counter() - t0
        identity = np.eye(2, 3, dtype=np.float32)
        dx, dy, angle = rigid_params_from_2x3(identity)
        loss, n_valid = registration_loss_after_warp(
            moving_img, moving_w, ref_img, ref_w, identity, min_valid=min_valid
        )
        return {
            "pass": pass_index,
            "quadrant": qname,
            "success": 0,
            "ecc_score": float("nan"),
            "dx": dx,
            "dy": dy,
            "angle_deg": angle,
            "loss": loss,
            "n_valid": n_valid,
            "runtime_seconds": runtime,
            "message": "Not enough valid reference pixels",
            "warp_matrix": identity,
        }

    warp = np.eye(2, 3, dtype=np.float32)
    criteria = (
        cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT,
        int(number_of_iterations),
        float(termination_eps),
    )

    try:
        ecc_score, warp = cv2.findTransformECC(
            templateImage=ref_ecc,
            inputImage=mov_ecc,
            warpMatrix=warp,
            motionType=cv2.MOTION_EUCLIDEAN,
            criteria=criteria,
            inputMask=combined_mask,
            gaussFiltSize=1,
        )

        if reseed:
            # Re-seed pass: recompute the combined mask at the found warp so the
            # mask accurately reflects which moving pixels are valid after the
            # transformation (the initial combined_mask was at the identity warp,
            # which diverges from truth as the correction grows).
            warped_mov_mask = cv2.warpAffine(
                mov_mask.astype(np.float32),
                warp,
                (ref_mask.shape[1], ref_mask.shape[0]),
                flags=cv2.INTER_NEAREST + cv2.WARP_INVERSE_MAP,
                borderMode=cv2.BORDER_CONSTANT,
                borderValue=0,
            )
            refined_combined = cv2.bitwise_and(
                ref_mask, (warped_mov_mask > 0.5).astype(np.uint8) * 255
            )
            if erode_kernel is not None:
                refined_combined = cv2.erode(refined_combined, erode_kernel)

            if int(np.count_nonzero(refined_combined)) >= min_valid:
                ecc_score2, warp = cv2.findTransformECC(
                    templateImage=ref_ecc,
                    inputImage=mov_ecc,
                    warpMatrix=warp.copy(),
                    motionType=cv2.MOTION_EUCLIDEAN,
                    criteria=criteria,
                    inputMask=refined_combined,
                    gaussFiltSize=1,
                )
                ecc_score = ecc_score2

        success = 1
        message = "OK"

    except cv2.error as e:
        ecc_score = float("nan")
        warp = np.eye(2, 3, dtype=np.float32)
        success = 0
        message = str(e).split("\n")[0]

    dx, dy, angle = rigid_params_from_2x3(warp)

    loss, n_valid = registration_loss_after_warp(
        moving_img=moving_img,
        moving_w=moving_w,
        ref_img=ref_img,
        ref_w=ref_w,
        warp_matrix=warp,
        min_valid=min_valid,
    )

    runtime = time.perf_counter() - t0

    return {
        "pass": pass_index,
        "quadrant": qname,
        "success": success,
        "ecc_score": float(ecc_score) if np.isfinite(ecc_score) else float("nan"),
        "dx": dx,
        "dy": dy,
        "angle_deg": angle,
        "loss": loss,
        "n_valid": n_valid,
        "runtime_seconds": runtime,
        "message": message,
        "warp_matrix": warp.astype(np.float32),
    }


# ------------------------------------------------------------
# Global correction fitting
# ------------------------------------------------------------


def rotation_matrix(theta_deg):
    t = math.radians(theta_deg)
    c = math.cos(t)
    s = math.sin(t)
    return np.array([[c, -s], [s, c]], dtype=np.float64)


def sign_matrix(qname):
    if qname == "++":
        return np.diag([+1.0, +1.0])
    if qname == "+-":
        return np.diag([+1.0, -1.0])
    if qname == "-+":
        return np.diag([-1.0, +1.0])
    if qname == "--":
        return np.diag([-1.0, -1.0])
    raise ValueError(f"Unknown quadrant {qname}")


def homogeneous_M(center_x, center_y, angle_deg, qname):
    R = rotation_matrix(angle_deg)
    S = sign_matrix(qname)
    A = R @ S

    M = np.eye(3, dtype=np.float64)
    M[0:2, 0:2] = A
    M[0:2, 2] = [center_x, center_y]
    return M


def rigid_params_from_homogeneous(H):
    A = H[0:2, 0:2]
    dx, dy = H[0:2, 2]
    angle = math.degrees(math.atan2(A[1, 0], A[0, 0]))
    return float(dx), float(dy), float(angle)


def predicted_registration_transform(
    current_center_x,
    current_center_y,
    current_angle_deg,
    dcx,
    dcy,
    dtheta_deg,
    qname,
    ref_qname="++",
):
    c0x = current_center_x
    c0y = current_center_y
    a0 = current_angle_deg

    c1x = current_center_x + dcx
    c1y = current_center_y + dcy
    a1 = current_angle_deg + dtheta_deg

    Mq0 = homogeneous_M(c0x, c0y, a0, qname)
    Mq1 = homogeneous_M(c1x, c1y, a1, qname)

    Mr0 = homogeneous_M(c0x, c0y, a0, ref_qname)
    Mr1 = homogeneous_M(c1x, c1y, a1, ref_qname)

    H = np.linalg.inv(Mq0) @ Mq1 @ np.linalg.inv(Mr1) @ Mr0
    return H


def global_fit_residual_vector(
    x,
    observed_transforms,
    current_center,
    current_angle_deg,
    w_trans=1.0,
    w_angle=10.0,
    ref_qname="++",
):
    dcx, dcy, dtheta = x
    residuals = []

    for qname, obs in observed_transforms.items():
        if qname == ref_qname:
            continue

        Hpred = predicted_registration_transform(
            current_center_x=current_center[0],
            current_center_y=current_center[1],
            current_angle_deg=current_angle_deg,
            dcx=dcx,
            dcy=dcy,
            dtheta_deg=dtheta,
            qname=qname,
            ref_qname=ref_qname,
        )

        pdx, pdy, pangle = rigid_params_from_homogeneous(Hpred)

        odx = obs["dx"]
        ody = obs["dy"]
        oangle = obs["angle_deg"]
        weight = math.sqrt(max(obs.get("weight", 1.0), 1e-12))

        residuals.append(weight * math.sqrt(w_trans) * (odx - pdx))
        residuals.append(weight * math.sqrt(w_trans) * (ody - pdy))
        residuals.append(weight * math.sqrt(w_angle) * wrap_angle_deg(oangle - pangle))

    if not residuals:
        return np.array([1e6], dtype=np.float64)

    return np.array(residuals, dtype=np.float64)


def global_objective_from_residuals(res):
    return float(np.mean(res * res))


def fit_global_correction_ecc(
    observed_transforms,
    current_center,
    current_angle_deg,
    pass_index,
    w_trans=1.0,
    w_angle=10.0,
    max_center_step=10.0,
    max_angle_step=5.0,
    ref_qname="++",
):
    t0 = time.perf_counter()
    rows = []

    def record(eval_index, candidate, is_new_best, best):
        cand_center = [
            current_center[0] + candidate[0],
            current_center[1] + candidate[1],
        ]
        cand_angle = current_angle_deg + candidate[2]

        best_center = [current_center[0] + best[0], current_center[1] + best[1]]
        best_angle = current_angle_deg + best[2]

        cand_obj = global_objective_from_residuals(
            global_fit_residual_vector(
                candidate,
                observed_transforms,
                current_center,
                current_angle_deg,
                w_trans=w_trans,
                w_angle=w_angle,
                ref_qname=ref_qname,
            )
        )
        best_obj = global_objective_from_residuals(
            global_fit_residual_vector(
                best,
                observed_transforms,
                current_center,
                current_angle_deg,
                w_trans=w_trans,
                w_angle=w_angle,
                ref_qname=ref_qname,
            )
        )

        rows.append(
            {
                "pass": pass_index,
                "eval_index": eval_index,
                "candidate_dcx": float(candidate[0]),
                "candidate_dcy": float(candidate[1]),
                "candidate_dtheta_deg": float(candidate[2]),
                "candidate_center_x": float(cand_center[0]),
                "candidate_center_y": float(cand_center[1]),
                "candidate_angle_deg": float(cand_angle),
                "candidate_objective": cand_obj,
                "is_new_best": int(is_new_best),
                "best_so_far_dcx": float(best[0]),
                "best_so_far_dcy": float(best[1]),
                "best_so_far_dtheta_deg": float(best[2]),
                "best_so_far_center_x": float(best_center[0]),
                "best_so_far_center_y": float(best_center[1]),
                "best_so_far_angle_deg": float(best_angle),
                "best_so_far_objective": best_obj,
            }
        )

    if SCIPY_AVAILABLE:
        eval_counter = {"n": 0}
        best_x = np.array([0.0, 0.0, 0.0], dtype=np.float64)
        best_obj = global_objective_from_residuals(
            global_fit_residual_vector(
                best_x,
                observed_transforms,
                current_center,
                current_angle_deg,
                w_trans=w_trans,
                w_angle=w_angle,
                ref_qname=ref_qname,
            )
        )

        def fun(x):
            res = global_fit_residual_vector(
                x,
                observed_transforms,
                current_center,
                current_angle_deg,
                w_trans=w_trans,
                w_angle=w_angle,
                ref_qname=ref_qname,
            )

            obj = global_objective_from_residuals(res)
            eval_counter["n"] += 1

            nonlocal_best = False
            if obj < eval_counter.get("best_obj", best_obj):
                eval_counter["best_obj"] = obj
                eval_counter["best_x"] = np.array(x, dtype=np.float64)
                nonlocal_best = True

            bx = eval_counter.get("best_x", best_x)
            record(eval_counter["n"], np.array(x, dtype=np.float64), nonlocal_best, bx)
            return res

        bounds = (
            np.array(
                [-max_center_step, -max_center_step, -max_angle_step], dtype=np.float64
            ),
            np.array(
                [+max_center_step, +max_center_step, +max_angle_step], dtype=np.float64
            ),
        )

        result = least_squares(
            fun,
            x0=np.array([0.0, 0.0, 0.0], dtype=np.float64),
            bounds=bounds,
            method="trf",
            xtol=1e-10,
            ftol=1e-10,
            gtol=1e-10,
            max_nfev=200,
        )

        best = result.x.astype(np.float64)
        best_obj = global_objective_from_residuals(
            global_fit_residual_vector(
                best,
                observed_transforms,
                current_center,
                current_angle_deg,
                w_trans=w_trans,
                w_angle=w_angle,
                ref_qname=ref_qname,
            )
        )
        method = "scipy_least_squares"
        success = bool(result.success)
        message = str(result.message)

    else:
        method = "fallback_coordinate_search"
        success = True
        message = "SciPy unavailable; used fallback coordinate search."

        best = np.array([0.0, 0.0, 0.0], dtype=np.float64)
        best_obj = global_objective_from_residuals(
            global_fit_residual_vector(
                best,
                observed_transforms,
                current_center,
                current_angle_deg,
                w_trans=w_trans,
                w_angle=w_angle,
                ref_qname=ref_qname,
            )
        )

        eval_index = 0
        steps = [
            np.array([2.0, 2.0, 1.0]),
            np.array([0.5, 0.5, 0.25]),
            np.array([0.1, 0.1, 0.05]),
            np.array([0.03, 0.03, 0.01]),
        ]

        for step in steps:
            improved = True
            while improved:
                improved = False
                for j in range(3):
                    for sgn in [-1.0, +1.0]:
                        cand = best.copy()
                        cand[j] += sgn * step[j]
                        cand[0] = np.clip(cand[0], -max_center_step, max_center_step)
                        cand[1] = np.clip(cand[1], -max_center_step, max_center_step)
                        cand[2] = np.clip(cand[2], -max_angle_step, max_angle_step)

                        res = global_fit_residual_vector(
                            cand,
                            observed_transforms,
                            current_center,
                            current_angle_deg,
                            w_trans=w_trans,
                            w_angle=w_angle,
                        )
                        obj = global_objective_from_residuals(res)
                        eval_index += 1

                        is_new_best = obj < best_obj
                        if is_new_best:
                            best = cand
                            best_obj = obj
                            improved = True

                        record(eval_index, cand, is_new_best, best)

    runtime = time.perf_counter() - t0

    return {
        "dcx": float(best[0]),
        "dcy": float(best[1]),
        "dtheta_deg": float(best[2]),
        "objective": float(best_obj),
        "method": method,
        "success": int(success),
        "message": message,
        "runtime_seconds": runtime,
        "rows": rows,
    }


def run_ecc_refinement(
    image,
    mask,
    init_center,
    init_angle_deg,
    UX,
    UY,
    passes,
    min_valid_fold,
    min_valid_registration,
    ecc_iterations,
    ecc_eps,
    ecc_blur_sigma,
    w_trans,
    w_angle,
    max_center_step,
    max_angle_step,
    loss_mode: str = "var_mean",
    erosion_px: int = 3,
    erode_mask: bool = True,
    reseed: bool = True,
    dynamic_ref: bool = True,
    verbose=True,
    debug_dir: "Path | None" = None,
):
    center = [float(init_center[0]), float(init_center[1])]
    angle = float(init_angle_deg)

    all_reg_rows = []
    all_global_rows = []
    pass_rows = []

    t0 = time.perf_counter()

    _, _, _, init_loss = fold_and_loss(
        image,
        mask,
        center[0],
        center[1],
        angle,
        UX,
        UY,
        min_valid=min_valid_fold,
        loss_mode=loss_mode,
    )
    best_loss = init_loss
    best_center = list(center)
    best_angle = angle
    best_pass = 0  # 0 means the initial estimate is the best

    for pass_index in range(1, passes + 1):
        _, _, _, fold_loss_before = fold_and_loss(
            image,
            mask,
            center[0],
            center[1],
            angle,
            UX,
            UY,
            min_valid=min_valid_fold,
            loss_mode=loss_mode,
        )

        values, weights = extract_quadrants(
            image,
            mask,
            center_x=center[0],
            center_y=center[1],
            angle_deg=angle,
            UX=UX,
            UY=UY,
        )

        if dynamic_ref:
            # Choose the quadrant with the most valid pixels as reference so that
            # ECC has the largest available overlap region for all three pairs.
            ref_qname = max(
                ("++", "+-", "-+", "--"),
                key=lambda q: int(np.count_nonzero(weights[q] > 0)),
            )
        else:
            ref_qname = "++"
        ref_img = values[ref_qname]
        ref_w = weights[ref_qname]

        observed = {
            ref_qname: {
                "dx": 0.0,
                "dy": 0.0,
                "angle_deg": 0.0,
                "weight": 1.0,
                "loss": 0.0,
                "n_valid": int(np.count_nonzero(ref_w > 0)),
            }
        }

        reg_runtime = 0.0

        for qname in ("++", "+-", "-+", "--"):
            if qname == ref_qname:
                continue
            reg = ecc_register_quadrant_to_reference(
                moving_img=values[qname],
                moving_w=weights[qname],
                ref_img=ref_img,
                ref_w=ref_w,
                pass_index=pass_index,
                qname=qname,
                number_of_iterations=ecc_iterations,
                termination_eps=ecc_eps,
                blur_sigma=ecc_blur_sigma,
                min_valid=min_valid_registration,
                erosion_px=erosion_px,
                erode_mask=erode_mask,
                reseed=reseed,
                debug_dir=debug_dir,
            )

            M = reg["warp_matrix"]

            all_reg_rows.append(
                {
                    "pass": reg["pass"],
                    "quadrant": reg["quadrant"],
                    "success": reg["success"],
                    "ecc_score": reg["ecc_score"],
                    "dx": reg["dx"],
                    "dy": reg["dy"],
                    "angle_deg": reg["angle_deg"],
                    "loss": reg["loss"],
                    "n_valid": reg["n_valid"],
                    "runtime_seconds": reg["runtime_seconds"],
                    "message": reg["message"],
                    "warp_00": float(M[0, 0]),
                    "warp_01": float(M[0, 1]),
                    "warp_02": float(M[0, 2]),
                    "warp_10": float(M[1, 0]),
                    "warp_11": float(M[1, 1]),
                    "warp_12": float(M[1, 2]),
                }
            )

            reg_runtime += reg["runtime_seconds"]

            # Confidence weights how much this quadrant's observed transform is
            # trusted in the global fit. Use the ECC score (a normalized
            # correlation coefficient in ~[0, 1], higher = better) rather than the
            # raw-intensity MSE, so the weighting is scale-invariant and needs no
            # dataset-specific tuning constant.
            if reg["success"] and np.isfinite(reg["ecc_score"]):
                confidence = reg["n_valid"] * max(reg["ecc_score"], 0.0)
            else:
                confidence = 1e-6

            observed[qname] = {
                "dx": reg["dx"],
                "dy": reg["dy"],
                "angle_deg": reg["angle_deg"],
                "weight": confidence,
                "loss": reg["loss"],
                "n_valid": reg["n_valid"],
                "success": reg["success"],
            }

        fit = fit_global_correction_ecc(
            observed_transforms=observed,
            current_center=center,
            current_angle_deg=angle,
            pass_index=pass_index,
            w_trans=w_trans,
            w_angle=w_angle,
            max_center_step=max_center_step,
            max_angle_step=max_angle_step,
            ref_qname=ref_qname,
        )

        all_global_rows.extend(fit["rows"])

        dcx = fit["dcx"]
        dcy = fit["dcy"]
        dtheta = fit["dtheta_deg"]

        new_center = [center[0] + dcx, center[1] + dcy]
        new_angle = angle + dtheta

        _, _, _, fold_loss_after = fold_and_loss(
            image,
            mask,
            new_center[0],
            new_center[1],
            new_angle,
            UX,
            UY,
            min_valid=min_valid_fold,
            loss_mode=loss_mode,
        )

        is_new_best = fold_loss_after < best_loss
        if is_new_best:
            best_loss = fold_loss_after
            best_center = list(new_center)
            best_angle = new_angle
            best_pass = pass_index

        pass_rows.append(
            {
                "pass": pass_index,
                "center_x_before": center[0],
                "center_y_before": center[1],
                "angle_before_deg": angle,
                "fold_loss_before": fold_loss_before,
                "dcx": dcx,
                "dcy": dcy,
                "dtheta_deg": dtheta,
                "fit_objective": fit["objective"],
                "center_x_after": new_center[0],
                "center_y_after": new_center[1],
                "angle_after_deg": new_angle,
                "fold_loss_after": fold_loss_after,
                "is_new_best": int(is_new_best),
                "best_loss_so_far": best_loss,
                "registration_runtime_seconds": reg_runtime,
                "global_fit_runtime_seconds": fit["runtime_seconds"],
            }
        )

        if verbose:
            best_marker = " *best*" if is_new_best else ""
            print(
                f"Pass {pass_index}: "
                f"dc=({dcx:.4f},{dcy:.4f}), "
                f"dtheta={dtheta:.4f}, "
                f"loss {fold_loss_before:.8g} -> {fold_loss_after:.8g}"
                f"{best_marker}, "
                f"center=({new_center[0]:.4f}, {new_center[1]:.4f}), "
                f"angle={new_angle:.4f}"
            )

        center = new_center
        angle = new_angle

    total_runtime = time.perf_counter() - t0

    return {
        "center": center,
        "angle_deg": angle,
        "best_center": best_center,
        "best_angle_deg": best_angle,
        "best_loss": best_loss,
        "best_pass": best_pass,
        "init_loss": init_loss,
        "all_registration_rows": all_reg_rows,
        "all_global_fit_rows": all_global_rows,
        "pass_rows": pass_rows,
        "total_runtime_seconds": total_runtime,
    }
