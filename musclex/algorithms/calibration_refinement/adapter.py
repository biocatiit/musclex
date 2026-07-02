"""Small callable facade for calibration refinement algorithms.

The copied refinement scripts are intentionally algorithm-heavy and expose
several lower-level functions. This module keeps GUI callers on a compact API.
"""

from __future__ import annotations

from importlib import import_module
from typing import Iterable, Sequence

import numpy as np

CENTER_METHOD_REGISTRATION = "registration"
CENTER_METHOD_GRADIENT = "gradient"
CENTER_METHOD_SEARCH = "search"

CENTER_SEARCH_LEVELS = "4:1:2:0.5,1:0.25:0.5:0.1,0.25:0.05:0.1:0.02"
ROTATION_SEARCH_LEVELS = "2:0.5,0.5:0.1,0.1:0.02"
LOCAL_SEARCH_RADIUS = 200


def _normalise_image(image: np.ndarray) -> np.ndarray:
    arr = np.asarray(image, dtype=np.float64)
    if arr.ndim != 2:
        raise ValueError("Calibration refinement requires a 2D image.")
    return arr


def _normalise_mask(mask, shape) -> np.ndarray:
    if mask is None:
        return np.zeros(shape, dtype=np.uint8)
    arr = np.asarray(mask)
    if arr.shape != shape:
        raise ValueError(
            f"Calibration refinement mask shape {arr.shape} does not match image shape {shape}."
        )
    return (arr != 0).astype(np.uint8)


def _normalise_center(center) -> tuple[float, float]:
    if center is None or len(center) != 2:
        raise ValueError("Calibration refinement requires an initial center.")
    return float(center[0]), float(center[1])


def _normalise_rotation(rotation) -> float:
    return float(rotation if rotation is not None else 0.0)


def _ordered_center_methods(methods: Iterable[str]) -> list[str]:
    selected = {str(method).lower() for method in methods}
    if CENTER_METHOD_SEARCH in selected:
        selected.update({CENTER_METHOD_REGISTRATION, CENTER_METHOD_GRADIENT})
    if CENTER_METHOD_GRADIENT in selected:
        selected.add(CENTER_METHOD_REGISTRATION)
    ordered = []
    for method in (
        CENTER_METHOD_REGISTRATION,
        CENTER_METHOD_GRADIENT,
        CENTER_METHOD_SEARCH,
    ):
        if method in selected:
            ordered.append(method)
    if not ordered:
        raise ValueError("Select at least one center refinement method.")
    return ordered


def _grid_for(image: np.ndarray, module, radius_x=None, radius_y=None):
    h, w = image.shape
    return module.make_canonical_grid(
        w,
        h,
        radius_x=radius_x,
        radius_y=radius_y,
    )


def refine_center(
    image: np.ndarray,
    mask,
    center: Sequence[float],
    rotation,
    methods: Iterable[str],
) -> dict:
    """Run the selected center refinement pipeline.

    Returns a dict with ``center``, ``rotation``, ``loss``, and per-stage results.
    The returned rotation is for diagnostics; GUI callers may choose not to save it.
    """
    image = _normalise_image(image)
    mask = _normalise_mask(mask, image.shape)
    current_center = _normalise_center(center)
    current_rotation = _normalise_rotation(rotation)

    stages = []
    for method in _ordered_center_methods(methods):
        if method == CENTER_METHOD_REGISTRATION:
            reg = import_module(
                "musclex.algorithms.calibration_refinement.refine_center.registration_refinement"
            )

            UX, UY = _grid_for(image, reg)
            result = reg.run_ecc_refinement(
                image=image,
                mask=mask,
                init_center=current_center,
                init_angle_deg=current_rotation,
                UX=UX,
                UY=UY,
                passes=20,
                min_valid_fold=2,
                min_valid_registration=100,
                ecc_iterations=200,
                ecc_eps=1e-5,
                ecc_blur_sigma=1.0,
                w_trans=1.0,
                w_angle=1.0,
                max_center_step=2.0,
                max_angle_step=1.0,
                loss_mode="fold_std_norm",
                erosion_px=3,
                erode_mask=True,
                reseed=True,
                dynamic_ref=True,
                verbose=False,
            )
            current_center = tuple(result["best_center"])
            current_rotation = float(result["best_angle_deg"])

        elif method == CENTER_METHOD_GRADIENT:
            grad = import_module(
                "musclex.algorithms.calibration_refinement.refine_center.gradient_refinement"
            )

            UX, UY = _grid_for(image, grad)
            result = grad.optimize_symmetry_gradient(
                image=image,
                mask=mask,
                init_center=current_center,
                init_angle_deg=current_rotation,
                UX=UX,
                UY=UY,
                max_center_step=1.0,
                max_angle_step=0.5,
                min_valid=2,
                loss_mode="fold_std_norm",
                maxiter=100,
                verbose=False,
            )
            current_center = (result["center_x"], result["center_y"])
            current_rotation = float(result["angle_deg"])

        elif method == CENTER_METHOD_SEARCH:
            search = import_module(
                "musclex.algorithms.calibration_refinement.refine_center.search_refinement"
            )

            UX, UY = _grid_for(
                image,
                search,
                radius_x=max(1, min(LOCAL_SEARCH_RADIUS, image.shape[1] // 2)),
                radius_y=max(1, min(LOCAL_SEARCH_RADIUS, image.shape[0] // 2)),
            )
            result = search.coarse_to_fine_search(
                image=image,
                mask=mask,
                init_center=current_center,
                init_angle_deg=current_rotation,
                UX=UX,
                UY=UY,
                levels=search.parse_levels(CENTER_SEARCH_LEVELS),
                min_valid=2,
                loss_mode="fold_std_norm",
                verbose=False,
            )
            current_center = (result["center_x"], result["center_y"])
            current_rotation = float(result["angle_deg"])

        stages.append({"method": method, "result": result})

    final = stages[-1]["result"]
    return {
        "center": (float(current_center[0]), float(current_center[1])),
        "rotation": float(current_rotation),
        "loss": final.get("best_loss", final.get("loss")),
        "stages": stages,
    }


def refine_rotation(image: np.ndarray, mask, center: Sequence[float], rotation) -> dict:
    """Run angle-only local search with the center fixed."""
    rotation_search = import_module(
        "musclex.algorithms.calibration_refinement.refine_rotation"
    )

    image = _normalise_image(image)
    mask = _normalise_mask(mask, image.shape)
    fixed_center = _normalise_center(center)
    current_rotation = _normalise_rotation(rotation)

    UX, UY = _grid_for(image, rotation_search)
    result = rotation_search.coarse_to_fine_search(
        image=image,
        mask=mask,
        init_center=fixed_center,
        init_angle_deg=current_rotation,
        UX=UX,
        UY=UY,
        levels=rotation_search.parse_angle_only_levels(ROTATION_SEARCH_LEVELS),
        min_valid=2,
        loss_mode="fold_std_norm",
        verbose=False,
    )
    return {
        "center": fixed_center,
        "rotation": float(result["angle_deg"]),
        "loss": result.get("loss"),
        "result": result,
    }
