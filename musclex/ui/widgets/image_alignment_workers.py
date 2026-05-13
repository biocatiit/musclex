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

# Re-export the canonical implementation so existing callers (this module and
# anything still importing _compute_fold_symmetry from here) keep working
# without dragging UI-layer code into headless paths. The function used to be
# defined in this file; it now lives in musclex.utils.fold_symmetry to avoid
# triggering the ui.widgets package __init__ (which loads Qt) when called
# from QuadrantFolder.
from musclex.utils.fold_symmetry import _compute_fold_symmetry  # noqa: F401


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
