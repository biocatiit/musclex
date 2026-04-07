"""Shared module-level helpers for AddIntensitiesSingleExp and AddIntensitiesMultipleExp.

All items here are pure (no window state) and safe to import in subprocess
workers via the standard top-level import mechanism.
"""

import traceback

from PySide6.QtCore import Qt, Signal, QRunnable, QObject, QSettings
from PySide6.QtWidgets import (
    QStyledItemDelegate, QStyleOptionViewItem,
    QDialog, QVBoxLayout, QHBoxLayout, QCheckBox, QPushButton, QTextBrowser,
)


class WorkflowGuideDialog(QDialog):
    """Reusable workflow-guide dialog.

    Parameters
    ----------
    title        : Window / heading title string.
    html         : HTML content to display in the browser widget.
    settings_key : QSettings key used to persist the "don't show again" choice.
    parent       : Optional parent widget.
    """

    def __init__(self, title: str, html: str, settings_key: str, parent=None):
        super().__init__(parent)
        self.setWindowTitle(title)
        self._settings_key = settings_key
        self.resize(600, 540)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(12, 12, 12, 12)
        layout.setSpacing(8)

        browser = QTextBrowser()
        browser.setHtml(html)
        browser.setOpenExternalLinks(False)
        browser.setReadOnly(True)
        layout.addWidget(browser, 1)

        bottom_row = QHBoxLayout()
        self._dont_show_chk = QCheckBox("Don't show this again")
        bottom_row.addWidget(self._dont_show_chk)
        bottom_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.setDefault(True)
        close_btn.clicked.connect(self._on_close)
        bottom_row.addWidget(close_btn)
        layout.addLayout(bottom_row)

    @staticmethod
    def _settings():
        return QSettings("BioCAT", "MuscleX")

    def _on_close(self):
        if self._dont_show_chk.isChecked():
            self._settings().setValue(self._settings_key, True)
        self.accept()

    @staticmethod
    def show_if_needed(title: str, html: str, settings_key: str, parent=None):
        """Show the dialog unless the user has previously suppressed it."""
        if WorkflowGuideDialog._settings().value(settings_key, False, type=bool):
            return
        WorkflowGuideDialog(title, html, settings_key, parent).exec()

    @staticmethod
    def show_always(title: str, html: str, settings_key: str, parent=None):
        """Show the dialog unconditionally (e.g. triggered by a toolbar button)."""
        WorkflowGuideDialog(title, html, settings_key, parent).exec()


class _ElideMiddleDelegate(QStyledItemDelegate):
    """Delegate that elides text in the middle when it exceeds the cell width."""

    def paint(self, painter, option, index):
        text = index.data(Qt.DisplayRole) or ""
        elided = option.fontMetrics.elidedText(text, Qt.ElideMiddle, option.rect.width() - 6)
        opt = QStyleOptionViewItem(option)
        self.initStyleOption(opt, index)
        opt.text = elided
        super().paint(painter, opt, index)


def _compute_image_diff(args):
    """Top-level function for subprocess: compute mean abs diff between two images.
    Each image is aligned to the base center/rotation.
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
    """Top-level function for subprocess: load image, compute center and rotation.

    manual_center / manual_rotation are intentionally NOT passed to ImageData so
    that the auto-detection always runs on the raw image.  The caller stores the
    result in the auto-geometry cache; the manual values are applied separately
    when the effective (final) center/rotation is needed.
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


def _sum_group_worker(args):
    """Top-level function for subprocess: load, sum/average images in one group, save to disk."""
    (group_num, dir_path, img_names, specs,
     per_img_transforms, base_center, base_rotation,
     blank_img, blank_weight, apply_blank,
     do_average, output_path, compress,
     rotation_mode) = args
    try:
        from musclex.utils.file_manager import load_image_via_spec
        import cv2 as _cv2
        import numpy as _np
        import fabio as _fabio

        def _transform_img(img, center, rotation, b_center, b_rotation):
            h, w = img.shape[:2]
            if center is not None and b_center is not None:
                tx = b_center[0] - center[0]
                ty = b_center[1] - center[1]
                if tx != 0 or ty != 0:
                    M = _np.float32([[1, 0, tx], [0, 1, ty]])
                    img = _cv2.warpAffine(img, M, (w, h))
            if rotation_mode == 'absolute':
                angle = rotation or 0
            else:
                angle = (rotation or 0) - (b_rotation or 0)
            if angle != 0 and b_center is not None:
                M2 = _cv2.getRotationMatrix2D(tuple(b_center), angle, 1)
                img = _cv2.warpAffine(img, M2, (w, h))
            return img

        blank_f = None
        if apply_blank and blank_img is not None:
            blank_f = blank_img.astype(_np.float32) * blank_weight

        images = []
        for i, (name, spec) in enumerate(zip(img_names, specs)):
            try:
                img = load_image_via_spec(dir_path, name, spec).astype(_np.float32)
            except Exception as e:
                print(f"Error loading {name}: {e}")
                continue

            if blank_f is not None:
                img = _np.clip(img - blank_f, 0, None)

            center, rotation = per_img_transforms[i]
            img = _transform_img(img, center, rotation, base_center, base_rotation)

            images.append(img)

        if not images:
            return {'group_num': group_num, 'output_path': None,
                    'n_images': 0, 'error': 'No images loaded'}

        result = images[0].copy()
        for img in images[1:]:
            if img.shape != result.shape:
                max_h = max(img.shape[0], result.shape[0])
                max_w = max(img.shape[1], result.shape[1])
                p = _np.zeros((max_h, max_w), dtype=result.dtype)
                p[:result.shape[0], :result.shape[1]] = result
                result = p
                q = _np.zeros((max_h, max_w), dtype=img.dtype)
                q[:img.shape[0], :img.shape[1]] = img
                img = q
            result += img

        if do_average:
            result /= len(images)

        if compress:
            from PIL import Image as _Image
            _Image.fromarray(result).save(output_path, compression='tiff_lzw')
        else:
            _fabio.tifimage.tifimage(data=result).write(output_path)

        return {'group_num': group_num, 'output_path': output_path,
                'n_images': len(images), 'total_intensity': float(_np.sum(result)),
                'error': None}
    except Exception as e:
        traceback.print_exc()
        return {'group_num': group_num, 'output_path': None,
                'n_images': 0, 'error': str(e)}


class _GeometryWorkerSignals(QObject):
    done = Signal(object, object, int)  # center, rotation, row_index


class _GeometryWorker(QRunnable):
    """Background thread worker for single-image center/rotation calculation."""

    def __init__(self, image_data, row):
        super().__init__()
        self.image_data = image_data
        self.row = row
        self.signals = _GeometryWorkerSignals()

    def run(self):
        try:
            center = self.image_data.center
            rotation = self.image_data.rotation
            self.signals.done.emit(center, rotation, self.row)
        except Exception:
            traceback.print_exc()
            self.signals.done.emit(None, None, self.row)
