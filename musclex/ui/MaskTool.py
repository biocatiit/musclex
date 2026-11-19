"""
Mask Tool for MuscleX

Reuses the existing MuscleX ImageViewerWidget and DisplayOptionsPanel.
Drawing is performed directly on the same Matplotlib canvas as the image.

Tools:
    - Rectangle
    - Oval
    - Polygon
    - Pencil
    - Eraser
    - Undo / Redo
    - Clear
    - Save / Load mask (.npy)

The existing ImageViewerWidget provides:
    - zoom / pan
    - intensity controls
    - grayscale / inverse grayscale
    - optional double zoom

The image coordinate system is intentionally the same as ImageViewerWidget:
event.xdata -> X pixel coordinate
event.ydata -> Y pixel coordinate
mask[y, x] -> image pixel
"""

from __future__ import annotations

import os
import base64
import json
import zlib
from pathlib import Path

import numpy as np

try:
    import cv2
except ImportError:  # pragma: no cover - only needed for interactive mask transforms
    cv2 = None

from matplotlib.colors import ListedColormap
from matplotlib.path import Path as MplPath
from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QApplication,
    QFileDialog,
    QHBoxLayout,
    QLabel,
    QMainWindow,
    QMessageBox,
    QPushButton,
    QSpinBox,
    QVBoxLayout,
    QWidget,
)

from .widgets.image_viewer_widget import ImageViewerWidget

try:
    from musclex.ui.stylesheet import stylesheet
except ImportError:
    stylesheet = ""


class MaskImageViewer(ImageViewerWidget):
    """Existing MuscleX viewer with pan disabled while a mask tool is drawing."""

    def __init__(self, parent=None):
        super().__init__(
            parent=parent,
            show_display_panel=True,
            show_double_zoom=True,
        )
        self.mask_drawing_enabled = False

    def _handle_pan_start(self, event):
        if self.mask_drawing_enabled:
            return
        super()._handle_pan_start(event)

    def _handle_pan_drag(self, event):
        if self.mask_drawing_enabled:
            return
        super()._handle_pan_drag(event)


class MaskTool(QWidget):
    """
    Interactive image mask editor.

    Parameters
    ----------
    image : numpy.ndarray, optional
        2-D image to display.
    parent : QWidget, optional
        Qt parent.
    """

    def __init__(self, image=None, parent=None):
        super().__init__(parent)

        # Embeddable editor: parent dialog owns the window and Save action.
        if stylesheet:
            self.setStyleSheet(stylesheet)

        self.viewer = MaskImageViewer(self)

        self.mask = None
        self.undo_stack = []
        self.redo_stack = []

        self.current_tool = None
        self.start_xy = None
        self.current_xy = None
        # self.polygon_points = []
        # self.last_pencil_xy = None
        self.polygon_points = []
        self.last_pencil_xy = None

        # Polygon vertex editing
        self.dragging_vertex = None       # index of vertex being dragged
        self.completed_polygon = None     # most recently completed polygon
        self.completed_polygon_base = None  # mask before that polygon was applied

        # Editable geometric masks.  Each completed rectangle/oval/polygon is
        # kept as an object so a later double-click can select and transform it
        # instead of treating the whole mask as an undifferentiated raster.
        self.editable_shapes = []
        self.next_shape_id = 0
        self.selected_shape_index = None
        self.edit_mode = False
        self.edit_drag_mode = None       # "vertex" or "translate"
        self.edit_vertex_index = None
        self.edit_start_xy = None
        self.edit_original_shape = None
        self.edit_original_mask = None
        self.edit_base_mask = None
        self.edit_preview_artist = None

        self.mask_artist = None
        self.low_threshold_artist = None
        self.high_threshold_artist = None
        self.low_threshold_mask = None
        self.high_threshold_mask = None
        self.preview_artist = None

        self.brush_size = 10
        self.mask_alpha = 0.35

        self._build_ui()
        self._connect_signals()

        if image is not None:
            self.set_image(image)

    # ------------------------------------------------------------------
    # UI
    # ------------------------------------------------------------------

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setContentsMargins(6, 6, 6, 6)
        root.setSpacing(6)

        # Main viewer.
        root.addWidget(self.viewer, 1)

        # Existing DisplayOptionsPanel from ImageViewerWidget.
        # It contains intensity, colormap, zoom and double-zoom controls.
        if self.viewer.display_panel is not None:
            root.addWidget(self.viewer.display_panel)

        # Mask controls.
        controls = QHBoxLayout()
        controls.setSpacing(5)

        controls.addWidget(QLabel("Tool:"))

        self.rectangleButton = QPushButton("Rectangle")
        self.ovalButton = QPushButton("Oval")
        self.polygonButton = QPushButton("Polygon")
        self.pencilButton = QPushButton("Pencil")
        self.eraserButton = QPushButton("Eraser")

        self.tool_buttons = [
            self.rectangleButton,
            self.ovalButton,
            self.polygonButton,
            self.pencilButton,
            self.eraserButton,
        ]

        for button in self.tool_buttons:
            button.setCheckable(True)
            controls.addWidget(button)

        controls.addWidget(QLabel("Width:"))

        self.brushSpinBox = QSpinBox()
        self.brushSpinBox.setRange(1, 200)
        self.brushSpinBox.setValue(self.brush_size)
        self.brushSpinBox.setSuffix(" px")
        controls.addWidget(self.brushSpinBox)

        self.undoButton = QPushButton("Undo")
        self.redoButton = QPushButton("Redo")
        self.clearButton = QPushButton("Clear")
        # Saving is handled by the containing Set Image Mask dialog.
        self.saveButton = QPushButton("Save Mask")
        self.saveButton.setVisible(False)
        self.loadButton = QPushButton("Load Mask")

        controls.addWidget(self.undoButton)
        controls.addWidget(self.redoButton)
        controls.addWidget(self.clearButton)
        controls.addWidget(self.loadButton)

        root.addLayout(controls)

        self.statusLabel = QLabel(
            "Select a drawing tool. Use the existing viewer controls for zoom/intensity."
        )
        root.addWidget(self.statusLabel)

    def _connect_signals(self):
        self.rectangleButton.clicked.connect(
            lambda: self._select_tool("rectangle")
        )
        self.ovalButton.clicked.connect(lambda: self._select_tool("oval"))
        self.polygonButton.clicked.connect(lambda: self._select_tool("polygon"))
        self.pencilButton.clicked.connect(lambda: self._select_tool("pencil"))
        self.eraserButton.clicked.connect(lambda: self._select_tool("eraser"))

        self.undoButton.clicked.connect(self.undo)
        self.redoButton.clicked.connect(self.redo)
        self.clearButton.clicked.connect(self.clear_mask)
        self.loadButton.clicked.connect(self.load_mask)

        self.brushSpinBox.valueChanged.connect(self._brush_size_changed)

        # ImageViewerWidget emits these raw Matplotlib events before its
        # internal pan/tool handling.
        self.viewer.mousePressed.connect(self._mouse_pressed)
        self.viewer.mouseMoved.connect(self._mouse_moved)
        self.viewer.mouseReleased.connect(self._mouse_released)

        # display_image() and display settings changes can recreate the
        # axes image, so redraw our mask overlay afterward.
        self.viewer.displayOptionsChanged.connect(
            lambda _options: self._redraw_mask_overlay()
        )

    # ------------------------------------------------------------------
    # Image / mask setup
    # ------------------------------------------------------------------

    def set_image(self, image):
        image = np.asarray(image)

        if image.ndim != 2:
            raise ValueError(
                f"MaskTool expects a 2-D image, got shape {image.shape}"
            )

        self.viewer.display_image(image)

        self.mask = np.zeros(image.shape, dtype=bool)
        self.undo_stack.clear()
        self.redo_stack.clear()
        self.polygon_points = []
        self.editable_shapes = []
        self.selected_shape_index = None
        self.edit_mode = False
        self.edit_drag_mode = None
        self.edit_vertex_index = None

        self._redraw_mask_overlay()

        self.statusLabel.setText(
            f"Image: {image.shape[1]} x {image.shape[0]} pixels"
        )

    def set_threshold_masks(self, low_mask=None, high_mask=None):
        """Update live threshold overlays without changing the editable mask.

        Threshold masks use the dialog convention: 1=keep, 0=masked.
        """
        shape = self.mask.shape if self.mask is not None else None
        prepared = []
        for threshold_mask in (low_mask, high_mask):
            if threshold_mask is None:
                prepared.append(None)
                continue
            arr = np.asarray(threshold_mask)
            if shape is None or arr.shape != shape:
                raise ValueError(
                    f"Threshold mask shape {arr.shape} does not match image shape {shape}."
                )
            prepared.append(arr.astype(bool, copy=True))
        self.low_threshold_mask, self.high_threshold_mask = prepared
        self._redraw_mask_overlay()

    def get_mask(self):
        """Return the current boolean mask."""
        if self.mask is None:
            return None
        return self.mask.copy()

    def set_mask(self, mask):
        """Load an existing boolean/0-1 mask into the editor."""
        if self.mask is None:
            return
        mask = np.asarray(mask).astype(bool)
        if mask.shape != self.mask.shape:
            raise ValueError(
                f"Mask shape {mask.shape} does not match image shape {self.mask.shape}."
            )
        self.mask = mask.copy()
        self.undo_stack.clear()
        self.redo_stack.clear()
        self._clear_preview()
        self._redraw_mask_overlay()

    def has_mask(self):
        return self.mask is not None and bool(np.any(self.mask))

    # ------------------------------------------------------------------
    # Tool selection
    # ------------------------------------------------------------------

    def _select_tool(self, tool_name):
        # Clicking an already selected tool toggles it off.
        if self.current_tool == tool_name:
            self._deactivate_tool()
            return

        self._clear_preview()

        for button in self.tool_buttons:
            button.setChecked(False)

        button_map = {
            "rectangle": self.rectangleButton,
            "oval": self.ovalButton,
            "polygon": self.polygonButton,
            "pencil": self.pencilButton,
            "eraser": self.eraserButton,
        }

        button_map[tool_name].setChecked(True)

        self.current_tool = tool_name
        self.viewer.mask_drawing_enabled = True
        self.start_xy = None
        self.current_xy = None
        self.polygon_points = []
        self.last_pencil_xy = None

        if tool_name == "polygon":
            self.statusLabel.setText(
                "Polygon: click points; double-click or right-click to finish."
            )
        else:
            self.statusLabel.setText(
                f"{tool_name.capitalize()}: drag on the image."
            )

    def _deactivate_tool(self):
        self._clear_preview()
        self.current_tool = None
        self.viewer.mask_drawing_enabled = False
        self.start_xy = None
        self.current_xy = None
        self.polygon_points = []
        self.last_pencil_xy = None

        for button in self.tool_buttons:
            button.setChecked(False)

        self.statusLabel.setText("Drawing disabled.")

    # ------------------------------------------------------------------
    # Mouse handling
    # ------------------------------------------------------------------

    @staticmethod
    def _event_xy(event):
        if event.xdata is None or event.ydata is None:
            return None

        return float(event.xdata), float(event.ydata)

    def _inside_image(self, x, y):
        if self.mask is None:
            return False

        h, w = self.mask.shape
        return 0 <= x < w and 0 <= y < h

    def _clip_xy(self, x, y):
        if self.mask is None:
            return x, y

        h, w = self.mask.shape
        return (
            float(np.clip(x, 0, w - 1)),
            float(np.clip(y, 0, h - 1)),
        )

    def _find_polygon_vertex(self, event, points):
        """Return the index of a nearby vertex, or None."""
        if not points or event.x is None or event.y is None:
            return None

        ax = self.viewer.axes
        click_xy = np.array([event.x, event.y], dtype=float)

        # Compare in screen pixels so hit-testing works at different zooms.
        vertices_px = ax.transData.transform(np.asarray(points, dtype=float))
        distances = np.linalg.norm(vertices_px - click_xy, axis=1)

        idx = int(np.argmin(distances))
        return idx if distances[idx] <= 9 else None


    def _redraw_edited_polygon(self):
        """Rebuild the mask using the edited polygon vertices."""
        if self.completed_polygon_base is None:
            return

        self.mask = self.completed_polygon_base.copy()
        self.polygon_points = list(self.completed_polygon)

        self._apply_polygon()

        self.polygon_points = []
        self._redraw_mask_overlay()

        # Show the edited polygon's vertices again.
        self.polygon_points = list(self.completed_polygon)
        self._draw_polygon_preview()
        self.polygon_points = []
    
    # ------------------------------------------------------------------
    # Editable completed masks
    # ------------------------------------------------------------------

    def _require_cv2(self):
        if cv2 is None:
            raise RuntimeError(
                "Interactive mask translation requires OpenCV (cv2). "
                "Install opencv-python in the MuscleX environment."
            )

    def _shape_mask(self, shape):
        """Rasterize one editable geometric shape from its vertices."""
        if self.mask is None:
            return None

        h, w = self.mask.shape
        result = np.zeros((h, w), dtype=bool)
        kind = shape.get("type")
        vertices = np.asarray(shape.get("vertices", []), dtype=float)

        if len(vertices) < 3:
            return result

        if kind == "polygon":
            pts = np.round(vertices).astype(np.int32)
            tmp = np.zeros((h, w), dtype=np.uint8)
            cv2.fillPoly(tmp, [pts], 1)
            return tmp.astype(bool)

        if kind in ("rectangle", "oval") and len(vertices) >= 4:
            xs = vertices[:, 0]
            ys = vertices[:, 1]
            x1, x2 = float(xs.min()), float(xs.max())
            y1, y2 = float(ys.min()), float(ys.max())

            xmin = max(0, int(np.floor(x1)))
            xmax = min(w - 1, int(np.ceil(x2)))
            ymin = max(0, int(np.floor(y1)))
            ymax = min(h - 1, int(np.ceil(y2)))
            if xmin > xmax or ymin > ymax:
                return result

            if kind == "rectangle":
                result[ymin:ymax + 1, xmin:xmax + 1] = True
                return result

            # Oval: use the exact bounding box represented by the four
            # vertices.  This avoids the old behavior where vertex edits
            # could produce an inconsistent ellipse.
            yy, xx = np.ogrid[ymin:ymax + 1, xmin:xmax + 1]
            cx = (x1 + x2) / 2.0
            cy = (y1 + y2) / 2.0
            rx = max((x2 - x1) / 2.0, 0.5)
            ry = max((y2 - y1) / 2.0, 0.5)
            result[ymin:ymax + 1, xmin:xmax + 1] = (
                ((xx - cx) / rx) ** 2 + ((yy - cy) / ry) ** 2 <= 1.0
            )
            return result

        return result

    def _add_editable_shape(self, shape):
        """Register one completed mask as an independent editable object."""
        shape = {
            "id": self.next_shape_id,
            "type": shape.get("type"),
            "vertices": [tuple(p) for p in shape.get("vertices", [])],
            # Pixels erased from this particular mask are kept separately
            # from the geometry so rebuilding/moving another mask cannot
            # accidentally restore them.
            "erased_mask": np.zeros_like(self.mask, dtype=bool),
        }
        self.next_shape_id += 1
        self.editable_shapes.append(shape)
        self.selected_shape_index = len(self.editable_shapes) - 1
        self._draw_selected_shape()

    def _shape_vertices(self, shape):
        """Return the editable vertices stored by the shape."""
        return [tuple(p) for p in shape.get("vertices", [])]

    def _set_shape_vertices(self, shape, points):
        """Set vertices, preserving rectangle/oval as a true 4-corner box."""
        points = [tuple(self._clip_xy(float(x), float(y))) for x, y in points]

        if shape.get("type") in ("rectangle", "oval") and len(points) >= 4:
            xs = [p[0] for p in points]
            ys = [p[1] for p in points]
            xmin, xmax = min(xs), max(xs)
            ymin, ymax = min(ys), max(ys)
            shape["vertices"] = [
                (xmin, ymin),
                (xmax, ymin),
                (xmax, ymax),
                (xmin, ymax),
            ]
        else:
            shape["vertices"] = points

    def _update_shape_vertex(self, shape, vertex_index, xy):
        """Move one vertex; rectangles/ovals keep an axis-aligned box."""
        points = self._shape_vertices(shape)
        if not (0 <= vertex_index < len(points)):
            return

        if shape.get("type") in ("rectangle", "oval") and len(points) == 4:
            # Opposite corner stays fixed. Reconstruct all four corners so
            # rectangle/oval geometry remains valid after every drag.
            opposite = points[(vertex_index + 2) % 4]
            moved = self._clip_xy(*xy)
            x1, y1 = moved
            x2, y2 = opposite
            self._set_shape_vertices(shape, [
                (min(x1, x2), min(y1, y2)),
                (max(x1, x2), min(y1, y2)),
                (max(x1, x2), max(y1, y2)),
                (min(x1, x2), max(y1, y2)),
            ])
        else:
            points[vertex_index] = self._clip_xy(*xy)
            self._set_shape_vertices(shape, points)

    def _clear_edit_overlay(self):
        """Remove the yellow selected-shape line/vertex overlay."""
        if self.edit_preview_artist is not None:
            try:
                self.edit_preview_artist.remove()
            except (ValueError, AttributeError):
                pass
            self.edit_preview_artist = None

    def _shape_visible_mask(self, shape):
        """Return a shape's raster mask after its persistent erasures."""
        shape_mask = self._shape_mask(shape)
        if shape_mask is None:
            return None

        erased = shape.get("erased_mask")
        if erased is not None and erased.shape == shape_mask.shape:
            shape_mask = shape_mask & ~erased
        return shape_mask

    def _shape_hit(self, x, y, tolerance=10.0):
        """Return (index, mode, vertex_index) for the top-most shape hit."""
        if not self.editable_shapes:
            return None

        ax = self.viewer.axes
        click_px = ax.transData.transform((x, y))

        for index in range(len(self.editable_shapes) - 1, -1, -1):
            shape = self.editable_shapes[index]
            points = self._shape_vertices(shape)

            # Vertex hit testing is done in screen pixels, not data units, so
            # the handles remain equally easy to select at different zooms.
            if points:
                vertices_px = ax.transData.transform(np.asarray(points, dtype=float))
                distances = np.linalg.norm(vertices_px - click_px, axis=1)
                vertex_index = int(np.argmin(distances))
                if distances[vertex_index] <= tolerance:
                    return index, "vertex", vertex_index

            shape_mask = self._shape_visible_mask(shape)
            iy, ix = int(round(y)), int(round(x))
            if (
                shape_mask is not None
                and 0 <= iy < shape_mask.shape[0]
                and 0 <= ix < shape_mask.shape[1]
                and shape_mask[iy, ix]
            ):
                return index, "translate", None

        return None

    def _draw_selected_shape(self):
        """Draw handles for the selected geometric mask."""
        self._clear_edit_overlay()

        if (
            not self.edit_mode
            or self.selected_shape_index is None
            or not (0 <= self.selected_shape_index < len(self.editable_shapes))
        ):
            self.viewer.canvas.draw_idle()
            return

        shape = self.editable_shapes[self.selected_shape_index]
        points = self._shape_vertices(shape)
        if not points:
            self.viewer.canvas.draw_idle()
            return

        ax = self.viewer.axes
        xs = [p[0] for p in points]
        ys = [p[1] for p in points]

        if shape["type"] == "polygon":
            xs = xs + [xs[0]]
            ys = ys + [ys[0]]

        (line,) = ax.plot(
            xs, ys,
            linewidth=1.5,
            color="yellow",
            marker="o",
            markersize=6,
            markerfacecolor="yellow",
            markeredgecolor="black",
            zorder=30,
        )
        self.edit_preview_artist = line
        self.viewer.canvas.draw_idle()

    def _rebuild_mask_from_shapes(self):
        """Rebuild geometric masks without losing persistent eraser edits."""
        if self.mask is None:
            return

        # Start from the non-geometric/raster contribution captured when the
        # current edit began.  Then rebuild every geometric mask using its
        # own persistent erased_mask.  This is the key point: an erasure is
        # not stored only in self.mask, so editing another shape cannot
        # recreate the erased pixels.
        if self.edit_base_mask is not None:
            self.mask = self.edit_base_mask.copy()
        elif self.edit_original_mask is not None:
            self.mask = self.edit_original_mask.copy()

        # The base mask can contain the old geometric contribution. Remove
        # all current geometric shapes before rebuilding them from their
        # definitions.  This also prevents a moved shape from being painted
        # twice.
        for shape in self.editable_shapes:
            shape_mask = self._shape_mask(shape)
            if shape_mask is not None:
                self.mask[shape_mask] = False

        for shape in self.editable_shapes:
            shape_mask = self._shape_visible_mask(shape)
            if shape_mask is not None:
                self.mask |= shape_mask

        self._redraw_mask_overlay()
        self._draw_selected_shape()

    def _translate_selected_shape(self, dx, dy):
        """Translate the selected shape by updating its geometric vertices."""
        if self.selected_shape_index is None:
            return

        shape = self.editable_shapes[self.selected_shape_index]

        points = [
            self._clip_xy(float(x + dx), float(y + dy))
            for x, y in shape.get("vertices", [])
        ]
        self._set_shape_vertices(shape, points)

        # Erased pixels belong to the mask being moved, so move that mask's
        # erasure history along with the geometry.  This keeps holes attached
        # to their original mask while leaving erasures on other masks alone.
        erased = shape.get("erased_mask")
        if erased is not None and np.any(erased):
            self._require_cv2()
            matrix = np.float32([[1, 0, dx], [0, 1, dy]])
            shifted = cv2.warpAffine(
                erased.astype(np.uint8),
                matrix,
                (erased.shape[1], erased.shape[0]),
                flags=cv2.INTER_NEAREST,
                borderMode=cv2.BORDER_CONSTANT,
                borderValue=0,
            )
            shape["erased_mask"] = shifted.astype(bool)

        self._rebuild_mask_from_shapes()

    def _enter_edit_mode(self, index):
        self._clear_preview()
        self.edit_mode = True
        self.selected_shape_index = index
        self.edit_drag_mode = None
        self.edit_vertex_index = None
        shape = self.editable_shapes[index]
        self.edit_original_shape = {
            "id": shape.get("id"),
            "type": shape["type"],
            "vertices": [tuple(p) for p in shape.get("vertices", [])],
            "erased_mask": (
                shape.get("erased_mask").copy()
                if shape.get("erased_mask") is not None
                else np.zeros_like(self.mask, dtype=bool)
            ),
        }
        self.edit_original_mask = self.mask.copy()
        selected_mask = self._shape_visible_mask(self.editable_shapes[index])
        if selected_mask is not None:
            self.edit_base_mask = self.mask.copy()
            self.edit_base_mask[selected_mask] = False
        else:
            self.edit_base_mask = self.mask.copy()

        # The edit itself is undoable as one operation.
        self._push_undo()

        self.viewer.mask_drawing_enabled = True
        self.statusLabel.setText(
            "Mask selected: drag vertices to reshape, drag inside to move, "
            "right-click to save the edit."
        )
        self._draw_selected_shape()

    def _commit_edit(self):
        if not self.edit_mode:
            return

        self.edit_original_shape = None
        self.edit_original_mask = None
        self.edit_base_mask = None
        self.edit_drag_mode = None
        self.edit_vertex_index = None
        self.edit_start_xy = None
        self.edit_mode = False

        # Saving an edit also ends the mask-selection state.  This prevents
        # the edited vertices/handles from remaining visible and prevents the
        # next click from continuing to edit the same mask.
        self.selected_shape_index = None
        self.viewer.mask_drawing_enabled = False

        # Deselect the drawing tool button as well.
        self.current_tool = None
        self.start_xy = None
        self.current_xy = None
        self.polygon_points = []
        self.last_pencil_xy = None
        for button in self.tool_buttons:
            button.setChecked(False)

        self._clear_edit_overlay()
        self._clear_preview()
        self.viewer.canvas.draw_idle()
        self.statusLabel.setText("Mask edit saved. Mask tool deselected.")

    def _cancel_edit(self):
        if not self.edit_mode:
            return

        if self.edit_original_mask is not None:
            self.mask = self.edit_original_mask.copy()
        if self.edit_original_shape is not None:
            original_id = self.edit_original_shape.get("id")
            restored_shape = {
                "type": self.edit_original_shape["type"],
                "vertices": [
                    tuple(p) for p in self.edit_original_shape["vertices"]
                ],
                "erased_mask": self.edit_original_shape.get("erased_mask", np.zeros_like(self.mask, dtype=bool)).copy(),
            }
            if original_id is not None:
                restored_shape["id"] = original_id
            self.editable_shapes[self.selected_shape_index] = restored_shape

        self.edit_original_shape = None
        self.edit_original_mask = None
        self.edit_base_mask = None
        self.edit_drag_mode = None
        self.edit_vertex_index = None
        self.edit_start_xy = None
        self.edit_mode = False
        self.viewer.mask_drawing_enabled = False
        self._clear_edit_overlay()
        self._redraw_mask_overlay()
        self.statusLabel.setText("Mask edit cancelled.")

    def _mouse_pressed(self, event):
        if self.mask is None:
            return

        xy = self._event_xy(event)
        if xy is None:
            return

        x, y = xy

        # Right-click commits the current edit.  If no edit is active,
        # preserve the existing polygon right-click-to-finish behavior.
        if event.button == 3:
            if self.edit_mode:
                self._commit_edit()
            elif self.current_tool == "polygon":
                self._finish_polygon()
            return

        # Double-click selects an existing completed geometric mask.
        # It works without first choosing a drawing tool.
        if getattr(event, "dblclick", False) and event.button == 1:
            hit = self._shape_hit(x, y)
            if hit is not None:
                index, mode, vertex_index = hit
                self._enter_edit_mode(index)
                if mode == "vertex":
                    self.edit_drag_mode = "vertex"
                    self.edit_vertex_index = vertex_index
                    self.edit_start_xy = (x, y)
                else:
                    self.edit_drag_mode = "translate"
                    self.edit_start_xy = (x, y)
                return

        # Once a mask is selected, a normal left-drag edits it.
        if self.edit_mode:
            if event.button != 1:
                return

            shape = self.editable_shapes[self.selected_shape_index]
            points = self._shape_vertices(shape)

            vertex_index = self._find_polygon_vertex(event, points)
            if vertex_index is not None:
                self.edit_drag_mode = "vertex"
                self.edit_vertex_index = vertex_index
                self.edit_start_xy = (x, y)
            elif self._shape_hit(x, y) is not None:
                self.edit_drag_mode = "translate"
                self.edit_start_xy = (x, y)
            return

        if self.current_tool is None:
            return

        # # Polygon uses clicks rather than drag.
        # if self.current_tool == "polygon":
        #     if event.button == 3:
        #         self._finish_polygon()
        #         return

        #     if event.button == 1:
        #         self.polygon_points.append((x, y))
        #         self._draw_polygon_preview()
        #     return

        # Polygon supports both vertex dragging and point placement.
        if self.current_tool == "polygon":

            if event.button == 3:
                self._finish_polygon()
                return

            if event.button != 1:
                return

            # 1) Drag a vertex of the polygon currently being drawn.
            idx = self._find_polygon_vertex(event, self.polygon_points)
            if idx is not None:
                self.dragging_vertex = ("drawing", idx)
                return

            # 2) Drag a vertex of the most recently completed polygon.
            if self.completed_polygon is not None:
                idx = self._find_polygon_vertex(
                    event, self.completed_polygon
                )
                if idx is not None:
                    self._push_undo()
                    self.dragging_vertex = ("completed", idx)
                    return

            # 3) Otherwise, add a new polygon point.
            self.polygon_points.append((x, y))
            self._draw_polygon_preview()
            return

        if event.button != 1:
            return

        if not self._inside_image(x, y):
            return

        self._push_undo()

        self.start_xy = (x, y)
        self.current_xy = (x, y)

        if self.current_tool in ("pencil", "eraser"):
            self.last_pencil_xy = (x, y)
            self._draw_brush_segment(
                x,
                y,
                x,
                y,
                erase=self.current_tool == "eraser",
            )

    def _mouse_moved(self, event):
        if self.mask is None:
            return

        xy = self._event_xy(event)
        if xy is None:
            return

        x, y = xy

        if self.edit_mode and self.edit_drag_mode is not None:
            shape = self.editable_shapes[self.selected_shape_index]
            if self.edit_start_xy is None:
                self.edit_start_xy = (x, y)

            if self.edit_drag_mode == "vertex":
                self._update_shape_vertex(
                    shape, self.edit_vertex_index, (x, y)
                )
                self._rebuild_mask_from_shapes()

            elif self.edit_drag_mode == "translate":
                dx = x - self.edit_start_xy[0]
                dy = y - self.edit_start_xy[1]
                self._translate_selected_shape(dx, dy)
                self.edit_start_xy = (x, y)

            return

        if self.current_tool is None:
            return

        xy = self._event_xy(event)
        if xy is None:
            return

        x, y = xy

        # if self.current_tool == "polygon":
        #     if self.polygon_points:
        #         self.current_xy = (x, y)
        #         self._draw_polygon_preview()
        #     return

        if self.current_tool == "polygon":

            # Dragging a vertex while drawing a polygon.
            if self.dragging_vertex is not None:
                mode, idx = self.dragging_vertex
                new_xy = self._clip_xy(x, y)

                if mode == "drawing":
                    self.polygon_points[idx] = new_xy
                    self._draw_polygon_preview()

                elif mode == "completed":
                    self.completed_polygon[idx] = new_xy
                    self._redraw_edited_polygon()

                return

            # Normal polygon preview while adding points.
            if self.polygon_points:
                self.current_xy = (x, y)
                self._draw_polygon_preview()

            return

        if self.start_xy is None:
            return

        self.current_xy = self._clip_xy(x, y)

        if self.current_tool in ("pencil", "eraser"):
            if self.last_pencil_xy is not None:
                self._draw_brush_segment(
                    self.last_pencil_xy[0],
                    self.last_pencil_xy[1],
                    self.current_xy[0],
                    self.current_xy[1],
                    erase=self.current_tool == "eraser",
                )

            self.last_pencil_xy = self.current_xy
            self._redraw_mask_overlay()
        else:
            self._draw_shape_preview()

    def _mouse_released(self, event):
        if self.edit_mode:
            if event.button == 1:
                self.edit_drag_mode = None
                self.edit_vertex_index = None
                self.edit_start_xy = None
                self._draw_selected_shape()
            return

        # Finish vertex dragging on mouse release.
        if self.current_tool == "polygon" and self.dragging_vertex is not None:
            self.dragging_vertex = None
            self.current_xy = None
            return
        if self.mask is None or self.current_tool is None:
            return

        if self.current_tool == "polygon":
            # Right-click is handled on press.
            return

        if event.button != 1 or self.start_xy is None:
            return

        xy = self._event_xy(event)
        if xy is not None:
            self.current_xy = self._clip_xy(*xy)

        if self.current_tool == "rectangle":
            self._apply_rectangle(self.start_xy, self.current_xy)
            x1, y1 = self.start_xy
            x2, y2 = self.current_xy
            self._add_editable_shape({
                "type": "rectangle",
                "vertices": [
                    (x1, y1), (x2, y1),
                    (x2, y2), (x1, y2),
                ],
            })

        elif self.current_tool == "oval":
            self._apply_oval(self.start_xy, self.current_xy)
            x1, y1 = self.start_xy
            x2, y2 = self.current_xy
            self._add_editable_shape({
                "type": "oval",
                "vertices": [
                    (x1, y1), (x2, y1),
                    (x2, y2), (x1, y2),
                ],
            })

        # Pencil/eraser are modified continuously during movement.

        self._clear_preview()
        self._redraw_mask_overlay()

        self.start_xy = None
        self.current_xy = None
        self.last_pencil_xy = None

    # ------------------------------------------------------------------
    # Raster drawing
    # ------------------------------------------------------------------

    def _copy_editable_shapes(self):
        """Deep-copy editable geometric mask definitions."""
        return [
            {
                "id": shape.get("id"),
                "type": shape["type"],
                "vertices": [tuple(p) for p in shape.get("vertices", [])],
                "erased_mask": (
                    shape.get("erased_mask").copy()
                    if shape.get("erased_mask") is not None
                    else np.zeros_like(self.mask, dtype=bool)
                ),
            }
            for shape in self.editable_shapes
        ]

    def _sync_next_shape_id(self):
        """Keep the next mask ID higher than every currently stored ID."""
        ids = [
            shape.get("id")
            for shape in self.editable_shapes
            if isinstance(shape.get("id"), int)
        ]
        self.next_shape_id = max(ids, default=-1) + 1

    def _push_undo(self):
        if self.mask is None:
            return

        self.undo_stack.append(
            (self.mask.copy(), self._copy_editable_shapes())
        )

        # Avoid an unbounded memory footprint.
        if len(self.undo_stack) > 30:
            self.undo_stack.pop(0)

        self.redo_stack.clear()

    def _apply_rectangle(self, p1, p2):
        if self.mask is None:
            return

        x1, y1 = p1
        x2, y2 = p2

        xmin = max(0, int(np.floor(min(x1, x2))))
        xmax = min(self.mask.shape[1] - 1, int(np.ceil(max(x1, x2))))
        ymin = max(0, int(np.floor(min(y1, y2))))
        ymax = min(self.mask.shape[0] - 1, int(np.ceil(max(y1, y2))))

        if xmin <= xmax and ymin <= ymax:
            self.mask[ymin : ymax + 1, xmin : xmax + 1] = True

    def _apply_oval(self, p1, p2):
        if self.mask is None:
            return

        x1, y1 = p1
        x2, y2 = p2

        xmin = max(0, int(np.floor(min(x1, x2))))
        xmax = min(self.mask.shape[1] - 1, int(np.ceil(max(x1, x2))))
        ymin = max(0, int(np.floor(min(y1, y2))))
        ymax = min(self.mask.shape[0] - 1, int(np.ceil(max(y1, y2))))

        if xmin > xmax or ymin > ymax:
            return

        yy, xx = np.ogrid[ymin : ymax + 1, xmin : xmax + 1]

        cx = (x1 + x2) / 2.0
        cy = (y1 + y2) / 2.0
        rx = max(abs(x2 - x1) / 2.0, 0.5)
        ry = max(abs(y2 - y1) / 2.0, 0.5)

        ellipse = ((xx - cx) / rx) ** 2 + ((yy - cy) / ry) ** 2 <= 1.0
        self.mask[ymin : ymax + 1, xmin : xmax + 1] |= ellipse

    def _apply_polygon(self):
        if self.mask is None or len(self.polygon_points) < 3:
            return

        points = np.asarray(self.polygon_points, dtype=float)

        xmin = max(0, int(np.floor(points[:, 0].min())))
        xmax = min(self.mask.shape[1] - 1, int(np.ceil(points[:, 0].max())))
        ymin = max(0, int(np.floor(points[:, 1].min())))
        ymax = min(self.mask.shape[0] - 1, int(np.ceil(points[:, 1].max())))

        if xmin > xmax or ymin > ymax:
            return

        yy, xx = np.mgrid[ymin : ymax + 1, xmin : xmax + 1]

        coords = np.column_stack((xx.ravel(), yy.ravel()))
        inside = MplPath(points).contains_points(coords)

        region = inside.reshape(xx.shape)
        self.mask[ymin : ymax + 1, xmin : xmax + 1] |= region

    def _draw_brush_segment(self, x1, y1, x2, y2, erase=False):
        if self.mask is None:
            return

        h, w = self.mask.shape

        radius = max(0.5, self.brush_size / 2.0)

        length = max(abs(x2 - x1), abs(y2 - y1))
        steps = max(1, int(np.ceil(length)))

        for t in np.linspace(0.0, 1.0, steps + 1):
            x = x1 + t * (x2 - x1)
            y = y1 + t * (y2 - y1)

            xmin = max(0, int(np.floor(x - radius)))
            xmax = min(w - 1, int(np.ceil(x + radius)))
            ymin = max(0, int(np.floor(y - radius)))
            ymax = min(h - 1, int(np.ceil(y + radius)))

            if xmin > xmax or ymin > ymax:
                continue

            yy, xx = np.ogrid[ymin : ymax + 1, xmin : xmax + 1]
            disk = (xx - x) ** 2 + (yy - y) ** 2 <= radius ** 2

            if erase:
                # Keep the erasure attached to every geometric mask touched
                # by the brush.  self.mask alone is not enough because a
                # later shape edit rebuilds self.mask from editable_shapes.
                for shape in self.editable_shapes:
                    shape_mask = self._shape_mask(shape)
                    if shape_mask is None:
                        continue
                    local_shape = shape_mask[ymin : ymax + 1, xmin : xmax + 1]
                    if not np.any(local_shape & disk):
                        continue
                    erased_mask = shape.get("erased_mask")
                    if erased_mask is None or erased_mask.shape != self.mask.shape:
                        erased_mask = np.zeros_like(self.mask, dtype=bool)
                        shape["erased_mask"] = erased_mask
                    erased_mask[ymin : ymax + 1, xmin : xmax + 1][disk & local_shape] = True

                self.mask[ymin : ymax + 1, xmin : xmax + 1][disk] = False
            else:
                self.mask[ymin : ymax + 1, xmin : xmax + 1][disk] = True

    # ------------------------------------------------------------------
    # Polygon / shape preview
    # ------------------------------------------------------------------

    def _draw_shape_preview(self):
        self._clear_preview()

        if self.start_xy is None or self.current_xy is None:
            return

        x1, y1 = self.start_xy
        x2, y2 = self.current_xy

        ax = self.viewer.axes

        if self.current_tool == "rectangle":
            from matplotlib.patches import Rectangle

            patch = Rectangle(
                (min(x1, x2), min(y1, y2)),
                abs(x2 - x1),
                abs(y2 - y1),
                fill=False,
                linewidth=1.5,
                edgecolor="red",
            )
            ax.add_patch(patch)
            self.preview_artist = patch

        elif self.current_tool == "oval":
            from matplotlib.patches import Ellipse

            patch = Ellipse(
                ((x1 + x2) / 2.0, (y1 + y2) / 2.0),
                abs(x2 - x1),
                abs(y2 - y1),
                fill=False,
                linewidth=1.5,
                edgecolor="red",
            )
            ax.add_patch(patch)
            self.preview_artist = patch

        self.viewer.canvas.draw_idle()

    def _draw_polygon_preview(self):
        self._clear_preview()

        if not self.polygon_points:
            return

        points = list(self.polygon_points)

        if self.current_xy is not None:
            points.append(self.current_xy)

        if len(points) < 2:
            return

        ax = self.viewer.axes
        xs = [p[0] for p in points]
        ys = [p[1] for p in points]

        (line,) = ax.plot(
            xs,
            ys,
            linewidth=1.5,
            color="red",
            marker="o",
            markersize=3,
        )

        self.preview_artist = line
        self.viewer.canvas.draw_idle()

    def _finish_polygon(self):
        if self.current_tool != "polygon":
            return

        if len(self.polygon_points) < 3:
            self.polygon_points = []
            self._clear_preview()
            return

        # self._push_undo()
        # self._apply_polygon()

        # self.polygon_points = []
        # self.current_xy = None
# Save the mask before applying this polygon.
        self._push_undo()
        self.completed_polygon_base = self.mask.copy()
        self.completed_polygon = list(self.polygon_points)

        self._apply_polygon()

        self._add_editable_shape({
            "type": "polygon",
            "vertices": [tuple(p) for p in self.polygon_points],
        })

        self.polygon_points = []
        self.current_xy = None

        self._clear_preview()
        self._redraw_mask_overlay()

        self.statusLabel.setText(
            "Polygon applied. Continue drawing or select another tool."
        )

    def _clear_preview(self):
        if self.preview_artist is not None:
            try:
                self.preview_artist.remove()
            except (ValueError, AttributeError):
                pass

        self.preview_artist = None
        self.viewer.canvas.draw_idle()

    # ------------------------------------------------------------------
    # Mask display
    # ------------------------------------------------------------------

    def _redraw_mask_overlay(self):
        if self.mask is None:
            return

        if self.mask_artist is not None:
            try:
                self.mask_artist.remove()
            except (ValueError, AttributeError):
                pass
            self.mask_artist = None

        if np.any(self.mask):
            # Transparent for False, visible red overlay for True.
            cmap = ListedColormap(
                [
                    (0.0, 0.0, 0.0, 0.0),
                    (1.0, 0.0, 0.0, self.mask_alpha),
                ]
            )
            self.mask_artist = self.viewer.axes.imshow(
                self.mask,
                cmap=cmap,
                interpolation="nearest",
                origin="upper",
                zorder=10,
            )

        # Threshold overlays are independent of the user's editable red mask.
        for attr in ("low_threshold_artist", "high_threshold_artist"):
            artist = getattr(self, attr, None)
            if artist is not None:
                try:
                    artist.remove()
                except (ValueError, AttributeError):
                    pass
                setattr(self, attr, None)

        for mask, color, attr in (
            (self.low_threshold_mask, (0.0, 1.0, 0.0, 0.38), "low_threshold_artist"),
            (self.high_threshold_mask, (0.0, 0.35, 1.0, 0.38), "high_threshold_artist"),
        ):
            if mask is None:
                continue
            rgba = np.zeros((*mask.shape, 4), dtype=float)
            rgba[~mask] = color
            if np.any(~mask):
                setattr(self, attr, self.viewer.axes.imshow(
                    rgba, interpolation="nearest", origin="upper", zorder=8
                ))

        self.viewer.canvas.draw_idle()

    # ------------------------------------------------------------------
    # Undo / redo / clear
    # ------------------------------------------------------------------

    def undo(self):
        if self.mask is None or not self.undo_stack:
            return

        self.redo_stack.append(
            (self.mask.copy(), self._copy_editable_shapes())
        )
        state = self.undo_stack.pop()
        if isinstance(state, tuple):
            self.mask, self.editable_shapes = state
            self._sync_next_shape_id()
        else:
            # Backward compatibility with any legacy in-memory undo entries.
            self.mask = state
            self.editable_shapes = []

        self.selected_shape_index = None
        self.edit_mode = False
        self.edit_original_shape = None
        self.edit_original_mask = None
        self.edit_base_mask = None

        self._clear_preview()
        self._redraw_mask_overlay()

    def redo(self):
        if self.mask is None or not self.redo_stack:
            return

        self.undo_stack.append(
            (self.mask.copy(), self._copy_editable_shapes())
        )
        state = self.redo_stack.pop()
        if isinstance(state, tuple):
            self.mask, self.editable_shapes = state
            self._sync_next_shape_id()
        else:
            self.mask = state
            self.editable_shapes = []

        self.selected_shape_index = None
        self.edit_mode = False
        self.edit_original_shape = None
        self.edit_original_mask = None
        self.edit_base_mask = None

        self._clear_preview()
        self._redraw_mask_overlay()

    def clear_mask(self):
        """Clear the selected editable mask, or all masks if none is selected."""
        if self.mask is None:
            return

        # If a completed geometric mask is selected, Clear means delete that
        # shape and its mask contribution -- not every mask in the image.
        if (
            self.selected_shape_index is not None
            and 0 <= self.selected_shape_index < len(self.editable_shapes)
        ):
            index = self.selected_shape_index
            self._push_undo()

            # During edit mode edit_base_mask is the image with the selected
            # shape removed. Rebuild from that base so overlapping shapes are
            # preserved correctly.
            if self.edit_mode and self.edit_base_mask is not None:
                new_mask = self.edit_base_mask.copy()
            else:
                selected_mask = self._shape_visible_mask(self.editable_shapes[index])
                new_mask = self.mask.copy()
                if selected_mask is not None:
                    new_mask[selected_mask] = False

                # Preserve the raster contribution of all remaining editable
                # shapes, including overlaps with the deleted shape.
                for other_index, shape in enumerate(self.editable_shapes):
                    if other_index == index:
                        continue
                    other_mask = self._shape_visible_mask(shape)
                    if other_mask is not None:
                        new_mask |= other_mask

            self.mask = new_mask
            self.editable_shapes.pop(index)
            self.selected_shape_index = None
            self.edit_mode = False
            self.edit_drag_mode = None
            self.edit_vertex_index = None
            self.edit_start_xy = None
            self.edit_original_shape = None
            self.edit_original_mask = None
            self.edit_base_mask = None
            self.viewer.mask_drawing_enabled = self.current_tool is not None

            self._clear_preview()
            self._clear_edit_overlay()
            self._redraw_mask_overlay()
            self.statusLabel.setText("Selected mask cleared. Other masks were kept.")
            return

        # No shape is selected: retain the original Clear All behavior.
        if not np.any(self.mask):
            return

        self._push_undo()
        self.mask[:] = False
        self.editable_shapes = []
        self.next_shape_id = 0
        self.selected_shape_index = None
        self.edit_mode = False
        self.edit_drag_mode = None
        self.edit_vertex_index = None
        self.edit_start_xy = None
        self.edit_original_shape = None
        self.edit_original_mask = None
        self.edit_base_mask = None

        self._clear_preview()
        self._clear_edit_overlay()
        self._redraw_mask_overlay()
        self.statusLabel.setText("All masks cleared.")

    def _brush_size_changed(self, value):
        self.brush_size = int(value)

    # ------------------------------------------------------------------
    # Save / load
    # ------------------------------------------------------------------

    def _mask_state_path(self, mask_path):
        """Return the sidecar JSON path used to persist editable geometry."""
        return os.path.splitext(mask_path)[0] + ".json"

    @staticmethod
    def _encode_bool_mask(mask):
        """Encode a boolean mask compactly for the JSON geometry sidecar."""
        packed = np.packbits(np.asarray(mask, dtype=np.uint8).ravel())
        return base64.b64encode(zlib.compress(packed.tobytes(), level=6)).decode("ascii")

    @staticmethod
    def _decode_bool_mask(encoded, shape):
        """Decode a boolean mask stored by _encode_bool_mask()."""
        raw = zlib.decompress(base64.b64decode(encoded.encode("ascii")))
        packed = np.frombuffer(raw, dtype=np.uint8)
        values = np.unpackbits(packed)[: int(np.prod(shape))]
        return values.reshape(tuple(shape)).astype(bool)

    def _serialize_editable_shapes(self):
        """Serialize the current editable geometry, including persistent erasures."""
        shapes = []
        mask_shape = list(self.mask.shape) if self.mask is not None else None

        for shape in self.editable_shapes:
            item = {
                "id": shape.get("id"),
                "type": shape.get("type"),
                "vertices": [
                    [float(point[0]), float(point[1])]
                    for point in shape.get("vertices", [])
                ],
            }

            erased = shape.get("erased_mask")
            if erased is not None and mask_shape is not None:
                if erased.shape != self.mask.shape:
                    raise ValueError(
                        f"Invalid erased mask shape {erased.shape}; "
                        f"expected {self.mask.shape}."
                    )
                if np.any(erased):
                    item["erased_mask"] = self._encode_bool_mask(erased)
                else:
                    item["erased_mask"] = None
            else:
                item["erased_mask"] = None

            shapes.append(item)

        return shapes

    def _save_editable_geometry(self, mask_path):
        """Save editable shape geometry next to the raster mask."""
        state_path = self._mask_state_path(mask_path)
        state = {
            "version": 1,
            "mask_file": os.path.basename(mask_path),
            "image_shape": list(self.mask.shape),
            "next_shape_id": int(self.next_shape_id),
            "shapes": self._serialize_editable_shapes(),
        }

        with open(state_path, "w", encoding="utf-8") as handle:
            json.dump(state, handle, indent=2)

        return state_path

    def _load_editable_geometry(self, mask_path):
        """Restore editable shape geometry from the PNG's JSON sidecar.

        Returns True when geometry was restored, False when no sidecar exists.
        """
        state_path = self._mask_state_path(mask_path)
        if not os.path.exists(state_path):
            return False

        with open(state_path, "r", encoding="utf-8") as handle:
            state = json.load(handle)

        if state.get("version") != 1:
            raise ValueError(
                f"Unsupported mask geometry version: {state.get('version')}"
            )

        saved_shape = tuple(state.get("image_shape", ()))
        if saved_shape != tuple(self.mask.shape):
            raise ValueError(
                f"Saved geometry shape {saved_shape} does not match "
                f"image shape {self.mask.shape}."
            )

        restored = []
        for raw_shape in state.get("shapes", []):
            shape = {
                "id": raw_shape.get("id"),
                "type": raw_shape["type"],
                "vertices": [
                    (float(point[0]), float(point[1]))
                    for point in raw_shape.get("vertices", [])
                ],
            }

            encoded_erased = raw_shape.get("erased_mask")
            if encoded_erased:
                shape["erased_mask"] = self._decode_bool_mask(
                    encoded_erased, self.mask.shape
                )
            else:
                shape["erased_mask"] = np.zeros_like(self.mask, dtype=bool)

            restored.append(shape)

        self.editable_shapes = restored
        self._sync_next_shape_id()

        # Preserve a larger next ID if it was explicitly saved.
        saved_next_id = state.get("next_shape_id")
        if isinstance(saved_next_id, int):
            self.next_shape_id = max(self.next_shape_id, saved_next_id)

        return True

    def save_mask(self):
        """Save the raster mask as PNG and editable geometry as a JSON sidecar."""
        if self.mask is None:
            QMessageBox.warning(self, "Mask Tool", "No image is loaded.")
            return

        path, _ = QFileDialog.getSaveFileName(
            self,
            "Save Mask",
            "filename_mask.png",
            "PNG Mask (*.png)",
        )

        if not path:
            return

        if not path.lower().endswith(".png"):
            path += ".png"

        try:
            # Boolean mask -> 8-bit grayscale PNG:
            # False = 0 (black), True = 255 (white).
            mask_image = self.mask.astype(np.uint8) * 255

            try:
                from imageio.v2 import imwrite
                imwrite(path, mask_image)
            except ImportError:
                from PIL import Image
                Image.fromarray(mask_image, mode="L").save(path)

            state_path = self._save_editable_geometry(path)

            self.statusLabel.setText(
                f"Mask saved: {path} (editable geometry: {os.path.basename(state_path)})"
            )

        except Exception as exc:
            QMessageBox.critical(
                self,
                "Save Mask",
                f"Could not save mask:\n{exc}",
            )

    def load_mask(self):
        """Load a PNG mask and restore editable geometry when its JSON sidecar exists."""
        if self.mask is None:
            QMessageBox.warning(self, "Mask Tool", "No image is loaded.")
            return

        path, _ = QFileDialog.getOpenFileName(
            self,
            "Load Mask",
            "",
            "PNG Mask (*.png)",
        )

        if not path:
            return

        try:
            try:
                from imageio.v2 import imread
                loaded = imread(path)
            except ImportError:
                from PIL import Image
                loaded = np.asarray(Image.open(path))

            loaded = np.asarray(loaded)

            # Support grayscale and RGB/RGBA PNG files. Any non-zero pixel
            # becomes part of the boolean mask.
            if loaded.ndim == 2:
                loaded = loaded > 0
            elif loaded.ndim == 3:
                loaded = np.any(loaded[..., :3] > 0, axis=2)
            else:
                raise ValueError(
                    f"Unsupported PNG dimensions: {loaded.shape}"
                )

            loaded = loaded.astype(bool)

            if loaded.shape != self.mask.shape:
                raise ValueError(
                    f"Mask shape {loaded.shape} does not match "
                    f"image shape {self.mask.shape}."
                )

            self._push_undo()
            self.mask = loaded.copy()

            # Start clean, then restore geometry if this PNG was created by
            # MaskTool's Save Mask action. A plain/legacy PNG remains usable
            # as a raster-only mask.
            self.editable_shapes = []
            self.next_shape_id = 0
            self.selected_shape_index = None
            self.edit_mode = False
            self.edit_drag_mode = None
            self.edit_vertex_index = None
            self.edit_start_xy = None
            self.edit_original_shape = None
            self.edit_original_mask = None
            self.edit_base_mask = None

            geometry_restored = self._load_editable_geometry(path)

            self._clear_preview()
            self._clear_edit_overlay()
            self._redraw_mask_overlay()

            if geometry_restored:
                self.statusLabel.setText(
                    f"Mask loaded: {path} — editable masks restored."
                )
            else:
                self.statusLabel.setText(
                    f"Mask loaded: {path} — raster mask only (no geometry sidecar found)."
                )

        except Exception as exc:
            QMessageBox.critical(
                self,
                "Load Mask",
                f"Could not load mask:\n{exc}",
            )


# ----------------------------------------------------------------------
# Optional standalone test
# ----------------------------------------------------------------------

if __name__ == "__main__":
    import sys

    app = QApplication(sys.argv)

    # Optional test image.
    # Replace this with your MuscleX image when integrating.
    test_image = np.random.random((512, 512))

    window = MaskTool(image=test_image)
    window.show()

    sys.exit(app.exec())