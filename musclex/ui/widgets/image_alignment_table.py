import math
import os

from PySide6.QtCore import Qt
from PySide6.QtGui import QBrush, QColor, QFont
from PySide6.QtWidgets import (
    QAbstractItemView, QHeaderView, QSizePolicy, QTableWidget, QTableWidgetItem,
)

from musclex.ui.add_intensities_common import _ElideMiddleDelegate


class ColKey:
    FRAME = 'FRAME'
    CENTER = 'CENTER'
    CENTER_MODE = 'CENTER_MODE'
    CENTER_DIST = 'CENTER_DIST'
    AUTO_CENTER = 'AUTO_CENTER'
    AUTO_MANUAL_DIST = 'AUTO_MANUAL_DIST'
    ROTATION = 'ROTATION'
    ROTATION_MODE = 'ROTATION_MODE'
    ROTATION_DIFF = 'ROTATION_DIFF'
    AUTO_ROTATION = 'AUTO_ROTATION'
    AUTO_ROT_DIFF = 'AUTO_ROT_DIFF'
    SIZE = 'SIZE'
    IMAGE_DIFF = 'IMAGE_DIFF'
    # Sum of per-pixel std-deviation across the 4 quadrants computed before
    # actual folding. Lower is better (perfectly symmetric image -> 0).
    FOLD_STD = 'FOLD_STD'


class ImageAlignmentTable(QTableWidget):
    """Reusable table widget for image alignment data.

    Column layout is passed via *col_map* (``{ColKey: int}``) so the same
    class works for both single-experiment and multi-experiment windows.
    """

    def __init__(self, col_map: dict, headers: list, parent=None):
        super().__init__(parent)
        self._col = col_map

        self.setColumnCount(len(headers))
        self.setHorizontalHeaderLabels(headers)
        self.setSelectionBehavior(QAbstractItemView.SelectRows)
        self.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self.setAlternatingRowColors(True)
        self.verticalHeader().setDefaultSectionSize(22)
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        if ColKey.FRAME in self._col:
            frame_col = self._col[ColKey.FRAME]
            self.setColumnWidth(frame_col, 200)
            self.setItemDelegateForColumn(frame_col, _ElideMiddleDelegate(self))
            header = self.horizontalHeader()
            header.setSectionResizeMode(frame_col, QHeaderView.Interactive)

    def col(self, key):
        """Return the column index for a *ColKey* constant."""
        return self._col[key]

    # ------------------------------------------------------------------
    # Cell-filling methods (accept pre-resolved data)
    # ------------------------------------------------------------------

    def fill_center(self, row, center_tuple, mode_str):
        """Fill the center coordinate and mode columns."""
        c_center = self._col[ColKey.CENTER]
        c_mode = self._col[ColKey.CENTER_MODE]
        if center_tuple is not None and mode_str is not None:
            cx, cy = center_tuple
            self.setItem(row, c_center, QTableWidgetItem(f"({cx:.1f}, {cy:.1f})"))
            self.setItem(row, c_mode, QTableWidgetItem(mode_str))
        else:
            self.setItem(row, c_center, QTableWidgetItem(""))
            self.setItem(row, c_mode, QTableWidgetItem(""))

    def fill_auto_center(self, row, auto_center_tuple):
        """Fill the auto-detected center column."""
        c = self._col[ColKey.AUTO_CENTER]
        if auto_center_tuple is not None:
            cx, cy = auto_center_tuple
            self.setItem(row, c, QTableWidgetItem(f"({cx:.1f}, {cy:.1f})"))
        else:
            self.setItem(row, c, QTableWidgetItem(""))

    def fill_auto_manual_dist(self, row, auto_center, effective_center,
                              dist_thresh_enabled, dist_thresh):
        """Fill auto-vs-effective center distance; highlight if over threshold."""
        c = self._col[ColKey.AUTO_MANUAL_DIST]
        if auto_center is not None and effective_center is not None:
            dx = auto_center[0] - effective_center[0]
            dy = auto_center[1] - effective_center[1]
            dist = math.hypot(dx, dy)
            item = QTableWidgetItem(f"{dist:.2f}")
            if dist_thresh_enabled and dist > dist_thresh:
                item.setBackground(QBrush(QColor(255, 100, 100)))
                item.setForeground(QBrush(QColor(255, 255, 255)))
            self.setItem(row, c, item)
        else:
            self.setItem(row, c, QTableWidgetItem(""))

    def fill_rotation(self, row, rotation_val, mode_str):
        """Fill the rotation value and mode columns."""
        c_rot = self._col[ColKey.ROTATION]
        c_mode = self._col[ColKey.ROTATION_MODE]
        if rotation_val is not None and mode_str is not None:
            self.setItem(row, c_rot, QTableWidgetItem(f"{rotation_val:.2f}°"))
            self.setItem(row, c_mode, QTableWidgetItem(mode_str))
        else:
            self.setItem(row, c_rot, QTableWidgetItem(""))
            self.setItem(row, c_mode, QTableWidgetItem(""))

    def fill_auto_rotation(self, row, auto_rotation_val):
        """Fill the auto-detected rotation column."""
        c = self._col[ColKey.AUTO_ROTATION]
        if auto_rotation_val is not None:
            self.setItem(row, c, QTableWidgetItem(f"{auto_rotation_val:.2f}°"))
        else:
            self.setItem(row, c, QTableWidgetItem(""))

    def fill_auto_rot_diff(self, row, auto_rotation, effective_rotation,
                           rot_thresh_enabled, rot_thresh):
        """Fill auto-vs-effective rotation difference; highlight if over threshold."""
        c = self._col[ColKey.AUTO_ROT_DIFF]
        if auto_rotation is not None and effective_rotation is not None:
            diff = auto_rotation - effective_rotation
            item = QTableWidgetItem(f"{diff:.2f}°")
            if rot_thresh_enabled and abs(diff) > rot_thresh:
                item.setBackground(QBrush(QColor(255, 100, 100)))
                item.setForeground(QBrush(QColor(255, 255, 255)))
            self.setItem(row, c, item)
        else:
            self.setItem(row, c, QTableWidgetItem(""))

    def fill_distance_deviation(self, row, effective_center, effective_rotation,
                                base_center, base_rotation):
        """Fill center-distance and rotation-diff relative to the global base."""
        c_dist = self._col[ColKey.CENTER_DIST]
        c_rot_diff = self._col[ColKey.ROTATION_DIFF]

        if base_center:
            if effective_center is not None:
                dx = effective_center[0] - base_center[0]
                dy = effective_center[1] - base_center[1]
                dist = math.hypot(dx, dy)
                self.setItem(row, c_dist, QTableWidgetItem(f"{dist:.2f}"))
            else:
                self.setItem(row, c_dist, QTableWidgetItem(""))
        else:
            self.setItem(row, c_dist, QTableWidgetItem(""))

        if base_rotation is not None:
            if effective_rotation is not None:
                deviation = effective_rotation - base_rotation
                self.setItem(row, c_rot_diff,
                             QTableWidgetItem(f"{deviation:.2f}°"))
            else:
                self.setItem(row, c_rot_diff, QTableWidgetItem(""))
        else:
            self.setItem(row, c_rot_diff, QTableWidgetItem(""))

    def fill_size(self, row, size_str, most_common_size):
        """Fill the image-size column; highlight if it differs from the mode."""
        c = self._col[ColKey.SIZE]
        item = QTableWidgetItem(size_str)
        if size_str and most_common_size and size_str != most_common_size:
            item.setBackground(QBrush(QColor(255, 100, 100)))
            item.setForeground(QBrush(QColor(255, 255, 255)))
        self.setItem(row, c, item)

    def fill_diff(self, row, diff_val, diff_thresh_enabled, diff_thresh_value):
        """Fill the image-diff column; highlight if above threshold."""
        c = self._col[ColKey.IMAGE_DIFF]
        text = f"{diff_val:.4f}" if diff_val is not None else ""
        item = QTableWidgetItem(text)
        if (diff_val is not None
                and diff_thresh_enabled
                and diff_thresh_value > 0
                and diff_val > diff_thresh_value):
            item.setBackground(QBrush(QColor(255, 100, 100)))
            item.setForeground(QBrush(QColor(255, 255, 255)))
        self.setItem(row, c, item)

    def fill_fold_std(self, row, std_val, thresh_enabled, thresh_value):
        """Fill the FOLD_STD column; highlight if above the symmetry threshold.

        The score is a sum (not a mean) so values can be large; we use the
        general-purpose ``g`` format to keep the column compact while
        preserving precision for tiny scores.
        """
        if ColKey.FOLD_STD not in self._col:
            return
        c = self._col[ColKey.FOLD_STD]
        if std_val is None:
            text = ""
        elif std_val >= 1000:
            text = f"{std_val:.1f}"
        else:
            text = f"{std_val:.4g}"
        item = QTableWidgetItem(text)
        if (std_val is not None
                and thresh_enabled
                and thresh_value > 0
                and std_val > thresh_value):
            item.setBackground(QBrush(QColor(255, 100, 100)))
            item.setForeground(QBrush(QColor(255, 255, 255)))
        self.setItem(row, c, item)

    # ------------------------------------------------------------------
    # Pure visual methods
    # ------------------------------------------------------------------

    def dim_row(self, row):
        """Apply grey foreground to all data columns of a row (visual only)."""
        dim = QBrush(QColor(160, 160, 160))
        for c in range(1, self.columnCount()):
            item = self.item(row, c)
            if item is None:
                item = QTableWidgetItem("")
                self.setItem(row, c, item)
            item.setForeground(dim)

    def apply_threshold_highlighting(self, dist_enabled, dist_thresh,
                                     rot_enabled, rot_thresh,
                                     diff_enabled, diff_thresh,
                                     symmetry_enabled=False, symmetry_thresh=0.0):
        """Re-apply (or clear) red highlighting based on threshold values.

        @param symmetry_enabled: when True the FOLD_STD column is highlighted
            against ``symmetry_thresh``. The argument is optional so existing
            callers (AISE) keep working without symmetry support.
        """
        _red_bg = QBrush(QColor(255, 100, 100))
        _red_fg = QBrush(QColor(255, 255, 255))
        c_dist = self._col[ColKey.AUTO_MANUAL_DIST]
        c_rot = self._col[ColKey.AUTO_ROT_DIFF]
        c_diff = self._col[ColKey.IMAGE_DIFF]
        c_sym = self._col.get(ColKey.FOLD_STD)

        for row in range(self.rowCount()):
            dist_item = self.item(row, c_dist)
            if dist_item and dist_item.text():
                try:
                    val = float(dist_item.text())
                    if dist_enabled and val > dist_thresh:
                        dist_item.setBackground(_red_bg)
                        dist_item.setForeground(_red_fg)
                    else:
                        dist_item.setData(Qt.BackgroundRole, None)
                        dist_item.setData(Qt.ForegroundRole, None)
                except ValueError:
                    pass

            dev_item = self.item(row, c_rot)
            if dev_item and dev_item.text():
                try:
                    val = abs(float(dev_item.text().rstrip("°")))
                    if rot_enabled and val > rot_thresh:
                        dev_item.setBackground(_red_bg)
                        dev_item.setForeground(_red_fg)
                    else:
                        dev_item.setData(Qt.BackgroundRole, None)
                        dev_item.setData(Qt.ForegroundRole, None)
                except ValueError:
                    pass

            diff_item = self.item(row, c_diff)
            if diff_item and diff_item.text():
                try:
                    val = float(diff_item.text())
                    if diff_enabled and diff_thresh > 0 and val > diff_thresh:
                        diff_item.setBackground(_red_bg)
                        diff_item.setForeground(_red_fg)
                    else:
                        diff_item.setData(Qt.BackgroundRole, None)
                        diff_item.setData(Qt.ForegroundRole, None)
                except ValueError:
                    pass

            if c_sym is not None:
                sym_item = self.item(row, c_sym)
                if sym_item and sym_item.text():
                    try:
                        val = float(sym_item.text())
                        if (symmetry_enabled and symmetry_thresh > 0
                                and val > symmetry_thresh):
                            sym_item.setBackground(_red_bg)
                            sym_item.setForeground(_red_fg)
                        else:
                            sym_item.setData(Qt.BackgroundRole, None)
                            sym_item.setData(Qt.ForegroundRole, None)
                    except ValueError:
                        pass

    def apply_misaligned_highlight(self, row, name, misaligned_names,
                                   skip_col=0):
        """Colour data columns red if the image is in *misaligned_names*."""
        if not misaligned_names:
            return
        base = os.path.basename(name)
        if name in misaligned_names or base in misaligned_names:
            highlight = QBrush(QColor(255, 120, 120))
            for c in range(skip_col + 1, self.columnCount()):
                item = self.item(row, c)
                if item is None:
                    item = QTableWidgetItem("")
                    self.setItem(row, c, item)
                item.setBackground(highlight)

    def apply_base_marker(self, row, name, base_image_filename):
        """Prefix the Frame cell with a star if this image is the global base."""
        c_frame = self._col[ColKey.FRAME]
        item = self.item(row, c_frame)
        if item is None:
            return
        base = os.path.basename(name)
        base_name = base_image_filename or ''
        is_base = (base == os.path.basename(base_name)) if base_name else False
        display = os.path.basename(name)
        if is_base:
            item.setText(f"\u2605 {display}")
            bold = QFont()
            bold.setBold(True)
            item.setFont(bold)
        else:
            item.setText(display)
            item.setFont(QFont())
