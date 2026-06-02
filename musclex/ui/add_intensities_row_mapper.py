"""Row-mapping strategies for AddIntensities windows.

Both AddIntensitiesSingleExp and AddIntensitiesMultipleExp need to translate
a table row index into an image name, a FileManager index, and vice versa.
The mapping rules differ:

* **SingleRowMapper** – identity mapping where ``row == fm_index`` and the
  image name comes from a plain list (``img_list``).
* **CartesianRowMapper** – reads the FileManager index from a
  ``Qt.UserRole`` value stored in a designated table column (Cartesian
  product of experiments x indices).

Both conform to the ``RowMapper`` protocol so consuming code can be written
once against the protocol regardless of which window it lives in.
"""

from __future__ import annotations

import os
from typing import TYPE_CHECKING, Callable, Optional, Protocol

from PySide6.QtCore import Qt
from PySide6.QtGui import QBrush, QColor
from PySide6.QtWidgets import QTableWidgetItem

if TYPE_CHECKING:
    from PySide6.QtWidgets import QTableWidget
    from musclex.ui.widgets.image_alignment_table import ColKey


class RowMapper(Protocol):
    """Minimal protocol for mapping table rows to image identities."""

    def name_for_row(self, row: int) -> Optional[str]: ...
    def fm_index_for_row(self, row: int) -> Optional[int]: ...
    def row_for_fm_index(self, fm_idx: int) -> Optional[int]: ...
    def row_count(self) -> int: ...
    def populate_table(self, table: QTableWidget) -> None: ...


class SingleRowMapper:
    """Identity mapping: ``row == fm_index``, names from *img_list*.

    The list is accessed through a callable so the mapper always sees the
    latest contents even when the list is replaced after folder load.
    """

    def __init__(self, get_img_list: Callable[[], list]):
        self._get = get_img_list

    def name_for_row(self, row: int) -> Optional[str]:
        lst = self._get()
        return lst[row] if 0 <= row < len(lst) else None

    def fm_index_for_row(self, row: int) -> Optional[int]:
        return row

    def row_for_fm_index(self, fm_idx: int) -> Optional[int]:
        return fm_idx

    def row_count(self) -> int:
        return len(self._get())

    def populate_table(self, table) -> None:
        """Fill the table with one row per image in the list."""
        from musclex.ui.widgets.image_alignment_table import ColKey

        names = self._get()
        table.setRowCount(len(names))
        for i, name in enumerate(names):
            item = QTableWidgetItem(os.path.basename(name))
            item.setToolTip(name)
            table.setItem(i, table.col(ColKey.FRAME), item)


class CartesianRowMapper:
    """Reads the FM index from ``Qt.UserRole`` in a table column.

    Used by AddIntensitiesMultipleExp where each table row stores its
    absolute FileManager index in ``COL_INDEX``.
    """

    _GROUP_BG = QColor(100, 149, 237)   # cornflower blue
    _GROUP_FG = QColor(255, 255, 255)

    def __init__(self, table: QTableWidget, col_index: int,
                 col_exp: int, workspace):
        self._table = table
        self._col_index = col_index
        self._col_exp = col_exp
        self._workspace = workspace
        self._exp_dirs: list = []

    def set_sources(self, exp_dirs: list) -> None:
        """Store the experiment directory paths used to build rows."""
        self._exp_dirs = list(exp_dirs)

    def fm_index_for_row(self, row: int) -> Optional[int]:
        item = self._table.item(row, self._col_index)
        if item is None:
            return None
        val = item.data(Qt.UserRole)
        return val if isinstance(val, int) else None

    def name_for_row(self, row: int) -> Optional[str]:
        fm = self._workspace.navigator.file_manager
        if fm is None:
            return None
        fm_idx = self.fm_index_for_row(row)
        if fm_idx is None or fm_idx >= len(fm.names):
            return None
        return fm.names[fm_idx]

    def row_for_fm_index(self, fm_idx: int) -> Optional[int]:
        for row in range(self._table.rowCount()):
            if self.fm_index_for_row(row) == fm_idx:
                return row
        return None

    def row_count(self) -> int:
        return self._table.rowCount()

    def populate_table(self, table) -> None:
        """Build the Cartesian product of experiments x indices.

        Each row's COL_INDEX item stores the absolute FileManager index as
        Qt.UserRole.  After filling, two stable sorts are applied (EXP then
        INDEX) to achieve "primary sort by INDEX, secondary by EXP".
        ``rebuild_spans`` is called at the end to merge cells.
        """
        from musclex.ui.widgets.image_alignment_table import ColKey

        fm = self._workspace.navigator.file_manager
        if fm is None or not self._exp_dirs:
            return

        index_map = {
            dp: fm.source_index_map[dp]
            for dp in self._exp_dirs
            if dp in (fm.source_index_map or {})
        }
        if not index_map:
            return

        ranges = list(index_map.values())
        n_indices = min(end - start + 1 for start, end in ranges)
        n_exps = len([dp for dp in self._exp_dirs if dp in index_map])
        total_rows = n_indices * n_exps
        table.setSortingEnabled(False)
        table.setRowCount(total_rows)

        row = 0
        for dp in self._exp_dirs:
            if dp not in index_map:
                continue
            start, _ = index_map[dp]
            if fm.source_labels and start < len(fm.source_labels):
                exp_label = fm.source_labels[start]
            else:
                exp_label = os.path.basename(dp.rstrip('/\\'))
            for idx in range(n_indices):
                fm_idx = start + idx
                name = fm.names[fm_idx]

                idx_item = QTableWidgetItem()
                idx_item.setData(Qt.DisplayRole, idx + 1)
                idx_item.setTextAlignment(Qt.AlignCenter)
                idx_item.setData(Qt.UserRole, fm_idx)
                table.setItem(row, self._col_index, idx_item)

                exp_item = QTableWidgetItem(exp_label)
                exp_item.setToolTip(name)
                table.setItem(row, self._col_exp, exp_item)

                frame_item = QTableWidgetItem(os.path.basename(name))
                frame_item.setToolTip(name)
                table.setItem(row, table.col(ColKey.FRAME), frame_item)

                row += 1

        table.sortItems(self._col_exp, Qt.AscendingOrder)
        table.sortItems(self._col_index, Qt.AscendingOrder)
        self.rebuild_spans(table, self._col_index)

    def rebuild_spans(self, table, span_col: int) -> None:
        """Merge consecutive rows that share the same value in *span_col*.

        Resets any existing spans on COL_INDEX and COL_EXP first, then scans
        *span_col* and applies ``setSpan`` + blue styling to each run of
        equal values.
        """
        for row in range(table.rowCount()):
            for c in (self._col_index, self._col_exp):
                if table.rowSpan(row, c) > 1:
                    table.setSpan(row, c, 1, 1)
            item = table.item(row, self._col_index)
            if item:
                item.setBackground(QBrush())
                item.setForeground(QBrush())
            item_exp = table.item(row, self._col_exp)
            if item_exp:
                item_exp.setBackground(QBrush())
                item_exp.setForeground(QBrush())

        i = 0
        while i < table.rowCount():
            item = table.item(i, span_col)
            if item is None:
                i += 1
                continue
            val = item.text()
            j = i + 1
            while j < table.rowCount():
                jitem = table.item(j, span_col)
                if jitem is None or jitem.text() != val:
                    break
                j += 1
            if j - i > 1:
                table.setSpan(i, span_col, j - i, 1)
            span_item = table.item(i, span_col)
            if span_item:
                span_item.setBackground(QBrush(self._GROUP_BG))
                span_item.setForeground(QBrush(self._GROUP_FG))
            i = j
