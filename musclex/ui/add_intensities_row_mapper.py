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

if TYPE_CHECKING:
    from PySide6.QtWidgets import QTableWidget


class RowMapper(Protocol):
    """Minimal protocol for mapping table rows to image identities."""

    def name_for_row(self, row: int) -> Optional[str]: ...
    def fm_index_for_row(self, row: int) -> Optional[int]: ...
    def row_for_fm_index(self, fm_idx: int) -> Optional[int]: ...
    def row_count(self) -> int: ...


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


class CartesianRowMapper:
    """Reads the FM index from ``Qt.UserRole`` in a table column.

    Used by AddIntensitiesMultipleExp where each table row stores its
    absolute FileManager index in ``COL_INDEX``.
    """

    def __init__(self, table: QTableWidget, col_index: int, workspace):
        self._table = table
        self._col_index = col_index
        self._workspace = workspace

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
