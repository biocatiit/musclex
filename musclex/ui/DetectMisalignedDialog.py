import os
from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QHeaderView, QAbstractItemView, QLabel, QProgressBar,
    QSizePolicy, QMenu, QRadioButton, QSpinBox, QWidget, QSplitter, QScrollArea, QFrame
)
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QBrush


class DetectMisalignedDialog(QDialog):
    """
    Dialog that lists all images from the file manager in a table.
    Columns: Group, Frame, Original Center, Center Mode Distance,
             Rotation, Rotation Mode Distance, Image Difference.
    Rows for misaligned images are highlighted in red when detection data
    is provided.

    Grouping: drag-select multiple rows, right-click → "Group" to assign a
    sequential group number shown as a merged cell in the Group column.
    Right-click on a group number cell → "Ungroup" to remove the group and
    renumber the remaining groups to stay sequential.
    """

    # Column indices
    COL_GROUP = 0
    COL_FRAME = 1
    COL_CENTER = 2
    COL_CENTER_MODE = 3
    COL_CENTER_DIST = 4
    COL_ROTATION = 5
    COL_ROTATION_MODE = 6
    COL_IMAGE_DIFF = 7

    HEADERS = [
        "Group",
        "Frame",
        "Original Center",
        "Center Mode",
        "distance",
        "Rotation",
        "Rotation Mode",
        "Image Difference",
    ]

    # Visual style for group cells
    _GROUP_BG = QColor(100, 149, 237)   # cornflower blue
    _GROUP_FG = QColor(255, 255, 255)

    def __init__(self, img_list, dir_path="", misaligned_names=None,
                 items_data=None, parent=None):
        """
        Parameters
        ----------
        img_list : list[str]
            All image file names (base names for tiff, full names for hdf5).
        dir_path : str
            Directory path (used for display only, not loading).
        misaligned_names : set[str] | None
            Names of images flagged as misaligned; highlighted in red.
        items_data : list[dict] | None
            Per-image metric dicts with keys:
            'name', 'center', 'center_dist', 'angle', 'angle_dist', 'image_diff'
            If None, only the Frame column is populated.
        parent : QWidget | None
        """
        super().__init__(parent)
        self.setWindowTitle("Detect Misaligned Images")
        self.img_list = img_list or []
        self.dir_path = dir_path
        self.misaligned_names = set(misaligned_names) if misaligned_names else set()
        self.items_data = items_data

        # Each entry: {'start': int, 'count': int, 'number': int}
        self._groups = []

        self._build_ui()
        self.resize(1400, 800)

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setContentsMargins(8, 8, 8, 8)
        root.setSpacing(6)

        # Status label
        self.statusLabel = QLabel("")
        self.statusLabel.setVisible(False)
        root.addWidget(self.statusLabel)

        # Progress bar (hidden until needed)
        self.progressBar = QProgressBar()
        self.progressBar.setVisible(False)
        root.addWidget(self.progressBar)

        # Table
        self.table = QTableWidget()
        self.table.setColumnCount(len(self.HEADERS))
        self.table.setHorizontalHeaderLabels(self.HEADERS)
        self.table.setSelectionBehavior(QAbstractItemView.SelectRows)
        self.table.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self.table.setAlternatingRowColors(True)
        self.table.verticalHeader().setDefaultSectionSize(22)
        self.table.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        header = self.table.horizontalHeader()
        header.setSectionResizeMode(self.COL_GROUP, QHeaderView.Fixed)
        self.table.setColumnWidth(self.COL_GROUP, 52)
        header.setSectionResizeMode(self.COL_FRAME, QHeaderView.Stretch)
        for col in range(2, len(self.HEADERS)):
            header.setSectionResizeMode(col, QHeaderView.ResizeToContents)

        # Context menu for grouping / ungrouping
        self.table.setContextMenuPolicy(Qt.CustomContextMenu)
        self.table.customContextMenuRequested.connect(self._on_context_menu)

        # Right panel (plain scrollable panel)
        self.right_panel = QScrollArea()
        self.right_panel.setWidgetResizable(True)
        self.right_panel.setFrameShape(QFrame.NoFrame)
        self.right_panel.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.right_panel.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)

        self._right_panel_content = QWidget()
        self._right_panel_layout = QVBoxLayout(self._right_panel_content)
        self._right_panel_layout.setContentsMargins(6, 6, 6, 6)
        self._right_panel_layout.setSpacing(6)
        self._right_panel_layout.addStretch()
        self.right_panel.setWidget(self._right_panel_content)

        self.splitter = QSplitter(Qt.Horizontal)
        self.splitter.addWidget(self.table)
        self.splitter.addWidget(self.right_panel)
        self.splitter.setStretchFactor(0, 1)
        self.splitter.setStretchFactor(1, 0)
        self.splitter.setSizes([1100, 300])
        root.addWidget(self.splitter)

        self.start_detection_btn = QPushButton("Start Detection")
        self._right_panel_layout.insertWidget(self._right_panel_layout.count() - 1, self.start_detection_btn)

        # Grouping mode selector
        self.radio_manual = QRadioButton("Select Group Manually")
        self.radio_manual.setChecked(True)
        self._right_panel_layout.insertWidget(self._right_panel_layout.count() - 1, self.radio_manual)

        self.radio_same_frame = QRadioButton("Same Frame Number")
        self._right_panel_layout.insertWidget(self._right_panel_layout.count() - 1, self.radio_same_frame)

        # Binning factor row (shown only when Same Frame Number is selected)
        self._binning_row = QWidget()
        binning_layout = QHBoxLayout(self._binning_row)
        binning_layout.setContentsMargins(16, 0, 0, 0)
        binning_layout.setSpacing(6)
        binning_layout.addWidget(QLabel("Binning factor:"))
        self.binning_spin = QSpinBox()
        self.binning_spin.setMinimum(1)
        self.binning_spin.setMaximum(256)
        self.binning_spin.setValue(1)
        binning_layout.addWidget(self.binning_spin)
        self._binning_row.setVisible(False)
        self._right_panel_layout.insertWidget(self._right_panel_layout.count() - 1, self._binning_row)

        self.radio_same_frame.toggled.connect(self._binning_row.setVisible)

        # Buttons
        btn_layout = QHBoxLayout()
        self.closeButton = QPushButton("Close")
        btn_layout.addStretch()
        btn_layout.addWidget(self.closeButton)
        root.addLayout(btn_layout)

        self.closeButton.clicked.connect(self.accept)

        # Populate with whatever data is already available
        self.populate()

    # ------------------------------------------------------------------
    # Data population
    # ------------------------------------------------------------------

    def populate(self):
        """Fill the table from img_list / items_data."""
        self._groups = []
        self.table.setRowCount(0)

        if self.items_data:
            self._populate_from_items_data()
        else:
            self._populate_names_only()

    def _populate_names_only(self):
        """Populate only the Frame column from img_list."""
        self.table.setRowCount(len(self.img_list))
        for row, name in enumerate(self.img_list):
            item = QTableWidgetItem(os.path.basename(name))
            item.setToolTip(name)
            self.table.setItem(row, self.COL_FRAME, item)
            self._apply_misaligned_highlight(row, name)

    def _populate_from_items_data(self):
        """Populate all columns from items_data dicts."""
        self.table.setRowCount(len(self.items_data))
        for row, d in enumerate(self.items_data):
            name = d.get("name", "")
            base = os.path.basename(name)

            def cell(text):
                return QTableWidgetItem(str(text))

            frame_item = QTableWidgetItem(base)
            frame_item.setToolTip(name)
            self.table.setItem(row, self.COL_FRAME, frame_item)

            cx, cy = d.get("center", (None, None))
            center_text = f"({cx:.1f}, {cy:.1f})" if cx is not None else ""
            self.table.setItem(row, self.COL_CENTER, cell(center_text))

            self.table.setItem(row, self.COL_CENTER_MODE,
                               cell(d.get("center_mode", "")))

            cd = d.get("center_dist")
            self.table.setItem(row, self.COL_CENTER_DIST,
                               cell(f"{cd:.4f}" if cd is not None else ""))

            ang = d.get("angle")
            self.table.setItem(row, self.COL_ROTATION,
                               cell(f"{ang:.4f}" if ang is not None else ""))

            self.table.setItem(row, self.COL_ROTATION_MODE,
                               cell(d.get("rotation_mode", "")))

            img_diff = d.get("image_diff")
            self.table.setItem(row, self.COL_IMAGE_DIFF,
                               cell(f"{img_diff:.4f}" if img_diff is not None else ""))

            self._apply_misaligned_highlight(row, name)

    def _apply_misaligned_highlight(self, row, name):
        """Colour the data columns red if the image is in misaligned_names.
        COL_GROUP is intentionally skipped to keep group cell appearance intact."""
        if not self.misaligned_names:
            return
        base = os.path.basename(name)
        if name in self.misaligned_names or base in self.misaligned_names:
            highlight = QBrush(QColor(255, 120, 120))
            for col in range(1, self.table.columnCount()):   # skip COL_GROUP
                item = self.table.item(row, col)
                if item is None:
                    item = QTableWidgetItem("")
                    self.table.setItem(row, col, item)
                item.setBackground(highlight)

    # ------------------------------------------------------------------
    # Grouping helpers
    # ------------------------------------------------------------------

    def _find_group_at_row(self, row):
        """Return the group dict that contains *row*, or None."""
        for group in self._groups:
            if group['start'] <= row < group['start'] + group['count']:
                return group
        return None

    def _render_groups(self):
        """Clear all group-column spans then re-render from self._groups."""
        # Reset every cell in the Group column to 1×1 span and clear text/colour
        for row in range(self.table.rowCount()):
            self.table.setSpan(row, self.COL_GROUP, 1, 1)
            item = self.table.item(row, self.COL_GROUP)
            if item:
                item.setText("")
                item.setBackground(QBrush())
                item.setForeground(QBrush())

        # Paint each group
        for group in self._groups:
            start = group['start']
            count = group['count']
            if count > 1:
                self.table.setSpan(start, self.COL_GROUP, count, 1)
            item = self.table.item(start, self.COL_GROUP)
            if item is None:
                item = QTableWidgetItem()
                self.table.setItem(start, self.COL_GROUP, item)
            item.setText(str(group['number']))
            item.setTextAlignment(Qt.AlignCenter)
            item.setBackground(QBrush(self._GROUP_BG))
            item.setForeground(QBrush(self._GROUP_FG))

    def _renumber_groups(self):
        """Sort groups by row position and assign sequential numbers (1, 2, 3…),
        then re-render."""
        self._groups.sort(key=lambda g: g['start'])
        for i, group in enumerate(self._groups):
            group['number'] = i + 1
        self._render_groups()

    def _group_rows(self, selected_rows):
        """Create a new group covering min…max of *selected_rows*.
        Any existing groups that overlap the new range are removed first."""
        start = min(selected_rows)
        end = max(selected_rows)
        count = end - start + 1

        # Remove overlapping groups
        self._groups = [
            g for g in self._groups
            if not (g['start'] <= end and g['start'] + g['count'] > start)
        ]

        self._groups.append({'start': start, 'count': count, 'number': 0})
        self._renumber_groups()

    def _ungroup(self, group):
        """Remove *group* and renumber remaining groups."""
        self._groups.remove(group)
        self._renumber_groups()

    # ------------------------------------------------------------------
    # Context menu
    # ------------------------------------------------------------------

    def _on_context_menu(self, pos):
        row = self.table.rowAt(pos.y())
        col = self.table.columnAt(pos.x())
        if row < 0:
            return

        global_pos = self.table.viewport().mapToGlobal(pos)
        menu = QMenu(self)

        # Right-click on the Group column: offer Ungroup if the row is grouped
        if col == self.COL_GROUP:
            group = self._find_group_at_row(row)
            if group is not None:
                ungroup_act = menu.addAction(f"Ungroup  (Group {group['number']})")
                chosen = menu.exec(global_pos)
                if chosen == ungroup_act:
                    self._ungroup(group)
            return

        # Right-click elsewhere: offer Group when ≥2 rows are selected
        selected_rows = sorted(set(idx.row() for idx in self.table.selectedIndexes()))
        if len(selected_rows) >= 2:
            group_act = menu.addAction("Group")
            chosen = menu.exec(global_pos)
            if chosen == group_act:
                self._group_rows(selected_rows)

    # ------------------------------------------------------------------
    # Public helpers for updating the dialog after detection
    # ------------------------------------------------------------------

    def setMisalignedNames(self, misaligned_names):
        self.misaligned_names = set(misaligned_names)
        self.populate()

    def setItemsData(self, items_data):
        self.items_data = items_data
        self.populate()

    def setStatus(self, text):
        self.statusLabel.setText(text)
        self.statusLabel.setVisible(bool(text))
