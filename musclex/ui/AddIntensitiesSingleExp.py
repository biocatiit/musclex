import os
import matplotlib.patches as mpatches
from PySide6.QtWidgets import (
    QMainWindow, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QHeaderView, QAbstractItemView, QLabel, QProgressBar,
    QSizePolicy, QMenu, QRadioButton, QSpinBox, QWidget, QSplitter,
    QScrollArea, QFrame, QStackedWidget, QCheckBox, QGroupBox
)
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QBrush, QFont
from musclex import __version__
from musclex.ui.widgets import ProcessingWorkspace
from musclex.ui.GlobalSettingsDialog import GlobalSettingsDialog


class AddIntensitiesSingleExp(QMainWindow):

    # Column indices
    COL_GROUP = 0
    COL_FRAME = 1
    COL_CENTER = 2
    COL_CENTER_MODE = 3
    COL_CENTER_DIST = 4
    COL_ROTATION = 5
    COL_ROTATION_MODE = 6
    COL_DEVIATION = 7
    COL_IMAGE_DIFF = 8

    HEADERS = [
        "Group",
        "Frame",
        "Original Center",
        "Center Mode",
        "distance",
        "Rotation",
        "Rotation Mode",
        "Deviation",
        "Image Difference",
    ]

    # Visual style for group cells
    _GROUP_BG = QColor(100, 149, 237)   # cornflower blue
    _GROUP_FG = QColor(255, 255, 255)

    def __init__(self):
        super().__init__()
        self.setWindowTitle("Muscle X Add Intensities Single Experiment v." + __version__)
        self.workspace = ProcessingWorkspace(settings_dir="")
        self.img_list = []
        self.misaligned_names = set()
        self._current_center = None
        self._base_image_filename = None
        self._navigating_from_table = False  # guard against re-entrant navigation

        # Each entry: {'start': int, 'count': int, 'number': int}
        self._groups = []

        self._build_ui()
        self.resize(1400, 800)
        self.show()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        central = QWidget()
        self.setCentralWidget(central)
        root = QVBoxLayout(central)
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
        self.table.itemSelectionChanged.connect(self._on_table_selection_changed)

        # Left side of splitter: select_panel (before load) / table (after load)
        self._left_stack = QStackedWidget()
        self._left_stack.addWidget(self.workspace.navigator.select_panel)  # index 0
        self._left_stack.addWidget(self.table)                              # index 1
        self._left_stack.setCurrentIndex(0)
        self.workspace.navigator.fileLoaded.connect(self._on_folder_loaded)
        self.workspace.navigator.scanComplete.connect(self._on_scan_complete)
        self.workspace.imageDataReady.connect(self._on_image_data_ready)

        # Right panel container (global settings group box + scrollable panel)
        right_container = QWidget()
        right_container_layout = QVBoxLayout(right_container)
        right_container_layout.setContentsMargins(0, 0, 0, 0)
        right_container_layout.setSpacing(4)

        # Global settings group box
        self.global_settings_group = QGroupBox("Global Settings")
        global_settings_layout = QHBoxLayout(self.global_settings_group)
        global_settings_layout.setContentsMargins(8, 6, 8, 6)
        self.global_settings_btn = QPushButton("Global Settings")
        global_settings_layout.addWidget(self.global_settings_btn)
        global_settings_layout.addStretch()
        right_container_layout.addWidget(self.global_settings_group)

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
        self.right_panel.setWidget(self._right_panel_content)
        right_container_layout.addWidget(self.right_panel)

        self.splitter = QSplitter(Qt.Horizontal)
        self.splitter.addWidget(self._left_stack)
        self.splitter.addWidget(right_container)
        self.splitter.setStretchFactor(0, 1)
        self.splitter.setStretchFactor(1, 0)
        self.splitter.setSizes([900, 500])
        right_container.setMinimumWidth(400)
        root.addWidget(self.splitter)

        self.image_viewer = self.workspace.navigator.image_viewer
        self._right_panel_layout.addWidget(self.image_viewer)
        self._right_panel_layout.addWidget(self.image_viewer.display_panel)

        self.centerChkBx = QCheckBox("Center")
        self.centerChkBx.setChecked(False)
        self.image_viewer.display_panel.add_to_top_slot(self.centerChkBx)
        self.centerChkBx.stateChanged.connect(self._redraw_overlays)



        self.start_detection_btn = QPushButton("Start Detection")
        self._right_panel_layout.addWidget(self.start_detection_btn)

        # Grouping mode selector
        self.radio_manual = QRadioButton("Select Group Manually")
        self.radio_manual.setChecked(True)
        self._right_panel_layout.addWidget(self.radio_manual)

        self.radio_bin_images = QRadioButton("Bin Images")
        self._right_panel_layout.addWidget(self.radio_bin_images)

        # Binning factor row (shown only when Bin Images is selected)
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
        self._right_panel_layout.addWidget(self._binning_row)
        self._right_panel_layout.addStretch()

        self.radio_bin_images.toggled.connect(self._binning_row.setVisible)

        self.global_settings_btn.clicked.connect(self._open_global_settings)

    # ------------------------------------------------------------------
    # Global settings dialog
    # ------------------------------------------------------------------

    def _open_global_settings(self):
        image_data = self.workspace._current_image_data
        if image_data is None:
            return
        dlg = GlobalSettingsDialog(self, self.workspace, image_data)
        dlg.globalBaseChanged.connect(self._on_global_base_changed)
        dlg.exec()

    def _on_global_base_changed(self):
        base = self.workspace.settings_manager.get_global_base()
        self._base_image_filename = base.get('base_image')
        self._update_table_data()

    # ------------------------------------------------------------------
    # Folder loaded → switch to table view
    # ------------------------------------------------------------------

    def _on_folder_loaded(self, dir_path):
        """Switch to table view with the initial file list (scan may still be running)."""
        self.img_list = list(self.workspace.navigator.file_manager.names)
        self.misaligned_names = set()
        self._init_table()
        self._sync_table_selection()
        self._left_stack.setCurrentIndex(1)

    def _on_scan_complete(self):
        """Refresh the file list once the background scan finishes."""
        self.img_list = list(self.workspace.navigator.file_manager.names)
        self._init_table()
        self._sync_table_selection()

    # ------------------------------------------------------------------
    # Data population
    # ------------------------------------------------------------------

    def _init_table(self):
        """Fully rebuild the table from img_list (resets rows and groups)."""
        self._groups = []
        self.table.setRowCount(0)
        self.table.setRowCount(len(self.img_list))
        for row, name in enumerate(self.img_list):
            item = QTableWidgetItem(os.path.basename(name))
            item.setToolTip(name)
            self.table.setItem(row, self.COL_FRAME, item)
            self._fill_center_columns(row, name)
            self._fill_rotation_columns(row, name)
            self._apply_misaligned_highlight(row, name)
            self._apply_base_marker(row, name)

    def _update_table_data(self):
        """Refresh center/rotation data columns for all existing rows (preserves selection and groups)."""
        for row, name in enumerate(self.img_list):
            if row >= self.table.rowCount():
                break
            self._fill_center_columns(row, name)
            self._fill_rotation_columns(row, name)
            self._apply_misaligned_highlight(row, name)
            self._apply_base_marker(row, name)

    def _fill_center_columns(self, row, name):
        """Fill COL_CENTER and COL_CENTER_MODE from workspace settings manager."""
        sm = self.workspace.settings_manager
        base = os.path.basename(name)
        key = base if sm.has_manual_center(base) else name

        manual = sm.get_center(key)
        if manual is not None:
            cx, cy = manual
            self.table.setItem(row, self.COL_CENTER,
                               QTableWidgetItem(f"({cx:.1f}, {cy:.1f})"))
            self.table.setItem(row, self.COL_CENTER_MODE,
                               QTableWidgetItem("Manual"))
            return

        auto = sm.get_auto_center(key) or sm.get_auto_center(base)
        if auto is not None:
            cx, cy = auto
            self.table.setItem(row, self.COL_CENTER,
                               QTableWidgetItem(f"({cx:.1f}, {cy:.1f})"))
            self.table.setItem(row, self.COL_CENTER_MODE,
                               QTableWidgetItem("Auto"))
        else:
            self.table.setItem(row, self.COL_CENTER, QTableWidgetItem(""))
            self.table.setItem(row, self.COL_CENTER_MODE, QTableWidgetItem(""))

    def _fill_rotation_columns(self, row, name):
        """Fill COL_ROTATION and COL_ROTATION_MODE from workspace settings manager."""
        sm = self.workspace.settings_manager
        base = os.path.basename(name)
        key = base if sm.has_manual_rotation(base) else name

        manual = sm.get_rotation(key)
        if manual is not None:
            self.table.setItem(row, self.COL_ROTATION,
                               QTableWidgetItem(f"{manual:.2f}°"))
            self.table.setItem(row, self.COL_ROTATION_MODE,
                               QTableWidgetItem("Manual"))
            return

        auto = sm.get_auto_rotation(key) or sm.get_auto_rotation(base)
        if auto is not None:
            self.table.setItem(row, self.COL_ROTATION,
                               QTableWidgetItem(f"{auto:.2f}°"))
            self.table.setItem(row, self.COL_ROTATION_MODE,
                               QTableWidgetItem("Auto"))
        else:
            self.table.setItem(row, self.COL_ROTATION, QTableWidgetItem(""))
            self.table.setItem(row, self.COL_ROTATION_MODE, QTableWidgetItem(""))

        self.table.setItem(row, self.COL_DEVIATION, QTableWidgetItem(""))

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

    def _apply_base_marker(self, row, name):
        """Prefix the Frame cell with a star if this image is the global base."""
        item = self.table.item(row, self.COL_FRAME)
        if item is None:
            return
        base = os.path.basename(name)
        base_name = self._base_image_filename or ''
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
    # Public helpers
    # ------------------------------------------------------------------

    def setMisalignedNames(self, misaligned_names):
        self.misaligned_names = set(misaligned_names)
        self._init_table()

    # ------------------------------------------------------------------
    # Table selection → navigate to image
    # ------------------------------------------------------------------

    def _on_table_selection_changed(self):
        """Navigate navigator to the image corresponding to the selected table row."""
        if self._navigating_from_table:
            return
        selected = self.table.selectedItems()
        if not selected:
            return
        row = self.table.currentRow()
        if row < 0 or row >= len(self.img_list):
            return
        self._navigating_from_table = True
        try:
            self.workspace.navigator.switch_to_image_by_index(row)
        finally:
            self._navigating_from_table = False

    # ------------------------------------------------------------------
    # Image data ready → display image and refresh table
    # ------------------------------------------------------------------

    def _on_image_data_ready(self, image_data):
        """Called when workspace has loaded and configured a new image."""
        self.image_viewer.display_image(image_data.img)
        self._current_center = image_data.center
        self._current_rotation = image_data.rotation
        self._redraw_overlays()
        self._update_table_data()

    def _sync_table_selection(self):
        """Highlight the table row that matches the navigator's current image index."""
        idx = self.workspace.navigator.current_index
        if 0 <= idx < self.table.rowCount():
            self.table.blockSignals(True)
            self.table.selectRow(idx)
            self.table.scrollTo(self.table.model().index(idx, 0))
            self.table.blockSignals(False)

    def _redraw_overlays(self):
        """Draw center circle if checkbox is checked; clear it otherwise."""
        ax = self.image_viewer.axes
        for patch in list(ax.patches):
            patch.remove()
        if self.centerChkBx.isChecked() and self._current_center is not None:
            cx, cy = self._current_center
            circle = mpatches.Circle((cx, cy), 10, color='g', fill=False, linewidth=1.5)
            ax.add_patch(circle)
        self.image_viewer.canvas.draw_idle()

    def setStatus(self, text):
        self.statusLabel.setText(text)
        self.statusLabel.setVisible(bool(text))
