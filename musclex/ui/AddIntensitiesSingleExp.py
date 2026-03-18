import os
import traceback
import numpy as np
import matplotlib.patches as mpatches
from PySide6.QtWidgets import (
    QMainWindow, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QHeaderView, QAbstractItemView, QLabel, QProgressBar,
    QSizePolicy, QMenu, QRadioButton, QSpinBox, QWidget, QSplitter,
    QScrollArea, QFrame, QStackedWidget, QCheckBox, QGroupBox, QStatusBar,
    QProgressDialog,
)
from PySide6.QtCore import Qt, Signal, QRunnable, QObject, QThreadPool, QTimer
from PySide6.QtGui import QColor, QBrush, QFont
from musclex import __version__
from musclex.ui.widgets import ProcessingWorkspace
from musclex.ui.GlobalSettingsDialog import GlobalSettingsDialog
from musclex.utils.task_manager import ProcessingTaskManager
from musclex.utils.image_processor import rotateImageAboutPoint


def _compute_geometry(args):
    """Top-level function for subprocess: load image, compute center and rotation."""
    dir_path, img_name, loader_spec, manual_center, manual_rotation, orientation_model = args
    try:
        from musclex.utils.file_manager import load_image_via_spec
        from musclex.utils.image_data import ImageData

        img = load_image_via_spec(dir_path, img_name, loader_spec)
        image_data = ImageData(
            img=img, img_path=dir_path, img_name=img_name,
            center=manual_center, rotation=manual_rotation,
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
    COL_SIZE = 8
    COL_TRANSFORM = 9
    COL_IMAGE_DIFF = 10

    HEADERS = [
        "Group",
        "Frame",
        "Original Center",
        "Center Mode",
        "distance",
        "Rotation",
        "Rotation Mode",
        "Deviation",
        "Size",
        "Transform",
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
        self._img_sizes: dict = {}  # img_name -> "WxH" string
        self._current_center = None
        self._current_rotation = None
        self._base_image_filename = None
        self._navigating_from_table = False  # guard against re-entrant navigation

        # Each entry: {'start': int, 'count': int, 'number': int}
        self._groups = []

        # Track rows that have been ignored
        self._ignored_rows: set = set()

        # Background thread pool for single-image geometry (keeps UI responsive)
        self._threadPool = QThreadPool()
        self._threadPool.setMaxThreadCount(1)

        # Debounce timer: delays image loading after selection changes (avoids
        # blocking the main thread on every intermediate row during drag-select)
        self._nav_debounce_timer = QTimer(self)
        self._nav_debounce_timer.setSingleShot(True)
        self._nav_debounce_timer.setInterval(120)
        self._nav_debounce_timer.timeout.connect(self._do_navigate_to_selected_row)

        # Multiprocessing batch detection
        self.taskManager = ProcessingTaskManager()
        self.processExecutor = None
        self._in_batch = False
        self.stop_process = False

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

        # Status bar at the bottom of the window (like EquatorWindow)
        self._statusBar = QStatusBar()
        self.statusLabel = QLabel("")
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self._statusBar.addWidget(self.statusLabel)
        self._statusBar.addPermanentWidget(self.progressBar)
        self.setStatusBar(self._statusBar)

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
        header.setSectionResizeMode(self.COL_FRAME, QHeaderView.ResizeToContents)
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
        self.workspace.needsReprocess.connect(
            lambda: self._on_image_data_ready(self.workspace._current_image_data)
            if self.workspace._current_image_data is not None else None
        )
        # Right panel container (global settings group box + scrollable panel)
        right_container = QWidget()
        right_container_layout = QVBoxLayout(right_container)
        right_container_layout.setContentsMargins(0, 0, 0, 0)
        right_container_layout.setSpacing(4)

        # Misaligned Detection group box
        self.misaligned_detection_group = QGroupBox("Misaligned Detection")
        misaligned_detection_layout = QVBoxLayout(self.misaligned_detection_group)
        misaligned_detection_layout.setContentsMargins(8, 6, 8, 6)
        misaligned_detection_layout.setSpacing(4)

        # Row 1: Global Settings button + live center/rotation readout
        global_row = QHBoxLayout()
        global_row.setSpacing(8)
        self.global_settings_btn = QPushButton("Global Settings")
        global_row.addWidget(self.global_settings_btn)
        self._global_center_label = QLabel("Center: —")
        self._global_center_label.setStyleSheet("font-size: 11px;")
        global_row.addWidget(self._global_center_label)
        self._global_rotation_label = QLabel("Rotation: —")
        self._global_rotation_label.setStyleSheet("font-size: 11px;")
        global_row.addWidget(self._global_rotation_label)
        global_row.addStretch()
        misaligned_detection_layout.addLayout(global_row)

        # Row 2: Start Detection button
        detection_row = QHBoxLayout()
        self.start_detection_btn = QPushButton("Start Detection")
        self.start_detection_btn.setCheckable(True)
        detection_row.addWidget(self.start_detection_btn)
        detection_row.addStretch()
        misaligned_detection_layout.addLayout(detection_row)

        right_container_layout.addWidget(self.misaligned_detection_group)

        # Image viewer placed directly below the misaligned group box (outside scroll area)
        self.image_viewer = self.workspace.navigator.image_viewer
        self.image_viewer.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Preferred)
        self.image_viewer.setMinimumHeight(200)
        right_container_layout.addWidget(self.image_viewer, 0)

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
        right_container_layout.addWidget(self.right_panel, 1)

        self.splitter = QSplitter(Qt.Horizontal)
        self.splitter.addWidget(self._left_stack)
        self.splitter.addWidget(right_container)
        self.splitter.setStretchFactor(0, 1)
        self.splitter.setStretchFactor(1, 0)
        self.splitter.setSizes([900, 500])
        right_container.setMinimumWidth(400)
        root.addWidget(self.splitter)
        self._right_panel_layout.addWidget(self.image_viewer.display_panel)
        self._right_panel_layout.addWidget(self.workspace._center_widget)
        self._right_panel_layout.addWidget(self.workspace._rotation_widget)
        self._right_panel_layout.addWidget(self.workspace._blank_mask_widget)

        self.centerChkBx = QCheckBox("Center")
        self.centerChkBx.setChecked(False)
        self.image_viewer.display_panel.add_to_top_slot(self.centerChkBx)
        self.centerChkBx.stateChanged.connect(self._redraw_overlays)






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
        self.binning_spin.setMinimum(2)
        self.binning_spin.setMaximum(256)
        self.binning_spin.setValue(2)
        binning_layout.addWidget(self.binning_spin)
        self._binning_row.setVisible(False)
        self._right_panel_layout.addWidget(self._binning_row)
        self._right_panel_layout.addStretch()

        self.sum_images_btn = QPushButton("Sum Images")
        self.sum_images_btn.setMinimumHeight(32)
        self._right_panel_layout.addWidget(self.sum_images_btn)

        self.radio_bin_images.toggled.connect(self._binning_row.setVisible)
        self.radio_bin_images.toggled.connect(self._on_bin_images_toggled)
        self.binning_spin.valueChanged.connect(self._on_binning_factor_changed)

        self.global_settings_btn.clicked.connect(self._open_global_settings)
        self.start_detection_btn.toggled.connect(self._on_detection_btn_toggled)

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
        self._sync_global_settings_state()
        self._update_table_data()

    def _sync_global_settings_state(self):
        """Read the saved global base and update _base_image_filename + UI labels."""
        base = self.workspace.settings_manager.get_global_base()
        self._base_image_filename = base.get('base_image')

        center = base.get('center')
        rotation = base.get('rotation')

        if center is not None:
            self._global_center_label.setText(f"Center: ({center[0]:.1f}, {center[1]:.1f})")
        else:
            self._global_center_label.setText("Center: —")

        if rotation is not None:
            self._global_rotation_label.setText(f"Rotation: {rotation:.2f}°")
        else:
            self._global_rotation_label.setText("Rotation: —")

    # ------------------------------------------------------------------
    # Folder loaded → switch to table view
    # ------------------------------------------------------------------

    def _on_folder_loaded(self, dir_path):
        """Switch to table view with the initial file list (scan may still be running)."""
        self.img_list = list(self.workspace.navigator.file_manager.names)
        self.misaligned_names = set()
        self._img_sizes = self.workspace.navigator.file_manager.image_sizes
        self._sync_global_settings_state()
        self._init_table()
        self._sync_table_selection()
        self._left_stack.setCurrentIndex(1)

    def _on_scan_complete(self):
        """Refresh the file list once the background scan finishes."""
        self.img_list = list(self.workspace.navigator.file_manager.names)
        self._img_sizes = self.workspace.navigator.file_manager.image_sizes
        self._sync_global_settings_state()
        self._init_table()
        self._sync_table_selection()

    # ------------------------------------------------------------------
    # Data population
    # ------------------------------------------------------------------

    def _init_table(self):
        """Fully rebuild the table from img_list (resets rows and groups)."""
        self._groups = []
        self._ignored_rows = set()
        self.table.setRowCount(0)
        self.table.setRowCount(len(self.img_list))
        for row, name in enumerate(self.img_list):
            item = QTableWidgetItem(os.path.basename(name))
            item.setToolTip(name)
            self.table.setItem(row, self.COL_FRAME, item)
            self._fill_center_columns(row, name)
            self._fill_rotation_columns(row, name)
            self._fill_distance_deviation(row, name)
            self._fill_size_column(row, name)
            self._fill_transform_column(row, name)
            self._apply_misaligned_highlight(row, name)
            self._apply_base_marker(row, name)

    def _update_table_data(self):
        """Refresh center/rotation data columns for all existing rows (preserves selection and groups)."""
        for row, name in enumerate(self.img_list):
            if row >= self.table.rowCount():
                break
            self._update_row_data(row, name)

    def _update_row_data(self, row, name):
        """Refresh center/rotation data columns for a single row."""
        if row < 0 or row >= self.table.rowCount():
            return
        self._fill_center_columns(row, name)
        self._fill_rotation_columns(row, name)
        self._fill_distance_deviation(row, name)
        self._fill_size_column(row, name)
        self._fill_transform_column(row, name)
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

    def _get_effective_center(self, name):
        """Return (cx, cy) for *name* — manual if present, else auto, else None."""
        sm = self.workspace.settings_manager
        base = os.path.basename(name)
        key = base if sm.has_manual_center(base) else name
        return sm.get_center(key) or sm.get_auto_center(key) or sm.get_auto_center(base)

    def _get_effective_rotation(self, name):
        """Return rotation angle for *name* — manual if present, else auto, else None."""
        sm = self.workspace.settings_manager
        base = os.path.basename(name)
        key = base if sm.has_manual_rotation(base) else name
        return sm.get_rotation(key) or sm.get_auto_rotation(key) or sm.get_auto_rotation(base)

    def _fill_distance_deviation(self, row, name):
        """Fill COL_CENTER_DIST and COL_DEVIATION relative to the global base."""
        base_info = self.workspace.settings_manager.get_global_base()
        base_center = base_info.get('center')
        base_rotation = base_info.get('rotation')

        # --- distance ---
        if base_center:
            img_center = self._get_effective_center(name)
            if img_center is not None:
                import math
                dx = img_center[0] - base_center[0]
                dy = img_center[1] - base_center[1]
                dist = math.hypot(dx, dy)
                self.table.setItem(row, self.COL_CENTER_DIST,
                                   QTableWidgetItem(f"{dist:.2f}"))
            else:
                self.table.setItem(row, self.COL_CENTER_DIST, QTableWidgetItem(""))
        else:
            self.table.setItem(row, self.COL_CENTER_DIST, QTableWidgetItem(""))

        # --- deviation ---
        if base_rotation is not None:
            img_rotation = self._get_effective_rotation(name)
            if img_rotation is not None:
                deviation = img_rotation - base_rotation
                self.table.setItem(row, self.COL_DEVIATION,
                                   QTableWidgetItem(f"{deviation:.2f}°"))
            else:
                self.table.setItem(row, self.COL_DEVIATION, QTableWidgetItem(""))
        else:
            self.table.setItem(row, self.COL_DEVIATION, QTableWidgetItem(""))

    def _fill_size_column(self, row, name):
        """Fill COL_SIZE with cached image dimensions (WxH) if available."""
        base = os.path.basename(name)
        size_str = self._img_sizes.get(name) or self._img_sizes.get(base, "")
        self.table.setItem(row, self.COL_SIZE, QTableWidgetItem(size_str))

    def _fill_transform_column(self, row, name):
        """Fill COL_TRANSFORM with a checkmark when the image has any manual setting."""
        sm = self.workspace.settings_manager
        base = os.path.basename(name)
        is_transformed = sm.has_manual_center(base) or sm.has_manual_rotation(base)
        item = QTableWidgetItem("\u2714" if is_transformed else "")
        item.setTextAlignment(Qt.AlignCenter)
        if is_transformed:
            item.setForeground(QBrush(QColor(0, 160, 0)))
        self.table.setItem(row, self.COL_TRANSFORM, item)

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

    def _on_bin_images_toggled(self, checked):
        """Apply or clear automatic bin-grouping when the radio button is toggled."""
        if checked:
            self._apply_bin_grouping()
        else:
            self._groups = []
            self._render_groups()

    def _on_binning_factor_changed(self):
        """Re-apply bin-grouping when the factor spinbox changes (only in bin mode)."""
        if self.radio_bin_images.isChecked():
            self._apply_bin_grouping()

    def _apply_bin_grouping(self):
        """Split the image list into sequential groups of *binning_spin* size."""
        factor = self.binning_spin.value()
        n = len(self.img_list)
        self._groups = []
        for i in range(0, n, factor):
            count = min(factor, n - i)
            self._groups.append({'start': i, 'count': count, 'number': 0})
        self._renumber_groups()

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

        selected_rows = sorted(set(idx.row() for idx in self.table.selectedIndexes()))

        # Group action (≥2 rows selected)
        group_act = None
        if len(selected_rows) >= 2:
            group_act = menu.addAction("Group")
            menu.addSeparator()

        # Correct / Ignore actions (any selection)
        n = len(selected_rows)
        label_suffix = f" ({n} images)" if n > 1 else ""
        correct_act = menu.addAction(f"Correct{label_suffix}")
        ignore_act = menu.addAction(f"Ignore{label_suffix}")

        chosen = menu.exec(global_pos)
        if chosen is None:
            return
        if chosen == group_act:
            self._group_rows(selected_rows)
        elif chosen == correct_act:
            for r in selected_rows:
                self._apply_correct(r)
        elif chosen == ignore_act:
            for r in selected_rows:
                self._apply_ignore(r)

    def _apply_correct(self, row):
        """Mark row as corrected: restore default text colour and remove from ignored set."""
        if row < 0 or row >= len(self.img_list):
            return
        was_ignored = row in self._ignored_rows
        self._ignored_rows.discard(row)
        if was_ignored:
            for col in range(1, self.table.columnCount()):
                item = self.table.item(row, col)
                if item is not None:
                    item.setForeground(QBrush())
        name = self.img_list[row]
        print(f"Correct: {os.path.basename(name)}")
        # Navigate only when a single row is corrected (multi-row: user can navigate manually)
        self.workspace.navigator.switch_to_image_by_index(row)

    def _apply_ignore(self, row):
        """Mark row as ignored: dim its text and add to ignored set."""
        if row < 0 or row >= len(self.img_list):
            return
        self._ignored_rows.add(row)
        name = self.img_list[row]
        print(f"Ignore: {os.path.basename(name)}")
        dim = QBrush(QColor(160, 160, 160))
        for col in range(1, self.table.columnCount()):
            item = self.table.item(row, col)
            if item is None:
                item = QTableWidgetItem("")
                self.table.setItem(row, col, item)
            item.setForeground(dim)

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
        """Restart debounce timer on every selection change.

        The actual navigation fires 120 ms after the last change, so rapid
        drag-select passes through intermediate rows without triggering I/O.
        """
        if self._navigating_from_table:
            return
        self._nav_debounce_timer.start()

    def _do_navigate_to_selected_row(self):
        """Called by debounce timer: navigate only when exactly 1 row is selected."""
        if self._navigating_from_table:
            return
        selected_rows = set(idx.row() for idx in self.table.selectedIndexes())
        if len(selected_rows) != 1:
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
        """Called when workspace has loaded and configured a new image.

        Offloads center/rotation calculation to a background thread; the
        rotated display copy is shown in _on_geometry_ready once geometry
        is known.
        """
        row = self.workspace.navigator.current_index
        worker = _GeometryWorker(image_data, row)
        worker.signals.done.connect(
            lambda c, r, i, d=image_data: self._on_geometry_ready(c, r, i, d)
        )
        self._threadPool.start(worker)

    def _on_geometry_ready(self, center, rotation, row, image_data=None):
        """Callback (main thread) after background geometry calculation finishes."""
        self._current_center = center
        self._current_rotation = rotation
        if image_data is not None:
            self.workspace.update_display(image_data)
            display_img = np.copy(image_data.img)
            if center is not None and rotation is not None and rotation != 0:
                display_img = rotateImageAboutPoint(display_img, center, rotation)
            self.image_viewer.display_image(display_img)
        self._redraw_overlays()
        if 0 <= row < len(self.img_list):
            self._update_row_data(row, self.img_list[row])

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

    # ------------------------------------------------------------------
    # Batch detection (multiprocessing)
    # ------------------------------------------------------------------

    def _init_process_executor(self):
        """Create a persistent ProcessPoolExecutor for batch detection."""
        from concurrent.futures import ProcessPoolExecutor
        import multiprocessing as _mp
        worker_count = max(1, (os.cpu_count() or 2) - 2)
        try:
            mp_ctx = _mp.get_context('spawn')
            self.processExecutor = ProcessPoolExecutor(max_workers=worker_count, mp_context=mp_ctx)
            print(f"Process pool initialised with {worker_count} workers (spawn)")
        except Exception as e:
            print(f"Failed to create process pool: {e}")
            self.processExecutor = None

    def _on_detection_btn_toggled(self, checked):
        """Handle the Start Detection / Stop toggle button."""
        if checked:
            if not self._in_batch:
                self.start_detection_btn.setText("Stop")
                self._start_detection()
        else:
            self.stopProcess()

    def stopProcess(self):
        """Cancel the running batch and wait for in-flight workers to finish."""
        self.stop_process = True
        if self.processExecutor:
            self.processExecutor.shutdown(wait=False, cancel_futures=True)
        running_count = self.taskManager.get_running_count()

        msg = f"Stopping Batch Processing\n\nWaiting for {running_count} tasks to complete..."
        self._stopProgress = QProgressDialog(msg, None, 0, 0, self)
        self._stopProgress.setWindowFlags(Qt.Window | Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint)
        self._stopProgress.setModal(False)
        self._stopProgress.show()

        self._stopMsgTimer = QTimer(self)
        self._stopMsgTimer.setInterval(300)
        self._stopMsgTimer.timeout.connect(self._updateStopProgress)
        self._stopMsgTimer.start()

    def _updateStopProgress(self):
        if not hasattr(self, '_stopProgress') or self._stopProgress is None:
            return
        running_count = self.taskManager.get_running_count()
        msg = f"Stopping Batch Processing\n\nWaiting for {running_count} tasks to complete..."
        self._stopProgress.setLabelText(msg)

        if running_count == 0:
            self._stopMsgTimer.stop()
            self._stopProgress.close()
            self._stopProgress = None
            self._on_batch_complete(stopped=True)

    def _start_detection(self):
        """Submit all images to the process pool for center/rotation calculation.

        Only lightweight metadata (file specs) is sent to workers — images are
        loaded inside each subprocess to keep the main thread responsive.
        """
        if self._in_batch:
            return
        if not self.img_list:
            return

        fm = self.workspace.navigator.file_manager
        if fm is None or not fm.specs:
            return

        if self.processExecutor is None:
            self._init_process_executor()
        if self.processExecutor is None:
            return

        self._in_batch = True
        self.stop_process = False
        self.taskManager.clear()

        n = len(self.img_list)
        self.progressBar.setMaximum(n)
        self.progressBar.setMinimum(0)
        self.progressBar.setValue(0)
        self.progressBar.setVisible(True)
        self.statusLabel.setText("Detecting center/rotation...")

        orientation_model = getattr(self.workspace, '_orientation_model', 0)
        dir_path = str(fm.dir_path)

        for i, img_name in enumerate(self.img_list):
            base = os.path.basename(img_name)
            spec = fm.specs[i] if i < len(fm.specs) else None
            manual_center, manual_rotation = self.workspace.get_manual_settings(base)
            job_args = (
                dir_path,
                base,
                spec,
                manual_center,
                manual_rotation,
                orientation_model,
            )
            future = self.processExecutor.submit(_compute_geometry, job_args)
            self.taskManager.submit_task(img_name, i, future)
            future.add_done_callback(self._on_future_done)

        print(f"Batch detection started: {n} images submitted")

    def _on_future_done(self, future):
        """Route future callback to the main thread via QTimer."""
        QTimer.singleShot(0, self, lambda f=future: self._on_batch_result(f))

    def _on_batch_result(self, future):
        """Handle a single completed future in the main thread."""
        try:
            try:
                result = future.result()
                error = result.get('error')
            except Exception as fut_exc:
                error = str(fut_exc)
                result = {'img_name': None, 'center': None, 'rotation': None, 'error': error}
            task = self.taskManager.complete_task(future, result, error)
            if task is None:
                return

            if self.stop_process:
                return

            if error:
                print(f"Detection error for {task.filename}: {error}")
            else:
                sm = self.workspace.settings_manager
                sm.set_auto_cache(
                    os.path.basename(task.filename),
                    result['center'],
                    result['rotation'],
                )

            stats = self.taskManager.get_statistics()
            self.progressBar.setValue(stats['completed'] + stats['failed'])
            self.statusLabel.setText(
                f"Detecting: {stats['completed'] + stats['failed']}/{stats['total']}"
            )

            if 0 <= task.job_index < len(self.img_list):
                self._update_row_data(task.job_index, self.img_list[task.job_index])

            if stats['pending'] == 0:
                self._on_batch_complete()

        except Exception as e:
            print(f"Batch result callback error: {e}")
            traceback.print_exc()

    def _on_batch_complete(self, stopped=False):
        """Clean up after all batch tasks have finished or been stopped."""
        stats = self.taskManager.get_statistics()

        if not stopped:
            self.workspace.settings_manager.save_auto_cache()

        if self.processExecutor:
            self.processExecutor.shutdown(wait=False)
            self.processExecutor = None

        self._in_batch = False
        self.stop_process = False
        self.progressBar.setVisible(False)

        self.start_detection_btn.blockSignals(True)
        self.start_detection_btn.setChecked(False)
        self.start_detection_btn.setText("Start Detection")
        self.start_detection_btn.blockSignals(False)

        if stopped:
            msg = (
                f"Detection stopped: {stats['completed']}/{stats['total']} completed, "
                f"{stats['failed']} failed"
            )
        else:
            msg = (
                f"Detection complete: {stats['completed']}/{stats['total']} succeeded, "
                f"{stats['failed']} failed, avg {stats['avg_time']:.2f}s/image"
            )
        self.statusLabel.setText(msg)
        print(msg)

    # ------------------------------------------------------------------
    # Misc
    # ------------------------------------------------------------------

    def setStatus(self, text):
        self.statusLabel.setText(text)
