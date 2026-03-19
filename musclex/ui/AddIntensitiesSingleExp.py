import os
import traceback
import numpy as np
import cv2
import matplotlib.patches as mpatches
from PySide6.QtWidgets import (
    QMainWindow, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QHeaderView, QAbstractItemView, QLabel, QProgressBar,
    QSizePolicy, QMenu, QRadioButton, QSpinBox, QDoubleSpinBox, QWidget, QSplitter,
    QScrollArea, QFrame, QStackedWidget, QCheckBox, QGroupBox, QStatusBar,
    QProgressDialog, QStyledItemDelegate, QStyleOptionViewItem, QMessageBox,
)
from PySide6.QtCore import Qt, Signal, QRunnable, QObject, QThreadPool, QTimer
from PySide6.QtGui import QColor, QBrush, QFont
from musclex import __version__
from musclex.ui.widgets import ProcessingWorkspace
from musclex.ui.GlobalSettingsDialog import GlobalSettingsDialog
from musclex.utils.task_manager import ProcessingTaskManager
from musclex.utils.image_processor import rotateImageAboutPoint


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
    Each image is aligned to the base center/rotation only when its has_transform flag is True.
    """
    (dir_path, img_name_a, img_name_b,
     spec_a, spec_b,
     center_a, rotation_a,
     center_b, rotation_b,
     base_center, base_rotation,
     has_transform_a, has_transform_b,
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

        img_a = load_image_via_spec(dir_path, img_name_a, spec_a)
        img_b = load_image_via_spec(dir_path, img_name_b, spec_b)
        ta = (_transform_img(img_a, center_a, rotation_a, base_center, base_rotation)
              if has_transform_a else img_a.astype(_np.float32))
        tb = (_transform_img(img_b, center_b, rotation_b, base_center, base_rotation)
              if has_transform_b else img_b.astype(_np.float32))
        diff = float(_np.mean(_np.abs(ta - tb)))
        return {'pair_index': pair_index, 'diff': diff, 'error': None}
    except Exception as e:
        traceback.print_exc()
        return {'pair_index': pair_index, 'diff': None, 'error': str(e)}


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


def _sum_group_worker(args):
    """Top-level function for subprocess: load, sum/average images in one group, save to disk."""
    (group_num, dir_path, img_names, specs,
     per_img_transforms, base_center, base_rotation,
     blank_img, blank_weight, apply_blank,
     do_average, output_path, compress) = args
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
            deviation = (rotation or 0) - (b_rotation or 0)
            if deviation != 0 and b_center is not None:
                M2 = _cv2.getRotationMatrix2D(tuple(b_center), deviation, 1)
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

            center, rotation, has_transform = per_img_transforms[i]
            if has_transform:
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
                'n_images': len(images), 'error': None}
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
        self._most_common_size: str = ""  # most frequent size across all images
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

        # Multiprocessing image-diff calculation
        self.diffTaskManager = ProcessingTaskManager()
        self.diffExecutor = None
        self._in_diff_batch = False

        # Multiprocessing sum-images
        self.sumTaskManager = ProcessingTaskManager()
        self.sumExecutor = None
        self._in_sum_batch = False

        # Threshold highlighting
        self._dist_threshold_enabled = False
        self._dist_threshold = 5.0
        self._dev_threshold_enabled = False
        self._dev_threshold = 2.0
        self._diff_percentile_threshold: float = None  # 80th pct of all diff values

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
        for col in range(1, len(self.HEADERS)):
            header.setSectionResizeMode(col, QHeaderView.Interactive)
        self.table.setColumnWidth(self.COL_FRAME, 200)
        self.table.setItemDelegateForColumn(self.COL_FRAME, _ElideMiddleDelegate(self.table))

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

        # Row 2: Start Detection button + Calculate Image Difference button
        detection_row = QHBoxLayout()
        self.start_detection_btn = QPushButton("Start Detection")
        self.start_detection_btn.setCheckable(True)
        detection_row.addWidget(self.start_detection_btn)
        self.calc_diff_btn = QPushButton("Calculate Image Difference")
        detection_row.addWidget(self.calc_diff_btn)
        detection_row.addStretch()
        misaligned_detection_layout.addLayout(detection_row)

        # Row 2b: Auto diff threshold readout (80th percentile)
        diff_thresh_row = QHBoxLayout()
        diff_thresh_row.setSpacing(6)
        diff_thresh_row.addWidget(QLabel("Image diff threshold (80th pct):"))
        self._diff_thresh_label = QLabel("—")
        self._diff_thresh_label.setStyleSheet("font-size: 11px; color: #555;")
        diff_thresh_row.addWidget(self._diff_thresh_label)
        diff_thresh_row.addStretch()
        misaligned_detection_layout.addLayout(diff_thresh_row)

        # Row 3: Distance threshold
        dist_thresh_row = QHBoxLayout()
        dist_thresh_row.setSpacing(6)
        self._dist_thresh_chk = QCheckBox("Distance threshold:")
        self._dist_thresh_chk.setChecked(False)
        dist_thresh_row.addWidget(self._dist_thresh_chk)
        self._dist_thresh_spin = QDoubleSpinBox()
        self._dist_thresh_spin.setRange(0.0, 10000.0)
        self._dist_thresh_spin.setDecimals(2)
        self._dist_thresh_spin.setSingleStep(0.5)
        self._dist_thresh_spin.setValue(self._dist_threshold)
        self._dist_thresh_spin.setSuffix(" px")
        self._dist_thresh_spin.setEnabled(False)
        self._dist_thresh_spin.setFixedWidth(100)
        dist_thresh_row.addWidget(self._dist_thresh_spin)
        dist_thresh_row.addStretch()
        misaligned_detection_layout.addLayout(dist_thresh_row)

        # Row 4: Deviation threshold
        dev_thresh_row = QHBoxLayout()
        dev_thresh_row.setSpacing(6)
        self._dev_thresh_chk = QCheckBox("Deviation threshold:")
        self._dev_thresh_chk.setChecked(False)
        dev_thresh_row.addWidget(self._dev_thresh_chk)
        self._dev_thresh_spin = QDoubleSpinBox()
        self._dev_thresh_spin.setRange(0.0, 360.0)
        self._dev_thresh_spin.setDecimals(2)
        self._dev_thresh_spin.setSingleStep(0.5)
        self._dev_thresh_spin.setValue(self._dev_threshold)
        self._dev_thresh_spin.setSuffix(" °")
        self._dev_thresh_spin.setEnabled(False)
        self._dev_thresh_spin.setFixedWidth(100)
        dev_thresh_row.addWidget(self._dev_thresh_spin)
        dev_thresh_row.addStretch()
        misaligned_detection_layout.addLayout(dev_thresh_row)

        self._dist_thresh_chk.toggled.connect(self._on_dist_threshold_toggled)
        self._dist_thresh_spin.valueChanged.connect(self._on_dist_threshold_changed)
        self._dev_thresh_chk.toggled.connect(self._on_dev_threshold_toggled)
        self._dev_thresh_spin.valueChanged.connect(self._on_dev_threshold_changed)

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

        self.avg_instead_of_sum_chk = QCheckBox("Compute Average Instead of Sum")
        self.compress_chk = QCheckBox("Compress the Resulting Images")
        self._right_panel_layout.addWidget(self.avg_instead_of_sum_chk)
        self._right_panel_layout.addWidget(self.compress_chk)

        self.sum_images_btn = QPushButton("Sum Images")
        self.sum_images_btn.setCheckable(True)
        self.sum_images_btn.setMinimumHeight(32)
        self._right_panel_layout.addWidget(self.sum_images_btn)

        self.radio_bin_images.toggled.connect(self._binning_row.setVisible)
        self.radio_bin_images.toggled.connect(self._on_bin_images_toggled)
        self.binning_spin.valueChanged.connect(self._on_binning_factor_changed)

        self.global_settings_btn.clicked.connect(self._open_global_settings)
        self.start_detection_btn.toggled.connect(self._on_detection_btn_toggled)
        self.calc_diff_btn.clicked.connect(self._on_calc_diff_btn_clicked)
        self.sum_images_btn.toggled.connect(self._on_sum_btn_toggled)

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
        self._compute_most_common_size()
        self._compute_diff_percentile_threshold()
        self._sync_global_settings_state()
        self._init_table()
        self._sync_table_selection()
        self._left_stack.setCurrentIndex(1)

    def _on_scan_complete(self):
        """Refresh the file list once the background scan finishes."""
        self.img_list = list(self.workspace.navigator.file_manager.names)
        self._img_sizes = self.workspace.navigator.file_manager.image_sizes
        self._compute_most_common_size()
        self._compute_diff_percentile_threshold()
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
        sm = self.workspace.settings_manager
        for row, name in enumerate(self.img_list):
            item = QTableWidgetItem(os.path.basename(name))
            item.setToolTip(name)
            self.table.setItem(row, self.COL_FRAME, item)
            self._update_row_data(row, name)
            if sm.has_ignore(os.path.basename(name)):
                self._apply_ignore(row)
        self.table.resizeColumnsToContents()

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
        self._fill_diff_column(row, name)
        self._apply_misaligned_highlight(row, name)
        self._apply_base_marker(row, name)
        if row in self._ignored_rows:
            self._dim_row(row)

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
                item = QTableWidgetItem(f"{dist:.2f}")
                if self._dist_threshold_enabled and dist > self._dist_threshold:
                    item.setBackground(QBrush(QColor(255, 100, 100)))
                    item.setForeground(QBrush(QColor(255, 255, 255)))
                self.table.setItem(row, self.COL_CENTER_DIST, item)
            else:
                self.table.setItem(row, self.COL_CENTER_DIST, QTableWidgetItem(""))
        else:
            self.table.setItem(row, self.COL_CENTER_DIST, QTableWidgetItem(""))

        # --- deviation ---
        if base_rotation is not None:
            img_rotation = self._get_effective_rotation(name)
            if img_rotation is not None:
                deviation = img_rotation - base_rotation
                item = QTableWidgetItem(f"{deviation:.2f}°")
                if self._dev_threshold_enabled and abs(deviation) > self._dev_threshold:
                    item.setBackground(QBrush(QColor(255, 100, 100)))
                    item.setForeground(QBrush(QColor(255, 255, 255)))
                self.table.setItem(row, self.COL_DEVIATION, item)
            else:
                self.table.setItem(row, self.COL_DEVIATION, QTableWidgetItem(""))
        else:
            self.table.setItem(row, self.COL_DEVIATION, QTableWidgetItem(""))

    def _compute_most_common_size(self):
        """Count image sizes and cache the most frequent one."""
        from collections import Counter
        counts = Counter(s for s in self._img_sizes.values() if s)
        self._most_common_size = counts.most_common(1)[0][0] if counts else ""

    def _compute_diff_percentile_threshold(self):
        """Compute the 80th-percentile of all cached diff values (mirrors old detectImages logic).
        Updates self._diff_percentile_threshold and the UI label."""
        sm = self.workspace.settings_manager
        values = [
            sm.get_image_diff(os.path.basename(name))
            for name in self.img_list
        ]
        values = [v for v in values if v is not None]
        if len(values) >= 2:
            self._diff_percentile_threshold = float(np.percentile(values, 80))
            self._diff_thresh_label.setText(f"{self._diff_percentile_threshold:.4f}")
        else:
            self._diff_percentile_threshold = None
            self._diff_thresh_label.setText("—")

    def _fill_size_column(self, row, name):
        """Fill COL_SIZE with cached image dimensions (WxH) if available.
        Text is coloured red when the size differs from the most common size."""
        base = os.path.basename(name)
        size_str = self._img_sizes.get(name) or self._img_sizes.get(base, "")
        item = QTableWidgetItem(size_str)
        if size_str and self._most_common_size and size_str != self._most_common_size:
            item.setBackground(QBrush(QColor(255, 100, 100)))
            item.setForeground(QBrush(QColor(255, 255, 255)))
        self.table.setItem(row, self.COL_SIZE, item)

    def _fill_transform_column(self, row, name):
        """Fill COL_TRANSFORM with a checkmark when the image is flagged for transform."""
        sm = self.workspace.settings_manager
        base = os.path.basename(name)
        needs_transform = sm.has_transform(base)
        item = QTableWidgetItem("\u2714" if needs_transform else "")
        item.setTextAlignment(Qt.AlignCenter)
        if needs_transform:
            item.setForeground(QBrush(QColor(0, 160, 0)))
        self.table.setItem(row, self.COL_TRANSFORM, item)

    def _fill_diff_column(self, row, name):
        """Fill COL_IMAGE_DIFF with the cached mean-abs-diff value (if available).
        Cells whose value exceeds the 80th-percentile threshold are highlighted red."""
        sm = self.workspace.settings_manager
        val = sm.get_image_diff(os.path.basename(name))
        text = f"{val:.4f}" if val is not None else ""
        item = QTableWidgetItem(text)
        if (val is not None
                and self._diff_percentile_threshold is not None
                and val > self._diff_percentile_threshold):
            item.setBackground(QBrush(QColor(255, 100, 100)))
            item.setForeground(QBrush(QColor(255, 255, 255)))
        self.table.setItem(row, self.COL_IMAGE_DIFF, item)

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

        # Need Transform / Don't Transform / Ignore actions (any selection)
        n = len(selected_rows)
        label_suffix = f" ({n} images)" if n > 1 else ""
        sm = self.workspace.settings_manager
        # If ALL selected rows are already flagged, offer "Don't Transform"; otherwise "Need Transform"
        all_flagged = all(
            sm.has_transform(os.path.basename(self.img_list[r]))
            for r in selected_rows if r < len(self.img_list)
        )
        if all_flagged:
            transform_act = menu.addAction(f"Don't Transform{label_suffix}")
        else:
            transform_act = menu.addAction(f"Need Transform{label_suffix}")
        all_ignored = all(r in self._ignored_rows for r in selected_rows)
        if all_ignored:
            ignore_act = menu.addAction(f"Cancel Ignore{label_suffix}")
        else:
            ignore_act = menu.addAction(f"Ignore{label_suffix}")

        chosen = menu.exec(global_pos)
        if chosen is None:
            return
        if chosen == group_act:
            self._group_rows(selected_rows)
        elif chosen == transform_act:
            if all_flagged:
                for r in selected_rows:
                    self._clear_transform(r)
            else:
                for r in selected_rows:
                    self._apply_transform(r)
            current = self.workspace.navigator.current_index
            if current in selected_rows:
                self._refresh_current_display()
        elif chosen == ignore_act:
            if all_ignored:
                for r in selected_rows:
                    self._clear_ignore(r)
            else:
                for r in selected_rows:
                    self._apply_ignore(r)

    def _refresh_current_display(self):
        """Re-render the currently displayed image using cached geometry.

        Called after transform flags change so the viewer immediately reflects
        whether the center-shift is applied, without reloading from disk.
        """
        image_data = self.workspace._current_image_data
        if image_data is None:
            return
        row = self.workspace.navigator.current_index
        center = self._current_center
        rotation = self._current_rotation
        display_img = np.copy(image_data.img)
        if center is not None and rotation is not None and rotation != 0:
            display_img = rotateImageAboutPoint(display_img, center, rotation)
        if 0 <= row < len(self.img_list):
            display_img = self._apply_center_shift_if_needed(
                display_img, center, self.img_list[row]
            )
        self.image_viewer.display_image(display_img)
        self._redraw_overlays()

    def _apply_transform(self, row):
        """Flag the image to be transformed during future calculations."""
        if row < 0 or row >= len(self.img_list):
            return
        name = self.img_list[row]
        base = os.path.basename(name)
        sm = self.workspace.settings_manager
        sm.set_transform(base)
        sm.save_transform()
        self._fill_transform_column(row, name)
        print(f"Marked for transform: {base}")

    def _clear_transform(self, row):
        """Remove the transform flag from the image."""
        if row < 0 or row >= len(self.img_list):
            return
        name = self.img_list[row]
        base = os.path.basename(name)
        sm = self.workspace.settings_manager
        sm.clear_transform(base)
        sm.save_transform()
        self._fill_transform_column(row, name)
        print(f"Transform cleared: {base}")

    def _dim_row(self, row):
        """Apply grey foreground to all data columns of a row (visual only)."""
        dim = QBrush(QColor(160, 160, 160))
        for col in range(1, self.table.columnCount()):
            item = self.table.item(row, col)
            if item is None:
                item = QTableWidgetItem("")
                self.table.setItem(row, col, item)
            item.setForeground(dim)

    def _apply_ignore(self, row):
        """Mark row as ignored: dim its text and add to ignored set."""
        if row < 0 or row >= len(self.img_list):
            return
        self._ignored_rows.add(row)
        name = self.img_list[row]
        base = os.path.basename(name)
        sm = self.workspace.settings_manager
        sm.set_ignore(base)
        sm.save_ignore()
        print(f"Ignore: {base}")
        self._dim_row(row)

    def _clear_ignore(self, row):
        """Remove the ignore flag from the row and restore normal text colour."""
        if row < 0 or row >= len(self.img_list):
            return
        self._ignored_rows.discard(row)
        name = self.img_list[row]
        base = os.path.basename(name)
        sm = self.workspace.settings_manager
        sm.clear_ignore(base)
        sm.save_ignore()
        print(f"Cancel Ignore: {base}")
        normal = QBrush(self.table.palette().color(self.table.foregroundRole()))
        for col in range(1, self.table.columnCount()):
            item = self.table.item(row, col)
            if item is not None:
                item.setForeground(normal)

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
            display_img = image_data.get_working_image()
            if center is not None and rotation is not None and rotation != 0:
                display_img = rotateImageAboutPoint(display_img, center, rotation)
            if 0 <= row < len(self.img_list):
                display_img = self._apply_center_shift_if_needed(
                    display_img, center, self.img_list[row]
                )
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

    def _apply_center_shift_if_needed(self, img, center, img_name):
        """If the image is flagged for transform, translate it so its center
        aligns with the global base center (same approach as QuadrantFolder.transformImage).
        Returns the (possibly shifted) image array."""
        if center is None:
            return img
        sm = self.workspace.settings_manager
        base = os.path.basename(img_name)
        if not sm.has_transform(base):
            return img
        base_info = sm.get_global_base()
        base_center = base_info.get('center')
        if base_center is None:
            return img
        tx = base_center[0] - center[0]
        ty = base_center[1] - center[1]
        if tx == 0 and ty == 0:
            return img
        h, w = img.shape[:2]
        M = np.float32([[1, 0, tx], [0, 1, ty]])
        return cv2.warpAffine(img, M, (w, h))

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

    # ------------------------------------------------------------------
    # Threshold highlighting
    # ------------------------------------------------------------------

    def _on_dist_threshold_toggled(self, checked):
        self._dist_thresh_spin.setEnabled(checked)
        self._dist_threshold_enabled = checked
        self._apply_threshold_highlighting()

    def _on_dist_threshold_changed(self, value):
        self._dist_threshold = value
        if self._dist_threshold_enabled:
            self._apply_threshold_highlighting()

    def _on_dev_threshold_toggled(self, checked):
        self._dev_thresh_spin.setEnabled(checked)
        self._dev_threshold_enabled = checked
        self._apply_threshold_highlighting()

    def _on_dev_threshold_changed(self, value):
        self._dev_threshold = value
        if self._dev_threshold_enabled:
            self._apply_threshold_highlighting()

    def _apply_threshold_highlighting(self):
        """Re-apply (or clear) red highlighting for distance and deviation columns."""
        _red_bg = QBrush(QColor(255, 100, 100))
        _red_fg = QBrush(QColor(255, 255, 255))

        for row in range(self.table.rowCount()):
            # --- distance ---
            dist_item = self.table.item(row, self.COL_CENTER_DIST)
            if dist_item and dist_item.text():
                try:
                    val = float(dist_item.text())
                    if self._dist_threshold_enabled and val > self._dist_threshold:
                        dist_item.setBackground(_red_bg)
                        dist_item.setForeground(_red_fg)
                    else:
                        dist_item.setData(Qt.BackgroundRole, None)
                        dist_item.setData(Qt.ForegroundRole, None)
                except ValueError:
                    pass

            # --- deviation ---
            dev_item = self.table.item(row, self.COL_DEVIATION)
            if dev_item and dev_item.text():
                try:
                    val = abs(float(dev_item.text().rstrip("°")))
                    if self._dev_threshold_enabled and val > self._dev_threshold:
                        dev_item.setBackground(_red_bg)
                        dev_item.setForeground(_red_fg)
                    else:
                        dev_item.setData(Qt.BackgroundRole, None)
                        dev_item.setData(Qt.ForegroundRole, None)
                except ValueError:
                    pass

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
    # Image difference batch calculation (multiprocessing)
    # ------------------------------------------------------------------

    def _on_calc_diff_btn_clicked(self):
        if self._in_diff_batch:
            return
        self._start_image_diff_calc()

    def _init_diff_executor(self):
        from concurrent.futures import ProcessPoolExecutor
        import multiprocessing as _mp
        worker_count = max(1, (os.cpu_count() or 2) - 2)
        try:
            mp_ctx = _mp.get_context('spawn')
            self.diffExecutor = ProcessPoolExecutor(max_workers=worker_count, mp_context=mp_ctx)
            print(f"Diff process pool initialised with {worker_count} workers (spawn)")
        except Exception as e:
            print(f"Failed to create diff process pool: {e}")
            self.diffExecutor = None

    def _start_image_diff_calc(self):
        """Submit consecutive image pairs for diff calculation."""
        if self._in_diff_batch:
            return
        n = len(self.img_list)
        if n < 2:
            return

        fm = self.workspace.navigator.file_manager
        if fm is None or not fm.specs:
            return

        if self.diffExecutor is None:
            self._init_diff_executor()
        if self.diffExecutor is None:
            return

        self._in_diff_batch = True
        self.stop_process = False
        self.diffTaskManager.clear()

        # Build list of active (non-ignored) image indices so that ignored
        # images are skipped and their neighbours are compared directly,
        # e.g. if index 1 is ignored the pair (0, 2) is compared instead of
        # (0, 1) and (1, 2).
        active_indices = [i for i in range(n) if i not in self._ignored_rows]
        pair_count = len(active_indices) - 1
        if pair_count < 1:
            self._in_diff_batch = False
            return

        self.progressBar.setMaximum(pair_count)
        self.progressBar.setMinimum(0)
        self.progressBar.setValue(0)
        self.progressBar.setVisible(True)
        self.statusLabel.setText("Calculating image differences...")
        self.calc_diff_btn.setEnabled(False)

        sm = self.workspace.settings_manager
        base_info = sm.get_global_base()
        base_center = base_info.get('center')
        base_rotation = base_info.get('rotation')
        dir_path = str(fm.dir_path)

        for pair_idx, (idx_a, idx_b) in enumerate(
                zip(active_indices, active_indices[1:]), start=1):
            name_a = self.img_list[idx_a]
            name_b = self.img_list[idx_b]
            base_a = os.path.basename(name_a)
            base_b = os.path.basename(name_b)
            spec_a = fm.specs[idx_a] if idx_a < len(fm.specs) else None
            spec_b = fm.specs[idx_b] if idx_b < len(fm.specs) else None
            center_a = self._get_effective_center(name_a)
            center_b = self._get_effective_center(name_b)
            rotation_a = self._get_effective_rotation(name_a)
            rotation_b = self._get_effective_rotation(name_b)
            has_transform_a = sm.has_transform(base_a)
            has_transform_b = sm.has_transform(base_b)

            job_args = (
                dir_path,
                base_a, base_b,
                spec_a, spec_b,
                center_a, rotation_a,
                center_b, rotation_b,
                list(base_center) if base_center else None,
                base_rotation,
                has_transform_a, has_transform_b,
                pair_idx,
            )
            future = self.diffExecutor.submit(_compute_image_diff, job_args)
            self.diffTaskManager.submit_task(base_b, pair_idx, future)
            future.add_done_callback(self._on_diff_future_done)

        print(f"Image diff calculation started: {pair_count} pairs submitted "
              f"({n - len(active_indices)} image(s) ignored)")

    def _on_diff_future_done(self, future):
        QTimer.singleShot(0, self, lambda f=future: self._on_diff_batch_result(f))

    def _on_diff_batch_result(self, future):
        try:
            try:
                result = future.result()
                error = result.get('error')
            except Exception as fut_exc:
                error = str(fut_exc)
                result = {'pair_index': None, 'diff': None, 'error': error}

            task = self.diffTaskManager.complete_task(future, result, error)
            if task is None:
                return

            if self.stop_process:
                return

            if error:
                print(f"Diff error for {task.filename}: {error}")
            else:
                sm = self.workspace.settings_manager
                sm.set_image_diff(os.path.basename(task.filename), result['diff'])

            stats = self.diffTaskManager.get_statistics()
            self.progressBar.setValue(stats['completed'] + stats['failed'])
            self.statusLabel.setText(
                f"Calculating diff: {stats['completed'] + stats['failed']}/{stats['total']}"
            )

            if 0 <= task.job_index < len(self.img_list):
                self._fill_diff_column(task.job_index, self.img_list[task.job_index])

            if stats['pending'] == 0:
                self._on_diff_batch_complete()

        except Exception as e:
            print(f"Diff batch result callback error: {e}")
            traceback.print_exc()

    def _on_diff_batch_complete(self, stopped=False):
        stats = self.diffTaskManager.get_statistics()

        if not stopped:
            self.workspace.settings_manager.save_image_diff()
            self._compute_diff_percentile_threshold()
            for row, name in enumerate(self.img_list):
                if row < self.table.rowCount():
                    self._fill_diff_column(row, name)

        if self.diffExecutor:
            self.diffExecutor.shutdown(wait=False)
            self.diffExecutor = None

        self._in_diff_batch = False
        self.progressBar.setVisible(False)
        self.calc_diff_btn.setEnabled(True)

        if stopped:
            msg = (
                f"Diff stopped: {stats['completed']}/{stats['total']} completed, "
                f"{stats['failed']} failed"
            )
        else:
            msg = (
                f"Diff complete: {stats['completed']}/{stats['total']} succeeded, "
                f"{stats['failed']} failed, avg {stats['avg_time']:.2f}s/pair"
            )
        self.statusLabel.setText(msg)
        print(msg)

    # ------------------------------------------------------------------
    # Sum images (multiprocessing)
    # ------------------------------------------------------------------

    def _init_sum_executor(self):
        from concurrent.futures import ProcessPoolExecutor
        import multiprocessing as _mp
        worker_count = max(1, (os.cpu_count() or 2) - 2)
        try:
            mp_ctx = _mp.get_context('spawn')
            self.sumExecutor = ProcessPoolExecutor(max_workers=worker_count, mp_context=mp_ctx)
            print(f"Sum process pool initialised with {worker_count} workers (spawn)")
        except Exception as e:
            print(f"Failed to create sum process pool: {e}")
            self.sumExecutor = None

    def _on_sum_btn_toggled(self, checked):
        if checked:
            if not self._in_sum_batch:
                self.sum_images_btn.setText("Stop")
                self._on_sum_images_clicked()
        else:
            self._stop_sum()

    def _stop_sum(self):
        """Cancel the running sum batch."""
        self.stop_process = True
        if self.sumExecutor:
            self.sumExecutor.shutdown(wait=False, cancel_futures=True)
        running_count = self.sumTaskManager.get_running_count()

        msg = f"Stopping Sum\n\nWaiting for {running_count} tasks to complete..."
        self._sumStopProgress = QProgressDialog(msg, None, 0, 0, self)
        self._sumStopProgress.setWindowFlags(
            Qt.Window | Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint)
        self._sumStopProgress.setModal(False)
        self._sumStopProgress.show()

        self._sumStopTimer = QTimer(self)
        self._sumStopTimer.setInterval(300)
        self._sumStopTimer.timeout.connect(self._update_sum_stop_progress)
        self._sumStopTimer.start()

    def _update_sum_stop_progress(self):
        if not hasattr(self, '_sumStopProgress') or self._sumStopProgress is None:
            return
        running_count = self.sumTaskManager.get_running_count()
        self._sumStopProgress.setLabelText(
            f"Stopping Sum\n\nWaiting for {running_count} tasks to complete...")
        if running_count == 0:
            self._sumStopTimer.stop()
            self._sumStopProgress.close()
            self._sumStopProgress = None
            self._on_sum_batch_complete(stopped=True)

    def _on_sum_images_clicked(self):
        if self._in_sum_batch:
            return
        if not self._groups:
            QMessageBox.information(self, "Sum Images",
                                    "No groups defined. Please create groups first.")
            return

        fm = self.workspace.navigator.file_manager
        if fm is None or not fm.specs:
            QMessageBox.warning(self, "Sum Images", "No folder loaded.")
            return

        dir_path = str(fm.dir_path)
        output_dir = os.path.join(dir_path, "aise_results")
        os.makedirs(output_dir, exist_ok=True)

        if self.sumExecutor is None:
            self._init_sum_executor()
        if self.sumExecutor is None:
            return

        do_average = self.avg_instead_of_sum_chk.isChecked()
        compress = self.compress_chk.isChecked()

        # Blank config (loaded once, shared across all groups)
        blank_mask_config = self.workspace.get_blank_mask_config()
        apply_blank = blank_mask_config['apply_blank']
        blank_weight = blank_mask_config['blank_weight']
        blank_img = None
        if apply_blank:
            from musclex.utils.file_manager import getBlankImageAndMask
            blank_img, _, _ = getBlankImageAndMask(dir_path, return_weight=True)

        # Transform base
        sm = self.workspace.settings_manager
        base_info = sm.get_global_base()
        base_center = list(base_info['center']) if base_info.get('center') else None
        base_rotation = base_info.get('rotation')

        self._in_sum_batch = True
        self.sumTaskManager.clear()

        sorted_groups = sorted(self._groups, key=lambda g: g['start'])
        jobs_submitted = 0

        for group in sorted_groups:
            start = group['start']
            count = group['count']
            group_num = group['number']

            active_rows = [
                r for r in range(start, start + count)
                if r not in self._ignored_rows
            ]
            if not active_rows:
                print(f"Group {group_num}: all images ignored, skipping.")
                continue

            img_names = [os.path.basename(self.img_list[r]) for r in active_rows]
            specs = [fm.specs[r] if r < len(fm.specs) else None for r in active_rows]

            per_img_transforms = []
            for r in active_rows:
                name = self.img_list[r]
                base = os.path.basename(name)
                center = self._get_effective_center(name)
                rotation = self._get_effective_rotation(name)
                has_transform = sm.has_transform(base)
                per_img_transforms.append((
                    list(center) if center else None,
                    rotation,
                    has_transform,
                ))

            # Mirror AddIntensitiesExp AISE filename convention:
            # {prefix}_{firstNum}_{lastNum}.tif  or  group_{num:05d}.tif
            first = img_names[0]
            last = img_names[-1]
            f_ind1 = first.rfind('_')
            f_ind2 = first.rfind('.')
            l_ind1 = last.rfind('_')
            l_ind2 = last.rfind('.')
            if (f_ind1 == -1 or f_ind2 == -1 or l_ind1 == -1 or l_ind2 == -1
                    or first[:f_ind1] != last[:l_ind1]):
                filename = "group_" + str(group_num).zfill(5) + '.tif'
            else:
                filename = (first[:f_ind1] + "_"
                            + first[f_ind1 + 1:f_ind2] + "_"
                            + last[l_ind1 + 1:l_ind2] + '.tif')
            output_path = os.path.join(output_dir, filename)

            job_args = (group_num, dir_path, img_names, specs,
                        per_img_transforms, base_center, base_rotation,
                        blank_img, blank_weight, apply_blank,
                        do_average, output_path, compress)
            future = self.sumExecutor.submit(_sum_group_worker, job_args)
            self.sumTaskManager.submit_task(filename, group_num, future)
            future.add_done_callback(self._on_sum_future_done)
            jobs_submitted += 1

        if jobs_submitted == 0:
            self._in_sum_batch = False
            if self.sumExecutor:
                self.sumExecutor.shutdown(wait=False)
                self.sumExecutor = None
            self.sum_images_btn.blockSignals(True)
            self.sum_images_btn.setChecked(False)
            self.sum_images_btn.setText("Sum Images")
            self.sum_images_btn.blockSignals(False)
            QMessageBox.information(self, "Sum Images",
                                    "All groups have every image ignored — nothing to sum.")
            return

        self.progressBar.setMaximum(jobs_submitted)
        self.progressBar.setMinimum(0)
        self.progressBar.setValue(0)
        self.progressBar.setVisible(True)
        op = "Averaging" if do_average else "Summing"
        self.statusLabel.setText(f"{op} images: 0/{jobs_submitted} groups...")
        print(f"Sum batch started: {jobs_submitted} group(s) submitted")

    def _on_sum_future_done(self, future):
        QTimer.singleShot(0, self, lambda f=future: self._on_sum_batch_result(f))

    def _on_sum_batch_result(self, future):
        try:
            try:
                result = future.result()
                error = result.get('error')
            except Exception as fut_exc:
                error = str(fut_exc)
                result = {'group_num': None, 'output_path': None,
                          'n_images': 0, 'error': error}

            task = self.sumTaskManager.complete_task(future, result, error)
            if task is None:
                return

            if self.stop_process:
                return

            if error:
                print(f"Sum error for group {result.get('group_num')}: {error}")
            else:
                print(f"Group {result['group_num']}: {result['n_images']} image(s) "
                      f"→ {result['output_path']}")

            stats = self.sumTaskManager.get_statistics()
            done = stats['completed'] + stats['failed']
            self.progressBar.setValue(done)
            op = "Averaging" if self.avg_instead_of_sum_chk.isChecked() else "Summing"
            self.statusLabel.setText(f"{op} images: {done}/{stats['total']} groups...")

            if stats['pending'] == 0:
                self._on_sum_batch_complete()

        except Exception as e:
            print(f"Sum batch result callback error: {e}")
            traceback.print_exc()

    def _on_sum_batch_complete(self, stopped=False):
        stats = self.sumTaskManager.get_statistics()

        if self.sumExecutor:
            self.sumExecutor.shutdown(wait=False)
            self.sumExecutor = None

        self._in_sum_batch = False
        self.stop_process = False
        self.progressBar.setVisible(False)

        self.sum_images_btn.blockSignals(True)
        self.sum_images_btn.setChecked(False)
        self.sum_images_btn.setText("Sum Images")
        self.sum_images_btn.blockSignals(False)

        op = "Average" if self.avg_instead_of_sum_chk.isChecked() else "Sum"
        if stopped:
            msg = (f"{op} stopped: {stats['completed']}/{stats['total']} group(s) saved, "
                   f"{stats['failed']} failed")
        else:
            msg = (f"{op} complete: {stats['completed']}/{stats['total']} group(s) saved, "
                   f"{stats['failed']} failed, avg {stats['avg_time']:.2f}s/group")
        self.statusLabel.setText(msg)
        print(msg)

    # ------------------------------------------------------------------
    # Misc
    # ------------------------------------------------------------------

    def setStatus(self, text):
        self.statusLabel.setText(text)
