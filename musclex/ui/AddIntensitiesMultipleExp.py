import os
import traceback
import numpy as np
import cv2
import matplotlib.patches as mpatches
from PySide6.QtWidgets import (
    QMainWindow, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QHeaderView, QAbstractItemView, QLabel, QProgressBar,
    QSizePolicy, QMenu, QRadioButton, QSpinBox, QDoubleSpinBox, QWidget, QSplitter,
    QScrollArea, QFrame, QStackedWidget, QCheckBox, QStatusBar,
    QProgressDialog, QStyledItemDelegate, QStyleOptionViewItem, QMessageBox,
    QTabBar,
)
from PySide6.QtCore import Qt, Signal, QRunnable, QObject, QThreadPool, QTimer
from PySide6.QtGui import QColor, QBrush, QFont
from musclex import __version__
from musclex.ui.widgets import ProcessingWorkspace, CollapsibleGroupBox
from musclex.ui.widgets.image_viewer_widget import ImageViewerWidget
from musclex.utils.task_manager import ProcessingTaskManager
from musclex.utils.image_processor import rotateImageAboutPoint
from musclex.utils.file_manager import load_image_via_spec


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


class AddIntensitiesMultipleExp(QMainWindow):

    # Column indices
    COL_INDEX = 0    # frame-index group label (replaces COL_GROUP)
    COL_EXP   = 1    # experiment directory basename (new)
    COL_FRAME = 2
    COL_CENTER = 3
    COL_CENTER_MODE = 4
    COL_CENTER_DIST = 5
    COL_AUTO_CENTER = 6
    COL_AUTO_MANUAL_DIST = 7
    COL_ROTATION = 8
    COL_ROTATION_MODE = 9
    COL_ROTATION_DIFF = 10
    COL_AUTO_ROTATION = 11
    COL_AUTO_ROT_DIFF = 12
    COL_SIZE = 13
    COL_IMAGE_DIFF = 14

    HEADERS = [
        "Index",
        "Experiment",
        "Frame",
        "Original Center",
        "Center\nMode",
        "Dist\nfrom Base",
        "Auto\nCenter",
        "Auto Center\nDifference",
        "Rotation",
        "Rotation\nMode",
        "Rot Diff\nfrom Base",
        "Auto\nRotation",
        "Auto Rot\nDifference",
        "Size",
        "Image\nDifference",
    ]

    # Visual style for group cells
    _GROUP_BG = QColor(100, 149, 237)   # cornflower blue
    _GROUP_FG = QColor(255, 255, 255)

    def __init__(self):
        super().__init__()
        self.setWindowTitle("Muscle X Add Intensities Multiple Experiments v." + __version__)
        self._current_inv_transform = None  # inverse affine (2x3) from display → original coords
        self.workspace = ProcessingWorkspace(
            settings_dir="",
            coord_transform_func=self._display_to_original_coords,
        )
        self.misaligned_names = set()
        self._img_sizes: dict = {}  # img_name -> "WxH" string
        self._most_common_size: str = ""  # most frequent size across all images
        self._current_center = None
        self._current_rotation = None
        self._base_image_filename = None
        self._overlay_lines = []  # lines added by _redraw_overlays (global center crosshair)
        self._navigating_from_table = False  # guard against re-entrant navigation

        self._span_col = self.COL_INDEX  # which column drives setSpan merge

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
        self._diff_pairs_in_flight: set = set()  # (idx_a, idx_b) pairs currently submitted
        self._row_geometry_cache: dict = {}  # row → (center, rotation) at last update

        # Multiprocessing sum-images
        self.sumTaskManager = ProcessingTaskManager()
        self.sumExecutor = None
        self._in_sum_batch = False
        self._sum_csv_rows = []
        self._sum_nonmasked_pixels = 0
        self._sum_blank_weight = 1.0

        # Result tab state
        self._result_entries: list = []
        # Each entry: {'filename': str, 'n_images': int, 'total_intensity': float, 'date': str}
        self._parent_dir: str = ""

        # Threshold highlighting
        self._dist_threshold_enabled = True
        self._dist_threshold = 5.0
        self._rot_diff_threshold_enabled = True
        self._rot_diff_threshold = 2.0
        self._diff_percentile_threshold: float = None  # 80th pct of all diff values (auto-computed)
        self._diff_thresh_enabled = True
        self._diff_thresh_value = 0.0

        self._cr_dialog = None

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
        header.setSectionResizeMode(self.COL_INDEX, QHeaderView.Fixed)
        self.table.setColumnWidth(self.COL_INDEX, 52)
        header.setSectionResizeMode(self.COL_EXP, QHeaderView.Interactive)
        self.table.setColumnWidth(self.COL_EXP, 120)
        for col in range(2, len(self.HEADERS)):
            header.setSectionResizeMode(col, QHeaderView.Interactive)
        self.table.setColumnWidth(self.COL_FRAME, 200)
        self.table.setItemDelegateForColumn(self.COL_FRAME, _ElideMiddleDelegate(self.table))

        # Context menu for grouping / ungrouping
        self.table.setContextMenuPolicy(Qt.CustomContextMenu)
        self.table.customContextMenuRequested.connect(self._on_context_menu)
        self.table.itemSelectionChanged.connect(self._on_table_selection_changed)

        # Parent directory and pending experiment sources
        self._parent_dir_path: str = ""
        self._pending_sources: list = []

        # Custom select panel for AIME
        self._select_panel = QWidget()
        _sel_layout = QVBoxLayout(self._select_panel)
        _sel_layout.setContentsMargins(24, 24, 24, 24)
        _sel_layout.setSpacing(8)
        _sel_layout.addStretch()

        _sel_title = QLabel("Select Experiments")
        _sel_title.setStyleSheet("font-weight: bold; font-size: 13px;")
        _sel_layout.addWidget(_sel_title)

        # Step 1: pick parent directory
        _dir_row = QHBoxLayout()
        self._parent_dir_label = QLabel("No folder selected")
        self._parent_dir_label.setStyleSheet("color: gray; font-size: 11px;")
        self._parent_dir_label.setWordWrap(True)
        _browse_btn = QPushButton("Browse Folder\u2026")
        _browse_btn.setMinimumHeight(32)
        _browse_btn.clicked.connect(self._on_browse_parent_dir)
        _dir_row.addWidget(_browse_btn)
        _dir_row.addWidget(self._parent_dir_label, 1)
        _sel_layout.addLayout(_dir_row)

        # Step 2: list of available items (H5 files + subdirs) with multi-select
        _avail_label = QLabel("Available experiments (Use Ctrl/Shift to select multiple):")
        _avail_label.setStyleSheet("font-size: 11px;")
        _sel_layout.addWidget(_avail_label)

        from PySide6.QtWidgets import QListWidget
        self._source_list_widget = QListWidget()
        self._source_list_widget.setMinimumHeight(160)
        self._source_list_widget.setSelectionMode(QListWidget.ExtendedSelection)
        self._source_list_widget.itemSelectionChanged.connect(self._on_source_selection_changed)
        _sel_layout.addWidget(self._source_list_widget)

        # Action row
        _action_row = QHBoxLayout()
        _sel_all_btn = QPushButton("Select All")
        _sel_all_btn.clicked.connect(self._source_list_widget.selectAll)
        _sel_none_btn = QPushButton("Select None")
        _sel_none_btn.clicked.connect(self._source_list_widget.clearSelection)
        self._load_sources_btn = QPushButton("Load")
        self._load_sources_btn.setMinimumHeight(32)
        self._load_sources_btn.setEnabled(False)
        self._load_sources_btn.setStyleSheet("font-weight: bold;")
        self._load_sources_btn.clicked.connect(self._on_load_sources)
        _action_row.addWidget(_sel_all_btn)
        _action_row.addWidget(_sel_none_btn)
        _action_row.addStretch()
        _action_row.addWidget(self._load_sources_btn)
        _sel_layout.addLayout(_action_row)

        self._exp_dirs_label = QLabel("")
        self._exp_dirs_label.setWordWrap(True)
        self._exp_dirs_label.setStyleSheet("color: gray; font-size: 11px;")
        _sel_layout.addWidget(self._exp_dirs_label)
        _sel_layout.addStretch()

        # Left side of splitter: select_panel (before load) / table (after load)
        self._left_stack = QStackedWidget()
        self._left_stack.addWidget(self._select_panel)  # index 0
        self._left_stack.addWidget(self.table)           # index 1
        self._left_stack.setCurrentIndex(0)
        self.workspace.imageDataReady.connect(self._on_image_data_ready)
        self.workspace.needsReprocess.connect(
            lambda: self._on_image_data_ready(self.workspace._current_image_data)
            if self.workspace._current_image_data is not None else None
        )
        self.workspace.batchSettingsChanged.connect(self._update_table_data)
        # Right panel container (global settings group box + scrollable panel)
        right_container = QWidget()
        right_container_layout = QVBoxLayout(right_container)
        right_container_layout.setContentsMargins(0, 0, 0, 0)
        right_container_layout.setSpacing(4)

        # Image viewer in a container (allows reparenting to dialog)
        self.image_viewer = self.workspace.navigator.image_viewer
        self.image_viewer.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Preferred)
        self.image_viewer.setMinimumHeight(200)
        # Navigator hides image_viewer until load_file() is called; AIME skips
        # load_file() so we need to show it explicitly here.
        self.image_viewer.setVisible(True)
        self._viewer_container = QWidget()
        self._viewer_container_layout = QVBoxLayout(self._viewer_container)
        self._viewer_container_layout.setContentsMargins(0, 0, 0, 0)
        self._viewer_container_layout.addWidget(self.image_viewer)
        right_container_layout.addWidget(self._viewer_container, 0)

        # Misaligned Detection group box
        self.misaligned_detection_group = CollapsibleGroupBox("Detect Misaligned Images")
        misaligned_detection_layout = QVBoxLayout()
        misaligned_detection_layout.setContentsMargins(8, 6, 8, 6)
        misaligned_detection_layout.setSpacing(4)

        # Row 1: Start Detection button
        detection_row = QHBoxLayout()
        self.start_detection_btn = QPushButton("Detect Centers && Rotations")
        self.start_detection_btn.setCheckable(True)
        self.start_detection_btn.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
        detection_row.addWidget(self.start_detection_btn)
        misaligned_detection_layout.addLayout(detection_row)

        # Row 3: Auto difference threshold
        dist_thresh_row = QHBoxLayout()
        dist_thresh_row.setSpacing(6)
        self._dist_thresh_chk = QCheckBox("Auto Diff threshold:")
        self._dist_thresh_chk.setChecked(True)
        dist_thresh_row.addWidget(self._dist_thresh_chk)
        self._dist_thresh_spin = QDoubleSpinBox()
        self._dist_thresh_spin.setRange(0.0, 10000.0)
        self._dist_thresh_spin.setDecimals(2)
        self._dist_thresh_spin.setSingleStep(0.5)
        self._dist_thresh_spin.setValue(self._dist_threshold)
        self._dist_thresh_spin.setSuffix(" px")
        self._dist_thresh_spin.setEnabled(True)
        self._dist_thresh_spin.setFixedWidth(100)
        dist_thresh_row.addWidget(self._dist_thresh_spin)
        dist_thresh_row.addStretch()
        misaligned_detection_layout.addLayout(dist_thresh_row)

        # Row 4: Auto-Rot Diff threshold
        dev_thresh_row = QHBoxLayout()
        dev_thresh_row.setSpacing(6)
        self._rot_diff_thresh_chk = QCheckBox("Auto-Rot Diff threshold:")
        self._rot_diff_thresh_chk.setChecked(True)
        dev_thresh_row.addWidget(self._rot_diff_thresh_chk)
        self._rot_diff_thresh_spin = QDoubleSpinBox()
        self._rot_diff_thresh_spin.setRange(0.0, 360.0)
        self._rot_diff_thresh_spin.setDecimals(2)
        self._rot_diff_thresh_spin.setSingleStep(0.5)
        self._rot_diff_thresh_spin.setValue(self._rot_diff_threshold)
        self._rot_diff_thresh_spin.setSuffix(" °")
        self._rot_diff_thresh_spin.setEnabled(True)
        self._rot_diff_thresh_spin.setFixedWidth(100)
        dev_thresh_row.addWidget(self._rot_diff_thresh_spin)
        dev_thresh_row.addStretch()
        misaligned_detection_layout.addLayout(dev_thresh_row)

        # Row 5: Image diff threshold (80th pct, user-adjustable)
        diff_thresh_row = QHBoxLayout()
        diff_thresh_row.setSpacing(6)
        self._diff_thresh_chk = QCheckBox("Image diff threshold (default: 80th pct):")
        self._diff_thresh_chk.setChecked(True)
        diff_thresh_row.addWidget(self._diff_thresh_chk)
        self._diff_thresh_spin = QDoubleSpinBox()
        self._diff_thresh_spin.setRange(0.0, 1e9)
        self._diff_thresh_spin.setDecimals(1)
        self._diff_thresh_spin.setSingleStep(1.0)
        self._diff_thresh_spin.setValue(self._diff_thresh_value)
        self._diff_thresh_spin.setEnabled(True)
        self._diff_thresh_spin.setFixedWidth(120)
        diff_thresh_row.addWidget(self._diff_thresh_spin)
        diff_thresh_row.addStretch()
        misaligned_detection_layout.addLayout(diff_thresh_row)

        self._dist_thresh_chk.toggled.connect(self._on_dist_threshold_toggled)
        self._dist_thresh_spin.valueChanged.connect(self._on_dist_threshold_changed)
        self._rot_diff_thresh_chk.toggled.connect(self._on_rot_diff_threshold_toggled)
        self._rot_diff_thresh_spin.valueChanged.connect(self._on_rot_diff_threshold_changed)
        self._diff_thresh_chk.toggled.connect(self._on_diff_thresh_toggled)
        self._diff_thresh_spin.valueChanged.connect(self._on_diff_thresh_changed)

        self.misaligned_detection_group.set_content_layout(misaligned_detection_layout)



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

        # ── Tab bar ────────────────────────────────────────────────────────
        self._tab_bar = QTabBar()
        self._tab_bar.addTab("Group by Index")
        self._tab_bar.addTab("Group by Exp")
        self._tab_bar.addTab("Result")
        self._tab_bar.setExpanding(False)
        root.addWidget(self._tab_bar)

        # Main stack: page 0 = splitter (Origin), page 1 = Result page
        self._result_page = self._build_result_page()
        self._main_stack = QStackedWidget()
        self._main_stack.addWidget(self.splitter)      # page 0
        self._main_stack.addWidget(self._result_page)  # page 1
        root.addWidget(self._main_stack)

        self._tab_bar.currentChanged.connect(self._on_main_tab_changed)
        self._movable_settings_container = QWidget()
        self._movable_settings_layout = QVBoxLayout(self._movable_settings_container)
        self._movable_settings_layout.setContentsMargins(0, 0, 0, 0)
        self._movable_settings_layout.addWidget(self.image_viewer.display_panel)
        self._movable_settings_layout.addWidget(self.workspace._blank_mask_widget)
        self._right_panel_layout.addWidget(self._movable_settings_container)

        self._right_panel_layout.addWidget(self.misaligned_detection_group)
        self.centerChkBx = QCheckBox("Original Center")
        self.centerChkBx.setChecked(False)
        self.centerChkBx.stateChanged.connect(self._redraw_overlays)

        self.baseCenterChkBx = QCheckBox("Global Base Center")
        self.baseCenterChkBx.setChecked(False)
        self.baseCenterChkBx.stateChanged.connect(self._redraw_overlays)

        _center_row = QWidget()
        _center_row_layout = QHBoxLayout(_center_row)
        _center_row_layout.setContentsMargins(0, 0, 0, 0)
        _center_row_layout.setSpacing(8)
        _center_row_layout.addWidget(self.centerChkBx)
        _center_row_layout.addWidget(self.baseCenterChkBx)
        _center_row_layout.addStretch()
        self.image_viewer.display_panel.add_to_top_slot(_center_row)






        # Image Operations collapsible group
        self._img_ops_group = CollapsibleGroupBox("Image Operations", start_expanded=True)
        _img_ops_layout = QVBoxLayout()
        _img_ops_layout.setSpacing(4)
        _img_ops_layout.setContentsMargins(4, 4, 4, 4)

        self.avg_instead_of_sum_chk = QCheckBox("Compute Average Instead of Sum")
        self.compress_chk = QCheckBox("Compress the Resulting Images")
        _img_ops_layout.addWidget(self.avg_instead_of_sum_chk)
        _img_ops_layout.addWidget(self.compress_chk)

        # Rotation transform mode
        _rot_label = QLabel("Rotation Transform Mode:")
        _rot_label.setStyleSheet("font-weight: bold; color: gray;")
        _img_ops_layout.addWidget(_rot_label)

        _rot_row = QWidget()
        _rot_row_layout = QHBoxLayout(_rot_row)
        _rot_row_layout.setContentsMargins(0, 0, 0, 0)
        _rot_row_layout.setSpacing(12)
        self.radio_rot_absolute = QRadioButton("Align to Make Equator Horizontal")
        self.radio_rot_absolute.setChecked(True)
        self.radio_rot_diff = QRadioButton("Align to Base Image Rotation")
        _rot_row_layout.addWidget(self.radio_rot_absolute)
        _rot_row_layout.addWidget(self.radio_rot_diff)
        _rot_row_layout.addStretch()
        _img_ops_layout.addWidget(_rot_row)

        self._img_ops_group.setLayout(_img_ops_layout)
        self._right_panel_layout.addWidget(self._img_ops_group)
        self._right_panel_layout.addStretch()

        self.sum_images_btn = QPushButton("Sum Images by Index")
        self.sum_images_btn.setCheckable(True)
        self.sum_images_btn.setMinimumHeight(32)
        self.sum_images_btn.setStyleSheet(
            "QPushButton { color: #ededed; background-color: #af6207 }")
        self._right_panel_layout.addWidget(self.sum_images_btn)

        self.start_detection_btn.toggled.connect(self._on_detection_btn_toggled)
        self.sum_images_btn.toggled.connect(self._on_sum_btn_toggled)

    # ------------------------------------------------------------------
    # Result page construction
    # ------------------------------------------------------------------

    def _build_result_page(self):
        page = QWidget()
        layout = QVBoxLayout(page)
        layout.setContentsMargins(0, 0, 0, 0)

        splitter = QSplitter(Qt.Horizontal)

        self._result_table = QTableWidget()
        self._result_table.setColumnCount(4)
        self._result_table.setHorizontalHeaderLabels(
            ["Filename", "N Images", "Total Intensity", "Date"])
        self._result_table.setSelectionBehavior(QAbstractItemView.SelectRows)
        self._result_table.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self._result_table.setAlternatingRowColors(True)
        self._result_table.verticalHeader().setDefaultSectionSize(22)
        self._result_table.horizontalHeader().setStretchLastSection(True)
        self._result_table.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self._result_table.itemSelectionChanged.connect(self._on_result_row_selected)

        self._result_viewer = ImageViewerWidget(show_display_panel=True)
        self._result_viewer.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        # Right side: viewer on top, display_panel below (mirrors Origin layout)
        right_area = QWidget()
        right_layout = QVBoxLayout(right_area)
        right_layout.setContentsMargins(0, 0, 0, 0)
        right_layout.setSpacing(4)
        right_layout.addWidget(self._result_viewer, 1)
        if self._result_viewer.display_panel is not None:
            right_layout.addWidget(self._result_viewer.display_panel, 0)

        splitter.addWidget(self._result_table)
        splitter.addWidget(right_area)
        splitter.setSizes([550, 550])
        splitter.setStretchFactor(0, 0)
        splitter.setStretchFactor(1, 1)

        layout.addWidget(splitter)
        return page

    # ------------------------------------------------------------------
    # Tab switching
    # ------------------------------------------------------------------

    def _on_main_tab_changed(self, index: int):
        if index <= 1:
            self._main_stack.setCurrentIndex(0)
            self._span_col = self.COL_INDEX if index == 0 else self.COL_EXP
            secondary = self.COL_EXP if index == 0 else self.COL_INDEX
            self.table.sortItems(secondary, Qt.AscendingOrder)
            self.table.sortItems(self._span_col, Qt.AscendingOrder)
            self._rebuild_spans()
        else:
            self._main_stack.setCurrentIndex(1)
            self._refresh_result_tab()

    def _refresh_result_tab(self):
        """Populate the result table from memory or CSV."""
        aime_results_dir = os.path.join(self._parent_dir, "aime_results") if self._parent_dir else ""
        if not self._result_entries and aime_results_dir:
            csv_path = os.path.join(aime_results_dir, 'intensities.csv')
            if os.path.exists(csv_path):
                import csv as _csv
                seen = {}
                try:
                    with open(csv_path, newline='') as f:
                        for row in _csv.reader(f):
                            if not row or row[0] == 'Filename':
                                continue
                            seen[row[0]] = {
                                'filename': row[0],
                                'date': row[1] if len(row) > 1 else '',
                                'total_intensity': row[2] if len(row) > 2 else '',
                                'n_images': row[7] if len(row) > 7 else '',
                            }
                    self._result_entries = list(seen.values())
                except Exception as e:
                    print(f"Could not read intensities.csv: {e}")

        self._result_table.setRowCount(0)
        for entry in self._result_entries:
            row = self._result_table.rowCount()
            self._result_table.insertRow(row)
            self._result_table.setItem(row, 0, QTableWidgetItem(str(entry['filename'])))
            self._result_table.setItem(row, 1, QTableWidgetItem(str(entry['n_images'])))
            self._result_table.setItem(row, 2, QTableWidgetItem(str(entry['total_intensity'])))
            self._result_table.setItem(row, 3, QTableWidgetItem(str(entry['date'])))

        if self._result_table.rowCount() > 0:
            self._result_table.selectRow(0)

    def _on_result_row_selected(self):
        """Load and display the selected result image."""
        row = self._result_table.currentRow()
        if row < 0 or row >= len(self._result_entries):
            return
        entry = self._result_entries[row]
        aime_results_dir = os.path.join(self._parent_dir, "aime_results") if self._parent_dir else ""
        if not aime_results_dir:
            return
        full_path = os.path.join(aime_results_dir, entry['filename'])
        if not os.path.exists(full_path):
            return
        try:
            img = load_image_via_spec(
                aime_results_dir, entry['filename'], ("tiff", full_path))
            # Mirror ImageNavigatorWidget behaviour: auto-scale intensity on new
            # image unless "Persist intensities" is checked in the display panel.
            if self._result_viewer.display_panel is not None:
                self._result_viewer.display_panel.update_from_image(
                    img, respect_persist=True)
            self._result_viewer.display_image(img)
        except Exception as e:
            print(f"Could not load result image {entry['filename']}: {e}")

    # ------------------------------------------------------------------
    # Global base helpers
    # ------------------------------------------------------------------

    def _on_global_base_changed(self):
        self._sync_global_settings_state()
        self._update_table_data()

    # ------------------------------------------------------------------
    # FM index ↔ table row helpers
    # ------------------------------------------------------------------

    def _fm_index_for_row(self, row: int):
        """Return the FileManager index stored in *row*, or None."""
        item = self.table.item(row, self.COL_INDEX)
        if item is None:
            return None
        val = item.data(Qt.UserRole)
        return val if isinstance(val, int) else None

    def _name_for_row(self, row: int):
        """Return the image filename (from FileManager) for *row*, or None."""
        fm = self.workspace.navigator.file_manager
        if fm is None:
            return None
        fm_idx = self._fm_index_for_row(row)
        if fm_idx is None or fm_idx >= len(fm.names):
            return None
        return fm.names[fm_idx]

    def _row_for_fm_index(self, fm_idx: int):
        """Return the table row whose stored FM index equals *fm_idx*, or None."""
        for row in range(self.table.rowCount()):
            if self._fm_index_for_row(row) == fm_idx:
                return row
        return None

    def _sync_global_settings_state(self):
        """Read the saved global base and update _base_image_filename."""
        base = self.workspace.settings_manager.get_global_base()
        self._base_image_filename = base.get('base_image')

    def _auto_set_global_base_if_missing(self):
        """If no global base is recorded yet, set the first image as the default."""
        if self._base_image_filename:
            return
        first_name = self._name_for_row(0)
        if first_name is None:
            return
        self.workspace.settings_manager.set_global_base(first_name)
        self.workspace.settings_manager.save_global_base()
        self._base_image_filename = first_name

    def _get_base_center(self):
        """Return the effective center of the global base image, or None."""
        if not self._base_image_filename:
            return None
        return self._get_effective_center(self._base_image_filename)

    def _get_base_rotation(self):
        """Return the effective rotation of the global base image, or None."""
        if not self._base_image_filename:
            return None
        return self._get_effective_rotation(self._base_image_filename)

    # ------------------------------------------------------------------
    # Experiment selection (AIME)
    # ------------------------------------------------------------------

    # Folder names generated by AIME/AISE that should not be treated as experiments
    _SYSTEM_DIRS = {'aime_results', 'aise_results', 'calibration'}

    def _on_browse_parent_dir(self):
        """Pick a parent directory and populate the available-experiments list."""
        from PySide6.QtWidgets import QFileDialog
        folder = QFileDialog.getExistingDirectory(
            self, "Select Experiment Parent Folder", self._parent_dir_path or "",
            QFileDialog.ShowDirsOnly | QFileDialog.DontResolveSymlinks,
        )
        if not folder:
            return

        self._parent_dir_path = folder
        self._parent_dir_label.setText(folder)
        self._parent_dir_label.setStyleSheet("color: black; font-size: 11px;")
        self._populate_available_sources(folder)

    def _populate_available_sources(self, parent_dir: str):
        """Fill the list widget with H5 files and subdirs found in parent_dir."""
        from PySide6.QtWidgets import QListWidgetItem
        self._source_list_widget.clear()
        self._pending_sources.clear()
        self._load_sources_btn.setEnabled(False)
        self._exp_dirs_label.setText("")

        try:
            entries = sorted(os.listdir(parent_dir))
        except OSError:
            return

        for entry in entries:
            full = os.path.join(parent_dir, entry)
            if os.path.isfile(full) and entry.lower().endswith(('.h5', '.hdf5')):
                item = QListWidgetItem(entry)
                item.setToolTip(full)
                self._source_list_widget.addItem(item)
            elif os.path.isdir(full) and entry not in self._SYSTEM_DIRS:
                item = QListWidgetItem(entry + "/")
                item.setToolTip(full)
                self._source_list_widget.addItem(item)

        total = self._source_list_widget.count()
        self._exp_dirs_label.setText(
            f"{total} item(s) found — select which to use as experiments"
            if total else "No H5 files or subdirectories found in this folder."
        )

    def _on_source_selection_changed(self):
        """Enable Load button when at least one item is selected."""
        self._load_sources_btn.setEnabled(
            len(self._source_list_widget.selectedItems()) > 0
        )

    def _on_load_sources(self):
        """Load selected items as experiments into FileManager and build the table."""
        selected_items = self._source_list_widget.selectedItems()
        if not selected_items or not self._parent_dir_path:
            return

        sources = [item.toolTip() for item in selected_items]
        self._pending_sources = sources

        fm = self.workspace.navigator.file_manager
        fm.load_from_sources(sources)
        self.workspace.set_settings_dir(self._parent_dir_path)

        self._on_directories_loaded(self._parent_dir_path, sources)

    def _on_directories_loaded(self, parent_dir, dir_paths):
        """Build table rows and auto-populate spans."""
        self._parent_dir = parent_dir
        self._exp_dirs = dir_paths
        fm = self.workspace.navigator.file_manager

        self.misaligned_names = set()
        self._img_sizes = fm.image_sizes
        self._compute_most_common_size()
        self._compute_diff_percentile_threshold()
        self._sync_global_settings_state()

        # Reset result state
        self._result_entries = []

        self._init_table()
        self._auto_set_global_base_if_missing()
        self._left_stack.setCurrentIndex(1)
        # Explicitly trigger the navigator so imageChanged → imageDataReady fires
        # (_sync_table_selection uses blockSignals so it won't drive the viewer)
        self.workspace.navigator.switch_to_image_by_index(0)
        self._sync_table_selection()

    # ------------------------------------------------------------------
    # Data population
    # ------------------------------------------------------------------

    def _init_table(self):
        """Fully rebuild the table from FileManager data.

        Each row's COL_INDEX item stores the absolute FileManager index as Qt.UserRole
        so every other method can look up the correct name/spec via _name_for_row /
        _fm_index_for_row.

        After filling all rows, two stable sorts are applied (EXP then INDEX) to
        achieve "primary sort by INDEX, secondary by EXP".  _rebuild_spans() then
        merges consecutive equal cells in the active span column.
        """
        self._ignored_rows = set()
        self._row_geometry_cache = {}
        self.table.setRowCount(0)

        fm = self.workspace.navigator.file_manager
        dir_paths = getattr(self, '_exp_dirs', [])
        if fm is None or not dir_paths:
            return

        index_map = {
            dp: fm.source_index_map[dp]
            for dp in dir_paths
            if dp in (fm.source_index_map or {})
        }
        if not index_map:
            return

        ranges = list(index_map.values())
        n_indices = min(end - start + 1 for start, end in ranges)
        n_exps = len([dp for dp in dir_paths if dp in index_map])
        total_rows = n_indices * n_exps
        self.table.setSortingEnabled(False)
        self.table.setRowCount(total_rows)

        sm = self.workspace.settings_manager
        row = 0
        for dp in dir_paths:
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
                self.table.setItem(row, self.COL_INDEX, idx_item)

                exp_item = QTableWidgetItem(exp_label)
                exp_item.setToolTip(name)
                self.table.setItem(row, self.COL_EXP, exp_item)

                frame_item = QTableWidgetItem(os.path.basename(name))
                frame_item.setToolTip(name)
                self.table.setItem(row, self.COL_FRAME, frame_item)

                self._update_row_data(row, name)
                if sm.has_ignore(name):
                    self._apply_ignore(row)
                row += 1

        self.table.sortItems(self.COL_EXP, Qt.AscendingOrder)
        self.table.sortItems(self.COL_INDEX, Qt.AscendingOrder)
        self._rebuild_spans()
        self.table.resizeColumnsToContents()

    def _update_table_data(self):
        """Refresh center/rotation data columns for all existing rows (preserves selection and groups)."""
        for row in range(self.table.rowCount()):
            name = self._name_for_row(row)
            if name:
                self._update_row_data(row, name)

    def _update_row_data(self, row, name):
        """Refresh center/rotation data columns for a single row."""
        if row < 0 or row >= self.table.rowCount():
            return
        self._fill_center_columns(row, name)
        self._fill_auto_center_column(row, name)
        self._fill_auto_manual_dist_column(row, name)
        self._fill_rotation_columns(row, name)
        self._fill_auto_rotation_column(row, name)
        self._fill_auto_rot_diff_column(row, name)
        self._fill_distance_deviation(row, name)
        self._fill_size_column(row, name)
        self._fill_diff_column(row, name)
        self._apply_misaligned_highlight(row, name)
        self._apply_base_marker(row, name)
        if row in self._ignored_rows:
            self._dim_row(row)

        new_geom = (self._get_effective_center(name), self._get_effective_rotation(name))
        if self._row_geometry_cache.get(row) != new_geom:
            self._row_geometry_cache[row] = new_geom
            self._trigger_diff_for_row(row)

    def _fill_center_columns(self, row, name):
        """Fill COL_CENTER and COL_CENTER_MODE from workspace settings manager."""
        sm = self.workspace.settings_manager

        manual = sm.get_center(name)
        if manual is not None:
            cx, cy = manual
            self.table.setItem(row, self.COL_CENTER,
                               QTableWidgetItem(f"({cx:.1f}, {cy:.1f})"))
            self.table.setItem(row, self.COL_CENTER_MODE,
                               QTableWidgetItem("Manual"))
            return

        auto = sm.get_auto_center(name)
        if auto is not None:
            cx, cy = auto
            self.table.setItem(row, self.COL_CENTER,
                               QTableWidgetItem(f"({cx:.1f}, {cy:.1f})"))
            self.table.setItem(row, self.COL_CENTER_MODE,
                               QTableWidgetItem("Auto"))
        else:
            self.table.setItem(row, self.COL_CENTER, QTableWidgetItem(""))
            self.table.setItem(row, self.COL_CENTER_MODE, QTableWidgetItem(""))

    def _fill_auto_center_column(self, row, name):
        """Fill COL_AUTO_CENTER with the raw auto-detected center (before any manual override)."""
        sm = self.workspace.settings_manager
        auto = sm.get_auto_center(name)
        if auto is not None:
            cx, cy = auto
            self.table.setItem(row, self.COL_AUTO_CENTER,
                               QTableWidgetItem(f"({cx:.1f}, {cy:.1f})"))
        else:
            self.table.setItem(row, self.COL_AUTO_CENTER, QTableWidgetItem(""))

    def _fill_auto_manual_dist_column(self, row, name):
        """Fill COL_AUTO_MANUAL_DIST with the distance between auto center and original center.
        Original center is the effective center (manual if set, else auto).
        Cells exceeding the distance threshold are highlighted red."""
        import math
        auto = self.workspace.settings_manager.get_auto_center(name)
        original = self._get_effective_center(name)
        if auto is not None and original is not None:
            dx = auto[0] - original[0]
            dy = auto[1] - original[1]
            dist = math.hypot(dx, dy)
            item = QTableWidgetItem(f"{dist:.2f}")
            if self._dist_threshold_enabled and dist > self._dist_threshold:
                item.setBackground(QBrush(QColor(255, 100, 100)))
                item.setForeground(QBrush(QColor(255, 255, 255)))
            self.table.setItem(row, self.COL_AUTO_MANUAL_DIST, item)
        else:
            self.table.setItem(row, self.COL_AUTO_MANUAL_DIST, QTableWidgetItem(""))

    def _fill_rotation_columns(self, row, name):
        """Fill COL_ROTATION and COL_ROTATION_MODE from workspace settings manager."""
        sm = self.workspace.settings_manager

        manual = sm.get_rotation(name)
        if manual is not None:
            self.table.setItem(row, self.COL_ROTATION,
                               QTableWidgetItem(f"{manual:.2f}°"))
            self.table.setItem(row, self.COL_ROTATION_MODE,
                               QTableWidgetItem("Manual"))
            return

        auto = sm.get_auto_rotation(name)
        if auto is not None:
            self.table.setItem(row, self.COL_ROTATION,
                               QTableWidgetItem(f"{auto:.2f}°"))
            self.table.setItem(row, self.COL_ROTATION_MODE,
                               QTableWidgetItem("Auto"))
        else:
            self.table.setItem(row, self.COL_ROTATION, QTableWidgetItem(""))
            self.table.setItem(row, self.COL_ROTATION_MODE, QTableWidgetItem(""))

        self.table.setItem(row, self.COL_ROTATION_DIFF, QTableWidgetItem(""))

    def _fill_auto_rotation_column(self, row, name):
        """Fill COL_AUTO_ROTATION with the raw auto-detected rotation (before any manual override)."""
        sm = self.workspace.settings_manager
        auto = sm.get_auto_rotation(name)
        if auto is not None:
            self.table.setItem(row, self.COL_AUTO_ROTATION,
                               QTableWidgetItem(f"{auto:.2f}°"))
        else:
            self.table.setItem(row, self.COL_AUTO_ROTATION, QTableWidgetItem(""))

    def _fill_auto_rot_diff_column(self, row, name):
        """Fill COL_AUTO_ROT_DIFF with the difference between auto rotation and original rotation.
        Cells exceeding the rotation-diff threshold are highlighted red."""
        sm = self.workspace.settings_manager
        auto = sm.get_auto_rotation(name)
        original = self._get_effective_rotation(name)
        if auto is not None and original is not None:
            diff = auto - original
            item = QTableWidgetItem(f"{diff:.2f}°")
            if self._rot_diff_threshold_enabled and abs(diff) > self._rot_diff_threshold:
                item.setBackground(QBrush(QColor(255, 100, 100)))
                item.setForeground(QBrush(QColor(255, 255, 255)))
            self.table.setItem(row, self.COL_AUTO_ROT_DIFF, item)
        else:
            self.table.setItem(row, self.COL_AUTO_ROT_DIFF, QTableWidgetItem(""))

    def _get_effective_center(self, name):
        """Return (cx, cy) for *name* — manual if present, else auto, else None."""
        sm = self.workspace.settings_manager
        return sm.get_center(name) or sm.get_auto_center(name)

    def _get_effective_rotation(self, name):
        """Return rotation angle for *name* — manual if present, else auto, else None."""
        sm = self.workspace.settings_manager
        return sm.get_rotation(name) or sm.get_auto_rotation(name)

    def _fill_distance_deviation(self, row, name):
        """Fill COL_CENTER_DIST and COL_ROTATION_DIFF relative to the global base."""
        base_center = self._get_base_center()
        base_rotation = self._get_base_rotation()

        # --- distance ---
        if base_center:
            img_center = self._get_effective_center(name)
            if img_center is not None:
                import math
                dx = img_center[0] - base_center[0]
                dy = img_center[1] - base_center[1]
                dist = math.hypot(dx, dy)
                self.table.setItem(row, self.COL_CENTER_DIST, QTableWidgetItem(f"{dist:.2f}"))
            else:
                self.table.setItem(row, self.COL_CENTER_DIST, QTableWidgetItem(""))
        else:
            self.table.setItem(row, self.COL_CENTER_DIST, QTableWidgetItem(""))

        # --- rotation difference ---
        if base_rotation is not None:
            img_rotation = self._get_effective_rotation(name)
            if img_rotation is not None:
                deviation = img_rotation - base_rotation
                self.table.setItem(row, self.COL_ROTATION_DIFF, QTableWidgetItem(f"{deviation:.2f}°"))
            else:
                self.table.setItem(row, self.COL_ROTATION_DIFF, QTableWidgetItem(""))
        else:
            self.table.setItem(row, self.COL_ROTATION_DIFF, QTableWidgetItem(""))

    def _compute_most_common_size(self):
        """Count image sizes and cache the most frequent one."""
        from collections import Counter
        counts = Counter(s for s in self._img_sizes.values() if s)
        self._most_common_size = counts.most_common(1)[0][0] if counts else ""

    def _compute_diff_percentile_threshold(self):
        """Compute the 80th-percentile of all cached diff values (mirrors old detectImages logic).
        Updates self._diff_percentile_threshold and syncs the spinbox to the computed value."""
        sm = self.workspace.settings_manager
        values = [
            sm.get_image_diff(self._name_for_row(row))
            for row in range(self.table.rowCount())
            if self._name_for_row(row) is not None
        ]
        values = [v for v in values if v is not None]
        if len(values) >= 2:
            self._diff_percentile_threshold = float(np.percentile(values, 80))
            self._diff_thresh_spin.blockSignals(True)
            self._diff_thresh_spin.setValue(self._diff_percentile_threshold)
            self._diff_thresh_spin.blockSignals(False)
            self._diff_thresh_value = self._diff_percentile_threshold
        else:
            self._diff_percentile_threshold = None

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

    def _fill_diff_column(self, row, name):
        """Fill COL_IMAGE_DIFF with the cached mean-abs-diff value (if available).
        Cells whose value exceeds the user-controlled threshold are highlighted red."""
        sm = self.workspace.settings_manager
        val = sm.get_image_diff(name)
        text = f"{val:.4f}" if val is not None else ""
        item = QTableWidgetItem(text)
        if (val is not None
                and self._diff_thresh_enabled
                and self._diff_thresh_value > 0
                and val > self._diff_thresh_value):
            item.setBackground(QBrush(QColor(255, 100, 100)))
            item.setForeground(QBrush(QColor(255, 255, 255)))
        self.table.setItem(row, self.COL_IMAGE_DIFF, item)

    def _apply_misaligned_highlight(self, row, name):
        """Colour the data columns red if the image is in misaligned_names.
        COL_INDEX is intentionally skipped to keep group cell appearance intact."""
        if not self.misaligned_names:
            return
        if name in self.misaligned_names or os.path.basename(name) in self.misaligned_names:
            highlight = QBrush(QColor(255, 120, 120))
            for col in range(1, self.table.columnCount()):   # skip COL_INDEX
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
        base_name = self._base_image_filename or ''
        is_base = (name == base_name) if base_name else False
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

    def _rebuild_spans(self):
        """Merge consecutive rows that share the same value in ``_span_col``.

        Resets any existing spans on COL_INDEX and COL_EXP first, then scans
        ``_span_col`` and applies ``setSpan`` + blue styling to each run of
        equal values.
        """
        for row in range(self.table.rowCount()):
            for c in (self.COL_INDEX, self.COL_EXP):
                if self.table.rowSpan(row, c) > 1:
                    self.table.setSpan(row, c, 1, 1)
            item = self.table.item(row, self.COL_INDEX)
            if item:
                item.setBackground(QBrush())
                item.setForeground(QBrush())
            item_exp = self.table.item(row, self.COL_EXP)
            if item_exp:
                item_exp.setBackground(QBrush())
                item_exp.setForeground(QBrush())

        col = self._span_col
        i = 0
        while i < self.table.rowCount():
            item = self.table.item(i, col)
            if item is None:
                i += 1
                continue
            val = item.text()
            j = i + 1
            while j < self.table.rowCount():
                jitem = self.table.item(j, col)
                if jitem is None or jitem.text() != val:
                    break
                j += 1
            if j - i > 1:
                self.table.setSpan(i, col, j - i, 1)
            span_item = self.table.item(i, col)
            if span_item:
                span_item.setBackground(QBrush(self._GROUP_BG))
                span_item.setForeground(QBrush(self._GROUP_FG))
            i = j

    # ------------------------------------------------------------------
    # Context menu
    # ------------------------------------------------------------------

    def _on_context_menu(self, pos):
        row = self.table.rowAt(pos.y())
        if row < 0:
            return

        global_pos = self.table.viewport().mapToGlobal(pos)
        menu = QMenu(self)

        selected_rows = sorted(set(idx.row() for idx in self.table.selectedIndexes()))

        # Single-row actions
        set_cr_act = None
        set_global_act = None
        if len(selected_rows) == 1:
            set_cr_act = menu.addAction("Set Center and Rotation")
            set_global_act = menu.addAction("Set as Global Base")
            menu.addSeparator()

        # Ignore actions (any selection)
        n = len(selected_rows)
        label_suffix = f" ({n} images)" if n > 1 else ""
        all_ignored = all(r in self._ignored_rows for r in selected_rows)
        if all_ignored:
            ignore_act = menu.addAction(f"Cancel Ignore{label_suffix}")
        else:
            ignore_act = menu.addAction(f"Ignore{label_suffix}")

        chosen = menu.exec(global_pos)
        if chosen is None:
            return
        if chosen == ignore_act:
            if all_ignored:
                for r in selected_rows:
                    self._clear_ignore(r)
            else:
                for r in selected_rows:
                    self._apply_ignore(r)
        elif chosen == set_cr_act:
            self._open_center_rotation_dialog(selected_rows[0])
        elif chosen == set_global_act:
            row = selected_rows[0]
            img_name = self._name_for_row(row)
            if img_name:
                self.workspace.settings_manager.set_global_base(img_name)
                self.workspace.settings_manager.save_global_base()
                self._on_global_base_changed()

    # ------------------------------------------------------------------
    # Center/Rotation dialog (reparent viewer + settings into popup)
    # ------------------------------------------------------------------

    def _open_center_rotation_dialog(self, row):
        """Open a non-modal dialog with image viewer and center/rotation settings."""
        if self._cr_dialog is not None and self._cr_dialog.isVisible():
            self._cr_dialog.raise_()
            return

        fm_idx = self._fm_index_for_row(row)
        if fm_idx is not None and fm_idx != self.workspace.navigator.current_index:
            self.workspace.navigator.switch_to_image_by_index(fm_idx)

        from PySide6.QtWidgets import QDialog

        dlg = QDialog(self)
        dlg.setWindowTitle("Set Center and Rotation")
        dlg.setWindowFlags(
            Qt.Window | Qt.WindowMinimizeButtonHint |
            Qt.WindowMaximizeButtonHint | Qt.WindowCloseButtonHint |
            Qt.WindowStaysOnTopHint
        )
        dlg.setAttribute(Qt.WA_DeleteOnClose, False)
        self._cr_dialog = dlg

        splitter = QSplitter(Qt.Horizontal, dlg)
        outer = QVBoxLayout(dlg)
        outer.setContentsMargins(0, 0, 0, 4)
        outer.addWidget(splitter)

        from PySide6.QtWidgets import QPushButton, QHBoxLayout as _QHBox
        _btn_row = _QHBox()
        _btn_row.addStretch()
        _close_btn = QPushButton("Close")
        _close_btn.setFixedWidth(100)
        _close_btn.clicked.connect(dlg.close)
        _btn_row.addWidget(_close_btn)
        _btn_row.addStretch()
        outer.addLayout(_btn_row)

        splitter.addWidget(self.image_viewer)

        settings_scroll = QScrollArea()
        settings_scroll.setWidgetResizable(True)
        settings_scroll.setFixedWidth(500)
        settings_content = QWidget()
        settings_layout = QVBoxLayout(settings_content)
        settings_layout.addWidget(self.image_viewer.display_panel)
        settings_layout.addWidget(self.workspace._blank_mask_widget)
        settings_layout.addWidget(self.workspace._center_widget)
        settings_layout.addWidget(self.workspace._rotation_widget)
        settings_layout.addStretch()
        settings_scroll.setWidget(settings_content)
        splitter.addWidget(settings_scroll)

        _cs = self.image_viewer.canvas.size()
        self._viewer_canvas_w_before = _cs.width()
        self._viewer_canvas_h_before = _cs.height()
        splitter.setSizes([800, 500])
        dlg.finished.connect(self._on_cr_dialog_closed)
        dlg.resize(1300, 700)
        dlg.show()

    def _on_cr_dialog_closed(self):
        """Reparent widgets back to main window when the dialog closes."""
        # Reset the figure's remembered size to pre-dialog dimensions so that
        # FigureCanvas.sizeHint() reports the original small size.  Without this,
        # sizeHint() returns the dialog's ~700 px height and Qt layout over-allocates
        # vertical space, causing aspect='equal' letterboxing.
        saved_w = getattr(self, '_viewer_canvas_w_before', 0)
        saved_h = getattr(self, '_viewer_canvas_h_before', 0)
        if saved_w > 0 and saved_h > 0:
            dpi = self.image_viewer.figure.dpi
            self.image_viewer.figure.set_size_inches(
                saved_w / dpi, saved_h / dpi, forward=False
            )

        self._viewer_container_layout.addWidget(self.image_viewer)

        self._movable_settings_layout.addWidget(self.image_viewer.display_panel)
        self._movable_settings_layout.addWidget(self.workspace._blank_mask_widget)

        self.workspace._center_widget.setParent(None)
        self.workspace._rotation_widget.setParent(None)

        self._cr_dialog = None

    def _refresh_current_display(self):
        """Re-render the currently displayed image using cached geometry."""
        image_data = self.workspace._current_image_data
        if image_data is None:
            return
        fm_idx = self.workspace.navigator.current_index
        center = self._current_center
        rotation = self._current_rotation
        display_img = np.copy(image_data.img)
        if center is not None and rotation is not None and rotation != 0:
            display_img = rotateImageAboutPoint(display_img, center, rotation)
        base_center = self._get_base_center()
        table_row = self._row_for_fm_index(fm_idx)
        if table_row is not None:
            name = self._name_for_row(table_row)
            if name:
                display_img = self._apply_center_shift_if_needed(display_img, center, name)
        self._current_inv_transform = self._build_display_inv_transform(
            center, rotation, base_center
        )
        self.image_viewer.display_image(display_img)
        self._redraw_overlays()

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
        if row < 0 or row >= self.table.rowCount():
            return
        name = self._name_for_row(row)
        if name is None:
            return
        self._ignored_rows.add(row)
        sm = self.workspace.settings_manager
        sm.set_ignore(name)
        sm.save_ignore()
        print(f"Ignore: {name}")
        self._dim_row(row)

    def _clear_ignore(self, row):
        """Remove the ignore flag from the row and restore normal text colour."""
        if row < 0 or row >= self.table.rowCount():
            return
        name = self._name_for_row(row)
        if name is None:
            return
        self._ignored_rows.discard(row)
        sm = self.workspace.settings_manager
        sm.clear_ignore(name)
        sm.save_ignore()
        print(f"Cancel Ignore: {name}")
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
        if row < 0 or row >= self.table.rowCount():
            return
        fm_idx = self._fm_index_for_row(row)
        if fm_idx is None:
            return
        self._navigating_from_table = True
        try:
            self.workspace.navigator.switch_to_image_by_index(fm_idx)
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
        fm_idx = self.workspace.navigator.current_index
        table_row = self._row_for_fm_index(fm_idx)
        worker = _GeometryWorker(image_data, table_row if table_row is not None else -1)
        worker.signals.done.connect(
            lambda c, r, i, d=image_data: self._on_geometry_ready(c, r, i, d)
        )
        self._threadPool.start(worker)

    def _on_geometry_ready(self, center, rotation, table_row, image_data=None):
        """Callback (main thread) after background geometry calculation finishes.
        *table_row* is the table row index (not the FM index).
        """
        self._current_center = center
        self._current_rotation = rotation
        if image_data is not None:
            self.workspace.update_display(image_data)
            display_img = image_data.get_working_image()
            if center is not None and rotation is not None and rotation != 0:
                display_img = rotateImageAboutPoint(display_img, center, rotation)
            base_center = self._get_base_center()
            if table_row is not None and table_row >= 0:
                name = self._name_for_row(table_row)
                if name:
                    display_img = self._apply_center_shift_if_needed(display_img, center, name)
            self._current_inv_transform = self._build_display_inv_transform(
                center, rotation, base_center
            )
            self.image_viewer.display_image(display_img)
        self._redraw_overlays()
        if table_row is not None and table_row >= 0:
            name = self._name_for_row(table_row)
            if name:
                self._update_row_data(table_row, name)

    def _sync_table_selection(self):
        """Highlight the table row that matches the navigator's current FM index."""
        fm_idx = self.workspace.navigator.current_index
        row = self._row_for_fm_index(fm_idx)
        if row is not None and 0 <= row < self.table.rowCount():
            self.table.blockSignals(True)
            self.table.selectRow(row)
            self.table.scrollTo(self.table.model().index(row, 0))
            self.table.blockSignals(False)

    def _apply_center_shift_if_needed(self, img, center, img_name):
        """Translate image so its center aligns with the global base center
        (same approach as QuadrantFolder.transformImage).
        Returns the (possibly shifted) image array."""
        if center is None:
            return img
        base_center = self._get_base_center()
        if base_center is None:
            return img
        tx = base_center[0] - center[0]
        ty = base_center[1] - center[1]
        if tx == 0 and ty == 0:
            return img
        h, w = img.shape[:2]
        M = np.float32([[1, 0, tx], [0, 1, ty]])
        return cv2.warpAffine(img, M, (w, h))

    def _build_display_inv_transform(self, center, rotation, base_center):
        """Build the inverse affine matrix (2x3) that maps display coordinates
        back to original image coordinates.

        The forward transform applied when rendering is:
          1. Rotate around ``center`` by ``rotation`` degrees
          2. Translate by (base_center - center)

        The inverse lets the workspace convert a click on the displayed image
        back to the original image coordinate, mirroring how QuadrantFoldingGUI
        uses ``inv_transform`` from QuadrantFolder.transformImage().
        """
        if center is None or base_center is None:
            return None

        # Step 1 forward matrix: rotate around center
        if rotation and rotation != 0:
            M_rot_3x3 = np.vstack([
                cv2.getRotationMatrix2D(tuple(center), rotation, 1),
                [0, 0, 1]
            ]).astype(np.float64)
        else:
            M_rot_3x3 = np.eye(3, dtype=np.float64)

        # Step 2 forward matrix: translation
        tx = base_center[0] - center[0]
        ty = base_center[1] - center[1]
        M_trans_3x3 = np.array(
            [[1, 0, tx], [0, 1, ty], [0, 0, 1]], dtype=np.float64
        )

        # Full forward: translate ∘ rotate  (rotate first, then translate)
        M_full = M_trans_3x3 @ M_rot_3x3

        # Return 2x3 inverse
        return np.linalg.inv(M_full)[:2, :]

    def _display_to_original_coords(self, x, y):
        """Convert a point from the displayed (transformed) image back to the
        original image coordinate system.  Passed to ProcessingWorkspace as
        ``coord_transform_func`` so center-setting tools operate correctly."""
        if self._current_inv_transform is None:
            return x, y
        pt = np.array([x, y, 1.0], dtype=np.float64)
        orig = self._current_inv_transform @ pt
        return float(orig[0]), float(orig[1])

    def _redraw_overlays(self):
        """Draw overlays: center circle and/or global center crosshair."""
        ax = self.image_viewer.axes

        for patch in list(ax.patches):
            patch.remove()
        for line in self._overlay_lines:
            try:
                line.remove()
            except ValueError:
                pass
        self._overlay_lines = []

        if self.centerChkBx.isChecked() and self._current_center is not None:
            cx, cy = self._current_center
            circle = mpatches.Circle((cx, cy), 10, color='g', fill=False, linewidth=1.5)
            ax.add_patch(circle)

        if self.baseCenterChkBx.isChecked():
            base_center = self._get_base_center()
            if base_center is not None:
                bx, by = base_center
                arm = 20
                h_line, = ax.plot([bx - arm, bx + arm], [by, by], color='r', linewidth=1.5)
                v_line, = ax.plot([bx, bx], [by - arm, by + arm], color='r', linewidth=1.5)
                self._overlay_lines.extend([h_line, v_line])

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

    def _on_rot_diff_threshold_toggled(self, checked):
        self._rot_diff_thresh_spin.setEnabled(checked)
        self._rot_diff_threshold_enabled = checked
        self._apply_threshold_highlighting()

    def _on_rot_diff_threshold_changed(self, value):
        self._rot_diff_threshold = value
        if self._rot_diff_threshold_enabled:
            self._apply_threshold_highlighting()

    def _on_diff_thresh_toggled(self, checked):
        self._diff_thresh_spin.setEnabled(checked)
        self._diff_thresh_enabled = checked
        self._apply_threshold_highlighting()

    def _on_diff_thresh_changed(self, value):
        self._diff_thresh_value = value
        if self._diff_thresh_enabled:
            self._apply_threshold_highlighting()

    def _apply_threshold_highlighting(self):
        """Re-apply (or clear) red highlighting for distance, rotation difference, and image diff columns."""
        _red_bg = QBrush(QColor(255, 100, 100))
        _red_fg = QBrush(QColor(255, 255, 255))

        for row in range(self.table.rowCount()):
            # --- auto-manual distance ---
            dist_item = self.table.item(row, self.COL_AUTO_MANUAL_DIST)
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

            # --- auto-rot diff ---
            dev_item = self.table.item(row, self.COL_AUTO_ROT_DIFF)
            if dev_item and dev_item.text():
                try:
                    val = abs(float(dev_item.text().rstrip("°")))
                    if self._rot_diff_threshold_enabled and val > self._rot_diff_threshold:
                        dev_item.setBackground(_red_bg)
                        dev_item.setForeground(_red_fg)
                    else:
                        dev_item.setData(Qt.BackgroundRole, None)
                        dev_item.setData(Qt.ForegroundRole, None)
                except ValueError:
                    pass

            # --- image diff ---
            diff_item = self.table.item(row, self.COL_IMAGE_DIFF)
            if diff_item and diff_item.text():
                try:
                    val = float(diff_item.text())
                    if self._diff_thresh_enabled and self._diff_thresh_value > 0 and val > self._diff_thresh_value:
                        diff_item.setBackground(_red_bg)
                        diff_item.setForeground(_red_fg)
                    else:
                        diff_item.setData(Qt.BackgroundRole, None)
                        diff_item.setData(Qt.ForegroundRole, None)
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
        if self.table.rowCount() == 0:
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

        n = self.table.rowCount()
        self.progressBar.setMaximum(n)
        self.progressBar.setMinimum(0)
        self.progressBar.setValue(0)
        self.progressBar.setVisible(True)
        self.statusLabel.setText("Detecting center/rotation...")

        orientation_model = getattr(self.workspace, '_orientation_model', 0)

        for row in range(n):
            fm_idx = self._fm_index_for_row(row)
            if fm_idx is None:
                continue
            img_name = fm.names[fm_idx]
            spec = fm.specs[fm_idx] if fm_idx < len(fm.specs) else None
            manual_center, manual_rotation = self.workspace.get_manual_settings(img_name)
            job_args = (
                "",
                img_name,
                spec,
                manual_center,
                manual_rotation,
                orientation_model,
            )
            future = self.processExecutor.submit(_compute_geometry, job_args)
            self.taskManager.submit_task(img_name, row, future)
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
                    task.filename,
                    result['center'],
                    result['rotation'],
                )

            stats = self.taskManager.get_statistics()
            self.progressBar.setValue(stats['completed'] + stats['failed'])
            self.statusLabel.setText(
                f"Detecting: {stats['completed'] + stats['failed']}/{stats['total']}"
            )

            if 0 <= task.job_index < self.table.rowCount():
                name = self._name_for_row(task.job_index)
                if name:
                    self._update_row_data(task.job_index, name)

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
    # Image difference calculation (triggered automatically on row update)
    # ------------------------------------------------------------------

    def _row_has_geometry(self, row):
        """Return True if *row* has a usable center (auto or manual)."""
        if row < 0 or row >= self.table.rowCount():
            return False
        name = self._name_for_row(row)
        return name is not None and self._get_effective_center(name) is not None

    def _trigger_diff_for_row(self, row):
        """Recompute the (at most) two diff pairs that involve *row*.

        Pair (prev_active, row)  → diff stored on row
        Pair (row, next_active)  → diff stored on next_active

        All callbacks run in the main thread (via QTimer.singleShot), so
        _diff_pairs_in_flight is safe to mutate here without a lock.
        """
        active = [i for i in range(self.table.rowCount()) if i not in self._ignored_rows]
        if row not in active:
            return

        pos = active.index(row)
        pairs = []
        if pos > 0:
            pairs.append((active[pos - 1], active[pos]))
        if pos < len(active) - 1:
            pairs.append((active[pos], active[pos + 1]))

        for idx_a, idx_b in pairs:
            if (idx_a, idx_b) in self._diff_pairs_in_flight:
                continue
            if not self._row_has_geometry(idx_a) or not self._row_has_geometry(idx_b):
                continue
            self._diff_pairs_in_flight.add((idx_a, idx_b))
            self._submit_diff_pair(idx_a, idx_b)

    def _submit_diff_pair(self, idx_a, idx_b):
        """Build args and submit a single (idx_a, idx_b) diff job."""
        from concurrent.futures import ProcessPoolExecutor
        import multiprocessing as _mp

        fm = self.workspace.navigator.file_manager
        if fm is None or not fm.specs:
            self._diff_pairs_in_flight.discard((idx_a, idx_b))
            return

        if self.diffExecutor is None:
            worker_count = max(1, (os.cpu_count() or 2) - 2)
            try:
                mp_ctx = _mp.get_context('spawn')
                self.diffExecutor = ProcessPoolExecutor(
                    max_workers=worker_count, mp_context=mp_ctx)
                print(f"Diff process pool initialised with {worker_count} workers (spawn)")
            except Exception as e:
                print(f"Failed to create diff process pool: {e}")
                self.diffExecutor = None

        if self.diffExecutor is None:
            self._diff_pairs_in_flight.discard((idx_a, idx_b))
            return

        name_a = self._name_for_row(idx_a)
        name_b = self._name_for_row(idx_b)
        if name_a is None or name_b is None:
            self._diff_pairs_in_flight.discard((idx_a, idx_b))
            return
        fm_idx_a = self._fm_index_for_row(idx_a)
        fm_idx_b = self._fm_index_for_row(idx_b)
        base_center = self._get_base_center()
        base_rotation = self._get_base_rotation()

        job_args = (
            "",
            name_a, name_b,
            fm.specs[fm_idx_a] if fm_idx_a is not None and fm_idx_a < len(fm.specs) else None,
            fm.specs[fm_idx_b] if fm_idx_b is not None and fm_idx_b < len(fm.specs) else None,
            self._get_effective_center(name_a),
            self._get_effective_rotation(name_a),
            self._get_effective_center(name_b),
            self._get_effective_rotation(name_b),
            list(base_center) if base_center else None,
            base_rotation,
            idx_b,  # job_index → table row whose diff column gets updated
        )
        future = self.diffExecutor.submit(_compute_image_diff, job_args)
        self.diffTaskManager.submit_task(name_b, idx_b, future)
        future.add_done_callback(
            lambda f, a=idx_a, b=idx_b: QTimer.singleShot(
                0, self, lambda: self._on_diff_result(f, a, b)
            )
        )

    def _on_diff_result(self, future, idx_a, idx_b):
        """Handle a completed diff future in the main thread."""
        self._diff_pairs_in_flight.discard((idx_a, idx_b))
        try:
            try:
                result = future.result()
                error = result.get('error')
            except Exception as fut_exc:
                error = str(fut_exc)
                result = {'pair_index': None, 'diff': None, 'error': error}

            self.diffTaskManager.complete_task(future, result, error)

            name_b = self._name_for_row(idx_b) if idx_b < self.table.rowCount() else None
            if error:
                print(f"Diff error for pair ({idx_a}, {idx_b}): {error}")
            elif name_b is not None:
                sm = self.workspace.settings_manager
                sm.set_image_diff(name_b, result['diff'])
                sm.save_image_diff()
                self._compute_diff_percentile_threshold()

            if name_b is not None and idx_b < self.table.rowCount():
                self._fill_diff_column(idx_b, name_b)

            if not self._diff_pairs_in_flight and self.diffExecutor is not None:
                self.diffExecutor.shutdown(wait=False)
                self.diffExecutor = None

        except Exception as e:
            print(f"Diff result callback error: {e}")
            traceback.print_exc()

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
            if self._in_sum_batch:
                return
            if self.table.rowCount() == 0:
                QMessageBox.information(self, "Sum Images",
                                        "No images loaded.")
                self.sum_images_btn.blockSignals(True)
                self.sum_images_btn.setChecked(False)
                self.sum_images_btn.blockSignals(False)
                return
            fm = self.workspace.navigator.file_manager
            if fm is None or not fm.specs:
                QMessageBox.warning(self, "Sum Images", "No folder loaded.")
                self.sum_images_btn.blockSignals(True)
                self.sum_images_btn.setChecked(False)
                self.sum_images_btn.blockSignals(False)
                return
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
        if self.table.rowCount() == 0:
            QMessageBox.information(self, "Sum Images",
                                    "No images loaded.")
            return

        fm = self.workspace.navigator.file_manager
        if fm is None or not fm.specs:
            QMessageBox.warning(self, "Sum Images", "No folder loaded.")
            return

        parent_dir = getattr(self, '_parent_dir', str(fm.dir_path))
        output_dir = os.path.join(parent_dir, "aime_results")
        os.makedirs(output_dir, exist_ok=True)

        if self.sumExecutor is None:
            self._init_sum_executor()
        if self.sumExecutor is None:
            return

        do_average = self.avg_instead_of_sum_chk.isChecked()
        compress = self.compress_chk.isChecked()
        rotation_mode = 'absolute' if self.radio_rot_absolute.isChecked() else 'diff'

        # Blank config (loaded once, shared across all groups)
        blank_mask_config = self.workspace.get_blank_mask_config()
        apply_blank = blank_mask_config['apply_blank']
        blank_weight = blank_mask_config['blank_weight']
        blank_img = None
        mask_for_csv = None
        from musclex.utils.file_manager import getBlankImageAndMask
        if apply_blank:
            blank_img, mask_for_csv, _ = getBlankImageAndMask(parent_dir, return_weight=True)
        else:
            _, mask_for_csv, _ = getBlankImageAndMask(parent_dir, return_weight=True)

        self._sum_blank_weight = blank_weight if apply_blank else 0.0
        self._sum_nonmasked_pixels = (
            int(np.sum(mask_for_csv == 1)) if mask_for_csv is not None else 0
        )
        self._sum_csv_rows = []

        # Transform base
        _bc = self._get_base_center()
        base_center = list(_bc) if _bc else None
        base_rotation = self._get_base_rotation()

        self._in_sum_batch = True
        self.sumTaskManager.clear()

        # Build index groups from COL_INDEX column (always groups by frame index)
        from collections import defaultdict
        index_groups = defaultdict(list)
        for row in range(self.table.rowCount()):
            item = self.table.item(row, self.COL_INDEX)
            if item is not None:
                index_groups[item.data(Qt.DisplayRole)].append(row)

        jobs_submitted = 0
        for group_num, rows in sorted(index_groups.items()):
            active_rows = [r for r in rows if r not in self._ignored_rows]
            if not active_rows:
                print(f"Group {group_num}: all images ignored, skipping.")
                continue

            img_names = [self._name_for_row(r) for r in active_rows]
            fm_indices = [self._fm_index_for_row(r) for r in active_rows]
            specs = [
                fm.specs[fi] if fi is not None and fi < len(fm.specs) else None
                for fi in fm_indices
            ]

            per_img_transforms = []
            for r in active_rows:
                name = self._name_for_row(r)
                center = self._get_effective_center(name)
                rotation = self._get_effective_rotation(name)
                per_img_transforms.append((
                    list(center) if center else None,
                    rotation,
                ))

            first = os.path.basename(img_names[0])
            last = os.path.basename(img_names[-1])
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

            job_args = (group_num, "", img_names, specs,
                        per_img_transforms, base_center, base_rotation,
                        blank_img, blank_weight, apply_blank,
                        do_average, output_path, compress,
                        rotation_mode)
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
                from datetime import datetime as _dt
                total_intensity = result.get('total_intensity', 0.0)
                nonmasked = self._sum_nonmasked_pixels
                avg_mask = (total_intensity / nonmasked) if nonmasked > 0 else 0.0
                _timestamp = _dt.now().strftime("%m/%d/%Y %H:%M:%S")
                self._sum_csv_rows.append([
                    os.path.basename(result['output_path']),
                    _timestamp,
                    total_intensity,       # Original Image Intensity (Total)
                    total_intensity,       # Masked Image Intensity (Total)
                    nonmasked,             # Number of Pixels Not Masked
                    avg_mask,              # Masked Image Intensity (Average)
                    self._sum_blank_weight,
                    result['n_images'],    # Binning Factor
                    False,                 # Drawn Mask (matches old code default)
                    False,                 # Computed Mask (matches old code default)
                ])
                self._result_entries.append({
                    'filename': os.path.basename(result['output_path']),
                    'n_images': result['n_images'],
                    'total_intensity': total_intensity,
                    'date': _timestamp,
                })

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

    def _write_sum_csv(self, output_dir):
        """Write accumulated CSV statistics to aime_results/intensities.csv."""
        if not self._sum_csv_rows:
            return
        import csv as _csv
        csv_path = os.path.join(output_dir, 'intensities.csv')
        write_header = not os.path.exists(csv_path)
        with open(csv_path, 'a', newline='') as f:
            writer = _csv.writer(f)
            if write_header:
                writer.writerow([
                    'Filename', 'Date',
                    'Original Image Intensity (Total)',
                    'Masked Image Intensity (Total)',
                    'Number of Pixels Not Masked',
                    'Masked Image Intensity (Average)',
                    'Blank Image Weight', 'Binning Factor',
                    'Drawn Mask', 'Computed Mask',
                ])
            for row in self._sum_csv_rows:
                writer.writerow(row)
        print(f"CSV written: {csv_path} ({len(self._sum_csv_rows)} rows)")
        self._sum_csv_rows = []

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
        self.sum_images_btn.setText("Sum Images by Index")
        self.sum_images_btn.blockSignals(False)

        if self._parent_dir and self._sum_csv_rows:
            output_dir = os.path.join(self._parent_dir, "aime_results")
            self._write_sum_csv(output_dir)

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
