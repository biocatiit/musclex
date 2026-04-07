import os
import traceback
import numpy as np
import cv2
import matplotlib.patches as mpatches
from PySide6.QtWidgets import (
    QMainWindow, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QHeaderView, QAbstractItemView, QLabel,
    QSizePolicy, QRadioButton, QSpinBox, QWidget, QSplitter,
    QScrollArea, QFrame, QStackedWidget, QCheckBox, QStatusBar,
    QProgressDialog, QMessageBox,
    QTabBar, QInputDialog,
)
from PySide6.QtCore import Qt, QThreadPool, QTimer

from PySide6.QtGui import QColor
from musclex import __version__
from musclex.ui.widgets import ProcessingWorkspace, CollapsibleGroupBox
from musclex.ui.widgets.image_viewer_widget import ImageViewerWidget
from musclex.utils.image_processor import rotateImageAboutPoint
from musclex.utils.file_manager import load_image_via_spec
from musclex.ui.add_intensities_row_mapper import CartesianRowMapper
from musclex.ui.add_intensities_common import (
    _sum_group_worker,
    _GeometryWorkerSignals,
    _GeometryWorker,
    WorkflowGuideDialog,
)
from musclex.ui.widgets.image_alignment_table import ColKey
from musclex.ui.widgets.image_alignment_widget import ImageAlignmentWidget


_AIME_WORKFLOW_HTML = """
<html>
<head>
<style>
  body { font-family: Arial, sans-serif; font-size: 13px; margin: 10px 16px; color: #222; }
  h2   { color: #2c5f9e; margin-bottom: 4px; }
  p.intro { margin-top: 0; color: #555; font-style: italic; }
  ol   { padding-left: 20px; }
  li   { margin-bottom: 10px; line-height: 1.5; }
  .step-title { font-weight: bold; color: #1a3d6b; }
  .hint { font-size: 12px; color: #666; margin-left: 4px; }
  hr   { border: none; border-top: 1px solid #ddd; margin: 12px 0; }
</style>
</head>
<body>
<h2>AIME Workflow Guide</h2>
<p class="intro">
  Follow these steps to load, align and sum images across multiple experiments.
  The <b>global image</b> is the reference to which all other images are aligned.
</p>
<hr/>
<ol>
  <li>
    <span class="step-title">Browse and select experiments</span><br/>
    <span class="hint">
      Click <i>Browse Folder&hellip;</i> to choose a parent directory.
      Select one or more experiments (subdirectories or H5 files) from the list,
      then click <i>Load</i>.
    </span>
  </li>
  <li>
    <span class="step-title">Set the global (reference) image</span><br/>
    <span class="hint">
      By default the first image is the global image.
      Right-click a row in the table and choose <i>Set as Global Image</i> to change it.
    </span>
  </li>
  <li>
    <span class="step-title">Detect misalignment</span><br/>
    <span class="hint">
      Click <i>Detect Centers &amp; Rotations</i>.
      The table highlights rows whose center distance or rotation difference
      exceeds the configured thresholds.
      Use <i>Compute Image Difference</i> to add pixel-level comparison scores.
    </span>
  </li>
  <li>
    <span class="step-title">Correct misaligned images</span><br/>
    <span class="hint">
      Select a misaligned row and adjust its center / rotation.
      Right-click &rarr; <i>Apply to Subsequent Images</i> for progressive drift.
      Right-click &rarr; <i>Ignore Image</i> to exclude an image from summation.
    </span>
  </li>
  <li>
    <span class="step-title">Choose a grouping mode</span><br/>
    <span class="hint">
      <b>Group by Index:</b> frames are grouped and summed by their index across experiments
      &mdash; frame&nbsp;1 from all experiments is summed together, frame&nbsp;2 from all experiments, and so on.<br/>
      <b>Group by Exp:</b> reorders the table to group rows by experiment for easier visual inspection;
      summing still produces one output file per frame index.
    </span>
  </li>
  <li>
    <span class="step-title">Apply operation and inspect results</span><br/>
    <span class="hint">
      Choose <i>Average</i> or <i>Sum</i> in the <i>Image Operations</i> panel,
      then click <i>Sum Images</i>.
      Switch to the <b>Result</b> tab to browse and inspect the output images.
    </span>
  </li>
</ol>
</body>
</html>
"""

_AIME_SETTINGS_KEY = "aime/hide_workflow_guide"
_AIME_WORKFLOW_TITLE = "AIME Workflow Guide"


class AddIntensitiesMultipleExp(QMainWindow):

    # Window-specific column indices
    COL_INDEX = 0    # frame-index group label
    COL_EXP   = 1    # experiment directory basename

    # Visual style for group cells
    _GROUP_BG = QColor(100, 149, 237)   # cornflower blue
    _GROUP_FG = QColor(255, 255, 255)

    def __init__(self):
        super().__init__()
        self.setWindowTitle("Muscle X Add Intensities Multiple Experiments v." + __version__)
        self._current_inv_transform = None
        self.workspace = ProcessingWorkspace(
            settings_dir="",
            coord_transform_func=self._display_to_original_coords,
        )
        self._current_center = None
        self._current_rotation = None
        self._overlay_lines = []

        self._span_col = self.COL_INDEX

        # Background thread pool for single-image geometry
        self._threadPool = QThreadPool()
        self._threadPool.setMaxThreadCount(1)

        # Multiprocessing sum-images
        from musclex.utils.task_manager import ProcessingTaskManager
        self.sumTaskManager = ProcessingTaskManager()
        self.sumExecutor = None
        self._in_sum_batch = False
        self.stop_process = False
        self._sum_csv_rows = []
        self._sum_nonmasked_pixels = 0
        self._sum_blank_weight = 1.0

        # Result tab state
        self._result_entries: list = []
        self._parent_dir: str = ""

        self._cr_dialog = None

        self._build_ui()
        self._row_mapper = CartesianRowMapper(
            self.panel.table, self.COL_INDEX, self.COL_EXP, self.workspace)
        self.panel.set_row_mapper(self._row_mapper)
        self.resize(1400, 800)
        self.show()
        QTimer.singleShot(0, lambda: WorkflowGuideDialog.show_if_needed(
            _AIME_WORKFLOW_TITLE, _AIME_WORKFLOW_HTML, _AIME_SETTINGS_KEY, self))

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        central = QWidget()
        self.setCentralWidget(central)
        root = QVBoxLayout(central)
        root.setContentsMargins(8, 8, 8, 8)
        root.setSpacing(6)

        # Alignment panel (owns table, detection controls, multiprocessing)
        col_map = {
            ColKey.FRAME: 2, ColKey.CENTER: 3, ColKey.CENTER_MODE: 4,
            ColKey.CENTER_DIST: 5, ColKey.AUTO_CENTER: 6,
            ColKey.AUTO_MANUAL_DIST: 7, ColKey.ROTATION: 8,
            ColKey.ROTATION_MODE: 9, ColKey.ROTATION_DIFF: 10,
            ColKey.AUTO_ROTATION: 11, ColKey.AUTO_ROT_DIFF: 12,
            ColKey.SIZE: 13, ColKey.IMAGE_DIFF: 14,
        }
        headers = [
            "Index", "Experiment", "Frame", "Original Center",
            "Center\nMode", "Dist\nfrom Base", "Auto\nCenter",
            "Auto Center\nDifference", "Rotation", "Rotation\nMode",
            "Rot Diff\nfrom Base", "Auto\nRotation",
            "Auto Rot\nDifference", "Size", "Image\nDifference",
        ]
        self.panel = ImageAlignmentWidget(
            workspace=self.workspace,
            row_mapper=None,  # set after _build_ui (needs table reference)
            col_map=col_map,
            headers=headers,
            worker_dir_path="",
        )
        # Column sizing
        header = self.panel.table.horizontalHeader()
        header.setSectionResizeMode(self.COL_INDEX, QHeaderView.Fixed)
        self.panel.table.setColumnWidth(self.COL_INDEX, 52)
        header.setSectionResizeMode(self.COL_EXP, QHeaderView.Interactive)
        self.panel.table.setColumnWidth(self.COL_EXP, 120)

        # Multiple uses the default context menu (no extra items like Group)
        self.panel.connect_default_context_menu()

        # Connect panel signals
        self.panel.rowSelected.connect(self._on_panel_row_selected)
        self.panel.requestSetCenterRotation.connect(
            self._open_center_rotation_dialog)
        self.panel.globalBaseChanged.connect(self._on_global_base_changed)

        # Status bar
        self._statusBar = QStatusBar()
        self._statusBar.addWidget(self.panel.statusLabel)
        self._statusBar.addPermanentWidget(self.panel.progressBar)
        self.setStatusBar(self._statusBar)

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
        self._left_stack.addWidget(self._select_panel)
        self._left_stack.addWidget(self.panel.table)
        self._left_stack.setCurrentIndex(0)
        self.workspace.imageDataReady.connect(self._on_image_data_ready)
        self.workspace.needsReprocess.connect(
            lambda: self._on_image_data_ready(self.workspace._current_image_data)
            if self.workspace._current_image_data is not None else None
        )
        self.workspace.batchSettingsChanged.connect(self.panel.refresh_all_rows)
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

        # (Detection controls are inside self.panel — no manual UI building needed)



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

        # ── Top bar: workflow guide button + tab bar ───────────────────────
        top_bar = QHBoxLayout()
        top_bar.setContentsMargins(0, 0, 0, 0)
        top_bar.setSpacing(8)

        self._tab_bar = QTabBar()
        self._tab_bar.addTab("Group by Index")
        self._tab_bar.addTab("Group by Exp")
        self._tab_bar.addTab("Result")
        self._tab_bar.setExpanding(False)
        top_bar.addWidget(self._tab_bar)
        top_bar.addStretch()

        self._workflow_btn = QPushButton("Workflow Guide")
        self._workflow_btn.setToolTip("Show the step-by-step workflow guide")
        self._workflow_btn.setFixedHeight(26)
        self._workflow_btn.clicked.connect(lambda: WorkflowGuideDialog.show_always(
            _AIME_WORKFLOW_TITLE, _AIME_WORKFLOW_HTML, _AIME_SETTINGS_KEY, self))
        top_bar.addWidget(self._workflow_btn)

        top_bar_widget = QWidget()
        top_bar_widget.setLayout(top_bar)
        root.addWidget(top_bar_widget)

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

        self._right_panel_layout.addWidget(self.panel)
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
            self.panel.table.sortItems(secondary, Qt.AscendingOrder)
            self.panel.table.sortItems(self._span_col, Qt.AscendingOrder)
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

        self._result_entries.sort(key=lambda e: e['filename'])
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
        self.panel.on_global_base_changed()

    def _auto_set_global_base_if_missing(self):
        """If no global base is recorded yet, set the first image as the default."""
        if self.panel._base_image_filename:
            return
        first_name = self._row_mapper.name_for_row(0)
        if first_name is None:
            return
        self.workspace.settings_manager.set_global_base(first_name)
        self.workspace.settings_manager.save_global_base()
        self.panel.set_base_image(first_name)

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

        self._row_mapper.set_sources(dir_paths)
        self.panel.set_img_sizes(fm.image_sizes)
        self.panel._compute_diff_percentile_threshold()
        self.panel.on_global_base_changed()

        self._result_entries = []

        self.panel.init_table()
        self._auto_set_global_base_if_missing()
        self._left_stack.setCurrentIndex(1)
        self.workspace.navigator.switch_to_image_by_index(0)
        self._sync_table_selection()

    def _rebuild_spans(self):
        """Delegate span rebuilding to the CartesianRowMapper."""
        self._row_mapper.rebuild_spans(
            self.panel.table, self._span_col)

    # ------------------------------------------------------------------
    # Center/Rotation dialog (reparent viewer + settings into popup)
    # ------------------------------------------------------------------

    def _open_center_rotation_dialog(self, row):
        """Open a non-modal dialog with image viewer and center/rotation settings."""
        if self._cr_dialog is not None and self._cr_dialog.isVisible():
            self._cr_dialog.raise_()
            return

        fm_idx = self._row_mapper.fm_index_for_row(row)
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
        base_center = self.panel.get_base_center()
        table_row = self._row_mapper.row_for_fm_index(fm_idx)
        if table_row is not None:
            name = self._row_mapper.name_for_row(table_row)
            if name:
                display_img = self._apply_center_shift_if_needed(display_img, center, name)
        self._current_inv_transform = self._build_display_inv_transform(
            center, rotation, base_center
        )
        self.image_viewer.display_image(display_img)
        self._redraw_overlays()

    # ------------------------------------------------------------------
    # Panel row selection → navigate to image
    # ------------------------------------------------------------------

    def _on_panel_row_selected(self, row):
        """Navigate to the image corresponding to the selected row."""
        fm_idx = self._row_mapper.fm_index_for_row(row)
        if fm_idx is None:
            return
        self.panel.set_navigating(True)
        try:
            self.workspace.navigator.switch_to_image_by_index(fm_idx)
        finally:
            self.panel.set_navigating(False)

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
        table_row = self._row_mapper.row_for_fm_index(fm_idx)
        worker = _GeometryWorker(image_data, table_row if table_row is not None else -1)
        worker.signals.done.connect(
            lambda c, r, i, d=image_data: self._on_geometry_ready(c, r, i, d)
        )
        self._threadPool.start(worker)

    def _on_geometry_ready(self, center, rotation, table_row, image_data=None):
        """Callback (main thread) after background geometry calculation finishes."""
        self._current_center = center
        self._current_rotation = rotation
        if image_data is not None:
            self.workspace.update_display(image_data)
            display_img = image_data.get_working_image()
            if center is not None and rotation is not None and rotation != 0:
                display_img = rotateImageAboutPoint(display_img, center, rotation)
            base_center = self.panel.get_base_center()
            if table_row is not None and table_row >= 0:
                name = self._row_mapper.name_for_row(table_row)
                if name:
                    display_img = self._apply_center_shift_if_needed(display_img, center, name)
            self._current_inv_transform = self._build_display_inv_transform(
                center, rotation, base_center
            )
            self.image_viewer.display_image(display_img)
        self._redraw_overlays()
        if table_row is not None and table_row >= 0:
            name = self._row_mapper.name_for_row(table_row)
            if name:
                self.panel.update_row(table_row, name)

    def _sync_table_selection(self):
        """Highlight the table row that matches the navigator's current FM index."""
        fm_idx = self.workspace.navigator.current_index
        row = self._row_mapper.row_for_fm_index(fm_idx)
        if row is not None:
            self.panel.select_row(row)

    def _apply_center_shift_if_needed(self, img, center, img_name):
        """Translate image so its center aligns with the global base center."""
        if center is None:
            return img
        base_center = self.panel.get_base_center()
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
            base_center = self.panel.get_base_center()
            if base_center is not None:
                bx, by = base_center
                arm = 20
                h_line, = ax.plot([bx - arm, bx + arm], [by, by], color='r', linewidth=1.5)
                v_line, = ax.plot([bx, bx], [by - arm, by + arm], color='r', linewidth=1.5)
                self._overlay_lines.extend([h_line, v_line])

        self.image_viewer.canvas.draw_idle()

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
            if self.panel.table.rowCount() == 0:
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
        if self.panel.table.rowCount() == 0:
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

        # Build index groups from COL_INDEX column (always groups by frame index)
        from collections import defaultdict
        index_groups = defaultdict(list)
        for row in range(self.panel.table.rowCount()):
            item = self.panel.table.item(row, self.COL_INDEX)
            if item is not None:
                index_groups[item.data(Qt.DisplayRole)].append(row)

        # Compute common prefix from all active image basenames to use as default
        all_basenames = []
        for rows in index_groups.values():
            for r in rows:
                if r not in self.panel.ignored_rows:
                    name = self._row_mapper.name_for_row(r)
                    all_basenames.append(os.path.splitext(os.path.basename(name))[0])
        default_base = os.path.commonprefix(all_basenames).rstrip('_') if all_basenames else "output"

        # Ask user for the output base name before starting any heavy work
        base_name, ok = QInputDialog.getText(
            self,
            "Output Base Name",
            "Enter the base name for output files.\n"
            "Files will be named:  <base>_00001.tif,  <base>_00002.tif, …",
            text=default_base,
        )
        if not ok:
            return
        base_name = base_name.strip() or default_base

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
        _bc = self.panel.get_base_center()
        base_center = list(_bc) if _bc else None
        base_rotation = self.panel.get_base_rotation()

        self._in_sum_batch = True
        self.sumTaskManager.clear()

        jobs_submitted = 0
        for group_num, rows in sorted(index_groups.items()):
            active_rows = [r for r in rows if r not in self.panel.ignored_rows]
            if not active_rows:
                print(f"Group {group_num}: all images ignored, skipping.")
                continue

            img_names = [self._row_mapper.name_for_row(r) for r in active_rows]
            fm_indices = [self._row_mapper.fm_index_for_row(r) for r in active_rows]
            specs = [
                fm.specs[fi] if fi is not None and fi < len(fm.specs) else None
                for fi in fm_indices
            ]

            per_img_transforms = []
            for r in active_rows:
                name = self._row_mapper.name_for_row(r)
                center = self.panel.get_effective_center(name)
                rotation = self.panel.get_effective_rotation(name)
                per_img_transforms.append((
                    list(center) if center else None,
                    rotation,
                ))

            filename = f"{base_name}_{str(group_num).zfill(5)}.tif"
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

        self.panel.progressBar.setMaximum(jobs_submitted)
        self.panel.progressBar.setMinimum(0)
        self.panel.progressBar.setValue(0)
        self.panel.progressBar.setVisible(True)
        op = "Averaging" if do_average else "Summing"
        self.panel.statusLabel.setText(f"{op} images: 0/{jobs_submitted} groups...")
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
            self.panel.progressBar.setValue(done)
            op = "Averaging" if self.avg_instead_of_sum_chk.isChecked() else "Summing"
            self.panel.statusLabel.setText(f"{op} images: {done}/{stats['total']} groups...")

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
        self.panel.progressBar.setVisible(False)

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
        self.panel.statusLabel.setText(msg)
        print(msg)

    # ------------------------------------------------------------------
    # Misc
    # ------------------------------------------------------------------

    def setStatus(self, text):
        self.panel.statusLabel.setText(text)
