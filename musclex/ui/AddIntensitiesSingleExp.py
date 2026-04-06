import os
import traceback
import numpy as np
import cv2
import matplotlib.patches as mpatches
from PySide6.QtWidgets import (
    QMainWindow, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QHeaderView, QAbstractItemView, QLabel,
    QSizePolicy, QMenu, QRadioButton, QSpinBox, QWidget, QSplitter,
    QScrollArea, QFrame, QStackedWidget, QCheckBox, QStatusBar,
    QProgressDialog, QMessageBox,
    QTabBar, QDialog, QTextBrowser,
)
from PySide6.QtCore import Qt, QThreadPool, QTimer, QSettings
from PySide6.QtGui import QColor, QBrush
from musclex import __version__
from musclex.ui.widgets import ProcessingWorkspace, CollapsibleGroupBox
from musclex.ui.widgets.image_viewer_widget import ImageViewerWidget
from musclex.utils.image_processor import rotateImageAboutPoint
from musclex.utils.file_manager import load_image_via_spec
from musclex.ui.add_intensities_row_mapper import SingleRowMapper
from musclex.ui.add_intensities_common import (
    _sum_group_worker,
    _GeometryWorkerSignals,
    _GeometryWorker,
)
from musclex.ui.widgets.image_alignment_table import ColKey
from musclex.ui.widgets.image_alignment_widget import ImageAlignmentWidget


_WORKFLOW_HTML = """
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
<h2>AISE Workflow Guide</h2>
<p class="intro">
  Follow these steps to align, inspect and sum your images.
  The <b>global image</b> is the reference to which all other images are aligned.
</p>
<hr/>
<ol>
  <li>
    <span class="step-title">Set the global (reference) image</span><br/>
    <span class="hint">
      By default the first image is the global image.
      Right-click a row in the table and choose <i>Set as Global Image</i> to change it.
    </span>
  </li>
  <li>
    <span class="step-title">Correct the center of the global image</span><br/>
    <span class="hint">
      Select the global image and adjust its center in the viewer.
      Right-click &rarr; <i>Apply to Subsequent Images</i> to propagate the correction.
    </span>
  </li>
  <li>
    <span class="step-title">Correct the orientation (rotation) if needed</span><br/>
    <span class="hint">
      Adjust the rotation of the global image.
      Right-click &rarr; <i>Apply to Subsequent Images</i> to propagate.
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
      Right-click &rarr; <i>Apply to Subsequent Images</i> if the drift is progressive.
      Right-click &rarr; <i>Ignore Image</i> to exclude an image from summation and misalignment detection.
    </span>
  </li>
  <li>
    <span class="step-title">Repeat detection &amp; correction as needed</span><br/>
    <span class="hint">
      Re-run <i>Detect Centers &amp; Rotations</i> after corrections to verify alignment.
    </span>
  </li>
  <li>
    <span class="step-title">Inspect alignment by stepping through images</span><br/>
    <span class="hint">
      Click rows in the table (or use arrow keys) to preview each image in the viewer
      and confirm alignment visually.
    </span>
  </li>
  <li>
    <span class="step-title">Define bins</span><br/>
    <span class="hint">
      <b>Fixed-size bins:</b> set the bin size in the <i>Image Operations</i> panel
      and click <i>Auto Group</i>.<br/>
      <b>Manual bins:</b> highlight the desired rows in the table,
      then right-click &rarr; <i>Group Selected</i>.
    </span>
  </li>
  <li>
    <span class="step-title">Apply operation to bins (Average or Sum)</span><br/>
    <span class="hint">
      Choose <i>Average</i> or <i>Sum</i> in the <i>Image Operations</i> panel,
      then click <i>Sum Images</i>.
    </span>
  </li>
  <li>
    <span class="step-title">Inspect results</span><br/>
    <span class="hint">
      Switch to the <b>Result</b> tab at the top to browse and inspect the output images.
    </span>
  </li>
</ol>
</body>
</html>
"""

_AISE_SETTINGS_KEY = "aise/hide_workflow_guide"


class WorkflowGuideDialog(QDialog):
    """Modal dialog showing the AISE workflow with a 'don't show again' checkbox."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("AISE Workflow Guide")
        self.resize(600, 580)

        layout = QVBoxLayout(self)
        layout.setContentsMargins(12, 12, 12, 12)
        layout.setSpacing(8)

        browser = QTextBrowser()
        browser.setHtml(_WORKFLOW_HTML)
        browser.setOpenExternalLinks(False)
        browser.setReadOnly(True)
        layout.addWidget(browser, 1)

        bottom_row = QHBoxLayout()
        self._dont_show_chk = QCheckBox("Don't show this again")
        bottom_row.addWidget(self._dont_show_chk)
        bottom_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.setDefault(True)
        close_btn.clicked.connect(self._on_close)
        bottom_row.addWidget(close_btn)
        layout.addLayout(bottom_row)

    @staticmethod
    def _settings():
        return QSettings("BioCAT", "MuscleX")

    def _on_close(self):
        if self._dont_show_chk.isChecked():
            self._settings().setValue(_AISE_SETTINGS_KEY, True)
        self.accept()

    @staticmethod
    def show_if_needed(parent=None):
        """Show the dialog unless the user has suppressed it."""
        if WorkflowGuideDialog._settings().value(_AISE_SETTINGS_KEY, False, type=bool):
            return
        dlg = WorkflowGuideDialog(parent)
        dlg.exec()

    @staticmethod
    def show_always(parent=None):
        """Show the dialog unconditionally (triggered by the toolbar button)."""
        dlg = WorkflowGuideDialog(parent)
        dlg.exec()


class AddIntensitiesSingleExp(QMainWindow):

    # Column indices
    COL_GROUP = 0

    # Visual style for group cells
    _GROUP_BG = QColor(100, 149, 237)   # cornflower blue
    _GROUP_FG = QColor(255, 255, 255)

    def __init__(self):
        super().__init__()
        self.setWindowTitle("Muscle X Add Intensities Single Experiment v." + __version__)
        self._current_inv_transform = None
        self.workspace = ProcessingWorkspace(
            settings_dir="",
            coord_transform_func=self._display_to_original_coords,
        )
        self.img_list = []
        self._row_mapper = SingleRowMapper(lambda: self.img_list)
        self._current_center = None
        self._current_rotation = None
        self._overlay_lines = []

        # Each entry: {'start': int, 'count': int, 'number': int}
        self._groups = []

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
        self._aise_results_dir: str = ""

        self._cr_dialog = None

        self._build_ui()
        self.resize(1400, 800)
        self.show()
        QTimer.singleShot(0, lambda: WorkflowGuideDialog.show_if_needed(self))

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
            ColKey.FRAME: 1, ColKey.CENTER: 2, ColKey.CENTER_MODE: 3,
            ColKey.CENTER_DIST: 4, ColKey.AUTO_CENTER: 5,
            ColKey.AUTO_MANUAL_DIST: 6, ColKey.ROTATION: 7,
            ColKey.ROTATION_MODE: 8, ColKey.ROTATION_DIFF: 9,
            ColKey.AUTO_ROTATION: 10, ColKey.AUTO_ROT_DIFF: 11,
            ColKey.SIZE: 12, ColKey.IMAGE_DIFF: 13,
        }
        headers = [
            "Group", "Frame", "Original Center", "Center\nMode",
            "Dist\nfrom Base", "Auto\nCenter", "Auto Center\nDifference",
            "Rotation", "Rotation\nMode", "Rot Diff\nfrom Base",
            "Auto\nRotation", "Auto Rot\nDifference", "Size",
            "Image\nDifference",
        ]
        self.panel = ImageAlignmentWidget(
            workspace=self.workspace,
            row_mapper=self._row_mapper,
            col_map=col_map,
            headers=headers,
            worker_dir_path="",
        )
        # Group column sizing
        header = self.panel.table.horizontalHeader()
        header.setSectionResizeMode(self.COL_GROUP, QHeaderView.Fixed)
        self.panel.table.setColumnWidth(self.COL_GROUP, 52)

        # Single has Group/Ungroup in context menu — use custom handler
        self.panel.table.customContextMenuRequested.connect(
            self._on_context_menu)

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

        # Left side of splitter: select_panel (before load) / table (after load)
        self._left_stack = QStackedWidget()
        self._left_stack.addWidget(self.workspace.navigator.select_panel)
        self._left_stack.addWidget(self.panel.table)
        self._left_stack.setCurrentIndex(0)
        self.workspace.navigator.fileLoaded.connect(self._on_folder_loaded)
        self.workspace.navigator.scanComplete.connect(self._on_scan_complete)
        self.workspace.imageDataReady.connect(self._on_image_data_ready)
        self.workspace.needsReprocess.connect(
            lambda: self._on_image_data_ready(self.workspace._current_image_data)
            if self.workspace._current_image_data is not None else None
        )
        self.workspace.batchSettingsChanged.connect(self.panel.refresh_all_rows)

        # Right panel container
        right_container = QWidget()
        right_container_layout = QVBoxLayout(right_container)
        right_container_layout.setContentsMargins(0, 0, 0, 0)
        right_container_layout.setSpacing(4)

        # Image viewer in a container (allows reparenting to dialog)
        self.image_viewer = self.workspace.navigator.image_viewer
        self.image_viewer.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Preferred)
        self.image_viewer.setMinimumHeight(200)
        self._viewer_container = QWidget()
        self._viewer_container_layout = QVBoxLayout(self._viewer_container)
        self._viewer_container_layout.setContentsMargins(0, 0, 0, 0)
        self._viewer_container_layout.addWidget(self.image_viewer)
        right_container_layout.addWidget(self._viewer_container, 0)



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
        self._tab_bar.addTab("Origin")
        self._tab_bar.addTab("Result")
        self._tab_bar.setExpanding(False)
        top_bar.addWidget(self._tab_bar)
        top_bar.addStretch()

        self._workflow_btn = QPushButton("Workflow Guide")
        self._workflow_btn.setToolTip("Show the step-by-step workflow guide")
        self._workflow_btn.setFixedHeight(26)
        self._workflow_btn.clicked.connect(lambda: WorkflowGuideDialog.show_always(self))
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

        # Grouping mode selector
        _grouping_label = QLabel("Grouping Mode:")
        _grouping_label.setStyleSheet("font-weight: bold; color: gray;")
        _img_ops_layout.addWidget(_grouping_label)

        _grouping_row = QWidget()
        _grouping_row_layout = QHBoxLayout(_grouping_row)
        _grouping_row_layout.setContentsMargins(0, 0, 0, 0)
        _grouping_row_layout.setSpacing(12)
        self.radio_manual = QRadioButton("Select Group Graphically")
        self.radio_manual.setChecked(True)
        self.radio_bin_images = QRadioButton("Bin Images")
        _grouping_row_layout.addWidget(self.radio_manual)
        _grouping_row_layout.addWidget(self.radio_bin_images)
        _grouping_row_layout.addStretch()
        _img_ops_layout.addWidget(_grouping_row)

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
        _img_ops_layout.addWidget(self._binning_row)

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

        self.sum_images_btn = QPushButton("Sum Images")
        self.sum_images_btn.setCheckable(True)
        self.sum_images_btn.setMinimumHeight(32)
        self.sum_images_btn.setStyleSheet(
            "QPushButton { color: #ededed; background-color: #af6207 }")
        self._right_panel_layout.addWidget(self.sum_images_btn)

        self.radio_bin_images.toggled.connect(self._binning_row.setVisible)
        self.radio_bin_images.toggled.connect(self._on_bin_images_toggled)
        self.binning_spin.valueChanged.connect(self._on_binning_factor_changed)

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
        self._main_stack.setCurrentIndex(index)
        if index == 1:
            self._refresh_result_tab()

    def _refresh_result_tab(self):
        """Populate the result table from memory or CSV."""
        if not self._result_entries and self._aise_results_dir:
            csv_path = os.path.join(self._aise_results_dir, 'intensities.csv')
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
        if not self._aise_results_dir:
            return
        full_path = os.path.join(self._aise_results_dir, entry['filename'])
        if not os.path.exists(full_path):
            return
        try:
            img = load_image_via_spec(
                self._aise_results_dir, entry['filename'], ("tiff", full_path))
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
        if self.panel._base_image_filename or not self.img_list:
            return
        first_name = self._row_mapper.name_for_row(0)
        if first_name is None:
            return
        first_name = os.path.basename(first_name)
        self.workspace.settings_manager.set_global_base(first_name)
        self.workspace.settings_manager.save_global_base()
        self.panel.set_base_image(first_name)

    # ------------------------------------------------------------------
    # Folder loaded → switch to table view
    # ------------------------------------------------------------------

    def _on_folder_loaded(self, dir_path):
        """Switch to table view with the initial file list (scan may still be running)."""
        fm = self.workspace.navigator.file_manager
        self.img_list = list(fm.names)
        self.panel.set_worker_dir_path(str(fm.dir_path))
        self.panel.set_img_sizes(fm.image_sizes)
        self.panel._compute_diff_percentile_threshold()
        self.panel.on_global_base_changed()
        self._auto_set_global_base_if_missing()
        self._groups = []
        self.panel.init_table()
        self._sync_table_selection()
        self._left_stack.setCurrentIndex(1)
        self._result_entries = []
        self._aise_results_dir = os.path.join(str(dir_path), "aise_results")

    def _on_scan_complete(self):
        """Refresh the file list once the background scan finishes."""
        fm = self.workspace.navigator.file_manager
        _, ftype, fpath = fm._get_current_file_info()
        if ftype == "h5" and fpath in fm.source_index_map:
            start, end = fm.source_index_map[fpath]
            fm.set_directory_listing(
                fm.dir_path,
                fm.names[start:end + 1],
                fm.specs[start:end + 1],
                source_index_map={fpath: (0, end - start)},
            )
        self.img_list = list(fm.names)
        self.panel.set_worker_dir_path(str(fm.dir_path))
        self.panel.set_img_sizes(fm.image_sizes)
        self.panel._compute_diff_percentile_threshold()
        self.panel.on_global_base_changed()
        self.panel.init_table()
        self._sync_table_selection()

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
        n = self._row_mapper.row_count()
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
        table = self.panel.table
        for row in range(table.rowCount()):
            table.setSpan(row, self.COL_GROUP, 1, 1)
            item = table.item(row, self.COL_GROUP)
            if item:
                item.setText("")
                item.setBackground(QBrush())
                item.setForeground(QBrush())

        for group in self._groups:
            start = group['start']
            count = group['count']
            if count > 1:
                table.setSpan(start, self.COL_GROUP, count, 1)
            item = table.item(start, self.COL_GROUP)
            if item is None:
                item = QTableWidgetItem()
                table.setItem(start, self.COL_GROUP, item)
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
        table = self.panel.table
        row = table.rowAt(pos.y())
        col = table.columnAt(pos.x())
        if row < 0:
            return

        global_pos = table.viewport().mapToGlobal(pos)
        menu = QMenu(self)

        if col == self.COL_GROUP:
            group = self._find_group_at_row(row)
            if group is not None:
                ungroup_act = menu.addAction(f"Ungroup  (Group {group['number']})")
                chosen = menu.exec(global_pos)
                if chosen == ungroup_act:
                    self._ungroup(group)
            return

        selected_rows = sorted(
            set(idx.row() for idx in table.selectedIndexes()))

        set_cr_act = None
        set_global_act = None
        if len(selected_rows) == 1:
            set_cr_act = menu.addAction("Set Center and Rotation")
            set_global_act = menu.addAction("Set as Global Base")
            menu.addSeparator()

        group_act = None
        if len(selected_rows) >= 2:
            group_act = menu.addAction("Group")
            menu.addSeparator()

        n = len(selected_rows)
        label_suffix = f" ({n} images)" if n > 1 else ""
        all_ignored = all(
            r in self.panel.ignored_rows for r in selected_rows)
        if all_ignored:
            ignore_act = menu.addAction(f"Cancel Ignore{label_suffix}")
        else:
            ignore_act = menu.addAction(f"Ignore{label_suffix}")

        chosen = menu.exec(global_pos)
        if chosen is None:
            return
        if chosen == group_act:
            self._group_rows(selected_rows)
        elif chosen == ignore_act:
            if all_ignored:
                for r in selected_rows:
                    self.panel._clear_ignore(r)
            else:
                for r in selected_rows:
                    self.panel._apply_ignore(r)
        elif chosen == set_cr_act:
            self._open_center_rotation_dialog(selected_rows[0])
        elif chosen == set_global_act:
            row = selected_rows[0]
            name = self._row_mapper.name_for_row(row)
            if name is not None:
                img_name = os.path.basename(name)
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

        if row != self.workspace.navigator.current_index:
            self.workspace.navigator.switch_to_image_by_index(row)

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
        outer = QHBoxLayout(dlg)
        outer.setContentsMargins(0, 0, 0, 0)
        outer.addWidget(splitter)

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
        row = self.workspace.navigator.current_index
        center = self._current_center
        rotation = self._current_rotation
        display_img = np.copy(image_data.img)
        if center is not None and rotation is not None and rotation != 0:
            display_img = rotateImageAboutPoint(display_img, center, rotation)
        base_center = self.panel.get_base_center()
        name = self._row_mapper.name_for_row(row)
        if name is not None:
            display_img = self._apply_center_shift_if_needed(
                display_img, center, name
            )
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
            base_center = self.panel.get_base_center()
            name = self._row_mapper.name_for_row(row)
            if name is not None:
                display_img = self._apply_center_shift_if_needed(
                    display_img, center, name
                )
            self._current_inv_transform = self._build_display_inv_transform(
                center, rotation, base_center
            )
            self.image_viewer.display_image(display_img)
        self._redraw_overlays()
        name = self._row_mapper.name_for_row(row)
        if name is not None:
            self.panel.update_row(row, name)

    def _sync_table_selection(self):
        """Highlight the table row that matches the navigator's current image index."""
        idx = self.workspace.navigator.current_index
        self.panel.select_row(idx)

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
            # Validate preconditions before committing the toggle
            if not self._groups:
                QMessageBox.information(self, "Sum Images",
                                        "No groups defined. Please create groups first.")
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
        rotation_mode = 'absolute' if self.radio_rot_absolute.isChecked() else 'diff'

        # Blank config (loaded once, shared across all groups)
        blank_mask_config = self.workspace.get_blank_mask_config()
        apply_blank = blank_mask_config['apply_blank']
        blank_weight = blank_mask_config['blank_weight']
        blank_img = None
        mask_for_csv = None
        from musclex.utils.file_manager import getBlankImageAndMask
        if apply_blank:
            blank_img, mask_for_csv, _ = getBlankImageAndMask(dir_path, return_weight=True)
        else:
            _, mask_for_csv, _ = getBlankImageAndMask(dir_path, return_weight=True)

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

        sorted_groups = sorted(self._groups, key=lambda g: g['start'])
        jobs_submitted = 0

        for group in sorted_groups:
            start = group['start']
            count = group['count']
            group_num = group['number']

            active_rows = [
                r for r in range(start, start + count)
                if r not in self.panel.ignored_rows
            ]
            if not active_rows:
                print(f"Group {group_num}: all images ignored, skipping.")
                continue

            img_names = [os.path.basename(self._row_mapper.name_for_row(r) or '') for r in active_rows]
            fm_indices = [self._row_mapper.fm_index_for_row(r) for r in active_rows]
            specs = [fm.specs[fi] if fi is not None and fi < len(fm.specs) else None for fi in fm_indices]

            per_img_transforms = []
            for r in active_rows:
                name = self._row_mapper.name_for_row(r)
                center = self.panel.get_effective_center(name) if name else None
                rotation = self.panel.get_effective_rotation(name)
                per_img_transforms.append((
                    list(center) if center else None,
                    rotation,
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
        """Write accumulated CSV statistics to aise_results/intensities.csv."""
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
        self.sum_images_btn.setText("Sum Images")
        self.sum_images_btn.blockSignals(False)

        fm = self.workspace.navigator.file_manager
        if fm is not None and self._sum_csv_rows:
            output_dir = os.path.join(str(fm.dir_path), "aise_results")
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
