"""Composite widget for image-alignment detection.

Owns the :class:`ImageAlignmentTable`, threshold controls, the detection
button, and all multiprocessing logic for batch centre/rotation detection
and pairwise image-diff calculation.

The parent window creates the widget, places ``widget.table`` wherever it
likes in its own layout, and connects a handful of signals.  The widget's
own visual body (the detection controls group box) is the widget itself.
"""

import os
import traceback

import numpy as np
from PySide6.QtCore import Qt, QTimer, Signal
from PySide6.QtGui import QBrush
from PySide6.QtWidgets import (
    QCheckBox,
    QDoubleSpinBox,
    QHBoxLayout,
    QLabel,
    QMenu,
    QProgressBar,
    QProgressDialog,
    QPushButton,
    QSizePolicy,
    QVBoxLayout,
    QWidget,
)

from musclex.ui.add_intensities_common import (
    _compute_geometry,
    _compute_geometry_with_symmetry,
    _compute_image_diff,
)
from musclex.ui.widgets import CollapsibleGroupBox
from musclex.ui.widgets.image_alignment_table import ColKey, ImageAlignmentTable
from musclex.utils.task_manager import ProcessingTaskManager


class ImageAlignmentWidget(QWidget):
    """Composite widget: alignment table + detection controls + multiprocessing.

    Parameters
    ----------
    workspace : ProcessingWorkspace
        Shared workspace that owns the navigator, settings manager, etc.
    row_mapper : RowMapper
        Strategy for translating table rows to image identities.
    col_map : dict
        ``{ColKey: int}`` mapping for the table columns.
    headers : list[str]
        Column header labels for the table.
    worker_dir_path : str
        Directory path passed to subprocess workers (``str(fm.dir_path)``
        for Single, ``""`` for Multiple).
    enable_symmetry_test : bool, default False
        When True, an additional "Symmetry std threshold" checkbox + spinbox is
        rendered, a ``FOLD_STD`` column is expected in the table, and batch
        detection will additionally compute the sum-of-std symmetry score for
        each image. Off by default so existing AISE callers are not affected.
    detection_button_position : {"top", "bottom_after_thresholds"}, default "top"
        Where to render the "Detect Centers && Rotations" button inside the
        detection group. "top" preserves the original AISE layout. The QF
        dialog uses ``"bottom_after_thresholds"`` so the button sits below
        every checkbox / threshold control.
    parent : QWidget | None
        Optional parent widget.
    """

    # --- Signals emitted by the widget ------------------------------------
    rowSelected = Signal(int)
    requestSetCenterRotation = Signal(int)
    globalBaseChanged = Signal()
    detectionFinished = Signal()

    def __init__(self, workspace, row_mapper, col_map, headers,
                 worker_dir_path="", enable_symmetry_test=False,
                 detection_button_position="top", parent=None):
        super().__init__(parent)
        self.workspace = workspace
        self._row_mapper = row_mapper
        self._worker_dir_path = worker_dir_path
        self._enable_symmetry_test = bool(enable_symmetry_test)
        if detection_button_position not in ("top", "bottom_after_thresholds"):
            detection_button_position = "top"
        self._detection_button_position = detection_button_position

        # --- State --------------------------------------------------------
        self._img_sizes: dict = {}
        self._most_common_size: str = ""
        self._base_image_filename = None
        self.misaligned_names: set = set()
        self._ignored_rows: set = set()
        self._row_geometry_cache: dict = {}

        # Threshold state
        self._dist_threshold_enabled = True
        self._dist_threshold = 5.0
        self._rot_diff_threshold_enabled = True
        self._rot_diff_threshold = 2.0
        self._diff_percentile_threshold: float = None
        self._diff_thresh_enabled = True
        self._diff_thresh_value = 0.0
        # Symmetry test state — only meaningful when enable_symmetry_test=True.
        # The "Highlight above" checkbox defaults to ON so users see flagged
        # rows automatically once detection finishes; the threshold itself is
        # auto-populated to the 80th-percentile of all fold_std_sum scores in
        # ``_compute_fold_std_percentile_threshold`` when the batch completes.
        self._symmetry_enabled = False
        self._symmetry_thresh_enabled = True
        self._symmetry_threshold = 0.0
        self._fold_std_percentile_threshold: float = None

        # Batch detection state
        self.taskManager = ProcessingTaskManager()
        self.processExecutor = None
        self._in_batch = False
        self.stop_process = False

        # Image-diff state
        self.diffTaskManager = ProcessingTaskManager()
        self.diffExecutor = None
        self._diff_pairs_in_flight: set = set()

        # --- Table (created here, placed by parent) -----------------------
        self.table = ImageAlignmentTable(col_map, headers, parent=None)
        self.table.setContextMenuPolicy(Qt.CustomContextMenu)
        self.table.itemSelectionChanged.connect(self._on_table_selection_changed)

        # Debounce timer for row selection
        self._navigating_from_table = False
        self._nav_debounce_timer = QTimer(self)
        self._nav_debounce_timer.setSingleShot(True)
        self._nav_debounce_timer.setInterval(120)
        self._nav_debounce_timer.timeout.connect(self._do_navigate_to_selected_row)

        # --- Build detection controls UI (this widget's own layout) -------
        self._build_controls_ui()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_controls_ui(self):
        root = QVBoxLayout(self)
        root.setContentsMargins(0, 0, 0, 0)
        root.setSpacing(4)

        self.misaligned_detection_group = CollapsibleGroupBox(
            "Detect Misaligned Images")
        layout = QVBoxLayout()
        layout.setContentsMargins(8, 6, 8, 6)
        layout.setSpacing(4)

        # Build the detection toggle button up-front; it may be inserted at the
        # top *or* at the bottom of ``layout`` depending on configuration.
        self.start_detection_btn = QPushButton(
            "Detect Centers && Rotations")
        self.start_detection_btn.setCheckable(True)
        self.start_detection_btn.setToolTip(
            "Start batch detection of center and rotation angle for all loaded images.\n"
            "If the symmetry test is enabled, the sum-of-std fold symmetry score is "
            "also computed for each image.\n"
            "Click again (Stop) to cancel the running batch.")
        self.start_detection_btn.setSizePolicy(
            QSizePolicy.Expanding, QSizePolicy.Fixed)

        if self._detection_button_position == "top":
            detection_row = QHBoxLayout()
            detection_row.addWidget(self.start_detection_btn)
            layout.addLayout(detection_row)

        # Distance threshold
        dist_row = QHBoxLayout()
        dist_row.setSpacing(6)
        self._dist_thresh_chk = QCheckBox("Auto Diff threshold:")
        self._dist_thresh_chk.setChecked(True)
        self._dist_thresh_chk.setToolTip(
            "Highlight images whose center deviates from the base by more than the threshold.\n"
            "Uncheck to disable this highlighting.")
        dist_row.addWidget(self._dist_thresh_chk)
        self._dist_thresh_spin = QDoubleSpinBox()
        self._dist_thresh_spin.setRange(0.0, 10000.0)
        self._dist_thresh_spin.setDecimals(2)
        self._dist_thresh_spin.setSingleStep(0.5)
        self._dist_thresh_spin.setValue(self._dist_threshold)
        self._dist_thresh_spin.setSuffix(" px")
        self._dist_thresh_spin.setEnabled(True)
        self._dist_thresh_spin.setFixedWidth(100)
        dist_row.addWidget(self._dist_thresh_spin)
        dist_row.addStretch()
        layout.addLayout(dist_row)

        # Rotation threshold
        rot_row = QHBoxLayout()
        rot_row.setSpacing(6)
        self._rot_diff_thresh_chk = QCheckBox("Auto-Rot Diff threshold:")
        self._rot_diff_thresh_chk.setChecked(True)
        self._rot_diff_thresh_chk.setToolTip(
            "Highlight images whose auto-detected rotation angle differs from the base by more than the threshold.\n"
            "Uncheck to disable this highlighting.")
        rot_row.addWidget(self._rot_diff_thresh_chk)
        self._rot_diff_thresh_spin = QDoubleSpinBox()
        self._rot_diff_thresh_spin.setRange(0.0, 360.0)
        self._rot_diff_thresh_spin.setDecimals(2)
        self._rot_diff_thresh_spin.setSingleStep(0.5)
        self._rot_diff_thresh_spin.setValue(self._rot_diff_threshold)
        self._rot_diff_thresh_spin.setSuffix(" °")
        self._rot_diff_thresh_spin.setEnabled(True)
        self._rot_diff_thresh_spin.setFixedWidth(100)
        rot_row.addWidget(self._rot_diff_thresh_spin)
        rot_row.addStretch()
        layout.addLayout(rot_row)

        # Image-diff threshold
        diff_row = QHBoxLayout()
        diff_row.setSpacing(6)
        self._diff_thresh_chk = QCheckBox(
            "Image diff threshold (default: 80th pct):")
        self._diff_thresh_chk.setChecked(True)
        self._diff_thresh_chk.setToolTip(
            "Highlight images whose pairwise image-diff score exceeds this threshold.\n"
            "The default is automatically set to the 80th percentile of all diff scores.\n"
            "Uncheck to disable this highlighting.")
        diff_row.addWidget(self._diff_thresh_chk)
        self._diff_thresh_spin = QDoubleSpinBox()
        self._diff_thresh_spin.setRange(0.0, 1e9)
        self._diff_thresh_spin.setDecimals(1)
        self._diff_thresh_spin.setSingleStep(1.0)
        self._diff_thresh_spin.setValue(self._diff_thresh_value)
        self._diff_thresh_spin.setEnabled(True)
        self._diff_thresh_spin.setFixedWidth(120)
        diff_row.addWidget(self._diff_thresh_spin)
        diff_row.addStretch()
        layout.addLayout(diff_row)

        # Symmetry test row — only rendered when symmetry detection is enabled.
        if self._enable_symmetry_test:
            sym_row = QHBoxLayout()
            sym_row.setSpacing(6)
            self._symmetry_enable_chk = QCheckBox("Run symmetry test on detection")
            self._symmetry_enable_chk.setChecked(False)
            self._symmetry_enable_chk.setToolTip(
                "When checked, batch detection additionally computes the sum of "
                "per-pixel standard deviation across the 4 quadrants (after "
                "centering and rotating each image, before actual folding).\n"
                "A lower score means a more symmetric image.")
            sym_row.addWidget(self._symmetry_enable_chk)

            self._symmetry_thresh_chk = QCheckBox("Highlight above:")
            # Pre-checked so highlighting kicks in automatically once the
            # symmetry test has populated values — the master toggle below
            # still gates the *Enabled* state until the test is turned on.
            self._symmetry_thresh_chk.setChecked(True)
            self._symmetry_thresh_chk.setEnabled(False)
            self._symmetry_thresh_chk.setToolTip(
                "Highlight rows whose fold std-sum exceeds this value.\n"
                "After detection completes, the threshold is auto-populated "
                "to the 80th percentile of all scores; tweak as needed.")
            sym_row.addWidget(self._symmetry_thresh_chk)

            self._symmetry_thresh_spin = QDoubleSpinBox()
            self._symmetry_thresh_spin.setRange(0.0, 1e12)
            self._symmetry_thresh_spin.setDecimals(2)
            self._symmetry_thresh_spin.setSingleStep(100.0)
            self._symmetry_thresh_spin.setValue(self._symmetry_threshold)
            self._symmetry_thresh_spin.setEnabled(False)
            self._symmetry_thresh_spin.setFixedWidth(140)
            sym_row.addWidget(self._symmetry_thresh_spin)
            sym_row.addStretch()
            layout.addLayout(sym_row)

        # Place the detection button at the bottom of the controls if requested.
        if self._detection_button_position == "bottom_after_thresholds":
            detection_row = QHBoxLayout()
            detection_row.addWidget(self.start_detection_btn)
            layout.addLayout(detection_row)

        # Wire threshold signals
        self._dist_thresh_chk.toggled.connect(
            self._on_dist_threshold_toggled)
        self._dist_thresh_spin.valueChanged.connect(
            self._on_dist_threshold_changed)
        self._rot_diff_thresh_chk.toggled.connect(
            self._on_rot_diff_threshold_toggled)
        self._rot_diff_thresh_spin.valueChanged.connect(
            self._on_rot_diff_threshold_changed)
        self._diff_thresh_chk.toggled.connect(
            self._on_diff_thresh_toggled)
        self._diff_thresh_spin.valueChanged.connect(
            self._on_diff_thresh_changed)

        if self._enable_symmetry_test:
            self._symmetry_enable_chk.toggled.connect(
                self._on_symmetry_enable_toggled)
            self._symmetry_thresh_chk.toggled.connect(
                self._on_symmetry_thresh_toggled)
            self._symmetry_thresh_spin.valueChanged.connect(
                self._on_symmetry_thresh_changed)

        self.misaligned_detection_group.set_content_layout(layout)
        root.addWidget(self.misaligned_detection_group)

        # Detection button signal
        self.start_detection_btn.toggled.connect(
            self._on_detection_btn_toggled)

        # Progress bar and status label (for parent to place in a status bar)
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusLabel = QLabel("")

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def set_row_mapper(self, row_mapper):
        """Replace the row mapper (e.g. after deferred CartesianRowMapper creation)."""
        self._row_mapper = row_mapper

    def set_worker_dir_path(self, path: str):
        """Update the worker directory path."""
        self._worker_dir_path = path

    def init_table(self):
        """Populate the table via the row mapper and refresh all data columns."""
        self._ignored_rows = set()
        self._row_geometry_cache = {}
        self.table.setRowCount(0)
        self._row_mapper.populate_table(self.table)
        sm = self.workspace.settings_manager
        for row in range(self.table.rowCount()):
            name = self._row_mapper.name_for_row(row)
            if name is not None:
                self._update_row_data(row, name)
                if sm.has_ignore(name):
                    self._apply_ignore(row)
        self.table.resizeColumnsToContents()

    def update_row(self, row, name):
        """Refresh a single row's data columns (called by parent after geometry ready)."""
        self._update_row_data(row, name)

    def refresh_all_rows(self):
        """Refresh data columns for all existing rows."""
        self._update_table_data()

    def set_img_sizes(self, sizes: dict):
        """Store image sizes and recompute most-common size."""
        self._img_sizes = sizes
        self._compute_most_common_size()

    def set_base_image(self, filename):
        """Set the base image filename and refresh the table."""
        self._base_image_filename = filename
        self._update_table_data()

    def set_misaligned_names(self, names):
        """Store misaligned names and rebuild the table."""
        self.misaligned_names = set(names)
        self.init_table()

    def on_global_base_changed(self):
        """Re-read the global base from settings and refresh the table."""
        base = self.workspace.settings_manager.get_global_base()
        self._base_image_filename = base.get('base_image')
        self._update_table_data()

    @property
    def ignored_rows(self) -> set:
        return self._ignored_rows

    @property
    def is_detecting(self) -> bool:
        return self._in_batch

    def get_effective_center(self, name):
        """Public accessor for effective center (manual or auto)."""
        return self._get_effective_center(name)

    def get_effective_rotation(self, name):
        """Public accessor for effective rotation (manual or auto)."""
        return self._get_effective_rotation(name)

    def get_base_center(self):
        """Public accessor for the base image's effective center."""
        return self._get_base_center()

    def get_base_rotation(self):
        """Public accessor for the base image's effective rotation."""
        return self._get_base_rotation()

    # ------------------------------------------------------------------
    # Data resolution helpers
    # ------------------------------------------------------------------

    def _update_row_data(self, row, name):
        """Refresh center/rotation data columns for a single row."""
        if row < 0 or row >= self.table.rowCount():
            return
        sm = self.workspace.settings_manager

        manual_c = sm.get_center(name)
        if manual_c is not None:
            self.table.fill_center(row, manual_c, "Manual")
        else:
            auto_c = sm.get_auto_center(name)
            self.table.fill_center(row, auto_c, "Auto" if auto_c else None)

        auto_center = sm.get_auto_center(name)
        self.table.fill_auto_center(row, auto_center)

        effective_center = self._get_effective_center(name)
        self.table.fill_auto_manual_dist(
            row, auto_center, effective_center,
            self._dist_threshold_enabled, self._dist_threshold,
        )

        manual_r = sm.get_rotation(name)
        if manual_r is not None:
            self.table.fill_rotation(row, manual_r, "Manual")
        else:
            auto_r = sm.get_auto_rotation(name)
            self.table.fill_rotation(row, auto_r, "Auto" if auto_r else None)

        auto_rotation = sm.get_auto_rotation(name)
        self.table.fill_auto_rotation(row, auto_rotation)

        effective_rotation = self._get_effective_rotation(name)
        self.table.fill_auto_rot_diff(
            row, auto_rotation, effective_rotation,
            self._rot_diff_threshold_enabled, self._rot_diff_threshold,
        )

        self.table.fill_distance_deviation(
            row, effective_center, effective_rotation,
            self._get_base_center(), self._get_base_rotation(),
        )

        self.table.fill_size(
            row, self._img_sizes.get(name, ""), self._most_common_size)

        diff_val = sm.get_image_diff(name)
        self.table.fill_diff(
            row, diff_val, self._diff_thresh_enabled, self._diff_thresh_value)

        if self._enable_symmetry_test and ColKey.FOLD_STD in self.table._col:
            sym_val = sm.get_fold_std_sum(name) if hasattr(
                sm, 'get_fold_std_sum') else None
            self.table.fill_fold_std(
                row, sym_val,
                self._symmetry_thresh_enabled, self._symmetry_threshold,
            )

        self.table.apply_misaligned_highlight(
            row, name, self.misaligned_names)
        self.table.apply_base_marker(
            row, name, self._base_image_filename)
        if row in self._ignored_rows:
            self.table.dim_row(row)

        new_geom = (effective_center, effective_rotation)
        if self._row_geometry_cache.get(row) != new_geom:
            self._row_geometry_cache[row] = new_geom
            self._trigger_diff_for_row(row)

    def _update_table_data(self):
        """Refresh center/rotation data columns for all existing rows."""
        for row in range(self.table.rowCount()):
            name = self._row_mapper.name_for_row(row)
            if name is not None:
                self._update_row_data(row, name)

    def _get_effective_center(self, name):
        """Return (cx, cy) — manual if present, else auto, else None."""
        sm = self.workspace.settings_manager
        return sm.get_center(name) or sm.get_auto_center(name)

    def _get_effective_rotation(self, name):
        """Return rotation angle — manual if present, else auto, else None."""
        sm = self.workspace.settings_manager
        return sm.get_rotation(name) or sm.get_auto_rotation(name)

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

    def _compute_most_common_size(self):
        """Count image sizes and cache the most frequent one."""
        from collections import Counter
        counts = Counter(s for s in self._img_sizes.values() if s)
        self._most_common_size = counts.most_common(1)[0][0] if counts else ""

    def _compute_diff_percentile_threshold(self):
        """Compute the 80th-percentile of all cached diff values.

        Updates ``_diff_percentile_threshold`` and syncs the spinbox.
        """
        sm = self.workspace.settings_manager
        values = [
            sm.get_image_diff(self._row_mapper.name_for_row(r))
            for r in range(self._row_mapper.row_count())
            if self._row_mapper.name_for_row(r) is not None
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

    def _compute_fold_std_percentile_threshold(self):
        """Auto-populate the symmetry highlight spinbox with the 80th-percentile
        of all cached fold-std-sum scores.

        Mirrors :meth:`_compute_diff_percentile_threshold` but for the FOLD_STD
        column. Called from :meth:`_on_batch_complete` after a symmetry-enabled
        run so users immediately see meaningful highlighting without having to
        guess a threshold by hand. Skipped silently when fewer than 2 scores
        are available (percentile is ill-defined and the default 0.0 from
        construction stays in place).
        """
        if not getattr(self, '_enable_symmetry_test', False):
            return
        sm = self.workspace.settings_manager
        if not hasattr(sm, 'get_fold_std_sum'):
            return
        values = [
            sm.get_fold_std_sum(self._row_mapper.name_for_row(r))
            for r in range(self._row_mapper.row_count())
            if self._row_mapper.name_for_row(r) is not None
        ]
        values = [v for v in values if v is not None]
        if len(values) >= 2:
            self._fold_std_percentile_threshold = float(np.percentile(values, 80))
            self._symmetry_thresh_spin.blockSignals(True)
            self._symmetry_thresh_spin.setValue(self._fold_std_percentile_threshold)
            self._symmetry_thresh_spin.blockSignals(False)
            self._symmetry_threshold = self._fold_std_percentile_threshold
        else:
            self._fold_std_percentile_threshold = None

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

    # --- Symmetry threshold callbacks (only active when enable_symmetry_test) -

    def _on_symmetry_enable_toggled(self, checked):
        """Master toggle: when off, the symmetry test is skipped during detection
        and the highlight threshold controls are disabled. When on, mirror the
        current ``Highlight above`` checkbox state into the spinbox so the
        default-checked highlight becomes immediately usable."""
        self._symmetry_enabled = bool(checked)
        self._symmetry_thresh_chk.setEnabled(self._symmetry_enabled)
        if self._symmetry_enabled:
            self._symmetry_thresh_enabled = self._symmetry_thresh_chk.isChecked()
            self._symmetry_thresh_spin.setEnabled(self._symmetry_thresh_enabled)
        else:
            self._symmetry_thresh_chk.setChecked(False)
            self._symmetry_thresh_spin.setEnabled(False)
            self._symmetry_thresh_enabled = False
        self._apply_threshold_highlighting()

    def _on_symmetry_thresh_toggled(self, checked):
        self._symmetry_thresh_enabled = bool(checked)
        self._symmetry_thresh_spin.setEnabled(self._symmetry_thresh_enabled)
        self._apply_threshold_highlighting()

    def _on_symmetry_thresh_changed(self, value):
        self._symmetry_threshold = float(value)
        if self._symmetry_thresh_enabled:
            self._apply_threshold_highlighting()

    def _apply_threshold_highlighting(self):
        """Delegate threshold highlighting to the table widget."""
        self.table.apply_threshold_highlighting(
            self._dist_threshold_enabled, self._dist_threshold,
            self._rot_diff_threshold_enabled, self._rot_diff_threshold,
            self._diff_thresh_enabled, self._diff_thresh_value,
            symmetry_enabled=self._symmetry_thresh_enabled,
            symmetry_thresh=self._symmetry_threshold,
        )

    # ------------------------------------------------------------------
    # Ignore management
    # ------------------------------------------------------------------

    def _apply_ignore(self, row):
        """Mark row as ignored: dim its text and add to ignored set."""
        if row < 0 or row >= self.table.rowCount():
            return
        name = self._row_mapper.name_for_row(row)
        if name is None:
            return
        self._ignored_rows.add(row)
        sm = self.workspace.settings_manager
        sm.set_ignore(name)
        sm.save_ignore()
        print(f"Ignore: {name}")
        self.table.dim_row(row)

    def _clear_ignore(self, row):
        """Remove the ignore flag and restore normal text colour."""
        if row < 0 or row >= self.table.rowCount():
            return
        name = self._row_mapper.name_for_row(row)
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
    # Context menu (common items)
    # ------------------------------------------------------------------

    def connect_default_context_menu(self):
        """Connect the built-in context menu handler to the table.

        Call this if the parent does not need to customise the menu.
        Parents that add extra items (e.g. Group/Ungroup) should connect
        their own handler to ``table.customContextMenuRequested`` instead.
        """
        self.table.customContextMenuRequested.connect(self._on_context_menu)

    def _on_context_menu(self, pos):
        row = self.table.rowAt(pos.y())
        if row < 0:
            return

        global_pos = self.table.viewport().mapToGlobal(pos)
        menu = QMenu(self)

        selected_rows = sorted(
            set(idx.row() for idx in self.table.selectedIndexes()))

        set_cr_act = None
        set_global_act = None
        if len(selected_rows) == 1:
            set_cr_act = menu.addAction("Set Center and Rotation")
            set_global_act = menu.addAction("Set as Global Base")
            menu.addSeparator()

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
            self.requestSetCenterRotation.emit(selected_rows[0])
        elif chosen == set_global_act:
            r = selected_rows[0]
            img_name = self._row_mapper.name_for_row(r)
            if img_name is not None:
                self.workspace.settings_manager.set_global_base(img_name)
                self.workspace.settings_manager.save_global_base()
                self.on_global_base_changed()
                self.globalBaseChanged.emit()

    # ------------------------------------------------------------------
    # Table selection → debounced signal
    # ------------------------------------------------------------------

    def _on_table_selection_changed(self):
        if self._navigating_from_table:
            return
        self._nav_debounce_timer.start()

    def _do_navigate_to_selected_row(self):
        if self._navigating_from_table:
            return
        selected_rows = set(
            idx.row() for idx in self.table.selectedIndexes())
        if len(selected_rows) != 1:
            return
        row = self.table.currentRow()
        if row < 0 or row >= self.table.rowCount():
            return
        fm_idx = self._row_mapper.fm_index_for_row(row)
        if fm_idx is None:
            return
        self.rowSelected.emit(row)

    def set_navigating(self, value: bool):
        """Guard against re-entrant navigation from programmatic row selection."""
        self._navigating_from_table = value

    def select_row(self, row: int):
        """Programmatically select a table row without triggering navigation."""
        if row < 0 or row >= self.table.rowCount():
            return
        self._navigating_from_table = True
        try:
            self.table.blockSignals(True)
            self.table.selectRow(row)
            self.table.scrollTo(self.table.model().index(row, 0))
            self.table.blockSignals(False)
        finally:
            self._navigating_from_table = False

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
            self.processExecutor = ProcessPoolExecutor(
                max_workers=worker_count, mp_context=mp_ctx)
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

        msg = (f"Stopping Batch Processing\n\n"
               f"Waiting for {running_count} tasks to complete...")
        self._stopProgress = QProgressDialog(msg, None, 0, 0, self)
        self._stopProgress.setWindowFlags(
            Qt.Window | Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint)
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
        msg = (f"Stopping Batch Processing\n\n"
               f"Waiting for {running_count} tasks to complete...")
        self._stopProgress.setLabelText(msg)

        if running_count == 0:
            self._stopMsgTimer.stop()
            self._stopProgress.close()
            self._stopProgress = None
            self._on_batch_complete(stopped=True)

    def _start_detection(self):
        """Submit all images to the process pool for center/rotation calculation."""
        if self._in_batch:
            return
        n = self._row_mapper.row_count()
        if n == 0:
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

        # Snapshot the symmetry-test toggle for the duration of this batch so
        # the user toggling it mid-run does not desync workers and result-
        # handling logic.
        self._batch_do_symmetry = bool(
            self._enable_symmetry_test and self._symmetry_enabled)

        self.progressBar.setMaximum(n)
        self.progressBar.setMinimum(0)
        self.progressBar.setValue(0)
        self.progressBar.setVisible(True)
        if self._batch_do_symmetry:
            self.statusLabel.setText(
                "Detecting center/rotation + fold symmetry...")
        else:
            self.statusLabel.setText("Detecting center/rotation...")

        orientation_model = getattr(self.workspace, '_orientation_model', 0)

        for row in range(n):
            img_name = self._row_mapper.name_for_row(row)
            if img_name is None:
                continue
            fm_idx = self._row_mapper.fm_index_for_row(row)
            spec = (fm.specs[fm_idx]
                    if fm_idx is not None and fm_idx < len(fm.specs)
                    else None)
            manual_center, manual_rotation = \
                self.workspace.get_manual_settings(img_name)
            if self._batch_do_symmetry:
                job_args = (
                    self._worker_dir_path,
                    img_name,
                    spec,
                    manual_center,
                    manual_rotation,
                    orientation_model,
                    True,
                )
                future = self.processExecutor.submit(
                    _compute_geometry_with_symmetry, job_args)
            else:
                job_args = (
                    self._worker_dir_path,
                    img_name,
                    spec,
                    manual_center,
                    manual_rotation,
                    orientation_model,
                )
                future = self.processExecutor.submit(
                    _compute_geometry, job_args)
            self.taskManager.submit_task(img_name, row, future)
            future.add_done_callback(self._on_future_done)

        print(f"Batch detection started: {n} images submitted "
              f"(symmetry={self._batch_do_symmetry})")

    def _on_future_done(self, future):
        """Route future callback to the main thread via QTimer."""
        QTimer.singleShot(
            0, self, lambda f=future: self._on_batch_result(f))

    def _on_batch_result(self, future):
        """Handle a single completed future in the main thread."""
        try:
            try:
                result = future.result()
                error = result.get('error')
            except Exception as fut_exc:
                error = str(fut_exc)
                result = {
                    'img_name': None, 'center': None,
                    'rotation': None, 'error': error,
                }
            task = self.taskManager.complete_task(future, result, error)
            if task is None:
                return

            # Always persist successful results into the in-memory cache —
            # even if Stop has been pressed — so work that finished before
            # the cancel arrives is not silently lost. ``_on_batch_complete``
            # writes the cache to disk regardless of stop state.
            if not error:
                sm = self.workspace.settings_manager
                sm.set_auto_cache(
                    task.filename,
                    result['center'],
                    result['rotation'],
                )
                # Persist the fold-symmetry score when present (only emitted by
                # the symmetry-aware worker). ``None`` simply means the symmetry
                # test was not requested for this image.
                fold_std = result.get('fold_std_sum')
                if fold_std is not None and hasattr(sm, 'set_fold_std_sum'):
                    sm.set_fold_std_sum(task.filename, fold_std)

            # When the user asked to stop, leave UI updates and the completion
            # check to the stopProcess state machine; data was already cached
            # above so nothing is lost.
            if self.stop_process:
                return

            if error:
                print(f"Detection error for {task.filename}: {error}")

            stats = self.taskManager.get_statistics()
            self.progressBar.setValue(stats['completed'] + stats['failed'])
            self.statusLabel.setText(
                f"Detecting: {stats['completed'] + stats['failed']}"
                f"/{stats['total']}"
            )

            if 0 <= task.job_index < self.table.rowCount():
                name = self._row_mapper.name_for_row(task.job_index)
                if name is not None:
                    self._update_row_data(task.job_index, name)

            if stats['pending'] == 0:
                self._on_batch_complete()

        except Exception as e:
            print(f"Batch result callback error: {e}")
            traceback.print_exc()

    def _on_batch_complete(self, stopped=False):
        """Clean up after all batch tasks have finished or been stopped."""
        stats = self.taskManager.get_statistics()

        # Persist whatever was accumulated in memory regardless of whether
        # the batch ran to completion or was interrupted via Stop. Without
        # this, partial-but-valid results from a stopped run would be lost
        # because ``_on_batch_result`` only writes to the in-memory cache.
        sm = self.workspace.settings_manager
        sm.save_auto_cache()
        ran_symmetry = getattr(self, '_batch_do_symmetry', False)
        if ran_symmetry and hasattr(sm, 'save_fold_std_sum'):
            sm.save_fold_std_sum()
        # When the batch ran with the symmetry test enabled, derive a sensible
        # default highlight threshold (80th percentile of all scores) and push
        # it into the spinbox so flagged rows light up without manual tuning.
        if ran_symmetry:
            self._compute_fold_std_percentile_threshold()
            self._apply_threshold_highlighting()
        # Reset the per-batch flag so subsequent toggles take effect cleanly.
        self._batch_do_symmetry = False

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
                f"Detection stopped: {stats['completed']}/{stats['total']}"
                f" completed, {stats['failed']} failed"
            )
        else:
            msg = (
                f"Detection complete: {stats['completed']}/{stats['total']}"
                f" succeeded, {stats['failed']} failed,"
                f" avg {stats['avg_time']:.2f}s/image"
            )
        self.statusLabel.setText(msg)
        print(msg)
        self.detectionFinished.emit()

    # ------------------------------------------------------------------
    # Image difference calculation
    # ------------------------------------------------------------------

    def _row_has_geometry(self, row):
        """Return True if *row* has a usable center (auto or manual)."""
        if row < 0 or row >= self.table.rowCount():
            return False
        name = self._row_mapper.name_for_row(row)
        return name is not None and self._get_effective_center(name) is not None

    def _trigger_diff_for_row(self, row):
        """Recompute the (at most) two diff pairs that involve *row*."""
        active = [
            i for i in range(self._row_mapper.row_count())
            if i not in self._ignored_rows
        ]
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
            if (not self._row_has_geometry(idx_a)
                    or not self._row_has_geometry(idx_b)):
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
                print(f"Diff process pool initialised with "
                      f"{worker_count} workers (spawn)")
            except Exception as e:
                print(f"Failed to create diff process pool: {e}")
                self.diffExecutor = None

        if self.diffExecutor is None:
            self._diff_pairs_in_flight.discard((idx_a, idx_b))
            return

        name_a = self._row_mapper.name_for_row(idx_a)
        name_b = self._row_mapper.name_for_row(idx_b)
        if name_a is None or name_b is None:
            self._diff_pairs_in_flight.discard((idx_a, idx_b))
            return
        fm_idx_a = self._row_mapper.fm_index_for_row(idx_a)
        fm_idx_b = self._row_mapper.fm_index_for_row(idx_b)
        base_center = self._get_base_center()
        base_rotation = self._get_base_rotation()

        job_args = (
            self._worker_dir_path,
            name_a, name_b,
            (fm.specs[fm_idx_a]
             if fm_idx_a is not None and fm_idx_a < len(fm.specs)
             else None),
            (fm.specs[fm_idx_b]
             if fm_idx_b is not None and fm_idx_b < len(fm.specs)
             else None),
            self._get_effective_center(name_a),
            self._get_effective_rotation(name_a),
            self._get_effective_center(name_b),
            self._get_effective_rotation(name_b),
            list(base_center) if base_center else None,
            base_rotation,
            idx_b,
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
                result = {
                    'pair_index': None, 'diff': None, 'error': error,
                }

            self.diffTaskManager.complete_task(future, result, error)

            name_b = (
                self._row_mapper.name_for_row(idx_b)
                if idx_b < self.table.rowCount() else None
            )
            if error:
                print(f"Diff error for pair ({idx_a}, {idx_b}): {error}")
            elif name_b is not None:
                sm = self.workspace.settings_manager
                sm.set_image_diff(name_b, result['diff'])
                sm.save_image_diff()
                self._compute_diff_percentile_threshold()

            if name_b is not None and idx_b < self.table.rowCount():
                diff_val = self.workspace.settings_manager.get_image_diff(
                    name_b)
                self.table.fill_diff(
                    idx_b, diff_val,
                    self._diff_thresh_enabled, self._diff_thresh_value,
                )

            if (not self._diff_pairs_in_flight
                    and self.diffExecutor is not None):
                self.diffExecutor.shutdown(wait=False)
                self.diffExecutor = None

        except Exception as e:
            print(f"Diff result callback error: {e}")
            traceback.print_exc()

    # ------------------------------------------------------------------
    # Convenience
    # ------------------------------------------------------------------

    def setStatus(self, msg):
        self.statusLabel.setText(msg)
