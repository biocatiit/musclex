"""
@file QFAlignmentDialog.py
@description Alignment and image-difference detection dialog for the QF module.

- Reuses AISE's :class:`ImageAlignmentWidget` without the AISE-specific business
  logic (grouping, summation, Result Tab).
- Shares the :class:`ProcessingWorkspace` owned by :class:`QuadrantFoldingGUI`:
  * settings_manager (center / rotation / global_base / auto_cache / image_diff / ignore)
  * navigator.file_manager (specs / names / image_sizes)
- Changes made inside this dialog (detection results, ignore, global base) are
  written back to settings_manager; the ``alignmentChanged`` signal notifies the
  QF main window to reprocess with the latest settings.

@usage
    dlg = QFAlignmentDialog(workspace, parent=qf_gui)
    dlg.alignmentChanged.connect(qf_gui.processImage)
    dlg.show()  # non-modal so the user can interact with both windows
"""

from __future__ import annotations

import logging
import os

from PySide6.QtCore import Qt, QTimer, Signal
from PySide6.QtWidgets import (
    QDialog,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QMessageBox,
    QPushButton,
    QStatusBar,
    QVBoxLayout,
)

from musclex.ui.add_intensities_row_mapper import SourceFolderRowMapper
from musclex.ui.widgets.image_alignment_table import ColKey
from musclex.ui.widgets.image_alignment_widget import ImageAlignmentWidget
from musclex.utils.settings_manager import SettingsManager

logger = logging.getLogger(__name__)


def _reload_cached_settings_managers(settings_manager_cache):
    """Reload source-folder settings that may have changed elsewhere.

    The QF workspace and alignment dialog can own different ``SettingsManager``
    instances for the same source folder.  Refinement and propagation save via
    the workspace instance, so the dialog must refresh its cached instances
    before displaying or snapshotting manual geometry for detection.
    """
    for manager in settings_manager_cache.values():
        manager.load()


def _persist_pending_batch_geometry(workspace, row_mapper, settings_resolver):
    """Persist pending Apply-to-All geometry in every loaded source folder.

    Multi-folder QF processing intentionally keeps Apply Center/Rotation -> All
    as a lazy, workspace-level override.  Batch processing materializes that
    override as each image is submitted, but alignment detection reads each
    source folder directly.  Flush the pending values here so both workflows
    observe the same manual geometry.

    Settings managers are saved once per source folder, even when that folder
    contains many images.
    """
    batch_center, batch_rotation = workspace.get_batch_all_geometry()
    if batch_center is None and batch_rotation is None:
        return

    center_managers = set()
    rotation_managers = set()
    for row in range(row_mapper.row_count()):
        name = row_mapper.name_for_row(row)
        if name is None:
            continue
        manager, key = settings_resolver(row, name)
        if batch_center is not None:
            manager.set_center(key, batch_center, "propagated_batch_folder")
            center_managers.add(manager)
        if batch_rotation is not None:
            manager.set_rotation(key, batch_rotation, "propagated_batch_folder")
            rotation_managers.add(manager)

    for manager in center_managers:
        manager.save_center()
    for manager in rotation_managers:
        manager.save_rotation()


class QFAlignmentDialog(QDialog):
    """
    @class QFAlignmentDialog
    @description Non-modal dialog for image alignment detection inside QF.

    Reuses the external ``ProcessingWorkspace`` so no folder reload or settings
    copy is needed. Designed to be shown non-modally (``show()``) so the user
    can switch freely between this dialog and the QF main window; every settings
    change is broadcast via :attr:`alignmentChanged`.
    """

    # Emitted when global base or detection results may affect QF rendering so
    # the main window can decide whether to reprocess the current image.
    alignmentChanged = Signal()

    def __init__(self, workspace, parent=None):
        """
        @param workspace: The ProcessingWorkspace instance owned by QuadrantFoldingGUI.
        @param parent: Qt parent widget, usually the QuadrantFoldingGUI instance.
        """
        super().__init__(parent)
        self.setWindowTitle("Detect Image Alignment")
        self.resize(1180, 640)
        # Allow minimize/maximize so long-running detections can be backgrounded.
        self.setWindowFlags(self.windowFlags() | Qt.WindowMinMaxButtonsHint)

        self.workspace = workspace
        self._settings_manager_cache = {}
        self._loaded_batch_sources = None
        self._load_selected_batch_sources_if_needed()
        # row == file_manager index (QF does not group images)
        self._row_mapper = SourceFolderRowMapper(self.workspace)

        self._build_ui()
        self._connect_signals()
        self._initialize_panel()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        """Build dialog UI: hint label -> detection controls -> table -> status bar -> Close button."""
        # QF does not need a Group column; Frame starts at column 0.
        # FOLD_STD is appended at the end so the symmetry score is read alongside
        # the existing image-diff metric.
        col_map = {
            ColKey.FOLDER: 0,
            ColKey.FRAME: 1,
            ColKey.CENTER: 2,
            ColKey.CENTER_MODE: 3,
            ColKey.CENTER_DIST: 4,
            ColKey.AUTO_CENTER: 5,
            ColKey.AUTO_MANUAL_DIST: 6,
            ColKey.ROTATION: 7,
            ColKey.ROTATION_MODE: 8,
            ColKey.ROTATION_DIFF: 9,
            ColKey.AUTO_ROTATION: 10,
            ColKey.AUTO_ROT_DIFF: 11,
            ColKey.SIZE: 12,
            ColKey.IMAGE_DIFF: 13,
            ColKey.FOLD_STD: 14,
            ColKey.FOLD_STD_NORM: 15,
        }
        headers = [
            "Folder",
            "Frame",
            "Current Applied\nCenter",
            "Center\nMode",
            "Dist\nfrom Base",
            "Auto\nCenter",
            "Auto-to-Applied\nDifference",
            "Rotation",
            "Rotation\nMode",
            "Rot Diff\nfrom Base",
            "Auto\nRotation",
            "Auto Rot\nDifference",
            "Size",
            "Image\nDifference",
            "Fold Std\n(sum)",
            "Fold Std\n(norm)",
        ]

        fm = self.workspace.navigator.file_manager
        worker_dir = str(fm.dir_path) if fm and fm.dir_path else ""

        self.panel = ImageAlignmentWidget(
            workspace=self.workspace,
            row_mapper=self._row_mapper,
            col_map=col_map,
            headers=headers,
            worker_dir_path=worker_dir,
            enable_symmetry_test=True,
            detection_button_position="bottom_after_thresholds",
            settings_resolver=self._settings_for_alignment_row,
            detection_preflight=self._prepare_source_geometry_for_detection,
            parent=self,
        )
        # Use the default context menu (Set Center/Rotation, Set Global Base, Ignore).
        self.panel.connect_default_context_menu()

        # Allow the Frame column to be resized interactively.
        header = self.panel.table.horizontalHeader()
        header.setSectionResizeMode(col_map[ColKey.FOLDER], QHeaderView.Interactive)
        header.setSectionResizeMode(col_map[ColKey.FRAME], QHeaderView.Interactive)

        # Brief usage hint at the top of the dialog.
        hint = QLabel(
            "Tips: Click a row to navigate to that image in the QF main window. "
            "Right-click for Set Global Base / Ignore. "
            "Detection also computes the sum of per-pixel std-deviation across "
            "the 4 quadrants by default (lower is more symmetric). After "
            "changing the global base or finishing detection, "
            "the QF main window will automatically reprocess with the updated "
            "settings."
        )
        hint.setWordWrap(True)
        hint.setStyleSheet("color: #555; padding: 2px 0;")

        # Status bar: progress bar + status text.
        self._status_bar = QStatusBar(self)
        self._status_bar.addWidget(self.panel.statusLabel)
        self._status_bar.addPermanentWidget(self.panel.progressBar)

        # Close button (bottom-right).
        self._close_btn = QPushButton("Close")
        self._close_btn.setToolTip(
            "Close this dialog. If detection is running it will be stopped first."
        )
        self._close_btn.clicked.connect(self.accept)
        bottom_row = QHBoxLayout()
        bottom_row.addStretch()
        bottom_row.addWidget(self._close_btn)

        root = QVBoxLayout(self)
        root.setContentsMargins(8, 8, 8, 8)
        root.setSpacing(6)
        root.addWidget(hint)
        # ImageAlignmentWidget renders only the detection controls (CollapsibleGroupBox);
        # the table is a sibling widget placed separately below.
        root.addWidget(self.panel)
        root.addWidget(self.panel.table, 1)
        root.addWidget(self._status_bar)
        root.addLayout(bottom_row)

    # ------------------------------------------------------------------
    # Signal connections
    # ------------------------------------------------------------------

    def _connect_signals(self):
        """Connect panel and navigator signals."""
        # Row selected in table -> switch the QF main window to that image.
        self.panel.rowSelected.connect(self._on_row_selected)
        # Global base change / detection finished -> notify QF to reprocess.
        self.panel.globalBaseChanged.connect(self.alignmentChanged.emit)
        self.panel.detectionFinished.connect(self.alignmentChanged.emit)
        # Right-click Set Center/Rotation: guide user back to the main window.
        self.panel.requestSetCenterRotation.connect(
            self._on_request_set_center_rotation
        )

        # Keep table in sync when the navigator navigates outside of this dialog.
        nav = self.workspace.navigator
        nav.imageChanged.connect(self._sync_selection_from_navigator)
        nav.fileLoaded.connect(self._on_folder_reloaded)

    # ------------------------------------------------------------------
    # Initialisation and synchronisation
    # ------------------------------------------------------------------

    def _initialize_panel(self):
        """Populate the table from the current workspace state and select the active row."""
        self._load_selected_batch_sources_if_needed()
        fm = self.workspace.navigator.file_manager
        if fm is None or not fm.names:
            logger.info("QFAlignmentDialog: file_manager is empty, skipping table init")
            return

        self._prepare_source_geometry_for_detection(refresh_table=False)

        worker_dir = str(fm.dir_path) if fm.dir_path else ""
        self.panel.set_worker_dir_path(worker_dir)
        self.panel.set_img_sizes(getattr(fm, "image_sizes", {}) or {})

        # Compute initial percentile threshold for image-diff (safe if no data yet).
        try:
            self.panel._compute_diff_percentile_threshold()
        except Exception as exc:
            logger.warning("Failed to initialise diff threshold: %s", exc)

        # Sync global base and populate rows.
        self.panel.on_global_base_changed()
        self.panel.init_table()

        # Highlight the row corresponding to the QF main window's current image.
        self._sync_selection_from_navigator()

    def _prepare_source_geometry_for_detection(self, refresh_table=True):
        """Make lazy batch-wide geometry visible to source-folder detection."""
        _reload_cached_settings_managers(self._settings_manager_cache)
        _persist_pending_batch_geometry(
            self.workspace,
            self._row_mapper,
            self._settings_for_alignment_row,
        )
        if refresh_table and hasattr(self, "panel"):
            self.panel.refresh_all_rows()

    def refresh_after_refinement(self, rerun_symmetry=True):
        """Refresh table data after QF refines center/rotation in the main window."""
        self._initialize_panel()
        if rerun_symmetry:
            self.panel.run_detection_with_symmetry()

    def _on_row_selected(self, row: int):
        """
        @description Row selected in table: navigate the QF main window to that image.

        Because navigator is shared, ``switch_to_image_by_index`` triggers
        ``imageDataReady``, which causes the QF main window to reprocess;
        this handler only cares about the navigation itself.
        """
        fm_idx = self._row_mapper.fm_index_for_row(row)
        if fm_idx is None:
            return
        if fm_idx == self.workspace.navigator.current_index:
            return
        # Guard against the re-entrant loop: select_row -> selectionChanged -> rowSelected.
        self.panel.set_navigating(True)
        try:
            self.workspace.navigator.switch_to_image_by_index(fm_idx)
        finally:
            self.panel.set_navigating(False)

    def _sync_selection_from_navigator(self, *_args):
        """
        @description Synchronise the table selection when the navigator switches image.

        ``imageChanged`` carries ``(img, filename, dir_path)`` but this method only
        needs ``current_index``, so ``*_args`` absorbs all positional arguments.
        """
        nav = self.workspace.navigator
        if nav is None or nav.file_manager is None:
            return
        idx = nav.current_index
        if idx is None or idx < 0 or idx >= self.panel.table.rowCount():
            return

        row = self._row_mapper.row_for_fm_index(idx)
        if row is None:
            return
        self.panel.select_row(row)

        # The QF main window may have just changed center/rotation; refresh that row.
        name = self._row_mapper.name_for_row(row)
        if name is not None:
            self.panel.update_row(row, name)

    def _on_folder_reloaded(self, *_args):
        """
        @description Rebuild the table when QF loads a new folder while this dialog is open.

        Defer to the next event-loop tick so the navigator's internal state has
        time to settle before ``init_table`` is called.
        """
        QTimer.singleShot(0, self._initialize_panel)

    def _load_selected_batch_sources_if_needed(self):
        """Use the same flattened FileManager view that QF batch processing uses."""
        parent = self.parent()
        folders = list(getattr(parent, "selected_batch_folders", []) or [])
        if not folders:
            self._loaded_batch_sources = None
            return

        fm = self.workspace.navigator.file_manager
        if fm is None:
            return

        source_key = tuple(str(folder) for folder in folders)
        if self._loaded_batch_sources == source_key and getattr(fm, "names", None):
            return

        try:
            fm.load_from_sources(folders)
            self._loaded_batch_sources = source_key
            self._settings_manager_cache.clear()
        except Exception as exc:
            logger.warning("Failed to load QF batch folders for alignment: %s", exc)

    def _settings_for_alignment_row(self, row, name):
        """Return the source folder SettingsManager and basename key for a row."""
        fm = self.workspace.navigator.file_manager
        if fm is None:
            return self.workspace.settings_manager, name

        fm_idx = self._row_mapper.fm_index_for_row(row)
        if fm_idx is None or fm_idx >= len(fm.specs):
            return self.workspace.settings_manager, name

        spec = fm.specs[fm_idx]
        source_dir = None
        key = os.path.basename(str(name))

        if isinstance(spec, tuple) and len(spec) >= 3 and spec[0] == "h5":
            source_dir = os.path.dirname(str(spec[1]))
        elif isinstance(spec, tuple) and len(spec) >= 2:
            source_path = str(spec[1])
            source_dir = os.path.dirname(source_path)
            key = os.path.basename(source_path)

        if not source_dir:
            return self.workspace.settings_manager, name

        manager = self._settings_manager_cache.get(source_dir)
        if manager is None:
            manager = SettingsManager(source_dir)
            self._settings_manager_cache[source_dir] = manager
        return manager, key

    def _on_request_set_center_rotation(self, row: int):
        """
        @description Right-click 'Set Center and Rotation': switch to that image in the
        QF main window and show a guidance message.

        The QF main window already provides full center/rotation tools; there is
        no need to replicate them here.
        """
        fm_idx = self._row_mapper.fm_index_for_row(row)
        if fm_idx is not None:
            self.workspace.navigator.switch_to_image_by_index(fm_idx)

        QMessageBox.information(
            self,
            "Set Center && Rotation",
            "Please use the tools in the QF main window "
            "(Set Center, Set Rotation, or Center+Rotation) to adjust the "
            "selected image. Changes will be reflected in this table "
            "automatically.",
        )
        # Proactively notify QF in case settings were already altered.
        self.alignmentChanged.emit()

    # ------------------------------------------------------------------
    # Lifecycle
    # ------------------------------------------------------------------

    def closeEvent(self, event):
        """Stop any running detection task before closing to prevent process-pool leaks."""
        try:
            if self.panel.is_detecting:
                logger.info(
                    "QFAlignmentDialog closing while detection is running; stopping"
                )
                self.panel.stopProcess()
        except Exception as exc:
            logger.warning("Failed to stop detection on dialog close: %s", exc)

        # Qt automatically disconnects signals to destroyed objects; no explicit
        # disconnect is needed here, and forcing one risks a RuntimeError during
        # Qt's own teardown sequence.
        super().closeEvent(event)
