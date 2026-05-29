"""
Copyright 1999 Illinois Institute of Technology

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL ILLINOIS INSTITUTE OF TECHNOLOGY BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of Illinois Institute
of Technology shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from Illinois Institute of Technology.
"""

import sys
import json
import traceback
import csv
import copy
import math
from os.path import split, splitext
from pathlib import Path
import matplotlib.patches as patches
import matplotlib.pyplot as plt
import pandas as pd
from PIL import Image
from musclex import __version__
from PySide6.QtCore import QRunnable, QThreadPool, QEventLoop, Signal, QTimer, QSize, QSignalBlocker
from PySide6.QtWidgets import QStyle
from queue import Queue
import fabio
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..utils import ImageData
from ..utils import qf_defaults

from ..modules.QuadrantFolder import QuadrantFolder
from ..csv_manager.QF_CSVManager import QF_CSVManager
from .pyqt_utils import *
from .BlankImageSettings import BlankImageSettings
from .ImageMaskTool import ImageMaskerWindow
# from .DoubleZoomGUI import DoubleZoom
# from .DoubleZoomViewer import DoubleZoom
from .widgets.double_zoom_widget import DoubleZoomWidget
# NOTE: SetCentDialog and SetAngleDialog moved to ImageSettingsPanel
from .ImageBlankDialog import ImageBlankDialog
from .ImageMaskDialog import ImageMaskDialog
from .BackgroundSubtractionDialog import BackgroundSubtractionDialog, _to_metric_text, _fraction_to_percent_for_ui, _percent_to_fraction_for_flags

from ..CalibrationSettings import CalibrationSettings
from threading import Lock
from scipy.ndimage import rotate
from .widgets.navigation_controls import NavigationControls
from .tools.tool_manager import ToolManager
from .tools.chords_center_tool import ChordsCenterTool
from .tools.perpendiculars_center_tool import PerpendicularsCenterTool
from .tools.rotation_tool import RotationTool
from .tools.center_rotate_tool import CenterRotateTool
from .widgets.image_viewer_widget import ImageViewerWidget
from .widgets.collapsible_right_panel import CollapsibleRightPanel
from .widgets.collapsible_groupbox import CollapsibleGroupBox
from .widgets.center_settings_widget import CenterSettingsWidget
from .widgets.rotation_settings_widget import RotationSettingsWidget
from .widgets.blank_mask_settings_widget import BlankMaskSettingsWidget
from .widgets import ProcessingWorkspace
from .base_gui import BaseGUI
from ..utils.background_search import makeFullImage, get_projection, find_i0_i1_peaks_auto, find_m_peak_auto
from ..utils.qf_defaults import parse_optimization_steps
import time
import random
from musclex import __version__

class QuadFoldParams:
    def __init__(self, flags, index, file_manager, parent):
        self.flags = flags
        self.index = index
        self.file_manager = file_manager
        self.parent = parent


class WorkerSignals(QObject):
    finished = Signal()
    error = Signal(object, object)  # (error_traceback, params) for retry support
    result = Signal(object)

class BaseProcessWorker(QRunnable):
    """Base class for off-UI-thread processing with consistent signal handling."""
    
    def __init__(self):
        super().__init__()
        self.signals = WorkerSignals()

    def run(self):
        """Template method - subclasses override _do_work()."""
        try:
            result = self._do_work()
        except Exception as e:
            self._handle_error(e)
        else:
            self._handle_success(result)
        finally:
            self.signals.finished.emit()
    
    def _do_work(self):
        """Override this method in subclasses."""
        raise NotImplementedError
    
    def _handle_error(self, error):
        """Default error handling.

        The ``error`` signal is declared ``Signal(object, object)`` because
        batch processing needs both a user-displayable payload and the
        original ``params`` object (so failed tasks can be re-queued for
        a retry pass). Single-image workers have no ``params`` and emit
        ``None`` in that slot.
        """
        traceback.print_exc()
        payload = {
            'error': str(error),
            'traceback': traceback.format_exc(),
            'image_name': self._get_image_name(),
        }
        self.signals.error.emit(payload, getattr(self, 'params', None))

    def _get_image_name(self):
        """Override in subclasses to provide a filename for error messages."""
        return ''

    def _handle_success(self, result):
        """Default success handling."""
        self.signals.result.emit(result)

class SingleImageWorker(BaseProcessWorker):
    """Run single-image QuadrantFolder processing off the UI thread."""
    
    def __init__(self, quad_fold, flags):
        super().__init__()
        self.quadFold = quad_fold
        self.flags = flags
    
    def _do_work(self):
        # QuadrantFolder.process() returns True when it ran the full
        # pipeline, False when it took the fast-path (cached
        # _folded.tif still valid). Stash it on the quadFold instance
        # so the UI thread can decide whether saveBackground() is
        # safe to call without needing to introduce a new signal slot.
        ran_slow_path = bool(self.quadFold.process(self.flags))
        self.quadFold._last_process_was_slow_path = ran_slow_path
        return self.quadFold

    def _get_image_name(self):
        return getattr(self.quadFold, 'img_name', '') or ''


class FolderImageWorker(BaseProcessWorker):
    """Worker thread for processing a single image with QuadrantFolder in batch."""

    BG_SUBTRACTION_DEFAULT = 'Circularly-symmetric'
    
    def __init__(self, params, image_center_settings, image_rotation_settings,
                 bgsub=BG_SUBTRACTION_DEFAULT, bgDict=None, bg_lock=None):
        super().__init__()
        self.params = params
        self.image_center_settings = image_center_settings
        self.image_rotation_settings = image_rotation_settings
        self.bgsub = bgsub
        self.bgDict = bgDict or {}
        self.lock = bg_lock
        self.batchProcessing = getattr(params.parent, "batchProcessing", False)
    
    def _do_work(self):
        """Complete processing pipeline."""
        # Load image
        img = self.params.file_manager.get_image_by_index(self.params.index)
        filename = self.params.file_manager.names[self.params.index]
        
        # Setup ImageData
        img_data = ImageData.from_settings_panel(
            img,
            self.params.file_manager.dir_path,
            filename,
            self.params.parent.workspace
        )
        
        # Resolve output directory from workspace dir_context (mirrors main GUI path)
        ws = getattr(self.params.parent, 'workspace', None)
        qf_output = ws.dir_context.output_dir if ws and ws.dir_context else None

        # Create and process
        self.quadFold = QuadrantFolder(img_data, self.params.parent, output_dir=qf_output)
        self.quadFold.info['bgsub'] = self.bgsub
        self.quadFold.process(self.params.flags)
        
        # Post-processing
        # Note: task-completion logging is done in main thread via thread_done()
        # so it can use workspace.dir_context.output_dir (the user-chosen output dir)
        # instead of the input image directory.
        self._save_background(filename)

        return self.quadFold

    def _get_image_name(self):
        try:
            return self.params.file_manager.names[self.params.index]
        except Exception:
            return ''

    def _save_background(self, filename):
        """Save background image and update shared dictionary."""
        info = self.quadFold.info
        result = self.quadFold.imgCache.get("BgSubFold")
        
        if result is None:
            return
        
        avg_fold = self.quadFold.imgCache.get("avg_fold")
        if avg_fold is None:
            return
        
        background = avg_fold - result
        result_img = makeFullImage(background)
        
        if info.get('rotate'):
            result_img = np.rot90(result_img)
        
        method = info.get('bgsub', 'None')
        if method and method != 'None':
            self._write_background_file(filename, result_img)
            self._update_background_dict(filename, result_img)

    def _write_background_file(self, filename, result_img):
        """Write background file to disk."""
        file_path = self.params.file_manager.dir_path
        bg_dir = os.path.join(file_path, "qf_results", "bg")
        os.makedirs(bg_dir, exist_ok=True)

        result_path = os.path.join(bg_dir, f"{filename}.bg.tif")
        result_img = result_img.astype("float32")
        fabio.tifimage.tifimage(data=result_img).write(result_path)

    def _update_background_dict(self, filename, result_img):
        """Thread-safe update of shared background dictionary."""
        if self.bgDict is not None:
            with self.lock:
                self.bgDict[filename] = np.sum(result_img)

class _BatchImageDataProxy:
    """Minimal stand-in for QuadrantFolder._image_data used by csvManager.

    QF_CSVManager.writeNewData only reads the ``.center`` attribute, so the
    real ImageData (which would need the image array in memory) is not worth
    reconstructing in the GUI process after the worker has already returned
    a final center tuple.
    """
    __slots__ = ('center',)

    def __init__(self, center):
        self.center = center


class _BatchQuadFoldProxy:
    """Lightweight QuadrantFolder stand-in for post-batch persistence.

    The real ``QuadrantFolder`` does its work in a separate process and
    then returns only the serializable bits (``info``, ``center``,
    ``rotation``, etc.). On the GUI side we still need to feed those
    bits into ``QF_CSVManager.writeNewData`` and
    ``_upsert_background_metrics_csv``, both of which access ``info``,
    ``img_name``, ``imgCache``, ``rotation``, ``_image_data.center``,
    and ``orig_image_center`` on a "quadFold" object. This proxy
    provides exactly those fields and nothing else.
    """
    __slots__ = ('img_name', 'info', 'rotation', 'imgCache',
                 '_image_data', 'orig_image_center', 'processing_flags')

    def __init__(self, img_name, info, center, rotation, has_result, processing_flags=None):
        self.img_name = img_name
        self.info = info if isinstance(info, dict) else {}
        self.rotation = rotation if rotation is not None else 0.0
        # csvManager.writeNewData treats 'resultImg' as a presence flag, so a
        # sentinel value is enough — we don't ship the actual ndarray back.
        self.imgCache = {'resultImg': True} if has_result else {}
        self._image_data = _BatchImageDataProxy(center) if center is not None else None
        self.orig_image_center = center
        # csvManager.writeNewData merges these into the row (data | processed_flags).
        self.processing_flags = processing_flags if isinstance(processing_flags, dict) else {}


class EventEmitter(QObject):
    statusTextRequested = Signal(str)


# qfsettings.json <-> widget bindings live in a Qt-free module so unit
# tests can import them without pulling PySide6 / a display. See
# musclex/utils/qf_settings_bindings.py for the source of truth.
from ..utils.qf_settings_bindings import (   # noqa: E402  (top-of-file placement)
    QF_SPINBOX_BINDINGS as _QF_SPINBOX_BINDINGS,
    QF_COMBO_TEXT_BINDINGS as _QF_COMBO_TEXT_BINDINGS,
    QF_CHECKBOX_BINDINGS as _QF_CHECKBOX_BINDINGS,
    classify_qf_setting_key as _classify_qf_setting_key,
    qf_setting_keys as _qf_setting_keys,
)


class QuadrantFoldingGUI(BaseGUI):
    """
    A class for window displaying all information of a selected image.
    This window contains 2 tabs : image, and result
    """
    def __init__(self):
        """
        Initial window
        """

        super().__init__()
        # NOTE: self.file_manager is now initialized by BaseGUI
        self.h5List = [] # if the file selected is an H5 file, regroups all the other h5 files names
        self.filePath = "" # current directory
        self.extent = None
        self.img = None
        self.quadFold = None # QuadrantFolder object
        self.current_image_data = None # Current ImageData object (holds image geometry and preprocessing)
        self.default_img_zoom = None # default zoom calculated after processing image
        self.default_result_img_zoom = None # default result image zoom calculated after processing image
        self.zoomOutClicked = False # to check whether zoom out is clicked for using default zoom value
        self.result_zoom = None # zoom location of result image (x,y range)
        self.function = None # current active function
        self.display_points = None # points to display for the current active function
        self.uiUpdating = False # update ui status flag (prevent recursive)
        self.checkableButtons = [] # list of checkable buttons
        self.updated = {'img': False, 'result': False} # update state of 2 tabs
        self.BGImages = []
        self.calSettings = None
        self.ignoreFolds = set()
        self.csv_bg = None
        # NOTE: orientationModel and modeOrientation now managed by ImageSettingsPanel
        # Access via: self.workspace._orientation_model, self.workspace._mode_rotation
        self.stop_process = False
        self.csvManager = None
        
        self.threadPool = QThreadPool()
        # NOTE: Batch processing now uses a separate multiprocessing
        # ProcessPoolExecutor (see initProcessExecutor / _batchProcessImages).
        # The QThreadPool above is kept for single-image processing
        # (SingleImageWorker) where cross-process pickling would just add
        # latency without benefit.
        self.tasksQueue = Queue()
        self.currentTask = None
        self.worker = None
        self.tasksDone = 0
        self.totalFiles = 1
        self.lock = Lock()

        # Multiprocessing task management (mirrors EquatorWindow)
        from ..utils.task_manager import ProcessingTaskManager
        self.taskManager = ProcessingTaskManager()
        self.processExecutor = None
        self.pendingJobArgs = {}  # job_index -> job_args (for retry)
        self.retryJobArgs = []    # job_args queued for the retry phase
        # User-Stop UX (mirrors EquatorWindow.stopProcess):
        # while a batch is winding down we show an indeterminate progress
        # dialog and poll taskManager.get_running_count() until all
        # in-flight workers have wound down.
        self._stop_initiated = False
        self._stopProgress = None
        self._stopMsgTimer = None
        self.batchProcessing = False  # Flag to indicate batch processing mode
        
        # Retry and statistics tracking
        self.retryQueue = Queue()  # Queue for failed tasks to retry
        self.isRetryPhase = False  # Flag to distinguish retry phase
        self.successCount = 0  # Successful on first attempt
        self.retrySuccessCount = 0  # Successful after retry
        self.retryFailCount = 0  # Failed even after retry
        self.firstAttemptErrors = {}  # {filename: error_message} for first attempt failures
        self.failedTaskErrors = {}  # {filename: error_message} for retry failures
        self.saveErrors = {}  # {filename: error_message} for save failures
        self.imageMaskingTool = None

        # NOTE: setCentDialog and setAngleDialog moved to ImageSettingsPanel

        self.rotationAngle = None

        self.calSettingsDialog = None
        
        # NOTE: imageCenterSettings and imageRotationSettings are now managed by ImageSettingsPanel
        # They are no longer stored here to avoid duplication
        
        self.thresh_mask = None

        # NOTE: Background directory scan is now handled by ImageNavigatorWidget
        # NOTE: _provisionalCount removed - use self.file_manager.is_scan_done() instead

        self.eventEmitter = EventEmitter()
        self.eventEmitter.statusTextRequested.connect(self._set_status_report_text)

        self._singleProcessing = False
        self._singleWorker = None
        # Successful single-image process completions for the image currently shown
        # (reset in _on_image_data_ready). Used e.g. to seed UI once per image.
        self._restoreOptimizeCheckboxAfterProcess = False
        self._OptimizationRunning = False
        self._stopOptimizationRequested = False
        self._persisted_evaluation_baseline = None
        self._persisted_eval_baseline_lock = Lock()
        self._persisted_synthetic_params = None
        self._persisted_synthetic_params_lock = Lock()

        self.initUI() # initial all GUI

        self.setConnections() # set triggered function for widgets
        # self.setMinimumHeight(900)
        self.resize(1200, 900)
        self.newImgDimension = None
        # NOTE: file_manager is now initialized earlier in __init__
        self._on_browse_file()

        # self.mask_min = None
        # self.mask_max = None
        # TODO: review whether these BG-related variables are still needed
        self.bgAsyncDict = {}

    # ===== BaseGUI abstract methods implementation =====
    
    def _setup_window(self):
        """Set window title"""
        from musclex import __version__
        self.setWindowTitle("Muscle X Quadrant Folding v." + __version__)
    
    def _create_tabs(self):
        """Create image tab and result tab"""
        # Create image tab with ProcessingWorkspace
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.imageTabLayout.setContentsMargins(0, 0, 0, 0)
        self.tabWidget.addTab(self.imageTab, "Original Image")
        
        # Create ProcessingWorkspace with select buttons
        # Navigator will show select buttons initially, then hide them after loading
        self.workspace = ProcessingWorkspace(
            settings_dir=self.filePath,
            coord_transform_func=self.getOrigCoordsCenter,
            get_display_center_func=self._get_display_center
        )
        self.imageTabLayout.addWidget(self.workspace, 1)
        
        # Expose navigator as standard attribute for BaseGUI
        self.navigator = self.workspace.navigator
        
        # Expose components for backward compatibility
        self.image_viewer = self.workspace.navigator.image_viewer
        self.file_manager = self.workspace.file_manager
        self.navControls = self.workspace.navigator.nav_controls
        self.right_panel = self.workspace.right_panel
        
        # Expose select buttons from navigator
        self.selectImageButton = self.workspace.navigator.select_image_btn
        
        # Reference to leftWidget for compatibility
        self.leftWidget = self.workspace.navigator.select_panel
        
        # Backward compatibility for display panel controls
        if self.image_viewer.display_panel:
            self.spminInt = self.image_viewer.display_panel.minIntSpnBx
            self.spmaxInt = self.image_viewer.display_panel.maxIntSpnBx
            self.logScaleIntChkBx = self.image_viewer.display_panel.logScaleChkBx
            self.persistIntensity = self.image_viewer.display_panel.persistChkBx
            self.imgZoomInB = self.image_viewer.display_panel.zoomInBtn
            self.imgZoomOutB = self.image_viewer.display_panel.zoomOutBtn
            self.minIntLabel = self.image_viewer.display_panel.minIntLabel
            self.maxIntLabel = self.image_viewer.display_panel.maxIntLabel
        
        # Backward compatibility for axes/canvas/figure
        self.imageAxes = self.image_viewer.axes
        self.imageCanvas = self.image_viewer.canvas
        self.imageFigure = self.image_viewer.figure
        
        # Add quadrant-specific display options
        self._add_display_options()
        
        # Add quadrant-specific settings to right panel
        self._create_quadrant_settings()
        
        # NOTE: Navigation is handled internally by ImageNavigatorWidget
        # QF only listens to imageChanged signal (connected in _additional_setup)
        
        # Create result tab
        self._create_result_tab()
    
    def _create_menu_bar(self):
        """Create menu bar with File and Help menus"""
        selectImageAction = QAction('Select an Image...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.setToolTip("Open an image file to load into Quadrant Folding (Ctrl+I)")
        selectImageAction.triggered.connect(self._on_browse_file)

        saveSettingsAction = QAction('Save Current Settings', self)
        saveSettingsAction.setShortcut('Ctrl+S')
        saveSettingsAction.setToolTip("Save the current center, rotation, ROI and background parameters to a settings file (Ctrl+S)")
        saveSettingsAction.triggered.connect(self.saveSettings)

        loadSettingsAction = QAction('Load Settings...', self)
        loadSettingsAction.setShortcut('Ctrl+Shift+S')
        loadSettingsAction.setToolTip(
            "Load processing parameters from a previously saved qfsettings.json "
            "(Ctrl+Shift+S). Per-image / runtime state is ignored.")
        loadSettingsAction.triggered.connect(self.loadSettings)

        aboutAct = QAction('About', self)
        aboutAct.setToolTip("Show information about MuscleX and Quadrant Folding")
        aboutAct.triggered.connect(self.showAbout)

        changeOutputDirAction = QAction('Change Output Directory...', self)
        changeOutputDirAction.setToolTip("Choose a different folder to write the folded image and CSV output")
        changeOutputDirAction.triggered.connect(self.workspace.change_output_directory)

        # Tools menu: image alignment and difference detection (reuses AISE's ImageAlignmentWidget)
        detectAlignmentAction = QAction('Detect Image Alignment...', self)
        detectAlignmentAction.setShortcut('Ctrl+D')
        detectAlignmentAction.setToolTip(
            "Open the alignment detection table to inspect/correct centers, "
            "rotations, and image differences across the loaded folder (Ctrl+D)"
        )
        detectAlignmentAction.triggered.connect(self._open_alignment_dialog)
        self._detectAlignmentAction = detectAlignmentAction

        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addAction(saveSettingsAction)
        fileMenu.addAction(loadSettingsAction)
        fileMenu.addSeparator()
        fileMenu.addAction(changeOutputDirAction)

        toolsMenu = menubar.addMenu('&Tools')
        toolsMenu.addAction(detectAlignmentAction)

        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)
    
    def _additional_setup(self):
        """Initialize patches, register tools, and setup background choice"""
        # Call parent to setup scan monitoring
        super()._additional_setup()
        
        # Set initial status bar text
        self.imgPathOnStatusBar.setText("  Please select an image or a folder to process")
        
        # For rmin, rmax visualization
        self._initialize_cicle_patches()        
        # Setup background choice UI
        self.bgChoiceInChanged()
        
        # Connect workspace/navigator signals
        # fileLoaded: Folder-level initialization (csvManager, etc.) - happens BEFORE first image
        self.workspace.navigator.fileLoaded.connect(self._on_folder_loaded)
        
        # imageDataReady: Receives ImageData ready for processing (replaces imageChanged)
        self.workspace.imageDataReady.connect(self._on_image_data_ready)
        
        # needsReprocess: Settings changed, reprocess current image
        self.workspace.needsReprocess.connect(self.processImage)
        
        # statusTextRequested: Update status bar
        self.workspace.statusTextRequested.connect(self._on_status_text_requested)

        # Output directory changed -> reset CSV manager
        self.workspace.outputDirChanged.connect(self._on_output_dir_changed)

    def _on_output_dir_changed(self, new_output_dir):
        """Reset CSV manager when the output directory changes."""
        if self.csvManager is not None:
            self.csvManager = QF_CSVManager(new_output_dir, extra_colnames=_qf_setting_keys(), version=__version__)

    # ------------------------------------------------------------------
    # Alignment / image-difference detection dialog (Tools -> Detect Image Alignment...)
    # ------------------------------------------------------------------

    def _open_alignment_dialog(self):
        """
        @description Open the :class:`QFAlignmentDialog`.

        The dialog reuses the current ProcessingWorkspace so no folder reload
        is needed. It is shown non-modally so the user can interact with both
        this main window and the dialog simultaneously.
        """
        if not self.file_manager or not self.file_manager.names:
            QMessageBox.information(
                self,
                "No Images Loaded",
                "Please load a folder before running alignment detection."
            )
            return

        existing = getattr(self, '_alignment_dialog', None)
        if existing is not None:
            try:
                existing.raise_()
                existing.activateWindow()
                return
            except RuntimeError:
                # The C++ object has already been destroyed; create a new one.
                self._alignment_dialog = None

        from .QFAlignmentDialog import QFAlignmentDialog
        dlg = QFAlignmentDialog(self.workspace, parent=self)
        dlg.alignmentChanged.connect(self._on_alignment_changed)
        dlg.finished.connect(self._on_alignment_dialog_closed)
        # WA_DeleteOnClose lets Qt destroy the object after closing to avoid stale references.
        dlg.setAttribute(Qt.WA_DeleteOnClose, True)
        self._alignment_dialog = dlg
        dlg.show()

    def _on_alignment_changed(self):
        """Settings changed in dialog (global base / detection finished) -> reprocess with latest settings."""
        if self.quadFold is not None:
            self.processImage()

    def _on_alignment_dialog_closed(self, *_args):
        """Clean up the dialog reference after it is closed."""
        self._alignment_dialog = None

    def _finalize_ui(self):
        """Final UI setup - set window constraints"""
        # Set minimum size for central widget to ensure usable UI
        self.centralWidget.setMinimumSize(700, 500)
        
        # Call parent to handle window resize and show
        super()._finalize_ui()
    
    # ===== UI setup methods =====
    
    # NOTE: _create_left_panel removed - left panel now managed by ImageNavigatorWidget
    
    def _add_display_options(self):
        """Add quadrant-specific display options to display panel"""
        self.showSeparator = QCheckBox()
        self.showSeparator.setText("Show Quadrant Separator")
        self.showSeparator.setChecked(True)
        self.showSeparator.setToolTip("Draw the horizontal/vertical lines separating the four quadrants on the result image")

        # Add quadrant-specific options to display panel's top slot
        self.image_viewer.display_panel.add_to_top_slot(self.showSeparator)
        
        # QuadrantFolding-specific: start with 0 decimals
        self.spminInt.setDecimals(0)
        self.spmaxInt.setDecimals(0)
        
        # Add zoom button to checkable buttons list
        self.checkableButtons.append(self.imgZoomInB)

    def _create_quadrant_settings(self):
        """Add quadrant-specific settings to right panel"""
        # Detect Image Alignment button – opens QFAlignmentDialog above the center settings.
        self._alignmentBtn = QPushButton("Detect Image Alignment...")
        self._alignmentBtn.setToolTip(
            "Open the alignment detection table to inspect center, rotation, "
            "and image-difference data across all loaded images (Ctrl+D)"
        )
        self._alignmentBtn.clicked.connect(self._open_alignment_dialog)
        self.right_panel.add_widget(self._alignmentBtn)

        # Add built-in settings widgets from workspace (workspace no longer adds these automatically)
        self.right_panel.add_widget(self.workspace._center_widget)
        self.right_panel.add_widget(self.workspace._rotation_widget)
        self.right_panel.add_widget(self.workspace._blank_mask_widget)
        
        # Add quadrant-specific settings groups
        self._create_processing_settings()
        self.right_panel.add_widget(self.settingsGroup)
        
        # Add checkable buttons from workspace settings widgets to checkableButtons list
        self.checkableButtons.extend([
            self.workspace._center_widget.setCenterRotationButton,
            self.workspace._center_widget.setCentByChords,
            self.workspace._center_widget.setCentByPerp,
            self.workspace._center_widget.setCentBtn,
            self.workspace._rotation_widget.setRotationButton,
            self.workspace._rotation_widget.setAngleBtn
        ])
        
        self._create_result_processing_settings()
        self.right_panel.add_widget(self.resProcGrpBx)
    
    def _create_processing_settings(self):
        """Create image processing settings group"""
        self.settingsGroup = CollapsibleGroupBox("Image Processing", start_expanded=True)
        self.settingsLayout = QGridLayout()
        
        self.compressFoldedImageChkBx = QCheckBox("Save Compressed Image")
        self.compressFoldedImageChkBx.setChecked(True)
        self.compressFoldedImageChkBx.setToolTip(
            "Saves the images as compressed tifs (might not be compatible with fit2d, but works with imagej)")
        
        self.toggleFoldImage = QCheckBox("Fold Image")
        self.toggleFoldImage.setChecked(True)
        self.toggleFoldImage.setToolTip(
            "When enabled, average the four quadrants into a single folded image.\n"
            "When disabled, the original (unfolded) image is used for background subtraction.")
        
        self.settingsLayout.addWidget(QLabel("Mask Threshold : Use Set Mask"), 0, 0, 1, 2)
        self.settingsLayout.addWidget(self.toggleFoldImage, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.compressFoldedImageChkBx, 1, 2, 1, 2)
        
        self.settingsGroup.setLayout(self.settingsLayout)
        # Note: settingsGroup is added in _create_quadrant_settings(), not here
    
    # NOTE: _create_center_rotation_settings and _create_blank_mask_settings
    # are now replaced by ImageSettingsPanel integration above

    def _create_result_processing_settings(self):
        """
        Create result processing section and launch button for background settings popup.
        """
        self.resProcGrpBx = CollapsibleGroupBox("Background Subtraction", start_expanded=False)
        self.resProcGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self._init_background_subtraction_dialog()

        # ===== ROI Settings =====
        self.setFitRoi = QPushButton("Set Region Of Interest (ROI)")
        self.setFitRoi.setCheckable(True)
        self.setFitRoi.setToolTip(
            "Activate ROI selection.\n"
            "Drag a rectangle on the folded image to limit background subtraction to that region.")
        self.unsetRoi = QPushButton("Unset ROI")
        self.unsetRoi.setToolTip("Remove the current ROI so the entire folded image is used for background subtraction")
        self.checkableButtons.append(self.setFitRoi)
        self.fixedRoiChkBx = QCheckBox("Persist ROI size")
        self.fixedRoiChkBx.setToolTip(
            "When enabled, the current ROI width/height is reused for every "
            "image you process. When disabled, each image keeps its own ROI.")
        self.fixedRoiChkBx.setChecked(False)
        # Width/Height of 0 is the "no ROI" sentinel: getFlags() only pushes
        # roi_w/h flags when both values are > 0.
        self.fixedRoiW = QSpinBox()
        self.fixedRoiW.setObjectName('fixedRoiW')
        self.fixedRoiW.setKeyboardTracking(False)
        self.fixedRoiW.setRange(0, 10000)
        self.fixedRoiW.setPrefix('W:')
        self.fixedRoiW.setSpecialValueText('W:off')
        self.fixedRoiH = QSpinBox()
        self.fixedRoiH.setObjectName('fixedRoiH')
        self.fixedRoiH.setKeyboardTracking(False)
        self.fixedRoiH.setRange(0, 10000)
        self.fixedRoiH.setPrefix('H:')
        self.fixedRoiH.setSpecialValueText('H:off')

        # ===== Section controls =====
        self.applyResultBGButton = QPushButton("Apply Default Optimization")
        self.applyResultBGButton.setStyleSheet("QPushButton { color: #ededed; background-color: #2986cc;}")
        self.openBGSettingsButton = QPushButton("Advanced Configuration")
        self.applyBGButtonProxy = QPushButton("Apply Selected Subtraction Settings")
        if hasattr(self, "applyBGButton"):
            self.applyBGButtonProxy.setStyleSheet(self.applyBGButton.styleSheet())

        self.bgOptionsLabel = QLabel("Options:")
        self.bgOptionsCB = QComboBox()
        self.bgOptionsCB.addItems([
            "Manual Setting | One Method",
            "Manual Setting | Transition",
            "Automated Processing",
        ])
        self.bgOptionsCB.setCurrentText("Manual Setting | One Method")

        self.downsampleProxyLabel = QLabel("Downsample:")
        self._downsample_proxy_syncing = False
        self.downsampleProxyCB = self._clone_combobox(self.downsampleCB)
        self._bind_proxy_combobox_downsample(self.downsampleProxyCB, self.downsampleCB)

        self.resultDisplayModeLabel = QLabel("Show:")
        self.resultDisplayModeCB = QComboBox()
        self.resultDisplayModeCB.addItems([
            "Subtracted",
            "Background",
            "Folded",
            "Evaluation Mask",
            "Synthetic Signal",
            "Synthetic Mask"
        ])
        self.resultDisplayModeCB.setCurrentIndex(0)
        self.resultDisplayModeCB.setToolTip(
            "Choose which result visualization to display in the Results tab")

        # ===== Current Configuration =====
        self.currentBGMethodLabelMain = QLabel("None")
        self.currentBGParamsLabelMain = QLabel("None")
        self.currentBGLossLabelMain = QLabel("None")
        self.currentBGParamsLabelMain.setWordWrap(True)


        self.bgSummaryLayout = QGridLayout()
        self.bgSummaryLayout.setContentsMargins(4, 4, 4, 4)
        self.bgSummaryLayout.setSpacing(8)


        def _make_section(title=None):
            section = QWidget()
            section.setObjectName("sectionContainer")
            section_layout = QGridLayout(section)
            section_layout.setContentsMargins(8, 8, 8, 8)
            section_layout.setSpacing(6)
            section_layout.setColumnStretch(0, 1)
            section_layout.setColumnStretch(1, 1)
            section_layout.setColumnStretch(2, 1)
            section_layout.setColumnStretch(3, 1)
            section.setStyleSheet("#sectionContainer { border: 1px solid #E6E6E6; border-radius: 4px; }")
            if title:
                title_label = QLabel(title)
                title_label.setStyleSheet("font-weight: bold; border: none;")

                header_layout = QHBoxLayout()
                header_layout.setContentsMargins(0, 0, 0, 0)
                header_layout.setSpacing(4)
                header_layout.addWidget(title_label)
                header_layout.addStretch(1)

                section_layout.addLayout(header_layout, 0, 0, 1, 4)
            return section, section_layout

        # 1) Background Subtraction
        bg_sub_section, bg_sub_layout = _make_section()
        bg_sub_layout.addWidget(self.resultDisplayModeLabel, 0, 0, 1, 2)
        bg_sub_layout.addWidget(self.resultDisplayModeCB, 0, 2, 1, 2)
        bg_sub_layout.addWidget(self.bgOptionsLabel, 1, 0, 1, 2)
        bg_sub_layout.addWidget(self.bgOptionsCB, 1, 2, 1, 2)
        bg_sub_layout.addWidget(self.downsampleProxyLabel, 2, 0, 1, 2)
        bg_sub_layout.addWidget(self.downsampleProxyCB, 2, 2, 1, 2)
        bg_sub_layout.addWidget(self.applyResultBGButton, 3, 0, 1, 2)
        bg_sub_layout.addWidget(self.openBGSettingsButton, 3, 2, 1, 2)
        

        # Manual settings (reusing dialog-owned widgets)
        self.manualSettingsContainer = QWidget()
        manual_settings_layout = QGridLayout(self.manualSettingsContainer)
        manual_settings_layout.setContentsMargins(0, 0, 0, 0)
        manual_settings_layout.setSpacing(6)
        self._create_manual_settings_proxy()
        self._populate_manual_processing_layout_proxy(manual_settings_layout)
        # self.bgSummaryLayout.addWidget(self.manualSettingsContainer, 5, 0, 1, 4)
        bg_sub_layout.addWidget(self.manualSettingsContainer, 5, 0, 1, 4)

        # Manual settings (OUT/background transition)
        self.manualSettingsOutContainer = QWidget()
        manual_settings_out_layout = QGridLayout(self.manualSettingsOutContainer)
        manual_settings_out_layout.setContentsMargins(0, 0, 0, 0)
        manual_settings_out_layout.setSpacing(6)
        self._create_manual_settings_out_widgets()
        self._populate_manual_processing_layout_out(manual_settings_out_layout)
        bg_sub_layout.addWidget(self.manualSettingsOutContainer, 6, 0, 1, 4)
        self.bgChoiceOutChanged()

        # Transition settings (radius/delta)
        self.transitionSettingsContainer = QWidget()
        transition_settings_layout = QGridLayout(self.transitionSettingsContainer)
        transition_settings_layout.setContentsMargins(0, 0, 0, 0)
        transition_settings_layout.setSpacing(6)
        self._create_transition_settings_proxy()
        self._populate_transition_processing_layout_proxy(transition_settings_layout)
        bg_sub_layout.addWidget(self.transitionSettingsContainer, 7, 0, 1, 4)

        self.bgSummaryLayout.addWidget(bg_sub_section, 4, 0, 1, 4)

        bg_sub_layout.addWidget(self.applyBGButtonProxy, 15, 0, 1, 4)



        # 2) Current Configuration
        current_section, current_layout = _make_section("Current Configuration")
        # TODO: define table in one place to be reused in the pop up window

        current_summary_widget = BackgroundSubtractionDialog.build_current_config_summary_widget(
            method_label=self.currentBGMethodLabelMain,
            params_label=self.currentBGParamsLabelMain,
            loss_label=self.currentBGLossLabelMain,
            title=None,
            min_params_width=320,
        )

        current_layout.addWidget(current_summary_widget, 1, 0, 1, 4)

        self.bgSummaryLayout.addWidget(current_section, 6, 0, 1, 4)

        self.resProcGrpBx.setLayout(self.bgSummaryLayout)

        # TODO: cache bg metrics
        self._on_bg_options_changed(self.bgOptionsCB.currentIndex())

    def _create_manual_settings_proxy(self):
        self._manual_proxy_syncing = False
        self._manual_proxy = {}

        self._manual_proxy["gaussFWHMLabel"] = self._clone_label(self.gaussFWHMLabel)
        self._manual_proxy["boxcarLabel"] = self._clone_label(self.boxcarLabel)
        self._manual_proxy["cycleLabel"] = self._clone_label(self.cycleLabel)
        self._manual_proxy["windowSizeLabel"] = self._clone_label(self.windowSizeLabel)
        self._manual_proxy["windowSepLabel"] = self._clone_label(self.windowSepLabel)
        self._manual_proxy["thetaBinLabel"] = self._clone_label(self.thetaBinLabel)
        self._manual_proxy["radialBinLabel"] = self._clone_label(self.radialBinLabel)
        self._manual_proxy["pixRangeLabel"] = self._clone_label(self.pixRangeLabel)
        self._manual_proxy["tensionLabel"] = self._clone_label(self.tensionLabel)
        self._manual_proxy["degreeLabel"] = self._clone_label(self.degreeLabel)
        self._manual_proxy["tophatLabel"] = self._clone_label(self.tophatLabel)
        self._manual_proxy["smoothLabel"] = self._clone_label(self.smoothLabel)
        self._manual_proxy["bgChoiceIn"] = self._clone_combobox(self.bgChoiceIn)
        self._manual_proxy["gaussFWHM"] = self._clone_spinbox(self.gaussFWHM)
        self._manual_proxy["boxcarX"] = self._clone_spinbox(self.boxcarX)
        self._manual_proxy["boxcarY"] = self._clone_spinbox(self.boxcarY)
        self._manual_proxy["cycle"] = self._clone_spinbox(self.cycle)
        self._manual_proxy["thetabinCB"] = self._clone_combobox(self.thetabinCB)
        self._manual_proxy["radialBinSpnBx"] = self._clone_spinbox(self.radialBinSpnBx)
        self._manual_proxy["winSizeX"] = self._clone_spinbox(self.winSizeX)
        self._manual_proxy["winSizeY"] = self._clone_spinbox(self.winSizeY)
        self._manual_proxy["winSepX"] = self._clone_spinbox(self.winSepX)
        self._manual_proxy["winSepY"] = self._clone_spinbox(self.winSepY)
        self._manual_proxy["minPixRange"] = self._clone_double_spinbox(self.minPixRange)
        self._manual_proxy["maxPixRange"] = self._clone_double_spinbox(self.maxPixRange)
        self._manual_proxy["smoothSpnBx"] = self._clone_double_spinbox(self.smoothSpnBx)
        self._manual_proxy["tensionSpnBx"] = self._clone_double_spinbox(self.tensionSpnBx)
        self._manual_proxy["degreeCB"] = self._clone_combobox(self.degreeCB)
        self._manual_proxy["tophatSpnBx"] = self._clone_spinbox(self.tophatSpnBx)

        self._bind_proxy_spinbox(self._manual_proxy["gaussFWHM"], self.gaussFWHM)
        self._bind_proxy_spinbox(self._manual_proxy["boxcarX"], self.boxcarX)
        self._bind_proxy_spinbox(self._manual_proxy["boxcarY"], self.boxcarY)
        self._bind_proxy_spinbox(self._manual_proxy["cycle"], self.cycle)
        self._bind_proxy_spinbox(self._manual_proxy["radialBinSpnBx"], self.radialBinSpnBx)
        self._bind_proxy_spinbox(self._manual_proxy["winSizeX"], self.winSizeX)
        self._bind_proxy_spinbox(self._manual_proxy["winSizeY"], self.winSizeY)
        self._bind_proxy_spinbox(self._manual_proxy["winSepX"], self.winSepX)
        self._bind_proxy_spinbox(self._manual_proxy["winSepY"], self.winSepY)
        self._bind_proxy_spinbox(self._manual_proxy["tophatSpnBx"], self.tophatSpnBx)

        self._bind_proxy_double_spinbox(self._manual_proxy["minPixRange"], self.minPixRange)
        self._bind_proxy_double_spinbox(self._manual_proxy["maxPixRange"], self.maxPixRange)
        self._bind_proxy_double_spinbox(self._manual_proxy["smoothSpnBx"], self.smoothSpnBx)
        self._bind_proxy_double_spinbox(self._manual_proxy["tensionSpnBx"], self.tensionSpnBx)

        self._bind_proxy_combobox(self._manual_proxy["bgChoiceIn"], self.bgChoiceIn)
        self._bind_proxy_combobox(self._manual_proxy["thetabinCB"], self.thetabinCB)
        self._bind_proxy_combobox(self._manual_proxy["degreeCB"], self.degreeCB)

    def _create_manual_settings_out_widgets(self):
        """Create OUT background controls owned by the main Quadrant Folding UI."""
        def _spinbox(min_val, max_val, value, step=1, suffix="", prefix=""):
            spnbx = QSpinBox()
            spnbx.setRange(min_val, max_val)
            spnbx.setValue(value)
            spnbx.setSingleStep(step)
            spnbx.setKeyboardTracking(False)
            if suffix:
                spnbx.setSuffix(suffix)
            if prefix:
                spnbx.setPrefix(prefix)
            return spnbx

        def _double_spinbox(min_val, max_val, value, decimals=2, step=0.1, suffix=""):
            spnbx = QDoubleSpinBox()
            spnbx.setRange(min_val, max_val)
            spnbx.setValue(value)
            spnbx.setDecimals(decimals)
            spnbx.setSingleStep(step)
            spnbx.setKeyboardTracking(False)
            if suffix:
                spnbx.setSuffix(suffix)
            return spnbx

        self.bgChoiceOut = QComboBox()
        for choice in self.allBGChoices:
            if choice == "2D Convexhull": # not supported for outside background
                continue
            self.bgChoiceOut.addItem(choice)
        self.bgChoiceOut.setCurrentIndex(0)

        # ===== R-min Settings =====
        self.setRminButton = QPushButton("Set Manual R-min")
        self.setRminButton.setCheckable(True)
        self.setRminButton.setToolTip(
            "Activate manual R-min adjustment.\n"
            "Click on the image to define the inner radius excluded from background fitting.")
        self.checkableButtons.append(self.setRminButton)

        self.gaussFWHMOutLabel = QLabel("Gaussian FWHM : ")
        self.gaussFWHMOut = _spinbox(
            qf_defaults.BG_PARAM_RANGE[0], qf_defaults.BG_PARAM_RANGE[1],
            qf_defaults.DEFAULT_GAUSSIAN_FWHM)

        self.boxcarOutLabel = QLabel("Box Car Size : ")
        self.boxcarOutX = _spinbox(
            qf_defaults.BG_PARAM_RANGE[0], qf_defaults.BG_PARAM_RANGE[1],
            qf_defaults.DEFAULT_BOXCAR_SIZE, prefix='X:')
        self.boxcarOutY = _spinbox(
            qf_defaults.BG_PARAM_RANGE[0], qf_defaults.BG_PARAM_RANGE[1],
            qf_defaults.DEFAULT_BOXCAR_SIZE, prefix='Y:')

        self.showRminChkBx = QCheckBox("Show R-min")
        self.showRminChkBx.setToolTip("Draw the R-min circle on the folded image")
        self.fixedRadiusRangeChkBx = QCheckBox("Persist R-min")
        self.fixedRadiusRangeChkBx.setToolTip(
            "When enabled, the current R-min is reused for every image you process.\n"
            "When disabled, R-min is recomputed per image.")

        self.cycleOutLabel = QLabel("Number of Cycles : ")
        self.cycleOut = _spinbox(
            qf_defaults.BG_PARAM_RANGE[0], qf_defaults.BG_PARAM_RANGE[1],
            qf_defaults.DEFAULT_CYCLES)

        self.windowSizeOutLabel = QLabel("Window Size : ")
        self.winSizeOutX = _spinbox(
            qf_defaults.BG_PARAM_RANGE[0], qf_defaults.BG_PARAM_RANGE[1],
            qf_defaults.DEFAULT_WINDOW_SIZE, prefix='X:')
        self.winSizeOutY = _spinbox(
            qf_defaults.BG_PARAM_RANGE[0], qf_defaults.BG_PARAM_RANGE[1],
            qf_defaults.DEFAULT_WINDOW_SIZE, prefix='Y:')

        self.windowSepOutLabel = QLabel("Window Separation : ")
        self.winSepOutX = _spinbox(
            qf_defaults.BG_PARAM_RANGE[0], qf_defaults.BG_PARAM_RANGE[1],
            qf_defaults.DEFAULT_WINDOW_SEP, prefix='X:')
        self.winSepOutY = _spinbox(
            qf_defaults.BG_PARAM_RANGE[0], qf_defaults.BG_PARAM_RANGE[1],
            qf_defaults.DEFAULT_WINDOW_SEP, prefix='Y:')

        self.minPixRangeOut = _double_spinbox(
            qf_defaults.PIXEL_RANGE_LIMIT[0], qf_defaults.PIXEL_RANGE_LIMIT[1],
            qf_defaults.DEFAULT_PIXEL_MIN, decimals=2, step=2, suffix="%")
        self.maxPixRangeOut = _double_spinbox(
            qf_defaults.PIXEL_RANGE_LIMIT[0], qf_defaults.PIXEL_RANGE_LIMIT[1],
            qf_defaults.DEFAULT_PIXEL_MAX, decimals=2, step=2, suffix="%")
        self.pixRangeLabelOut = QLabel("Pixel Range : ")

        self.thetaBinOutLabel = QLabel("Bin Theta (deg) : ")
        self.thetaBinOutCB = QComboBox()
        self.thetaBinOutCB.addItems(qf_defaults.THETA_BIN_OPTIONS)
        self.thetaBinOutCB.setCurrentIndex(qf_defaults.DEFAULT_THETA_BIN_INDEX)

        self.radialBinOutLabel = QLabel("Radial Bin : ")
        self.radialBinOutSpnBx = _spinbox(
            qf_defaults.RADIAL_BIN_RANGE[0], qf_defaults.RADIAL_BIN_RANGE[1],
            qf_defaults.DEFAULT_RADIAL_BIN, suffix=" Pixel(s)")

        self.smoothOutLabel = QLabel("Smoothing factor : ")
        self.smoothOutSpnBx = _double_spinbox(0, 10000, qf_defaults.DEFAULT_SMOOTHING, decimals=2, step=0.1)

        self.tensionOutLabel = QLabel("Tension factor : ")
        self.tensionOutSpnBx = _double_spinbox(0, 100, qf_defaults.DEFAULT_TENSION, decimals=2, step=0.1)

        self.tophatOutLabel = QLabel("Top-hat Disk Size: ")
        self.tophatOutSpnBx = _spinbox(
            qf_defaults.TOPHAT_RANGE[0], qf_defaults.TOPHAT_RANGE[1],
            qf_defaults.DEFAULT_TOPHAT_SIZE)

    def _create_transition_settings_proxy(self):
        self._transition_proxy_syncing = False
        self._transition_proxy = {}

        self._transition_proxy["tranRSpnBx"] = self._clone_spinbox(self.tranRSpnBx)
        self._transition_proxy["tranDeltaSpnBx"] = self._clone_spinbox(self.tranDeltaSpnBx)
        self._transition_proxy["showTranRadDeltaChkBx"] = self._clone_checkbox(self.showTranRadDeltaChkBx)

        self._bind_proxy_spinbox_transition(self._transition_proxy["tranRSpnBx"], self.tranRSpnBx)
        self._bind_proxy_spinbox_transition(self._transition_proxy["tranDeltaSpnBx"], self.tranDeltaSpnBx)
        self._bind_proxy_checkbox(self._transition_proxy["showTranRadDeltaChkBx"], self.showTranRadDeltaChkBx)

    def _populate_manual_processing_layout_proxy(self, layout):
        # Match BackgroundSubtractionDialog._populate_manual_processing_layout:
        # one label + control per row so shared QLabel instances are not dropped
        # from the grid by duplicate (row, col) placement.
        row = 0

        self.innerBackgroundSectionHeading = self._create_bg_section_heading("Inner background")
        layout.addWidget(self.innerBackgroundSectionHeading, row, 0, 1, 2)
        row += 1

        layout.addWidget(QLabel("Subtraction Method:"), row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["bgChoiceIn"], row, 1, 1, 1)
        row += 1


        layout.addWidget(self._manual_proxy["degreeLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["degreeCB"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["tophatLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["tophatSpnBx"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["cycleLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["cycle"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["gaussFWHMLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["gaussFWHM"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["boxcarLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["boxcarX"], row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["boxcarY"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["windowSizeLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["winSizeX"], row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["winSizeY"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["windowSepLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["winSepX"], row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["winSepY"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["thetaBinLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["thetabinCB"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["radialBinLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["radialBinSpnBx"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["pixRangeLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["minPixRange"], row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["maxPixRange"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["tensionLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["tensionSpnBx"], row, 1, 1, 1)
        row += 1

        layout.addWidget(self._manual_proxy["smoothLabel"], row, 0, 1, 1)
        layout.addWidget(self._manual_proxy["smoothSpnBx"], row, 1, 1, 1)

    def _populate_manual_processing_layout_out(self, layout):
        row = 0

        layout.addWidget(self._create_bg_section_heading("Outer Background"), row, 0, 1, 2)
        row += 1

        layout.addWidget(self._create_settings_separator(), row, 0, 1, 2)
        row += 1

        layout.addWidget(QLabel("Subtraction Method:"), row, 0, 1, 1)
        layout.addWidget(self.bgChoiceOut, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.cycleOutLabel, row, 0, 1, 1)
        layout.addWidget(self.cycleOut, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.gaussFWHMOutLabel, row, 0, 1, 1)
        layout.addWidget(self.gaussFWHMOut, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.boxcarOutLabel, row, 0, 1, 1)
        layout.addWidget(self.boxcarOutX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.boxcarOutY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.windowSizeOutLabel, row, 0, 1, 1)
        layout.addWidget(self.winSizeOutX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.winSizeOutY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.windowSepOutLabel, row, 0, 1, 1)
        layout.addWidget(self.winSepOutX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.winSepOutY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.thetaBinOutLabel, row, 0, 1, 1)
        layout.addWidget(self.thetaBinOutCB, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.radialBinOutLabel, row, 0, 1, 1)
        layout.addWidget(self.radialBinOutSpnBx, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.pixRangeLabelOut, row, 0, 1, 1)
        layout.addWidget(self.minPixRangeOut, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.maxPixRangeOut, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.tensionOutLabel, row, 0, 1, 1)
        layout.addWidget(self.tensionOutSpnBx, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.tophatOutLabel, row, 0, 1, 1)
        layout.addWidget(self.tophatOutSpnBx, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.smoothOutLabel, row, 0, 1, 1)
        layout.addWidget(self.smoothOutSpnBx, row, 1, 1, 1)

    def _populate_transition_processing_layout_proxy(self, layout):
        row = 0
        layout.addWidget(self._create_bg_section_heading("Transition Settings"), row, 0, 1, 4)
        row += 1
        layout.addWidget(self._create_settings_separator(), row, 0, 1, 4)
        row += 1
        layout.addWidget(QLabel("Transition Radius:"), row, 0, 1, 2)
        layout.addWidget(self._transition_proxy["tranRSpnBx"], row, 2, 1, 2)
        row += 1
        layout.addWidget(QLabel("Transition Delta:"), row, 0, 1, 2)
        layout.addWidget(self._transition_proxy["tranDeltaSpnBx"], row, 2, 1, 2)
        row += 1
        layout.addWidget(self._transition_proxy["showTranRadDeltaChkBx"], row, 0, 1, 4)

    def _clone_spinbox(self, source):
        proxy = QSpinBox()
        proxy.setRange(source.minimum(), source.maximum())
        proxy.setSingleStep(source.singleStep())
        proxy.setKeyboardTracking(source.keyboardTracking())
        proxy.setPrefix(source.prefix())
        proxy.setSuffix(source.suffix())
        proxy.setValue(source.value())
        return proxy

    def _clone_double_spinbox(self, source):
        proxy = QDoubleSpinBox()
        proxy.setRange(source.minimum(), source.maximum())
        proxy.setDecimals(source.decimals())
        proxy.setSingleStep(source.singleStep())
        proxy.setKeyboardTracking(source.keyboardTracking())
        proxy.setPrefix(source.prefix())
        proxy.setSuffix(source.suffix())
        proxy.setValue(source.value())
        return proxy

    def _clone_combobox(self, source):
        proxy = QComboBox()
        for i in range(source.count()):
            proxy.addItem(source.itemText(i))
        proxy.setCurrentIndex(source.currentIndex())
        return proxy

    def _clone_checkbox(self, source):
        proxy = QCheckBox(source.text())
        proxy.setChecked(source.isChecked())
        proxy.setToolTip(source.toolTip())
        return proxy

    def _clone_label(self, source):
        proxy = QLabel(source.text())
        proxy.setToolTip(source.toolTip())
        proxy.setStyleSheet(source.styleSheet())
        proxy.setAlignment(source.alignment())
        proxy.setHidden(source.isHidden())
        return proxy

    def _create_settings_separator(self):
        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setFrameShadow(QFrame.Sunken)
        return separator

    def _create_bg_section_heading(self, text):
        label = QLabel(text)
        label.setStyleSheet("font-weight: bold;")
        return label

    def _bind_proxy_spinbox(self, proxy, source):
        def _proxy_changed(value):
            if self._manual_proxy_syncing:
                return
            self._manual_proxy_syncing = True
            source.setValue(value)
            self._manual_proxy_syncing = False

        def _source_changed(value):
            if self._manual_proxy_syncing:
                return
            self._manual_proxy_syncing = True
            proxy.setValue(value)
            self._manual_proxy_syncing = False

        proxy.valueChanged.connect(_proxy_changed)
        source.valueChanged.connect(_source_changed)

    def _bind_proxy_double_spinbox(self, proxy, source):
        def _proxy_changed(value):
            if self._manual_proxy_syncing:
                return
            self._manual_proxy_syncing = True
            source.setValue(value)
            self._manual_proxy_syncing = False

        def _source_changed(value):
            if self._manual_proxy_syncing:
                return
            self._manual_proxy_syncing = True
            proxy.setValue(value)
            self._manual_proxy_syncing = False

        proxy.valueChanged.connect(_proxy_changed)
        source.valueChanged.connect(_source_changed)

    def _bind_proxy_combobox(self, proxy, source):
        def _proxy_changed(index):
            if self._manual_proxy_syncing:
                return
            self._manual_proxy_syncing = True
            source.setCurrentIndex(index)
            self._manual_proxy_syncing = False

        def _source_changed(index):
            if self._manual_proxy_syncing:
                return
            self._manual_proxy_syncing = True
            proxy.setCurrentIndex(index)
            self._manual_proxy_syncing = False

        proxy.currentIndexChanged.connect(_proxy_changed)
        source.currentIndexChanged.connect(_source_changed)

    def _bind_proxy_combobox_downsample(self, proxy, source):
        def _proxy_changed(index):
            if self._downsample_proxy_syncing:
                return
            self._downsample_proxy_syncing = True
            source.setCurrentIndex(index)
            self._downsample_proxy_syncing = False

        def _source_changed(index):
            if self._downsample_proxy_syncing:
                return
            self._downsample_proxy_syncing = True
            proxy.setCurrentIndex(index)
            self._downsample_proxy_syncing = False

        proxy.currentIndexChanged.connect(_proxy_changed)
        source.currentIndexChanged.connect(_source_changed)

    def _bind_proxy_spinbox_transition(self, proxy, source):
        def _proxy_changed(value):
            if self._transition_proxy_syncing:
                return
            self._transition_proxy_syncing = True
            source.setValue(value)
            self._transition_proxy_syncing = False

        def _source_changed(value):
            if self._transition_proxy_syncing:
                return
            self._transition_proxy_syncing = True
            proxy.setValue(value)
            self._transition_proxy_syncing = False

        proxy.valueChanged.connect(_proxy_changed)
        source.valueChanged.connect(_source_changed)

    def _bind_proxy_checkbox(self, proxy, source):
        def _proxy_changed(checked):
            if self._transition_proxy_syncing:
                return
            self._transition_proxy_syncing = True
            source.setChecked(checked)
            self._transition_proxy_syncing = False

        def _source_changed(checked):
            if self._transition_proxy_syncing:
                return
            self._transition_proxy_syncing = True
            proxy.setChecked(checked)
            self._transition_proxy_syncing = False

        proxy.toggled.connect(_proxy_changed)
        source.toggled.connect(_source_changed)


    def applyDefaultOptimization(self, skip_confirm: bool = False):
        """Force automated optimization mode and process current image."""
        if self._OptimizationRunning:
            self._stopOptimizationRequested = True
            self.stop_process = True
            if self.quadFold is not None:
                self.quadFold.info['optimize'] = False
            self.optimizeFlag = False
            self._restoreOptimizeCheckboxAfterProcess = False
            self._set_optimization_button_running(False)
            return

        if not skip_confirm:
            msg_box = QMessageBox(self)
            msg_box.setIcon(QMessageBox.Warning)
            msg_box.setWindowTitle("Run Default Optimization")
            msg_box.setText("This operation may take some time.")
            msg_box.setInformativeText("Do you want to continue?")
            msg_box.setStandardButtons(QMessageBox.Ok | QMessageBox.Cancel)
            msg_box.setDefaultButton(QMessageBox.Ok)
            if msg_box.exec_() != QMessageBox.Ok:
                return

        QApplication.processEvents()
        if not self.ableToProcess():
            return

        default_methods = qf_defaults.DEFAULT_OPTIMIZATION_METHODS
        checked = self.optimizeFlag is True
        if not checked:
            self.optimizeFlag = True
            if len(self.optimizationMethodsList.selectedItems()) == 0:
                self._set_selected_optimization_methods(default_methods)

        self._restoreOptimizeCheckboxAfterProcess = not checked
        self.stop_process = False
        self._set_optimization_button_running(True)

        self.deleteInfo(['bgsubimg'])
        self.deleteInfo(['result_bg'])
        self.deleteImgCache(['BgSubFold'])

        self.resultDisplayModeCB.setCurrentText("Subtracted")
        self.resultDisplayModeCB.setEnabled(False)

        self._addDefaultOptimizationConfig = True
        self.processImage()

        if not checked:
            self.optimizeFlag = False
        

    def _init_background_subtraction_dialog(self):
        """Create popup dialog and expose its widgets for backward compatibility."""
        self.bgSubDialog = BackgroundSubtractionDialog(self)

        # Keep legacy attributes in sync with dialog-owned state
        self.manualBackgroundAssignments = self.bgSubDialog.manualBackgroundAssignments
        self.backgroundConfigurations = self.bgSubDialog.backgroundConfigurations
        

        for attr in [
            'allBGChoices',
            'bgChoiceIn', 'setRminRmaxButton',
            'processingModeCB',
            'bgChoiceOut',
            'rminSpnBx', 'rmaxSpnBx', 'rminLabel', 'rmaxLabel',
            'downsampleLabel', 'downsampleCB', 'smoothImageChkbx',
             'showRminRmaxChkBx', 'fixedRadiusRangeChkBx', 
            'equatorMaskHeightSpnBx', 'equatorCenterBeamSpnBx',
            'm1SpnBx', 'layerLineWidthSpnBx',
            'gaussFWHMLabel', 'gaussFWHM',
            'boxcarLabel', 'boxcarX', 'boxcarY',
            'cycleLabel', 'cycle',
            'windowSizeLabel', 'winSizeX', 'winSizeY',
            'windowSepLabel', 'winSepX', 'winSepY',
            'minPixRange', 'maxPixRange', 'pixRangeLabel',
            'thetaBinLabel', 'thetabinCB',
            'radialBinSpnBx', 'radialBinLabel',
            'smoothSpnBx', 'smoothLabel',
            'tensionSpnBx', 'tensionLabel',
            'tophatSpnBx', 'tophatLabel',
            'degreeLabel', 'degreeCB',
            'gaussFWHMOutLabel', 'gaussFWHMOut',
            'boxcarOutLabel', 'boxcarOutX', 'boxcarOutY',
            'cycleOutLabel', 'cycleOut',
            'windowSizeOutLabel', 'winSizeOutX', 'winSizeOutY',
            'windowSepOutLabel', 'winSepOutX', 'winSepOutY',
            'minPixRangeOut', 'maxPixRangeOut', 'pixRangeLabelOut',
            'thetaBinOutLabel', 'thetaBinOutCB',
            'radialBinOutSpnBx', 'radialBinOutLabel',
            'smoothOutSpnBx', 'smoothOutLabel',
            'tensionOutSpnBx', 'tensionOutLabel',
            'tophatOutSpnBx', 'tophatOutLabel',
            'tranRLabel', 'tranRSpnBx',
            'tranDeltaLabel', 'tranDeltaSpnBx',
            'showTranRadDeltaChkBx',
            'applyBGButton',
            'optimizeFlag',
            'optimizationMethodsList',
            'stepsLineEdit',
            'maxIterationsSpnBx',
            'earlyStopSpnBx',
            'optimizeTimeoutLabel',
            'optimizeTimeoutSpnBx',
            'meanMSESpnBx',
            'meanNegSynSpnBx',
            'meanNonBaselineSpnBx',
            'meanNegConSpnBx',
            'meanSmoothSpnBx',
            'evaluationBaselineSpnBx',
            'persistEvaluationBaselineChkBx',
            'amplitudeSpnBx',
            'sigmaXSpnBx',
            'sigmaYSpnBx',
            'persistSyntheticDataChkBx',
            'freqCB',
            'weightMSESpnBx',
            'weightNegSynSpnBx',
            'weightNonBaselineSpnBx',
            'weightNegConSpnBx',
            'weightSmoothSpnBx',
            'currentBGMethodLabel',
            'currentBGModeLabel',
            'currentBGParamsLabel',
            'currentBGLossLabel',
            'bgMetricsTable',
            'bgMetricsTableTitle',
            'saveMetricsToCsvChkBx',
            'addBackgroundConfigButton',
            'backgroundConfigsTable',
            'deleteBackgroundConfigButton',
            'optimizeEachImageChkBx',
            'chooseConfigurationsAutoChkBx',
            'createNewConfigurationsChkBx',
            'assignConfgurationsManually',
            'processFolderWithSelections',
        ]:
            setattr(self, attr, getattr(self.bgSubDialog, attr))

        self.checkableButtons.append(self.setRminRmaxButton)
        if hasattr(self, "persistEvaluationBaselineChkBx"):
            self.persistEvaluationBaselineChkBx.toggled.connect(self._on_persist_evaluation_baseline_toggled)
        if hasattr(self, "persistSyntheticDataChkBx"):
            self.persistSyntheticDataChkBx.toggled.connect(self._on_persist_synthetic_data_toggled)

    def openBackgroundSubtractionDialog(self):
        """Open the background subtraction settings popup."""
        self._update_bg_method_summary()
        self.bgSubDialog.show()
        self.bgSubDialog.raise_()
        self.bgSubDialog.activateWindow()

    def _get_persisted_evaluation_baseline(self):
        with self._persisted_eval_baseline_lock:
            value = self._persisted_evaluation_baseline
        if value is None:
            return None
        try:
            parsed = float(value)
        except Exception:
            return None
        return parsed if parsed > 0.0 else None

    def _register_persisted_evaluation_baseline(self, baseline):
        try:
            parsed = float(baseline)
        except Exception:
            return self._get_persisted_evaluation_baseline()
        if parsed <= 0.0:
            return self._get_persisted_evaluation_baseline()

        with self._persisted_eval_baseline_lock:
            existing = self._persisted_evaluation_baseline
            if existing is None:
                self._persisted_evaluation_baseline = parsed
                resolved = parsed
            else:
                resolved = float(existing)
        return float(resolved)

    def _clear_persisted_evaluation_baseline(self):
        with self._persisted_eval_baseline_lock:
            self._persisted_evaluation_baseline = None

    def _register_persisted_baseline_from_processed_info(self, info):
        if not isinstance(info, dict):
            return
        if not (
            hasattr(self, "persistEvaluationBaselineChkBx")
            and self.persistEvaluationBaselineChkBx.isChecked()
        ):
            return
        baseline = info.get("evaluation_baseline", None)
        resolved = self._register_persisted_evaluation_baseline(baseline)
        if resolved is None:
            return
        if hasattr(self, "evaluationBaselineSpnBx"):
            blocker = QSignalBlocker(self.evaluationBaselineSpnBx)
            self.evaluationBaselineSpnBx.setValue(float(resolved))
            del blocker

    def _on_persist_evaluation_baseline_toggled(self, checked):
        if checked:
            if hasattr(self, "evaluationBaselineSpnBx"):
                baseline = float(self.evaluationBaselineSpnBx.value())
                if baseline > 0.0:
                    self._register_persisted_evaluation_baseline(baseline)
        else:
            self._clear_persisted_evaluation_baseline()

    def _is_synthetic_data_persisted(self):
        """Return True when the "Persist synthetic data" checkbox is checked."""
        return (
            hasattr(self, "persistSyntheticDataChkBx")
            and self.persistSyntheticDataChkBx is not None
            and self.persistSyntheticDataChkBx.isChecked()
        )

    def _get_persisted_synthetic_params(self):
        """Return the persisted ``(amplitude, sigma_x, sigma_y)`` tuple, or ``None``.

        Returns ``None`` if no synthetic params have been registered yet or
        if any of them is non-positive (which means the optimizer / GUI
        should recompute them from the image).
        """
        with self._persisted_synthetic_params_lock:
            value = self._persisted_synthetic_params
        if value is None:
            return None
        try:
            amp, sx, sy = (float(value[0]), float(value[1]), float(value[2]))
        except Exception:
            return None
        if amp <= 0.0 or sx <= 0.0 or sy <= 0.0:
            return None
        return (amp, sx, sy)

    def _register_persisted_synthetic_params(self, amplitude, sigma_x, sigma_y):
        """Persist synthetic params on first use; later calls return the existing tuple.

        This mirrors ``_register_persisted_evaluation_baseline``: once a valid
        tuple is captured (e.g. after the first per-image computation), it
        sticks across image navigations while the checkbox stays checked.
        """
        try:
            amp = float(amplitude)
            sx = float(sigma_x)
            sy = float(sigma_y)
        except Exception:
            return self._get_persisted_synthetic_params()
        if amp <= 0.0 or sx <= 0.0 or sy <= 0.0:
            return self._get_persisted_synthetic_params()

        with self._persisted_synthetic_params_lock:
            existing = self._persisted_synthetic_params
            if existing is None:
                self._persisted_synthetic_params = (amp, sx, sy)
                resolved = (amp, sx, sy)
            else:
                resolved = tuple(float(v) for v in existing)
        return resolved

    def _clear_persisted_synthetic_params(self):
        with self._persisted_synthetic_params_lock:
            self._persisted_synthetic_params = None

    def _register_persisted_synthetic_from_processed_info(self, info):
        """After processing, persist synthetic params from ``info`` if the checkbox is on."""
        if not isinstance(info, dict):
            return
        if not self._is_synthetic_data_persisted():
            return
        resolved = self._register_persisted_synthetic_params(
            info.get("synthetic_amplitude"),
            info.get("synthetic_sigma_x"),
            info.get("synthetic_sigma_y"),
        )
        if resolved is None:
            return
        amp, sx, sy = resolved
        self.uiUpdating = True
        try:
            if hasattr(self, "amplitudeSpnBx"):
                blocker = QSignalBlocker(self.amplitudeSpnBx)
                self.amplitudeSpnBx.setValue(amp)
                del blocker
            if hasattr(self, "sigmaXSpnBx"):
                blocker = QSignalBlocker(self.sigmaXSpnBx)
                self.sigmaXSpnBx.setValue(sx)
                del blocker
            if hasattr(self, "sigmaYSpnBx"):
                blocker = QSignalBlocker(self.sigmaYSpnBx)
                self.sigmaYSpnBx.setValue(sy)
                del blocker
        finally:
            self.uiUpdating = False

    def _on_persist_synthetic_data_toggled(self, checked):
        if checked:
            amp = float(self.amplitudeSpnBx.value()) if hasattr(self, "amplitudeSpnBx") else 0.0
            sx = float(self.sigmaXSpnBx.value()) if hasattr(self, "sigmaXSpnBx") else 0.0
            sy = float(self.sigmaYSpnBx.value()) if hasattr(self, "sigmaYSpnBx") else 0.0
            if amp > 0.0 and sx > 0.0 and sy > 0.0:
                self._register_persisted_synthetic_params(amp, sx, sy)
        else:
            self._clear_persisted_synthetic_params()


    def _format_bg_params_text(self, params):
        if not isinstance(params, dict) or len(params) == 0:
            return "None"

        parts = []
        for key in sorted(params.keys()):
            value = params[key]
            try:
                value_text = f"{float(value):.6g}"
            except Exception:
                value_text = str(value)
            parts.append(f"{key}={value_text}")
        return ", ".join(parts)

    def _update_bg_method_summary(self):
        if not hasattr(self, 'currentBGMethodLabel'):
            return

        method = None
        params = None
        loss = None

        if self.quadFold is not None:
            result_bg = self.quadFold.info.get('result_bg', {}) or {}
            method = result_bg.get('method', None)
            params = result_bg.get('final_params', None)
            loss = result_bg.get('loss', None)

            if method in (None, ""):
                method = self.quadFold.info.get('bgsub', None)

        method_text = "None" if method in (None, "") else str(method)
        params_text = self._format_bg_params_text(params)
        loss_text = "—" if loss is None else _to_metric_text(loss, decimal_places=4)

        self.currentBGMethodLabel.setText(method_text)
        self.currentBGParamsLabel.setText(params_text)
        self.currentBGLossLabel.setText(loss_text)

        if hasattr(self, 'currentBGMethodLabelMain'):
            self.currentBGMethodLabelMain.setText(method_text)
            self.currentBGParamsLabelMain.setText(params_text)
            self.currentBGLossLabelMain.setText(loss_text)

    def _set_selected_optimization_methods(self, methods):
        """Set selected optimization methods in list widget."""
        method_set = set(methods)
        for i in range(self.optimizationMethodsList.count()):
            item = self.optimizationMethodsList.item(i)
            item.setSelected(item.text() in method_set)

    def _create_result_tab(self):
        """Create the result tab"""
        self.resultTab = QWidget()
        self.resultTab.setContentsMargins(0, 0, 0, 0)
        self.resultTabLayout = QHBoxLayout(self.resultTab)
        self.tabWidget.addTab(self.resultTab, "Results")
        
        # Create result canvas and right panel
        self._create_result_canvas()
        self._create_result_right_panel()
    
    def _create_result_canvas(self):
        """Create result canvas for displaying processed image"""
        # Reuse shared ImageViewerWidget for result display
        self.result_viewer = ImageViewerWidget(show_display_panel=True, show_double_zoom=True)
        
        # Keep backward-compatible references used across this class
        self.resultFigure = self.result_viewer.figure
        self.resultAxes = self.result_viewer.axes
        self.resultCanvas = self.result_viewer.canvas

        # Keep legacy/manual interaction behavior for Result tab
        # (disable built-in non-modal pan/scroll handlers in ImageViewerWidget)
        self.result_viewer._handle_pan_start = lambda event: None
        self.result_viewer._handle_pan_drag = lambda event: None
        self.result_viewer._handle_pan_end = lambda event: None
        self.result_viewer._handle_wheel_zoom = lambda event: None

        # Expose display panel controls for backward compatibility
        if self.result_viewer.display_panel:
            self.result_display_panel = self.result_viewer.display_panel
            self.spResultminInt = self.result_display_panel.minIntSpnBx
            self.spResultmaxInt = self.result_display_panel.maxIntSpnBx
            self.resLogScaleIntChkBx = self.result_display_panel.logScaleChkBx
            self.resPersistIntensity = self.result_display_panel.persistChkBx
            self.resultZoomInB = self.result_display_panel.zoomInBtn
            self.resultZoomOutB = self.result_display_panel.zoomOutBtn
            self.resultminIntLabel = self.result_display_panel.minIntLabel
            self.resultmaxIntLabel = self.result_display_panel.maxIntLabel

            # QuadrantFolding-specific defaults
            self.spResultminInt.setDecimals(0)
            self.spResultmaxInt.setDecimals(0)

            self.checkableButtons.append(self.resultZoomInB)

        self._add_result_display_options()

        self.resultTabLayout.addWidget(self.result_viewer, 1)  # Add stretch factor to fill space
    
    def _create_result_right_panel(self):
        """Create right panel for result tab using CollapsibleRightPanel (same as Image tab)"""
        # Create CollapsibleRightPanel to match Image tab layout
        self.result_right_panel = CollapsibleRightPanel(
            parent=self, 
            title="Options", 
            settings_key="quadrant/result_right_panel",
            start_visible=True
        )
    
        # Add widgets to panel (same structure as Image tab)
        self.result_right_panel.add_widget(self.result_viewer.display_panel)
        self.result_right_panel.add_widget(self.resProcGrpBx)
        
        # Navigation widget container (added to bottom, navControls moved here dynamically on tab switch)
        self.buttonsLayout2 = QGridLayout()
        buttonsWidget = QWidget()
        buttonsWidget.setLayout(self.buttonsLayout2)
        self.result_right_panel.add_bottom_widget(buttonsWidget)
        
        # Set fixed width
        self.result_right_panel.setFixedWidth(500)
        
        # Add to layout
        self.resultTabLayout.addWidget(self.result_right_panel, 0)  # No stretch, fixed width
        
        # Keep old references for backward compatibility
        self.rightLayout = self.result_right_panel.content_layout
        self.rightFrame = self.result_right_panel.content_widget
        self.res_scroll_areaImg = self.result_right_panel.scroll_area
    
    def _add_result_display_options(self):

        self.rotate90Chkbx = QCheckBox("Rotate 90 degree")
        self.rotate90Chkbx.setToolTip("Rotate the result image by 90 degrees for display")

        self.result_display_panel.add_to_bottom_slot(self.rotate90Chkbx)


    
    def _initialize_cicle_patches(self):
        """Initialize patch objects for circles"""
        self.circle_patch = None
        self.circle_patch_delta_in = None
        self.circle_patch_delta_out = None
        self.circle_patch_rmin = None
        self.circle_patch_rmax = None
    

    def setConnections(self):
        """
        Set all triggered functions for widgets
        """
        self.tabWidget.currentChanged.connect(self.onTabChanged)

        ##### Image Tab #####
        # Note: intensity/log_scale/colormap changes are handled automatically by ImageViewerWidget
        self.showSeparator.stateChanged.connect(self.refreshAllTabs)
        
        ##### Navigation Controls (shared between tabs) #####
        # Note: Basic navigation (next/prev/nextFile/prevFile) is now connected by BaseGUI's _connect_standard_navigation()
        self.navControls.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.navControls.processH5Button.toggled.connect(self.h5batchProcBtnToggled)
        self.processFolderWithSelections.clicked.connect(self.navControls.processFolderButton.click)

        # NOTE: Filename editing is handled internally by ImageNavigatorWidget._on_filename_changed()
        # which emits imageChanged signal that triggers _on_image_changed()
        # self.navControls.filenameLineEdit.editingFinished.connect(self.fileNameChanged)
        self.spResultmaxInt.valueChanged.connect(self.refreshResultTab)
        self.spResultminInt.valueChanged.connect(self.refreshResultTab)
        self.resLogScaleIntChkBx.stateChanged.connect(self.refreshResultTab)
        self.resultDisplayModeCB.currentIndexChanged.connect(self._on_result_display_mode_changed)
        self.toggleFoldImage.stateChanged.connect(self.onFoldChkBoxToggled)
        self.compressFoldedImageChkBx.stateChanged.connect(self.compressFoldedImageChanged)

        self.showRminRmaxChkBx.stateChanged.connect(self.toggleCircleRminRmax)
        self.rminSpnBx.valueChanged.connect(self.toggleCircleRminRmax)
        self.rmaxSpnBx.valueChanged.connect(self.toggleCircleRminRmax)

        self.showTranRadDeltaChkBx.stateChanged.connect(self.toggleCircleTransition)
        self.tranRSpnBx.valueChanged.connect(self.toggleCircleTransition)
        self.tranDeltaSpnBx.valueChanged.connect(self.toggleCircleTransition)

        self.equatorMaskHeightSpnBx.valueChanged.connect(self.equatorMaskHeightChanged)
        self.equatorCenterBeamSpnBx.valueChanged.connect(self.equatorMaskCenterBeamChanged)
        self.m1SpnBx.valueChanged.connect(self.m1Changed)
        self.layerLineWidthSpnBx.valueChanged.connect(self.layerLineMaskWidthChanged)

        self.amplitudeSpnBx.valueChanged.connect(self._refresh_synthetic_preview_if_visible)
        self.sigmaXSpnBx.valueChanged.connect(self._refresh_synthetic_preview_if_visible)
        self.sigmaYSpnBx.valueChanged.connect(self._refresh_synthetic_preview_if_visible)
        self.freqCB.currentIndexChanged.connect(self._refresh_synthetic_preview_if_visible)

        # self.expandImage.stateChanged.connect(self.expandImageChecked)

        self.selectImageButton.clicked.connect(self._on_browse_file)
        # Note: zoom buttons are handled internally by ImageViewerWidget
        
        # ===== Center Settings Widget Connections =====
        # NOTE: Tool buttons (chords, perpendiculars) are handled by ImageSettingsPanel
        # NOTE: Apply/Restore are handled by ImageSettingsPanel
        
        # QF-specific center buttons - now handled by workspace
        # NOTE: calibrationButton is now connected in ProcessingWorkspace._connect_signals()
        # NOTE: setCenterRotation and setCentBtn are now handled by ImageSettingsPanel
        
        # ===== Rotation Settings Widget Connections =====
        # NOTE: Rotation tool button is handled by ImageSettingsPanel
        # NOTE: Apply/Restore are handled by ImageSettingsPanel
        
        # NOTE: setAngleBtn and autoOrientationRequested are now connected directly in ImageSettingsPanel
        
        ##### Image Viewer Signals #####
        # Connect ImageViewerWidget signals instead of direct matplotlib events
        # NOTE: toolCompleted is handled by ImageSettingsPanel
        self.image_viewer.coordinatesChanged.connect(self._on_image_coordinates_changed)
        self.image_viewer.rightClickAt.connect(self._on_image_right_click)

        ##### Result Tab #####
        self.rotate90Chkbx.stateChanged.connect(self.processImage)
        self.resultZoomInB.clicked.connect(self.resultZoomIn)
        self.resultZoomOutB.clicked.connect(self.resultZoomOut)
        self.resultFigure.canvas.mpl_connect('button_press_event', self.resultClicked)
        self.resultFigure.canvas.mpl_connect('motion_notify_event', self.resultOnMotion)
        self.resultFigure.canvas.mpl_connect('button_release_event', self.resultReleased)
        self.resultFigure.canvas.mpl_connect('scroll_event', self.resultScrolled)

        # Empty cell image and mask settings widget - direct connections
        # Blank/Mask settings now handled by ImageSettingsPanel internally
        # Blank/Mask checkbox changes now handled by ImageSettingsPanel internally

        # Background Subtraction
        self.setFitRoi.clicked.connect(self.setFitRoiClicked)
        self.unsetRoi.clicked.connect(self.unsetRoiClicked)
        self.fixedRoiChkBx.stateChanged.connect(self.fixedRoiChecked)
        self.fixedRoiW.editingFinished.connect(self.fixedRoiChanged)
        self.fixedRoiH.editingFinished.connect(self.fixedRoiChanged)
        self.openBGSettingsButton.clicked.connect(self.openBackgroundSubtractionDialog)
        self.applyResultBGButton.clicked.connect(self.applyDefaultOptimization)
        self.addBackgroundConfigButton.clicked.connect(self.bgSubDialog.addBackgroundConfiguration)
        self.deleteBackgroundConfigButton.clicked.connect(self.bgSubDialog.deleteSelectedBackgroundConfiguration)
        self.assignConfgurationsManually.clicked.connect(self.bgSubDialog.openManualBackgroundAssignmentsDialog)
        self.backgroundConfigsTable.itemSelectionChanged.connect(self.bgSubDialog._on_background_config_selection_changed)
        self.backgroundConfigsTable.customContextMenuRequested.connect(self.bgSubDialog._show_background_config_context_menu)
        self.bgChoiceIn.currentIndexChanged.connect(self.bgChoiceInChanged)
        self.bgOptionsCB.currentIndexChanged.connect(self._on_bg_options_changed)
        if hasattr(self, "bgChoiceOut"):
            self.bgChoiceOut.currentIndexChanged.connect(self.bgChoiceOutChanged)
        
        self.minPixRange.valueChanged.connect(self.pixRangeChanged)
        self.maxPixRange.valueChanged.connect(self.pixRangeChanged)

        self.setRminRmaxButton.clicked.connect(self.setManualRminRmax)
        self.rminSpnBx.valueChanged.connect(self.RminChanged)
        self.rmaxSpnBx.valueChanged.connect(self.RmaxChanged)

        self.applyBGButton.clicked.connect(self.applyBGSub)
        self.applyBGButtonProxy.clicked.connect(self.applyBGButton.click)

        # self.blankImageGrp.clicked.connect(self.blankChecked)


    def _on_result_display_mode_changed(self,):
        self.refreshResultTab()
        self._refresh_synthetic_preview_if_visible()

    def _on_bg_options_changed(self, index):
        selection = self.bgOptionsCB.currentText()
        is_manual_one = selection == "Manual Setting | One Method"
        is_manual_transition = selection == "Manual Setting | Transition"
        is_automated = selection == "Automated Processing"

        self.applyResultBGButton.setVisible(is_automated)
        self.openBGSettingsButton.setVisible(is_automated)
        if hasattr(self, "applyBGButtonProxy"):
            self.applyBGButtonProxy.setVisible(is_manual_one or is_manual_transition)
        if hasattr(self, "manualSettingsContainer"):
            self.manualSettingsContainer.setVisible(is_manual_one or is_manual_transition)
        if hasattr(self, "innerBackgroundSectionHeading"):
            self.innerBackgroundSectionHeading.setVisible(is_manual_transition)
        if hasattr(self, "manualSettingsOutContainer"):
            self.manualSettingsOutContainer.setVisible(is_manual_transition)
        if hasattr(self, "transitionSettingsContainer"):
            self.transitionSettingsContainer.setVisible(is_manual_transition)
            if not is_manual_transition:
                self.showTranRadDeltaChkBx.setChecked(False)
        if hasattr(self, "bgSubDialog") and hasattr(self.bgSubDialog, "processingModeCB"):
            if is_manual_one or is_manual_transition:
                self.bgSubDialog.processingModeCB.setCurrentText("Manual")
                self.bgSubDialog.manualGroup.setVisible(True)
            elif is_automated:
                self.bgSubDialog.processingModeCB.setCurrentText("Automated")
        if self.quadFold is None:
            return
        else:
            self.quadFold.info["bg_options"] = index


    def updateLeftWidgetWidth(self):
        """Update left panel width based on image viewer visibility."""
        # leftWidget is now navigator's select_panel
        if self.leftWidget and self.imageCanvas.isVisible():
            # Remove the minimum width constraint when image is showing
            self.leftWidget.setMinimumWidth(0)
        elif self.leftWidget:
            # Set the minimum width when only buttons are showing
            self.leftWidget.setMinimumWidth(650)


    def compressFoldedImageChanged(self):
        """
        Handle when the compress folded image is checked or unchecked
        """
        if self.quadFold is not None and not self.uiUpdating:
            self.saveResults()

    def fixedRoiChecked(self):
        """
        Triggered when "Persist ROI size" is checked or unchecked.

        Pure UI state -- nothing is written to info here. Whether the
        current spinbox values are propagated to subsequent images is
        decided at processing time by getFlags().
        """
        return
    def toggleCircleTransition(self):
        if self.showTranRadDeltaChkBx.isChecked():
            # Remove existing circle if any
            if self.circle_patch is not None:
                try:
                    self.circle_patch.remove()
                except:
                    self.circle_patch = None
            if self.circle_patch_delta_in is not None:
                try:
                    self.circle_patch_delta_in.remove()
                except:
                    self.circle_patch_delta_in = None
            if self.circle_patch_delta_out is not None:
                try:
                    self.circle_patch_delta_out.remove()
                except:
                    self.circle_patch_delta_out = None

            # Create new circle (adjust x, y, radius as needed)
            radius = self.tranRSpnBx.value()
            delta = self.tranDeltaSpnBx.value()
            center = self.quadFold.center

            self.circle_patch = plt.Circle(center, radius,
                                        fill=False,
                                        color='red',
                                        linestyle='-',
                                        linewidth=1)
            self.circle_patch_delta_in = plt.Circle(center, radius+delta,
                                        fill=False,
                                        color='orange',
                                        linestyle='-.',
                                        linewidth=1)
            self.circle_patch_delta_out = plt.Circle(center, radius-delta,
                                        fill=False,
                                        color='orange',
                                        linestyle='-.',
                                        linewidth=1)

            # Add the circle to the axes
            self.resultAxes.add_patch(self.circle_patch)
            self.resultAxes.add_patch(self.circle_patch_delta_in)
            self.resultAxes.add_patch(self.circle_patch_delta_out)
        else:
            # Remove the circle if checkbox is unchecked
            if self.circle_patch is not None:
                self.circle_patch.remove()
                self.circle_patch = None
            if self.circle_patch_delta_in is not None:
                self.circle_patch_delta_in.remove()
                self.circle_patch_delta_in = None
            if self.circle_patch_delta_out is not None:
                self.circle_patch_delta_out.remove()
                self.circle_patch_delta_out = None

        # Redraw the canvas to show changes
        self.resultCanvas.draw()

    def toggleCircleRminRmax(self):
        if self.showRminRmaxChkBx.isChecked():
            if self.circle_patch_rmin is not None:
                try:
                    self.circle_patch_rmin.remove()
                except:
                    self.circle_patch_rmin = None

            if self.circle_patch_rmax is not None:
                try:
                    self.circle_patch_rmax.remove()
                except:
                    self.circle_patch_rmax = None

            rmin = self.rminSpnBx.value()
            rmax = self.rmaxSpnBx.value()
            center = self.quadFold.center

            self.circle_patch_rmin = plt.Circle(center, rmin,
                                        fill=False,
                                        color='green',
                                        linestyle='-',
                                        linewidth=1)

            self.circle_patch_rmax = plt.Circle(center, rmax,
                                        fill=False,
                                        color='green',
                                        linestyle='-',
                                        linewidth=1)

            self.resultAxes.add_patch(self.circle_patch_rmin)
            self.resultAxes.add_patch(self.circle_patch_rmax)
        else:
            if self.circle_patch_rmin is not None:
                self.circle_patch_rmin.remove()
                self.circle_patch_rmin = None
            if self.circle_patch_rmax is not None:
                self.circle_patch_rmax.remove()
                self.circle_patch_rmax = None

        self.resultCanvas.draw()


    def fixedRoiChanged(self):
        """
        Triggered when an ROI width/height spinbox value is edited.

        The spinbox is the source of truth for ROI; getFlags() reads it at
        processing time. We only need to kick off a reprocess so the new
        size takes effect.
        """
        if self.quadFold is None or self.uiUpdating:
            return
        self.result_zoom = None
        self.zoomOutClicked = True
        self.default_result_img_zoom = None
        self.processImage()

    def _updateRoiSpinboxesLive(self, w, h):
        """
        Push live ROI width/height values into the spinboxes without firing
        signals that would trigger reprocessing. Used during mouse drag.
        """
        for sb, val in ((self.fixedRoiW, w), (self.fixedRoiH, h)):
            blocker = QSignalBlocker(sb)
            sb.setValue(max(sb.minimum(), min(int(round(val)), sb.maximum())))
            del blocker


    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard
        """
        key = event.key()

        if key == Qt.Key_Right:
            self.nextClicked()
        elif key == Qt.Key_Left:
            self.prevClicked()
        elif key == Qt.Key_Escape:
            self.refreshAllTabs()
            self.workspace._center_widget.setCenterRotationButton.setChecked(False)
            self.workspace._center_widget.setCentByChords.setChecked(False)
            self.workspace._center_widget.setCentByPerp.setChecked(False)
            self.workspace._rotation_widget.setRotationButton.setChecked(False)



    # Blank/Mask settings moved to ImageSettingsPanel

    # Blank/Mask checkbox changes moved to ImageSettingsPanel

    def _get_display_center(self):
        """
        Get center in DISPLAY (transformed) coordinates.
        
        After transformImage(), the image is centered and the center moves to
        (w//2, h//2) in the transformed image. This is the center that should
        be used for drawing rotation lines on the displayed image.
        
        Returns:
            (x, y) center in display coordinates, or None if not available
        """
        if self.quadFold:
            return self.quadFold.center
        return None

    def getOrigCoordsCenter(self, x, y):
        """
        Convert coordinates from current (transformed) image to original image coordinates.
        Uses the inverse transformation matrix for accurate conversion.
        
        Args:
            x, y: Coordinates in current (possibly transformed) image
            
        Returns:
            (orig_x, orig_y): Coordinates in original image space
        """
        if self.quadFold:
            inv_transform = self.quadFold.info.get("inv_transform")
            
            if inv_transform is not None:
                # Convert using inverse transformation matrix
                point = np.array([x, y, 1])
                origin_point = inv_transform @ point
                origin_x, origin_y = origin_point
                return origin_x, origin_y
            else:
                # No transformation applied yet, coordinates are already in original space
                return x, y
        
        return x, y


    def getRectanglePatch(self, center, w, h):
        """
        Give the rectangle patch
        """
        leftTopCorner = (center[0] - w//2, center[1] - h//2)
        sq = patches.Rectangle(leftTopCorner, w, h, linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted')
        return sq

    def setFitRoiClicked(self):
        """
        Triggered when the Set fit roi button is clicked
        """
        if self.quadFold is None:
            return
        if self.setFitRoi.isChecked():
            self.imgPathOnStatusBar.setText(
                "Drag mouse pointer to select the box size, click on the image to accept (ESC to cancel)")
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.imageCanvas.draw_idle()
            self.function = ['fit_region']
            self.display_points = ['fit_region']
        else:
            self.function = None
            self.display_points = None
            self.resetStatusbar()

    def unsetRoiClicked(self):
        """
        Triggered when the unset roi button is clicked
        """
        if self.quadFold is not None:
            self.fixedRoiChkBx.setChecked(False)
            for sb in (self.fixedRoiW, self.fixedRoiH):
                blocker = QSignalBlocker(sb)
                sb.setValue(0)
                del blocker
            self.result_zoom = None
            self.zoomOutClicked = True
            self.default_result_img_zoom = None
            self.processImage()

    # NOTE: setCenterByPerpClicked, setCenterByChordsClicked, and setCentBtnClicked are now handled by ImageSettingsPanel

    # NOTE: _handle_apply_center, _handle_restore_auto_center, _handle_apply_rotation, 
    # _handle_restore_auto_rotation removed - now handled by ProcessingWorkspace with notifications
    
    # NOTE: _applyManualCenter, _restoreAutoCenter, _applyManualRotation, _restoreAutoRotation
    # have been moved to ImageSettingsPanel for better encapsulation
    
    # NOTE: setRotation is now handled by ImageSettingsPanel
    
    
    # ===== ImageViewerWidget Signal Handlers =====
    
    # NOTE: _on_tool_completed removed - no longer needed
    # 
    # All tools (chords, perpendiculars, rotation, center_rotate) are handled by
    # ImageSettingsPanel, which:
    #   - Updates settings
    #   - Updates ImageData
    #   - Updates UI
    #   - Emits needsReprocess signal → triggers processImage()
    #
    # Cache invalidation is handled automatically by:
    #   - QuadrantFolder.process() detects ImageData fingerprint changes
    #   - Automatically clears dependent caches (avg_fold, rmin, rmax)
    #
    # No manual cleanup needed in GUI!
    
    def _on_image_coordinates_changed(self, x, y, value):
        """
        Handler for coordinate changes (mouse movement over image).
        Updates the status bar with QuadrantFolding-specific information:
        - Current coordinates with calibrated distance/q-value
        - Original (pre-transform) coordinates
        
        Args:
            x, y: Coordinates in image space
            value: Pixel value at that position
        """
        
        # Get extent and center for coordinate transformations
        if self.quadFold is not None:
            center = self.quadFold.center
        else:
            center = self.current_image_data.center
        
        # Calculate distance from center
        mouse_distance = np.sqrt((center[0] - x) ** 2 + (center[1] - y) ** 2)
        
        # Business logic: Apply calibration if available
        unit = "px"
        distance_value = mouse_distance
        
        if self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
            scale = self.calSettings['scale']
            d = mouse_distance / scale
            
            if d > 0.01:
                # Convert to reciprocal space (q value)
                distance_value = 1.0 / d
                unit = "nm^-1"
            else:
                distance_value = mouse_distance
        
        # Update current coordinates status bar
        self.imgCoordOnStatusBar.setText(
            f"Cursor (Current coords): x={x:.2f}, y={y:.2f}, "
            f"value={value:.2f}, distance={distance_value:.2f} {unit}"
        )
        
        # Business logic: Transform to original coordinates and display
        o_x, o_y = self.getOrigCoordsCenter(x, y)
        self.left_status.setText(f"Cursor (Original coords): x={o_x:.2f}, y={o_y:.2f}")
    
    def _on_image_right_click(self, x, y):
        """
        Handler for right-click on image (from ImageViewerWidget).
        This is only called when no tool is handling the event.
        
        Args:
            x, y: Coordinates where right-click occurred
        """
        # Right-click menu for quadrant folding (ignore quadrant)
        if self.quadFold is None:
            return
        
        menu = QMenu(self)
        fold_number = self.quadFold.getFoldNumber(x, y)
        self.function = ["ignorefold", (x, y)]
        self.display_points = ["ignorefold", (x, y)]
        
        if fold_number not in self.quadFold.info["ignore_folds"]:
            ignoreThis = QAction('Ignore This Quadrant', self)
            ignoreThis.setToolTip("Exclude this quadrant from the fold so it does not contribute to the averaged result")
            ignoreThis.triggered.connect(self.addIgnoreQuadrant)
            menu.addAction(ignoreThis)
        else:
            unignoreThis = QAction('Unignore This Quadrant', self)
            unignoreThis.setToolTip("Re-include this quadrant in the fold")
            unignoreThis.triggered.connect(self.removeIgnoreQuadrant)
            menu.addAction(unignoreThis)
        
        # Clean up state when menu is closed without selection
        menu.aboutToHide.connect(lambda: self._clear_ignorefold_state())
        
        menu.popup(QCursor.pos())
    
    def _clear_ignorefold_state(self):
        """
        Clear ignorefold state if it's still set.
        This is called when the right-click menu is closed without selection.
        """
        if self.function is not None and self.function[0] == 'ignorefold':
            self.function = None
            self.display_points = None
    
    
    def _on_status_text_requested(self, text: str):
        """
        Handle status text update request from ImageSettingsPanel.
        
        Args:
            text: Status text to display (empty string means reset to default)
        """
        if text:
            self.imgPathOnStatusBar.setText(text)
        else:
            self.resetStatusbar()

    def resultZoomIn(self):
        """
        Trigger when set zoom in button is pressed (result tab)
        """
        if self.resultZoomInB.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a rectangle on the image to zoom in (ESC to cancel)")
            ax = self.resultAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.resultCanvas.draw_idle()
            self.function = ["r_zoomin"]
            self.display_points = ["r_zoomin"]
        else:
            self.function = None
            self.display_points = None
            self.resetStatusbar()

    def resultZoomOut(self):
        """
        Trigger when set zoom out button is pressed (result tab)
        """
        self.resultZoomInB.setChecked(False)
        self.result_zoom = None
        self.zoomOutClicked = True
        self.default_img_zoom = None
        self.default_result_img_zoom = None
        self.refreshResultTab()

    def RminChanged(self):
        """
        Triggered when R-min spinbox changes
        :return:
        """
        if  self.rminSpnBx.value() > 0 and not self.uiUpdating:
            self.setRmin(self.rminSpnBx.value())

    def RmaxChanged(self):
        """
        Triggered when R-max spinbox changes
        :return:
        """
        if  self.rmaxSpnBx.value() > 0 and not self.uiUpdating:
            self.setRmax(self.rmaxSpnBx.value())

    def setRmin(self, rmin):
        """
        Manual set R-min
        :param rmin: r-min value in pixel
        :return:
        """
        self.quadFold.info['rmin'] = rmin

        self.uiUpdating = True
        self.rminSpnBx.setValue(rmin)
        self.uiUpdating = False


    def _is_mask_preview_visible(self):
        if not hasattr(self, "resultDisplayModeCB"):
            return False
        return self.resultDisplayModeCB.currentText() in ("Evaluation Mask", "Synthetic Mask")

    def _on_evaluation_mask_settings_changed(self):
        """Evaluation mask controls changed; drop cached mask so Apply rebuilds it."""
        if self.uiUpdating or self.quadFold is None:
            return

        self._push_session_bg_eval_settings_to_info()
        self.deleteImgCache(['mask', 'synthetic_mask'])
        self._refresh_mask_preview_if_visible()

    def _refresh_mask_preview_if_visible(self):
        if not self.ableToProcess() or not self._is_mask_preview_visible():
            return

        # Push current UI values into info
        self._sync_eval_mask_settings_for_preview()

        # Force recompute on next draw
        self.deleteImgCache(['mask', 'synthetic_mask'])

        self.updated['result'] = False
        self.updateResultTab()

    def _is_synthetic_preview_visible(self):
        if not hasattr(self, "resultDisplayModeCB"):
            return False
        return self.resultDisplayModeCB.currentText() in ("Synthetic Signal", "Synthetic Mask")

    def _sync_synthetic_settings_for_preview(self):
        if self.quadFold is None:
            return
        info = self.quadFold.info
        info['synthetic_amplitude'] = float(self.amplitudeSpnBx.value())
        info['synthetic_sigma_x'] = float(self.sigmaXSpnBx.value())
        info['synthetic_sigma_y'] = float(self.sigmaYSpnBx.value())
        info['freq'] = str(self.freqCB.currentText())
        info['save_metrics_to_csv'] = bool(
            hasattr(self, "saveMetricsToCsvChkBx")
            and self.saveMetricsToCsvChkBx is not None
            and self.saveMetricsToCsvChkBx.isChecked()
        )

    def _refresh_synthetic_preview_if_visible(self):
        if self.uiUpdating or not self.ableToProcess() or not self._is_synthetic_preview_visible():
            return
        if self.quadFold is None or 'avg_fold' not in self.quadFold.imgCache:
            return

        self._sync_synthetic_settings_for_preview()

        self.deleteImgCache(['synthetic_mask','synthetic_data', 'avg_fold_with_syn','_avg_fold_with_syn',\
             'BgFold_syn', 'BgSubFold_syn', 'BgFold_syn_in', 'BgSubFold_syn_in', \
                'BgFold_syn_out', 'BgSubFold_syn_out'])

        try:
            self.quadFold.createArtificialData()
        except Exception:
            return

        self.updated['result'] = False
        self.updateResultTab()


    def setRmax(self, rmax):
        """
        Manual set R-max
        :param rmax: r-max value in pixel
        :return:
        """
        self.quadFold.info['rmax'] = rmax

        self.uiUpdating = True
        self.rmaxSpnBx.setValue(rmax)
        self.uiUpdating = False

    def equatorMaskHeightChanged(self):
        """Triggered when Equator Mask Height changes."""
        self._on_evaluation_mask_settings_changed()

    def equatorMaskCenterBeamChanged(self):
        """Triggered when Equator Mask Center Beam changes."""
        self._on_evaluation_mask_settings_changed()

    def m1Changed(self):
        """Triggered when M1 spacing changes."""
        self._on_evaluation_mask_settings_changed()

    def layerLineMaskWidthChanged(self):
        """Triggered when layer line width changes."""
        self._on_evaluation_mask_settings_changed()



    def resultClicked(self, event):
        """
        Triggered when mouse presses on image in result tab
        """
        if not self.ableToProcess():
            return

        x = event.xdata
        y = event.ydata
        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.imgCoordOnStatusBar.setText("")
            ax = self.resultAxes
            bounds = ax.get_window_extent().get_points()  ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[0] - ylim[1]) / (bounds[0][1] - bounds[1][1])  ### todo
            cy = ylim[1] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])
            x = int(round(x))
            y = int(round(y))

        # Provide different behavior depending on current active function
        if self.function is None:
            self.function = ["r_move", (x, y)]
            self.display_points = ["r_move", (x, y)]
        else:
            func = self.function
            if func[0] == "r_zoomin":
                # Set new zoom in location
                func.append((x, y))
                self.display_points.append((x, y))
                if len(func) == 3:
                    p1 = func[1]
                    p2 = func[2]
                    self.result_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.display_points = None
                    self.resultZoomInB.setChecked(False)
                    self.refreshResultTab()
            elif func[0] == "rminmax":
                # Set new R-min and R-max. The result image is 2x the
                # avg_fold (top-left quadrant mirrored 4 ways), so its
                # geometric center is exactly (avg_fold.shape[1],
                # avg_fold.shape[0]) -- which is shape // 2 of the
                # full result. Using resultImg directly avoids
                # depending on info['avg_fold'] being in memory (it
                # isn't on the fast-path).
                img = self.quadFold.imgCache['resultImg']
                center = (img.shape[1] / 2, img.shape[0] / 2)
                radius = distance((x, y), center)
                func.append(radius)
                self.display_points.append(radius)
                ax = self.resultAxes
                ax.add_patch(
                    patches.Circle(center, radius, linewidth=2, edgecolor='r', facecolor='none', linestyle='solid'))
                if len(func) == 3:
                    rmin = int(round(min(func[1:])))
                    rmax = int(round(max(func[1:])))

                    self.setRmin(rmin)
                    self.setRmax(rmax)
                    self.function = None
                    self.display_points = None
                    self.setRminRmaxButton.setChecked(False)

                    for patch in ax.patches[:]:
                        patch.remove()
                    self.resultCanvas.draw()



            elif func[0] == "fit_region":
                result_shape = self.quadFold.imgCache['resultImg'].shape
                cx = result_shape[1] / 2
                cy = result_shape[0] / 2
                w = max(1, int(round(2 * abs(x - cx))))
                h = max(1, int(round(2 * abs(y - cy))))
                print("Selected Fit Region size is W=", w, " H=", h)

                # Spinboxes are the canonical ROI state -- write here, then
                # let processImage()/getFlags() push the value into info.
                self._updateRoiSpinboxesLive(w, h)
                print("New Image shape ", result_shape)
                self.setFitRoi.setChecked(False)
                self.result_zoom = None
                self.zoomOutClicked = True
                self.default_result_img_zoom = None
                self.processImage()

    def resultOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """
        if not self.ableToProcess():
            return

        x = event.xdata
        y = event.ydata
        ax = self.resultAxes
        img = self.quadFold.imgCache["resultImg"]
        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < img.shape[1] and y < img.shape[0]:
                self.imgCoordOnStatusBar.setText(
                    "x=" + str(x) + ', y=' + str(y) + ", value=" + str(np.round(img[y][x], 2)))

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.imgCoordOnStatusBar.setText("")
            bounds = ax.get_window_extent().get_points()  ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[0] - ylim[1]) / (bounds[0][1] - bounds[1][1])  ### todo
            cy = ylim[1] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])
            x = int(round(x))
            y = int(round(y))

        if self.function is None:
            return

        func = self.function

        if func[0] == "r_zoomin" and len(self.function) == 2:
            # draw rectangle
            if len(ax.patches) > 0:
                ax.patches[0].remove()
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.resultCanvas.draw_idle()
        elif func[0] == "rminmax":
            # draw circles. See the resultClicked() rminmax branch for
            # why we read the center from imgCache['resultImg'] instead
            # of info['avg_fold'].
            img = self.quadFold.imgCache['resultImg']
            center = (img.shape[1] / 2 - 1, img.shape[0] / 2 - 1)
            radius = distance((x, y), center)
            if len(ax.patches) > len(self.function) - 1:
                ax.patches[-1].remove()
            ax.add_patch(
                patches.Circle(center, radius, linewidth=2, edgecolor='r', facecolor='none', linestyle='solid'))
            self.resultCanvas.draw_idle()

        elif func[0] == "fit_region":
            cx = img.shape[1] / 2
            cy = img.shape[0] / 2
            if len(ax.patches) > 0:
                for i in range(len(ax.patches) - 1, -1, -1):
                    ax.patches[i].remove()
            w = int(round(2 * abs(x - cx)))
            h = int(round(2 * abs(y - cy)))
            w = max(1, w)
            h = max(1, h)
            sq = self.getRectanglePatch((cx, cy), w, h)
            ax.add_patch(sq)
            self.resultCanvas.draw_idle()
            self._updateRoiSpinboxesLive(w, h)
            self.imgCoordOnStatusBar.setText(
                "x=" + str(int(round(x))) + ", y=" + str(int(round(y))) +
                "  ROI W=" + str(w) + ", H=" + str(h))

        elif func[0] == "r_move":
            # move zoom in location when image dragged
            if self.result_zoom is not None:
                move = (func[1][0] - x, func[1][1] - y)
                self.result_zoom = getNewZoom(self.result_zoom, move, img.shape[1], img.shape[0])
                ax.set_xlim(self.result_zoom[0])
                ax.set_ylim(self.result_zoom[1])
                #ax.invert_yaxis()
                self.resultCanvas.draw_idle()

    def resultReleased(self, event):
        """
        Triggered when mouse released from image in result tab
        """
        if self.function is not None and self.function[0] == "r_move":
            self.function = None
            self.display_points = None

    def resultScrolled(self, event):
        """
        This function is called when a mouse scrolled on the image in result tab. This will affect zoom-in and zoom-out
        """
        if self.quadFold is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img = self.quadFold.imgCache["resultImg"]
        img_size = img.shape

        if self.result_zoom is None:
            self.result_zoom = [(0, img_size[1]), (0, img_size[0])]

        zoom_height = self.result_zoom[1][1] - self.result_zoom[1][0]
        zoom_width = self.result_zoom[0][1] - self.result_zoom[0][0]

        clicked_x_percentage = 1. * (x - self.result_zoom[0][0]) / zoom_width
        clicked_y_percentage = 1. * (y - self.result_zoom[1][0]) / zoom_height

        step_x = .1 * zoom_width
        step_y = .1 * zoom_height
        if direction == 'up':  # zoom in
            step_x *= -1
            step_y *= -1
        zoom_width = min(img_size[1], max(zoom_width + step_x, 50))
        zoom_height = min(img_size[0], max(zoom_height + step_y, 50))

        x1 = x - clicked_x_percentage * zoom_width
        x2 = x1 + zoom_width
        y1 = y - clicked_y_percentage * zoom_height
        y2 = y1 + zoom_height

        if x1 < 0:
            x1 = 0
            x2 = zoom_width

        if y1 < 0:
            y1 = 0
            y2 = zoom_height

        if x2 > img_size[1]:
            x2 = img_size[1]
            x1 = img_size[1] - zoom_width

        if y2 > img_size[0]:
            y2 = img_size[0]
            y1 = img_size[0] - zoom_height

        self.result_zoom = [(x1, x2), (y1, y2)]
        ax = self.resultAxes
        ax.set_xlim(self.result_zoom[0])
        ax.set_ylim(self.result_zoom[1])
        # Y-axis already inverted in updateResultTab, don't toggle it again
        self.resultCanvas.draw_idle()

    def setManualRminRmax(self):
        """
        Prepare for R-min settings after button clicked
        """
        if self.setRminRmaxButton.isChecked():
            self.imgPathOnStatusBar.setText(
                "Select R-min and R-max on the image (ESC to cancel)")
            self.function = ['rminmax'] # set active function
            self.display_points = ['rminmax']
            ax = self.resultAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            self.resultCanvas.draw_idle()
        else:
            self.function = None
            self.display_points = None
            self.setRminRmaxButton.setChecked(False)
            self.refreshResultTab()
            self.resetStatusbar()


    def pixRangeChanged(self):
        """
        Trigger when pixel range is changed
        """
        if self.minPixRange.value() > self.maxPixRange.value():
            # Check value
            self.minPixRange.setValue(self.maxPixRange.value())
            return


    def bgChoiceInChanged(self):
        """
        Trigger when background subtraction method is changed
        Available Choices : 'None', '2D Convexhull', 'Circularly-symmetric', 'Roving Window', 'White-top-hats', 'Smoothed-Gaussian', 'Smoothed-BoxCar'
        """
        choice = self.bgChoiceIn.currentText()

        def _set_hidden(widget, hidden):
            if widget is not None:
                widget.setHidden(hidden)

        def _set_hidden_proxy(key, hidden):
            _set_hidden(getattr(self, key, None), hidden)
            if hasattr(self, "_manual_proxy") and key in self._manual_proxy:
                _set_hidden(self._manual_proxy[key], hidden)

        _set_hidden_proxy('tophatSpnBx', not choice == 'White-top-hats')
        _set_hidden_proxy('tophatLabel', not choice == 'White-top-hats')
        _set_hidden_proxy('windowSizeLabel', not choice == 'Roving Window')
        _set_hidden_proxy('winSizeX', not choice == 'Roving Window')
        _set_hidden_proxy('winSizeY', not choice == 'Roving Window')
        _set_hidden_proxy('windowSepLabel', not choice == 'Roving Window')
        _set_hidden_proxy('winSepX', not choice == 'Roving Window')
        _set_hidden_proxy('winSepY', not choice == 'Roving Window')
        _set_hidden_proxy('maxPixRange', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_proxy('minPixRange', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_proxy('pixRangeLabel', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_proxy('gaussFWHMLabel', not choice == 'Smoothed-Gaussian')
        _set_hidden_proxy('gaussFWHM', not choice == 'Smoothed-Gaussian')
        _set_hidden_proxy('boxcarLabel', not choice == 'Smoothed-BoxCar')
        _set_hidden_proxy('boxcarX', not choice == 'Smoothed-BoxCar')
        _set_hidden_proxy('boxcarY', not choice == 'Smoothed-BoxCar')
        _set_hidden_proxy('degreeLabel', not choice == '2D Convexhull')
        _set_hidden_proxy('degreeCB', not choice == '2D Convexhull')
        _set_hidden_proxy('cycleLabel', not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        _set_hidden_proxy('cycle', not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        _set_hidden_proxy('thetaBinLabel', True)
        _set_hidden_proxy('thetabinCB', True)

        _set_hidden_proxy('radialBinSpnBx', not choice == 'Circularly-symmetric')
        _set_hidden_proxy('radialBinLabel', not choice == 'Circularly-symmetric')
        _set_hidden_proxy('smoothLabel', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_proxy('smoothSpnBx', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_proxy('tensionLabel', not choice in ('Roving Window'))
        _set_hidden_proxy('tensionSpnBx', not choice in ('Roving Window'))

    def bgChoiceOutChanged(self):
        """
        Trigger when OUT background subtraction method is changed.
        """
        choice = self.bgChoiceOut.currentText()

        def _set_hidden(widget, hidden):
            if widget is not None:
                widget.setHidden(hidden)

        def _set_hidden_out_widget(key, hidden):
            _set_hidden(getattr(self, key, None), hidden)

        _set_hidden_out_widget('tophatOutSpnBx', not choice == 'White-top-hats')
        _set_hidden_out_widget('tophatOutLabel', not choice == 'White-top-hats')
        _set_hidden_out_widget('windowSizeOutLabel', not choice == 'Roving Window')
        _set_hidden_out_widget('winSizeOutX', not choice == 'Roving Window')
        _set_hidden_out_widget('winSizeOutY', not choice == 'Roving Window')
        _set_hidden_out_widget('windowSepOutLabel', not choice == 'Roving Window')
        _set_hidden_out_widget('winSepOutX', not choice == 'Roving Window')
        _set_hidden_out_widget('winSepOutY', not choice == 'Roving Window')
        _set_hidden_out_widget('maxPixRangeOut', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_out_widget('minPixRangeOut', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_out_widget('pixRangeLabelOut', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_out_widget('gaussFWHMOutLabel', not choice == 'Smoothed-Gaussian')
        _set_hidden_out_widget('gaussFWHMOut', not choice == 'Smoothed-Gaussian')
        _set_hidden_out_widget('boxcarOutLabel', not choice == 'Smoothed-BoxCar')
        _set_hidden_out_widget('boxcarOutX', not choice == 'Smoothed-BoxCar')
        _set_hidden_out_widget('boxcarOutY', not choice == 'Smoothed-BoxCar')
        _set_hidden_out_widget('cycleOutLabel', not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        _set_hidden_out_widget('cycleOut', not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        _set_hidden_out_widget('thetaBinOutLabel', True)
        _set_hidden_out_widget('thetaBinOutCB', True)

        _set_hidden_out_widget('radialBinOutSpnBx', not choice == 'Circularly-symmetric')
        _set_hidden_out_widget('radialBinOutLabel', not choice == 'Circularly-symmetric')
        _set_hidden_out_widget('smoothOutLabel', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_out_widget('smoothOutSpnBx', not choice in ('Roving Window', 'Circularly-symmetric'))
        _set_hidden_out_widget('tensionOutLabel', not choice in ('Roving Window'))
        _set_hidden_out_widget('tensionOutSpnBx', not choice in ('Roving Window'))


    def updateImportedBG(self):
        """
        Update
        :return:
        """
        text = 'Imported Background :\n'
        if len(self.BGImages) > 0:
            imgs = [split(p)[1] for p in self.BGImages]
            text += "\n".join(list(map(str, imgs)))
        self.importedBG.setText(text)

    def applyBGSub(self, skip_confirm: bool = False):
        """
        Reprocess about background subtraction when some parameters are changed
        """
        QApplication.processEvents()
        if self._OptimizationRunning:
            self._stopOptimizationRequested = True
            self.stop_process = True
            if self.quadFold is not None:
                self.quadFold.info['optimize'] = False
            self.optimizeFlag = False
            
            self._restoreOptimizeCheckboxAfterProcess = False
            self._set_optimization_button_running(False)
            return

        if hasattr(self, "processingModeCB") and self.processingModeCB.currentText() == "Automated" and not skip_confirm:
            parent = self.bgSubDialog if hasattr(self, "bgSubDialog") else self
            msg_box = QMessageBox(parent)
            msg_box.setIcon(QMessageBox.Warning)
            msg_box.setWindowTitle("Run Automated Optimization")
            msg_box.setText("Automated optimization may take some time.")
            msg_box.setInformativeText("Do you want to continue?")
            msg_box.setStandardButtons(QMessageBox.Ok | QMessageBox.Cancel)
            msg_box.setDefaultButton(QMessageBox.Ok)
            if msg_box.exec_() != QMessageBox.Ok:
                return

        if not self.ableToProcess():
            return

        if hasattr(self, "processingModeCB") and self.processingModeCB.currentText() == "Automated":
            checked = self.optimizeFlag is True
            if not checked:
                self.optimizeFlag = True

            self._restoreOptimizeCheckboxAfterProcess = not checked
            self.stop_process = False
            self._set_optimization_button_running(True)

        self._push_session_bg_eval_settings_to_info()
        self.deleteInfo(['result_bg'])
        self.deleteImgCache([
            'mask',
            'BgSubFold', 'BgSubFold_out', 'BgSubFold_syn', 'BgSubFold_syn_out',
            'BgFold', 'BgFold_out', 'BgFold_syn', 'BgFold_syn_out',
            'BgSubFold_in', 'BgFold_in', 'BgSubFold_syn_in', 'BgFold_syn_in',
            'BgSubFold_syn_out', 'BgFold_syn_out',
        ])
        # self._force_no_fast_path_on_process = True

        self.resultDisplayModeCB.setEnabled(False)

        self.processImage()

        if hasattr(self, "processingModeCB") and self.processingModeCB.currentText() == "Automated" and not checked:
            self.optimizeFlag = False





    # NOTE: orientationModelChanged removed - fully handled by ImageSettingsPanel.set_orientation_model() now

    # NOTE: getModeRotation removed - use workspace.calculate_mode_rotation() directly

    def ableToProcess(self):
        """
        Check if image can be processed
        """
        return self.quadFold is not None and not self.uiUpdating and not self._singleProcessing

    def resetAllManual(self):
        """
        Remove center from QuadrantFolder to make it recalculate everything from finding center
        """
        # Reset center to force recalculation (use None to trigger auto mode)
        self.current_image_data.update_manual_center(None)
        self.processImage()

    def addIgnoreQuadrant(self):
        """
        Trigger when a quadrant is ignored
        """
        fold_number = self.quadFold.getFoldNumber(self.function[1][0], self.function[1][1])
        self.function = None
        self.display_points = None
        self.ignoreFolds.add(fold_number)
        self.deleteImgCache(['avg_fold'])
        self.processImage()

    def removeIgnoreQuadrant(self):
        """
        Trigger when a quadrant is unignored
        """
        fold_number = self.quadFold.getFoldNumber(self.function[1][0], self.function[1][1])
        self.function = None
        self.display_points = None
        self.ignoreFolds.remove(fold_number)
        self.deleteImgCache(['avg_fold'])
        self.processImage()

    def deleteInfo(self, delList):
        """
        Remove input keys from info dict of current QuadrantFolder object
        :param delList: list of keys
        """
        if self.ableToProcess():
            for inf in delList:
                if inf in self.quadFold.info.keys():
                    del self.quadFold.info[inf]

    def deleteImgCache(self, delList):
        """
        Remove input keys from imgCache dict of current QuadrantFolder object
        :param delList: list of keys
        """
        if self.ableToProcess():
            for inf in delList:
                if inf in self.quadFold.imgCache:
                    del self.quadFold.imgCache[inf]

    def initialWidgets(self, img, previnfo):
        """
        Restore processing parameter widgets for current image.
        
        Note: Display panel intensity controls (spmaxInt, spminInt, labels) are now 
        automatically updated by ImageViewerWidget.display_image(), so we don't need 
        to handle them here. We only handle QuadrantFolding-specific widgets.
        
        :param img: selected image
        :param previnfo: info of the last image
        """
        self.uiUpdating = True
        
        # Get image statistics for Result panel (still needed)
        min_val = img.min()
        max_val = img.max()
        
        # Display panel (spmaxInt, spminInt, etc.) is automatically updated by ImageViewerWidget!
        # We only need to handle QuadrantFolding-specific widgets here.
        
        # Update Result panel decimals (these are QF-specific, not part of display panel)
        if 'float' in str(img.dtype):
            self.spResultmaxInt.setDecimals(2)
            self.spResultminInt.setDecimals(2)
        else:
            self.spResultmaxInt.setDecimals(2)
            self.spResultminInt.setDecimals(2)

        info = self.quadFold.info

        # Evaluation baseline: per-image (from cached info / auto after process) vs persist across navigations.
        eb_min = qf_defaults.MIN_EVAL_BASELINE
        persist_eval_baseline = (
            hasattr(self, "persistEvaluationBaselineChkBx")
            and self.persistEvaluationBaselineChkBx is not None
            and self.persistEvaluationBaselineChkBx.isChecked()
        )
        if previnfo is not None and persist_eval_baseline:
            prev_b = previnfo.get("evaluation_baseline")
            try:
                if prev_b is not None:
                    fv = float(prev_b)
                    if fv > 0:
                        self.evaluationBaselineSpnBx.setValue(max(eb_min, fv))
            except (TypeError, ValueError):
                pass
        else:
            cur_b = info.get("evaluation_baseline")
            try:
                if cur_b is not None:
                    fv = float(cur_b)
                    if fv > 0:
                        self.evaluationBaselineSpnBx.setValue(max(eb_min, fv))
                    else:
                        self.evaluationBaselineSpnBx.setValue(qf_defaults.DEFAULT_EVAL_BASELINE)
                else:
                    self.evaluationBaselineSpnBx.setValue(qf_defaults.DEFAULT_EVAL_BASELINE)
            except (TypeError, ValueError):
                self.evaluationBaselineSpnBx.setValue(qf_defaults.DEFAULT_EVAL_BASELINE)

        if "bgsub" in info:
            self.bgChoiceIn.setCurrentIndex(self.allBGChoices.index(info['bgsub']))
            if info['bgsub'] != 'None':
                self.tophatSpnBx.setValue(int(info.get('tophat', self.tophatSpnBx.value())))
                self.maxPixRange.setValue(float(info.get("cirmax", self.maxPixRange.value())))
                self.minPixRange.setValue(float(info.get("cirmin", self.minPixRange.value())))

                self.radialBinSpnBx.setValue(int(info.get('radial_bin', self.radialBinSpnBx.value())))
                self.smoothSpnBx.setValue(float(info.get('smooth', self.smoothSpnBx.value())))
                self.tensionSpnBx.setValue(float(info.get('tension', self.tensionSpnBx.value())))

                if previnfo is None or not self.fixedRadiusRangeChkBx.isChecked():
                    self.rminSpnBx.setValue(int(info.get('rmin', self.rminSpnBx.value())))
                else:
                    self.rminSpnBx.setValue(previnfo['rmin'])

                if previnfo is None or not self.fixedRadiusRangeChkBx.isChecked():
                    self.rmaxSpnBx.setValue(int(info.get('rmax', self.rmaxSpnBx.value())))
                else:
                    self.rmaxSpnBx.setValue(previnfo['rmax'])

                self.winSizeX.setValue(int(info.get('win_size_x', self.winSizeX.value())))
                self.winSizeY.setValue(int(info.get('win_size_y', self.winSizeY.value())))
                self.winSepX.setValue(int(info.get('win_sep_x', self.winSepX.value())))
                self.winSepY.setValue(int(info.get('win_sep_y', self.winSepY.value())))
                self.gaussFWHM.setValue(int(info.get('fwhm', self.gaussFWHM.value())))
                self.boxcarX.setValue(int(info.get('boxcar_x', self.boxcarX.value())))
                self.boxcarY.setValue(int(info.get('boxcar_y', self.boxcarY.value())))
                self.cycle.setValue(int(info.get('cycles', self.cycle.value())))
                self.degreeCB.setCurrentIndex(1)

            if 'optimize' in info:
                self.optimizeFlag = bool(info['optimize'])
            if 'steps' in info and isinstance(info['steps'], (list, tuple)):
                self.stepsLineEdit.setText(", ".join([str(v) for v in info['steps']]))
            if 'max_iterations' in info:
                self.maxIterationsSpnBx.setValue(int(info['max_iterations']))
            if 'early_stop' in info:
                self.earlyStopSpnBx.setValue(float(info['early_stop']))
            if 'optimize_timeout' in info:
                self.optimizeTimeoutSpnBx.setValue(int(info['optimize_timeout']))
            if 'methods' in info and isinstance(info['methods'], (list, tuple)):
                self._set_selected_optimization_methods(info['methods'])
            if 'downsample' in info:
                self.downsampleCB.setCurrentText(str(info['downsample']))
            if 'smooth_image' in info:
                self.smoothImageChkbx.setChecked(info['smooth_image'])

        self._sync_metric_and_synthetic_widgets_from_info(info=info)
        self._push_session_bg_eval_settings_to_info(info=info)

        # Range is already set to allow any value at spinbox creation
        if not self.resPersistIntensity.isChecked():
            self.spResultmaxInt.setValue(max_val * .1)
            self.spResultminInt.setValue(min_val)
        self.spResultmaxInt.setSingleStep(max_val * .05)
        self.spResultminInt.setSingleStep(max_val * .05)

        if 'rotate' in info:
            self.rotate90Chkbx.setChecked(info['rotate'])

        self.uiUpdating = False

    def _push_session_bg_eval_settings_to_info(self, info=None):
        """Apply evaluation / mask / synthetic controls from the UI into ``info``.

        Session-global widgets drive processing when navigating images; each folder ``info`` is updated so the backend reads consistent keys.

        Evaluation baseline is only pushed when "Persist evaluation baseline" is checked; otherwise each image keeps its own ``evaluation_baseline`` (from cache / per-image ``evaluateResult``)."""
        if not hasattr(self, "quadFold") or self.quadFold is None:
            return
        if info is None:
            info = self.quadFold.info
        if not isinstance(info, dict):
            return
        persist_eval = (
            hasattr(self, "persistEvaluationBaselineChkBx")
            and self.persistEvaluationBaselineChkBx is not None
            and self.persistEvaluationBaselineChkBx.isChecked()
        )
        if persist_eval:
            info['evaluation_baseline'] = max(
                qf_defaults.MIN_EVAL_BASELINE,
                float(self.evaluationBaselineSpnBx.value()),
            )
        info['equator_mask_height'] = int(self.equatorMaskHeightSpnBx.value())
        info['equator_center_beam_width'] = int(self.equatorCenterBeamSpnBx.value())
        info['m1'] = int(self.m1SpnBx.value())
        info['layer_line_width'] = int(self.layerLineWidthSpnBx.value())
        # Synthetic Gaussian params: same shape as evaluation_baseline above.
        # When persistence is OFF we leave info alone so the per-image
        # values computed by QuadrantFolder._ensure_synthetic_gaussian_params
        # are preserved. When persistence is ON, the UI / persisted tuple
        # wins across images.
        persist_synth = self._is_synthetic_data_persisted()
        info['persist_synthetic_data'] = persist_synth
        if persist_synth:
            persisted = self._get_persisted_synthetic_params()
            if persisted is not None:
                amp, sx, sy = persisted
            else:
                amp = float(self.amplitudeSpnBx.value())
                sx = float(self.sigmaXSpnBx.value())
                sy = float(self.sigmaYSpnBx.value())
            info['synthetic_amplitude'] = amp
            info['synthetic_sigma_x'] = sx
            info['synthetic_sigma_y'] = sy
        info['freq'] = str(self.freqCB.currentText())

    def _seed_synthetic_spinboxes_from_ensure_defaults(self):
        """Set synthetic Gaussian spinboxes using ``ensureSyntheticGaussianDefaults`` once ``avg_fold`` exists.

        Called after the first successful process for the current image so values match the folded pattern;
        respects ``MIN_SYNTHETIC_*`` via ``QuadrantFolder._ensure_synthetic_gaussian_params``."""
        if not hasattr(self, "quadFold") or self.quadFold is None:
            return
        if "avg_fold" not in self.quadFold.imgCache:
            return
        computed = self.quadFold.ensureSyntheticGaussianDefaults()
        if computed is None:
            return
        amp, sig_x, sig_y = computed
        self.uiUpdating = True
        try:
            self.amplitudeSpnBx.setValue(float(amp))
            self.sigmaXSpnBx.setValue(float(sig_x))
            self.sigmaYSpnBx.setValue(float(sig_y))
        finally:
            self.uiUpdating = False
        self._push_session_bg_eval_settings_to_info()

    def _sync_metric_and_synthetic_widgets_from_info(self, info=None):
        """
        Sync metric table controls from cached info (means and weights).

        Evaluation baseline respects "Persist evaluation baseline" (see Background Subtraction settings): when off, baseline is restored per image from ``info``; when on, UI wins across navigations via ``_push_session_bg_eval_settings_to_info``.

        Equator / layer mask parameters remain session-driven and are
        written into ``info`` by ``_push_session_bg_eval_settings_to_info``.
        Synthetic Gaussian parameters and evaluation baseline are loaded
        from ``info`` when present.
        Refresh the loss table after spinboxes are updated so table text
        matches cache.
        """
        if info is None:
            if not hasattr(self, "quadFold") or self.quadFold is None:
                return
            info = self.quadFold.info
        if not isinstance(info, dict):
            return

        if 'mean_metric_values' in info and isinstance(info['mean_metric_values'], dict):
            means = info['mean_metric_values']
            self.meanMSESpnBx.setValue(float(means.get('MSE_SYN_MEAN', self.meanMSESpnBx.value())))
            self.meanNegSynSpnBx.setValue(_fraction_to_percent_for_ui(means.get('SHARE_NEG_SYN_MEAN', self.meanNegSynSpnBx.value())))
            self.meanNonBaselineSpnBx.setValue(_fraction_to_percent_for_ui(means.get('SHARE_NON_BASELINE_MEAN', self.meanNonBaselineSpnBx.value())))
            self.meanNegConSpnBx.setValue(_fraction_to_percent_for_ui(means.get('SHARE_NEG_CON_MEAN', self.meanNegConSpnBx.value())))
            self.meanSmoothSpnBx.setValue(float(means.get('SMOOTH_MEAN', self.meanSmoothSpnBx.value())))

        if 'metric_weights' in info and isinstance(info['metric_weights'], dict):
            weights = info['metric_weights']
            self.weightMSESpnBx.setValue(float(weights.get('MSE', self.weightMSESpnBx.value())))
            self.weightNegSynSpnBx.setValue(float(weights.get('Share_Neg_Synthetic', self.weightNegSynSpnBx.value())))
            self.weightNonBaselineSpnBx.setValue(float(weights.get('Share_Non_Baseline', self.weightNonBaselineSpnBx.value())))
            self.weightNegConSpnBx.setValue(float(weights.get('Share_Neg_Connected', self.weightNegConSpnBx.value())))
            self.weightSmoothSpnBx.setValue(float(weights.get('Smoothness', self.weightSmoothSpnBx.value())))

        if 'evaluation_baseline' in info:
            self.evaluationBaselineSpnBx.setValue(max(
                qf_defaults.MIN_EVAL_BASELINE,
                float(info.get('evaluation_baseline', self.evaluationBaselineSpnBx.value()))
            ))
        # Synthetic Gaussian params follow the persist-synthetic-data toggle
        # (see "Persist synthetic data" in Background Subtraction settings).
        # When OFF, the spinboxes display the most recent per-image computed
        # values; we never restore them from prior cache here because that
        # would defeat the point of recomputing per image. Once processing
        # completes for the current image, _seed_synthetic_spinboxes_from_ensure_defaults
        # repopulates the spinboxes with what was actually computed.
        if self._is_synthetic_data_persisted():
            persisted = self._get_persisted_synthetic_params()
            if persisted is not None:
                amp, sx, sy = persisted
                self.amplitudeSpnBx.setValue(amp)
                self.sigmaXSpnBx.setValue(sx)
                self.sigmaYSpnBx.setValue(sy)
            else:
                if 'synthetic_amplitude' in info:
                    self.amplitudeSpnBx.setValue(float(info.get('synthetic_amplitude', self.amplitudeSpnBx.value())))
                if 'synthetic_sigma_x' in info:
                    self.sigmaXSpnBx.setValue(float(info.get('synthetic_sigma_x', self.sigmaXSpnBx.value())))
                if 'synthetic_sigma_y' in info:
                    self.sigmaYSpnBx.setValue(float(info.get('synthetic_sigma_y', self.sigmaYSpnBx.value())))
        if 'freq' in info:
            freq_value = str(info.get('freq', self.freqCB.currentText()))
            idx = self.freqCB.findText(freq_value)
            if idx >= 0:
                self.freqCB.setCurrentIndex(idx)

        if hasattr(self, "bgSubDialog"):
            self.bgSubDialog._populate_loss_params_table()




    def onFoldChkBoxToggled(self):
        if self.quadFold is not None:
            self.deleteImgCache(['avg_fold'])
            self.deleteImgCache(['BgSubFold'])
            self.processImage()


    def closeEvent(self, ev):
        """
        Close the event
        """
        # ✅ Save settings before closing
        if hasattr(self, 'workspace'):
            self.workspace.save_settings()

        # Tear down any in-flight Stop dialog/timer so they don't fire
        # callbacks on a half-destroyed widget.
        if getattr(self, '_stopMsgTimer', None) is not None:
            try:
                self._stopMsgTimer.stop()
            except Exception:
                pass
            self._stopMsgTimer = None
        if getattr(self, '_stopProgress', None) is not None:
            try:
                self._stopProgress.close()
            except Exception:
                pass
            self._stopProgress = None

        # Tear down the multiprocessing pool so child processes don't
        # outlive the GUI. We use wait=False because we don't want the
        # GUI close path to block on long-running optimizations; futures
        # whose work is in-flight will be abandoned.
        if getattr(self, 'processExecutor', None) is not None:
            try:
                self.processExecutor.shutdown(wait=False, cancel_futures=True)
            except Exception:
                pass
            self.processExecutor = None

        self.close()

    def markFixedInfo(self, currentInfo, prevInfo):
        """
        Clean up info dict when changing images
        """
        
        # Clean up detector info if not manually set
        if (not (self.calSettingsDialog and self.calSettingsDialog.manDetector.isChecked())) and (prevInfo is not None):
            if 'detector' in currentInfo:
                del currentInfo['detector']

    def refreshAllTabs(self):
        """
        Set all tab update status to be not update, and Refresh (Redraw) all tab
        """
        self.updated['img'] = False
        self.updated['result'] = False
        self.function = None
        self.display_points = None
        self.updateUI()
        self.resetStatusbar()

    def refreshImageTab(self):
        """
        Refresh (Redraw) image tab
        """
        self.updated['img'] = False
        self.updateUI()

    def refreshResultTab(self):
        """
        Refresh (Redraw) result tab
        """
        self.updated['result'] = False
        self.updateUI()

    def onTabChanged(self, index):
        """
        Handle tab switching by moving the navigation controls to the current tab
        """
        # Remove navControls from its current parent layout
        current_parent = self.navControls.parent()
        if current_parent:
            current_layout = current_parent.layout()
            if current_layout:
                current_layout.removeWidget(self.navControls)
        
        # Add navControls to the appropriate layout based on tab index
        if index == 0:  # Image tab
            # In Image tab, navControls should be in right_panel's bottom area (fixed, always visible)
            self.right_panel.add_bottom_widget(self.navControls)
        elif index == 1:  # Results tab
            # In Results tab, navControls should be in result_right_panel's bottom area
            self.buttonsLayout2.addWidget(self.navControls, 0, 0, 1, 1)
        
        # Trigger UI update
        self.updateUI()

    def updateUI(self):
        """
        Update current all widget in current tab , spinboxes, and refresh status bar
        """
        if self.ableToProcess():
            if self.tabWidget.currentIndex() == 0:
                self.updateImageTab()
            elif self.tabWidget.currentIndex() == 1:
                self.updateResultTab()

            for b in self.checkableButtons:
                b.setChecked(False)


    def updateImageTab(self):
        """
        Display image in image tab, and draw lines
        """
        if self.updated['img']:
            return

        self.uiUpdating = True
        try:
            if self.quadFold is None or self.quadFold.orig_img is None:
                return

            img = self.quadFold.orig_img
            extent = [0,0]
            center = self.quadFold.center
            self.extent = extent

            # Use ImageViewerWidget's API to display image
            # - Automatically preserves zoom (if not first time)
            # - Automatically maintains vmin/vmax/log_scale/colormap from display_panel
            # - Handles invert_yaxis internally
            self.image_viewer.display_image(img)
            
            # Get axes for drawing overlays
            ax = self.imageAxes

            if self.showSeparator.isChecked():
                # Draw quadrant separator
                ax.axvline(center[0], color='y')
                ax.axhline(center[1], color='y')


            o_x, o_y = self.getOrigCoordsCenter(center[0], center[1])
            # self.calSettingsDialog.centerX.setValue(o_x)
            # self.calSettingsDialog.centerY.setValue(o_y)

            if len(self.quadFold.info["ignore_folds"]) > 0:
                # Draw cross line in ignored quadrant
                for fold in self.quadFold.info["ignore_folds"]:
                    if fold == 0:
                        ax.plot([0, center[0]], [center[1], 0], color="w")
                        ax.plot([0, center[0]], [0, center[1]], color="w")
                    if fold == 1:
                        ax.plot([center[0], img.shape[1] - extent[0]], [center[1], 0], color="w")
                        ax.plot([center[0], img.shape[1] - extent[0]], [0, center[1]], color="w")
                    if fold == 2:
                        ax.plot([0, center[0]], [center[1], img.shape[0] - extent[1]], color="w")
                        ax.plot([0, center[0]], [img.shape[0] - extent[1], center[1]], color="w")
                    if fold == 3:
                        ax.plot([center[0], img.shape[1] - extent[0]], [center[1], img.shape[0] - extent[1]], color="w")
                        ax.plot([center[0], img.shape[1] - extent[0]], [img.shape[0] - extent[1], center[1]], color="w")

            # Apply layout and redraw to show overlays
            self.imageFigure.tight_layout()
            self.imageCanvas.draw()

            self.updated['img'] = True
        finally:
            self.uiUpdating = False

    def redrawCenter(self):
        ax = self.imageAxes
        imshape = self.quadFold.curr_dims
        center = list(self.quadFold.center)
        extent = self.extent

        for i in range(len(ax.lines)-1,-1,-1):
            ax.lines[i].remove()

        if self.showSeparator.isChecked():
            # Draw quadrant separator
            ax.axvline(center[0], color='y')
            ax.axhline(center[1], color='y')
        if len(self.quadFold.info["ignore_folds"]) > 0:
            # Draw cross line in ignored quadrant
            for fold in self.quadFold.info["ignore_folds"]:
                if fold == 0:
                    ax.plot([0, center[0]], [center[1], 0], color="w")
                    ax.plot([0, center[0]], [0, center[1]], color="w")
                if fold == 1:
                    ax.plot([center[0], imshape[1] - extent[0]], [center[1], 0], color="w")
                    ax.plot([center[0], imshape[1] - extent[0]], [0, center[1]], color="w")
                if fold == 2:
                    ax.plot([0, center[0]], [center[1], imshape[0] - extent[1]], color="w")
                    ax.plot([0, center[0]], [imshape[0] - extent[1], center[1]], color="w")
                if fold == 3:
                    ax.plot([center[0], imshape[1] - extent[0]], [center[1], imshape[0] - extent[1]], color="w")
                    ax.plot([center[0], imshape[1] - extent[0]], [imshape[0] - extent[1], center[1]], color="w")



    def getExtentAndCenter(self):
        """
        Give the extent and the center of the image
        """
        if self.quadFold is None:
            return [0, 0], (0, 0)

        # If center already exists, return it with zero extent
        if self.quadFold.center is not None:
            return [0, 0], self.quadFold.center
        
        # Otherwise, find the center first
        if self.quadFold.orig_image_center is None:
            self.quadFold.findCenter()
            self.statusPrint("Done.")
        
        # Now center should be set
        if self.quadFold.center is not None:
            center = self.quadFold.center
        else:
            center = self.quadFold.orig_image_center
        
        # Return zero extent and center
        return [0, 0], center

    def _sync_eval_mask_settings_for_preview(self):
        """Sync current mask-related UI values into `quadFold.info` for live preview."""
        if self.quadFold is None:
            return

        info = self.quadFold.info

        rmin_val = int(self.rminSpnBx.value())
        rmax_val = int(self.rmaxSpnBx.value())
        if rmin_val > 0:
            info['rmin'] = rmin_val
        if rmax_val > 0:
            info['rmax'] = rmax_val

        info['equator_mask_height'] = int(self.equatorMaskHeightSpnBx.value())
        info['equator_center_beam_width'] = int(self.equatorCenterBeamSpnBx.value())
        info['m1'] = int(self.m1SpnBx.value())
        info['layer_line_width'] = int(self.layerLineWidthSpnBx.value())

    # NOTE: setCenter() removed - callers now directly use ImageSettingsPanel.set_center_from_source()

    # NOTE: setAngle() removed - callers now directly use ImageSettingsPanel.set_rotation_from_source()

    def updateResultTab(self):
        """
        Display result image in result tab
        """
        if self.updated['result']:
            return

        self.uiUpdating = True
        try:
            img = self.quadFold.imgCache.get('resultImg', None)
            if img is None:
                img = self.quadFold.imgCache.get('resultFolded', None)
                
            if img is None:
                self.uiUpdating = False
                return
            display_mode = "Subtracted"
            if hasattr(self, 'resultDisplayModeCB'):
                display_mode = self.resultDisplayModeCB.currentText()

            try:
                if display_mode == "Background" or display_mode == "Folded":
                    bg_fold = self.quadFold.imgCache.get('BgFold', None)
                    if bg_fold is not None:
                        background = makeFullImage(bg_fold)
                        img = background if display_mode == "Background" else img + background
                        if 'rotate' in self.quadFold.info and self.quadFold.info['rotate']:
                            img = np.rot90(img)

                elif display_mode == "Synthetic Signal" or display_mode == "Synthetic Mask":
                    syn = self.quadFold.imgCache.get('synthetic_data', None)
                    if syn is not None:
                        img = img + syn

            except Exception:
                img = self.quadFold.imgCache['resultImg']

            ## Update Widgets
            self.resultminIntLabel.setText("Min intensity (" + str(round(img.min(), 2)) + ") : ")
            self.resultmaxIntLabel.setText("Max intensity (" + str(round(img.max(), 2)) + ") : ")
            # Range is already set to allow any value at spinbox creation
            self.rminSpnBx.setValue(self.quadFold.info['rmin'])
            self.rmaxSpnBx.setValue(self.quadFold.info['rmax'])

            if 'transition_radius' in self.quadFold.info:
                self.tranRSpnBx.setValue(self.quadFold.info['transition_radius'])
            if 'transition_delta' in self.quadFold.info:
                self.tranDeltaSpnBx.setValue(self.quadFold.info['transition_delta'])

            # convert image for displaying
            # img = getBGR(get8bitImage(img, max=self.spResultmaxInt.value(), min=self.spResultminInt.value()))
            # Get current colormap from image viewer display panel (fallback to gray)
            current_cmap = 'gray'
            if self.image_viewer.display_panel:
                current_cmap = self.image_viewer.display_panel.get_color_map()

            # Use ImageViewerWidget to render the result image
            self.result_viewer.set_display_options(
                vmin=self.spResultminInt.value(),
                vmax=self.spResultmaxInt.value(),
                log_scale=self.resLogScaleIntChkBx.isChecked(),
                colormap=current_cmap
            )
            self.result_viewer.display_image(img)

            ax = self.resultAxes

            show_eval_mask = False
            show_synthetic_mask = False

            if display_mode == "Evaluation Mask":
                show_eval_mask = True

            if display_mode == "Synthetic Mask":
                show_synthetic_mask = True

            if show_eval_mask:
                self._sync_eval_mask_settings_for_preview()
                self.quadFold.createMask()
                mask = self.quadFold.imgCache['mask']
                mask = mask.astype(bool)
                if mask is not None:
                    ax.imshow(mask, cmap="Greys", alpha=0.2, interpolation="nearest")
            
            if display_mode == "Synthetic Mask" and show_synthetic_mask:
                synthetic_mask = self.quadFold.imgCache.get('synthetic_mask', None)
                if synthetic_mask is not None:
                    ax.imshow(synthetic_mask, cmap="Greys", alpha=0.5, interpolation="nearest")


            # Set Zoom in location
            if self.result_zoom is not None and len(self.result_zoom) == 2:
                ax.set_xlim(self.result_zoom[0])
                ax.set_ylim(self.result_zoom[1])
            elif self.default_result_img_zoom is not None and len(self.default_result_img_zoom) == 2:
                ax.set_xlim(self.default_result_img_zoom[0])
                ax.set_ylim(self.default_result_img_zoom[1])
            else:
                ax.set_xlim((0, img.shape[1]))
                ax.set_ylim((0, img.shape[0]))

            self.result_zoom = [ax.get_xlim(), ax.get_ylim()]
            ax.invert_yaxis()
            self.resultFigure.tight_layout()
            self.resultCanvas.draw()

            self.toggleCircleTransition()
            self.toggleCircleRminRmax()

            self.updated['result'] = True
        finally:
            self.uiUpdating = False


    def writeProcessingLog(self):
        """
        Write processing summary and error log to qf_results directory.
        """
        from datetime import datetime
        
        out = self.workspace.dir_context.output_dir if self.workspace.dir_context else self.filePath
        result_path = join(out, 'qf_results')
        os.makedirs(result_path, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Calculate totals
        totalSuccess = self.successCount + self.retrySuccessCount
        totalFailed = self.retryFailCount
        totalProcessed = totalSuccess + totalFailed
        
        # Count save errors
        saveErrorCount = len(self.saveErrors) if hasattr(self, 'saveErrors') else 0
        
        # Write summary log
        summary_file = join(result_path, f'processing_summary_{timestamp}.txt')
        with open(summary_file, 'w', encoding='utf-8') as f:
            f.write(f"Processing Summary\n")
            f.write(f"==================\n")
            f.write(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            f.write(f"Directory: {self.filePath}\n\n")
            f.write(f"Total processed: {totalProcessed}\n")
            f.write(f"Successful (first attempt): {self.successCount}\n")
            f.write(f"Successful (after retry): {self.retrySuccessCount}\n")
            f.write(f"Failed after retry: {totalFailed}\n")
            if saveErrorCount > 0:
                f.write(f"Save errors (processed but failed to save): {saveErrorCount}\n")
        
        print(f"Processing summary saved to: {summary_file}")
        
        # Write error log if there are failures or save errors
        if self.failedTaskErrors or (hasattr(self, 'saveErrors') and self.saveErrors):
            error_file = join(result_path, f'error_log_{timestamp}.txt')
            with open(error_file, 'w', encoding='utf-8') as f:
                f.write(f"Error Log\n")
                f.write(f"=========\n")
                f.write(f"Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
                f.write(f"Directory: {self.filePath}\n")
                f.write(f"Total processing failed: {len(self.failedTaskErrors)}\n")
                f.write(f"Total save failed: {saveErrorCount}\n\n")
                
                # Processing errors (failed after retry)
                if self.failedTaskErrors:
                    f.write(f"{'#'*60}\n")
                    f.write(f"# PROCESSING ERRORS (failed after retry)\n")
                    f.write(f"{'#'*60}\n\n")
                    
                    for filename, retry_error in self.failedTaskErrors.items():
                        f.write(f"{'='*60}\n")
                        f.write(f"File: {filename}\n")
                        f.write(f"{'='*60}\n\n")
                        
                        # Write first attempt error
                        first_error = self.firstAttemptErrors.get(filename, "N/A")
                        f.write(f"--- First Attempt Error ---\n")
                        f.write(f"{first_error}\n\n")
                        
                        # Write retry error
                        f.write(f"--- Retry Attempt Error ---\n")
                        f.write(f"{retry_error}\n\n")
                
                # Save errors (processed successfully but failed to save)
                if hasattr(self, 'saveErrors') and self.saveErrors:
                    f.write(f"{'#'*60}\n")
                    f.write(f"# SAVE ERRORS (processed but failed to save)\n")
                    f.write(f"{'#'*60}\n\n")
                    
                    for filename, save_error in self.saveErrors.items():
                        f.write(f"{'='*60}\n")
                        f.write(f"File: {filename}\n")
                        f.write(f"{'='*60}\n")
                        f.write(f"{save_error}\n\n")
            
            print(f"Error log saved to: {error_file}")

    def showProcessingFinishedMessage(self):
        msgBox = QMessageBox()
        msgBox.setWindowTitle("Processing Complete")
        
        # Calculate totals
        totalSuccess = self.successCount + self.retrySuccessCount
        totalFailed = self.retryFailCount
        totalProcessed = totalSuccess + totalFailed
        saveErrorCount = len(self.saveErrors) if hasattr(self, 'saveErrors') else 0
        
        # Build summary text
        summaryText = f"Folder finished processing\n\n"
        summaryText += f"Total processed: {totalProcessed}\n"
        summaryText += f"Successful: {totalSuccess}"
        
        if self.retrySuccessCount > 0:
            summaryText += f" (including {self.retrySuccessCount} after retry)"
        summaryText += "\n"
        
        if totalFailed > 0:
            summaryText += f"Failed after retry: {totalFailed}\n"
        
        if saveErrorCount > 0:
            summaryText += f"Save errors: {saveErrorCount}\n"
        
        # Set icon based on errors
        if totalFailed > 0 or saveErrorCount > 0:
            msgBox.setIcon(QMessageBox.Warning)
        else:
            msgBox.setIcon(QMessageBox.Information)
        
        msgBox.setText(summaryText)
        
        # Show failed file details if any
        detailText = ""
        if self.failedTaskErrors:
            detailText = "Processing failed files:\n"
            for filename, error in self.failedTaskErrors.items():
                # Show only first line of error for brevity
                error_brief = error.split('\n')[-2] if '\n' in error else error[:100]
                detailText += f"  - {filename}: {error_brief}\n"
        
        if hasattr(self, 'saveErrors') and self.saveErrors:
            detailText += "\nSave failed files:\n"
            for filename, error in self.saveErrors.items():
                error_brief = error.split('\n')[-2] if '\n' in error else error[:100]
                detailText += f"  - {filename}: {error_brief}\n"
        
        if detailText:
            msgBox.setDetailedText(detailText)
        
        msgBox.setInformativeText("Do you want to exit the application or just close this message?")

        # Add buttons
        exitButton = msgBox.addButton("Exit", QMessageBox.ActionRole)
        closeButton = msgBox.addButton("Close", QMessageBox.ActionRole)

        msgBox.exec_()

        # Check which button was clicked
        if msgBox.clickedButton() == exitButton:
            sys.exit(0)  # Closes the entire application
        elif msgBox.clickedButton() == closeButton:
            # Just close the popup - do nothing more
            pass

    def processImage(self):
        """
        Process Image by getting all flags and call process() of QuadrantFolder object
        Then, write data and update UI.

        QuadrantFolder.process() returns True when it actually ran the
        full pipeline and False when it took the fast-path (cached
        _folded.tif still valid). On the fast-path we still update the
        UI / CSV / save user variants, but we skip background-image
        regeneration since BgSubFold / avg_fold weren't reconstructed.
        """
        if not self.ableToProcess():
            return

        flags = self.getFlags()
        self._singleProcessing = True

        self._singleWorker = SingleImageWorker(self.quadFold, flags)
        self._singleWorker.signals.result.connect(self._on_single_processing_result)
        self._singleWorker.signals.error.connect(self._on_single_processing_error)
        self._singleWorker.signals.finished.connect(self._on_single_processing_finished)
        self.threadPool.start(self._singleWorker)


    @Slot(object)
    def _on_single_processing_result(self, quad_fold):
        print("Single processing result received")
        # Mark single-image processing complete before UI refresh so updateUI() is not blocked.
        self._singleProcessing = False
        self.quadFold = quad_fold
        self._register_persisted_baseline_from_processed_info(quad_fold.info)
        self._register_persisted_synthetic_from_processed_info(quad_fold.info)
        if quad_fold.info.get('stopped'):
            return

        persist_eval_ok = (
            hasattr(self, "persistEvaluationBaselineChkBx")
            and self.persistEvaluationBaselineChkBx is not None
            and not self.persistEvaluationBaselineChkBx.isChecked()
        )
        if persist_eval_ok:
            eb = quad_fold.info.get('evaluation_baseline')
            try:
                if eb is not None:
                    fv = float(eb)
                    if fv > 0:
                        self.uiUpdating = True
                        try:
                            self.evaluationBaselineSpnBx.setValue(max(qf_defaults.MIN_EVAL_BASELINE, fv))
                        finally:
                            self.uiUpdating = False
            except (TypeError, ValueError):
                pass

        self._process_count_for_current_image += 1
        if self._process_count_for_current_image == 1 and "avg_fold" in quad_fold.imgCache:
            self._seed_synthetic_spinboxes_from_ensure_defaults()

        self.updateParams()
        self.refreshAllTabs()
        if getattr(self, '_addDefaultOptimizationConfig', False) and self.quadFold is not None:
            self.quadFold.info.setdefault('result_bg', {})
            self.quadFold.info['result_bg']['selected_configuration_name'] = "Default Optimization"
        if self.csvManager is not None:
            self.csvManager.writeNewData(self.quadFold)
        self._upsert_background_metrics_csv(quad_fold=self.quadFold)

        self.toggleCircleRminRmax()
        print("UI updated with new processing result")
        self.resultDisplayModeCB.setEnabled(True)

        # Update settings panel display (shows current center and rotation)
        # This is needed after processing to reflect auto-calculated values
        if self.current_image_data:
            self.workspace.update_display(self.current_image_data)

            # Update center display with TRANSFORMED coordinates (from QuadrantFolder)
            # After transformImage(), center moves to (w//2, h//2)
            if self.quadFold:
                self.workspace._center_widget.update_current_center(
                    self.quadFold.center  # Transformed coordinates
                )

        # Slow vs fast path is stashed by SingleImageWorker._do_work().
        # Default to True so older call sites stay safe.
        ran_slow_path = bool(getattr(self.quadFold, "_last_process_was_slow_path", True))
        self.saveResults(full_process=ran_slow_path)
        print("Single processing complete")

        if self._restoreOptimizeCheckboxAfterProcess:
            self._restoreOptimizeCheckboxAfterProcess = False
            self.optimizeFlag = False


    @Slot(object, object)
    def _on_single_processing_error(self, error_payload, _params):
        if self._restoreOptimizeCheckboxAfterProcess:
            self._restoreOptimizeCheckboxAfterProcess = False
            self.optimizeFlag = False

        image_name = ''
        tb = ''
        error_str = ''
        if isinstance(error_payload, dict):
            image_name = str(error_payload.get('image_name', '') or '')
            tb = str(error_payload.get('traceback', '') or '')
            error_str = str(error_payload.get('error', '') or '')
        else:
            tb = str(error_payload)
            error_str = tb

        errMsg = QMessageBox(self)
        errMsg.setStandardButtons(QMessageBox.Ok)
        errMsg.setFixedWidth(500)

        if error_str.startswith('Image has no valid signal'):
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.setText('Invalid image')
            errMsg.setInformativeText(
                f"Image : {image_name}\n\n{error_str}"
            )
        else:
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.setText('Unexpected error')
            msg = 'Please report the problem with error message below and the input image\n\n'
            msg += "Image : " + image_name
            msg += "\n\n" + tb
            errMsg.setInformativeText(msg)

        errMsg.exec_()

    @Slot()
    def _on_single_processing_finished(self):
        self._singleProcessing = False
        self._singleWorker = None
        # self._force_no_fast_path_on_process = False

        if self._OptimizationRunning:
            self._set_optimization_button_running(False)
            if getattr(self, '_addDefaultOptimizationConfig', False):
                print("Adding default optimization configuration to background subtraction dialog")
                self.bgSubDialog.addBackgroundConfiguration(name="Default Optimization")
                self._addDefaultOptimizationConfig = False
            self._sync_bg_widgets_from_info()
        if self._stopOptimizationRequested:
            self._stopOptimizationRequested = False
            self.stop_process = False
            self.processImage()

    def _set_optimization_button_running(self, running: bool):
        format_running = "QPushButton { color: #7a1f1f; background-color: #f8d7da; }"
        format_stopped = "QPushButton { color: #ededed; background-color: #2986cc;}"
        self._OptimizationRunning = running
        if running:
            self.applyResultBGButton.setText("Stop Optimization")
            self.applyResultBGButton.setStyleSheet(format_running)
            if hasattr(self, "applyBGButton"):
                self.applyBGButton.setText("Stop Optimization")
                self.applyBGButton.setStyleSheet(format_running)
        else:
            self.applyResultBGButton.setText("Apply Default Optimization")
            self.applyResultBGButton.setStyleSheet(format_stopped)
            if hasattr(self, "applyBGButton"):
                self.applyBGButton.setText("Apply Selected Subtraction Settings")
                self.applyBGButton.setStyleSheet(format_stopped)


    def initProcessExecutor(self):
        """Initialize persistent process pool for parallel batch processing.

        Mirrors EquatorWindow.initProcessExecutor(): a single
        ProcessPoolExecutor is reused across batches so we don't pay
        spawn/import overhead on every Process Folder run.
        """
        from concurrent.futures import ProcessPoolExecutor
        from ..headless.mp_executor import init_worker
        import multiprocessing as _mp

        worker_count = int(os.environ.get(
            'MUSCLEX_WORKERS', max(1, (os.cpu_count() or 2) - 2)
        ))

        try:
            # Use 'spawn' context to avoid Qt fork-safety issues.
            # 'fork' (Linux default) copies Qt's mutexes/file-descriptors into
            # the child, causing workers to crash immediately.
            mp_ctx = _mp.get_context('spawn')
            self.processExecutor = ProcessPoolExecutor(
                max_workers=worker_count,
                initializer=init_worker,
                mp_context=mp_ctx,
            )
            print(f"QF: initialized process pool with {worker_count} workers (spawn)")
        except Exception as e:
            print(f"QF: failed to create process pool: {e}")
            print("  Batch processing will run in the main process (single worker)")
            self.processExecutor = None

    def addTask(self, i):
        """Submit one image (by index) to the batch process pool.
        Builds a plain-Python ``job_args`` dict from current GUI state so
        the worker process can fully reconstruct an ``ImageData`` +
        ``QuadrantFolder`` without any Qt / parent references.
        """
        if self.processExecutor is None:
            self.initProcessExecutor()

        if self.processExecutor is None:
            # Pool unavailable — fall back to in-process processing so we
            # at least keep going (worker count = 1).
            self._processOneInProcess(i)
            return

        job_args = self._build_qf_job_args(i)
        if job_args is None:
            return

        from ..headless.mp_executor import process_one_qf_image
        future = self.processExecutor.submit(process_one_qf_image, job_args)
        self.taskManager.submit_task(job_args['filename'], i, future)
        # Track job_args so retry phase can resubmit without re-reading widgets.
        self.pendingJobArgs[i] = job_args
        future.add_done_callback(self._onFutureDone)

    def _build_qf_job_args(self, job_index, override_flags=None):
        """Snapshot all picklable state needed to process one image headlessly.

        Run only on the main thread; the returned dict is fully self-contained
        and will be pickled across to the worker process.
        """
        if not self.file_manager or job_index < 0 or job_index >= len(self.file_manager.names):
            return None

        filename = self.file_manager.names[job_index]
        spec = self.file_manager.specs[job_index]

        # Snapshot flags. Use deepcopy so any subsequent UI edits while the
        # batch is running don't sneak into already-submitted jobs.
        flags = copy.deepcopy(override_flags if override_flags is not None else self.getFlags())

        # Manual center/rotation (per-image, from settings JSON via panel).
        manual_center, manual_rotation = (None, None)
        if self.workspace is not None:
            try:
                manual_center, manual_rotation = self.workspace.get_manual_settings(filename)
            except Exception:
                manual_center, manual_rotation = (None, None)

        blank_mask_config = {'apply_blank': False, 'apply_mask': False, 'blank_weight': 1.0}
        orientation_model = 0
        inpaint = False
        detector = None
        if self.workspace is not None:
            try:
                blank_mask_config = self.workspace.get_blank_mask_config()
            except Exception:
                pass
            orientation_model = getattr(self.workspace, '_orientation_model', 0) or 0
            inpaint = getattr(self.workspace, 'inpaint_enabled', False)
        if self.calSettings is not None and 'detector' in self.calSettings:
            detector = self.calSettings['detector']

        out = (self.workspace.dir_context.output_dir
               if self.workspace and self.workspace.dir_context
               else self.file_manager.dir_path)

        compress_folded = True
        if hasattr(self, 'compressFoldedImageChkBx'):
            try:
                compress_folded = bool(self.compressFoldedImageChkBx.isChecked())
            except Exception:
                pass

        return {
            'dir_path': self.file_manager.dir_path,
            'filename': filename,
            'spec': spec,
            'flags': flags,
            'bgsub': self.bgChoiceIn.currentText() if hasattr(self, 'bgChoiceIn') else 'None',
            'output_dir': out,
            'manual_center': tuple(manual_center) if manual_center is not None else None,
            'manual_rotation': manual_rotation,
            'apply_blank': bool(blank_mask_config.get('apply_blank', False)),
            'apply_mask': bool(blank_mask_config.get('apply_mask', False)),
            'blank_weight': float(blank_mask_config.get('blank_weight', 1.0)),
            'inpaint': bool(inpaint),
            'orientation_model': int(orientation_model),
            'detector': detector,
            'compress_folded': compress_folded,
            'job_index': int(job_index),
        }

    def _processOneInProcess(self, job_index):
        """Fallback: run one image's batch pipeline in the GUI process.

        Only used when ``ProcessPoolExecutor`` could not be created
        (sandbox / fork-restricted environments). Mirrors the worker
        return shape and routes through the same completion handler so
        batch accounting (success/retry/UI progress) stays identical.
        """
        from ..headless.mp_executor import process_one_qf_image
        from concurrent.futures import Future

        job_args = self._build_qf_job_args(job_index)
        if job_args is None:
            return

        self.pendingJobArgs[job_index] = job_args
        future = Future()
        future.set_running_or_notify_cancel()
        self.taskManager.submit_task(job_args['filename'], job_index, future)
        try:
            result = process_one_qf_image(job_args)
            future.set_result(result)
        except Exception as e:
            future.set_exception(e)
        self._onFutureDone(future)

    def _onFutureDone(self, future):
        """Process-pool callback. Bounces work back to the main thread.

        ``concurrent.futures`` invokes done-callbacks from a background
        thread inside the executor manager; touching Qt widgets there is
        unsafe. Route through QTimer.singleShot(0, ...) so the rest of
        the completion path runs on the GUI thread (same pattern as
        EquatorWindow._onFutureDone).
        """
        QTimer.singleShot(0, self, lambda: self.onImageProcessed(future))

    def onImageProcessed(self, future):
        """Main-thread handler for one completed batch image.

        Mirrors EquatorWindow.onImageProcessed:
            1. Drain the future / capture errors
            2. Hand the result to taskManager
            3. Persist results via the main thread (CSV, tasks_done.txt,
               background metrics, aggregated bg dict)
            4. Update progress bar / status
            5. When the batch is fully accounted for, kick off retry or
               run the finalize path (onBatchComplete).
        """
        try:
            try:
                result = future.result()
                error = result.get('error') if isinstance(result, dict) else None
            except Exception as fut_exc:
                # Worker process died abruptly (BrokenProcessPool, segfault, ...).
                error = traceback.format_exc()
                result = {'filename': None, 'info': None, 'error': error}

            task = self.taskManager.complete_task(future, result, error)
            if not task:
                return

            filename = task.filename or (result.get('filename') if isinstance(result, dict) else None)

            if error:
                self._record_task_error(task.job_index, filename, error)
            else:
                self._record_task_success(task, result)

            stats = self.taskManager.get_statistics()

            # During user-initiated Stop, the stop dialog owns the UX and
            # waits for real running workers to wind down. Cancelled queued
            # futures can complete in a burst; do not let that burst jump the
            # main progress bar or trigger early finalization.
            if getattr(self, '_stop_initiated', False):
                return

            # Progress bar update: use taskManager as the single source of
            # truth instead of maintaining a second tasksDone counter.
            if self.progressBar.maximum() != 100:
                self.progressBar.setRange(0, 100)
                self.progressBar.setFormat("Retrying: %p%" if self.isRetryPhase else "%p%")
            if stats['total'] > 0:
                done = stats['completed'] + stats['failed']
                self.progressBar.setValue(int(100.0 * done / stats['total']))

            # All accounted for? Kick off retry phase or finalize.
            if (stats['pending'] == 0 and
                    stats['completed'] + stats['failed'] == stats['total']):
                self._on_batch_phase_complete()
        except Exception as cb_exc:
            print(f"QF onImageProcessed callback error: {cb_exc}")
            traceback.print_exc()

    def _record_task_success(self, task, result):
        """Persist a successful worker result on the main thread.

        Worker has already written the folded.tif + bg.tif + qf_cache to
        disk. Main thread is responsible for the things that need GUI
        state or that need to be serialized across the batch:
        ``tasks_done.txt``, the CSV summary, background metrics CSV,
        ``bgAsyncDict``, and the retry/stats counters.
        """
        filename = task.filename
        info = result.get('info') if isinstance(result, dict) else None
        self._register_persisted_baseline_from_processed_info(info)
        center = result.get('center') if isinstance(result, dict) else None
        rotation = result.get('rotation') if isinstance(result, dict) else None
        has_result = bool(result.get('has_result')) if isinstance(result, dict) else False
        bg_sum = result.get('bg_sum') if isinstance(result, dict) else None
        processing_flags = result.get('processing_flags') if isinstance(result, dict) else None

        out = (self.workspace.dir_context.output_dir
               if self.workspace and self.workspace.dir_context
               else self.filePath)

        try:
            os.makedirs(join(out, 'qf_results'), exist_ok=True)
            with open(join(out, 'qf_results', 'tasks_done.txt'), 'a') as fh:
                fh.write(filename + ' saving image\n')
        except (OSError, IOError) as e:
            print(f"Warning: Failed to write to tasks_done.txt for {filename}: {e}")

        # CSV write needs a quadFold-like object. Build a lightweight
        # shim mirroring the fields csvManager.writeNewData() reads.
        try:
            if info is not None and self.csvManager is not None:
                self.csvManager.writeNewData(_BatchQuadFoldProxy(filename, info, center, rotation, has_result, processing_flags))
        except Exception as e:
            print(f"Failed to write CSV for {filename}: {e}")

        try:
            if info is not None:
                self._upsert_background_metrics_csv(
                    quad_fold=_BatchQuadFoldProxy(filename, info, center, rotation, has_result)
                )
        except Exception as e:
            print(f"Failed to upsert background metrics for {filename}: {e}")

        if bg_sum is not None:
            self.bgAsyncDict[filename] = bg_sum

        # Drop the snapshot — we no longer need to retry this job.
        self.pendingJobArgs.pop(task.job_index, None)

        if self.isRetryPhase:
            self.retrySuccessCount += 1
        else:
            self.successCount += 1

    def _record_task_error(self, job_index, filename, error_str):
        """Track a failed worker invocation for retry / final reporting.

        When the user has pressed Stop (``_stop_initiated``), we treat
        every error as final and do *not* queue it for retry — the
        whole point of Stop is to wind down, not to keep grinding.
        """
        filename = filename or f"<job_{job_index}>"

        if getattr(self, '_stop_initiated', False):
            # User-initiated stop: cancelled/failed tasks are final.
            self.failedTaskErrors[filename] = error_str
            self.pendingJobArgs.pop(job_index, None)
            print(f"Task cancelled during stop: {filename}")
            return

        if self.isRetryPhase:
            self.retryFailCount += 1
            self.failedTaskErrors[filename] = error_str
            self.pendingJobArgs.pop(job_index, None)
            print(f"Task failed after retry: {filename}")
        else:
            self.firstAttemptErrors[filename] = error_str
            # Queue the same job_args for resubmission in the retry phase.
            job_args = self.pendingJobArgs.get(job_index)
            if job_args is not None:
                self.retryJobArgs.append(job_args)
            print(f"Task failed, queued for retry: {filename}")

    def _on_batch_phase_complete(self):
        """Either start the retry phase, or run the final batch cleanup."""
        if (not self.isRetryPhase
                and self.retryJobArgs
                and not getattr(self, '_stop_initiated', False)):
            retry_count = len(self.retryJobArgs)
            print(f"Starting retry phase with {retry_count} failed tasks")
            self.isRetryPhase = True
            self.totalFiles = retry_count
            self.tasksDone = 0
            # Retry is a new accounting phase. Success/failure counters above
            # already preserve first-attempt results, so reset taskManager to
            # make retry progress/statistics reflect only retry submissions.
            self.taskManager.clear()
            self.progressBar.setRange(0, 100)
            self.progressBar.setValue(0)
            self.progressBar.setFormat("Retrying: %p%")

            to_submit, self.retryJobArgs = self.retryJobArgs, []
            from ..headless.mp_executor import process_one_qf_image
            for job_args in to_submit:
                if self.stop_process or self.processExecutor is None:
                    self._processOneInProcess(job_args['job_index'])
                    continue
                future = self.processExecutor.submit(process_one_qf_image, job_args)
                self.taskManager.submit_task(job_args['filename'], job_args['job_index'], future)
                self.pendingJobArgs[job_args['job_index']] = job_args
                future.add_done_callback(self._onFutureDone)
            return

        self._finalize_batch()

    def _finalize_batch(self):
        """Wrap up a batch: aggregated CSVs, button state, summary popup.

        Idempotent: if ``batchProcessing`` is already False (e.g. a stale
        callback fires after the polling-stop dialog already finalized
        the batch) this method short-circuits without double-writing
        artefacts or double-popping a completion dialog.
        """
        if not getattr(self, 'batchProcessing', False):
            return
        print("All batch tasks complete")
        self.batchProcessing = False
        self._force_recalc_bg_for_batch = False

        # Shut down the process pool now that nothing else will be
        # submitted. We use wait=False because, at this point, every
        # worker has already finished (either normally or via the
        # stop-poll which only releases this path when running_count==0).
        if self.processExecutor is not None:
            try:
                self.processExecutor.shutdown(wait=False, cancel_futures=False)
            except Exception:
                pass
            self.processExecutor = None

        if hasattr(self, "bgSubDialog"):
            try:
                self.bgSubDialog._load_background_configurations_from_cache()
            except Exception:
                pass

        self.progressBar.setVisible(False)
        try:
            self.navControls.filenameLineEdit.setEnabled(True)
        except Exception:
            pass

        try:
            self.navControls.processFolderButton.toggled.disconnect(self.batchProcBtnToggled)
            self.navControls.processFolderButton.setChecked(False)
            self.navControls.processFolderButton.setEnabled(True)
            self.navControls.processFolderButton.setText("Process Current Folder")
            self.navControls.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        except Exception:
            pass
        try:
            self.navControls.processH5Button.setChecked(False)
            self.navControls.processH5Button.setEnabled(True)
            self.navControls.processH5Button.setText("Process Current H5 File")
        except Exception:
            pass

        try:
            if self.csvManager is not None:
                self.csvManager.sortCSV()
        except Exception as e:
            print(f"Failed to sort summary CSV: {e}")

        out = self.workspace.dir_context.output_dir if self.workspace.dir_context else self.filePath
        try:
            os.makedirs(join(out, 'qf_results'), exist_ok=True)
            os.makedirs(join(out, 'qf_results', 'bg'), exist_ok=True)
            with open(join(out, 'qf_results', 'bg', 'background_sum.csv'), 'w', newline='') as csvfile:
                writer = csv.writer(csvfile)
                writer.writerow(['Name', 'Sum'])
                for name, total in self.bgAsyncDict.items():
                    writer.writerow([name, total])
        except Exception as e:
            print(f"Failed to write aggregated background_sum.csv: {e}")

        self.pendingJobArgs.clear()
        self.retryJobArgs = []
        # Clear task manager last — any stale callbacks that fire after
        # this point will find their future missing and return early
        # from onImageProcessed (see complete_task -> None check).
        try:
            self.taskManager.clear()
        except Exception:
            pass

        self.writeProcessingLog()
        # Only show the "Complete" popup on natural completion. For
        # user-initiated Stop we let _updateStopProgress show its own
        # "Stopped" message instead, so the user doesn't get two popups.
        if not getattr(self, '_stop_initiated', False):
            self.showProcessingFinishedMessage()


    def onProcessingFinished(self):

        self.updateParams()
        self.refreshAllTabs()
        self.resetStatusbar2()
        self.csvManager.writeNewData(self.quadFold)
        self.saveResults()

        self.currentTask = None



    def saveResults(self, full_process=True):
        """
        Save the result image to qf_results.

        Filename rules:
          - compress unchecked  -> <name>_folded.tif
          - compress checked    -> <name>_folded_compressed.tif

        Both variants are full-size copies of resultImg and are eligible
        for fast-path reload next session.

        :param full_process: True if the slow-path ran (process() did
            the full pipeline). When False (fast-path), bg.tif from a
            previous session is still on disk and BgSubFold / avg_fold
            weren't reconstructed, so saveBackground is skipped. The
            result tif is also still on disk (that's how we got here),
            but we re-emit it anyway to honor any newly-requested
            variant change.
        """
        print("SAVE RESULTS")
        if 'resultImg' not in self.quadFold.imgCache:
            return

        out = self.workspace.dir_context.output_dir if self.workspace.dir_context else self.filePath
        result_path = fullPath(out, 'qf_results')
        createFolder(result_path)

        base, _ = splitext(str(join(result_path, self.quadFold.img_name)))
        img = self.quadFold.imgCache['resultImg'].astype("float32")

        compress = self.compressFoldedImageChkBx.isChecked()

        try:
            suffix = '_folded_compressed.tif' if compress else '_folded.tif'
            out_file = base + suffix
            if compress:
                Image.fromarray(img).save(out_file, compression='tiff_lzw')
            else:
                fabio.tifimage.tifimage(data=img).write(out_file)
        except Exception as e:
            print("Error saving image", e)
            if self.batchProcessing and hasattr(self, 'saveErrors'):
                import traceback
                self.saveErrors[self.quadFold.img_name] = traceback.format_exc()

        if full_process:
            self.saveBackground()

    def saveBackground(self):
        """
        Save the background in the bg folder.

        Skipped when the upstream process() took the fast-path: in that
        case the BG-sub intermediates (BgSubFold, avg_fold) were never
        reconstructed -- only resultImg was reloaded from
        _folded.tif. The previous session's bg.tif is still on disk
        so nothing is lost. Same defensive pattern as
        FolderImageWorker._save_background.
        """
        result = self.quadFold.imgCache.get("BgSubFold", None)
        avg_fold = self.quadFold.imgCache.get("avg_fold", None)
        if result is None or avg_fold is None:
            return
        info = self.quadFold.info
        background = avg_fold - result
        resultImg = makeFullImage(background)

        if 'rotate' in info and info['rotate']:
            resultImg = np.rot90(resultImg)

        method = info['bgsub']
        print(method)
        if method != 'None':
            
            filename = self.file_manager.current_image_name
            out = self.workspace.dir_context.output_dir if self.workspace.dir_context else self.filePath
            bg_path = fullPath(out, os.path.join("qf_results", "bg"))
            result_path = fullPath(bg_path, filename + ".bg.tif")

            # create bg folder
            createFolder(bg_path)
            resultImg = resultImg.astype("float32")
            fabio.tifimage.tifimage(data=resultImg).write(result_path)

            total_inten = np.sum(resultImg)
            csv_path = join(bg_path, 'background_sum.csv')
            if self.csv_bg is None:
                # create csv file to save total intensity for background
                if exists(csv_path):
                    self.csv_bg = pd.read_csv(csv_path)
                else:
                    self.csv_bg = pd.DataFrame(columns=['Name', 'Sum'])
                self.csv_bg = self.csv_bg.set_index('Name')

            if filename in self.csv_bg.index:
                self.csv_bg = self.csv_bg.drop(index=filename)


            #TEMPORARY!! ONLY COUNT WHAT'S BETWEEN THE RMIN AND RMAX FOR RESULT IMAGE
            ###########################################################################
            csv_yc, csv_xc = background.shape

            csv_h, csv_w = resultImg.shape
            csv_y, csv_x = np.ogrid[:csv_h, :csv_w]

            csv_dists = np.sqrt((csv_x - csv_xc) ** 2 + (csv_y - csv_yc) ** 2)

            if 'rmin' not in self.quadFold.info or self.quadFold.info['rmin'] is None:
                print("Setting Rmin to default: 0")
                self.quadFold.info['rmin'] = 0

            if 'rmax' not in self.quadFold.info or self.quadFold.info['rmax'] is None:
                print("Setting Rmax to default: 100")
                self.quadFold.info['rmax'] = 100

            csv_mask = (csv_dists >= self.quadFold.info['rmin']) & (csv_dists <= self.quadFold.info['rmax'])

            csv_total = np.sum(resultImg[csv_mask])

            self.csv_bg.loc[filename] = pd.Series({'Sum':total_inten})
            self.csv_bg.to_csv(csv_path)

    def _upsert_background_metrics_csv(self, quad_fold=None, flags=None):
        """
        Upsert one row per image into qf_results/bg/background_metrics.csv.
        Row key is image filename (ImageName).
        """
        qf = quad_fold if quad_fold is not None else self.quadFold
        if qf is None or not hasattr(qf, "info"):
            return

        info = qf.info if isinstance(qf.info, dict) else {}
        save_metrics_enabled = bool(
            hasattr(self, "saveMetricsToCsvChkBx")
            and self.saveMetricsToCsvChkBx is not None
            and self.saveMetricsToCsvChkBx.isChecked()
        )
        if not save_metrics_enabled:
            return

        result_bg = info.get("result_bg", {}) or {}
        raw_metrics = result_bg.get("metrics_raw", {}) or {}
        norm_metrics = result_bg.get("metrics_normalized", {}) or {}
        loss = result_bg.get("loss", None)
        if not isinstance(raw_metrics, dict):
            raw_metrics = {}
        if not isinstance(norm_metrics, dict):
            norm_metrics = {}
        if len(raw_metrics) == 0 and len(norm_metrics) == 0 and loss is None:
            return

        filename = str(getattr(qf, "img_name", "") or "")
        method = result_bg.get("method", None)
        final_params = result_bg.get("final_params", None)
        params_text = self._format_bg_params_text(final_params)
        metric_weights = info.get("metric_weights", {})

        mean_metric_values = info.get("mean_metric_values", {})
        # Equator metrics are produced by evaluateResult() under result_bg.
        # Keep top-level info fallback for compatibility with older caches/sessions.
        equator_raw_metrics = result_bg.get("metrics_equator_raw", info.get("metrics_equator_raw", {})) or {}
        equator_norm_metrics = result_bg.get("metrics_equator_normalized", info.get("metrics_equator_normalized", {})) or {}

        row_data = {
            "Method": str(method),
            "BgParameters": params_text,
            "Loss": loss,
            "Raw_MSE": raw_metrics.get("MSE", None),
            "Raw_Share_Neg_Synthetic": raw_metrics.get("Share_Neg_Synthetic", None),
            "Raw_Share_Non_Baseline": raw_metrics.get("Share_Non_Baseline", None),
            "Raw_Share_Neg_Connected": raw_metrics.get("Share_Neg_Connected", None),
            "Raw_Smoothness": raw_metrics.get("Smoothness", None),
            "Norm_MSE": norm_metrics.get("MSE", None),
            "Norm_Share_Neg_Synthetic": norm_metrics.get("Share_Neg_Synthetic", None),
            "Norm_Share_Non_Baseline": norm_metrics.get("Share_Non_Baseline", None),
            "Norm_Share_Neg_Connected": norm_metrics.get("Share_Neg_Connected", None),
            "Norm_Smoothness": norm_metrics.get("Smoothness", None),
            "Weight_MSE": metric_weights.get("MSE", None),
            "Weight_Share_Neg_Synthetic": metric_weights.get("Share_Neg_Synthetic", None),
            "Weight_Share_Non_Baseline": metric_weights.get("Share_Non_Baseline", None),
            "Weight_Share_Neg_Connected": metric_weights.get("Share_Neg_Connected", None),
            "Weight_Smoothness": metric_weights.get("Smoothness", None),
            "Mean_MSE_SYN": mean_metric_values.get("MSE_SYN_MEAN", None),
            "Mean_SHARE_NEG_SYN": mean_metric_values.get("SHARE_NEG_SYN_MEAN", None),
            "Mean_SHARE_NON_BASELINE": mean_metric_values.get("SHARE_NON_BASELINE_MEAN", None),
            "Mean_SHARE_NEG_CON": mean_metric_values.get("SHARE_NEG_CON_MEAN", None),
            "Mean_SMOOTH": mean_metric_values.get("SMOOTH_MEAN", None),
            "Evaluation_Baseline_Value": info.get("evaluation_baseline", None) if isinstance(info, dict) else None,
            "Synthetic_Amplitude_Value": info.get("synthetic_amplitude", None) if isinstance(info, dict) else None,
            "Synthetic_Sigma_X_Value": info.get("synthetic_sigma_x", None) if isinstance(info, dict) else None,
            "Synthetic_Sigma_Y_Value": info.get("synthetic_sigma_y", None) if isinstance(info, dict) else None,
            "Equator_Raw_MSE": equator_raw_metrics.get("MSE", None),
            "Equator_Raw_Share_Neg_Synthetic": equator_raw_metrics.get("Share_Neg_Synthetic", None),
            "Equator_Raw_Share_Non_Baseline": equator_raw_metrics.get("Share_Non_Baseline", None),
            "Equator_Raw_Share_Neg_Connected": equator_raw_metrics.get("Share_Neg_Connected", None),
            "Equator_Raw_Smoothness": equator_raw_metrics.get("Smoothness", None),
            "Equator_Norm_MSE": equator_norm_metrics.get("MSE", None),
            "Equator_Norm_Share_Neg_Synthetic": equator_norm_metrics.get("Share_Neg_Synthetic", None),
            "Equator_Norm_Share_Non_Baseline": equator_norm_metrics.get("Share_Non_Baseline", None),
            "Equator_Norm_Share_Neg_Connected": equator_norm_metrics.get("Share_Neg_Connected", None),
            "Equator_Norm_Smoothness": equator_norm_metrics.get("Smoothness", None),
        }
        ordered_columns = list(row_data.keys())

        try:
            csv_path = join(self.filePath, "qf_results", "bg", "background_metrics.csv")
            os.makedirs(os.path.dirname(csv_path), exist_ok=True)

            if exists(csv_path):
                df = pd.read_csv(csv_path)
            else:
                df = pd.DataFrame(columns=["ImageName"] + ordered_columns)

            if "ImageName" in df.columns:
                df = df.set_index("ImageName")
            else:
                df.index.name = "ImageName"

            for col in ordered_columns:
                if col not in df.columns:
                    df[col] = np.nan
            df = df.reindex(columns=ordered_columns)
            df.loc[filename] = pd.Series(row_data)
            df.sort_index(inplace=True)
            df.to_csv(csv_path, index_label="ImageName")
        except Exception as e:
            print(f"Failed to upsert background metrics CSV for {filename}: {e}")

    def updateParams(self):
        """
        Update the parameters
        """
        try:
            info = self.quadFold.info
            if 'orientation_model' in info:
                # Update Panel's orientation model from cached info
                self.workspace._orientation_model = info['orientation_model']
        except:
            print("EXCEPTION IN UPDATE PARAMS")
        else:
            print("UPDATE PARAMS SUCCESS")

        self._update_bg_method_summary()
        self.bgSubDialog._update_bg_metrics_table()
        

    def _sync_bg_widgets_from_info(self):
        """
        Sync background-subtraction widgets (spinboxes/combos) from `self.quadFold.info`.

        This is needed after automated optimization: the optimized params are written into
        `quadFold.info` by `QuadrantFolder`, but the UI spinboxes won't update unless we
        explicitly propagate the new values.
        """
        if not hasattr(self, "quadFold") or self.quadFold is None:
            return
        info = getattr(self.quadFold, "info", None)
        if not isinstance(info, dict):
            return

        def _set_widget_value(widget, value, proxy_key=None):
            for target in (widget, self._manual_proxy.get(proxy_key) if hasattr(self, "_manual_proxy") and proxy_key else None):
                if target is None:
                    continue
                try:
                    target.blockSignals(True)
                    target.setValue(value)
                finally:
                    target.blockSignals(False)

        mapping = [
            ("tophat", "tophatSpnBx", int),
            ("cirmax", "maxPixRange", float),
            ("cirmin", "minPixRange", float),
            ("radial_bin", "radialBinSpnBx", int),
            ("smooth", "smoothSpnBx", float),
            ("tension", "tensionSpnBx", float),
            ("rmin", "rminSpnBx", int),
            ("rmax", "rmaxSpnBx", int),
            ("win_size_x", "winSizeX", int),
            ("win_size_y", "winSizeY", int),
            ("win_sep_x", "winSepX", int),
            ("win_sep_y", "winSepY", int),
            ("fwhm", "gaussFWHM", int),
            ("boxcar_x", "boxcarX", int),
            ("boxcar_y", "boxcarY", int),
            ("cycles", "cycle", int),
        ]

        for key, widget_attr, caster in mapping:
            if key not in info or not hasattr(self, widget_attr):
                continue
            widget = getattr(self, widget_attr, None)
            if widget is None:
                continue
            try:
                val = caster(info.get(key))
            except Exception:
                continue
            _set_widget_value(widget, val, widget_attr)

    def resetStatusbar(self):
        """
        Reset the status bar
        
        Note: File path display is now automatically handled by BaseGUI
        through ImageNavigatorWidget.filePathTextReady signal.
        """
        # File path update moved to BaseGUI._setup_file_path_updates()
        # The navigator automatically emits filePathTextReady when images change
        pass
        
    def resetStatusbar2(self):
        """
        Reset the status bar, but search using self.quadFold.info
        
        Note: filenameLineEdit is automatically updated by ImageNavigatorWidget,
        no need to update it here.
        """
        
        index = self.file_manager.names.index(self.quadFold.img_name)
        #DOES NOT GET HERE
        fileFullPath = fullPath(self.filePath, self.file_manager.names[index])
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(index + 1) + '/' + str(len(self.file_manager.names)) + ') : ' + fileFullPath)

    # NOTE: _on_scan_complete is no longer needed
    # Background scanning is now handled internally by ImageNavigatorWidget

    def getFlags(self):
        """
        Get all flags for QuadrantFolder process() from widgets
        :return: flags (dict)
        """
        flags = {}

        # image
        flags['orientation_model'] = self.workspace._orientation_model
        flags["ignore_folds"] = self.ignoreFolds

        sm = self.workspace.settings_manager
        flags['blank_mask'] = sm.blank_enabled
        flags['apply_mask'] = sm.mask_enabled
        
        flags['fold_image'] = self.toggleFoldImage.isChecked()

        # ===== Background removal flags =====
        flags['bgsub'] = self.bgChoiceIn.currentText()
        flags["cirmin"] = self.minPixRange.value()
        flags["cirmax"] = self.maxPixRange.value()
        flags['win_size_x'] = self.winSizeX.value()
        flags['win_size_y'] = self.winSizeY.value()
        flags['win_sep_x'] = self.winSepX.value()
        flags['win_sep_y'] = self.winSepY.value()
        # NOTE: bin_theta / bin_theta_out widgets (thetabinCB / thetaBinOutCB)
        # exist for UI compatibility but their values are not consumed by
        # QuadrantFolder.process(); deliberately not written into flags.
        flags['radial_bin'] = self.radialBinSpnBx.value()
        flags['smooth'] = self.smoothSpnBx.value()
        flags['tension'] = self.tensionSpnBx.value()
        flags["tophat"] = self.tophatSpnBx.value()
        flags['fwhm'] = self.gaussFWHM.value()
        flags['boxcar_x'] = self.boxcarX.value()
        flags['boxcar_y'] = self.boxcarY.value()
        flags['cycles'] = self.cycle.value()
        flags['degree'] = float(self.degreeCB.currentText())

        # ===== Background removal flags Out =====
        flags['bgsub_out'] = self.bgChoiceOut.currentText()
        flags["cirmin_out"] = self.minPixRangeOut.value()
        flags["cirmax_out"] = self.maxPixRangeOut.value()
        flags['win_size_x_out'] = self.winSizeOutX.value()
        flags['win_size_y_out'] = self.winSizeOutY.value()
        flags['win_sep_x_out'] = self.winSepOutX.value()
        flags['win_sep_y_out'] = self.winSepOutY.value()
        # bin_theta_out intentionally omitted (see note above).
        flags['radial_bin_out'] = self.radialBinOutSpnBx.value()
        flags['smooth_out'] = self.smoothOutSpnBx.value()
        flags['tension_out'] = self.tensionOutSpnBx.value()
        flags["tophat_out"] = self.tophatOutSpnBx.value()
        flags['fwhm_out'] = self.gaussFWHMOut.value()
        flags['boxcar_x_out'] = self.boxcarOutX.value()
        flags['boxcar_y_out'] = self.boxcarOutY.value()
        flags['cycles_out'] = self.cycleOut.value()

        # ===== Transition settings =====
        if hasattr(self, "tranRSpnBx"):
            flags['transition_radius'] = self.tranRSpnBx.value()
        if hasattr(self, "tranDeltaSpnBx"):
            flags['transition_delta'] = self.tranDeltaSpnBx.value()

        # ===== Background optimization flags (automated processing) =====
        optimize_each_image = bool(
            hasattr(self, "optimizeEachImageChkBx") and self.optimizeEachImageChkBx.isChecked()
        )
        flags['optimize_each_image'] = optimize_each_image
        flags['optimize'] = (self.optimizeFlag is True) or (self.batchProcessing and optimize_each_image)
        flags['bg_options'] = self.bgOptionsCB.currentIndex()    
        flags['methods'] = self._get_selected_optimization_methods()
        flags['steps'] = parse_optimization_steps(self.stepsLineEdit.text())
        flags['max_iterations'] = self.maxIterationsSpnBx.value()
        flags['early_stop'] = self.earlyStopSpnBx.value()
        flags['optimize_timeout'] = self.optimizeTimeoutSpnBx.value()
        flags['mean_metric_values'] = {
            'MSE_SYN_MEAN': float(self.meanMSESpnBx.value()),
            'SHARE_NEG_SYN_MEAN': _percent_to_fraction_for_flags(self.meanNegSynSpnBx.value()),
            'SHARE_NON_BASELINE_MEAN': _percent_to_fraction_for_flags(self.meanNonBaselineSpnBx.value()),
            'SHARE_NEG_CON_MEAN': _percent_to_fraction_for_flags(self.meanNegConSpnBx.value()),
            'SMOOTH_MEAN': float(self.meanSmoothSpnBx.value()),
        }
        persist_eval_baseline = bool(
            hasattr(self, "persistEvaluationBaselineChkBx")
            and self.persistEvaluationBaselineChkBx is not None
            and self.persistEvaluationBaselineChkBx.isChecked()
        )
        flags['persist_evaluation_baseline'] = persist_eval_baseline
        # When persistence is OFF the baseline is recomputed per image
        # (QuadrantFolder.updateInfo overwrites info['evaluation_baseline']
        # to 0.0/parent), so the UI spinbox value is meaningless. Emit 0.0
        # to match the headless path and the value actually used.
        if persist_eval_baseline:
            flags['evaluation_baseline'] = max(
                qf_defaults.MIN_EVAL_BASELINE,
                float(self.evaluationBaselineSpnBx.value()),
            )
        else:
            flags['evaluation_baseline'] = 0.0
        # Synthetic Gaussian params: when persistence is OFF we send 0 so
        # QuadrantFolder._ensure_synthetic_gaussian_params recomputes them
        # from the current image; when ON we send either the persisted
        # tuple (registered after a previous image) or the UI values.
        persist_synth = self._is_synthetic_data_persisted()
        flags['persist_synthetic_data'] = persist_synth
        if persist_synth:
            persisted = self._get_persisted_synthetic_params()
            if persisted is not None:
                amp, sx, sy = persisted
            else:
                amp = float(self.amplitudeSpnBx.value())
                sx = float(self.sigmaXSpnBx.value())
                sy = float(self.sigmaYSpnBx.value())
            flags['synthetic_amplitude'] = amp
            flags['synthetic_sigma_x'] = sx
            flags['synthetic_sigma_y'] = sy
        else:
            flags['synthetic_amplitude'] = 0.0
            flags['synthetic_sigma_x'] = 0.0
            flags['synthetic_sigma_y'] = 0.0
        flags['freq'] = str(self.freqCB.currentText())
        flags['metric_weights'] = {
            'MSE': float(self.weightMSESpnBx.value()),
            'Share_Neg_Synthetic': float(self.weightNegSynSpnBx.value()),
            'Share_Non_Baseline': float(self.weightNonBaselineSpnBx.value()),
            'Share_Neg_Connected': float(self.weightNegConSpnBx.value()),
            'Smoothness': float(self.weightSmoothSpnBx.value()),
        }

        # Evaluation mask settings
        flags['equator_mask_height'] = self.equatorMaskHeightSpnBx.value()
        flags['equator_center_beam_width'] = self.equatorCenterBeamSpnBx.value()
        flags['m1'] = self.m1SpnBx.value()
        flags['layer_line_width'] = self.layerLineWidthSpnBx.value()
        flags['smooth_image'] = self.smoothImageChkbx.isChecked()
        flags['downsample'] = int(self.downsampleCB.currentText())
        flags['save_metrics_to_csv'] = bool(
            hasattr(self, "saveMetricsToCsvChkBx")
            and self.saveMetricsToCsvChkBx is not None
            and self.saveMetricsToCsvChkBx.isChecked()
        )
        # Output compression flag. Mirrors what saveSettings() persists so
        # the flag is present in getFlags() (and therefore in summary.csv),
        # matching the headless path which reads it from qfsettings.json.
        flags['compressed'] = bool(
            hasattr(self, "compressFoldedImageChkBx")
            and self.compressFoldedImageChkBx is not None
            and self.compressFoldedImageChkBx.isChecked()
        )

        # Auto-select from saved user background configurations (used mainly in batch processing)
        flags['choose_configurations_auto'] = (
            self.chooseConfigurationsAutoChkBx.isChecked() and not optimize_each_image
        )

        # TODO: add create new configurations for outliers

        bg_configs = self.bgSubDialog.backgroundConfigurations if hasattr(self, "bgSubDialog") else []
        flags['background_configurations'] = bg_configs if isinstance(bg_configs, list) else []

        flags['batch_processing'] = self.batchProcessing
        flags['force_recalc_bg'] = bool(getattr(self, "_force_recalc_bg_for_batch", False))

        if self.batchProcessing and not optimize_each_image:
            resolved_manual_assignments = {}
            if len(flags['background_configurations']) > 0:
                resolved_manual_assignments = self.bgSubDialog._resolve_manual_background_assignments_for_batch(flags['background_configurations'])
            flags['manual_background_assignments'] = copy.deepcopy(resolved_manual_assignments)
        else:
            flags['manual_background_assignments'] = {}
    
        # Apply mode orientation if enabled
        mode_rotation = self.workspace._mode_rotation
        if mode_rotation is not None:
            # Set rotation using Panel's public method
            self.workspace.set_rotation_from_source(
                self.file_manager.current_image_name,
                mode_rotation,
                "ModeAngle"
            )
            flags["mode_angle"] = mode_rotation

        if self.rminSpnBx.value() > 0:
            flags['fixed_rmin'] = self.rminSpnBx.value()

        if self.tranRSpnBx.value() > 0:
            flags['transition_radius'] = self.tranRSpnBx.value()

        if self.tranDeltaSpnBx.value() > 0:
            flags['transition_delta'] = self.tranDeltaSpnBx.value()

        roi_w = self.fixedRoiW.value()
        roi_h = self.fixedRoiH.value()
        if roi_w > 0 and roi_h > 0:
            # Spinbox is the source of truth for ROI. Whether it carries
            # over to the next image is controlled by "Persist ROI size":
            # if unchecked, the spinbox is reset to 0 on image switch
            # (see _update_ui_for_image), so this branch won't fire there.
            flags['roi_w'] = roi_w
            flags['roi_h'] = roi_h

        if self.rmaxSpnBx.value() > 0:
            flags['fixed_rmax'] = self.rmaxSpnBx.value()

        flags['rotate'] = self.rotate90Chkbx.isChecked()

        if self.calSettings is not None and 'detector' in self.calSettings:
            flags['detector'] = self.calSettings['detector']

        # if getattr(self, '_force_no_fast_path_on_process', False):
        #     flags['no_fast_path'] = True

        return flags
    

    def _get_selected_optimization_methods(self):
        """Return selected optimization methods from the list widget."""
        if not hasattr(self, "optimizationMethodsList"):
            return []

        selected = []
        for i in range(self.optimizationMethodsList.count()):
            item = self.optimizationMethodsList.item(i)
            if item is not None and item.isSelected():
                selected.append(item.text())

        if len(selected) == 0 and hasattr(self, "bgSubDialog"):
            return list(qf_defaults.DEFAULT_OPTIMIZATION_METHODS)

        return selected



    # NOTE: onNewFileSelected removed - replaced by _on_folder_loaded and _on_image_changed
    # The new flow:
    #   1. Navigator emits fileLoaded -> _on_folder_loaded (initializes csvManager)
    #   2. Navigator emits imageChanged -> _on_image_changed (processes image)
    # This ensures proper initialization order and separation of concerns.


    def resetWidgets(self):
        """
        Reset the widgets
        """
        self.uiUpdating = True
        self.rminSpnBx.setValue(-1)
        self.rmaxSpnBx.setValue(-1)
        self.uiUpdating = False

    # NOTE: load/save CenterSettings and RotationSettings are now handled by ImageSettingsPanel
    # NOTE: _create_image_data() moved to ImageData.from_settings_panel() factory method

    def _on_browse_file(self):
        """
        Handle file browse button click.
        
        Delegates to Navigator which will:
        1. Open file dialog
        2. Load file
        3. Emit fileLoaded signal -> _on_folder_loaded (init csvManager)
        4. Emit imageChanged signal -> _on_image_changed (process image)
        """
        if self.workspace.navigator.browse_file():
            pass

    # NOTE: _on_browse_folder() removed - folder selection not currently used
    # When needed, should properly handle folder paths vs file paths in FileManager

    def batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.navControls.processFolderButton.isChecked():
            if not self.progressBar.isVisible():
                self.navControls.processFolderButton.setText("Stop")
                self.processFolder()
        else:
            self.clearTasks()
    

    def clearTasks(self):
        """
        Handle user "Stop" click during a batch.

        Mirrors ``EquatorWindow.stopProcess``: cancel queued futures, show
        an indeterminate "Stopping..." progress dialog, then poll the task
        manager's running-count and finalize the batch when every worker
        process has wound down and every completion callback is accounted for.

        Workers that have already started cannot be killed mid-pipeline
        (no Python-level interrupt), but ``cancel_futures=True`` prevents
        any *queued* task from starting. The popup tells the user how
        many are still running so the wait is visible instead of silent.
        """
        # If no batch is running, just reset UI bits and bail out.
        if not getattr(self, 'batchProcessing', False):
            self._reset_batch_ui_only()
            return

        # Mark stop so retry phase / completion popup is skipped.
        self.stop_process = True
        self._stop_initiated = True

        # Discard any retry queue: user wants out, not another attempt.
        self.retryJobArgs = []

        # Switch the main progress bar to busy mode while Stop is winding
        # down. The stop dialog below shows the actual running-task count;
        # the main bar should not jump forward as queued futures cancel.
        try:
            self.progressBar.setRange(0, 0)
            self.progressBar.setFormat("Stopping...")
            self.progressBar.setVisible(True)
        except Exception:
            pass

        # Cancel pending futures. Already-running workers keep going to
        # avoid leaving half-written cache / tif on disk; we'll wait for
        # them in _updateStopProgress.
        if self.processExecutor is not None:
            try:
                self.processExecutor.shutdown(wait=False, cancel_futures=True)
            except Exception:
                pass
            # Don't clear self.processExecutor here — _finalize_batch will.
            # We need it to stay around so add_done_callback can still fire.

        # Single-image thread pool (unused in batch) — safe to clear.
        if self.threadPool is not None:
            try:
                self.threadPool.clear()
            except Exception:
                pass

        # Drain any leftover bookkeeping that never made it to the pool.
        try:
            while not self.tasksQueue.empty():
                self.tasksQueue.get_nowait()
        except Exception:
            pass

        # Repaint the folder button as "Stopping..." so the user gets
        # immediate feedback even before the modal popup is up.
        try:
            self.navControls.processFolderButton.toggled.disconnect(self.batchProcBtnToggled)
            self.navControls.processFolderButton.setText("Stopping...")
            self.navControls.processFolderButton.setChecked(False)
            self.navControls.processFolderButton.setEnabled(False)
            self.navControls.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        except Exception:
            pass
        try:
            self.navControls.processH5Button.setText("Stopping...")
            self.navControls.processH5Button.setEnabled(False)
        except Exception:
            pass

        running_count = self.taskManager.get_running_count()

        msg = (f"Stopping Batch Processing\n\n"
               f"Waiting for {running_count} task(s) to complete...")
        # ``None`` cancel button → user can't cancel the stop itself.
        self._stopProgress = QProgressDialog(msg, None, 0, 0, self)
        self._stopProgress.setWindowTitle("Stopping...")
        self._stopProgress.setWindowFlags(
            Qt.Window | Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint
        )
        self._stopProgress.setModal(False)
        self._stopProgress.setMinimumDuration(0)
        self._stopProgress.show()

        self._stopMsgTimer = QTimer(self)
        self._stopMsgTimer.setInterval(300)
        self._stopMsgTimer.timeout.connect(self._updateStopProgress)
        self._stopMsgTimer.start()

        # Trigger an immediate check in case running_count is already 0
        # (e.g. user clicked Stop after all tasks had already started
        # their cleanup phase).
        QTimer.singleShot(0, self._updateStopProgress)

    def _updateStopProgress(self):
        """Tick-handler for the 'Stopping...' progress dialog.

        Runs on the GUI thread every 300ms. Once no worker is running and
        every completion/cancellation callback has been accounted for, we
        close the dialog, run the standard batch finalize path, and notify
        the user.
        """
        if self._stopProgress is None:
            return
        try:
            running_count = self.taskManager.get_running_count()
            stats = self.taskManager.get_statistics()
        except Exception:
            running_count = 0
            stats = {'pending': 0}

        pending_count = stats.get('pending', 0)
        if running_count > 0:
            msg = (f"Stopping Batch Processing\n\n"
                   f"Waiting for {running_count} task(s) to complete...")
        else:
            msg = (f"Stopping Batch Processing\n\n"
                   f"Finalizing {pending_count} task callback(s)...")
        try:
            self._stopProgress.setLabelText(msg)
        except Exception:
            pass

        if running_count > 0 or pending_count > 0:
            return

        # All workers have wound down and every completion/cancellation
        # callback has been accounted for. Tear the dialog + timer down,
        # then run final cleanup (idempotent w.r.t. _finalize_batch).
        if self._stopMsgTimer is not None:
            try:
                self._stopMsgTimer.stop()
            except Exception:
                pass
            self._stopMsgTimer = None
        try:
            self._stopProgress.close()
        except Exception:
            pass
        self._stopProgress = None

        # _finalize_batch is idempotent: it short-circuits if
        # batchProcessing is already False (this can happen when the
        # last callback already ran _on_batch_phase_complete before the
        # polling tick fired).
        if getattr(self, 'batchProcessing', False):
            self._finalize_batch()

        # Show a brief 'stopped' summary so the user has a clear marker
        # of completion (mirrors EQ's _cleanupAfterBatch UX, which uses
        # the dialog close itself as the marker).
        if self._stop_initiated:
            self._stop_initiated = False
            self._show_batch_stopped_message()

    def _reset_batch_ui_only(self):
        """Reset batch-related UI without touching pool / task state.

        Used when ``clearTasks`` is called while no batch is running
        (defensive — e.g. toggling the Process Folder button while the
        progress bar is hidden).
        """
        self.progressBar.setVisible(False)
        try:
            self.navControls.filenameLineEdit.setEnabled(True)
        except Exception:
            pass
        try:
            self.navControls.processFolderButton.setText("Process Current Folder")
            self.navControls.processFolderButton.setChecked(False)
            self.navControls.processFolderButton.setEnabled(True)
        except Exception:
            pass
        try:
            self.navControls.processH5Button.setText("Process Current H5 File")
            self.navControls.processH5Button.setChecked(False)
            self.navControls.processH5Button.setEnabled(True)
        except Exception:
            pass
        self.currentTask = None
        self._batch_background_configurations = []
        self._force_recalc_bg_for_batch = False

    def _show_batch_stopped_message(self):
        """Brief popup acknowledging a user-initiated stop."""
        try:
            totalSuccess = self.successCount + self.retrySuccessCount
            totalProcessed = totalSuccess + self.retryFailCount
            cancelledCount = max(0, self.totalFiles - totalProcessed) if self.totalFiles else 0

            msgBox = QMessageBox(self)
            msgBox.setWindowTitle("Batch Stopped")
            msgBox.setIcon(QMessageBox.Information)
            text = (
                f"Batch processing was stopped by user.\n\n"
                f"Completed: {totalSuccess}\n"
                f"Cancelled / not processed: {cancelledCount}"
            )
            if self.retryFailCount > 0:
                text += f"\nFailed: {self.retryFailCount}"
            msgBox.setText(text)
            msgBox.setStandardButtons(QMessageBox.Ok)
            msgBox.exec_()
        except Exception as e:
            print(f"QF: could not show stopped message: {e}")
        
    def h5batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.navControls.processH5Button.isChecked():
            if not self.progressBar.isVisible():
                self.navControls.processH5Button.setText("Stop")
                self.processH5File()
        else:
            self.clearTasks()

    def processFolder(self):
        """
        Triggered when a folder has been selected to process it
        """
        idxs = range(len(self.file_manager.names))
        self._process_image_list(idxs, text="Process Current Folder")

    def _process_image_list(self, img_ids, text):
        """
        Triggered when a folder has been selected to process it
        """

        errMsg = QMessageBox()
        errMsg.setText(text)
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'

        flags = self.getFlags()
        manual_bg_selected = self.bgOptionsCB.currentText() in (
            "Manual Setting | One Method",
            "Manual Setting | Transition",
        )

        # ======= Manual assignments and auto selection both depend on saved configurations. ======
        optimize_each_image = bool(
            hasattr(self, "optimizeEachImageChkBx") and self.optimizeEachImageChkBx.isChecked()
        )

        has_manual_assignments = (
            hasattr(self, "bgSubDialog")
            and isinstance(self.bgSubDialog.manualBackgroundAssignments, dict)
            and len(self.bgSubDialog.manualBackgroundAssignments) > 0
        )

        resolved_manual_assignments = {}
        self._background_configurations = self.bgSubDialog._read_background_configurations()
        if not optimize_each_image and len(self._background_configurations) != 0:
            resolved_manual_assignments = self.bgSubDialog._resolve_manual_background_assignments_for_batch(
                self._background_configurations
            )
            if has_manual_assignments and len(resolved_manual_assignments) == 0:
                    QMessageBox.warning(
                        self,
                        "Invalid Manual Assignments",
                        "Manual assignments were set, but none match the current saved configurations. Batch processing was stopped."
                    )
                    self.navControls.processFolderButton.setChecked(False)
                    self.navControls.processH5Button.setChecked(False)
                    return


        text += "\nCurrent Settings"

        if len(self.ignoreFolds) > 0:
            text += "\n  - Ignore Folds : " + str(list(self.ignoreFolds))
        
        # Show orientation finding method
        orientation_methods = ["Max Intensity", "GMM", "Herman Factor (Half Pi)", "Herman Factor (Pi)"]
        orientation_model = self.workspace._orientation_model
        orientation_text = orientation_methods[orientation_model] if orientation_model is not None else "Max Intensity"
        text += "\n  - Orientation Finding : " + orientation_text
        
        mode_rotation = self.workspace._mode_rotation
        if mode_rotation is not None:
            text += "\n  - Mode Orientation : Enabled"
        
        # Show empty cell image configuration if exists
        if flags.get('blank_mask', False):
            sm = self.workspace.settings_manager
            try:
                import json
                with open(sm.blank_config_path, "r") as f:
                    blank_config = json.load(f)
                blank_file = Path(blank_config.get("file_path", "")).name
                blank_weight = blank_config.get("weight", 1.0)
                text += f"\n  - Empty Cell Image : {blank_file} (weight: {blank_weight})"
            except:
                text += "\n  - Empty Cell Image : Enabled"

        def _append_bg_method_details(method_key: str, suffix: str = "In"):
            method = flags.get(method_key, "None")
            label = f"\n  - Background Subtraction Method ({suffix}) : {method}"
            text_parts = [label]

            suffix = "" if suffix == "In" else "_out"

            if method != 'None':
                if suffix != "_out":
                    if 'fixed_rmin' in flags:
                        text_parts.append("\n  - R-min : " + str(flags["fixed_rmin" + suffix]))
                    if 'fixed_rmax' in flags:
                        text_parts.append("\n  - R-max : " + str(flags["fixed_rmax" + suffix]))

                if method in ['Circularly-symmetric', 'Roving Window']:
                    text_parts.append(
                        "\n  - Pixel Range (Percentage) : "
                        + str(flags.get("cirmin" + suffix, ""))
                        + "% - "
                        + str(flags.get("cirmax" + suffix, ""))
                        + "%"
                    )

                if method == 'Circularly-symmetric':
                    text_parts.append("\n  - Radial Bin : " + str(flags.get("radial_bin" + suffix, "")))
                    text_parts.append("\n  - Smooth : " + str(flags.get("smooth" + suffix, "")))
                elif method == '2D Convexhull':
                    text_parts.append("\n  - Step (deg) : " + str(flags.get("degree" + suffix, "")))
                elif method == 'White-top-hats':
                    text_parts.append("\n  - Tophat (inside R-max) : " + str(flags.get("tophat" + suffix, "")))
                elif method == 'Smoothed-Gaussian':
                    text_parts.append("\n  - FWHM : " + str(flags.get("fwhm" + suffix, "")))
                    text_parts.append("\n  - Number of cycle : " + str(flags.get("cycles" + suffix, "")))
                elif method == 'Smoothed-BoxCar':
                    text_parts.append("\n  - Box car width : " + str(flags.get("boxcar_x" + suffix, "")))
                    text_parts.append("\n  - Box car height : " + str(flags.get("boxcar_y" + suffix, "")))
                    text_parts.append("\n  - Number of cycle : " + str(flags.get("cycles" + suffix, "")))

            return "".join(text_parts)

        bg_options_text = self.bgOptionsCB.currentText() if hasattr(self, "bgOptionsCB") else "Automated Processing"
        if bg_options_text == "Manual Setting | One Method":
            text += _append_bg_method_details("bgsub", "In")
        elif bg_options_text == "Manual Setting | Transition":
            text += _append_bg_method_details("bgsub", "In")
            text += _append_bg_method_details("bgsub_out", "Out")
            if hasattr(self, "tranRSpnBx"):
                text += "\n  - Merge Transition Radius : " + str(self.tranRSpnBx.value())
            if hasattr(self, "tranDeltaSpnBx"):
                text += "\n  - Merge Transition Delta : " + str(self.tranDeltaSpnBx.value())

    
        if optimize_each_image:
            text += "\n  - Optimize Each Image : Enabled"
        elif self.chooseConfigurationsAutoChkBx.isChecked():
            text += "\n  - Auto Configuration Selection : Enabled"
            text += f"\n  - Saved Configurations Loaded : {len(self._background_configurations)}"
        if not optimize_each_image and len(resolved_manual_assignments) > 0:
            text += f"\n  - Manual Assignments : {len(resolved_manual_assignments)} image(s)"

        text += '\n\nAre you sure you want to process ' + str(len(img_ids)) + ' image(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        if ret == QMessageBox.Yes:
            # Reset progress bar for batch processing (percentage mode)
            self.progressBar.setRange(0, 100)
            self.progressBar.setFormat("%p%")
            self.progressBar.setValue(0)
            self.progressBar.setVisible(True)
            
            self.stop_process = False
            self.batchProcessing = True  # Enable batch processing mode
            self._force_recalc_bg_for_batch = manual_bg_selected
            self.totalFiles = len(img_ids)
            self.tasksDone = 0

            # Reset retry and statistics tracking
            self.retryQueue = Queue()
            self.isRetryPhase = False
            self.successCount = 0
            self.retrySuccessCount = 0
            self.retryFailCount = 0
            self.firstAttemptErrors = {}
            self.failedTaskErrors = {}
            self.saveErrors = {}

            # Reset multiprocessing bookkeeping (mirrors EquatorWindow)
            self.taskManager.clear()
            self.pendingJobArgs.clear()
            self.retryJobArgs = []
            self.bgAsyncDict = {}
            # Clear any leftover Stop-state from a previous interrupted batch.
            self._stop_initiated = False
            if self._stopProgress is not None:
                try:
                    self._stopProgress.close()
                except Exception:
                    pass
                self._stopProgress = None
            if self._stopMsgTimer is not None:
                try:
                    self._stopMsgTimer.stop()
                except Exception:
                    pass
                self._stopMsgTimer = None
            if self.processExecutor is None:
                self.initProcessExecutor()

            for idx, i in enumerate(img_ids):
                if self.stop_process:
                    break
                self.addTask(i)
                # Process UI events periodically to keep Stop button responsive
                if idx % 10 == 0:
                    QApplication.processEvents()

            # self.progressBar.setVisible(False)
            # Note: Don't setChecked(False) here - it will trigger clearTasks() and clear the queue!
            # The button will be reset in _finalize_batch() when all tasks complete
        else:
            # User cancelled the dialog, reset button
            self.navControls.processFolderButton.setChecked(False)


    def processH5File(self):
        """
        Triggered when a folder with multiple H5 files has been selected to process it
        """
        start_idx, end_idx = self.file_manager.get_current_h5_range()
        if start_idx is None or end_idx is None:
            return
        self._process_image_list(range(start_idx, end_idx + 1), text="Process Current H5 File")


    def saveSettings(self):
        """
        save settings to json
        """
        # Start from all current UI flags
        settings = dict(self.getFlags())

        # Remove per-session / per-image keys that should not be persisted.
        # batch_processing / force_recalc_bg / manual_background_assignments
        # are runtime state (run-scoped flags; manual_background_assignments
        # is keyed by image filename and would silently leak when this
        # settings file is reused on a different image set).
        for key in ('ignore_folds', 'orientation_model', 'blank_mask',
                    'apply_mask', 'mode_angle', 'roi_w', 'roi_h', 'detector',
                    'center', 'fold_image', 'rotate',
                    'batch_processing', 'force_recalc_bg',
                    'manual_background_assignments'):
            settings.pop(key, None)

        # The 'out'-radius / transition fields are only consumed when
        # bg_options==1 (Transition mode). Strip them otherwise so the
        # persisted file reflects only what will actually be used.
        if settings.get('bg_options') != 1:
            for key in ('bgsub_out', 'cirmin_out', 'cirmax_out',
                        'win_size_x_out', 'win_size_y_out',
                        'win_sep_x_out', 'win_sep_y_out',
                        'radial_bin_out', 'smooth_out', 'tension_out',
                        'tophat_out', 'fwhm_out',
                        'boxcar_x_out', 'boxcar_y_out', 'cycles_out',
                        'transition_radius', 'transition_delta'):
                settings.pop(key, None)

        # Output compression flag (not in getFlags, driven by its own checkbox)
        settings['compressed'] = self.compressFoldedImageChkBx.isChecked()

        # Persist ROI only when the user explicitly enabled "Persist ROI size"
        if self.fixedRoiChkBx.isChecked():
            settings['fixed_roi_w'] = self.fixedRoiW.value()
            settings['fixed_roi_h'] = self.fixedRoiH.value()
        else:
            settings.pop('fixed_roi_w', None)
            settings.pop('fixed_roi_h', None)

        if self.quadFold is not None and 'bgsub' in self.quadFold.info:
            settings['bgsub'] = self.quadFold.info['bgsub']

        filename = getSaveFile(os.path.join("musclex", "settings", "qfsettings.json"), None)
        if filename != "":
            with open(filename, 'w') as f:
                json.dump(settings, f)


    def loadSettings(self):
        """
        Load settings from a previously-saved qfsettings.json and push
        them into the widgets so the GUI state mirrors what headless /
        the test suite would see when handed the same JSON.

        Driven by the module-level binding tables
        (_QF_SPINBOX_BINDINGS / _QF_COMBO_TEXT_BINDINGS /
        _QF_CHECKBOX_BINDINGS) plus a few special-case keys handled
        inline. Per-image and runtime state (_QF_SKIP_KEYS) is
        deliberately ignored even if present.

        Before triggering the reprocess we invalidate three layers of
        cached state, mirroring what applyBGSub() does for the
        manual "Apply" button. Otherwise loading a settings file
        that changes the BG method (e.g. None -> Circularly-symmetric)
        would reuse the stale BgSubFold / BgFold from the previous
        method and the new CSV row would still report a zero bgSum:

          - processing_fingerprint: dropped so process() cannot take
            the fast-path on the cached _folded.tif written under
            the old parameters.
          - info['result_bg']: dropped so the BG-search fallback
            rebuilds method / final_params from the just-loaded
            settings instead of inheriting whatever was sticky from
            the previous run.
          - imgCache[BgSubFold* / BgFold*]: dropped so
            applyBackgroundSubtraction() actually recomputes; its
            in-method guard `if "BgSubFold" not in self.imgCache`
            would otherwise silently skip the recompute and leave
            the displayed/saved bg image at the previous method's
            value (most visibly: bgSum=0 when the previous method
            was 'None').
        """
        default_dir = os.path.join("musclex", "settings")
        filename = getAFile(filtr='Settings (*.json)', path=default_dir, parent=self)
        if not filename:
            return

        try:
            with open(filename, 'r') as f:
                loaded = json.load(f)
        except Exception as exc:
            QMessageBox.warning(
                self, "Load Settings",
                f"Failed to read {filename}:\n{exc}"
            )
            return

        if not isinstance(loaded, dict):
            QMessageBox.warning(
                self, "Load Settings",
                "Settings file did not contain a JSON object."
            )
            return

        unknown_keys = [k for k in loaded
                        if _classify_qf_setting_key(k) == 'unknown']
        if unknown_keys:
            # Warn but proceed -- unknown keys are likely future
            # additions or hand-edits; the rest of the file can still
            # be applied.
            print(f"[loadSettings] Ignoring unknown keys in {filename}: {unknown_keys}")

        self._apply_loaded_qf_settings(loaded)

        if self.quadFold is not None:
            self.quadFold.info.pop('processing_fingerprint', None)
            # Mirror applyBGSub(): a method/params change must invalidate
            # the BG-subtraction caches end-to-end, or the next reprocess
            # will hit applyBackgroundSubtraction()'s `if "BgSubFold" not
            # in self.imgCache` guard and silently reuse the previous
            # method's intermediates.
            self.deleteInfo(['result_bg'])
            self.deleteImgCache([
                'BgSubFold', 'BgSubFold_out', 'BgSubFold_syn', 'BgSubFold_syn_out',
                'BgFold', 'BgFold_out', 'BgFold_syn', 'BgFold_syn_out',
            ])
            # Mirror the post-rebase session-push contract: synthetic /
            # mask / freq widgets are session-global, and processImage()
            # reads them out of info (not directly from widgets), so we
            # need to flush the just-loaded widget values into info
            # before reprocessing or process() would see stale info
            # values from the previous image.
            self._push_session_bg_eval_settings_to_info()

        if self.ableToProcess():
            self.processImage()

    def _apply_loaded_qf_settings(self, loaded):
        """
        Apply a sanitized qfsettings.json dict to the UI widgets.

        Signals are blocked around the bulk of the assignment so we
        don't trigger N reprocess passes mid-load; the post-load
        processImage() call (or the bg_options handler we fire
        manually at the end) drives the single re-render.
        """
        self.uiUpdating = True
        try:
            for k, w in _QF_SPINBOX_BINDINGS:
                if k not in loaded:
                    continue
                widget = getattr(self, w, None)
                if widget is None:
                    continue
                widget.blockSignals(True)
                try:
                    widget.setValue(loaded[k])
                finally:
                    widget.blockSignals(False)

            for k, w, xform in _QF_COMBO_TEXT_BINDINGS:
                if k not in loaded:
                    continue
                widget = getattr(self, w, None)
                if widget is None:
                    continue
                # xform produces text that matches a combo item exactly
                # (e.g. _degree_to_combo turns 1.0 -> "1"); findText is
                # exact by default and that's what we want.
                text = xform(loaded[k])
                idx = widget.findText(text)
                if idx >= 0:
                    widget.blockSignals(True)
                    try:
                        widget.setCurrentIndex(idx)
                    finally:
                        widget.blockSignals(False)
                    # Fire change handler exactly once (visibility /
                    # panel updates depend on it). Doing this AFTER
                    # unblocking keeps signal emission centralized.
                    widget.currentIndexChanged.emit(idx)
                else:
                    print(f"[loadSettings] combo '{w}' has no item matching "
                          f"{text!r} (from {k}={loaded[k]!r}); skipping.")

            for k, w in _QF_CHECKBOX_BINDINGS:
                if k not in loaded:
                    continue
                widget = getattr(self, w, None)
                if widget is None:
                    continue
                widget.blockSignals(True)
                try:
                    widget.setChecked(bool(loaded[k]))
                finally:
                    widget.blockSignals(False)

            # Special keys ----------------------------------------------------

            if 'optimize' in loaded:
                self.optimizeFlag = bool(loaded['optimize'])

            if 'methods' in loaded and isinstance(loaded['methods'], (list, tuple)):
                self._set_selected_optimization_methods(loaded['methods'])

            if 'steps' in loaded and isinstance(loaded['steps'], (list, tuple)):
                self.stepsLineEdit.setText(", ".join(str(v) for v in loaded['steps']))

            if 'background_configurations' in loaded \
                    and isinstance(loaded['background_configurations'], list) \
                    and hasattr(self, 'bgSubDialog'):
                # Replace wholesale -- the dialog will rebuild its UI
                # from this list the next time it is opened.
                self.bgSubDialog.backgroundConfigurations = list(
                    loaded['background_configurations']
                )

            # Paired ROI: only enable "Persist ROI size" when both
            # dimensions are present and positive.
            roi_w = loaded.get('fixed_roi_w')
            roi_h = loaded.get('fixed_roi_h')
            if (isinstance(roi_w, (int, float)) and isinstance(roi_h, (int, float))
                    and roi_w > 0 and roi_h > 0):
                self.fixedRoiW.blockSignals(True)
                self.fixedRoiH.blockSignals(True)
                self.fixedRoiChkBx.blockSignals(True)
                try:
                    self.fixedRoiW.setValue(int(roi_w))
                    self.fixedRoiH.setValue(int(roi_h))
                    self.fixedRoiChkBx.setChecked(True)
                finally:
                    self.fixedRoiW.blockSignals(False)
                    self.fixedRoiH.blockSignals(False)
                    self.fixedRoiChkBx.blockSignals(False)

            # Metric means / weights -- the only fields the sync helper
            # still handles (fraction<->percent conversion lives in there).
            metric_info = {k: loaded[k] for k in (
                'mean_metric_values', 'metric_weights',
            ) if k in loaded}
            if metric_info:
                self._sync_metric_and_synthetic_widgets_from_info(info=metric_info)

            # synthetic Gaussian / evaluation baseline / freq used to be
            # routed through _sync_metric_and_synthetic_widgets_from_info,
            # but the post-rebase design treats them as session-global
            # widgets pushed *into* info via _push_session_bg_eval_settings_to_info.
            # That means the helper no longer reads them, so we have to
            # set the widgets directly here or loadSettings would silently
            # drop these JSON fields. (freq is already handled by the combo
            # loop above via QF_COMBO_TEXT_BINDINGS.)
            #
            # The SpinBoxes accept 0.0 as a sentinel (min_val=0); the
            # backend (_ensure_synthetic_gaussian_params) re-derives a
            # real value from the folded image when info has 0/<=0 and
            # then clamps with MIN_SYNTHETIC_* / MIN_EVAL_BASELINE. So a
            # plain setValue here mirrors the headless qfsettings.json path.
            for json_key, widget_attr in (
                ('evaluation_baseline', 'evaluationBaselineSpnBx'),
                ('synthetic_amplitude', 'amplitudeSpnBx'),
                ('synthetic_sigma_x',   'sigmaXSpnBx'),
                ('synthetic_sigma_y',   'sigmaYSpnBx'),
            ):
                if json_key not in loaded:
                    continue
                widget = getattr(self, widget_attr, None)
                if widget is None:
                    continue
                try:
                    val = float(loaded[json_key])
                except (TypeError, ValueError):
                    continue
                widget.blockSignals(True)
                try:
                    widget.setValue(val)
                finally:
                    widget.blockSignals(False)

            # bg_options: applied last so the manual / transition /
            # automated container visibility reflects the just-loaded
            # in/out parameter values.
            if 'bg_options' in loaded:
                try:
                    idx = int(loaded['bg_options'])
                except (TypeError, ValueError):
                    idx = -1
                if 0 <= idx < self.bgOptionsCB.count():
                    self.bgOptionsCB.setCurrentIndex(idx)
                    # setCurrentIndex fires _on_bg_options_changed when
                    # the index actually changes; force-fire when it
                    # was already at idx so visibility is consistent.
                    self._on_bg_options_changed(idx)

        finally:
            self.uiUpdating = False


    def _on_folder_loaded(self, dir_path: str):
        """
        Called when a new folder/file is loaded (BEFORE first image loads).
        
        Initializes QF-specific folder-level data structures.
        This ensures csvManager is ready before any image processing begins.
        
        Args:
            dir_path: Directory path of the loaded file/folder
        """
        QApplication.setOverrideCursor(Qt.WaitCursor)
        
        try:
            # Update file path
            self.filePath = dir_path
            
            # Initialize CSV Manager (QF-specific, folder-level)
            if self.file_manager.dir_path and self.file_manager.names:
                try:
                    csv_dir = self.workspace.dir_context.output_dir if self.workspace.dir_context else self.filePath
                    self.csvManager = QF_CSVManager(csv_dir, extra_colnames=_qf_setting_keys(), version=__version__)
                    self.ignoreFolds = set()
                    self.resetWidgets()
                    if hasattr(self, "bgSubDialog"):
                        self.bgSubDialog.manualBackgroundAssignments = {}
                    self._background_configurations = []
                    
                    # Update left widget width
                    self.updateLeftWidgetWidth()
                    self.bgSubDialog._load_background_configurations_from_cache()
                    
                except Exception as e:
                    print("Exception occurred while creating CSVManager:", e)
                    tb_str = traceback.format_exc()
                    print(f"Full traceback: {tb_str}\n")
                    
                    msg = QMessageBox()
                    msg.setInformativeText(
                        f"Permission denied when creating a folder at {self.filePath}. "
                        "Please check the folder permissions."
                    )
                    msg.setStandardButtons(QMessageBox.Ok)
                    msg.setWindowTitle("Error Creating CSVManager")
                    msg.setStyleSheet("QLabel{min-width: 500px;}")
                    msg.exec_()
                    
                    self.csvManager = None
                    QApplication.restoreOverrideCursor()
                    return
            
            QApplication.restoreOverrideCursor()
            
        except Exception as e:
            print(f"Error in _on_folder_loaded: {e}")
            traceback.print_exc()
            QApplication.restoreOverrideCursor()
    
    def _on_image_data_ready(self, image_data):
        """
        Called when ImageData is ready for processing.
        
        This is the simplified version - workspace has already:
        - Created ImageData with all settings
        - Tried to auto-show calibration dialog
        
        Args:
            image_data: ImageData instance ready to process
        """
        # Check if csvManager is initialized
        if self.csvManager is None:
            print("Warning: csvManager not initialized. Skipping image processing.")
            return
        
        # Store ImageData
        self.current_image_data = image_data
        
        # Create QuadrantFolder processor
        qf_output = self.workspace.dir_context.output_dir if self.workspace.dir_context else None
        self.quadFold = QuadrantFolder(self.current_image_data, self, output_dir=qf_output)
        self._process_count_for_current_image = 0

        # Update UI for new image
        self._update_ui_for_image()
        
        # Process the image
        self.processImage()
    
    def _update_ui_for_image(self):
        """
        Update UI elements when a new image is loaded.
        
        Separated from onImageChanged for clarity.
        
        Note: filename and nav mode are automatically updated by ImageNavigatorWidget,
        no need to update them here.
        """
        # Update image info display
        original_image = self.quadFold.orig_img
        self.imgDetailOnStatusBar.setText(
            f"{original_image.shape[0]}x{original_image.shape[1]} : {original_image.dtype}"
        )

        # ROI: if "Persist ROI size" is off, drop any ROI carried over from
        # the previous image. If it's on, leave the spinboxes alone so the
        # same size applies to this image.
        if not self.fixedRoiChkBx.isChecked():
            for sb in (self.fixedRoiW, self.fixedRoiH):
                blocker = QSignalBlocker(sb)
                sb.setValue(0)
                del blocker

        # Reset viewer zoom state so the new image isn't shown stuck at the
        # previous image's pan/zoom rectangle (which can look like the
        # display area "stayed small" after switching).
        self.result_zoom = None
        self.default_img_zoom = None
        self.default_result_img_zoom = None
        self.zoomOutClicked = True

        if (
            hasattr(self, "persistEvaluationBaselineChkBx")
            and hasattr(self, "evaluationBaselineSpnBx")
            and not self.persistEvaluationBaselineChkBx.isChecked()
        ):
            self._clear_persisted_evaluation_baseline()
            blocker = QSignalBlocker(self.evaluationBaselineSpnBx)
            self.evaluationBaselineSpnBx.setValue(0.0)
            del blocker

        if (
            hasattr(self, "persistSyntheticDataChkBx")
            and hasattr(self, "amplitudeSpnBx")
            and hasattr(self, "sigmaXSpnBx")
            and hasattr(self, "sigmaYSpnBx")
            and not self.persistSyntheticDataChkBx.isChecked()
        ):
            self._clear_persisted_synthetic_params()

        # Restore cache state if available
        if hasattr(self.quadFold, 'info'):
            previnfo = self.quadFold.info
            
            # Restore ignore folds
            if 'ignore_folds' in self.quadFold.info:
                self.ignoreFolds = self.quadFold.info['ignore_folds']
            
            # Initialize widgets with previous info
            self.initialWidgets(original_image, previnfo)
            self.markFixedInfo(self.quadFold.info, previnfo)

        # Update workspace display (includes blank/mask states)
        self.workspace.update_display(self.current_image_data)

        # Auto-adjust downsample based on image size
        self._set_downsample_from_image_size(original_image)

    def _set_downsample_from_image_size(self, image):
        """Use cached downsample when available, otherwise set size-based default."""
        if image is None or not hasattr(self, 'downsampleCB'):
            return

        try:
            info = self.quadFold.info if hasattr(self, "quadFold") and self.quadFold is not None else {}
            cached_downsample = info.get('downsample', None) if isinstance(info, dict) else None

            if cached_downsample not in (None, ""):
                try:
                    cached_text = str(int(cached_downsample))
                    if self.downsampleCB.findText(cached_text) >= 0:
                        self.downsampleCB.setCurrentText(cached_text)
                        return
                except Exception:
                    pass

            h, w = image.shape[:2]
            default_text = "1" if max(h, w) < 1000 else "2"
            self.downsampleCB.setCurrentText(default_text)

            # Keep info consistent so this default can be cached and reused next load.
            if isinstance(info, dict):
                info['downsample'] = int(default_text)
        except Exception:
            pass

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        self.eventEmitter.statusTextRequested.emit(str(text))
        print(text)

    @Slot(str)
    def _set_status_report_text(self, text):
        """UI-thread safe status text updater used by worker threads."""
        self.statusReport.setText(text)

    def processPendingEvents(self):
        """Allow long-running workers to keep Qt responsive by pumping pending events."""
        try:
            QApplication.processEvents(QEventLoop.AllEvents, 25)
        except Exception:
            QApplication.processEvents()

    # NOTE: fileNameChanged() removed - handled by ImageNavigatorWidget._on_filename_changed()
    # The widget automatically emits imageChanged signal which triggers _on_image_changed()

    def showAbout(self):
        """
        Display About Dialog
        """
        msgBox = QMessageBox()
        msgBox.setWindowTitle("About")
        msgBox.setTextFormat(Qt.RichText)
        msgBox.setText("<br><br><br>" +
                       "Quadrant Folder is running under" +
                       "<h2>Muscle X v" +
                       __version__ +
                       "</h2><br><br>" +
                       "&copy;2023 BioCAT <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://www.bio.aps.anl.gov/") +
                       "Documentation : <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://musclex.readthedocs.io/en/latest/") +
                       "GitHub : <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://github.com/biocatiit/musclex") +
                       "Send Feedback or Issues : <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://github.com/biocatiit/musclex/issues"))
        msgBox.setStandardButtons(QMessageBox.Ok)
        msgBox.exec_()