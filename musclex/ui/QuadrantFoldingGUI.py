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
from PySide6.QtCore import QRunnable, QThreadPool, QEventLoop, Signal, QTimer, QSize
from PySide6.QtWidgets import QStyle
from queue import Queue
import fabio
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..utils import ImageData

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
from ..utils.background_search import makeFullImage
import time
import random

class QuadFoldParams:
    def __init__(self, flags, index, file_manager, parent):
        self.flags = flags
        self.index = index
        self.file_manager = file_manager
        self.parent = parent


class WorkerSignals(QObject):
    finished = Signal()
    error = Signal(object)
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
        """Default error handling."""
        traceback.print_exc()
        self.signals.error.emit({
            'error': str(error),
            'traceback': traceback.format_exc(),
        })
    
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
        self.quadFold.process(self.flags)
        return self.quadFold


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
        
        # Create and process
        self.quadFold = QuadrantFolder(img_data, self.params.parent)
        self.quadFold.info['bgsub'] = self.bgsub
        self.quadFold.process(self.params.flags)
        
        # Post-processing
        self._save_background(filename)
        self._log_completion()
        
        return self.quadFold


    def _save_background(self, filename):
        """Save background image and update shared dictionary."""
        info = self.quadFold.info
        result = self.quadFold.imgCache.get("BgSubFold")
        
        if result is None:
            return
        
        avg_fold = info.get("avg_fold")
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
    
    def _log_completion(self):
        """Thread-safe task completion logging."""
        if self.lock is not None:
            with self.lock:
                self._write_task_log()
    
    def _write_task_log(self):
        """Write task completion to log file."""
        log_path = os.path.join(
            self.quadFold.img_path,
            "qf_results",
            "tasks_done.txt"
        )
        with open(log_path, "a") as f:
            f.write(f"{self.quadFold.img_name} saving image\n")

class EventEmitter(QObject):
    statusTextRequested = Signal(str)

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
        # self.chordLines = []
        # self.chordpoints = []
        self.csvManager = None
        
        self.threadPool = QThreadPool()
        # Limit for concurrent workers used by Process Folder / Process H5.
        # Change this value to control how many tasks run in parallel.
        self.maxConcurrentProcesses = min(8, int(self.threadPool.maxThreadCount() / 3))
        self.tasksQueue = Queue()
        self.currentTask = None
        self.worker = None
        self.tasksDone = 0
        self.totalFiles = 1
        self.lock = Lock()
        self.batchProcessing = False  # Flag to indicate batch processing mode
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
        self._restoreOptimizeCheckboxAfterProcess = False
        self._OptimizationRunning = False
        self._stopOptimizationRequested = False

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
        self.selectFolder = self.workspace.navigator.select_folder_btn
        
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
        selectImageAction.triggered.connect(self._on_browse_file)
        
        selectFolderAction = QAction('Select a Folder...', self)
        selectFolderAction.setShortcut('Ctrl+F')
        # NOTE: Folder selection removed - not currently used
        # selectFolderAction.triggered.connect(self._on_browse_folder)
        
        saveSettingsAction = QAction('Save Current Settings', self)
        saveSettingsAction.setShortcut('Ctrl+S')
        saveSettingsAction.triggered.connect(self.saveSettings)
        
        aboutAct = QAction('About', self)
        aboutAct.triggered.connect(self.showAbout)
        
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addAction(selectFolderAction)
        fileMenu.addAction(saveSettingsAction)
        
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
        
        self.cropFoldedImageChkBx = QCheckBox("Save Cropped Image (Original Size)")
        self.cropFoldedImageChkBx.setChecked(False)
        
        # Add quadrant-specific options to display panel's top slot
        self.image_viewer.display_panel.add_to_top_slot(self.showSeparator)
        self.image_viewer.display_panel.add_to_top_slot(self.cropFoldedImageChkBx)
        
        # QuadrantFolding-specific: start with 0 decimals
        self.spminInt.setDecimals(0)
        self.spmaxInt.setDecimals(0)
        
        # Add zoom button to checkable buttons list
        self.checkableButtons.append(self.imgZoomInB)

    def _create_quadrant_settings(self):
        """Add quadrant-specific settings to right panel"""
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

        # ===== Section controls =====
        self.applyResultBGButton = QPushButton("Apply Default Optimization")
        self.applyResultBGButton.setStyleSheet("QPushButton { color: #ededed; background-color: #2986cc;}")
        self.openBGSettingsButton = QPushButton("Advanced Configuration")

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
        bg_sub_layout.addWidget(self.resultDisplayModeLabel, 2, 0, 1, 1)
        bg_sub_layout.addWidget(self.resultDisplayModeCB, 2, 2, 1, 2)
        bg_sub_layout.addWidget(self.applyResultBGButton, 1, 0, 1, 4)
        bg_sub_layout.addWidget(self.openBGSettingsButton, 3, 2, 1, 2)
        self.bgSummaryLayout.addWidget(bg_sub_section, 2, 0, 1, 4)

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

        self.bgSummaryLayout.addWidget(current_section, 3, 0, 1, 4)

        self.resProcGrpBx.setLayout(self.bgSummaryLayout)

        # TODO: cache bg metrics
        # self._clear_bg_metrics_table()


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

        default_methods = ['Circularly-symmetric', 'White-top-hats', 'Smoothed-Gaussian']
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
        self.processImage()

        if not checked:
            self.optimizeFlag = False

        self.bgSubDialog.addBackgroundConfiguration(name="Default Optimization")
        

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
            'tophat1SpnBx', 'tophat1Label',
            'deg1Label', 'deg1CB',
            'applyBGButton',
            'optimizeFlag',
            'optimizationMethodsList',
            'stepsLineEdit',
            'maxIterationsSpnBx',
            'earlyStopSpnBx',
            'meanMSESpnBx',
            'meanNegSynSpnBx',
            'meanNonBaselineSpnBx',
            'meanNegConSpnBx',
            'meanSmoothSpnBx',
            'evaluationBaselineSpnBx',
            'ampSpnBx',
            'sigmaXDivSpnBx',
            'sigmaYDivSpnBx',
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
            'addBackgroundConfigButton',
            'backgroundConfigsTable',
            'deleteBackgroundConfigButton',
            'chooseConfigurationsAutoChkBx',
            'createNewConfigurationsChkBx',
            'assignConfgurationsManually',
            'processFolderWithSelections',
        ]:
            setattr(self, attr, getattr(self.bgSubDialog, attr))

        self.checkableButtons.append(self.setRminRmaxButton)

    def openBackgroundSubtractionDialog(self):
        """Open the background subtraction settings popup."""
        self._update_bg_method_summary()
        if 'evaluation_baseline' in self.quadFold.info:
            self.evaluationBaselineSpnBx.setValue(float(self.quadFold.info.get('evaluation_baseline',  self.evaluationBaselineSpnBx.value())))
        self.bgSubDialog.show()
        self.bgSubDialog.raise_()
        self.bgSubDialog.activateWindow()


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
        loss_text = "—" if loss is None else _to_metric_text(loss)

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
        """Add result display options (QF-specific)."""
        self.rotate90Chkbx = QCheckBox("Rotate 90 degree")

        extra_controls = QWidget()
        extra_layout = QGridLayout(extra_controls)
        extra_layout.setContentsMargins(0, 0, 0, 0)
        extra_layout.addWidget(self.rotate90Chkbx, 0, 0, 1, 1)
        self.result_display_panel.add_to_bottom_slot(extra_controls)

    
    def _initialize_cicle_patches(self):
        """Initialize patch objects for circles"""
        self.circle_patch_rmin = None
        self.circle_patch_rmax = None
    

    def setConnections(self):
        """
        Set all triggered functions for widgets
        """
        self.tabWidget.currentChanged.connect(self.onTabChanged)

        ##### Image Tab #####
        # NOTE: selectFolder.clicked connection removed - folder selection not currently used
        # self.selectFolder.clicked.connect(self._on_browse_folder)
        # Note: intensity/log_scale/colormap changes are handled automatically by ImageViewerWidget
        # via update_display_settings() which preserves overlays - no need to call refreshImageTab()
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
        self.cropFoldedImageChkBx.stateChanged.connect(self.cropFoldedImageChanged)
        self.compressFoldedImageChkBx.stateChanged.connect(self.compressFoldedImageChanged)

        self.showRminRmaxChkBx.stateChanged.connect(self.toggleCircleRminRmax)
        self.rminSpnBx.valueChanged.connect(self.toggleCircleRminRmax)
        self.rmaxSpnBx.valueChanged.connect(self.toggleCircleRminRmax)

        self.equatorMaskHeightSpnBx.valueChanged.connect(self.equatorMaskHeightChanged)
        self.equatorCenterBeamSpnBx.valueChanged.connect(self.equatorMaskCenterBeamChanged)
        self.m1SpnBx.valueChanged.connect(self.m1Changed)
        self.layerLineWidthSpnBx.valueChanged.connect(self.layerLineMaskWidthChanged)

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
        self.openBGSettingsButton.clicked.connect(self.openBackgroundSubtractionDialog)
        self.applyResultBGButton.clicked.connect(self.applyDefaultOptimization)
        self.addBackgroundConfigButton.clicked.connect(self.bgSubDialog.addBackgroundConfiguration)

        self.deleteBackgroundConfigButton.clicked.connect(self.bgSubDialog.deleteSelectedBackgroundConfiguration)
        self.assignConfgurationsManually.clicked.connect(self.bgSubDialog.openManualBackgroundAssignmentsDialog)
        self.backgroundConfigsTable.itemSelectionChanged.connect(self.bgSubDialog._on_background_config_selection_changed)
        self.backgroundConfigsTable.customContextMenuRequested.connect(self.bgSubDialog._show_background_config_context_menu)
        # self.setFitRoi.clicked.connect(self.setFitRoiClicked)
        # self.unsetRoi.clicked.connect(self.unsetRoiClicked)
        # self.fixedRoiChkBx.stateChanged.connect(self.fixedRoiChecked)
        # self.fixedRoi.editingFinished.connect(self.fixedRoiChanged)
        self.bgChoiceIn.currentIndexChanged.connect(self.bgChoiceInChanged)
        
        self.minPixRange.valueChanged.connect(self.pixRangeChanged)
        self.maxPixRange.valueChanged.connect(self.pixRangeChanged)

        self.setRminRmaxButton.clicked.connect(self.setManualRminRmax)
        self.rminSpnBx.valueChanged.connect(self.RminChanged)
        self.rmaxSpnBx.valueChanged.connect(self.RmaxChanged)

        self.applyBGButton.clicked.connect(self.applyBGSub)

        # self.blankImageGrp.clicked.connect(self.blankChecked)


    def _on_result_display_mode_changed(self, index):
        self.refreshResultTab()


    # NOTE: updateCurrentCenter removed - use workspace.update_display() instead



    def updateLeftWidgetWidth(self):
        """Update left panel width based on image viewer visibility."""
        # leftWidget is now navigator's select_panel
        if self.leftWidget and self.imageCanvas.isVisible():
            # Remove the minimum width constraint when image is showing
            self.leftWidget.setMinimumWidth(0)
        elif self.leftWidget:
            # Set the minimum width when only buttons are showing
            self.leftWidget.setMinimumWidth(650)


    def cropFoldedImageChanged(self):
        """
        Handle when the crop to original size checkbox is checked or unchecked
        """
        if self.quadFold is not None and not self.uiUpdating:
            self.saveResults()

    def compressFoldedImageChanged(self):
        """
        Handle when the compress folded image is checked or unchecked
        """
        if self.quadFold is not None and not self.uiUpdating:
            self.saveResults()

    def fixedRoiChecked(self):
        """
        Triggered when fixed ROI Radius is checked or unchecked
        """
        self.fixedRoi.setEnabled(self.fixedRoiChkBx.isChecked())
        if not self.fixedRoiChkBx.isChecked() and self.quadFold is not None:
            if 'fixed_roi_rad' in self.quadFold.info:
                del self.quadFold.info['fixed_roi_rad']
            self.processImage()

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
        Triggered when fixed ROI Radius spinbox value is changed
        """
        if self.quadFold is not None and not self.uiUpdating:
            self.quadFold.info['fixed_roi_rad'] = self.fixedRoi.value()
            self.result_zoom = None
            self.processImage()


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
            self.fixedRoi.setEnabled(False)
            self.fixedRoiChkBx.setChecked(False)
            if 'fixed_roi_rad' in self.quadFold.info:
                del self.quadFold.info['fixed_roi_rad']
            if 'roi_rad' in self.quadFold.info:
                del self.quadFold.info['roi_rad']
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
            ignoreThis.triggered.connect(self.addIgnoreQuadrant)
            menu.addAction(ignoreThis)
        else:
            unignoreThis = QAction('Unignore This Quadrant', self)
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

    def _refresh_mask_preview_if_visible(self):
        if not self.ableToProcess() or not self._is_mask_preview_visible():
            return

        # Push current UI values into info
        self._sync_eval_mask_settings_for_preview()

        # Force recompute on next draw
        self.deleteInfo(['mask', 'synthetic_mask'])

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
        if self.uiUpdating:
            return

        if self.quadFold is not None:
            self.quadFold.info['equator_mask_height'] = self.equatorMaskHeightSpnBx.value()

        self._refresh_mask_preview_if_visible()


    def equatorMaskCenterBeamChanged(self):
        """Triggered when Equator Mask Center Beam changes."""
        if self.uiUpdating:
            return
        
        if self.quadFold is not None:
            self.quadFold.info['equator_center_beam_width'] = self.equatorCenterBeamSpnBx.value()

        self._refresh_mask_preview_if_visible()

        

    def m1Changed(self):
        """Triggered when M1 spacing changes."""
        if self.uiUpdating:
            return

        if self.quadFold is not None:
            self.quadFold.info['m1'] = self.m1SpnBx.value()

        self._refresh_mask_preview_if_visible()


    def layerLineMaskWidthChanged(self):
        """Triggered when layer line width changes."""
        if self.uiUpdating:
            return

        if self.quadFold is not None:
            self.quadFold.info['layer_line_width'] = self.layerLineWidthSpnBx.value()


        self._refresh_mask_preview_if_visible()



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
                # Set new R-min and R-max
                img = self.quadFold.info['avg_fold']
                center = (img.shape[1], img.shape[0])
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
                # both width and height selected
                center = self.quadFold.imgCache['resultImg'].shape[0] / 2, self.quadFold.imgCache['resultImg'].shape[1] / 2
                radius = max(abs(x-center[0]), abs(y-center[1]))
                print("Selected Fit Reg Radius is ", radius)

                self.quadFold.info['roi_rad'] = radius
                self.fixedRoi.setValue(int(radius))
                print("New Image shape ", self.quadFold.imgCache['resultImg'].shape)
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
            # draw circles
            img = self.quadFold.info['avg_fold']
            center = (img.shape[1] - 1, img.shape[0] - 1)
            radius = distance((x, y), center)
            if len(ax.patches) > len(self.function) - 1:
                ax.patches[-1].remove()
            ax.add_patch(
                patches.Circle(center, radius, linewidth=2, edgecolor='r', facecolor='none', linestyle='solid'))
            self.resultCanvas.draw_idle()

        elif func[0] == "fit_region":
            center = img.shape[0] / 2, img.shape[1] / 2
            if len(ax.patches) > 0:
                for i in range(len(ax.patches) - 1, -1, -1):
                    ax.patches[i].remove()
            radius = 2 * max(abs(x-center[0]), abs(y-center[1]))
            sq = self.getRectanglePatch(center, radius, radius)
            ax.add_patch(sq)
            self.resultCanvas.draw_idle()

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

        # self.rrangeSettingFrame.setHidden(False)
        # self.rrangeSettingFrame.setEnabled(choice != 'None')

        self.tophat1SpnBx.setHidden(not choice == 'White-top-hats')
        self.tophat1Label.setHidden(not choice == 'White-top-hats')
        self.windowSizeLabel.setHidden(not choice == 'Roving Window')
        self.winSizeX.setHidden(not choice == 'Roving Window')
        self.winSizeY.setHidden(not choice == 'Roving Window')
        self.windowSepLabel.setHidden(not choice == 'Roving Window')
        self.winSepX.setHidden(not choice == 'Roving Window')
        self.winSepY.setHidden(not choice == 'Roving Window')
        self.maxPixRange.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.minPixRange.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.pixRangeLabel.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.gaussFWHMLabel.setHidden(not choice == 'Smoothed-Gaussian')
        self.gaussFWHM.setHidden(not choice == 'Smoothed-Gaussian')
        self.boxcarLabel.setHidden(not choice == 'Smoothed-BoxCar')
        self.boxcarX.setHidden(not choice == 'Smoothed-BoxCar')
        self.boxcarY.setHidden(not choice == 'Smoothed-BoxCar')
        self.deg1Label.setHidden(not choice == '2D Convexhull')
        self.deg1CB.setHidden(not choice == '2D Convexhull')
        self.cycleLabel.setHidden(not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        self.cycle.setHidden(not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        self.thetaBinLabel.setHidden(True)
        self.thetabinCB.setHidden(True)

        self.radialBinSpnBx.setHidden(not choice == 'Circularly-symmetric')
        self.radialBinLabel.setHidden(not choice == 'Circularly-symmetric')
        self.smoothLabel.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.smoothSpnBx.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.tensionLabel.setHidden(not choice in ('Roving Window'))
        self.tensionSpnBx.setHidden(not choice in ('Roving Window'))


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

        self.deleteInfo(['bgsubimg']) # delete result_img to make QuadrantFolder reproduce background subtracted image
        self.deleteInfo(['result_bg'])
        self.deleteImgCache(['BgSubFold'])

        self.resultDisplayModeCB.setEnabled(False)

        self.processImage()

        if hasattr(self, "processingModeCB") and self.processingModeCB.currentText() == "Automated" and not checked:
            self.optimizeFlag = False



    # NOTE: updateCenterModeIndicator and updateRotationModeIndicator removed
    # These are now handled by ImageSettingsPanel.update_display()

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
        self.deleteInfo(['avg_fold'])
        self.processImage()

    def removeIgnoreQuadrant(self):
        """
        Trigger when a quadrant is unignored
        """
        fold_number = self.quadFold.getFoldNumber(self.function[1][0], self.function[1][1])
        self.function = None
        self.display_points = None
        self.ignoreFolds.remove(fold_number)
        self.deleteInfo(['avg_fold'])
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
        if "bgsub" in info:
            self.bgChoiceIn.setCurrentIndex(self.allBGChoices.index(info['bgsub']))
            if info['bgsub'] != 'None':
                self.tophat1SpnBx.setValue(int(info.get('tophat', self.tophat1SpnBx.value())))
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
                self.deg1CB.setCurrentIndex(1)

            if 'optimize' in info:
                self.optimizeFlag = bool(info['optimize'])
            if 'equator_mask_height' in info:
                self.equatorMaskHeightSpnBx.setValue(int(info['equator_mask_height']))
            if 'equator_center_beam_width' in info:
                self.equatorCenterBeamSpnBx.setValue(int(info['equator_center_beam_width']))
            if 'm1' in info:
                self.m1SpnBx.setValue(int(info['m1']))
            if 'layer_line_width' in info:
                self.layerLineWidthSpnBx.setValue(int(info['layer_line_width']))
            if 'steps' in info and isinstance(info['steps'], (list, tuple)):
                self.stepsLineEdit.setText(", ".join([str(v) for v in info['steps']]))
            if 'max_iterations' in info:
                self.maxIterationsSpnBx.setValue(int(info['max_iterations']))
            if 'early_stop' in info:
                self.earlyStopSpnBx.setValue(float(info['early_stop']))
            if 'methods' in info and isinstance(info['methods'], (list, tuple)):
                self._set_selected_optimization_methods(info['methods'])

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
            self.evaluationBaselineSpnBx.setValue(float(info.get('evaluation_baseline',  self.evaluationBaselineSpnBx.value())))
        if 'amp' in info:
            self.ampSpnBx.setValue(float(info.get('amp', self.ampSpnBx.value())))
        if 'sigma_x_div' in info:
            self.sigmaXDivSpnBx.setValue(float(info.get('sigma_x_div', self.sigmaXDivSpnBx.value())))
        if 'sigma_y_div' in info:
            self.sigmaYDivSpnBx.setValue(float(info.get('sigma_y_div', self.sigmaYDivSpnBx.value())))
        if 'freq' in info:
            freq_value = str(info.get('freq', self.freqCB.currentText()))
            idx = self.freqCB.findText(freq_value)
            if idx >= 0:
                self.freqCB.setCurrentIndex(idx)


        # Range is already set to allow any value at spinbox creation
        if not self.resPersistIntensity.isChecked():
            self.spResultmaxInt.setValue(max_val * .1)
            self.spResultminInt.setValue(min_val)
        self.spResultmaxInt.setSingleStep(max_val * .05)
        self.spResultminInt.setSingleStep(max_val * .05)

        if 'rotate' in info:
            self.rotate90Chkbx.setChecked(info['rotate'])

        self.uiUpdating = False



    def onFoldChkBoxToggled(self):
        if self.quadFold is not None:
            self.quadFold.deleteFromDict(self.quadFold.info, 'avg_fold')
            # self.quadFold.info['avg_fold'] = self.quadFold.orig_img
            # self.quadFold.deleteFromDict(self.quadFold.imgCache, 'resultImg')
            self.quadFold.deleteFromDict(self.quadFold.imgCache, 'BgSubFold')
            self.processImage()


    def closeEvent(self, ev):
        """
        Close the event
        """
        # ✅ Save settings before closing
        if hasattr(self, 'workspace'):
            self.workspace.save_settings()
        
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
                img = self.quadFold.imCache.get('resultFolded', None)
                
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
                    syn = self.quadFold.info.get('synthetic_data', None)
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
                mask = self.quadFold.info['mask']
                mask = mask.astype(bool)
                if mask is not None:
                    ax.imshow(mask, cmap="Greys", alpha=0.2, interpolation="nearest")
            
            if display_mode == "Synthetic Mask" and show_synthetic_mask:
                synthetic_mask = self.quadFold.info.get('synthetic_mask', None)
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

            # self.toggleCircleTransition()
            self.toggleCircleRminRmax()

            self.updated['result'] = True
        finally:
            self.uiUpdating = False


    def showProcessingFinishedMessage(self):
        msgBox = QMessageBox()
        msgBox.setIcon(QMessageBox.Information)
        msgBox.setWindowTitle("Processing Complete")
        msgBox.setText("Folder finished processing")
        msgBox.setInformativeText("Do you want to exit the application or just close this message?")

        # Add buttons
        exitButton = msgBox.addButton("Exit", QMessageBox.ActionRole)
        closeButton = msgBox.addButton("Close", QMessageBox.ActionRole)

        # (Optional) Set a fixed width if you like
        # msgBox.setFixedWidth(300)

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
        Then, write data and update UI
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
        if quad_fold.info.get('stopped'):
            return

        self.updateParams()
        self.refreshAllTabs()
        if self.csvManager is not None:
            self.csvManager.writeNewData(self.quadFold)

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

        self.saveResults()
        print("Single processing complete")

        if self._restoreOptimizeCheckboxAfterProcess:
            self._restoreOptimizeCheckboxAfterProcess = False
            self.optimizeFlag = False


    @Slot(object)
    def _on_single_processing_error(self, error_payload):
        if self._restoreOptimizeCheckboxAfterProcess:
            self._restoreOptimizeCheckboxAfterProcess = False
            self.optimizeFlag = False
            
        errMsg = QMessageBox(self)
        errMsg.setText('Unexpected error')
        msg = 'Please report the problem with error message below and the input image\n\n'
        image_name = ''
        tb = ''
        if isinstance(error_payload, dict):
            image_name = str(error_payload.get('image_name', '') or '')
            tb = str(error_payload.get('traceback', '') or '')
        else:
            tb = str(error_payload)

        msg += "Image : " + image_name
        msg += "\n\n" + tb
        errMsg.setInformativeText(msg)
        errMsg.setStandardButtons(QMessageBox.Ok)
        errMsg.setIcon(QMessageBox.Warning)
        errMsg.setFixedWidth(500)
        errMsg.exec_()

    @Slot()
    def _on_single_processing_finished(self):
        self._singleProcessing = False
        self._singleWorker = None

        # self.resultDisplayModeCB.setEnabled(True)

        if self._OptimizationRunning:
            self._set_optimization_button_running(False)
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


    def addTask(self, i):
        params = QuadFoldParams(self.getFlags(), i, self.file_manager, self)

        self.tasksQueue.put(params)

        # If there's no task currently running, start the next task
        self.startNextTask()

    def thread_done(self, quadFold):

        if self.lock is not None:
            self.lock.acquire()

        self.quadFold = quadFold

        # In batch processing mode, skip UI updates for each task
        # Only update UI when processing the last task
        if not self.batchProcessing:
            self.onProcessingFinished()
        else:
            # In batch mode, write CSV data and save results without UI refresh
            self.csvManager.writeNewData(self.quadFold)
            self.saveResults()  # Save result images to qf_results/

        if self.lock is not None:
            self.lock.release()

    def thread_finished(self):

        self.tasksDone += 1
        # Ensure progress bar is in percentage mode (0-100 range)
        if self.progressBar.maximum() != 100:
            self.progressBar.setRange(0, 100)
            self.progressBar.setFormat("%p%")
        self.progressBar.setValue(int(100. / self.totalFiles * self.tasksDone))
        
        if not self.tasksQueue.empty():
            self.startNextTask()
        else:
            if self.threadPool.activeThreadCount() == 0 and self.tasksDone == self.totalFiles:
                print("All threads are complete")
                self.batchProcessing = False  # Disable batch processing mode
                if hasattr(self, "bgSubDialog"):
                    self.bgSubDialog.backgroundConfigurations = []
                self.progressBar.setVisible(False)
                self.navControls.filenameLineEdit.setEnabled(True)
                
                # Reset the button state - temporarily disconnect signal to avoid triggering clearTasks()
                self.navControls.processFolderButton.toggled.disconnect(self.batchProcBtnToggled)
                self.navControls.processFolderButton.setChecked(False)
                self.navControls.processFolderButton.setText("Process Current Folder")
                self.navControls.processFolderButton.toggled.connect(self.batchProcBtnToggled)
                
                self.csvManager.sortCSV()
                os.makedirs(join(self.filePath, 'qf_results'), exist_ok=True) #Makes qf_results folder if it doesn't already exist.
                os.makedirs(join(self.filePath, 'qf_results/bg'), exist_ok=True) #Makes bg subfolder if it doesn't already exist.
                with open(join(self.filePath, 'qf_results/bg/background_sum.csv'), 'w', newline='') as csvfile:
                    writer = csv.writer(csvfile)

                    writer.writerow(['Name', 'Sum'])

                    for name, sum in self.bgAsyncDict.items():
                        writer.writerow([name, sum])

                # Note: UI is NOT refreshed after batch processing
                # The displayed image remains the same as before batch processing started
                # If you want to display a specific image after completion, you can manually navigate to it
                
                self.showProcessingFinishedMessage()

    def startNextTask(self):
        self.progressBar.setVisible(True)
        self.navControls.filenameLineEdit.setEnabled(False)
        bg_csv_lock = Lock()
        worker_limit = max(1, min(self.maxConcurrentProcesses, self.threadPool.maxThreadCount()))
        
        while not self.tasksQueue.empty() and self.threadPool.activeThreadCount() < worker_limit:
            params = self.tasksQueue.get()
            self.currentTask = FolderImageWorker(params, 
                                      self.workspace._center_settings, 
                                      self.workspace._rotation_settings,
                                      self.bgChoiceIn.currentText(),
                                      bgDict=self.bgAsyncDict, bg_lock=bg_csv_lock)
            self.currentTask.signals.result.connect(self.thread_done)
            self.currentTask.signals.finished.connect(self.thread_finished)

            self.threadPool.start(self.currentTask)


    def onProcessingFinished(self):

        self.updateParams()
        self.refreshAllTabs()
        self.resetStatusbar2()
        self.csvManager.writeNewData(self.quadFold)
        self.saveResults()

        self.currentTask = None



    def saveResults(self):
        """
        Save result to folder qf_results
        """
        print("SAVE RESULTS")
        if 'resultImg' in self.quadFold.imgCache:
            result_path = fullPath(self.filePath, 'qf_results')
            createFolder(result_path)

            result_file = str(join(result_path, self.quadFold.img_name))
            result_file, _ = splitext(result_file)
            img = self.quadFold.imgCache['resultImg']
            img = img.astype("float32")

            # metadata = json.dumps([True, self.quadFold.initImg.shape])
            if self.cropFoldedImageChkBx.isChecked():
                print("Cropping folded image ")
                ylim, xlim = self.quadFold.initImg.shape
                xlim, ylim = int(xlim / 2), int(ylim / 2)
                cx,cy = self.quadFold.center
                xl,xh = (cx - xlim, cx + xlim)
                yl,yh = (cy - ylim, cy + ylim)
                print("Before cropping ", img.shape)
                img = img[max(yl,0):yh, max(xl,0):xh]
                print("After cropping, ", img.shape)
                result_file += '_folded_cropped.tif'
                if self.compressFoldedImageChkBx.isChecked():
                    result_file += '_folded_cropped_compressed.tif'
                    tif_img = Image.fromarray(img)
                    tif_img.save(result_file, compression='tiff_lzw')
                else:
                    result_file += '_folded_cropped.tif'
                    fabio.tifimage.tifimage(data=img).write(result_file)
            else:
                try:
                    if self.compressFoldedImageChkBx.isChecked():
                        result_file += '_folded_compressed.tif'
                        tif_img = Image.fromarray(img)
                        tif_img.save(result_file, compression='tiff_lzw')
                    else:
                        result_file += '_folded.tif'
                        fabio.tifimage.tifimage(data=img).write(result_file)
                except Exception as e:
                    print("Error saving image", e)
            self.saveBackground()

    def saveBackground(self):
        """
        Save the background in the bg folder
        """
        info = self.quadFold.info
        result = self.quadFold.imgCache["BgSubFold"]
        avg_fold = info["avg_fold"]
        print("Avg_fold shape:")
        print(avg_fold.shape)
        print("result shape: ")
        print(result.shape)
        background = avg_fold-result
        resultImg = makeFullImage(background)

        if 'rotate' in info and info['rotate']:
            resultImg = np.rot90(resultImg)

        method = info['bgsub']
        print(method)
        if method != 'None':
            
            filename = self.file_manager.current_image_name
            bg_path = fullPath(self.filePath, os.path.join("qf_results", "bg"))
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

    def updateParams(self):
        """
        Update the parameters
        """
        try:
            info = self.quadFold.info
            if 'orientation_model' in info:
                # Update Panel's orientation model from cached info
                self.workspace._orientation_model = info['orientation_model']
            if not self.zoomOutClicked and self.quadFold.initImg is not None:
                _, center = self.getExtentAndCenter()
                print(center)
                cx, cy = center
                cxr, cyr = self.quadFold.center
                print(self.quadFold.initImg)
                xlim, ylim = self.quadFold.initImg.shape
                xlim, ylim = int(xlim/2), int(ylim/2)
                self.default_img_zoom = [(cx-xlim, cx+xlim), (cy-ylim, cy+ylim)]
                self.default_result_img_zoom = [(cxr-xlim, cxr+xlim), (cyr-ylim, cyr+ylim)]
        except:
            print("EXCEPTION IN UPDATE PARAMS")
        else:
            print("UPDATE PARAMS SUCCESS")

        self._update_bg_method_summary()
        self.bgSubDialog._update_bg_metrics_table()
        

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

        # Check if empty cell image settings exist and is enabled
        settings_dir = Path(self.filePath) / "settings"
        blank_config_path = settings_dir / "blank_image_settings.json"
        blank_disabled_flag = settings_dir / ".blank_image_disabled"
        flags['blank_mask'] = blank_config_path.exists() and not blank_disabled_flag.exists()
        
        # Check if mask settings exist and is enabled
        mask_file_path = settings_dir / "mask.tif"
        mask_disabled_flag = settings_dir / ".mask_disabled"
        flags['apply_mask'] = mask_file_path.exists() and not mask_disabled_flag.exists()
        
        flags['fold_image'] = self.toggleFoldImage.isChecked()

        # ===== Background removal flags =====
        flags['bgsub'] = self.bgChoiceIn.currentText()
        flags['downsample'] = int(self.downsampleCB.currentText())
        flags["cirmin"] = self.minPixRange.value()
        flags["cirmax"] = self.maxPixRange.value()
        flags['win_size_x'] = self.winSizeX.value()
        flags['win_size_y'] = self.winSizeY.value()
        flags['win_sep_x'] = self.winSepX.value()
        flags['win_sep_y'] = self.winSepY.value()
        flags["bin_theta"] = int(self.thetabinCB.currentText())
        flags['radial_bin'] = self.radialBinSpnBx.value()
        flags['smooth'] = self.smoothSpnBx.value()
        flags['tension'] = self.tensionSpnBx.value()
        flags["tophat"] = self.tophat1SpnBx.value()
        flags['fwhm'] = self.gaussFWHM.value()
        flags['boxcar_x'] = self.boxcarX.value()
        flags['boxcar_y'] = self.boxcarY.value()
        flags['cycles'] = self.cycle.value()
        flags['degree'] = float(self.deg1CB.currentText())

        # ===== Background optimization flags (automated processing) =====
        flags['optimize'] = self.optimizeFlag is True
        flags['methods'] = self._get_selected_optimization_methods()
        flags['steps'] = self._parse_optimization_steps()
        flags['max_iterations'] = self.maxIterationsSpnBx.value()
        flags['early_stop'] = self.earlyStopSpnBx.value()
        flags['mean_metric_values'] = {
            'MSE_SYN_MEAN': float(self.meanMSESpnBx.value()),
            'SHARE_NEG_SYN_MEAN': _fraction_to_percent_for_ui(self.meanNegSynSpnBx.value()),
            'SHARE_NON_BASELINE_MEAN': _fraction_to_percent_for_ui(self.meanNonBaselineSpnBx.value()),
            'SHARE_NEG_CON_MEAN': _fraction_to_percent_for_ui(self.meanNegConSpnBx.value()),
            'SMOOTH_MEAN': float(self.meanSmoothSpnBx.value()),
        }
        flags['evaluation_baseline'] = float(self.evaluationBaselineSpnBx.value())
        flags['amp'] = float(self.ampSpnBx.value())
        flags['sigma_x_div'] = float(self.sigmaXDivSpnBx.value())
        flags['sigma_y_div'] = float(self.sigmaYDivSpnBx.value())
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

        # Auto-select from saved user background configurations (used mainly in batch processing)
        flags['choose_configurations_auto'] = self.chooseConfigurationsAutoChkBx.isChecked()

        # TODO: add create new configurations for outliers

        bg_configs = self.bgSubDialog.backgroundConfigurations if hasattr(self, "bgSubDialog") else []
        flags['background_configurations'] = bg_configs if isinstance(bg_configs, list) else []

        if self.batchProcessing:
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
        if self.rmaxSpnBx.value() > 0:
            flags['fixed_rmax'] = self.rmaxSpnBx.value()

        flags['rotate'] = self.rotate90Chkbx.isChecked()

        if self.calSettings is not None and 'detector' in self.calSettings:
            flags['detector'] = self.calSettings['detector']

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
            default_methods = getattr(self.bgSubDialog, "DEFAULT_OPTIMIZATION_METHODS", [])
            return list(default_methods) if default_methods else []

        return selected


    def _parse_optimization_steps(self):
        """Parse optimization step sizes from UI text."""
        default_steps = [50, 30, 10, 7, 5, 3, 1]
        raw = self.stepsLineEdit.text().replace(';', ',').strip()
        if not raw:
            return default_steps

        steps = []
        for part in raw.split(','):
            val = part.strip()
            if not val:
                continue
            try:
                num = float(val)
                if num > 0:
                    if abs(num - int(num)) < 1e-9:
                        steps.append(int(num))
                    else:
                        steps.append(num)
            except ValueError:
                continue
        return steps if len(steps) > 0 else default_steps

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
        Stop scheduling new tasks, clear queued tasks, and reset UI state.
        Running tasks will be allowed to finish.
        """
        # Prevent any further enqueuing/scheduling
        self.stop_process = True

        # Clear any runnables that have been queued to the pool but not yet started
        if self.threadPool is not None:
            try:
                self.threadPool.clear()
            except Exception:
                pass

        # Drain our local queue of params that haven't been submitted yet
        try:
            while not self.tasksQueue.empty():
                self.tasksQueue.get_nowait()
        except Exception:
            pass

        # Reset UI elements
        self.progressBar.setVisible(False)
        self.navControls.filenameLineEdit.setEnabled(True)

        # Restore button texts and toggle off
        try:
            self.navControls.processFolderButton.setText("Process Current Folder")
            self.navControls.processFolderButton.setChecked(False)
        except Exception:
            pass
        try:
            self.navControls.processH5Button.setText("Process Current H5 File")
            self.navControls.processH5Button.setChecked(False)
        except Exception:
            pass

        # Do not keep a reference to a current task that may be finishing
        # It will still signal finished; we just won't schedule more
        self.currentTask = None
        self._batch_background_configurations = []
        
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

        # ======= Manual assignments and auto selection both depend on saved configurations. ======
        has_manual_assignments = (
            hasattr(self, "bgSubDialog")
            and isinstance(self.bgSubDialog.manualBackgroundAssignments, dict)
            and len(self.bgSubDialog.manualBackgroundAssignments) > 0
        )
        resolved_manual_assignments = {}

        if self.chooseConfigurationsAutoChkBx.isChecked() or has_manual_assignments:
            self._background_configurations = self.bgSubDialog._read_background_configurations()
            if len(self._background_configurations) == 0:
                QMessageBox.warning(
                    self,
                    "No Saved Background Configurations",
                    "Batch processing requires saved background configurations, but none were found in the cache for this folder/context. Batch processing was stopped."
                )
                self.navControls.processFolderButton.setChecked(False)
                self.navControls.processH5Button.setChecked(False)
                return

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
        else:
            self._background_configurations = []
            resolved_manual_assignments = {}

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
            blank_config_path = Path(self.filePath) / "settings" / "blank_image_settings.json"
            try:
                import json
                with open(blank_config_path, "r") as f:
                    blank_config = json.load(f)
                blank_file = Path(blank_config.get("file_path", "")).name
                blank_weight = blank_config.get("weight", 1.0)
                text += f"\n  - Empty Cell Image : {blank_file} (weight: {blank_weight})"
            except:
                text += "\n  - Empty Cell Image : Enabled"

    
        if self.chooseConfigurationsAutoChkBx.isChecked():
            text += "\n  - Auto Configuration Selection : Enabled"
            text += f"\n  - Saved Configurations Loaded : {len(self._background_configurations)}"
        if len(resolved_manual_assignments) > 0:
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
            self.totalFiles = len(img_ids)
            self.tasksDone = 0
            for idx, i in enumerate(img_ids):
                if self.stop_process:
                    break
                self.addTask(i)
                # Process UI events periodically to keep Stop button responsive
                if idx % 10 == 0:
                    QApplication.processEvents()

            # self.progressBar.setVisible(False)
            # Note: Don't setChecked(False) here - it will trigger clearTasks() and clear the queue!
            # The button will be reset in thread_finished() when all tasks complete
        else:
            # User cancelled the dialog, reset button
            self.navControls.processFolderButton.setChecked(False)


    def processH5File(self):
        """
        Triggered when a folder with multiple H5 files has been selected to process it
        """
        start_idx, end_idx = self.file_manager.get_current_h5_range()
        self._process_image_list(range(start_idx, end_idx + 1), text="Process Current H5 File")


    def saveSettings(self):
        """
        save settings to json
        """
        settings = self.calSettings
        if self.quadFold is not None:
            if settings is None:
                settings = {}
            settings['compressed'] = self.compressFoldedImageChkBx.isChecked()
        # if self.quadFold is not None and 'fixed_roi_rad' in self.quadFold.info:
        #     settings['fixed_roi_rad'] = self.quadFold.info['fixed_roi_rad']
        if self.quadFold is not None and 'bgsub' in self.quadFold.info:
            settings['bgsub'] = self.quadFold.info['bgsub']
        filename = getSaveFile(os.path.join("musclex", "settings", "qfsettings.json"), None)
        if filename != "":
            with open(filename, 'w') as f:
                json.dump(settings, f)



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
                    self.csvManager = QF_CSVManager(self.filePath)
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
        self.quadFold = QuadrantFolder(self.current_image_data, self)
        
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
        
        # Restore cache state if available
        if hasattr(self.quadFold, 'info'):
            previnfo = self.quadFold.info
            
            # Restore ignore folds
            if 'ignore_folds' in self.quadFold.info:
                self.ignoreFolds = self.quadFold.info['ignore_folds']
            
            # Initialize widgets with previous info
            self.initialWidgets(original_image, previnfo)
            self.markFixedInfo(self.quadFold.info, previnfo)
            
            # Handle crop checkbox
            if 'saveCroppedImage' not in self.quadFold.info:
                self.quadFold.info['saveCroppedImage'] = self.cropFoldedImageChkBx.isChecked()
        
        # Update workspace display (includes blank/mask states)
        self.workspace.update_display(self.current_image_data)

        # Auto-adjust downsample based on image size
        self._set_downsample_from_image_size(original_image)

    def _set_downsample_from_image_size(self, image):
        """Set downsample to 1 when image is smaller than 1000px."""
        if image is None or not hasattr(self, 'downsampleCB'):
            return

        try:
            h, w = image.shape[:2]
            if max(h, w) < 1000:
                self.downsampleCB.setCurrentText("1")
            else:
                self.downsampleCB.setCurrentText("2")
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