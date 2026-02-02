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

import shutil
import sys
import copy
import traceback
import json
import os
from os.path import exists, splitext, join
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
from threading import Lock
import numpy as np
import cv2
from PySide6.QtCore import QRunnable, QThreadPool, QEventLoop, Signal
from queue import Queue
import fabio
import tifffile
from musclex import __version__
from ..utils.misc_utils import inverseNmFromCenter
from ..utils.file_manager import fullPath, getImgFiles, createFolder
from ..utils.image_processor import getPerpendicularLineHomogenous, calcSlope, getIntersectionOfTwoLines, getBGR, get8bitImage, getNewZoom, getCenter, rotateImageAboutPoint, rotatePoint, processImageForIntCenter, getMaskThreshold
from ..utils.image_data import ImageData
from ..modules.ProjectionProcessor import ProjectionProcessor, ProcessingBox
from ..ui.ProjectionBoxTab import ProjectionBoxTab
from ..CalibrationSettings import CalibrationSettings
from ..csv_manager import PT_CSVManager
from .ImageMaskTool import ImageMaskerWindow
from .DoubleZoomGUI import DoubleZoom
from .pyqt_utils import *
from .base_gui import BaseGUI
from .widgets import ProcessingWorkspace
from .widgets.collapsible_groupbox import CollapsibleGroupBox
from .tools.placeholder_tool import PlaceholderTool

class ProjectionParams:
    def __init__(self, settings, index, file_manager, gui):
        """
        Parameters for Worker thread (batch processing).
        
        Args:
            settings: Processing settings dict (for display/legacy purposes)
            index: Image index in file_manager.names
            file_manager: FileManager instance
            gui: Parent GUI instance (for accessing ProcessingWorkspace and state)
        """
        self.settings = settings  # For display in confirmation dialogs
        self.index = index
        self.file_manager = file_manager
        self.gui = gui

class WorkerSignals(QObject):
    
    finished = Signal()
    error = Signal(tuple)
    result = Signal(object)


class Worker(QRunnable):

    def __init__(self, params=None, projProc=None, settings=None):
        super().__init__()
        self.settings = settings if settings is not None else (params.settings if params else None)
        self.params = params
        self.projProc = projProc
        self.signals = WorkerSignals()
    
    @classmethod
    def fromProjProc(cls, projProc, settings):
        return cls(projProc=projProc, settings=settings)
    
    @classmethod
    def fromParams(cls, params):
        return cls(params=params)
        
    @Slot()
    def run(self):
        try:
            if self.projProc is None and self.params:
                # Load image from FileManager
                img = self.params.file_manager.get_image_by_index(self.params.index)
                filename = self.params.file_manager.names[self.params.index]
                
                # Create ImageData using factory method
                from ..utils.image_data import ImageData
                image_data = ImageData.from_settings_panel(
                    img, 
                    self.params.file_manager.dir_path, 
                    filename,
                    self.params.gui.workspace
                )
                
                # Create ProjectionProcessor with ImageData
                self.projProc = ProjectionProcessor(image_data)
            
            # Apply settings directly to state
            if self.settings:
                if 'mask_thres' in self.settings:
                    self.projProc.state.mask_thres = self.settings['mask_thres']
                if 'lambda_sdd' in self.settings:
                    self.projProc.state.lambda_sdd = self.settings['lambda_sdd']
            
            self.projProc.process()
        except:
            traceback.print_exc()
            self.signals.error.emit((traceback.format_exc()))
            infMsg = QMessageBox()
            img_name = self.params.file_manager.names[self.params.index] if self.params else "unknown"
            infMsg.setText("Error trying to open " + str(img_name))
            infMsg.setInformativeText("This usually means that the image is corrupted or missing.  Skipping this image")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
        else:
            self.signals.result.emit(self.projProc)
        finally:
            self.signals.finished.emit()

class BoxDetails(QDialog):
    """
    This class is for Popup window when a box is added
    """
    def __init__(self, current_box_names, oriented=False):
        super().__init__(None)
        self.setWindowTitle("Adding a Box")
        self.box_names = current_box_names
        self.oriented = oriented
        self.initUI()

    def initUI(self):
        """
        Initial UI for the dialog
        """
        self.boxLayout = QGridLayout(self)
        self.boxName = QLineEdit()
        self.bgChoice = QComboBox()
        self.bgChoice.addItem("Fitting Gaussians")
        self.bgChoice.addItem("Convex Hull")
        self.bgChoice.addItem("No Background")
        if not self.oriented:
            self.axisChoice = QComboBox()
            self.axisChoice.addItem("Horizontal")
            self.axisChoice.addItem("Vertical")

        self.bottons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel,
                                              Qt.Horizontal, self)
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)
        self.bottons.setFixedWidth(200)

        self.boxLayout.addWidget(QLabel("Box name : "), 0, 0, 1, 1)
        self.boxLayout.addWidget(self.boxName, 0, 1, 1, 1)
        self.boxLayout.addWidget(QLabel("Background Subtraction Method : "), 1, 0, 1, 1)
        self.boxLayout.addWidget(self.bgChoice, 1, 1, 1, 1)
        if not self.oriented:
            self.boxLayout.addWidget(QLabel("Axis of Projection : "), 2, 0, 1, 1)
            self.boxLayout.addWidget(self.axisChoice, 2, 1, 1, 1)
            self.boxLayout.addWidget(self.bottons, 3, 0, 1, 2, Qt.AlignCenter)
        else:
            self.boxLayout.addWidget(self.bottons, 2, 0, 1, 2, Qt.AlignCenter)

    def okClicked(self):
        """
        Triggered when OK is clicked
        """
        box_name = str(self.boxName.text())
        if len(box_name) == 0:
            errMsg = QMessageBox()
            errMsg.setText('Adding a box Error')
            errMsg.setInformativeText('Please specify the box name')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setInformativeText(box_name+' has already been added. Please select another name')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
        else:
            self.accept()

    def getDetails(self):
        """
        Give details about the current status
        """
        if self.oriented:
            return str(self.boxName.text()), self.bgChoice.currentIndex(), None
        else:
            return str(self.boxName.text()), self.bgChoice.currentIndex(), self.axisChoice.currentIndex()
        
class EditBoxDetails(QDialog):
    
    def __init__(self, boxes):
        """
        Args:
            boxes: Dict[str, ProcessingBox] - box objects
        """
        super().__init__(None)
        self.boxes = boxes
        self.setWindowTitle("Edit a Box")
        print(boxes)
        self.initUI()
        
    def initUI(self):
        self.boxLayout = QGridLayout(self)
        self.boxNames = QComboBox()
        for key in self.boxes.keys():
            self.boxNames.addItem(key)
            
        self.box_height = QDoubleSpinBox()
        self.box_height.setDecimals(2)
        self.box_height.setMinimum(0)
        self.box_height.setMaximum(10000)
        self.box_width = QDoubleSpinBox()
        self.box_width.setDecimals(2)
        self.box_width.setMinimum(0)
        self.box_width.setMaximum(10000)
        
        self.boxWidthLabel = QLabel("Box Width Mode : ")
        self.boxWidthMode = QComboBox()
        width_modes = ['Center', 'Left', 'Right']
        self.boxWidthMode.addItems(width_modes)
        self.boxHeightLabel = QLabel("Box Height Mode : ")
        self.boxHeightMode = QComboBox()
        height_modes = ['Center', 'Top', 'Bottom']
        self.boxHeightMode.addItems(height_modes)
        
        self.bottons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel,
                                              Qt.Horizontal, self)
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)
        self.bottons.setFixedWidth(200)
        
        self.boxLayout.addWidget(QLabel("Box name : "), 0, 0, 1, 1)
        self.boxLayout.addWidget(self.boxNames, 0, 1, 1, 1)
        self.boxLayout.addWidget(QLabel("Box Height : "), 1, 0, 1, 1)
        self.boxLayout.addWidget(self.box_height, 1, 1, 1, 1)
        self.boxLayout.addWidget(self.boxHeightLabel, 2, 0, 1, 1)
        self.boxLayout.addWidget(self.boxHeightMode, 2, 1, 1, 1)
        self.boxLayout.addWidget(QLabel("Box Width : "), 3, 0, 1, 1)
        self.boxLayout.addWidget(self.box_width, 3, 1, 1, 1)
        self.boxLayout.addWidget(self.boxWidthLabel, 4, 0, 1, 1)
        self.boxLayout.addWidget(self.boxWidthMode, 4, 1, 1, 1)
        self.boxLayout.addWidget(self.bottons, 5, 0, 1, 2, Qt.AlignCenter)
        
        self.boxNames.currentIndexChanged.connect(self.updateBoxInfo)
        
        self.updateBoxInfo()
        
    def updateBoxInfo(self):
        box_name = str(self.boxNames.currentText())
        box_obj = self.boxes[box_name]
        box = box_obj.coordinates
        
        if box_obj.type == 'oriented':
            self.box_height.setValue(box[4])
            self.box_width.setValue(box[3])
            
            # self.boxWidthLabel.setVisible(False)
            # self.boxWidthMode.setVisible(False)
            # self.boxHeightLabel.setVisible(False)
            # self.boxHeightMode.setVisible(False)
            
        else:
            x1,x2 = box[0]
            width = abs(x1 - x2)
            y1, y2 = box[1]
            height = abs(y1 - y2)
            print(height, width)
            self.box_height.setValue(height)
            self.box_width.setValue(width)
                
                # self.boxWidthLabel.setVisible(True)
                # self.boxWidthMode.setVisible(True)
                # self.boxHeightLabel.setVisible(True)
                # self.boxHeightMode.setVisible(True)
                
        self.boxHeightMode.setCurrentIndex(0)
        self.boxWidthMode.setCurrentIndex(0)
        
        
    def okClicked(self):
        """
        Triggered when OK is clicked
        """
        self.current_box = str(self.boxNames.currentText())
        self.new_height = self.box_height.value()
        self.new_width = self.box_width.value()
        self.height_mode = self.boxHeightMode.currentText()
        self.width_mode = self.boxWidthMode.currentText()
        self.accept()
        

class ProjectionTracesGUI(BaseGUI):
    """
    This class is for Projection Traces GUI Object
    """
    def __init__(self):
        """
        Initial window
        """
        
        super().__init__()
        # Note: self.file_manager is now initialized by workspace.file_manager in _create_tabs()
        self.filePath = "" # current directory
        self.current_file = 0
        self.dir_path = ""
        self.update_plot = {'img':True}
        self.stop_process = False
        self.projProc = None
        self.syncUI = False
        self.csvManager = None
        self.masked = False
        self.img_zoom = None
        self.function = None # current active function
        
        # Unified data structure: all box data in ProcessingBox objects
        self.boxes = {}  # Dict[str, ProcessingBox]
        self.boxes_on_img = {}  # Visual representations on matplotlib
        
        self.centerx = None
        self.centery = None
        # Note: center_func, rotated, rotationAngle removed - rotation state now managed by ImageData
        self.numberOfFiles = 0
        self.refit = False
        self.checkableButtons = [] # list of checkable buttons
        
        self.chordLines = []
        self.chordpoints = []
        
        self.threadPool = QThreadPool()
        self.tasksQueue = Queue()
        self.loop = QEventLoop()
        self.currentTask = None
        self.worker = None 
        self.tasksDone = 0
        self.totalFiles = 1
        self.lock = Lock()
        
        self.initUI() # initial all GUI
        
        self.setConnections() # set triggered function for widgets
        self.resize(1200, 900)
        
        self.doubleZoomGUI = DoubleZoom(self.displayImgFigure)

    # ===== BaseGUI abstract methods implementation =====
    
    def _setup_window(self):
        """Set window title"""
        from musclex import __version__
        self.setWindowTitle("Muscle X Projection Traces v." + __version__)
    
    def _tabs_closable(self) -> bool:
        """PT allows closing tabs"""
        return True
    
    def _tab_stylesheet(self) -> str:
        """PT uses smaller tabs"""
        return "QTabBar::tab { height: 20px; width: 200px; }"
    
    def _create_tabs(self):
        """Create image tab and box tabs"""

        self.imageTab = QWidget()
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.tabWidget.addTab(self.imageTab, "Image")
        # Make first tab not closable
        self.tabWidget.tabBar().setTabButton(0, QTabBar.LeftSide, None)
        self.tabWidget.tabBar().setTabButton(0, QTabBar.RightSide, None)

        self.workspace = ProcessingWorkspace(
            settings_dir=self.filePath
        )
        self.imageTabLayout.addWidget(self.workspace, 1)

        # Expose navigator as standard attribute for BaseGUI
        self.navigator = self.workspace.navigator

        # Expose components for backward compatibility (following QF pattern)
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
            self.minIntSpnBx = self.image_viewer.display_panel.minIntSpnBx
            self.maxIntSpnBx = self.image_viewer.display_panel.maxIntSpnBx
            self.logScaleIntChkBx = self.image_viewer.display_panel.logScaleChkBx
            self.persistIntensity = self.image_viewer.display_panel.persistChkBx
            self.minIntLabel = self.image_viewer.display_panel.minIntLabel
            self.maxIntLabel = self.image_viewer.display_panel.maxIntLabel
        
        # Add PT-specific display options (only the 3 unique checkboxes)
        self._add_display_options()
        
        # Setup right panel widgets in desired order
        self._setup_right_panel_widgets()
        
        # Add navigation controls to right panel bottom (following QF pattern)
        self.right_panel.add_bottom_widget(self.navControls)
    
    def _setup_right_panel_widgets(self):
        """
        Setup right panel widgets in desired order.
        
        This method controls the layout of all settings widgets in the right panel.
        Order:
        1. Display Panel (already added by workspace)
        2. Quadrant Folded checkbox (PT-specific)
        3. Box Settings (add boxes before center settings)
        4. Center Settings
        5. Rotation Settings
        6. Blank/Mask Settings
        7. Pattern Settings (mask threshold)
        8. Peaks Settings
        9. Export Settings
        """
        # 2. Quadrant Folded checkbox (PT-specific)
        qf_checkbox = self.workspace.create_qf_checkbox()
        self.workspace.right_panel.add_widget(qf_checkbox)
        
        # 3. Box Settings (placed after QF checkbox, before center settings)
        self._create_box_settings()
        
        # 4-6. Add built-in settings widgets
        self.workspace.right_panel.add_widget(self.workspace._center_widget)
        self.workspace.right_panel.add_widget(self.workspace._rotation_widget)
        self.workspace.right_panel.add_widget(self.workspace._blank_mask_widget)
        
        # 7-9. Add remaining PT-specific settings
        self._create_pattern_settings()
        self._create_peaks_settings()
        self._create_export_settings()
        
    def _create_pattern_settings(self):
        """Create pattern properties settings group (mask threshold only)"""

        # ===== PT-specific settings (mask threshold) =====
        # Note: QF checkbox is now added separately above center settings
        self.propGrp = QGroupBox("Pattern Settings (Optional)")
        self.propGrp.setEnabled(False)
        self.propLayout = QGridLayout(self.propGrp)
        
        # Mask threshold spinbox (PT-specific)
        self.maskThresSpnBx = QDoubleSpinBox()
        self.maskThresSpnBx.setMinimum(-10000)
        self.maskThresSpnBx.setMaximum(10000)
        self.maskThresSpnBx.setValue(-999)
        self.maskThresSpnBx.setKeyboardTracking(False)

        # Layout - only mask threshold now
        self.propLayout.addWidget(QLabel('Mask Threshold:'), 0, 0, 1, 2)
        self.propLayout.addWidget(self.maskThresSpnBx, 0, 2, 1, 2)
        
        # Add to right panel
        self.workspace.right_panel.add_widget(self.propGrp)
    
    def _create_box_settings(self):
        """Create box selection settings group"""
        # Box selection
        self.boxGrp = CollapsibleGroupBox("Add Boxes", start_expanded=True)
        self.boxGrp.setEnabled(False)
        self.boxesLayout = QVBoxLayout()
        self.addBoxButton = QPushButton("Add Axis Aligned Box")
        self.addBoxButton.setCheckable(True)
        self.addOrientedBoxButton = QPushButton("Add Oriented Box")
        self.addOrientedBoxButton.setCheckable(True)
        self.addCenterOrientedBoxButton = QPushButton("Add Centered Oriented Box")
        self.addCenterOrientedBoxButton.setCheckable(True)
        self.editBoxButton = QPushButton('Edit Boxes')
        self.clearBoxButton = QPushButton('Clear All Boxes')
        self.checkableButtons.append(self.addBoxButton)
        self.checkableButtons.append(self.addOrientedBoxButton)
        self.checkableButtons.append(self.addCenterOrientedBoxButton)
        self.boxesLayout.addWidget(self.addBoxButton)
        self.boxesLayout.addWidget(self.addOrientedBoxButton)
        self.boxesLayout.addWidget(self.addCenterOrientedBoxButton)
        self.boxesLayout.addWidget(self.editBoxButton)
        self.boxesLayout.addWidget(self.clearBoxButton)
        self.boxGrp.setLayout(self.boxesLayout)
        
        # Add to right panel
        self.workspace.right_panel.add_widget(self.boxGrp)
    
    def _create_peaks_settings(self):
        """Create peaks selection settings group"""
        # Peaks Selection
        self.selectPeaksGrp = QGroupBox("Peaks")
        self.selectPeaksGrp.setEnabled(False)
        self.selectPeaksLayout = QVBoxLayout(self.selectPeaksGrp)
        self.selectPeaksButton = QPushButton("Select Approximate Peak Locations")
        self.selectPeaksButton.setCheckable(True)
        self.checkableButtons.append(self.selectPeaksButton)
        self.selectPeaksLayout.addWidget(self.selectPeaksButton)
        
        # Add to right panel
        self.workspace.right_panel.add_widget(self.selectPeaksGrp)
        
    def _create_export_settings(self):
        """Create export settings group"""
        # Export Settings
        self.exportGrp = QGroupBox("Export Options")
        self.exportLayout = QVBoxLayout(self.exportGrp)
        self.exportChkBx = QCheckBox("Export All 1-D Projections")
        self.exportChkBx.setChecked(False)
        self.exportChkBx.setToolTip("Export original and background-subtracted histograms to text files")
        self.exportLayout.addWidget(self.exportChkBx)
        
        # Add to right panel
        self.workspace.right_panel.add_widget(self.exportGrp)
    
    def _add_display_options(self):
        """Add PT-specific display options (only the 3 unique checkboxes)"""
        # Add PT-specific checkboxes to display panel
        self.centerChkBx = QCheckBox("Center")
        self.centerChkBx.setChecked(False)
        
        self.boxesChkBx = QCheckBox("Boxes")
        self.boxesChkBx.setChecked(True)
        
        self.peaksChkBx = QCheckBox("Peaks")
        self.peaksChkBx.setChecked(True)
        
        # Add to display panel's top slot
        self.workspace.navigator.image_viewer.display_panel.add_to_top_slot(self.centerChkBx)
        self.workspace.navigator.image_viewer.display_panel.add_to_top_slot(self.boxesChkBx)
        self.workspace.navigator.image_viewer.display_panel.add_to_top_slot(self.peaksChkBx)
        
    
 
    def _create_menu_bar(self):
        """Create menu bar"""
        saveSettingAction = QAction('Save Current Settings', self)
        saveSettingAction.setShortcut('Ctrl+S')
        saveSettingAction.triggered.connect(self.saveSettings)

        loadSettingAction = QAction('Load Settings', self)
        loadSettingAction.setShortcut('Ctrl+L')
        loadSettingAction.triggered.connect(self.loadSettings)


        menubar = self.menuBar()
        # menubar.setNativeMenuBar(False)
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(saveSettingAction)
        fileMenu.addAction(loadSettingAction)

        aboutAct = QAction('About', self)
        aboutAct.triggered.connect(self.showAbout)
        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)
    
    # NOTE: _create_status_bars() removed - using BaseGUI's default implementation
    # BaseGUI provides all necessary status bar widgets:
    # - self.statusReport, self.imgCoordOnStatusBar, self.imgDetailOnStatusBar
    # - self.imgPathOnStatusBar, self.progressBar
    # - self.left_status (in lowerStatusBar)
    
    def _finalize_ui(self):
        """Final UI setup - set window constraints"""
        # Set minimum size for central widget to ensure usable UI
        self.centralWidget.setMinimumSize(700, 500)
        
        # Call parent to handle window resize and show
        super()._finalize_ui()
    
    def _additional_setup(self):
        """Additional PT-specific setup"""
        # Call parent to setup scan monitoring
        super()._additional_setup()
        
        # Create aliases for backward compatibility with PT code
        self.displayImgCanvas = self.workspace.navigator.image_viewer.canvas
        self.displayImgAxes = self.workspace.navigator.image_viewer.axes
        self.displayImgFigure = self.workspace.navigator.image_viewer.figure
        
        # Backward compatibility: ProjectionBoxTab uses pixel_detail
        # This is an alias for imgDetailOnStatusBar from BaseGUI (created in _create_status_bars)
        self.pixel_detail = self.imgDetailOnStatusBar
        
        # Connect workspace/navigator signals (following QF pattern)
        # fileLoaded: Folder-level initialization (csvManager, boxes, etc.) - happens BEFORE first image
        self.workspace.navigator.fileLoaded.connect(self._on_folder_loaded)
        
        # imageDataReady: Receives ImageData ready for processing (replaces imageChanged)
        self.workspace.imageDataReady.connect(self._on_image_data_ready)
        
        # needsReprocess: Settings changed, reprocess current image
        self.workspace.needsReprocess.connect(self._on_needs_reprocess)
        
        # statusTextRequested: Update status bar
        self.workspace.statusTextRequested.connect(self._on_status_text_requested)
    
    def updateLeftWidgetWidth(self):
        """Update left widget width based on canvas visibility"""
        if self.displayImgCanvas.isVisible():
            # Remove the minimum width constraint when canvas is visible
            self.leftWidget.setMinimumWidth(0)
        else:
            # Set the minimum width for when the canvas is hidden
            self.leftWidget.setMinimumWidth(650)

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.tabWidget.currentChanged.connect(self.updateUI)
        self.tabWidget.tabCloseRequested.connect(self.removeTab)

        # NOTE: ProcessingWorkspace signals are now connected in _additional_setup()
        # This follows the QF pattern where workspace signals are connected after initialization

        # Note: QF checkbox signal is now handled internally by workspace
        # workspace.needsReprocess will trigger processImage() automatically

        self.boxesChkBx.stateChanged.connect(self.updateImage)
        self.peaksChkBx.stateChanged.connect(self.updateImage)
        self.centerChkBx.stateChanged.connect(self.updateImage)

        # Note: Blank/Mask connections now handled by ProcessingWorkspace

        # Mask
        self.maskThresSpnBx.valueChanged.connect(self.maskThresChanged)

        # select boxes
        self.addBoxButton.clicked.connect(self.addABox)
        self.addOrientedBoxButton.clicked.connect(self.addOrientedBox)
        self.addCenterOrientedBoxButton.clicked.connect(self.addOrientedBox)
        self.editBoxButton.clicked.connect(self.editBoxes)
        self.clearBoxButton.clicked.connect(self.clearBoxes)

        # select peaks
        self.selectPeaksButton.clicked.connect(self.addPeaks)

        # Export 1-D Projections checkbox
        self.exportChkBx.stateChanged.connect(self.exportHistograms)

        # Process Folder buttons (access via navControls)
        self.navControls.processFolderButton.clicked.connect(self.batchProcBtnToggled)
        self.navControls.processH5Button.clicked.connect(self.h5batchProcBtnToggled)

        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)
        self.displayImgFigure.canvas.mpl_connect('button_release_event', self.imgReleased)
        self.displayImgFigure.canvas.mpl_connect('figure_leave_event', self.leaveImage)
        self.displayImgFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)

        # Register PlaceholderTool to block ImageViewerWidget's built-in pan during PT operations
        if hasattr(self, 'image_viewer') and self.image_viewer:
            self.image_viewer.tool_manager.register_tool('pt_operation', PlaceholderTool)

    def _on_status_text_requested(self, text: str):
        """
        Handle status text update request from ProcessingWorkspace.
        
        Args:
            text: Status text to display
        """
        if hasattr(self, 'statusReport'):
            self.statusReport.setText(text)

    def maskThresChanged(self):
        """
        Trigger when Mask threshold is changed.
        Writes directly to state and clears histograms.
        """
        if self.projProc is not None:
            # Write directly to state
            self.projProc.state.mask_thres = self.maskThresSpnBx.value()
            # Clear histograms for all boxes
            for box in self.projProc.boxes.values():
                box.hist = None
            print("Mask threshold changed")
            self.processImage()

    # Note: calibrationClicked() removed - calibration now handled by ProcessingWorkspace



    def clearImage(self):
        """
        Clear the image of all the plot on it
        """
        ax = self.displayImgAxes
        for i in range(len(ax.lines)-1,-1,-1):
            ax.lines[i].remove()
        for _, box in self.boxes_on_img.items():
            box['rect'].remove()
            box['text'].remove()
        self.displayImgCanvas.draw_idle()


    def updateImage(self):
        """
        Refresh image tab
        """
        self.update_plot['img'] = True
        self.updateUI()


    def updatePeaks(self, name, peaks):
        """
        Update peaks in both folder template and processor.
        
        :param name: Box name
        :param peaks: Full mirrored peaks (e.g., [10, 20, -10, -20])
        
        Design:
        - self.boxes[name].peaks: Store only first half (folder template)
        - projProc.boxes[name].peaks: Store full peaks (for immediate processing)
        """
        # Update folder template (only first half - user-selected side)
        if name in self.boxes:
            self.boxes[name].peaks = peaks[:len(peaks)//2] if peaks else []
            print(f"Updated folder template peaks for '{name}': {len(self.boxes[name].peaks)} peaks (first half)")
        
        # Update processor (full peaks for immediate processing)
        if self.projProc and name in self.projProc.boxes:
            self.projProc.boxes[name].peaks = peaks
            # Clear cached results to force re-fitting with new peaks
            self.projProc.boxes[name].clear_results(from_stage='fit')
            print(f"Updated processor peaks for '{name}': {len(peaks)} peaks (full)")

    def addPeakstoBox(self, name, peaks):
        """
        add peaks to box and process image
        :param name:
        :param peaks:
        :return:
        """
        self.updatePeaks(name, peaks)
        self.processImage()

    def addPeaks(self):
        """
        Triggered when Add a Box pressed
        :return:
        """
        if self.projProc is None:
            self.selectPeaksButton.setChecked(False)
            return

        if self.selectPeaksButton.isChecked():
            # Start function
            if self.function is None:
                self.selectPeaksButton.setText("Done")
                self.setLeftStatus(
                    "Add left and right peaks simultaneously to boxes by clicking inside a box (ESC to cancel)")
                peaks = {}
                self.function = ['peaks', peaks]
                # Activate placeholder tool to block ImageViewerWidget's built-in pan
                if hasattr(self, 'image_viewer') and self.image_viewer:
                    self.image_viewer.tool_manager.activate_tool('pt_operation')
                ax = self.displayImgAxes
                for i in range(len(ax.lines)-1,-1,-1):
                    ax.lines[i].remove()
                self.displayImgCanvas.draw_idle()
            else:
                # ignore if there're other function being active
                self.selectPeaksButton.setChecked(False)
                return
        else:
            if self.function is not None and len(self.function) == 2:
                # When Done clicked
                peaks = self.function[1]
                for name in peaks.keys():
                    self.updatePeaks(name, peaks[name])

            print("peaks")
            self.processImage()

    def batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.navControls.processFolderButton.isChecked():
            if not self.progressBar.isVisible():
                self.navControls.processFolderButton.setText("Stop")
                self.processFolder()
        else:
            self.stop_process = True
    
    def h5batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.navControls.processH5Button.isChecked():
            if not self.progressBar.isVisible():
                self.navControls.processH5Button.setText("Stop")
                self.processH5File()
        else:
            self.stop_process = True

    def processFolder(self):
        """
        Triggered when a folder has been selected to process it
        """
        idxs = range(len(self.file_manager.names))
        self._process_image_list(idxs, text="Process Current Folder")

    def processH5File(self):
        """
        Triggered when a folder with multiple H5 files has been selected to process it
        """
        start_idx, end_idx = self.file_manager.get_current_h5_range()
        self._process_image_list(range(start_idx, end_idx + 1), text="Process Current H5 File")

    def _process_image_list(self, img_ids, text):
        """
        Process a list of images by index.
        
        Args:
            img_ids: Iterable of image indices to process
            text: Dialog title text
        """
        img_ids = list(img_ids)  # Convert range to list for len()
        self.numberOfFiles = len(img_ids)

        errMsg = QMessageBox()
        errMsg.setText(text)
        info_text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'
        settings = self.getSettings()

        info_text += "\nCurrent Settings"
        for bn in self.boxes.keys():
            box = self.boxes[bn]
            info_text += "\n\n  - Box "+str(bn)+" : " + str(box.coordinates)
            info_text += "\n     - Peaks : "
            if box.peaks:
                info_text += str(box.peaks)
            else:
                info_text += "-"

            info_text += '\n     - Background Subtraction : '
            if box.bgsub == 0:
                info_text += 'Fitting Gaussians'
            else:
                info_text += 'Convex Hull'

            if box.hull_range:
                info_text += '\n     - Convex Hull Range : '+str(box.hull_range)

        if 'lambda_sdd' in settings:
            info_text += "\n  - Lambda Sdd : " + str(settings["lambda_sdd"])

        info_text += '\n\nAre you sure you want to process ' + str(
            self.numberOfFiles) + ' image(s)? \nThis might take a long time.'
        errMsg.setInformativeText(info_text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            self.progressBar.setVisible(True)
            self.stop_process = False
            self.totalFiles = self.numberOfFiles
            self.tasksDone = 0
            for idx, i in enumerate(img_ids):
                if self.stop_process:
                    break
                self.addTask(i)
                # Process UI events periodically to keep Stop button responsive
                if idx % 10 == 0:
                    QApplication.processEvents()
        else:
            # User cancelled the dialog, reset buttons
            self.navControls.processFolderButton.setChecked(False)
            self.navControls.processH5Button.setChecked(False)
        
        # Reset button text based on mode
        if self.workspace.navigator.is_h5_mode:
            self.navControls.processFolderButton.setText("Process Current H5 File")
        else:
            self.navControls.processFolderButton.setText("Process Current Folder")

    def clearBoxes(self):
        """
        Clear all boxes
        """
        self.boxes = {}
        self.boxes_on_img = {}
        
        # Clear boxes from processor as well
        if self.projProc:
            self.projProc.boxes.clear()
            
        self.removeAllTabs()
        self.processImage()

    def addABox(self):
        """
        Triggered when Add a Box pressed
        :return:
        """
        if self.projProc is None:
            self.addBoxButton.setChecked(False)
            return

        if self.addBoxButton.isChecked():
            if self.function is None:
                # Start function
                self.addBoxButton.setText("Done")
                self.setLeftStatus("Add a box to the image by drawing a rectangle (ESC to cancel)")
                self.function = ['box']
                # Activate placeholder tool to block ImageViewerWidget's built-in pan
                if hasattr(self, 'image_viewer') and self.image_viewer:
                    self.image_viewer.tool_manager.activate_tool('pt_operation')
                ax = self.displayImgAxes
                for line in list(ax.lines):
                    line.remove()
                self.displayImgCanvas.draw_idle()
            else:
                self.addBoxButton.setChecked(False)
                self.function = None
                # Deactivate placeholder tool
                if hasattr(self, 'image_viewer') and self.image_viewer:
                    self.image_viewer.tool_manager.deactivate_tool('pt_operation')
                return

    def addOrientedBox(self):
        """
        Triggered when Add Oriented Boxes is pressed
        :return:
        """
        if self.projProc is None:
            self.addOrientedBoxButton.setChecked(False)
            self.addCenterOrientedBoxButton.setChecked(False)
            return

        if self.addOrientedBoxButton.isChecked() and not self.addCenterOrientedBoxButton.isChecked():
            if self.function is None:
                # Start function
                self.addOrientedBoxButton.setText("Done")
                self.setLeftStatus("Select a pivot point indicating the box center (ESC to cancel)")
                self.function = ['oriented_box']
                # Activate placeholder tool to block ImageViewerWidget's built-in pan
                if hasattr(self, 'image_viewer') and self.image_viewer:
                    self.image_viewer.tool_manager.activate_tool('pt_operation')
                ax = self.displayImgAxes
                for line in list(ax.lines):
                    line.remove()
                self.displayImgCanvas.draw_idle()
            else:
                self.addOrientedBoxButton.setChecked(False)
                self.function = None
                # Deactivate placeholder tool
                if hasattr(self, 'image_viewer') and self.image_viewer:
                    self.image_viewer.tool_manager.deactivate_tool('pt_operation')
                return

        elif self.addCenterOrientedBoxButton.isChecked() and not self.addOrientedBoxButton.isChecked():
            if self.function is None:
                # Start function
                self.addCenterOrientedBoxButton.setText("Done")
                self.setLeftStatus("Drag to select the rotation angle and length of the projection axis (ESC to cancel)")
                self.function = ['center_oriented_box']
                self.function.append(self.projProc.center)
                # Activate placeholder tool to block ImageViewerWidget's built-in pan
                if hasattr(self, 'image_viewer') and self.image_viewer:
                    self.image_viewer.tool_manager.activate_tool('pt_operation')
                ax = self.displayImgAxes
                for line in list(ax.lines):
                    line.remove()
                self.displayImgCanvas.draw_idle()
            else:
                self.addOrientedBoxButton.setChecked(False)
                self.function = None
                # Deactivate placeholder tool
                if hasattr(self, 'image_viewer') and self.image_viewer:
                    self.image_viewer.tool_manager.deactivate_tool('pt_operation')
                return
        else:
            self.addCenterOrientedBoxButton.setChecked(False)
            self.addOrientedBoxButton.setChecked(False)
            self.resetUI()
            
    def editBoxes(self):
        if len(self.boxes) > 0:
            dialog = EditBoxDetails(self.boxes)
            if dialog.exec_():
                height = dialog.new_height
                width = dialog.new_width
                target_box = dialog.current_box
                height_mode = dialog.height_mode
                width_mode = dialog.width_mode
                self.updateBoxDetails(target_box, height, width, height_mode, width_mode)
        else:
            errMsg = QMessageBox()
            errMsg.setText('No boxes to edit.')
            msg = 'Please add a box before editing.'
            errMsg.setInformativeText(msg)
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.setFixedWidth(600)
            errMsg.exec_()
            
    def updateBoxDetails2(self, box_name, height, width, height_mode, width_mode):
    
        box_obj = self.boxes[box_name]
        box = box_obj.coordinates

        if box_obj.type == 'oriented':
            bx, by = box[2]
            current_width = box[3]
            current_height = box[4]
            angle = box[5]
            cx, cy = box[6]

            # Determine new points for each side based on the modes
            bl = rotatePoint((cx, cy), (bx, by), -np.radians(angle))
            br = (bl[0] + current_width, bl[1])
            tl = (bl[0], bl[1] + current_height)
            tr = (bl[0] + current_width, bl[1] + current_height)
            
            print(bl, br, tl, tr)

            if height_mode == 'Center':
                bl = (bl[0], bl[1] - (height - current_height) / 2)
                tl = (tl[0], tl[1] + (height - current_height) / 2)
            elif height_mode == 'Top':
                tl = (tl[0], tl[1] + (height - current_height))
                tr = (tr[0], tr[1] + (height - current_height))
            elif height_mode == 'Bottom':
                bl = (bl[0], bl[1] - (height - current_height))
                br = (br[0], br[1] - (height - current_height))

            if width_mode == 'Center':
                bl = (bl[0] - (width - current_width) / 2, bl[1])
                br = (br[0] + (width - current_width) / 2, br[1])
            elif width_mode == 'Left':
                bl = (bl[0] - (width - current_width), bl[1])
                tl = (tl[0] - (width - current_width), tl[1])
            elif width_mode == 'Right':
                br = (br[0] + (width - current_width), br[1])
                tr = (tr[0] + (width - current_width), tr[1])

            # Rotate the updated points back to the original orientation
            bl_rot = rotatePoint((cx, cy), bl, np.radians(angle))
            br_rot = rotatePoint((cx, cy), br, np.radians(angle))
            tl_rot = rotatePoint((cx, cy), tl, np.radians(angle))
            tr_rot = rotatePoint((cx, cy), tr, np.radians(angle))
            
            (bl_rot, br_rot, tl_rot, tr_rot)

            # Update the box with the new points
            box_obj.coordinates = [bl_rot, br_rot, tl_rot, tr_rot, width, height, angle, (cx, cy)]
        else:
            # Handle non-oriented box (as in your original code)
            x1, x2 = box[0]
            y1, y2 = box[1]
            current_width = abs(x1 - x2)
            current_height = abs(y1 - y2)
            height_diff = height - current_height
            width_diff = width - current_width

            if height_diff != 0 or width_diff != 0:
                if height_mode == 'Center':
                    y1 = y1 - height_diff/2
                    y2 = y2 + height_diff/2
                elif height_mode == 'Top':
                    y1 = y1 - height_diff
                elif height_mode == 'Bottom':
                    y2 = y2 + height_diff

                if width_mode == 'Center':
                    x1 = x1 - width_diff/2
                    x2 = x2 + width_diff/2
                elif width_mode == 'Left':
                    x1 = x1 - width_diff
                elif width_mode == 'Right':
                    x2 = x2 + width_diff

                box_obj.coordinates = [(x1, x2), (y1, y2)]
            
    def updateBoxDetails(self, box_name, height, width, height_mode, width_mode):
        box_obj = self.boxes[box_name]
        box = box_obj.coordinates
        if box_obj.type == 'oriented':
            bx, by = box[2]
            current_width = box[3]
            current_height = box[4]
            height_diff = height - current_height
            width_diff = width - current_width
            angle = box[5]
            cx, cy = box[6]
            new_point = rotatePoint((cx, cy), (bx, by), -np.radians(angle))
            if height_diff != 0 or width_diff != 0:
                if height_mode == 'Center':
                    new_height = new_point[1] - height_diff/2
                elif height_mode == 'Top':
                    new_height = new_point[1] - height_diff
                    cy = cy - height_diff/2
                elif height_mode == 'Bottom':
                    new_height = new_point[1]
                    cy = cy + height_diff/2
                if width_mode == 'Center':
                    new_width = new_point[0] - width_diff/2
                elif width_mode == 'Left':
                    new_width = new_point[0] - width_diff
                    cx = cx - width_diff/2
                elif width_mode == 'Right':
                    new_width = new_point[0]
                    cx = cx + width_diff/2
                translated_point = (new_width, new_height)
                
                new_bl = rotatePoint((cx,cy), (translated_point[0], translated_point[1]), np.radians(angle))
                x1, y1 = translated_point
                x2 = x1 + width
                y2 = y1 + height
                box_obj.coordinates = ((x1, x2), (y1, y2), new_bl, width, height, angle, (cx,cy))         
        else:
            x1,x2 = box[0]
            y1,y2 = box[1]
            current_width = abs(x1 - x2)
            current_height = abs(y1 - y2)
            height_diff = height - current_height
            width_diff = width - current_width
            
            if height_diff != 0 or width_diff != 0:
                print(height_mode)
                print(width_mode)
                if height_mode == 'Center':
                    y1 = y1 - height_diff/2
                    y2 = y2 + height_diff/2
                elif height_mode == 'Top':
                    y1 = y1 - height_diff   
                elif height_mode == 'Bottom':
                    y2 = y2 + height_diff
        
                if width_mode == 'Center':
                    x1 = x1 - width_diff/2
                    x2 = x2 + width_diff/2
                elif width_mode == 'Left':
                    x1 = x1 - width_diff
                elif width_mode == 'Right':
                    x2 = x2 + width_diff
    
                box_obj.coordinates = [(x1, x2), (y1, y2)]
        
        # Sync coordinates to processor
        if self.projProc and box_name in self.projProc.boxes:
            self.projProc.boxes[box_name].coordinates = box_obj.coordinates
            # Clear histogram results since box region changed
            self.projProc.boxes[box_name].clear_results(from_stage='hist')
        
        for artist in self.boxes_on_img[box_name].values():
            artist.remove()
        del self.boxes_on_img[box_name]     
        self.boxes_on_img[box_name] = self.genBoxArtists(box_name, box_obj.coordinates, box_obj.type)
        self.processImage()
            
        
            
    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard
        """
        key = event.key()

        if key == Qt.Key_Escape:
            self.resetUI()
        elif key == Qt.Key_D:
            self.tabWidget.setCurrentIndex((self.tabWidget.currentIndex() + 1) % self.tabWidget.count())
        elif key == Qt.Key_A:
            self.tabWidget.setCurrentIndex((self.tabWidget.currentIndex() - 1) % self.tabWidget.count())
        elif key == Qt.Key_Q:
            self.close()

    def mousePressEvent(self, event):
        """
        Clear focus when mouse pressed
        """
        focused_widget = QApplication.focusWidget()
        if focused_widget is not None:
            focused_widget.clearFocus()

    # Note: PT navigation methods now use FileManager (from BaseGUI)
    # These are kept for backward compatibility but delegate to FileManager



    def setH5Mode(self, file_name):
        """
        Displays the right set of buttons depending on whether we're in H5 mode.
        """
        if self.workspace.navigator.is_h5_mode:
            self.navControls.nextFileButton.show()
            self.navControls.prevFileButton.show()
            self.navControls.processH5Button.show()
            self.navControls.processFolderButton.setText("Process Current H5 File")
        else:
            self.navControls.nextFileButton.hide()
            self.navControls.prevFileButton.hide()
            self.navControls.processH5Button.hide()
            self.navControls.processFolderButton.setText("Process Current Folder")

    def removeTab(self, index):
        """
        Remove the tab selected by index
        """
        if index != 0:
            widget = self.tabWidget.widget(index)
            if widget is not None:
                name = widget.name
                # Remove from unified box storage
                if name in self.boxes:
                    del self.boxes[name]
                # Remove from processor
                if self.projProc and name in self.projProc.boxes:
                    del self.projProc.boxes[name]
                # Remove visual representation
                if name in self.boxes_on_img:
                    for artist in self.boxes_on_img[name].values():
                        artist.remove()
                    del self.boxes_on_img[name]
                widget.deleteLater()
            self.processImage()
            self.tabWidget.removeTab(index)

    def removeAllTabs(self):
        """
        Remove old box tabs
        :return:
        """
        while self.tabWidget.count() > 1:
            self.tabWidget.removeTab(1)

    def addBoxTabs(self):
        """
        Add box tabs based on current image's boxes.
        Uses projProc.boxes if available (current image), otherwise uses folder template.
        
        Handles open parameter editors during image switch:
        - Saves which boxes had open parameter editors
        - After creating new tabs, updates the editors with new image data
        """
        # Save current tab index and name
        current_index = self.tabWidget.currentIndex()
        was_on_box_tab = current_index > 0  # Was on a box tab (not image tab)
        current_tab_name = None
        if was_on_box_tab:
            current_tab_name = self.tabWidget.tabText(current_index).replace("Box ", "")
        
        # Save info about open parameter editors before removing tabs
        # {box_name: QRect} - save geometry (position + size) for each open editor
        open_param_editors = {}
        for i in range(1, self.tabWidget.count()):  # Skip image tab (index 0)
            tab = self.tabWidget.widget(i)
            if isinstance(tab, ProjectionBoxTab) and tab.param_editor_active:
                # Save dialog geometry before closing
                if tab.param_editor_dialog is not None:
                    open_param_editors[tab.name] = tab.param_editor_dialog.geometry()
                else:
                    open_param_editors[tab.name] = None
                # Close the dialog before removing the tab
                # This ensures proper cleanup (onParameterEditorClosed will be triggered)
                tab.closeParameterEditor()
        
        self.removeAllTabs()

        # Use current image's boxes if available, otherwise use folder template
        if self.projProc is not None:
            boxes_to_display = self.projProc.boxes
        else:
            boxes_to_display = self.boxes

        # Rebuild tabs and track the index to restore
        restore_index = 0
        new_tabs = {}  # {box_name: ProjectionBoxTab} to reopen editors after
        for idx, name in enumerate(boxes_to_display.keys(), start=1):
            proj_tab = ProjectionBoxTab(self, name)
            self.tabWidget.addTab(proj_tab, "Box "+str(name))
            new_tabs[name] = proj_tab
            
            # Check if this is the previously selected tab
            if current_tab_name == name:
                restore_index = idx
        
        # Restore tab selection:
        # 1. If the exact same box exists, select it
        # 2. If user was on a box tab but that box no longer exists, select first box tab
        # 3. If user was on image tab, stay on image tab
        if restore_index > 0:
            self.tabWidget.setCurrentIndex(restore_index)
        elif was_on_box_tab and len(boxes_to_display) > 0:
            # Was on a box tab, but that box no longer exists -> select first box tab
            self.tabWidget.setCurrentIndex(1)
        
        # Reopen parameter editors for boxes that still exist in the new image
        # This is done after tab restoration so the UI is in a stable state
        for box_name, saved_geometry in open_param_editors.items():
            if box_name in new_tabs:
                new_tab = new_tabs[box_name]
                # Check if the new image has fit_results for this box
                box = new_tab.get_box()
                if box and box.fit_results is not None:
                    # Reopen parameter editor with new image's data
                    print(f"Reopening parameter editor for box '{box_name}' after image switch")
                    new_tab.openParameterEditor()
                    # Restore saved geometry (position + size)
                    if saved_geometry is not None and new_tab.param_editor_dialog is not None:
                        new_tab.param_editor_dialog.setGeometry(saved_geometry)
                else:
                    print(f"Box '{box_name}' has no fit_results in new image, not reopening editor")

    def imgClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if self.projProc is None:
            return

        x = event.xdata
        y = event.ydata

        ax = self.displayImgAxes
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

        func = self.function

        # Provide different behavior depending on current active function
        if func is None:
            # Enable image panning when clicking on empty areas
            self.function = ['im_move', (x, y)]

        elif func[0] == 'box':
            # First draw two lines
            func.append((x, y))
            if len(func) == 3:
                # A box added
                points = self.function[1:]
                x1 = int(round(min(points[0][0], points[1][0])))
                y1 = int(round(min(points[0][1], points[1][1])))
                x2 = int(round(max(points[0][0], points[1][0])))
                y2 = int(round(max(points[0][1], points[1][1])))
                boxDialog = BoxDetails(self.boxes.keys())
                result = boxDialog.exec_()
                if result == 1:
                    name, bgsub, axis = boxDialog.getDetails()
                    box_type = 'h' if axis == 0 else 'v'
                    # Create new ProcessingBox
                    new_box = ProcessingBox(
                        name=name,
                        coordinates=((x1, x2), (y1, y2)),
                        type=box_type,
                        bgsub=bgsub,
                        peaks=[],
                        merid_bg=True
                    )
                    self.boxes[name] = new_box
                    self.boxes_on_img[name] = self.genBoxArtists(name, new_box.coordinates, box_type)
                    
                    # Add new box to processor (create independent copy to avoid shared reference)
                    if self.projProc:
                        box_copy = ProcessingBox(
                            name=new_box.name,
                            coordinates=new_box.coordinates,
                            type=new_box.type,
                            bgsub=new_box.bgsub,
                            peaks=new_box.peaks.copy() if new_box.peaks else [],
                            merid_bg=new_box.merid_bg,
                            hull_range=new_box.hull_range,
                            param_bounds=new_box.param_bounds.copy() if new_box.param_bounds else {},
                            use_common_sigma=new_box.use_common_sigma,
                            peak_tolerance=new_box.peak_tolerance,
                            sigma_tolerance=new_box.sigma_tolerance,
                        )
                        # Expand peaks for processor (folder template keeps first half only)
                        self._expand_peaks_mirrored(box_copy)
                        self.projProc.state.boxes[name] = box_copy
                        
                self.function = None
                # Deactivate placeholder tool after box operation completes
                if hasattr(self, 'image_viewer') and self.image_viewer:
                    self.image_viewer.tool_manager.deactivate_tool('pt_operation')
                self.addBoxTabs()
                self.processImage()

        elif func[0] == 'oriented_box' or func[0] == 'center_oriented_box':
            if len(func) == 1: # select a pivot
                axis_size = 5
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                self.displayImgCanvas.draw_idle()
                func.append((x,y))
                self.setLeftStatus("Drag to select the rotation angle and length of the projection axis (ESC to cancel)")

            elif len(func) == 2: # rotate and extend around the pivot
                pivot = func[1]
                deltax = x - pivot[0]
                deltay = y - pivot[1]
                x2 = pivot[0] - deltax
                y2 = pivot[1] - deltay
                for line in list(ax.lines):
                    line.remove()
                ax.plot([x, x2], [y, y2], color="r")
                self.displayImgCanvas.draw_idle()

                if abs(x - pivot[0]) == 0:
                    new_angle = -90
                else:
                    new_angle = -180. * np.arctan((pivot[1] - y) / (pivot[0] - x)) / np.pi

                func.append((x,x2,y,y2, new_angle))
                self.setLeftStatus("Drag to select the width of the box (must be less than the length) (ESC to cancel)")

            elif len(func) == 3: # drag to select the width
                if len(ax.patches) > 0:
                    for i in range(len(ax.patches)-1,len(self.boxes.keys())-1,-1):
                        ax.patches[i].remove()
                    # ax.patches = ax.patches[:len(self.boxes.keys())]
                for line in list(ax.lines):
                    line.remove()

                box_angle = func[2][4]
                angle = np.radians(90 - box_angle)

                # display the box as it's drawn by the user
                p_mouse = np.array([x,y])
                if func[2][0] <= func[2][1]: # p1 is to the left
                    p_left = np.array((func[2][0], func[2][2]))
                    p_right = np.array((func[2][1], func[2][3]))
                else:
                    p_left = np.array((func[2][1], func[2][3]))
                    p_right = np.array((func[2][0], func[2][2]))

                v1 = p_left - p_right
                v2 = p_mouse - p_right

                u = v1/np.linalg.norm(v1)
                n = (-u[1], u[0])

                height = np.abs(np.dot(v2, n))
                width = np.linalg.norm(p_left - p_right)

                if height*2 < width:
                    x1_left = p_left[0] - height*np.cos(angle)
                    y1_left = p_left[1] - height*np.sin(angle)
                    x2_left = p_left[0] + height*np.cos(angle)
                    y2_left = p_left[1] + height*np.sin(angle)
                    x1_right = p_right[0] - height*np.cos(angle)
                    y1_right = p_right[1] - height*np.sin(angle)
                    x2_right= p_right[0] + height*np.cos(angle)
                    y2_right= p_right[1] + height*np.sin(angle)

                    ax.plot([func[2][0], func[2][1]], [func[2][2], func[2][3]], color="r")

                    ax.plot([p_left[0], x2_left], [p_left[1], y2_left], color="r", linestyle='dotted')
                    ax.plot([x1_left, p_left[0]], [y1_left, p_left[1]], color="r", linestyle='dotted')

                    ax.plot([p_right[0], x2_right], [p_right[1], y2_right], color="r", linestyle='dotted')
                    ax.plot([x1_right, p_right[0]], [y1_right, p_right[1]], color="r", linestyle='dotted')

                    ax.plot([x1_left, x1_right], [y1_left, y1_right], color="r", linestyle='dotted')
                    ax.plot([x2_left, x2_right], [y2_left, y2_right], color="r", linestyle='dotted')

                    rot_angle = box_angle * -1
                    bottom_left = (x1_left, y1_left)

                    ax.add_patch(patches.Rectangle(bottom_left, width, height*2, angle=rot_angle,
                                                   linewidth=1, edgecolor='g', facecolor='none', linestyle='dotted'))
                    self.displayImgCanvas.draw_idle()

                    boxDialog = BoxDetails(self.boxes.keys(), oriented=True)
                    result = boxDialog.exec_()
                    if result == 1:
                        # get the image (already rotated in process() if needed)
                        img = copy.copy(self.projProc.orig_img)

                        blx, bly = int(bottom_left[0]), int(bottom_left[1])
                        pivot = func[1]
                        cx, cy = pivot[0], pivot[1]

                        # get the image rotated around the box center
                        img = rotateImageAboutPoint(img, (cx, cy), rot_angle)

                        x1, y1 = rotatePoint((cx, cy), (blx, bly), -np.radians(rot_angle))
                        x2 = x1 + width
                        y2 = y1 + height*2

                        # add the oriented box
                        name, bgsub, _ = boxDialog.getDetails()
                        # Create new ProcessingBox for oriented type
                        new_box = ProcessingBox(
                            name=name,
                            coordinates=((x1, x2), (y1, y2), bottom_left, width, height*2, rot_angle, pivot),
                            type='oriented',
                            bgsub=bgsub,
                            peaks=[],
                            merid_bg=True
                        )
                        self.boxes[name] = new_box
                        self.boxes_on_img[name] = self.genBoxArtists(name, new_box.coordinates, 'oriented')
                        self.function = None
                        # Deactivate placeholder tool after oriented box operation completes
                        if hasattr(self, 'image_viewer') and self.image_viewer:
                            self.image_viewer.tool_manager.deactivate_tool('pt_operation')
                        
                        # Add new box to processor (create independent copy to avoid shared reference)
                        if self.projProc:
                            box_copy = ProcessingBox(
                                name=new_box.name,
                                coordinates=new_box.coordinates,
                                type=new_box.type,
                                bgsub=new_box.bgsub,
                                peaks=new_box.peaks.copy() if new_box.peaks else [],
                                merid_bg=new_box.merid_bg,
                                hull_range=new_box.hull_range,
                                param_bounds=new_box.param_bounds.copy() if new_box.param_bounds else {},
                                use_common_sigma=new_box.use_common_sigma,
                                peak_tolerance=new_box.peak_tolerance,
                                sigma_tolerance=new_box.sigma_tolerance,
                            )
                            # Expand peaks for processor (folder template keeps first half only)
                            self._expand_peaks_mirrored(box_copy)
                            self.projProc.state.boxes[name] = box_copy

                        self.addBoxTabs()
                        self.processImage()

        elif func[0] == "peaks":
            peaks = func[1]
            if len(self.boxes.keys()) > 0:
                for name in self.boxes.keys():
                    box_obj = self.boxes[name]
                    box = box_obj.coordinates
                    boxx = box[0]
                    boxy = box[1]
                    typ = box_obj.type
                    centerx = self.centerx
                    centery = self.centery
                    comp_x, comp_y = x, y # use a placeholder x,y for comparisons

                    # if oriented, then rotate x and y into the box
                    if typ == 'oriented':
                        # switch center to the box center
                        centerx, centery = box[6][0], box[6][1]
                        # d = np.sqrt((centerx-comp_x)**2+(centery-comp_y)**2)
                        comp_x, comp_y = rotatePoint((centerx, centery), (comp_x, comp_y), -np.radians(box[5]))

                    if boxx[0] <= comp_x <= boxx[1] and boxy[0] <= comp_y <= boxy[1]:
                        if name not in peaks:
                            peaks[name] = []

                        if typ == 'h':
                            distance = int(round(abs(centerx-comp_x)))
                            peaks[name].append(distance)
                            ax.plot((centerx-distance, centerx-distance), boxy, color='r')
                            ax.plot((centerx+distance, centerx+distance), boxy, color='r')
                        elif typ == 'oriented':
                            distance = np.sqrt((centerx-comp_x)**2+(centery-comp_y)**2)
                            edge_1 = rotatePoint((centerx, centery), (centerx-distance, boxy[0]), np.radians(box[5]))
                            edge_2 = rotatePoint((centerx, centery), (centerx-distance, boxy[1]), np.radians(box[5]))
                            edge_3 = rotatePoint((centerx, centery), (centerx+distance, boxy[0]), np.radians(box[5]))
                            edge_4 = rotatePoint((centerx, centery), (centerx+distance, boxy[1]), np.radians(box[5]))

                            ax.plot((edge_1[0], edge_2[0]), (edge_1[1], edge_2[1]), color='r')
                            ax.plot((edge_3[0], edge_4[0]), (edge_3[1], edge_4[1]), color='r')

                            peaks[name].append(distance)
                        else:
                            distance = int(round(abs(centery - comp_y)))
                            peaks[name].append(distance)
                            ax.plot(boxx, (centery - distance, centery - distance), color='r')
                            ax.plot(boxx, (centery + distance, centery + distance), color='r')
                        break
                self.displayImgCanvas.draw_idle()



    def genBoxArtists(self, name, box, btype):
        """
        Generate aritists to represent a box on Axes.
        """
        if btype in ('h', 'v'):
            x, x2, y, y2 = box[0][0], box[0][1], box[1][0], box[1][1]
            w, h = x2 - x, y2 - y

            box = {}
            if btype == 'h':
                box['rect'] = patches.Rectangle((x, y), w, h,
                    linewidth=1, edgecolor='#95f70c', facecolor='none')
                box['text'] = matplotlib.text.Text(x + w + 10, y + h / 2., name, color='#95f70c',
                    fontsize=10, horizontalalignment='left', verticalalignment='center')
            else:
                box['rect'] = patches.Rectangle((x, y), w, h,
                    linewidth=1, edgecolor='y', facecolor='none')
                box['text'] = matplotlib.text.Text(x + w /2. , y - 20, name, color='y',
                    fontsize=10, horizontalalignment='center', verticalalignment='center')
        elif btype == 'oriented':
            bottom_left, w, h, angle = box[2], box[3], box[4], box[5]
            x, y = box[0][0], box[1][0]
            box = {}
            box['rect'] = patches.Rectangle(bottom_left, w, h, angle=angle,
                linewidth=1, edgecolor='#95f70c', facecolor='none')
            box['text'] = matplotlib.text.Text(bottom_left[0]-(20*len(name)), bottom_left[1]-30, name, color='#95f70c',
                fontsize=10, horizontalalignment='left', verticalalignment='center')
        return box

    def imgOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """
        if self.projProc is None:
            return

        x = event.xdata
        y = event.ydata
        img = self.projProc.orig_img
        ax = self.displayImgAxes
        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            unit = "px"
            calSettings = self.workspace.calibration_settings if hasattr(self, 'workspace') else None
            if calSettings is not None and 'scale' in calSettings:
                if 'center' in calSettings and calSettings['center'] is not None:
                    center = calSettings['center']
                else:
                    center = self.projProc.center
                q, unit = inverseNmFromCenter([x, y], center, calSettings['scale'])
                # constant = calSettings["silverB"] * calSettings["radius"]
                # calib_distance = mouse_distance * 1.0/constant
                # calib_distance = f"{calib_distance:.4f}"
            if x < img.shape[1] and y < img.shape[0]:
                if calSettings is not None and 'scale' in calSettings:
                    self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x])+ ", distance=" + str(q) + unit)
                else:
                    center = self.projProc.center
                    mouse_distance = np.sqrt((center[0] - x) ** 2 + (center[1] - y) ** 2)
                    mouse_distance = f"{mouse_distance:.4f}"
                    self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]) + ", distance=" + str(mouse_distance) + unit)

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

        func = self.function
        if func is None:
            return

        if func[0] == 'box':
            if len(func) == 1:
                # cross lines
                for line in list(ax.lines):
                    if line.get_label() != "Blue Dot":
                        line.remove()
                ax.axhline(y, color='y', linestyle='dotted')
                ax.axvline(x, color='y', linestyle='dotted')
                self.displayImgCanvas.draw_idle()
            elif len(func) == 2:
                # draw rectangle
                if len(ax.patches) > 0:
                    for i in range(len(ax.patches)-1,len(self.boxes.keys())-1,-1):
                        ax.patches[i].remove()
                    # ax.patches = ax.patches[:len(self.boxes.keys())]
                for line in list(ax.lines):
                    if line.get_label() != "Blue Dot":
                        line.remove()
                start_pt = func[-1]
                w = abs(start_pt[0] - x)
                h = abs(start_pt[1] - y)
                x = min(start_pt[0], x)
                y = min(start_pt[1], y)
                ax.add_patch(patches.Rectangle((x, y), w, h,
                                            linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
                self.displayImgCanvas.draw_idle()

        elif func[0] == 'oriented_box' or func[0] == 'center_oriented_box':
            if len(func) == 1:
                axis_size = 5
                for line in list(ax.lines):
                    if line.get_label() != "Blue Dot":
                        line.remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

                self.displayImgCanvas.draw_idle()
            if len(func) == 2:
                # draw line as angle
                pivot = func[1]
                deltax = x - pivot[0]
                deltay = y - pivot[1]
                x2 = pivot[0] - deltax
                y2 = pivot[1] - deltay
                for line in list(ax.lines):
                    if line.get_label() != "Blue Dot":
                        line.remove()
                ax.plot([x, x2], [y, y2], color="r")
                self.displayImgCanvas.draw_idle()
            elif len(func) == 3: # get the width of the box
                if len(ax.patches) > 0:
                    for i in range(len(ax.patches)-1,len(self.boxes.keys())-1,-1):
                        ax.patches[i].remove()
                    # ax.patches = ax.patches[:len(self.boxes.keys())]

                angle = np.radians(90 - func[2][4])
                p_mouse = np.array([x,y])
                if func[2][0] < func[2][1]: # p1 is to the left
                    p_left = np.array((func[2][0], func[2][2]))
                    p_right = np.array((func[2][1], func[2][3]))
                else:
                    p_left = np.array((func[2][1], func[2][3]))
                    p_right = np.array((func[2][0], func[2][2]))

                v1 = p_left - p_right
                v2 = p_mouse - p_right
                u = v1/np.linalg.norm(v1)
                n = (-u[1], u[0])

                height = np.abs(np.dot(v2, n))
                width = np.linalg.norm(p_left - p_right)

                if height*2 < width:

                    x1_left = p_left[0] - height*np.cos(angle)
                    y1_left = p_left[1] - height*np.sin(angle)
                    x2_left = p_left[0] + height*np.cos(angle)
                    y2_left = p_left[1] + height*np.sin(angle)
                    x1_right = p_right[0] - height*np.cos(angle)
                    y1_right = p_right[1] - height*np.sin(angle)
                    x2_right= p_right[0] + height*np.cos(angle)
                    y2_right= p_right[1] + height*np.sin(angle)

                    for line in list(ax.lines):
                        if line.get_label() != "Blue Dot":
                            line.remove()
                    ax.plot([func[2][0], func[2][1]], [func[2][2], func[2][3]], color="r")

                    ax.plot([p_left[0], x2_left], [p_left[1], y2_left], color="r", linestyle='dotted')
                    ax.plot([x1_left, p_left[0]], [y1_left, p_left[1]], color="r", linestyle='dotted')

                    ax.plot([p_right[0], x2_right], [p_right[1], y2_right], color="r", linestyle='dotted')
                    ax.plot([x1_right, p_right[0]], [y1_right, p_right[1]], color="r", linestyle='dotted')

                    ax.plot([x1_left, x1_right], [y1_left, y1_right], color="r", linestyle='dotted')
                    ax.plot([x2_left, x2_right], [y2_left, y2_right], color="r", linestyle='dotted')

                    self.displayImgCanvas.draw_idle()

        elif func[0] == "im_move":
            # change zoom-in location (x,y ranges) to move around image
            if self.img_zoom is not None:
                move = (func[1][0] - x, func[1][1] - y)
                self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[1], img.shape[0])
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
                ax.invert_yaxis()
                self.displayImgCanvas.draw_idle()


    def imgReleased(self, event):
        """
        Triggered when mouse released from image
        """
        if self.function is not None:
            func = self.function
            if func[0] == 'im_move':
                self.function = None

    def leaveImage(self, event):
        """
        Set pixel information is empty if mouse leaves figure
        """
        self.imgCoordOnStatusBar.setText("")

    def imgScrolled(self, event):
        """
        This function is called when a mouse scrolled on the image in image tab. This will affect zoom-in and zoom-out
        """
        if self.projProc is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img_size = self.projProc.orig_img.shape

        if self.img_zoom is None:
            # init values if it's None
            self.img_zoom = [(0, img_size[1]), (0, img_size[0])]

        zoom_height = self.img_zoom[1][1] - self.img_zoom[1][0]
        zoom_width = self.img_zoom[0][1] - self.img_zoom[0][0]

        clicked_x_percentage = 1. * (x - self.img_zoom[0][0]) / zoom_width
        clicked_y_percentage = 1. * (y - self.img_zoom[1][0]) / zoom_height

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

        # Set new x, y ranges
        self.img_zoom = [(x1, x2), (y1, y2)]

        # To zoom-in or zoom-out is setting x and y limit of figure
        ax = self.displayImgAxes
        ax.set_xlim(self.img_zoom[0])
        ax.set_ylim(self.img_zoom[1])
        ax.invert_yaxis()
        self.displayImgCanvas.draw_idle()

    # NOTE: _create_image_data() moved to ImageData.from_settings_panel() factory method

    # ==================== ProcessingWorkspace Integration (New Pattern) ====================
    
    def _on_folder_loaded(self, dir_path: str):
        """
        Called when a new file/folder is loaded (BEFORE first image loads).
        
        This is the folder-level initialization hook, following the QF pattern.
        Handles:
        - Loading cached boxes and peaks
        - Creating CSV manager
        - Updating UI state
        - Setting H5 mode
        
        Args:
            dir_path: Directory path of the loaded file/folder
        """
        # Update directory path
        self.dir_path = dir_path
        
        # Enable PT-specific settings groups
        self.propGrp.setEnabled(True)
        self.boxGrp.setEnabled(True)
        
        # Load cached box configuration from previous session
        cache = self.loadBoxesConfig()
        if cache is not None:
            self.boxes = cache['boxes']
            self.centerx = cache.get('centerx')
            self.centery = cache.get('centery')
            # Create visual representations
            for name, box in self.boxes.items():
                self.boxes_on_img[name] = self.genBoxArtists(name, box.coordinates, box.type)
        else:
            self.boxes = {}
        
        # Create CSV manager for this folder
        self.csvManager = PT_CSVManager(self.dir_path, self.boxes)
        
        # Add box tabs for loaded boxes
        self.addBoxTabs()
        self.selectPeaksGrp.setEnabled(False)
        
        print(f"Folder loaded: {dir_path}")
        print(f"  - Boxes loaded: {len(self.boxes)}")
    
    def _on_image_data_ready(self, image_data):
        """
        Called when ImageData is ready for processing (main processing entry point).
        
        Cache-first strategy:
        1. Create ProjectionProcessor (automatically loads image-level cache if exists)
        2. If cache exists → use it directly (skip folder template)
        3. If no cache → use folder template as initial config
        4. Process image
        5. Update folder cache with refined results
        
        Args:
            image_data: ImageData instance ready for processing
        """
        try:
            # Create ProjectionProcessor (automatically loads image cache in __init__)
            self.projProc = ProjectionProcessor(image_data)
            
            # Check if image-level cache was loaded
            image_cache_loaded = len(self.projProc.boxes) > 0
            if image_cache_loaded:
                for name, proc_box in self.projProc.boxes.items():
                    if name not in self.boxes_on_img:
                        self.boxes_on_img[name] = self.genBoxArtists(name, proc_box.coordinates, proc_box.type)
            else:
                # Transfer folder template to Processor (without results)
                for name, box in self.boxes.items():
                    # Direct copy of configuration (folder template already has config-only, first-half peaks)
                    box_copy = ProcessingBox(
                        name=box.name,
                        coordinates=box.coordinates,
                        type=box.type,
                        bgsub=box.bgsub,
                        peaks=box.peaks.copy() if box.peaks else [],  # Direct copy, already first half
                        merid_bg=box.merid_bg,
                        hull_range=box.hull_range,
                        param_bounds=box.param_bounds.copy() if box.param_bounds else {},
                        use_common_sigma=box.use_common_sigma,
                        peak_tolerance=box.peak_tolerance,
                        sigma_tolerance=box.sigma_tolerance,
                        # Results fields default to None
                    )
                    print(f"  After direct copy: {len(box_copy.peaks)} peaks")
                    
                    # Expand peaks by mirroring user-selected peaks
                    # (folder template only contains first half)
                    self._expand_peaks_mirrored(box_copy)
                    
                    self.projProc.state.boxes[name] = box_copy
                    
                    # Update visual representations
                    if name not in self.boxes_on_img:
                        self.boxes_on_img[name] = self.genBoxArtists(name, box.coordinates, box.type)
            
            # Update ProcessingWorkspace display
            self.workspace.update_display(image_data)
            
            # Initialize non-view UI for new image
            self.initMaskThreshold(self.projProc)
            self.refreshStatusbar()
            self.updateCenter()
            
            # Refresh box tabs to match current boxes
            # This ensures tabs are synchronized with the actual boxes for this image
            self.addBoxTabs()
            
            # Process the image
            # If image-level cache exists, skip ProjectionProcessor.process() (expensive full pipeline)
            # and only run UI/data export steps using cached results.
            self.processImage(use_cache=image_cache_loaded)
            
        except Exception as e:
            import traceback
            QMessageBox.critical(
                self, 
                "Error Processing Image", 
                f"Failed to process image: {str(e)}\n\n{traceback.format_exc()}"
            )
            print(f"Error in _on_image_data_ready: {e}")
            traceback.print_exc()
    
    def _on_needs_reprocess(self):
        """
        Handle workspace needsReprocess signal.
        
        Called when settings change (e.g., quadrant folded checkbox, center, rotation).
        Performs PT-specific UI updates before reprocessing.
        """
        if self.projProc is None:
            return
        
        # Update center display
        self.updateCenter()
        
        # Refresh box tabs (needed when quadrant folded state changes)
        self.addBoxTabs()
        
        # Reprocess image with new settings
        self.processImage()
        
        # Update image display
        self.updateImage()

    # ==================== Navigation Interface (For Child Components) ====================
    
    def prevClicked(self):
        """
        Navigate to previous image.
        
        Public interface method for child components (e.g., ProjectionBoxTab).
        ProjectionBoxTab has prev/next buttons in each box tab that allow users to
        navigate between images without switching back to the main image tab.
        
        This method provides a stable interface that child components can depend on,
        while hiding the internal implementation details (workspace/navigator structure).
        """
        if hasattr(self, 'workspace') and self.workspace.navigator:
            self.workspace.navigator.navigate_prev()
    
    def nextClicked(self):
        """
        Navigate to next image.
        
        Public interface method for child components (e.g., ProjectionBoxTab).
        ProjectionBoxTab has prev/next buttons in each box tab that allow users to
        navigate between images without switching back to the main image tab.
        
        This method provides a stable interface that child components can depend on,
        while hiding the internal implementation details (workspace/navigator structure).
        """
        if hasattr(self, 'workspace') and self.workspace.navigator:
            self.workspace.navigator.navigate_next()
    
    def prevFileClicked(self):
        """
        Navigate to previous H5 file.
        
        Public interface method for child components that need H5 file navigation.
        Used when working with HDF5 files that contain multiple images.
        """
        if hasattr(self, 'workspace') and self.workspace.navigator:
            self.workspace.navigator.navigate_prev_file()
    
    def nextFileClicked(self):
        """
        Navigate to next H5 file.
        
        Public interface method for child components that need H5 file navigation.
        Used when working with HDF5 files that contain multiple images.
        """
        if hasattr(self, 'workspace') and self.workspace.navigator:
            self.workspace.navigator.navigate_next_file()

    def initMaskThreshold(self, projProc):
        """
        Initialize mask threshold UI state for the current image.

        Note: Intensity UI (min/max, steps, labels, persist) is managed by ImageViewerWidget
        via DisplayOptionsPanel.update_from_image() during image display.
        """
        img = projProc.orig_img
        self.syncUI = True

        self.maskThresSpnBx.valueChanged.disconnect(self.maskThresChanged)  # Avoid an extra run at launch
        if self.projProc.state.mask_thres is not None:
            self.maskThresSpnBx.setValue(self.projProc.state.mask_thres)
        elif self.maskThresSpnBx.value() == -999:
            self.maskThresSpnBx.setValue(getMaskThreshold(img))
        self.maskThresSpnBx.valueChanged.connect(self.maskThresChanged)
        # self.maskThresSpnBx.setRange(img.min(), img.max())
        # Note: blank_mask state now managed by ProcessingWorkspace
        self.syncUI = False

    def updateCenter(self, refit=False):
        """
        Update the image center (simplified - center is now managed by ImageData).
        
        This method now mainly syncs legacy variables (centerx/centery) from
        the ImageData's dynamic center property and ensures checkbox state is correct.
        """
        if self.projProc is None:
            return
        
        # Get center from ImageData (handles quadrant_folded, manual, computed automatically)
        center = self.projProc.center
        self.centerx = center[0]
        self.centery = center[1]

        self.projProc.cache = None
        self.refit = refit

    def processImage(self, use_cache: bool = False):
        """
        Process Image by applying settings and calling process() of ProjectionProcessor.
        Then, write data and update UI.
        
        Args:
            use_cache: If True, use cached computation results (rotation still applied)
        """
        if self.projProc is None:
            return
        QApplication.setOverrideCursor(Qt.WaitCursor)
        QApplication.processEvents()
        try:
            # Apply current settings to processor state
            self.applySettings()
            # Process with cache flag - rotation always applied, computation conditionally skipped
            self.projProc.process(use_existing_cache=use_cache)

        except Exception:
            QApplication.restoreOverrideCursor()
            errMsg = QMessageBox()
            errMsg.setText('Unexpected error')
            msg = 'Please report the problem with error message below and the input image\n\n'
            msg += "Error : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
            errMsg.setInformativeText(msg)
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.setFixedWidth(300)
            errMsg.exec_()
            raise
        
        print("after")
        print("Center:", self.projProc.center)
        if hasattr(self, 'projProc') and self.projProc:
            # Update center display with coordinates from ProjectionProcessor
            self.workspace._center_widget.update_current_center(
                self.projProc.center  # Current center coordinates
            )
        
        # === Update folder cache with refined results ===
        self._update_folder_cache_from_results()
        
        # Note: Keep self.boxes as folder template (config-only, peaks = first half)
        # Don't sync full results back to maintain clean state
        
        self.resetUI()
        self.refreshStatusbar()

        # Use projProc.boxes for CSV columns (has full peaks info from processing)
        self.csvManager.setColumnNames(self.projProc.boxes)
        self.csvManager.writeNewData(self.projProc)
        self.exportHistograms()
        QApplication.restoreOverrideCursor()

    def thread_done(self, projProc):
        if self.lock is not None:
            self.lock.acquire()
        self.projProc = projProc
        
        self.onProcessingFinished()
        
        if self.lock is not None:
            self.lock.release()
        
    # placeholder method 
    def thread_finished(self):
        print("thread finished")
        if self.progressBar.isVisible():
            self.tasksDone += 1
            self.progressBar.setValue(int(100. / self.numberOfFiles * self.tasksDone))
        
        if not self.tasksQueue.empty():
            self.startNextTask()
        else:
            if self.threadPool.activeThreadCount() == 0:
                print("all threads are done")
                self.progressBar.setVisible(False)
                self.csvManager.sortCSV()
    
    def addTask(self, i):
        """Add a processing task to the queue (for batch processing)."""
        params = ProjectionParams(
            settings=self.getSettings(),
            index=i,
            file_manager=self.file_manager,
            gui=self
        )
        self.tasksQueue.put(params)
        
        self.startNextTask()
            
    def startNextTask(self):
        while not self.tasksQueue.empty() and self.threadPool.activeThreadCount() < self.threadPool.maxThreadCount() / 2:
            params = self.tasksQueue.get()
            self.currentTask = Worker.fromParams(params)
            self.currentTask.signals.result.connect(self.thread_done)
            self.currentTask.signals.finished.connect(self.thread_finished)
            self.threadPool.start(self.currentTask)
    
    
    def onProcessingFinished(self):
        # Update folder cache with refined results
        self._update_folder_cache_from_results()
        
        # Note: Keep self.boxes as folder template (config-only, peaks = first half)
        # Don't sync full results back to maintain clean state
        
        self.resetUI()
        self.refreshStatusbar()
        
        # Use projProc.boxes for CSV columns (has full peaks info from processing)
        self.csvManager.setColumnNames(self.projProc.boxes)
        self.csvManager.writeNewData(self.projProc)
        self.exportHistograms()
        
        QApplication.restoreOverrideCursor()
        self.currentTask = None
        print("all done")
        

    def exportHistograms(self):
        """
        Export both original histograms and background subtracted histograms if Export All Projections is checked
        :return:
        """
        if self.exportChkBx.isChecked() and self.projProc:
            path = fullPath(self.dir_path, os.path.join('pt_results', '1d_projections'))
            createFolder(path)
            fullname = str(self.projProc.filename)
            filename, _ = splitext(fullname)
            orig_hists = {name: box.hist for name, box in self.projProc.boxes.items() if box.hist is not None}
            subtr_hists = {name: box.subtracted_hist for name, box in self.projProc.boxes.items() if box.subtracted_hist is not None}

            for k in orig_hists.keys():
                hist = orig_hists[k]
                xs = np.arange(len(hist))
                f = open(fullPath(path, filename+'_box_'+str(k)+'_original.txt'), 'w')
                coords = zip(xs, hist)
                f.write("\n".join(list(map(lambda c : str(c[0])+"\t"+str(c[1]), coords))))
                if k in subtr_hists:
                    sub_hist = subtr_hists[k]
                    f = open(fullPath(path, filename+'_box_' + str(k) + '_subtracted.txt'), 'w')
                    coords = zip(xs, sub_hist)
                    f.write("\n".join(list(map(lambda c: str(c[0]) + "\t" + str(c[1]), coords))))

    def _expand_peaks_mirrored(self, box: ProcessingBox):
        """
        Expand peaks in a ProcessingBox by mirroring the first half.
        
        User-selected peaks (first half) are mirrored to create symmetric peaks.
        For example: [10, 20, 30] -> [10, 20, 30, -10, -20, -30]
        
        Modifies box.peaks in-place.
        
        Note: hull_range is NOT mirrored because it's already a symmetric concept:
        hull_range = (start, end) means distance from center, applied to both sides.
        
        Args:
            box: ProcessingBox with user-selected peaks (first half only)
        """
        if not box.peaks:
            print(f"  [_expand_peaks_mirrored] Box '{box.name}': No peaks to expand")
            return
        
        # Mirror peaks: first half stays, add mirrored second half
        user_peaks = box.peaks  # Already only the first half
        mirrored_peaks = [-p for p in user_peaks]
        box.peaks = user_peaks + mirrored_peaks
        
        print(f"  [_expand_peaks_mirrored] Box '{box.name}': {len(user_peaks)} user peaks → {len(box.peaks)} total peaks")
        print(f"    User selected: {user_peaks}")
        print(f"    After mirroring: {box.peaks}")
        
        # hull_range doesn't need mirroring - it's already symmetric
        # (start, end) defines distance ranges from center for both positive and negative sides
    
    def _box_to_dict(self, box: ProcessingBox) -> dict:
        return {
            'name': box.name,
            'coordinates': box.coordinates,
            'type': box.type,
            'bgsub': box.bgsub,
            'peaks': box.peaks,
            'merid_bg': box.merid_bg,
            'hull_range': box.hull_range,
            'param_bounds': box.param_bounds,
            'use_common_sigma': box.use_common_sigma,
            'peak_tolerance': box.peak_tolerance,
            'sigma_tolerance': box.sigma_tolerance,
        }

    def _dict_to_box(self, box_dict: dict) -> ProcessingBox:
        # Convert lists back to tuples where needed (JSON doesn't preserve tuple type)
        coordinates = box_dict['coordinates']
        if isinstance(coordinates, list):
            coordinates = tuple(tuple(item) if isinstance(item, list) else item 
                              for item in coordinates)
        
        hull_range = box_dict.get('hull_range')
        if hull_range is not None and isinstance(hull_range, list):
            hull_range = tuple(hull_range)
        
        return ProcessingBox(
            name=box_dict['name'],
            coordinates=coordinates,
            type=box_dict['type'],
            bgsub=box_dict['bgsub'],
            peaks=box_dict.get('peaks', []),
            merid_bg=box_dict.get('merid_bg', False),
            hull_range=hull_range,
            param_bounds=box_dict.get('param_bounds', {}),
            use_common_sigma=box_dict.get('use_common_sigma', False),
            peak_tolerance=box_dict.get('peak_tolerance', 2.0),
            sigma_tolerance=box_dict.get('sigma_tolerance', 100.0),  # Default 100% (changed from 5.0)
        )

    def saveBoxesConfig(self):
        """
        Save folder-level box configuration (without processing results).
        This serves as a template for new images in the folder.
        """
        cache = {
            'boxes': {
                name: self._box_to_dict(box)
                for name, box in self.boxes.items()
            },
            'centerx': self.centerx,
            'centery': self.centery,
            'mask_thres': self.maskThresSpnBx.value()
        }
        cache_dir = fullPath(self.dir_path, 'pt_cache')
        createFolder(cache_dir)
        cache_file = fullPath(cache_dir, 'boxes_config.json')
        with open(cache_file, "w") as f:
            json.dump(cache, f, indent=2)
 
    def loadBoxesConfig(self):
        """
        Load folder-level box configuration from cache.
        Returns dict with 'boxes', 'centerx', 'centery', 'mask_thres' or None.
        """
        cache_file = fullPath(fullPath(self.dir_path, 'pt_cache'), 'boxes_config.json')
        if exists(cache_file):
            try:
                with open(cache_file, "r") as f:
                    cache = json.load(f)
                if cache is not None:
                    cache['boxes'] = {
                        name: self._dict_to_box(box_dict)
                        for name, box_dict in cache['boxes'].items()
                    }
                    return cache
            except Exception as e:
                print(f"Warning: Failed to load box config cache: {e}")
        return None

    def saveSettings(self):
        """
        Save current settings to json file.
        Includes box configurations, center, mask threshold, and calibration parameters.
        """
        if self.projProc is None:
            return
        
        cache = {
            'boxes': {
                name: self._box_to_dict(box)
                for name, box in self.boxes.items()
            },
            'centerx': self.centerx,
            'centery': self.centery,
            'mask_thres': self.maskThresSpnBx.value()
        }
        
        # Add calibration settings if available
        # These are needed by headless mode to reproduce the same processing
        calSettings = self.workspace.calibration_settings if hasattr(self, 'workspace') else None
        if calSettings is not None:
            # Calibration type (determines which formula to use)
            if 'type' in calSettings:
                cache['type'] = calSettings['type']
            
            # Image-based calibration parameters (using silver behenate)
            if 'radius' in calSettings:
                cache['radius'] = calSettings['radius']
            if 'silverB' in calSettings:
                cache['silverB'] = calSettings['silverB']
            
            # Instrument-based calibration parameters
            if 'lambda' in calSettings:
                cache['lambda'] = calSettings['lambda']
            if 'sdd' in calSettings:
                cache['sdd'] = calSettings['sdd']
            if 'pixel_size' in calSettings:
                cache['pixel_size'] = calSettings['pixel_size']
            
            # Calibration center (may differ from centerx/centery)
            if 'center' in calSettings:
                cache['center'] = calSettings['center']
            
            # Detector type
            if 'detector' in calSettings:
                cache['detector'] = calSettings['detector']

        filename = getSaveFile(os.path.join("musclex", "settings", "ptsettings.json"), None)
        if filename != "":
            with open(filename, "w") as f:
                json.dump(cache, f, indent=2)
    
    def loadSettings(self):
        """
        Load template from a JSON file and apply to current folder.
        This will:
        1. Copy the selected template to current folder's pt_cache/boxes_config.json
        2. Load and apply the configuration to runtime variables
        3. Update UI and visualizations
        """
        # Let user select a template file
        filename, _ = QFileDialog.getOpenFileName(
            self,
            "Load Template",
            os.path.join("musclex", "settings"),
            "JSON Files (*.json)"
        )
        
        if filename == "":
            return
        
        if not self.dir_path:
            print("Warning: No directory loaded")
            return
        
        # Copy template to current folder's cache
        cache_dir = fullPath(self.dir_path, 'pt_cache')
        createFolder(cache_dir)
        cache_file = fullPath(cache_dir, 'boxes_config.json')
        shutil.copyfile(filename, cache_file)
        
        # Load configuration
        cache = self.loadBoxesConfig()
        if cache is None:
            print("Warning: Failed to load template configuration")
            return
        
        # Clear existing boxes
        self.boxes = {}
        self.boxes_on_img = {}
        
        # Apply configuration to runtime variables
        self.boxes = cache['boxes']
        self.centerx = cache.get('centerx')
        self.centery = cache.get('centery')
        
        # Update mask threshold spinbox
        if 'mask_thres' in cache:
            self.maskThresSpnBx.setValue(cache['mask_thres'])
        
        # Sync with projProc if it exists
        if self.projProc is not None:
            for name, box in self.boxes.items():
                # Create a copy with unexpanded peaks
                box_copy = ProcessingBox(
                    name=box.name,
                    coordinates=box.coordinates,
                    type=box.type,
                    bgsub=box.bgsub,
                    peaks=box.peaks.copy() if box.peaks else [],
                    merid_bg=box.merid_bg,
                    hull_range=box.hull_range,
                    param_bounds=box.param_bounds.copy() if box.param_bounds else {},
                    use_common_sigma=box.use_common_sigma,
                    peak_tolerance=box.peak_tolerance,
                    sigma_tolerance=box.sigma_tolerance,
                )
                # Expand peaks by mirroring (only in projProc copy)
                self._expand_peaks_mirrored(box_copy)
                # Update projProc's boxes to match
                self.projProc.state.boxes[name] = box_copy
            # Update mask threshold
            if 'mask_thres' in cache:
                self.projProc.state.mask_thres = cache['mask_thres']
        
        # Create visual representations for boxes
        for name, box in self.boxes.items():
            self.boxes_on_img[name] = self.genBoxArtists(name, box.coordinates, box.type)
        
        # Update CSV manager with new boxes
        if self.csvManager is not None:
            self.csvManager = PT_CSVManager(self.dir_path, self.boxes)
        
        # Redraw image with new boxes
        if hasattr(self, 'displayImgCanvas'):
            self.displayImgCanvas.draw_idle()
        
        print(f"Template loaded: {filename}")
        print(f"  - Boxes applied: {len(self.boxes)}")
        
        # If image is loaded, rebuild tabs and reprocess
        if self.projProc is not None:
            self.addBoxTabs()
            self.processImage()
            print("  ✓ Image reprocessed with new template")
                



        
    def _extract_refined_peaks(self, proc_box: ProcessingBox):
        """
        Extract refined peak positions from fit results.
        
        The fit results contain p_0, p_1, p_2... which are the optimal peak positions
        found by the fitter (relative to centerX).
        
        IMPORTANT: Returns only the FIRST HALF of refined peaks (user-selected side).
        The fitting preserves peak order, so the first half corresponds to the original
        user-selected peaks.
        
        NOTE: This method assumes proc_box.fit_results is not None.
        Caller must check this before calling.
        
        Returns:
            List of refined peak positions (first half only)
        """
        fit_results = proc_box.fit_results
        refined_peaks = []
        
        # Extract all p_i parameters
        i = 0
        while f'p_{i}' in fit_results:
            peak_pos = fit_results[f'p_{i}']
            refined_peaks.append(float(peak_pos))
            i += 1
        
        # Return only the first half (user-selected side)
        result = refined_peaks[:len(refined_peaks)//2]
        print(f"  [_extract_refined_peaks] Box '{proc_box.name}': {len(refined_peaks)} refined peaks → {len(result)} (first half)")
        print(f"    All refined: {refined_peaks}")
        print(f"    Saved (first half): {result}")
        return result
    
    def _update_folder_cache_from_results(self):
        """
        Update folder-level cache with refined results from current image processing.
        
        Key updates:
        1. peaks: Use refined peak positions from fit results
        2. param_bounds: Parameter bounds learned during fitting
        3. Other configuration parameters
        
        This allows subsequent images to benefit from improved initial values.
        """
        for name, proc_box in self.projProc.boxes.items():
            
            # Skip boxes without fit results
            if proc_box.fit_results is None:
                continue
            
            # Extract refined peak positions
            refined_peaks = self._extract_refined_peaks(proc_box)
            
            if name not in self.boxes:
                # New box: add to folder cache with refined peaks
                self.boxes[name] = ProcessingBox(
                    name=proc_box.name,
                    coordinates=proc_box.coordinates,
                    type=proc_box.type,
                    bgsub=proc_box.bgsub,
                    peaks=refined_peaks,  # Directly use refined peaks (already first half)
                    merid_bg=proc_box.merid_bg,
                    hull_range=proc_box.hull_range,
                    param_bounds=proc_box.param_bounds.copy() if proc_box.param_bounds else {},
                    use_common_sigma=proc_box.use_common_sigma,
                    peak_tolerance=proc_box.peak_tolerance,
                    sigma_tolerance=proc_box.sigma_tolerance,
                )
                print(f"Added new box to folder cache: {name}")
                print(f"  - Refined peaks: {refined_peaks}")
            else:
                # Existing box: update configuration
                folder_box = self.boxes[name]
                
                # Update basic configuration
                folder_box.coordinates = proc_box.coordinates
                folder_box.type = proc_box.type
                folder_box.bgsub = proc_box.bgsub
                folder_box.merid_bg = proc_box.merid_bg
                folder_box.hull_range = proc_box.hull_range
                
                # === Key: Update with refined peak positions ===
                old_peaks = folder_box.peaks
                folder_box.peaks = refined_peaks
                
                # Show refinement progress
                if old_peaks != refined_peaks and len(old_peaks) == len(refined_peaks):
                    print(f"Box {name}: Refined peaks")
                    for i, (old, new) in enumerate(zip(old_peaks, refined_peaks)):
                        delta = new - old
                        print(f"  Peak {i}: {old:.2f} → {new:.2f} (Δ{delta:+.3f})")
                
                # Update fitting parameters
                folder_box.param_bounds = proc_box.param_bounds.copy() if proc_box.param_bounds else {}
                folder_box.use_common_sigma = proc_box.use_common_sigma
                folder_box.peak_tolerance = proc_box.peak_tolerance
                folder_box.sigma_tolerance = proc_box.sigma_tolerance
        
        # Save updated configuration to disk
        self.saveBoxesConfig()

    def applySettings(self):
        """
        Apply current UI settings directly to ProjectionProcessor state.
        
        Note: This method writes settings directly to projProc.state instead of
        returning a settings dict. Box configurations are managed by ProcessingBox
        objects in projProc.boxes.
        """
        if self.projProc is None:
            return
        
        # Mask threshold
        self.projProc.state.mask_thres = self.maskThresSpnBx.value()
        
        # Handle refit flag - clear fit results directly
        if self.refit:
            for box in self.projProc.boxes.values():
                box.clear_results(from_stage='fit')
            self.refit = False

        # Calibration settings
        calSettings = self.workspace.calibration_settings if hasattr(self, 'workspace') else None
        if calSettings is not None:
            if 'type' in calSettings:
                if calSettings["type"] == "img":
                    self.projProc.state.lambda_sdd = calSettings["silverB"] * calSettings["radius"]
                elif calSettings["type"] == "cont":
                    self.projProc.state.lambda_sdd = 1. * calSettings["lambda"] * calSettings["sdd"] / calSettings["pixel_size"]

            if "detector" in calSettings:
                self.projProc.state.detector = calSettings["detector"]
    
    def getSettings(self):
        """
        Get current settings for display purposes (batch processing dialogs).
        
        Note: For processing, use applySettings() instead which writes directly to state.
        This method is kept for backward compatibility with batch processing UI.
        """
        settings = {}
        settings['mask_thres'] = self.maskThresSpnBx.value()
        
        calSettings = self.workspace.calibration_settings if hasattr(self, 'workspace') else None
        if calSettings is not None:
            if 'type' in calSettings:
                if calSettings["type"] == "img":
                    settings["lambda_sdd"] = calSettings["silverB"] * calSettings["radius"]
                elif calSettings["type"] == "cont":
                    settings["lambda_sdd"] = 1. * calSettings["lambda"] * calSettings["sdd"] / calSettings["pixel_size"]
        
        return settings

    def refreshStatusbar(self):
        """
        Update status bar with image information.
        
        Note: File path display is now automatically handled by BaseGUI
        through ImageNavigatorWidget.filePathTextReady signal.
        """
        if self.projProc is None:
            return
        
        # File path update moved to BaseGUI._setup_file_path_updates()
        # The navigator automatically emits filePathTextReady when images change
        
        # Update image details
        img = self.projProc.orig_img
        calSettings = self.workspace.calibration_settings if hasattr(self, 'workspace') else None
        if calSettings:
            self.imgDetailOnStatusBar.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype) + " " + "(Image Calibrated)")
        else:
            self.imgDetailOnStatusBar.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype))
        self.imgCoordOnStatusBar.setText("")
        # QApplication.processEvents()

    def setLeftStatus(self, s):
        """
        Set text on status bar on the left
        :param s: input text (str)
        """
        self.left_status.setText(s)
        # QApplication.processEvents()

    def resetUI(self):
        """
        Refresh all tabs
        """
        self.function = None
        # Deactivate placeholder tool when resetting UI (e.g., ESC pressed)
        if hasattr(self, 'image_viewer') and self.image_viewer:
            self.image_viewer.tool_manager.deactivate_tool('pt_operation')
        # self.graph_zoom = None
        QApplication.restoreOverrideCursor()
        self.selectPeaksButton.setText("Select Approximate Peak Locations")
        self.addBoxButton.setText("Add Axis Aligned Box")
        self.addOrientedBoxButton.setText("Add Oriented Box")
        self.addCenterOrientedBoxButton.setText("Add Center Oriented Box")

        for b in self.checkableButtons:
            b.setChecked(False)
        for k in self.update_plot:
            self.update_plot[k] = True
        for i in range(1, self.tabWidget.count()):
            tab = self.tabWidget.widget(i)
            tab.clearFlags()

        self.updateUI()

    def updateUI(self):
        """
        Update the UI
        """
        if self.projProc is not None and not self.syncUI:
            print("SELF.PROJPROC IS NOT NONE AND NO SYNCUI")
            ind = self.tabWidget.currentIndex()
            if ind == 0:
                # if image tab is selected
                self.updateImageTab()
            else:
                tab = self.tabWidget.widget(ind)
                tab.updateUI()

    def updateImageTab(self):
        """
        Draw all UI in image tab using ImageViewerWidget API.
        
        Note: Image rotation is now performed in ProjectionProcessor.process(),
        so we directly display self.projProc.orig_img which is already rotated if needed.
        This simplifies the display logic and ensures GUI shows the processed image.
        """

        if self.projProc is None or self.syncUI or not self.update_plot['img']:
            return
        
        # Get the image to display (already rotated in process() if needed)
        img = self.projProc.orig_img
        
        # Use ImageViewerWidget's API to display image
        # - Automatically uses vmin/vmax/log_scale/colormap from display_panel
        # - Automatically preserves zoom (if not first time)
        # - Sets _current_image so intensity changes work
        self.image_viewer.display_image(img)
        
        # Get axes for drawing overlays (boxes, peaks, center)
        ax = self.displayImgAxes
        
        # Draw boxes
        if len(self.boxes.keys()) > 0:
            self.selectPeaksGrp.setEnabled(True)
            if self.boxesChkBx.isChecked():
                for name, aritists in self.boxes_on_img.items():
                    ax.add_patch(aritists['rect'])
                    ax.add_artist(aritists['text'])

            # Draw peaks (from fit results - subpixel accuracy)
            if self.peaksChkBx.isChecked():
                for name in self.projProc.boxes.keys():
                    box_obj = self.projProc.boxes[name]
                    
                    # Use fit_results for accurate peak positions (subpixel precision)
                    if not box_obj.fit_results:
                        continue
                    
                    center = self.projProc.center
                    centerx = center[0]
                    centery = center[1]
                    box = box_obj.coordinates
                    
                    # Extract all p_i from fit_results (already includes both sides)
                    i = 0
                    while f'p_{i}' in box_obj.fit_results:
                        p_rel = box_obj.fit_results[f'p_{i}']  # Relative to center (float)
                        
                        if box_obj.type == 'h':
                            abs_x = centerx + p_rel  # Absolute position (subpixel)
                            ax.plot((abs_x, abs_x), box[1], color='m', linewidth=1)
                        elif box_obj.type == 'oriented':
                            # For oriented boxes, p_rel is relative to box center
                            box_center = box[6]  # (cx, cy)
                            angle = np.radians(box[5])
                            
                            # Calculate edge points for this peak
                            edge_1 = rotatePoint(box_center, (box_center[0] + p_rel, box[1][0]), angle)
                            edge_2 = rotatePoint(box_center, (box_center[0] + p_rel, box[1][1]), angle)
                            ax.plot((edge_1[0], edge_2[0]), (edge_1[1], edge_2[1]), color='r', linewidth=1)
                        else:  # vertical
                            abs_y = centery + p_rel  # Absolute position (subpixel)
                            ax.plot(box[0], (abs_y, abs_y), color='r', linewidth=1)
                        
                        i += 1

        # Draw center circle
        if self.centerChkBx.isChecked():
            center = self.projProc.center
            circle = plt.Circle(center, 10, color='g')
            ax.add_patch(circle)

        # Update calibration dialog center values
        center = self.projProc.center

        
        # Apply layout and redraw to show overlays
        # Note: Zoom is automatically managed by ImageViewerWidget.display_image()
        self.displayImgFigure.tight_layout()
        self.displayImgCanvas.draw()

    def doubleZoomToOrigCoord(self, x, y):
        """
        Compute the new x and y for double zoom to orig coord
        """
        M = [[1/10, 0, 0], [0, 1/10, 0],[0, 0, 1]]
        dzx, dzy = self.doubleZoomPt
        x, y, _ = np.dot(M, [x, y, 1])
        newX = dzx -10 + x
        newY = dzy - 10 + y
        return (newX, newY)

    def drawPerpendiculars(self):
        """
        Draw perpendiculars on the image
        """
        ax = self.displayImgAxes
        points = self.chordpoints
        self.chordLines = []
        for i, p1 in enumerate(points):
            for p2 in points[i + 1:]:
                slope, cent = getPerpendicularLineHomogenous(p1, p2)
                if slope == float('inf'):
                    y_vals = np.array(ax.get_ylim())
                    x_vals = cent[0] + np.zeros(y_vals.shape)
                    self.chordLines.append([slope, cent[0]])
                else:
                    x_vals = np.array(ax.get_xlim())
                    y_vals = (x_vals - cent[0]) * slope + cent[1]
                    self.chordLines.append([slope, cent[1] - slope * cent[0]])
                ax.plot(x_vals, y_vals, linestyle='dashed', color='b')

    def showAbout(self):
        """
        Display About Dialog
        """
        msgBox = QMessageBox()
        msgBox.setWindowTitle("About")
        msgBox.setTextFormat(Qt.RichText)
        msgBox.setText("<br><br><br>" +
                       "Projection Traces is running under" +
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
