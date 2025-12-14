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
import copy
import pickle
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
from ..modules.ProjectionProcessor import ProjectionProcessor
from ..ui.ProjectionBoxTab import ProjectionBoxTab
from ..CalibrationSettings import CalibrationSettings
from ..csv_manager import PT_CSVManager
from .ImageMaskTool import ImageMaskerWindow
from .DoubleZoomGUI import DoubleZoom
from .pyqt_utils import *
from .base_gui import BaseGUI
from .widgets import ProcessingWorkspace

class ProjectionParams:
    def __init__(self, settings, index, file_manager, gui):
        """
        Parameters for Worker thread (batch processing).
        
        Args:
            settings: Processing settings dict
            index: Image index in file_manager.names
            file_manager: FileManager instance
            gui: Parent GUI instance (for accessing ProcessingWorkspace)
        """
        self.settings = settings
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
            
            self.projProc.process(self.settings)
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
    
    def __init__(self, all_boxes, box_types):
        super().__init__(None)
        self.all_boxes = all_boxes
        self.box_types = box_types
        self.setWindowTitle("Edit a Box")
        print(all_boxes)
        self.initUI()
        
    def initUI(self):
        self.boxLayout = QGridLayout(self)
        self.boxNames = QComboBox()
        for key in self.all_boxes.keys():
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
        box = self.all_boxes[box_name]
        if box_name in self.box_types.keys():
            if self.box_types[box_name] == 'oriented':
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
        super().__init__()
        
        # Initialize FileManager (BaseGUI no longer creates it)
        from ..utils.file_manager import FileManager
        self.file_manager = FileManager()
        
        # PT keeps current_file/imgList/dir_path for backward compatibility with getImgFiles()
        self.current_file = 0
        self.dir_path = ""
        self.filePath = ""  # current directory (required by ProcessingWorkspace)
        self.calSettings = None
        self.update_plot = {'img':True}
        self.imgList = []
        self.h5List = [] # if the file selected is an H5 file, regroups all the other h5 files names
        self.h5index = 0
        self.stop_process = False
        self.projProc = None
        self.syncUI = False
        self.csvManager = None
        self.masked = False
        self.img_zoom = None
        self.function = None
        self.allboxes = {}
        self.boxes_on_img = {}
        self.boxtypes = {}
        self.bgsubs = {}
        self.merid_bg = {}
        self.peaks = {}
        self.hull_ranges = {}
        self.centerx = None
        self.centery = None
        # Note: center_func removed - center state now managed by ImageData
        self.rotated = True
        self.rotationAngle = 0
        self.numberOfFiles = 0
        self.refit = False

        self.chordLines = []
        self.chordpoints = []
        # self.setStyleSheet(getStyleSheet())
        self.checkableButtons = []
        
        self.threadPool = QThreadPool()
        self.tasksQueue = Queue()
        self.loop = QEventLoop()
        self.currentTask = None
        self.worker = None 
        self.tasksDone = 0
        self.totalFiles = 1
        self.lock = Lock()
        
        self.initUI()
        self.setConnections()

        self.doubleZoomGUI = DoubleZoom(self.displayImgFigure)

        # NOTE: browseFile() removed - file browsing now handled by workspace.navigator

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
        self.imageTabLayout.addWidget(self.workspace)

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
        
        # Add PT-specific settings to right panel
        self._create_pattern_settings()
        self._create_box_settings()
        self._create_peaks_settings()
        self._create_export_settings()
        
        # Add navigation controls to right panel bottom (following QF pattern)
        self.right_panel.add_bottom_widget(self.navControls)
        
    def _create_pattern_settings(self):
        """Create pattern properties settings group"""

        # ===== PT-specific settings (QF checkbox, mask threshold) =====
        self.propGrp = QGroupBox("Pattern Settings (Optional)")
        self.propGrp.setEnabled(False)
        self.propLayout = QGridLayout(self.propGrp)

        self.qfChkBx = QCheckBox("Quadrant Folded?")
        self.qfChkBx.setChecked(True)
        self.maskThresSpnBx = QDoubleSpinBox()
        self.maskThresSpnBx.setMinimum(-10000)
        self.maskThresSpnBx.setMaximum(10000)
        self.maskThresSpnBx.setValue(-999)
        self.maskThresSpnBx.setKeyboardTracking(False)

        self.propLayout.addWidget(self.qfChkBx, 0, 0, 1, 2)
        self.propLayout.addWidget(QLabel('Mask Threshold:'), 1, 0, 1, 2)
        self.propLayout.addWidget(self.maskThresSpnBx, 1, 2, 1, 2)
        
        # Add to right panel
        self.workspace.right_panel.add_widget(self.propGrp)
    
    def _create_box_settings(self):
        """Create box selection settings group"""
        # Box selection
        self.boxGrp = QGroupBox("Add Boxes")
        self.boxGrp.setEnabled(False)
        self.boxesLayout = QVBoxLayout(self.boxGrp)
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
        saveSettingsAction = QAction('Save Current Settings', self)
        saveSettingsAction.setShortcut('Ctrl+S')
        saveSettingsAction.triggered.connect(self.saveSettings)

        menubar = self.menuBar()
        # menubar.setNativeMenuBar(False)
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(saveSettingsAction)

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
        """Custom finalization for PT"""
        self.resize(1300, 700)
        self.show()
    
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
        self.workspace.needsReprocess.connect(self.processImage)
        
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

        # Pattern Properties (PT-specific)
        self.qfChkBx.stateChanged.connect(self.qfChkBxClicked)


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

    def _on_status_text_requested(self, text: str):
        """
        Handle status text update request from ProcessingWorkspace.
        
        Args:
            text: Status text to display
        """
        if hasattr(self, 'statusReport'):
            self.statusReport.setText(text)

    def saveSettings(self):
        """
        save settings to json
        """
        if self.projProc is not None:
            settings = self.calSettings if self.calSettings is not None else {}
            cache = self.loadBoxesAndPeaks()
            if cache is not None:
                settings.update(cache)
                # for b in settings["boxes"].items():
                #     if isinstance(b[1][-1], np.ndarray):
                #         settings["boxes"][b[0]] = [x for x in b[1]]
                #         # settings["boxes"][b[0]][-1] = b[1][-1].tolist()
                #         settings["boxes"][b[0]].pop(-1)
                # print(settings["boxes"])
            filename = getSaveFile(os.path.join("musclex", "settings", "ptsettings.json"), None)
            if filename != "":
                with open(filename, 'w') as f:
                    json.dump(settings, f)

    # Note: blankChecked() and blankSettingClicked() removed - now handled by ProcessingWorkspace

    def maskThresChanged(self):
        """
        Trigger when Mask threshold is changed
        """
        if self.projProc is not None:
            self.projProc.info['hists'] = {}
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

    def qfChkBxClicked(self):
        """
        Triggered when the quadrant fold checkbox is clicked.
        
        Updates ImageData's quadrant_folded state to match user's selection.
        """
        # Update ImageData's quadrant_folded state
        if self.projProc and self.projProc._image_data:
            self.projProc._image_data.quadrant_folded = self.qfChkBx.isChecked()
        
        # Update rotated flag
        if self.qfChkBx.isChecked():
            self.rotated = False
        else:
            self.rotated = True
        
        self.updateCenter()
        print("qfbox")
        self.processImage()
        self.addBoxTabs()
        self.updateImage()

    def updatePeaks(self, name, peaks):
        """
        update peaks in box name
        :param name:
        :param peaks:
        :return:
        """
        self.peaks[name] = peaks

        # if name in self.hull_ranges:
        #     del self.hull_ranges[name]

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
                self.processH5Folder()
        else:
            self.stop_process = True

    def processFolder(self):
        """
        Process the folder selected
        """
        self.numberOfFiles = len(self.imgList)

        errMsg = QMessageBox()
        errMsg.setText('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'
        settings = self.getSettings()

        text += "\nCurrent Settings"
        for bn in self.allboxes.keys():
            text += "\n\n  - Box "+str(bn)+" : " + str(self.allboxes[bn])
            text += "\n     - Peaks : "
            if bn in self.peaks:
                text += str(self.peaks[bn])
            else:
                text += "-"

            if bn in self.bgsubs:
                text += '\n     - Background Subtraction : '
                if self.bgsubs[bn] == 0:
                    text += 'Fitting Gaussians'
                else:
                    text += 'Convex Hull'

            if bn in self.hull_ranges:
                text += '\n     - Convex Hull Range : '+str(self.hull_ranges[bn])

        if 'lambda_sdd' in settings:
            text += "\n  - Lambda Sdd : " + str(settings["lambda_sdd"])

        text += '\n\nAre you sure you want to process ' + str(
            self.numberOfFiles) + ' image(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            self.progressBar.setVisible(True)
            self.stop_process = False
            for i in range(self.numberOfFiles):
                if self.stop_process:
                    break
                self.addTask(i)
                # self.progressBar.setValue(int(100. / self.numberOfFiles * i))
                # QApplication.processEvents()
                # self.nextClicked()
            # self.startNextTask()
            # self.progressBar.setVisible(False)
        
        self.navControls.processFolderButton.setChecked(False)
        if self.ext in ['.h5', '.hdf5']:
            self.navControls.processFolderButton.setText("Process Current H5 File")
        else:
            self.navControls.processFolderButton.setText("Process Current Folder")

    def processH5Folder(self):
        """
        Process the folder selected
        """
        self.numberOfFiles = len(self.imgList)

        errMsg = QMessageBox()
        errMsg.setText('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'
        settings = self.getSettings()

        text += "\nCurrent Settings"
        for bn in self.allboxes.keys():
            text += "\n\n  - Box "+str(bn)+" : " + str(self.allboxes[bn])
            text += "\n     - Peaks : "
            if bn in self.peaks:
                text += str(self.peaks[bn])
            else:
                text += "-"

            if bn in self.bgsubs:
                text += '\n     - Background Subtraction : '
                if self.bgsubs[bn] == 0:
                    text += 'Fitting Gaussians'
                else:
                    text += 'Convex Hull'

            if bn in self.hull_ranges:
                text += '\n     - Convex Hull Range : '+str(self.hull_ranges[bn])

        if 'lambda_sdd' in settings:
            text += "\n  - Lambda Sdd : " + str(settings["lambda_sdd"])

        text += '\n\nAre you sure you want to process ' + str(
            len(self.h5List)) + ' H5 file(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            self.progressBar.setVisible(True)
            self.stop_process = False
            for _ in range(len(self.h5List)):
                for i in range(self.numberOfFiles):
                    if self.stop_process:
                        break
                    self.progressBar.setValue(int(100. / self.numberOfFiles * i))
                    QApplication.processEvents()
                    self.nextClicked()
                if self.stop_process:
                    break
                self.nextFileClicked()
            self.progressBar.setVisible(False)

        self.navControls.processH5Button.setChecked(False)
        self.navControls.processH5Button.setText("Process All H5 Files")

    def clearBoxes(self):
        """
        Clear all boxes
        """
        self.allboxes = {}
        self.boxtypes = {}
        self.boxes_on_img = {}
        self.bgsubs = {}
        self.merid_bg = {}
        self.peaks = {}
        self.hull_ranges = {}
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
                ax = self.displayImgAxes
                for line in list(ax.lines):
                    line.remove()
                self.displayImgCanvas.draw_idle()
            else:
                self.addBoxButton.setChecked(False)
                self.function = None
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
                ax = self.displayImgAxes
                for line in list(ax.lines):
                    line.remove()
                self.displayImgCanvas.draw_idle()
            else:
                self.addOrientedBoxButton.setChecked(False)
                self.function = None
                return

        elif self.addCenterOrientedBoxButton.isChecked() and not self.addOrientedBoxButton.isChecked():
            if self.function is None:
                # Start function
                self.addCenterOrientedBoxButton.setText("Done")
                self.setLeftStatus("Drag to select the rotation angle and length of the projection axis (ESC to cancel)")
                self.function = ['center_oriented_box']
                self.function.append(self.projProc.center)
                ax = self.displayImgAxes
                for line in list(ax.lines):
                    line.remove()
                self.displayImgCanvas.draw_idle()
            else:
                self.addOrientedBoxButton.setChecked(False)
                self.function = None
                return
        else:
            self.addCenterOrientedBoxButton.setChecked(False)
            self.addOrientedBoxButton.setChecked(False)
            self.resetUI()
            
    def editBoxes(self):
        if len(self.allboxes) > 0:
            dialog = EditBoxDetails(self.allboxes, self.boxtypes)
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
    
        box = self.allboxes[box_name]

        if self.boxtypes[box_name] == 'oriented':
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
            self.allboxes[box_name] = [bl_rot, br_rot, tl_rot, tr_rot, width, height, angle, (cx, cy)]
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

                self.allboxes[box_name] = [(x1, x2), (y1, y2)]
            
    def updateBoxDetails(self, box_name, height, width, height_mode, width_mode):
        box = self.allboxes[box_name]
        if self.boxtypes[box_name] == 'oriented':
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
                self.allboxes[box_name] = ((x1, x2), (y1, y2), new_bl, width, height, angle, (cx,cy))         
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
    
                self.allboxes[box_name] = [(x1, x2), (y1, y2)]
                
                
        for artist in self.boxes_on_img[box_name].values():
            artist.remove()
        del self.boxes_on_img[box_name]     
        self.boxes_on_img[box_name] = self.genBoxArtists(box_name, self.allboxes[box_name], self.boxtypes[box_name])
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
        Sets the H5 list of file and displays the right set of buttons depending on the file selected
        """
        if self.ext in ['.h5', '.hdf5']:
            for file in os.listdir(self.dir_path):
                if file.endswith(".h5") or file.endswith(".hdf5"):
                    self.h5List.append(file)
            self.h5index = self.h5List.index(os.path.split(file_name)[1])
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
                del self.allboxes[name]
                del self.boxtypes[name]
                del self.bgsubs[name]
                for artist in self.boxes_on_img[name].values():
                    artist.remove()
                del self.boxes_on_img[name]
                if name in self.peaks:
                    del self.peaks[name]
                if name in self.hull_ranges:
                    del self.hull_ranges[name]
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
        Add old box tabs
        :return:
        """
        self.removeAllTabs()

        for name in self.allboxes.keys():
            proj_tab = ProjectionBoxTab(self, name)
            self.tabWidget.addTab(proj_tab, "Box "+str(name))

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
            for name, box in self.allboxes.items():
                if self.boxtypes[name] == 'h' or self.boxtypes[name] == 'v':
                    breadth = 10
                    x1, x2, y1, y2 = box[0][0], box[0][1], box[1][0], box[1][1]
                    if (x1 - breadth <= x <= x1 + breadth or x2 - breadth <= x <= x2 + breadth) and \
                       y1 - breadth <= y <= y2 + breadth or x1 - breadth <= x <= x2 + breadth and \
                       (y1 - breadth <= y <= y1 + breadth or y2 - breadth <= y <= y2 + breadth):
                        self.function = ['box_move', name, (x, y)]
                        break
            else:
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
                boxDialog = BoxDetails(self.allboxes.keys())
                result = boxDialog.exec_()
                if result == 1:
                    name, bgsub, axis = boxDialog.getDetails()
                    self.allboxes[name] = ((x1, x2), (y1, y2))
                    self.boxtypes[name] = 'h' if axis == 0 else 'v'
                    self.boxes_on_img[name] = self.genBoxArtists(name, self.allboxes[name], self.boxtypes[name])
                    self.bgsubs[name] = bgsub
                    self.merid_bg[name] = True
                self.function = None
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
                    for i in range(len(ax.patches)-1,len(self.allboxes.keys())-1,-1):
                        ax.patches[i].remove()
                    # ax.patches = ax.patches[:len(self.allboxes.keys())]
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

                    boxDialog = BoxDetails(self.allboxes.keys(), oriented=True)
                    result = boxDialog.exec_()
                    if result == 1:
                        # get the image the box was selected on
                        if self.rotated:
                            img = self.projProc.getRotatedImage()
                        else:
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
                        self.allboxes[name] = ((x1, x2), (y1, y2), bottom_left, width, height*2, rot_angle, pivot) #, img)
                        self.boxtypes[name] = 'oriented'
                        self.boxes_on_img[name] = self.genBoxArtists(name, self.allboxes[name], self.boxtypes[name])
                        self.bgsubs[name] = bgsub
                        self.merid_bg[name] = True
                        self.function = None

                        self.addBoxTabs()
                        self.processImage()

        elif func[0] == "peaks":
            peaks = func[1]
            if len(self.allboxes.keys()) > 0:
                for name in self.allboxes.keys():
                    box = self.allboxes[name]
                    boxx = box[0]
                    boxy = box[1]
                    typ = self.boxtypes[name]
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
            if self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
                if 'center' in self.calSettings and self.calSettings['center'] is not None:
                    center = self.calSettings['center']
                else:
                    center = self.projProc.center
                q, unit = inverseNmFromCenter([x, y], center, self.calSettings['scale'])
                # constant = self.calSettings["silverB"] * self.calSettings["radius"]
                # calib_distance = mouse_distance * 1.0/constant
                # calib_distance = f"{calib_distance:.4f}"
            if x < img.shape[1] and y < img.shape[0]:
                if self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
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
                    for i in range(len(ax.patches)-1,len(self.allboxes.keys())-1,-1):
                        ax.patches[i].remove()
                    # ax.patches = ax.patches[:len(self.allboxes.keys())]
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
                    for i in range(len(ax.patches)-1,len(self.allboxes.keys())-1,-1):
                        ax.patches[i].remove()
                    # ax.patches = ax.patches[:len(self.allboxes.keys())]

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

        elif func[0] == 'box_move':
            box = self.boxes_on_img[func[1]]
            offset = (x - func[2][0], y - func[2][1])
            xy = box['rect'].get_xy()
            xy_t = box['text'].get_position()
            box['rect'].set_xy((xy[0] + offset[0], xy[1] + offset[1]))
            box['text'].set_position((xy_t[0] + offset[0], xy_t[1] + offset[1]))
            self.displayImgCanvas.draw_idle()
            func[2] = (x, y)


    def imgReleased(self, event):
        """
        Triggered when mouse released from image
        """
        if self.function is not None:
            func = self.function
            if func[0] == 'im_move':
                self.function = None
            if func[0] == 'box_move':
                box = self.boxes_on_img[func[1]]
                w, h = box['rect'].get_width(), box['rect'].get_height()
                xy = box['rect'].get_xy()
                self.allboxes[func[1]] = ((xy[0], xy[0] + w), (xy[1], xy[1] + h))
                self.function = None
                self.addBoxTabs()
                self.processImage()

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
        
        # Load cached boxes and peaks from previous session
        cache = self.loadBoxesAndPeaks()
        if cache is not None:
            self.allboxes = cache['boxes']
            self.peaks = cache['peaks']
            self.boxtypes = cache['types']
            self.bgsubs = cache['bgsubs']
            self.merid_bg = cache['merid_bg']
            self.hull_ranges = cache['hull_ranges']
            self.centerx = cache['centerx']
            self.centery = cache['centery']
            # Note: center_func removed - no longer needed with ImageData
            for name, box in self.allboxes.items():
                self.boxes_on_img[name] = self.genBoxArtists(name, box, self.boxtypes[name])
        else:
            self.allboxes = {}
            self.peaks = {}
        
        # Create CSV manager for this folder
        self.csvManager = PT_CSVManager(self.dir_path, self.allboxes, self.peaks)
        
        # Add box tabs for loaded boxes
        self.addBoxTabs()
        self.selectPeaksGrp.setEnabled(False)
        
        print(f"Folder loaded: {dir_path}")
        print(f"  - Boxes loaded: {len(self.allboxes)}")
        print(f"  - Peaks loaded: {len(self.peaks)}")
    
    def _on_image_data_ready(self, image_data):
        """
        Called when ImageData is ready for processing (main processing entry point).
        
        This replaces the old onImageChanged() method, following the QF pattern.
        The ProcessingWorkspace has already:
        - Loaded the image
        - Created ImageData with center/rotation/blank/mask settings
        - Applied manual settings if they exist
        
        Args:
            image_data: ImageData instance ready for processing
        """
        try:
            # Create ProjectionProcessor with the ready ImageData
            self.projProc = ProjectionProcessor(image_data)
            
            # Update ProcessingWorkspace display (settings panel, etc.)
            self.workspace.update_display(image_data)
            
            # Sync quadrant folded state from ImageData to checkbox
            self.syncQuadrantFoldedState(image_data)
            

            
            # Initialize UI for new image
            self.initMinMaxIntensities(self.projProc)
            self.refreshStatusbar()
            self.updateCenter()
            
            # Process the new image
            self.processImage()
            
        except Exception as e:
            import traceback
            QMessageBox.critical(
                self, 
                "Error Processing Image", 
                f"Failed to process image: {str(e)}\n\n{traceback.format_exc()}"
            )
            print(f"Error in _on_image_data_ready: {e}")
            traceback.print_exc()

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

    # ==================== Legacy Methods (Deprecated) ====================

    def onImageSelect(self, fullfilename):
        """
        [LEGACY] Triggered when a new image is selected via old browseFile() method.
        
        NOTE: This method is deprecated and will be removed in future versions.
        New code should use workspace.load_from_file() which triggers the proper
        signal chain: fileLoaded -> imageDataReady
        
        For now, we delegate to workspace to maintain compatibility.
        """
        # Delegate to workspace's load_from_file for proper signal flow
        self.workspace.load_from_file(fullfilename)
        
        # Sync legacy variables (PT still uses getImgFiles for fileList/ext)
        self.dir_path, self.imgList, self.current_file, self.fileList, self.ext = getImgFiles(fullfilename)
        
        if self.dir_path is not None and self.imgList is not None and self.imgList:
            # Hide left panel buttons and show canvas
            self.selectImageButton.setHidden(True)
            self.selectFolder.setHidden(True)
            self.displayImgCanvas.setHidden(False)
            self.updateLeftWidgetWidth()
            
            # Set H5 mode if needed
            self.setH5Mode(fullfilename)
            
            # NOTE: Image processing now happens via _on_image_data_ready() signal

    def _on_file_manager_changed(self):
        """
        [DEPRECATED] Hook method called when FileManager navigates to a new image.
        
        This method is no longer needed with ProcessingWorkspace integration.
        Navigation is now handled by ImageNavigatorWidget which emits imageDataReady.
        
        Keeping this as a no-op for now to avoid breaking any remaining references.
        """
        # Legacy sync for compatibility
        if self.file_manager.names:
            self.imgList = self.file_manager.names
            self.current_file = self.file_manager.current
            self.dir_path = self.file_manager.dir_path
        
        # NOTE: Image processing now happens via _on_image_data_ready() signal
        # This old path should not be used anymore

    def onImageChanged(self, first_run=False):
        """
        [DEPRECATED] Need to be called when image is changed.
        
        This method is deprecated in favor of _on_image_data_ready().
        The new signal-based approach (ProcessingWorkspace.imageDataReady) handles
        all image changes automatically.
        
        Keeping this for backward compatibility but it should not be called
        in the new flow.
        """
        # If this gets called, it means there's still old code using it
        print("WARNING: onImageChanged() is deprecated. Use _on_image_data_ready() via signals.")
        
        try:
            # Load image from FileManager
            img = self.file_manager.current_image
            img_name = self.file_manager.current_image_name
            
            # Create ImageData
            current_image_data = ImageData.from_settings_panel(
                img, self.dir_path, img_name, self.workspace
            )
            
            # Create ProjectionProcessor with ImageData
            self.projProc = ProjectionProcessor(current_image_data)
            
            # Update ProcessingWorkspace display
            if hasattr(self, 'workspace'):
                self.workspace.update_display(current_image_data)
            
            # Sync quadrant folded state from ImageData to checkbox
            self.syncQuadrantFoldedState(current_image_data)
            
            
            # Initialize UI
            self.initMinMaxIntensities(self.projProc)
            self.refreshStatusbar()
            self.updateCenter()
            
            # Process new image
            self.processImage()
            
        except Exception as e:
            import traceback
            infMsg = QMessageBox()
            infMsg.setText("Error")
            infMsg.setInformativeText(f"{str(e)}\n\n{traceback.format_exc()}")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            traceback.print_exc()

    def syncQuadrantFoldedState(self, image_data):
        """
        Sync quadrant folded state from ImageData to GUI checkbox.
        
        ImageData auto-detects quadrant folded from filename/metadata,
        this method just syncs the result to the UI.
        
        Args:
            image_data: ImageData object with auto-detected quadrant_folded state
        """
        if self.projProc is None:
            return
        
        # Get auto-detected state from ImageData
        quadrant_folded = image_data.is_quadrant_folded
        
        # Update checkbox state (disconnect signal to avoid triggering processImage again)
        self.qfChkBx.stateChanged.disconnect(self.qfChkBxClicked)
        self.qfChkBx.setChecked(quadrant_folded)
        self.qfChkBx.stateChanged.connect(self.qfChkBxClicked)

    def initMinMaxIntensities(self, projProc):
        """
        Set preference for image min & max intesity spinboxes, and initial their value
        :param projProc: current Projection Processor object
        :return:
        """
        img = projProc.orig_img
        self.syncUI = True
        self.minIntSpnBx.setMinimum(img.min())
        self.minIntSpnBx.setMaximum(img.max())
        self.maxIntSpnBx.setMinimum(img.min())
        self.maxIntSpnBx.setMaximum(img.max())
        self.minIntLabel.setText("Min Intensity <br/>("+str(img.min())+")")
        self.maxIntLabel.setText("Max Intensity <br/>("+str(img.max())+")")
        step = (img.max() - img.min()) * 0.07  # set spinboxes step as 7% of image range
        self.minIntSpnBx.setSingleStep(step)
        self.maxIntSpnBx.setSingleStep(step)

        # use cached values if they're available
        if not self.persistIntensity.isChecked():
            if "minInt" in self.projProc.info and "maxInt" in self.projProc.info:
                self.minIntSpnBx.setValue(self.projProc.info["minInt"])
                self.maxIntSpnBx.setValue(self.projProc.info["maxInt"])
            else:
                if self.maxIntSpnBx.value() == 0:
                    self.minIntSpnBx.setValue(img.min())  # init min intensity as min value
                    self.maxIntSpnBx.setValue(img.max() * 0.1)  # init max intensity as 20% of max value

        self.maskThresSpnBx.valueChanged.disconnect(self.maskThresChanged) # Avoid an extra run at launch
        if 'mask_thres' in self.projProc.info:
            self.maskThresSpnBx.setValue(self.projProc.info['mask_thres'])
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

    def processImage(self):
        """
        Process Image by getting all settings and call process() of ProjectionTraces object
        Then, write data and update UI
        """
        if self.projProc is None:
            return
        QApplication.setOverrideCursor(Qt.WaitCursor)
        QApplication.processEvents()
        settings = self.getSettings()
        try:
            self.projProc.process(settings)
            # self.currentTask = Worker.fromProjProc(self.projProc, settings)
            # self.currentTask.signals.result.connect(self.thread_done)
            # self.currentTask.signals.finished.connect(self.thread_finished)
            # self.threadPool.start(self.currentTask)
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
        self.resetUI()
        self.refreshStatusbar()
        self.cacheBoxesAndPeaks()

        self.csvManager.setColumnNames(self.allboxes, self.peaks)
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
        self.resetUI()
        self.refreshStatusbar()
        self.cacheBoxesAndPeaks()
        self.csvManager.setColumnNames(self.allboxes, self.peaks)
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
            orig_hists = self.projProc.info['hists']
            subtr_hists = self.projProc.info['subtracted_hists']

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

    def cacheBoxesAndPeaks(self):
        """
        Save the boxes and peaks in the cache file
        """
        cache = {
            'boxes' : self.allboxes,
            'peaks' : self.peaks,
            'types' : self.boxtypes,
            'bgsubs' : self.bgsubs,
            'merid_bg' : self.merid_bg,
            'hull_ranges' : self.hull_ranges,
            'centerx' : self.centerx,
            'centery' : self.centery,
            # Note: center_func removed - state now in ImageData
            'mask_thres' : self.maskThresSpnBx.value()
        }
        cache_dir = fullPath(self.dir_path, 'pt_cache')
        createFolder(cache_dir)
        cache_file = fullPath(cache_dir, 'boxes_peaks.info')
        pickle.dump(cache, open(cache_file, "wb"))

    def loadBoxesAndPeaks(self):
        """
        Load the boxes and peaks stored in the cache file, if it exists
        """
        cache_file = fullPath(fullPath(self.dir_path, 'pt_cache'), 'boxes_peaks.info')
        if exists(cache_file):
            cache = pickle.load(open(cache_file, "rb"))
            if cache is not None:
                return cache
        return None

    def getSettings(self):
        """
        Give the current settings
        :return: settings
        """
        settings = {}
        # add boxes
        settings['boxes'] = self.allboxes

        # add box types
        settings['types'] = self.boxtypes

        # add bgsub methods
        settings['bgsubs'] = self.bgsubs

        # add meridian bg on/off
        settings['merid_bg'] = self.merid_bg

        # add peaks location
        settings['peaks'] = self.peaks

        # add hull ranges
        settings['hull_ranges'] = self.hull_ranges

        # add GMM mode settings
        if hasattr(self, 'gmm_boxes'):
            settings['gmm_mode'] = self.gmm_boxes

        # add blank image and mask (from ProcessingWorkspace)
        if hasattr(self, 'workspace'):
            blank_mask_config = self.workspace.get_blank_mask_config()
            settings['blank_mask'] = blank_mask_config['apply_blank']
        else:
            settings['blank_mask'] = False

        settings['mask_thres'] = self.maskThresSpnBx.value()

        if self.refit:
            settings['refit'] = self.refit
            self.refit = False

        if self.rotated:
            settings['rotated'] = True
            if self.rotationAngle != 0:
                settings['rotationAngle'] = self.rotationAngle

        if self.calSettings is not None:
            if 'type' in self.calSettings:
                if self.calSettings["type"] == "img":
                    settings["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
                elif self.calSettings["type"] == "cont":
                    settings["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings["pixel_size"]
            
            # For quadrant folded images, never use calibrated center (always use geometric center)
            # For manual center, also don't override with calibrated center
            quadrant_folded = self.qfChkBx.isChecked()
            has_manual_center = (self.projProc and self.projProc._image_data and 
                                self.projProc._image_data.has_manual_center)
            if "center" in self.calSettings and not has_manual_center and not quadrant_folded:
                settings["center"] = self.calSettings["center"]
            
            if "detector" in self.calSettings:
                self.projProc.info["detector"] = self.calSettings["detector"]

        return settings

    def refreshStatusbar(self):
        """
        Set Left status bar to be image detail
        Set Right status bar to by image shape and type
        Clear pixel detail
        """
        if self.projProc is None:
            return
        self.setLeftStatus( "(" + str(self.current_file + 1) + "/" + str(len(self.imgList)) + ") " + fullPath(self.dir_path,
                                                                                            self.projProc.filename))
        img = self.projProc.orig_img
        if self.calSettings is not None and not self.calSettings:
            self.imgDetailOnStatusBar.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype))
        elif self.calSettings is not None and self.calSettings:
            self.imgDetailOnStatusBar.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype) + " " + "(Image Calibrated)")
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
        
        This method now uses self.image_viewer.display_image() instead of directly
        manipulating matplotlib axes, which enables automatic intensity UI updates
        and proper integration with the display panel.
        """

        if self.projProc is None or self.syncUI or not self.update_plot['img']:
            return
        
        # Get the image to display
        if self.rotated:
            img = self.projProc.getRotatedImage()
        else:
            img = self.projProc.orig_img
        img = np.flipud(img)
        
        # Use ImageViewerWidget's API to display image
        # - Automatically uses vmin/vmax/log_scale/colormap from display_panel
        # - Automatically preserves zoom (if not first time)
        # - Sets _current_image so intensity changes work
        self.image_viewer.display_image(img)
        
        # Get axes for drawing overlays (boxes, peaks, center)
        ax = self.displayImgAxes
        
        # Draw boxes
        if len(self.allboxes.keys()) > 0:
            self.selectPeaksGrp.setEnabled(True)
            if self.boxesChkBx.isChecked():
                for name, aritists in self.boxes_on_img.items():
                    ax.add_patch(aritists['rect'])
                    ax.add_artist(aritists['text'])

            # Draw peaks
            if self.peaksChkBx.isChecked():
                for name in self.peaks.keys():
                    center = self.projProc.center
                    centerx = center[0]
                    centery = center[1]
                    for p in self.peaks[name]:
                        if self.boxtypes[name] == 'h':
                            ax.plot((centerx - p, centerx - p), self.allboxes[name][1], color='m')
                            ax.plot((centerx + p, centerx + p), self.allboxes[name][1], color='m')
                        elif self.boxtypes[name] == 'oriented':
                            box = self.allboxes[name]
                            edge_1 = rotatePoint((box[6][0], box[6][1]), (box[6][0]-p, box[1][0]), np.radians(box[5]))
                            edge_2 = rotatePoint((box[6][0], box[6][1]), (box[6][0]-p, box[1][1]), np.radians(box[5]))
                            edge_3 = rotatePoint((box[6][0], box[6][1]), (box[6][0]+p, box[1][0]), np.radians(box[5]))
                            edge_4 = rotatePoint((box[6][0], box[6][1]), (box[6][0]+p, box[1][1]), np.radians(box[5]))
                            ax.plot((edge_1[0], edge_2[0]), (edge_1[1], edge_2[1]), color='r')
                            ax.plot((edge_3[0], edge_4[0]), (edge_3[1], edge_4[1]), color='r')
                        else:
                            ax.plot(self.allboxes[name][0], (centery - p, centery - p), color='r')
                            ax.plot(self.allboxes[name][0], (centery + p, centery + p), color='r')

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
