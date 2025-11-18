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
from matplotlib.colors import LogNorm, Normalize, ListedColormap
import matplotlib.pyplot as plt
import pandas as pd
from PIL import Image
from musclex import __version__
from PySide6.QtCore import QRunnable, QThreadPool, QEventLoop, Signal, QTimer
from queue import Queue
import fabio
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.QuadrantFolder import QuadrantFolder
from ..csv_manager.QF_CSVManager import QF_CSVManager
from .pyqt_utils import *
from .BlankImageSettings import BlankImageSettings
from .ImageMaskTool import ImageMaskerWindow
# from .DoubleZoomGUI import DoubleZoom
# from .DoubleZoomViewer import DoubleZoom
from .widgets.double_zoom_widget import DoubleZoomWidget
from .SetCentDialog import SetCentDialog
from .SetAngleDialog import SetAngleDialog
from .ImageBlankDialog import ImageBlankDialog
from .ImageMaskDialog import ImageMaskDialog
from ..CalibrationSettings import CalibrationSettings
from threading import Lock
from scipy.ndimage import rotate
from .widgets.navigation_controls import NavigationControls
from .tools.tool_manager import ToolManager
from .tools.chords_center_tool import ChordsCenterTool
from .tools.perpendiculars_center_tool import PerpendicularsCenterTool
from .tools.rotation_tool import RotationTool
from .tools.center_rotate_tool import CenterRotateTool
from .tools.zoom_rectangle_tool import ZoomRectangleTool
from .widgets.image_viewer_widget import ImageViewerWidget
from .widgets.collapsible_right_panel import CollapsibleRightPanel
from .widgets.collapsible_groupbox import CollapsibleGroupBox
import time
import random

class QuadFoldParams:
    def __init__(self, flags, index, file_manager, parent):
        self.flags = flags
        self.index = index
        self.file_manager = file_manager
        self.parent = parent


class ApplyCenterDialog(QDialog):
    """Dialog for choosing how to apply center to images"""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Apply Center")
        
        layout = QVBoxLayout()
        layout.addWidget(QLabel("Apply current center to:"))
        
        # Create radio buttons for exclusive selection
        self.subsequentRadio = QRadioButton("Apply to subsequent images")
        self.previousRadio = QRadioButton("Apply to previous images")
        self.allRadio = QRadioButton("Apply to all images")
        
        # Set default selection
        self.subsequentRadio.setChecked(True)
        
        layout.addWidget(self.subsequentRadio)
        layout.addWidget(self.previousRadio)
        layout.addWidget(self.allRadio)
        
        # OK and Cancel buttons
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttonBox.accepted.connect(self.accept)
        buttonBox.rejected.connect(self.reject)
        layout.addWidget(buttonBox)
        
        self.setLayout(layout)
    
    def getSelection(self):
        """Returns 'subsequent', 'previous', or 'all'"""
        if self.subsequentRadio.isChecked():
            return 'subsequent'
        elif self.previousRadio.isChecked():
            return 'previous'
        else:
            return 'all'


class RestoreAutoCenterDialog(QDialog):
    """Dialog for choosing how to restore auto center to images"""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Restore Auto Center")
        
        layout = QVBoxLayout()
        layout.addWidget(QLabel("Restore auto center to:"))
        
        # Create radio buttons for exclusive selection
        self.currentRadio = QRadioButton("Apply to current image")
        self.subsequentRadio = QRadioButton("Apply to subsequent images")
        self.previousRadio = QRadioButton("Apply to previous images")
        self.allRadio = QRadioButton("Apply to all images")
        
        # Set default selection to current image
        self.currentRadio.setChecked(True)
        
        layout.addWidget(self.currentRadio)
        layout.addWidget(self.subsequentRadio)
        layout.addWidget(self.previousRadio)
        layout.addWidget(self.allRadio)
        
        # OK and Cancel buttons
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttonBox.accepted.connect(self.accept)
        buttonBox.rejected.connect(self.reject)
        layout.addWidget(buttonBox)
        
        self.setLayout(layout)
    
    def getSelection(self):
        """Returns 'current', 'subsequent', 'previous', or 'all'"""
        if self.currentRadio.isChecked():
            return 'current'
        elif self.subsequentRadio.isChecked():
            return 'subsequent'
        elif self.previousRadio.isChecked():
            return 'previous'
        else:
            return 'all'


class ApplyRotationDialog(QDialog):
    """Dialog for choosing how to apply rotation to images"""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Apply Rotation")
        
        layout = QVBoxLayout()
        layout.addWidget(QLabel("Apply current rotation to:"))
        
        # Create radio buttons for exclusive selection
        self.subsequentRadio = QRadioButton("Apply to subsequent images")
        self.previousRadio = QRadioButton("Apply to previous images")
        self.allRadio = QRadioButton("Apply to all images")
        
        # Set default selection
        self.subsequentRadio.setChecked(True)
        
        layout.addWidget(self.subsequentRadio)
        layout.addWidget(self.previousRadio)
        layout.addWidget(self.allRadio)
        
        # OK and Cancel buttons
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttonBox.accepted.connect(self.accept)
        buttonBox.rejected.connect(self.reject)
        layout.addWidget(buttonBox)
        
        self.setLayout(layout)
    
    def getSelection(self):
        """Returns 'subsequent', 'previous', or 'all'"""
        if self.subsequentRadio.isChecked():
            return 'subsequent'
        elif self.previousRadio.isChecked():
            return 'previous'
        else:
            return 'all'


class RestoreAutoRotationDialog(QDialog):
    """Dialog for choosing how to restore auto rotation to images"""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Restore Auto Rotation")
        
        layout = QVBoxLayout()
        layout.addWidget(QLabel("Restore auto rotation to:"))
        
        # Create radio buttons for exclusive selection
        self.currentRadio = QRadioButton("Apply to current image")
        self.subsequentRadio = QRadioButton("Apply to subsequent images")
        self.previousRadio = QRadioButton("Apply to previous images")
        self.allRadio = QRadioButton("Apply to all images")
        
        # Set default selection to current image
        self.currentRadio.setChecked(True)
        
        layout.addWidget(self.currentRadio)
        layout.addWidget(self.subsequentRadio)
        layout.addWidget(self.previousRadio)
        layout.addWidget(self.allRadio)
        
        # OK and Cancel buttons
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttonBox.accepted.connect(self.accept)
        buttonBox.rejected.connect(self.reject)
        layout.addWidget(buttonBox)
        
        self.setLayout(layout)
    
    def getSelection(self):
        """Returns 'current', 'subsequent', 'previous', or 'all'"""
        if self.currentRadio.isChecked():
            return 'current'
        elif self.subsequentRadio.isChecked():
            return 'subsequent'
        elif self.previousRadio.isChecked():
            return 'previous'
        else:
            return 'all'


class AutoOrientationDialog(QDialog):
    """Dialog for configuring automatic orientation settings"""
    def __init__(self, parent=None, current_orientation_model=None, mode_orientation_enabled=False):
        super().__init__(parent)
        self.setWindowTitle("Auto Orientation Settings")
        
        layout = QVBoxLayout()
        
        # Orientation Finding
        orientationLayout = QHBoxLayout()
        orientationLayout.addWidget(QLabel("Orientation Finding:"))
        self.orientationCmbBx = QComboBox()
        self.orientationCmbBx.addItem("Max Intensity")
        self.orientationCmbBx.addItem("GMM")
        self.orientationCmbBx.addItem("Herman Factor (Half Pi)")
        self.orientationCmbBx.addItem("Herman Factor (Pi)")
        if current_orientation_model is not None:
            self.orientationCmbBx.setCurrentIndex(current_orientation_model)
        orientationLayout.addWidget(self.orientationCmbBx)
        layout.addLayout(orientationLayout)
        
        layout.addSpacing(10)
        
        # Mode Orientation
        self.modeAngleChkBx = QCheckBox("Mode Orientation")
        self.modeAngleChkBx.setChecked(mode_orientation_enabled)
        self.modeAngleChkBx.setToolTip("Use the most common orientation angle from all images in the folder")
        layout.addWidget(self.modeAngleChkBx)
        
        layout.addSpacing(20)
        
        # OK and Cancel buttons
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttonBox.accepted.connect(self.accept)
        buttonBox.rejected.connect(self.reject)
        layout.addWidget(buttonBox)
        
        self.setLayout(layout)
        self.setMinimumWidth(350)
    
    def getOrientationModel(self):
        """Returns the selected orientation model index"""
        return self.orientationCmbBx.currentIndex()
    
    def getModeOrientationEnabled(self):
        """Returns whether mode orientation is enabled"""
        return self.modeAngleChkBx.isChecked()


class WorkerSignals(QObject):

    finished = Signal()
    error = Signal(tuple)
    result = Signal(object)


class Worker(QRunnable):

    def __init__(self, params, image_center_settings, image_rotation_settings,
                 bgsub = 'Circularly-symmetric',
                 bgDict = None, bg_lock=None):

        super().__init__()
        self.flags = params.flags
        self.params = params
        self.signals = WorkerSignals()
        self.lock = Lock()

        # Store reference to imageCenterSettings and imageRotationSettings dicts
        self.imageCenterSettings = image_center_settings
        self.imageRotationSettings = image_rotation_settings

        self.bgsub = bgsub

        self.bgDict = bgDict

        self.qf_lock = Lock()

    @Slot()
    def run(self):
        try:
            img = self.params.file_manager.get_image_by_index(self.params.index)
            filename = self.params.file_manager.names[self.params.index]

            self.quadFold = QuadrantFolder(img, self.params.file_manager.dir_path, filename, self.params.parent)
            
            # Don't clear info - let cache work!
            # Only set specific fields that need to be set
            self.quadFold.info['bgsub'] = self.bgsub

            # Apply image-specific center settings if available
            # Presence in imageCenterSettings means manual mode
            if filename in self.imageCenterSettings:
                settings = self.imageCenterSettings[filename]
                center = tuple(settings['center'])
                # Restore center from settings (no need to save again)
                self.quadFold.setBaseCenter(center)

            # Apply image-specific rotation settings if available
            # Presence in imageRotationSettings means manual mode
            if filename in self.imageRotationSettings:
                settings = self.imageRotationSettings[filename]
                # Set base_rotation before processing
                self.quadFold.setBaseRotation(settings['rotation'])

            self.quadFold.process(self.flags)
            self.saveBackground()


            if self.qf_lock is not None:
                self.qf_lock.acquire()
            with open(self.quadFold.img_path + "/qf_results/tasks_done.txt", "a") as file:
                file.write(self.quadFold.img_name + " saving image"+ "\n")
            if self.qf_lock is not None:
                self.qf_lock.release()
        except:
            traceback.print_exc()
            self.signals.error.emit((traceback.format_exc()))
        else:
            self.signals.result.emit(self.quadFold)
        finally:
            self.signals.finished.emit()


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
        resultImg = self.quadFold.makeFullImage(background)

        if 'rotate' in info and info['rotate']:
            #pass
            resultImg = np.rot90(resultImg)

        method = info['bgsub']
        print(method)
        if method != 'None':

            filename = self.params.fileName
            bg_path = fullPath(self.params.filePath, os.path.join("qf_results", "bg"))
            result_path = fullPath(bg_path, filename + ".bg.tif")

            # create bg folder
            createFolder(bg_path)
            resultImg = resultImg.astype("float32")
            fabio.tifimage.tifimage(data=resultImg).write(result_path)

            #self.bgCSV(np.sum(resultImg), bg_path)
            self.bgDict[self.params.fileName] = np.sum(resultImg)


    def bgCSV(self, total_inten, bg_path):
            filename = self.params.fileName
            csv_path = join(bg_path, f'background_sum_{self.params.fileName}.csv')

                # create csv file to save total intensity for background
            if exists(csv_path):
                self.csv_bg = pd.DataFrame(columns=['Name', 'Sum'])
                self.csv_bg.loc[filename] = pd.Series({'Name': filename, 'Sum': total_inten})
                self.csv_bg.to_csv(csv_path, mode='a', header=not os.path.exists(csv_path), index=False)
            else:
                self.csv_bg = pd.DataFrame(columns=['Name', 'Sum'])
                self.csv_bg.loc[filename] = pd.Series({'Name': filename, 'Sum': total_inten})
                self.csv_bg.to_csv(csv_path, mode='a')

class EventEmitter(QObject):
    pass  # Signal definitions removed - GUI updates happen in processImage()

class QuadrantFoldingGUI(QMainWindow):

    """
    A class for window displaying all information of a selected image.
    This window contains 2 tabs : image, and result
    """
    def __init__(self):
        """
        Initial window
        """

        super().__init__()
        self.h5List = [] # if the file selected is an H5 file, regroups all the other h5 files names
        self.filePath = "" # current directory
        self.extent = None
        self.img = None
        self.quadFold = None # QuadrantFolder object
        self.img_zoom = None # zoom location of original image (x,y range)
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
        self.orientationModel = None
        self.modeOrientation = None
        self.stop_process = False
        self.chordLines = []
        self.chordpoints = []
        self.csvManager = None
        self._provisionalCount = False
        
        self.threadPool = QThreadPool()
        self.tasksQueue = Queue()
        self.currentTask = None
        self.worker = None
        self.tasksDone = 0
        self.totalFiles = 1
        self.lock = Lock()
        self.qf_lock = Lock()
        self.batchProcessing = False  # Flag to indicate batch processing mode
        self.imageMaskingTool = None

        self.setCentDialog = None

        self.setAngleDialog = None

        self.rotationAngle = None

        self.calSettingsDialog = None
        
        # Store center settings for each image
        # Presence in this dict = manual mode, absence = auto mode
        # Format: {"filename": {"center": [x, y], "source": "calibration"|"user_click"|"propagated"}}
        self.imageCenterSettings = {}
        
        # Store rotation settings for each image
        # Presence in this dict = manual mode, absence = auto mode
        # Format: {"filename": {"rotation": angle, "source": "user_click"|"propagated"}}
        self.imageRotationSettings = {}

        self.thresh_mask = None

        # Background directory scan support (must be ready before first browseFile call)
        self._scan_result = None
        self._scan_timer = QTimer(self)
        self._scan_timer.setInterval(250)
        self._scan_timer.timeout.connect(self._checkScanDone)

        self.eventEmitter = EventEmitter()

        self.initUI() # initial all GUI

        self.setConnections() # set triggered function for widgets
        # self.setMinimumHeight(900)
        self.resize(1200, 900)
        self.newImgDimension = None
        self.file_manager = None
        self.browseFile()

        self.mask_min = None
        self.mask_max = None

        self.last_executed = time.time() #Records when the handler was last executed
        self.min_interval = 0.2 #Minimum miliseconds between handler function call

        self.bgAsyncDict = {}

    def initUI(self):
        """
        Open a file finder and return the name of the file selected
        """
        self.setWindowTitle("Muscle X Quadrant Folding v." + __version__)

        self.scrollArea = QScrollArea()
        #self.scrollArea.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        #self.scrollArea.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)


        self.scrollArea.setWidgetResizable(True)
        self.centralWidget = QWidget(self)

        self.scrollArea.setWidget(self.centralWidget)
        #self.setCentralWidget(self.centralWidget)
        self.mainVLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.scrollArea)

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 200px; }")
        self.mainVLayout.addWidget(self.tabWidget)

        ##### Image Tab #####
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.tabWidget.addTab(self.imageTab, "Original Image")

        self.verImgLayout = QVBoxLayout()
        self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)

        self.leftWidget = QWidget()
        self.leftWidget.setLayout(self.verImgLayout)
        # Remove minimum width constraint to allow image viewer to use more space
        # The select buttons have fixed width (300px), so leftWidget will naturally size to fit

        self.selectImageButton = QPushButton('Click Here to Select an Image...')
        self.selectImageButton.setFixedHeight(100)
        self.selectImageButton.setFixedWidth(300)

        self.selectFolder = QPushButton('Click Here to Select a Folder...')
        self.selectFolder.setFixedHeight(100)
        self.selectFolder.setFixedWidth(300)

        self.bgWd = QWidget()
        self.verImgLayout.addWidget(self.selectImageButton)

        # Create ImageViewerWidget with built-in display panel and double zoom
        # Pass self as parent so DoubleZoom can access self.quadFold
        self.image_viewer = ImageViewerWidget(parent=self, show_display_panel=True, show_double_zoom=True)
        
        # Backward compatibility: expose axes, canvas, figure for legacy code
        self.imageAxes = self.image_viewer.axes
        self.imageCanvas = self.image_viewer.canvas
        self.imageFigure = self.image_viewer.figure
        
        self.imageCanvas.setHidden(True)  # Initially hidden
        # Set stretch factors: leftWidget(0), image_viewer(1) to make image area expand
        self.imageTabLayout.addWidget(self.leftWidget, 0)  # Don't stretch, use minimum size
        self.imageTabLayout.addWidget(self.image_viewer, 1)  # Stretch to fill available space

        # Create CollapsibleRightPanel to wrap all right-side options
        self.right_panel = CollapsibleRightPanel(
            parent=self, 
            title="Options", 
            settings_key="quadrant/right_panel",
            start_visible=True,
            show_toggle_internally=False  # Toggle button will be floating
        )

        # Quadrant-specific display options (add to display panel's top slot)
        self.showSeparator = QCheckBox()
        self.showSeparator.setText("Show Quadrant Separator")
        self.showSeparator.setChecked(True)

        self.cropFoldedImageChkBx = QCheckBox("Save Cropped Image (Original Size)")
        self.cropFoldedImageChkBx.setChecked(False)
        
        # Add quadrant-specific options to display panel's top slot (before basic controls)
        self.image_viewer.display_panel.add_to_top_slot(self.showSeparator)
        self.image_viewer.display_panel.add_to_top_slot(self.cropFoldedImageChkBx)
        
        # Backward compatibility: expose built-in display panel controls
        self.spminInt = self.image_viewer.display_panel.minIntSpnBx
        self.spmaxInt = self.image_viewer.display_panel.maxIntSpnBx
        self.logScaleIntChkBx = self.image_viewer.display_panel.logScaleChkBx
        self.persistIntensity = self.image_viewer.display_panel.persistChkBx
        self.imgZoomInB = self.image_viewer.display_panel.zoomInBtn
        self.imgZoomOutB = self.image_viewer.display_panel.zoomOutBtn
        self.minIntLabel = self.image_viewer.display_panel.minIntLabel
        self.maxIntLabel = self.image_viewer.display_panel.maxIntLabel
        self.doubleZoom = self.image_viewer.double_zoom  # Now displayed in display panel
        
        # QuadrantFolding-specific: start with 0 decimals (initialWidgets updates to 2 for float images)
        self.spminInt.setDecimals(0)
        self.spmaxInt.setDecimals(0)
        
        # Add zoom button to checkable buttons list
        self.checkableButtons.append(self.imgZoomInB)

        self.settingsGroup = CollapsibleGroupBox("Image Processing", start_expanded=True)
        self.settingsLayout = QGridLayout()
        #self.settingsLayout.setScaledContents(False)
        #self.settingsLayout.setWidgetResizable(False)
        #self.settingsGroup.setLayout(self.settingsLayout)
        #self.settingsGroup.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        #self.settingsGroup.setFixedHeight(300)
        #self.settingsGroup.setMinimumSize(400, 200)

        self.setCenterRotationButton = QPushButton("Quick Center and Rotation Angle")
        self.setCenterRotationButton.setCheckable(False)
        self.checkableButtons.append(self.setCenterRotationButton)

        self.setCenterGroup = CollapsibleGroupBox("Set Center", start_expanded=True)
        self.setCenterLayout = QGridLayout()
        self.calibrationButton = QPushButton("Set Center by Calibration")

        self.setCentByChords = QPushButton("Set Center by Chords")
        self.setCentByChords.setCheckable(False)
        self.checkableButtons.append(self.setCentByChords)
        self.setCentByPerp = QPushButton("Set Center by Perpendiculars")
        self.setCentByPerp.setCheckable(False)
        self.checkableButtons.append(self.setCentByPerp)

        self.setCentBtn = QPushButton("Set Center Manually")
        self.setCentBtn.setCheckable(False)
        self.checkableButtons.append(self.setCentBtn)

        self.rotationAngleGroup = CollapsibleGroupBox("Set Rotation Angle", start_expanded=True)
        self.rotationAngleLayout = QGridLayout()
        self.setRotationButton = QPushButton("Set Angle Interactively")
        self.setRotationButton.setCheckable(False)
        self.checkableButtons.append(self.setRotationButton)

        self.setAngleBtn = QPushButton("Set Angle Manually")
        self.setAngleBtn.setCheckable(False)
        self.checkableButtons.append(self.setAngleBtn)
        
        self.setAutoOrientationBtn = QPushButton("Set Auto Orientation")
        self.setAutoOrientationBtn.setCheckable(False)

        self.imageCenter = QLabel()
        self.imageCenter.setStyleSheet("color: green")
        self.applyCenterMode = QLabel()
        self.applyCenterMode.setStyleSheet("color: green")

        self.applyCenterBtn = QPushButton("Apply Center")
        self.restoreAutoCenterBtn = QPushButton("Restore Auto Center")

        self.rotationAngleLabel = QLabel()
        self.rotationAngleLabel.setStyleSheet("color: green")
        self.applyRotationMode = QLabel()
        self.applyRotationMode.setStyleSheet("color: green")
        
        self.applyRotationBtn = QPushButton("Apply Rotation")
        self.restoreAutoRotationBtn = QPushButton("Restore Auto Rotation")

        self.compressFoldedImageChkBx = QCheckBox("Save Compressed Image")
        self.compressFoldedImageChkBx.setChecked(True)
        self.compressFoldedImageChkBx.setToolTip("Saves the images as compressed tifs (might not be compatible with fit2d, but works with imagej)")

        self.toggleFoldImage = QCheckBox("Fold Image")
        self.toggleFoldImage.setChecked(True)

        centerLayoutRowIndex = 0
        self.setCenterLayout.addWidget(self.setCenterRotationButton, centerLayoutRowIndex, 0, 1, 4)
        centerLayoutRowIndex += 1
        self.setCenterLayout.addWidget(self.calibrationButton, centerLayoutRowIndex, 0, 1, 2)
        self.setCenterLayout.addWidget(self.setCentBtn, centerLayoutRowIndex, 2, 1, 2)
        centerLayoutRowIndex += 1
        self.setCenterLayout.addWidget(self.setCentByChords, centerLayoutRowIndex, 0, 1, 2)
        self.setCenterLayout.addWidget(self.setCentByPerp, centerLayoutRowIndex, 2, 1, 2)
        centerLayoutRowIndex += 1

        self.setCenterLayout.addWidget(self.imageCenter, centerLayoutRowIndex, 0, 1, 4)
        centerLayoutRowIndex += 1
        self.setCenterLayout.addWidget(self.applyCenterMode, centerLayoutRowIndex, 0, 1, 4)
        centerLayoutRowIndex += 1
        self.setCenterLayout.addWidget(self.applyCenterBtn, centerLayoutRowIndex, 0, 1, 2)
        self.setCenterLayout.addWidget(self.restoreAutoCenterBtn, centerLayoutRowIndex, 2, 1, 2)
        centerLayoutRowIndex += 1

        rotationAngleRowIndex = 0
        self.rotationAngleLayout.addWidget(self.setAutoOrientationBtn, rotationAngleRowIndex, 0, 1, 4)
        rotationAngleRowIndex += 1
        self.rotationAngleLayout.addWidget(self.setRotationButton, rotationAngleRowIndex, 0, 1, 2)
        self.rotationAngleLayout.addWidget(self.setAngleBtn, rotationAngleRowIndex, 2, 1, 2)
        rotationAngleRowIndex += 1
        self.rotationAngleLayout.addWidget(self.rotationAngleLabel, rotationAngleRowIndex, 0, 1, 4)
        rotationAngleRowIndex += 1
        self.rotationAngleLayout.addWidget(self.applyRotationMode, rotationAngleRowIndex, 0, 1, 4)
        rotationAngleRowIndex += 1
        self.rotationAngleLayout.addWidget(self.applyRotationBtn, rotationAngleRowIndex, 0, 1, 2)
        self.rotationAngleLayout.addWidget(self.restoreAutoRotationBtn, rotationAngleRowIndex, 2, 1, 2)
        rotationAngleRowIndex += 1

        settingsRowIndex = 0

        self.settingsLayout.addWidget(QLabel("Mask Threshold : Use Set Mask"), settingsRowIndex, 0, 1, 2)
        settingsRowIndex += 1

        self.settingsLayout.addWidget(self.toggleFoldImage, settingsRowIndex, 0, 1, 2)
        self.settingsLayout.addWidget(self.compressFoldedImageChkBx, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1

        # Blank Image Settings
        self.blankImageGrp = CollapsibleGroupBox("Apply Blank Image and Mask", start_expanded=True)

        self.blankImageLayout = QGridLayout()
        self.blankSettingButton = QPushButton("Set Empty Cell Image")
        self.blankImageLayout.addWidget(self.blankSettingButton, 0, 0, 1, 2)
        self.maskSettingButton = QPushButton("Set Mask")
        self.blankImageLayout.addWidget(self.maskSettingButton, 0, 2, 1, 2)
        
        # Checkboxes to enable/disable blank image and mask
        self.applyBlankImageChkBx = QCheckBox("Apply Empty Cell Image")
        self.applyBlankImageChkBx.setEnabled(False)  # Disabled until settings exist
        self.blankImageLayout.addWidget(self.applyBlankImageChkBx, 1, 0, 1, 2)
        
        self.applyMaskChkBx = QCheckBox("Apply Mask")
        self.applyMaskChkBx.setEnabled(False)  # Disabled until settings exist
        self.blankImageLayout.addWidget(self.applyMaskChkBx, 1, 2, 1, 2)

        # Result processing and background Subtraction
        self.resProcGrpBx = CollapsibleGroupBox("Result Processing", start_expanded=False)
        self.resProcGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)

        self.setFitRoi = QPushButton("Set Region Of Interest (ROI)")
        self.setFitRoi.setCheckable(True)
        self.unsetRoi = QPushButton("Unset ROI")
        self.checkableButtons.append(self.setFitRoi)
        self.fixedRoiChkBx = QCheckBox("Fixed ROI Radius:")
        self.fixedRoiChkBx.setChecked(False)
        self.fixedRoi = QSpinBox()
        self.fixedRoi.setObjectName('fixedRoi')
        self.fixedRoi.setKeyboardTracking(False)
        self.fixedRoi.setRange(1, 10000)
        self.fixedRoi.setEnabled(False)


        self.allBGChoices = ['None', '2D Convexhull', 'Circularly-symmetric', 'White-top-hats', 'Smoothed-Gaussian', 'Smoothed-BoxCar', 'Roving Window']
        self.bgChoiceIn = QComboBox()
        self.bgChoiceIn.setCurrentIndex(0)
        for c in self.allBGChoices:
            self.bgChoiceIn.addItem(c)

        self.bgChoiceOut = QComboBox()
        self.bgChoiceOut.setCurrentIndex(0)
        self.allBGChoicesOut = ['None', 'Circularly-symmetric', 'White-top-hats', 'Smoothed-Gaussian', 'Smoothed-BoxCar', 'Roving Window']
        for c in self.allBGChoicesOut:
            self.bgChoiceOut.addItem(c)

        self.setRminButton = QPushButton("Set Manual R-min")
        self.setRminButton.setCheckable(True)
        self.checkableButtons.append(self.setRminButton)

        self.rminSpnBx = QSpinBox()
        self.rminSpnBx.setSingleStep(2)
        self.rminSpnBx.setValue(-1)
        self.rminSpnBx.setRange(-1, 3000)
        self.rminSpnBx.setKeyboardTracking(False)
        self.rminLabel = QLabel("R-min")

        self.showRminChkBx = QCheckBox("Show R-min")
        # self.radiusLabel = QLabel("Radius Range : ")
        self.fixedRadiusRangeChkBx = QCheckBox("Persist R-min")

        self.gaussFWHMLabel = QLabel("Gaussian FWHM : ")
        self.gaussFWHM = QSpinBox()
        self.gaussFWHM.setRange(1, 3000)
        self.gaussFWHM.setValue(10)
        self.gaussFWHM.setKeyboardTracking(False)

        self.boxcarLabel = QLabel("Box car size : ")
        self.boxcarX = QSpinBox()
        self.boxcarX.setRange(1, 3000)
        self.boxcarX.setValue(10)
        self.boxcarX.setPrefix('X:')
        self.boxcarX.setKeyboardTracking(False)
        self.boxcarY = QSpinBox()
        self.boxcarY.setRange(1, 3000)
        self.boxcarY.setValue(10)
        self.boxcarY.setPrefix('Y:')
        self.boxcarY.setKeyboardTracking(False)

        self.cycleLabel = QLabel("Number of cycle : ")
        self.cycle = QSpinBox()
        self.cycle.setValue(5)
        self.cycle.setKeyboardTracking(False)
        self.cycle.setRange(1, 3000)

        self.windowSizeLabel = QLabel("Window Size : ")
        self.winSizeX = QSpinBox()
        self.winSizeX.setPrefix('X:')
        self.winSizeX.setKeyboardTracking(False)
        self.winSizeX.setRange(1, 3000)
        self.winSizeX.setValue(10)
        self.winSizeY = QSpinBox()
        self.winSizeY.setPrefix('Y:')
        self.winSizeY.setKeyboardTracking(False)
        self.winSizeY.setRange(1, 3000)
        self.winSizeY.setValue(10)

        self.windowSepLabel = QLabel("Window Separation : ")
        self.winSepX = QSpinBox()
        self.winSepX.setPrefix('X:')
        self.winSepX.setKeyboardTracking(False)
        self.winSepX.setRange(1, 3000)
        self.winSepX.setValue(10)
        self.winSepY = QSpinBox()
        self.winSepY.setPrefix('Y:')
        self.winSepY.setKeyboardTracking(False)
        self.winSepY.setRange(1, 3000)
        self.winSepY.setValue(10)

        self.minPixRange = QDoubleSpinBox()
        self.minPixRange.setSuffix("%")
        self.minPixRange.setDecimals(2)
        self.minPixRange.setSingleStep(2)
        self.minPixRange.setValue(0)
        self.minPixRange.setRange(0, 100)
        self.minPixRange.setKeyboardTracking(False)

        self.maxPixRange = QDoubleSpinBox()
        self.maxPixRange.setSuffix("%")
        self.maxPixRange.setDecimals(2)
        self.maxPixRange.setSingleStep(2)
        self.maxPixRange.setValue(25)
        self.maxPixRange.setRange(0, 100)
        self.maxPixRange.setKeyboardTracking(False)
        self.pixRangeLabel = QLabel("Pixel Range : ")

        self.thetaBinLabel = QLabel("Bin Theta (deg) : ")
        self.thetabinCB = QComboBox()
        self.thetabinCB.addItems(["3", "5", "10", "15", "30", "45", "90"])
        self.thetabinCB.setCurrentIndex(4)

        self.radialBinSpnBx = QSpinBox()
        self.radialBinSpnBx.setRange(1, 100)
        self.radialBinSpnBx.setValue(10)
        self.radialBinSpnBx.setKeyboardTracking(False)
        self.radialBinSpnBx.setSuffix(" Pixel(s)")
        self.radialBinLabel = QLabel("Radial Bin : ")

        self.smoothSpnBx = QDoubleSpinBox()
        self.smoothSpnBx.setRange(0, 10000)
        self.smoothSpnBx.setValue(0.1)
        self.smoothSpnBx.setKeyboardTracking(False)
        self.smoothLabel = QLabel("Smoothing factor : ")

        self.tensionSpnBx = QDoubleSpinBox()
        self.tensionSpnBx.setRange(0, 100)
        self.tensionSpnBx.setValue(1)
        self.tensionSpnBx.setKeyboardTracking(False)
        self.tensionLabel = QLabel("Tension factor : ")

        self.tophat1SpnBx = QSpinBox()
        self.tophat1SpnBx.setRange(1, 100)
        self.tophat1SpnBx.setValue(5)
        self.tophat1SpnBx.setKeyboardTracking(False)
        self.tophat1Label = QLabel("Top-hat disk size: ")

        self.tophat2SpnBx = QSpinBox()
        self.tophat2SpnBx.setRange(1, 100)
        self.tophat2SpnBx.setValue(20)
        self.tophat2SpnBx.setKeyboardTracking(False)
        self.tophat2Label = QLabel("Top-hat disk size : ")

        self.gaussFWHM2Label = QLabel("Gaussian FWHM : ")
        self.gaussFWHM2 = QSpinBox()
        self.gaussFWHM2.setRange(1, 3000)
        self.gaussFWHM2.setValue(20)
        self.gaussFWHM2.setKeyboardTracking(False)

        self.boxcar2Label = QLabel("Box car size : ")
        self.boxcar2X = QSpinBox()
        self.boxcar2X.setRange(1, 3000)
        self.boxcar2X.setValue(20)
        self.boxcar2X.setPrefix('X:')
        self.boxcar2X.setKeyboardTracking(False)
        self.boxcar2Y = QSpinBox()
        self.boxcar2Y.setRange(1, 3000)
        self.boxcar2Y.setValue(20)
        self.boxcar2Y.setPrefix('Y:')
        self.boxcar2Y.setKeyboardTracking(False)

        self.cycle2Label = QLabel("Number of cycle : ")
        self.cycle2 = QSpinBox()
        self.cycle2.setValue(5)
        self.cycle2.setKeyboardTracking(False)
        self.cycle2.setRange(1, 3000)

        self.windowSize2Label = QLabel("Window Size : ")
        self.winSize2X = QSpinBox()
        self.winSize2X.setPrefix('X:')
        self.winSize2X.setKeyboardTracking(False)
        self.winSize2X.setRange(1, 3000)
        self.winSize2X.setValue(20)
        self.winSize2Y = QSpinBox()
        self.winSize2Y.setPrefix('Y:')
        self.winSize2Y.setKeyboardTracking(False)
        self.winSize2Y.setRange(1, 3000)
        self.winSize2Y.setValue(20)

        self.windowSep2Label = QLabel("Window Separation : ")
        self.winSep2X = QSpinBox()
        self.winSep2X.setPrefix('X:')
        self.winSep2X.setKeyboardTracking(False)
        self.winSep2X.setRange(1, 3000)
        self.winSep2X.setValue(10)
        self.winSep2Y = QSpinBox()
        self.winSep2Y.setPrefix('Y:')
        self.winSep2Y.setKeyboardTracking(False)
        self.winSep2Y.setRange(1, 3000)
        self.winSep2Y.setValue(10)

        self.minPixRange2 = QDoubleSpinBox()
        self.minPixRange2.setSuffix("%")
        self.minPixRange2.setDecimals(2)
        self.minPixRange2.setSingleStep(2)
        self.minPixRange2.setValue(0)
        self.minPixRange2.setRange(0, 100)
        self.minPixRange2.setKeyboardTracking(False)

        self.maxPixRange2 = QDoubleSpinBox()
        self.maxPixRange2.setSuffix("%")
        self.maxPixRange2.setDecimals(2)
        self.maxPixRange2.setSingleStep(2)
        self.maxPixRange2.setValue(25)
        self.maxPixRange2.setRange(0, 100)
        self.maxPixRange2.setKeyboardTracking(False)
        self.pixRange2Label = QLabel("Pixel Range : ")

        self.thetaBin2Label = QLabel("Bin Theta (deg) : ")
        self.thetabinCB2 = QComboBox()
        self.thetabinCB2.addItems(["3", "5", "10", "15", "30", "45", "90"])
        self.thetabinCB2.setCurrentIndex(4)

        self.deg1Label = QLabel("Step (deg) : ")
        self.deg1CB = QComboBox()
        self.deg1CB.addItems(["0.5", "1", "2", "3", "5", "9", "10", "15"])
        self.deg1CB.setCurrentIndex(1)

        self.deg2Label = QLabel("Step (deg) : ")
        self.deg2CB = QComboBox()
        self.deg2CB.addItems(["0.5","1", "2", "3", "5", "9", "10", "15"])
        self.deg2CB.setCurrentIndex(2)

        self.radialBin2SpnBx = QSpinBox()
        self.radialBin2SpnBx.setRange(1, 100)
        self.radialBin2SpnBx.setValue(10)
        self.radialBin2SpnBx.setKeyboardTracking(False)
        self.radialBin2SpnBx.setSuffix(" Pixel(s)")
        self.radialBin2Label = QLabel("Radial Bin : ")

        self.smooth2SpnBx = QDoubleSpinBox()
        self.smooth2SpnBx.setRange(0, 10000)
        self.smooth2SpnBx.setValue(0.1)
        self.smooth2SpnBx.setKeyboardTracking(False)
        self.smooth2Label = QLabel("Smoothing factor : ")

        self.tension2SpnBx = QDoubleSpinBox()
        self.tension2SpnBx.setRange(0, 100)
        self.tension2SpnBx.setValue(1)
        self.tension2SpnBx.setKeyboardTracking(False)
        self.tension2Label = QLabel("Tension factor : ")

        self.tranRSpnBx = QSpinBox()
        self.tranRSpnBx.setRange(-1, 5000)
        self.tranRSpnBx.setValue(-1)
        self.tranRSpnBx.setKeyboardTracking(False)
        self.tranRLabel = QLabel("Transition Radius : ")

        self.tranDeltaSpnBx = QSpinBox()
        self.tranDeltaSpnBx.setRange(-1, 2000)
        self.tranDeltaSpnBx.setValue(-1)
        self.tranDeltaSpnBx.setKeyboardTracking(False)
        self.tranDeltaLabel = QLabel("Transition Delta : ")

        self.applyBGButton = QPushButton("Apply")
        # self.applyBG2Button = QPushButton("Apply (Out)")

        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        separator.setLineWidth(1)

        separator_2 = QFrame()
        separator_2.setFrameShape(QFrame.HLine)
        separator_2.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        separator_2.setLineWidth(1)


        self.showTranRadDeltaChkBx = QCheckBox("Show Transition Radius and Delta")

        self.outBGWidgets = [self.tranDeltaSpnBx, self.tranDeltaLabel, self.tranRSpnBx, self.tranRLabel, self.showTranRadDeltaChkBx, separator, separator_2]

        self.bgLayout = QGridLayout()
        self.bgLayout.addWidget(self.setFitRoi, 0, 0, 1, 3)
        self.bgLayout.addWidget(self.unsetRoi, 0, 3, 1, 1)
        self.bgLayout.addWidget(self.fixedRoiChkBx, 1, 0, 1, 2)
        self.bgLayout.addWidget(self.fixedRoi, 1, 2, 1, 2)
        self.bgLayout.addWidget(QLabel("Background Subtraction (In) :"), 2, 0, 1, 2)
        self.bgLayout.addWidget(self.bgChoiceIn, 2, 2, 1, 2)



        # R-min settings
        self.rrangeSettingFrame = QFrame()
        self.rrangeSettingLayout = QGridLayout(self.rrangeSettingFrame)
        self.rrangeSettingLayout.setContentsMargins(0, 0, 0, 0)

        self.rrangeSettingLayout.addWidget(self.setRminButton, 2, 2, 1, 2)
        self.rrangeSettingLayout.addWidget(self.rminLabel, 2, 0, 1, 1)

        self.rrangeSettingLayout.addWidget(self.rminSpnBx, 2, 1, 1, 1)

        self.rrangeSettingLayout.addWidget(self.fixedRadiusRangeChkBx, 3, 0, 1, 1)

        self.rrangeSettingLayout.addWidget(self.showRminChkBx, 3, 2, 1, 1)

        self.bgLayout.addWidget(self.rrangeSettingFrame, 3, 0, 3, 4)



        # Gaussian FWHM
        self.bgLayout.addWidget(self.gaussFWHMLabel, 6, 0, 1, 2)
        self.bgLayout.addWidget(self.gaussFWHM, 6, 2, 1, 2)

        # Box car size
        self.bgLayout.addWidget(self.boxcarLabel, 7, 0, 1, 2)
        self.bgLayout.addWidget(self.boxcarX, 7, 2, 1, 1)
        self.bgLayout.addWidget(self.boxcarY, 7, 3, 1, 1)

        # Number of cycles
        self.bgLayout.addWidget(self.cycleLabel, 8, 0, 1, 2)
        self.bgLayout.addWidget(self.cycle, 8, 2, 1, 2)

        # Theta bin
        self.bgLayout.addWidget(self.thetaBinLabel, 9, 0, 1, 2)
        self.bgLayout.addWidget(self.thetabinCB, 9, 2, 1, 2)

        # Radial bin
        self.bgLayout.addWidget(self.radialBinLabel, 10, 0, 1, 2)
        self.bgLayout.addWidget(self.radialBinSpnBx, 10, 2, 1, 2)

        # Window size
        self.bgLayout.addWidget(self.windowSizeLabel, 11, 0, 1, 2)
        self.bgLayout.addWidget(self.winSizeX, 11, 2, 1, 1)
        self.bgLayout.addWidget(self.winSizeY, 11, 3, 1, 1)

        # Window Seperation
        self.bgLayout.addWidget(self.windowSepLabel, 12, 0, 1, 2)
        self.bgLayout.addWidget(self.winSepX, 12, 2, 1, 1)
        self.bgLayout.addWidget(self.winSepY, 12, 3, 1, 1)

        # Pixel ranges
        self.bgLayout.addWidget(self.pixRangeLabel, 13, 0, 1, 2)
        self.bgLayout.addWidget(self.minPixRange, 13, 2, 1, 1)
        self.bgLayout.addWidget(self.maxPixRange, 13, 3, 1, 1)

        # Smooth
        self.bgLayout.addWidget(self.smoothLabel, 14, 0, 1, 1)
        self.bgLayout.addWidget(self.smoothSpnBx, 14, 1, 1, 1)

        # Tension
        self.bgLayout.addWidget(self.tensionLabel, 14, 2, 1, 1)
        self.bgLayout.addWidget(self.tensionSpnBx, 14, 3, 1, 1)

        # CH deg step
        self.bgLayout.addWidget(self.deg1Label, 15, 0, 1, 2)
        self.bgLayout.addWidget(self.deg1CB, 15, 2, 1, 2)

        # White top hat
        self.bgLayout.addWidget(self.tophat1Label, 16, 0, 1, 2)
        self.bgLayout.addWidget(self.tophat1SpnBx, 16, 2, 1, 2)

        self.bgLayout.addWidget(separator, 20, 0, 1, 4)


        self.bgLayout.addWidget(QLabel("Background Subtraction (Out) :"), 22, 0, 1, 2)
        self.bgLayout.addWidget(self.bgChoiceOut, 22, 2, 1, 2)

        # Gaussian FWHM
        self.bgLayout.addWidget(self.gaussFWHM2Label, 25, 0, 1, 2)
        self.bgLayout.addWidget(self.gaussFWHM2, 25, 2, 1, 2)

        # Box car size
        self.bgLayout.addWidget(self.boxcar2Label, 26, 0, 1, 2)
        self.bgLayout.addWidget(self.boxcar2X, 26, 2, 1, 1)
        self.bgLayout.addWidget(self.boxcar2Y, 26, 3, 1, 1)

        # Number of cycles
        self.bgLayout.addWidget(self.cycle2Label, 27, 0, 1, 2)
        self.bgLayout.addWidget(self.cycle2, 27, 2, 1, 2)

        # Theta bin
        self.bgLayout.addWidget(self.thetaBin2Label, 28, 0, 1, 2)
        self.bgLayout.addWidget(self.thetabinCB2, 28, 2, 1, 2)

        # Radial bin
        self.bgLayout.addWidget(self.radialBin2Label, 29, 0, 1, 2)
        self.bgLayout.addWidget(self.radialBin2SpnBx, 29, 2, 1, 2)

        # Window size
        self.bgLayout.addWidget(self.windowSize2Label, 30, 0, 1, 2)
        self.bgLayout.addWidget(self.winSize2X, 30, 2, 1, 1)
        self.bgLayout.addWidget(self.winSize2Y, 30, 3, 1, 1)

        # Window Seperation
        self.bgLayout.addWidget(self.windowSep2Label, 31, 0, 1, 2)
        self.bgLayout.addWidget(self.winSep2X, 31, 2, 1, 1)
        self.bgLayout.addWidget(self.winSep2Y, 31, 3, 1, 1)

        # Pixel ranges
        self.bgLayout.addWidget(self.pixRange2Label, 32, 0, 1, 2)
        self.bgLayout.addWidget(self.minPixRange2, 32, 2, 1, 1)
        self.bgLayout.addWidget(self.maxPixRange2, 32, 3, 1, 1)

        # Smooth
        self.bgLayout.addWidget(self.smooth2Label, 33, 0, 1, 2)
        self.bgLayout.addWidget(self.smooth2SpnBx, 33, 2, 1, 2)

        # Tension
        self.bgLayout.addWidget(self.tension2Label, 34, 0, 1, 2)
        self.bgLayout.addWidget(self.tension2SpnBx, 34, 2, 1, 2)

        # CH deg step
        self.bgLayout.addWidget(self.deg2Label, 35, 0, 1, 2)
        self.bgLayout.addWidget(self.deg2CB, 35, 2, 1, 2)

        # White top hat 2
        self.bgLayout.addWidget(self.tophat2Label, 36, 0, 1, 2)
        self.bgLayout.addWidget(self.tophat2SpnBx, 36, 2, 1, 2)

        self.bgLayout.addWidget(separator_2, 40, 0, 1, 4)

        # Merging params
        self.bgLayout.addWidget(self.tranRLabel, 42, 0, 1, 1)
        self.bgLayout.addWidget(self.tranRSpnBx, 42, 1, 1, 1)

        self.bgLayout.addWidget(self.tranDeltaLabel, 42, 2, 1, 1)
        self.bgLayout.addWidget(self.tranDeltaSpnBx, 42, 3, 1, 1)

        self.bgLayout.addWidget(self.showTranRadDeltaChkBx, 43, 0, 1, 4)

        # Merging params
        # self.bgLayout.addWidget(self.mergeGradientLabel, 41, 0, 1, 2)

        # Apply button
        self.bgLayout.addWidget(self.applyBGButton, 45, 0, 1, 4)

        # self.bgLayout.setColumnStretch(0, 2)

        self.resProcGrpBx.setLayout(self.bgLayout)

        # Set layouts for all CollapsibleGroupBox widgets
        self.blankImageGrp.setLayout(self.blankImageLayout)
        self.setCenterGroup.setLayout(self.setCenterLayout)
        self.rotationAngleGroup.setLayout(self.rotationAngleLayout)
        self.settingsGroup.setLayout(self.settingsLayout)

        # Single reusable navigation widget (shared between tabs)
        self.navControls = NavigationControls(process_folder_text="Process Current Folder", process_h5_text="Process Current H5 File")

        # Add options to the CollapsibleRightPanel's scrollable content area
        self.right_panel.add_widget(self.image_viewer.display_panel)
        self.right_panel.add_widget(self.blankImageGrp)
        self.right_panel.add_widget(self.setCenterGroup)
        self.right_panel.add_widget(self.rotationAngleGroup)
        self.right_panel.add_widget(self.settingsGroup)
        
        # Add navigation controls to the fixed bottom area (always visible, no scrolling)
        self.right_panel.add_bottom_widget(self.navControls)
        
        # Set fixed width for the right panel
        self.right_panel.setFixedWidth(500)
        
        # Add some top margin to the panel to avoid overlap with toggle button
        self.right_panel.setContentsMargins(0, 35, 0, 0)  # 35px top margin for toggle button

        # Add right panel directly to the main layout (stretch=0 to keep fixed width)
        self.imageTabLayout.addWidget(self.right_panel, 0)
        
        # Setup floating toggle button
        # Make toggle button a child of imageTab so it floats above content
        self.right_panel.toggle_btn.setParent(self.imageTab)
        self.right_panel.toggle_btn.raise_()  # Bring to front
        self.right_panel.toggle_btn.show()
        
        # Position toggle button in top-right corner (will be updated in resizeEvent)
        self._position_toggle_button()

        ##### Result Tab #####
        self.resultTab = QWidget()
        self.resultTab.setContentsMargins(0, 0, 0, 0)
        self.resultTabLayout = QHBoxLayout(self.resultTab)
        self.tabWidget.addTab(self.resultTab, "Results")

        # self.leftLayout = QVBoxLayout()
        # self.leftFrame = QFrame()
        # self.leftFrame.setFixedWidth(300)
        # self.leftFrame.setLayout(self.leftLayout)
        # self.resultTabLayout.addWidget(self.leftFrame)

        self.resultFigure = plt.figure()
        self.resultAxes = self.resultFigure.add_subplot(111)
        self.resultAxes.set_aspect('equal', adjustable="box")
        self.resultVLayout = QVBoxLayout()
        self.resultCanvas = FigureCanvas(self.resultFigure)
        self.resultTabLayout.addWidget(self.resultCanvas)

        self.rightLayout = QVBoxLayout()
        self.rightFrame = QFrame()
        self.rightFrame.setFixedWidth(500)
        self.rightFrame.setLayout(self.rightLayout)

        self.res_scroll_areaImg = QScrollArea()
        self.res_scroll_areaImg.setWidgetResizable(True)
        self.res_scroll_areaImg.setWidget(self.rightFrame)

        self.res_scroll_areaImg.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        self.resultTabLayout.addWidget(self.res_scroll_areaImg)

        # Display Options
        self.resultDispOptGrp = CollapsibleGroupBox("Display Options", start_expanded=True)
        self.resultDispOptLayout = QGridLayout()

        self.rotate90Chkbx = QCheckBox("Rotate 90 degree")

        self.spResultmaxInt = QDoubleSpinBox()
        self.spResultmaxInt.setRange(-1e10, 1e10)  # Allow any value
        self.spResultmaxInt.setToolTip(
            "Reduction in the maximal intensity shown to allow for more details in the image.")
        self.spResultmaxInt.setKeyboardTracking(False)
        self.spResultmaxInt.setSingleStep(5)
        self.spResultmaxInt.setDecimals(0)

        self.spResultminInt = QDoubleSpinBox()
        self.spResultminInt.setRange(-1e10, 1e10)  # Allow any value
        self.spResultminInt.setToolTip(
            "Increase in the minimal intensity shown to allow for more details in the image.")
        self.spResultminInt.setKeyboardTracking(False)
        self.spResultminInt.setSingleStep(5)
        self.spResultminInt.setDecimals(0)

        self.resultZoomInB = QPushButton("Zoom In")
        self.resultZoomInB.setCheckable(True)
        self.resultZoomOutB = QPushButton("Full")
        self.checkableButtons.append(self.resultZoomInB)

        self.resultminIntLabel = QLabel("Min intensity : ")
        self.resultmaxIntLabel = QLabel("Max intensity : ")
        self.resLogScaleIntChkBx = QCheckBox("Log scale intensity")
        self.resPersistIntensity = QCheckBox("Persist intensities")

        self.resultDispOptLayout.addWidget(self.rotate90Chkbx, 0, 0, 1, 2)
        self.resultDispOptLayout.addWidget(self.resultminIntLabel, 1, 0, 1, 2)
        self.resultDispOptLayout.addWidget(self.spResultminInt, 2, 0, 1, 2)
        self.resultDispOptLayout.addWidget(self.resultmaxIntLabel, 1, 2, 1, 2)
        self.resultDispOptLayout.addWidget(self.spResultmaxInt, 2, 2, 1, 2)
        self.resultDispOptLayout.addWidget(self.resLogScaleIntChkBx, 3, 0, 1, 2)
        self.resultDispOptLayout.addWidget(self.resPersistIntensity, 3, 2, 1, 2)
        self.resultDispOptLayout.addWidget(self.resultZoomInB, 4, 0, 1, 2)
        self.resultDispOptLayout.addWidget(self.resultZoomOutB, 4, 2, 1, 2)

        # self.leftLayout.addWidget(self.resultDispOptGrp)
        # self.leftLayout.addSpacing(10)
        # self.leftLayout.addWidget(self.blankImageGrp)
        # self.leftLayout.addStretch()

        # Set layout for resultDispOptGrp (CollapsibleGroupBox)
        self.resultDispOptGrp.setLayout(self.resultDispOptLayout)

        self.rightLayout.addWidget(self.resultDispOptGrp)
        self.rightLayout.addSpacing(10)
        self.rightLayout.addWidget(self.resProcGrpBx)
        self.rightLayout.addStretch()

        # Navigation widget container for Results tab (widget moved here on tab switch)
        self.buttonsLayout2 = QGridLayout()
        # navControls will be added here dynamically when switching to Results tab
        self.rightLayout.addLayout(self.buttonsLayout2)

        #### Status bar #####
        self.statusBar = QStatusBar()
        self.progressBar = QProgressBar()
        self.progressBar.setMaximum(100)
        self.progressBar.setMinimum(0)
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusReport = QLabel()
        self.imgDetailOnStatusBar = QLabel()
        self.imgCoordOnStatusBar = QLabel()
        self.imgPathOnStatusBar = QLabel()
        self.imgPathOnStatusBar.setText("  Please select an image or a folder to process")
        self.statusBar.addPermanentWidget(self.statusReport)
        self.statusBar.addPermanentWidget(self.imgCoordOnStatusBar)
        self.statusBar.addPermanentWidget(self.imgDetailOnStatusBar)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.statusBar.addWidget(QLabel("    "))
        self.statusBar.addWidget(self.imgPathOnStatusBar)

        self.lowerStatusBar = QStatusBar()
        self.left_status = QLabel()
        self.lowerStatusBar.addWidget(self.left_status)

        self.mainVLayout.addWidget(self.statusBar)
        self.mainVLayout.addWidget(self.lowerStatusBar)
        #self.setStatusBar(self.statusBar)

        # show transition radius and delta
        self.circle_patch = None
        self.circle_patch2 = None
        self.circle_patch3 = None

        # show rmin
        self.circle_patch_rmin = None


        #### Menu Bar #####
        selectImageAction = QAction('Select an Image...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.triggered.connect(self.browseFile)

        selectFolderAction = QAction('Select a Folder...', self)
        selectFolderAction.setShortcut('Ctrl+F')
        selectFolderAction.triggered.connect(self.browseFolder)

        saveSettingsAction = QAction('Save Current Settings', self)
        saveSettingsAction.setShortcut('Ctrl+S')
        saveSettingsAction.triggered.connect(self.saveSettings)

        menubar = self.menuBar()
        # menubar.setNativeMenuBar(False)
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addAction(selectFolderAction)
        fileMenu.addAction(saveSettingsAction)

        aboutAct = QAction('About', self)
        aboutAct.triggered.connect(self.showAbout)
        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)

        self.bgChoiceInChanged()
        self.bgChoiceOutChanged()
        

        self.image_viewer.tool_manager.register_tool('chords', ChordsCenterTool)
        self.image_viewer.tool_manager.register_tool('perpendiculars', PerpendicularsCenterTool)
        # RotationTool needs a function to get current center
        self.image_viewer.tool_manager.register_tool('rotation', lambda axes, canvas: RotationTool(axes, canvas, self._get_current_center))
        # CenterRotateTool needs a function to convert coordinates
        self.image_viewer.tool_manager.register_tool('center_rotate', lambda axes, canvas: CenterRotateTool(axes, canvas, self.getOrigCoordsCenter))
        # ZoomRectangleTool for image zoom selection (with immediate callback)
        self.image_viewer.tool_manager.register_tool('zoom_rectangle', lambda axes, canvas: ZoomRectangleTool(axes, canvas, self._apply_zoom_immediately))
        
        self.show()


    def setConnections(self):
        """
        Set all triggered functions for widgets
        """
        self.tabWidget.currentChanged.connect(self.onTabChanged)

        ##### Image Tab #####
        self.selectFolder.clicked.connect(self.browseFolder)
        # Connect built-in display panel signals
        self.image_viewer.display_panel.intensityChanged.connect(lambda vmin, vmax: self.refreshImageTab())
        self.image_viewer.display_panel.logScaleChanged.connect(lambda enabled: self.refreshImageTab())
        self.image_viewer.display_panel.colorMapChanged.connect(lambda cmap: self.refreshImageTab())
        self.showSeparator.stateChanged.connect(self.refreshAllTabs)
        
        ##### Navigation Controls (shared between tabs) #####
        self.navControls.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.navControls.processH5Button.toggled.connect(self.h5batchProcBtnToggled)
        self.navControls.nextButton.clicked.connect(self.nextClicked)
        self.navControls.prevButton.clicked.connect(self.prevClicked)
        self.navControls.nextFileButton.clicked.connect(self.nextFileClicked)
        self.navControls.prevFileButton.clicked.connect(self.prevFileClicked)
        self.navControls.filenameLineEdit.editingFinished.connect(self.fileNameChanged)
        self.spResultmaxInt.valueChanged.connect(self.refreshResultTab)
        self.spResultminInt.valueChanged.connect(self.refreshResultTab)
        self.resLogScaleIntChkBx.stateChanged.connect(self.refreshResultTab)
        self.toggleFoldImage.stateChanged.connect(self.onFoldChkBoxToggled)
        self.cropFoldedImageChkBx.stateChanged.connect(self.cropFoldedImageChanged)
        self.compressFoldedImageChkBx.stateChanged.connect(self.compressFoldedImageChanged)

        self.showRminChkBx.stateChanged.connect(self.toggleCircleRmin)
        self.rminSpnBx.valueChanged.connect(self.toggleCircleRmin)

        self.showTranRadDeltaChkBx.stateChanged.connect(self.toggleCircleTransition)
        self.tranRSpnBx.valueChanged.connect(self.toggleCircleTransition)
        self.tranDeltaSpnBx.valueChanged.connect(self.toggleCircleTransition)

        # self.expandImage.stateChanged.connect(self.expandImageChecked)

        self.selectImageButton.clicked.connect(self.browseFile)
        # Connect built-in display panel zoom buttons
        self.image_viewer.display_panel.zoomInRequested.connect(self.imageZoomIn)
        self.image_viewer.display_panel.zoomOutRequested.connect(self.imageZoomOut)
        self.calibrationButton.clicked.connect(self.calibrationClicked)
        self.setCenterRotationButton.clicked.connect(self.setCenterRotation)
        self.setRotationButton.clicked.connect(self.setRotation)
        self.setCentByChords.clicked.connect(self.setCenterByChordsClicked)
        self.setCentByPerp.clicked.connect(self.setCenterByPerpClicked)
        self.setCentBtn.clicked.connect(self.setCentBtnClicked)
        self.setAngleBtn.clicked.connect(self.setAngleBtnClicked)
        self.setAutoOrientationBtn.clicked.connect(self.setAutoOrientationClicked)
        self.applyCenterBtn.clicked.connect(self.applyCenterClicked)
        self.restoreAutoCenterBtn.clicked.connect(self.restoreAutoCenterClicked)
        self.applyRotationBtn.clicked.connect(self.applyRotationClicked)
        self.restoreAutoRotationBtn.clicked.connect(self.restoreAutoRotationClicked)
        
        ##### Image Viewer Signals #####
        # Connect ImageViewerWidget signals instead of direct matplotlib events
        self.image_viewer.mousePressed.connect(self.imageClicked)
        self.image_viewer.mouseMoved.connect(self.imageOnMotion)
        self.image_viewer.mouseReleased.connect(self.imageReleased)
        self.image_viewer.mouseScrolled.connect(self.imgScrolled)
        self.image_viewer.coordinatesChanged.connect(self._on_image_coordinates_changed)
        self.image_viewer.rightClickAt.connect(self._on_image_right_click)
        self.image_viewer.toolCompleted.connect(self._on_tool_completed)
        self.image_viewer.preciseCoordinatesSelected.connect(self._on_precise_coordinates)

        ##### Result Tab #####
        self.rotate90Chkbx.stateChanged.connect(self.processImage)
        self.resultZoomInB.clicked.connect(self.resultZoomIn)
        self.resultZoomOutB.clicked.connect(self.resultZoomOut)
        self.resultFigure.canvas.mpl_connect('button_press_event', self.resultClicked)
        self.resultFigure.canvas.mpl_connect('motion_notify_event', self.resultOnMotion)
        self.resultFigure.canvas.mpl_connect('button_release_event', self.resultReleased)
        self.resultFigure.canvas.mpl_connect('scroll_event', self.resultScrolled)

        # Blank image
        self.blankSettingButton.clicked.connect(self.blankSettingClicked)
        self.applyBlankImageChkBx.stateChanged.connect(self.applyBlankImageChanged)
        # Mask
        self.maskSettingButton.clicked.connect(self.maskSettingClicked)
        self.applyMaskChkBx.stateChanged.connect(self.applyMaskChanged)

        # Background Subtraction
        self.setFitRoi.clicked.connect(self.setFitRoiClicked)
        self.unsetRoi.clicked.connect(self.unsetRoiClicked)
        self.fixedRoiChkBx.stateChanged.connect(self.fixedRoiChecked)
        self.fixedRoi.editingFinished.connect(self.fixedRoiChanged)
        self.bgChoiceIn.currentIndexChanged.connect(self.bgChoiceInChanged)
        self.bgChoiceOut.currentIndexChanged.connect(self.bgChoiceOutChanged)
        self.minPixRange.valueChanged.connect(self.pixRangeChanged)
        self.maxPixRange.valueChanged.connect(self.pixRangeChanged)

        self.setRminButton.clicked.connect(self.setManualRmin)
        self.rminSpnBx.valueChanged.connect(self.RminChanged)

        self.tranRSpnBx.valueChanged.connect(self.TranRChanged)
        self.tranDeltaSpnBx.valueChanged.connect(self.TranDeltaChanged)

        self.applyBGButton.clicked.connect(self.applyBGSub)

        # self.blankImageGrp.clicked.connect(self.blankChecked)


        # Change Apply Button Color when BG sub arguments are changed
        self.tophat1SpnBx.valueChanged.connect(self.highlightApply)
        self.winSizeX.valueChanged.connect(self.highlightApply)
        self.winSizeY.valueChanged.connect(self.highlightApply)
        self.maxPixRange.valueChanged.connect(self.highlightApply)
        self.minPixRange.valueChanged.connect(self.highlightApply)
        self.gaussFWHM.valueChanged.connect(self.highlightApply)
        self.boxcarX.valueChanged.connect(self.highlightApply)
        self.boxcarY.valueChanged.connect(self.highlightApply)
        self.deg1CB.currentIndexChanged.connect(self.highlightApply)
        self.cycle.valueChanged.connect(self.highlightApply)
        self.radialBinSpnBx.valueChanged.connect(self.highlightApply)
        self.smoothSpnBx.valueChanged.connect(self.highlightApply)
        self.tensionSpnBx.valueChanged.connect(self.highlightApply)

        self.tophat2SpnBx.valueChanged.connect(self.highlightApply)
        self.winSize2X.valueChanged.connect(self.highlightApply)
        self.winSize2Y.valueChanged.connect(self.highlightApply)
        self.maxPixRange2.valueChanged.connect(self.highlightApply)
        self.minPixRange2.valueChanged.connect(self.highlightApply)
        self.gaussFWHM2.valueChanged.connect(self.highlightApply)
        self.boxcar2X.valueChanged.connect(self.highlightApply)
        self.boxcar2Y.valueChanged.connect(self.highlightApply)
        self.deg2CB.currentIndexChanged.connect(self.highlightApply)
        self.cycle2.valueChanged.connect(self.highlightApply)
        self.radialBin2SpnBx.valueChanged.connect(self.highlightApply)
        self.smooth2SpnBx.valueChanged.connect(self.highlightApply)
        self.tension2SpnBx.valueChanged.connect(self.highlightApply)

        # self.tranRSpnBx.valueChanged.connect(self.highlightApply)
        # self.tranDeltaSpnBx.valueChanged.connect(self.highlightApply)




    def updateCurrentCenter(self, center):
        self.imageCenter.setText(
            f"Center (Current coords): x={center[0]:.2f}, y={center[1]:.2f} px"
        )



    def updateLeftWidgetWidth(self):
        if self.imageCanvas.isVisible():
            # Remove the minimum width constraint
            self.leftWidget.setMinimumWidth(0)
        else:
            # Set the minimum width for when the canvas is hidden
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
            # if 'roi_rad' in self.quadFold.info:
            #     del self.quadFold.info['roi_rad']
            self.processImage()

    def toggleCircleRmin(self):
        if self.showRminChkBx.isChecked():
            # Remove existing circle if any
            if self.circle_patch_rmin is not None:
                try:
                    self.circle_patch_rmin.remove()
                except:
                    self.circle_patch_rmin = None

            # Create new circle (adjust x, y, radius as needed)
            radius = self.rminSpnBx.value()
            center = self.quadFold.center

            self.circle_patch_rmin = plt.Circle(center, radius,
                                        fill=False,
                                        color='green',
                                        linestyle='-',
                                        linewidth=1)


            # Add the circle to the axes
            self.resultAxes.add_patch(self.circle_patch_rmin)
        else:
            # Remove the circle if checkbox is unchecked
            if self.circle_patch_rmin is not None:
                self.circle_patch_rmin.remove()
                self.circle_patch_rmin = None

        # Redraw the canvas to show changes
        self.resultCanvas.draw()

    def toggleCircleTransition(self):
        if self.showTranRadDeltaChkBx.isChecked():
            # Remove existing circle if any
            if self.circle_patch is not None:
                try:
                    self.circle_patch.remove()
                except:
                    self.circle_patch = None
            if self.circle_patch2 is not None:
                try:
                    self.circle_patch2.remove()
                except:
                    self.circle_patch3 = None
            if self.circle_patch3 is not None:
                try:
                    self.circle_patch3.remove()
                except:
                    self.circle_patch3 = None

            # Create new circle (adjust x, y, radius as needed)
            radius = self.tranRSpnBx.value()
            delta = self.tranDeltaSpnBx.value()
            center = self.quadFold.center

            self.circle_patch = plt.Circle(center, radius,
                                        fill=False,
                                        color='red',
                                        linestyle='-',
                                        linewidth=1)
            self.circle_patch2 = plt.Circle(center, radius+delta,
                                        fill=False,
                                        color='orange',
                                        linestyle='-.',
                                        linewidth=1)
            self.circle_patch3 = plt.Circle(center, radius-delta,
                                        fill=False,
                                        color='orange',
                                        linestyle='-.',
                                        linewidth=1)

            # Add the circle to the axes
            self.resultAxes.add_patch(self.circle_patch)
            self.resultAxes.add_patch(self.circle_patch2)
            self.resultAxes.add_patch(self.circle_patch3)
        else:
            # Remove the circle if checkbox is unchecked
            if self.circle_patch is not None:
                self.circle_patch.remove()
                self.circle_patch = None
            if self.circle_patch2 is not None:
                self.circle_patch2.remove()
                self.circle_patch2 = None
            if self.circle_patch3 is not None:
                self.circle_patch3.remove()
                self.circle_patch3 = None

        # Redraw the canvas to show changes
        self.resultCanvas.draw()

    def fixedRoiChanged(self):
        """
        Triggered when fixed ROI Radius spinbox value is changed
        """
        if self.quadFold is not None and not self.uiUpdating:
            self.quadFold.info['fixed_roi_rad'] = self.fixedRoi.value()
            self.result_zoom = None
            self.processImage()

    def blankChecked(self):
        """
        Handle when the Blank image and mask is checked or unchecked
        """
        if self.quadFold is not None and not self.uiUpdating:
            self.quadFold.delCache()
            fileName = self.file_manager.current_image_name
            img = self.file_manager.current_image
            self.quadFold = QuadrantFolder(img, self.filePath, fileName, self)
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
            self.setCenterRotationButton.setChecked(False)
            self.setCentByChords.setChecked(False)
            self.setCentByPerp.setChecked(False)
            self.setRotationButton.setChecked(False)

    # def expandImageChecked(self):
    #     """
    #     Triggered when the expand image checkbox is changed
    #     """
    #     if self.ableToProcess():
    #         self.newImgDimension = None
    #         self.onImageChanged()

    def blankSettingClicked(self):
        """
        Trigger when Set Blank Image and Mask clicked
        """
        if self.quadFold is None or self.quadFold.start_img is None:
            return

        image = self.quadFold.start_img.copy()
        settings_dir_path = Path(self.filePath) / "settings"
        
        try:
            settings_dir_path.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            print("Exception occurred:", e)
            tb_str = traceback.format_exc()
            print(f"Full traceback: {tb_str}\n")
            return

        # Pass image data directly (no file I/O needed)
        imageBlankDialog = ImageBlankDialog(
            image_data=image,
            settings_dir_path=settings_dir_path,
            vmin=self.spminInt.value(),
            vmax=self.spmaxInt.value()
        )

        dialogCode = imageBlankDialog.exec()

        if dialogCode == QDialog.Accepted:
            # Update checkbox state based on settings
            self.updateBlankMaskCheckboxStates()
            # Clear cache because blank image settings changed
            # This ensures the image will be reprocessed with new blank settings
            if self.quadFold is not None:
                self.quadFold.delCache()
                print("Cleared cache due to blank image settings change")
            self.processImage()
        else:
            assert dialogCode == QDialog.Rejected, f"ImageBlankDialog closed with unexpected code:{dialogCode}"
            # Still update checkbox states in case settings were deleted
            self.updateBlankMaskCheckboxStates()

    def maskSettingClicked(self):
        if self.quadFold is None or self.quadFold.start_img is None:
            return

        image = self.quadFold.start_img.copy()
        settings_dir_path = Path(self.filePath) / "settings"
        
        try:
            settings_dir_path.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            print("Exception occurred:", e)
            tb_str = traceback.format_exc()
            print(f"Full traceback: {tb_str}\n")
            return

        # Pass image data directly (no file I/O needed)
        imageMaskDialog = ImageMaskDialog(
            image_data=image,
            settings_dir_path=settings_dir_path,
            vmin=self.spminInt.value(),
            vmax=self.spmaxInt.value()
        )

        dialogCode = imageMaskDialog.exec()

        if dialogCode == QDialog.Accepted:
            # Update checkbox state based on settings
            self.updateBlankMaskCheckboxStates()
            # Clear cache because mask settings changed
            if self.quadFold is not None:
                self.quadFold.delCache()
                print("Cleared cache due to mask settings change")
            self.processImage()
        else:
            assert dialogCode == QDialog.Rejected, f"ImageMaskDialog closed with unexpected code:{dialogCode}"
            # Still update checkbox states in case settings were deleted
            self.updateBlankMaskCheckboxStates()

    def updateBlankMaskCheckboxStates(self):
        """
        Update the state of blank image and mask checkboxes based on whether settings exist
        """
        if not self.filePath:
            return
        
        settings_dir = Path(self.filePath) / "settings"
        
        # Check if blank image settings exist
        blank_config_path = settings_dir / "blank_image_settings.json"
        blank_exists = blank_config_path.exists()
        blank_disabled_flag = settings_dir / ".blank_image_disabled"
        
        # Check if mask settings exist
        mask_file_path = settings_dir / "mask.tif"
        mask_exists = mask_file_path.exists()
        mask_disabled_flag = settings_dir / ".mask_disabled"
        
        # Update blank image checkbox
        self.uiUpdating = True  # Prevent triggering the handlers during update
        if blank_exists:
            self.applyBlankImageChkBx.setEnabled(True)
            # Check if disabled flag exists
            self.applyBlankImageChkBx.setChecked(not blank_disabled_flag.exists())
        else:
            self.applyBlankImageChkBx.setEnabled(False)
            self.applyBlankImageChkBx.setChecked(False)
        
        # Update mask checkbox
        if mask_exists:
            self.applyMaskChkBx.setEnabled(True)
            # Check if disabled flag exists
            self.applyMaskChkBx.setChecked(not mask_disabled_flag.exists())
        else:
            self.applyMaskChkBx.setEnabled(False)
            self.applyMaskChkBx.setChecked(False)
        self.uiUpdating = False

    def applyBlankImageChanged(self):
        """
        Handle when the apply blank image checkbox is toggled
        """
        if self.quadFold is None or self.uiUpdating:
            return
        
        # Create/delete a flag file to indicate whether to apply blank image
        settings_dir = Path(self.filePath) / "settings"
        blank_disabled_flag = settings_dir / ".blank_image_disabled"
        
        if self.applyBlankImageChkBx.isChecked():
            # Remove the disabled flag if it exists
            if blank_disabled_flag.exists():
                blank_disabled_flag.unlink()
        else:
            # Create the disabled flag
            blank_disabled_flag.touch()
        
        # Recreate QuadrantFolder object to trigger config fingerprint validation
        # This ensures transform matrices and all cached state are properly reset
        filename = self.file_manager.current_image_name
        img = self.file_manager.current_image
        
        # Preserve manual settings
        saved_base_center = self.quadFold.base_center
        saved_base_rotation = self.quadFold.rotation
        
        # Delete file cache
        self.quadFold.delCache()
        
        # Recreate object (will trigger config fingerprint check)
        self.quadFold = QuadrantFolder(img, self.filePath, filename, self)
        
        # Restore manual settings if they were set
        if filename in self.imageCenterSettings:
            self.quadFold.setBaseCenter(saved_base_center)
        if filename in self.imageRotationSettings:
            self.quadFold.setBaseRotation(saved_base_rotation)
        
        # Reprocess
        self.processImage()

    def applyMaskChanged(self):
        """
        Handle when the apply mask checkbox is toggled
        """
        if self.quadFold is None or self.uiUpdating:
            return
        
        # Create/delete a flag file to indicate whether to apply mask
        settings_dir = Path(self.filePath) / "settings"
        mask_disabled_flag = settings_dir / ".mask_disabled"
        
        if self.applyMaskChkBx.isChecked():
            # Remove the disabled flag if it exists
            if mask_disabled_flag.exists():
                mask_disabled_flag.unlink()
        else:
            # Create the disabled flag
            mask_disabled_flag.touch()
        
        # Recreate QuadrantFolder object to trigger config fingerprint validation
        # This ensures transform matrices and all cached state are properly reset
        filename = self.file_manager.current_image_name
        img = self.file_manager.current_image
        
        # Preserve manual settings
        saved_base_center = self.quadFold.base_center
        saved_base_rotation = self.quadFold.rotation
        
        # Delete file cache
        self.quadFold.delCache()
        
        # Recreate object (will trigger config fingerprint check)
        self.quadFold = QuadrantFolder(img, self.filePath, filename, self)
        
        # Restore manual settings if they were set
        if filename in self.imageCenterSettings:
            self.quadFold.setBaseCenter(saved_base_center)
        if filename in self.imageRotationSettings:
            self.quadFold.setBaseRotation(saved_base_rotation)
        
        # Reprocess
        self.processImage()

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


    #IS THIS USED?
    def getNewCoordsCenter(self, x, y):
        """
        Calculate the center in new image coordinates
        given original image coordinates
        """
        _, center = self.getExtentAndCenter()
        base_rotation = self.quadFold.rotation
        angle = 0 if base_rotation is None else -base_rotation * math.pi / 180
        cos_a = math.cos(angle)
        sin_a = math.sin(angle)
        #mouse pos in center-as-origin points
        dx =  x - center[0]
        dy = y - center[1]
        #apply rotation
        x1 =  dx * cos_a + dy * sin_a
        y1 = -dx * sin_a + dy * cos_a
        #Move the origin back to the bottom left
        x_ir = x1 + center[0]
        y_ir = y1 + center[1]
        #Apply inverse translation to the point
        o_x = x_ir
        o_y = y_ir
        return o_x, o_y

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

    def setCenterByPerpClicked(self):
        """
        Prepare for manual center selection using perpendicular peaks.
        Now using the new tool system.
        """
        if self.quadFold is None:
            return
        
        if self.setCentByPerp.isChecked():
            # Activate the perpendiculars center tool
            self.image_viewer.tool_manager.activate_tool('perpendiculars')
        else:
            # Deactivate the tool and get the result
            result = self.image_viewer.tool_manager.deactivate_tool('perpendiculars')
            
            if result:
                print("Perpendiculars center found:", result)
                # Convert to original coordinates
                x, y = result
                orig_x, orig_y = self.getOrigCoordsCenter(x, y)
                self.setCenter((orig_x, orig_y), "Perpendicular")
                self.deleteInfo(['avg_fold'])
                self.newImgDimension = None
                self.processImage()
            else:
                print("Perpendiculars center calculation failed (need at least 2 line pairs)")

    def setCenterByChordsClicked(self):
        """
        Prepare for manual rotation center setting by selecting chords.
        Now using the new tool system.
        """
        if self.quadFold is None:
            return

        if self.setCentByChords.isChecked():
            # Activate the chords center tool
            self.image_viewer.tool_manager.activate_tool('chords')
        else:
            # Deactivate the tool and get the result
            result = self.image_viewer.tool_manager.deactivate_tool('chords')
            
            if result:
                print("Chords center found:", result)
                # Convert to original coordinates
                x, y = result
                orig_x, orig_y = self.getOrigCoordsCenter(x, y)
                self.setCenter((orig_x, orig_y), "Chords")
                self.deleteInfo(['avg_fold'])
                self.newImgDimension = None
                self.processImage()
            else:
                print("Chords center calculation failed (need 3+ points)")

    def setCentBtnClicked(self):
        if self.quadFold:
            center = self.quadFold.base_center

            if center:
                img = self.file_manager.current_image.copy()
                self.setCentDialog = SetCentDialog(self,
                    img,
                    center,
                    isLogScale=self.logScaleIntChkBx.isChecked(),
                    vmin=self.spminInt.value(),
                    vmax=self.spmaxInt.value()
                )
                dialogCode = self.setCentDialog.exec()

                # print(f"SetCentDialog dialogCode: {dialogCode}")

                if dialogCode == QDialog.Accepted:
                    center = self.setCentDialog.center
                    self.setCenter(center, "SetCentDialog")
                    self.processImage()
                else:
                    assert dialogCode == QDialog.Rejected, f"SetCentDialog closed with unexpected code:{dialogCode}"

    def applyCenterClicked(self):
        """Handle Apply Center button click"""
        if not self.quadFold or not self.quadFold.base_center:
            QMessageBox.warning(self, "No Center", "No center available to apply.")
            return
        
        dialog = ApplyCenterDialog(self)
        if dialog.exec() == QDialog.Accepted:
            selection = dialog.getSelection()
            self._applyManualCenter(self.quadFold.base_center, selection)
            QMessageBox.information(self, "Center Applied", 
                f"Center {self.quadFold.base_center} applied to {selection} images.")
    
    def restoreAutoCenterClicked(self):
        """Handle Restore Auto Center button click"""
        dialog = RestoreAutoCenterDialog(self)
        if dialog.exec() == QDialog.Accepted:
            selection = dialog.getSelection()
            self._restoreAutoCenter(selection)
            
            # Process current image immediately for 'current' or 'all' selections
            if selection == 'current' or selection == 'all':
                self.quadFold.setBaseCenter(None)
                self.processImage()
            
            QMessageBox.information(self, "Auto Center Restored", 
                f"Auto center restored for {selection} images.")
    
    def applyRotationClicked(self):
        """Handle Apply Rotation button click"""
        if not self.quadFold or self.quadFold.rotation is None:
            QMessageBox.warning(self, "No Rotation", "No rotation available to apply.")
            return
        
        dialog = ApplyRotationDialog(self)
        if dialog.exec() == QDialog.Accepted:
            selection = dialog.getSelection()
            self._applyManualRotation(self.quadFold.rotation, selection)
            QMessageBox.information(self, "Rotation Applied", 
                f"Rotation {self.quadFold.rotation:.2f}° applied to {selection} images.")
    
    def restoreAutoRotationClicked(self):
        """Handle Restore Auto Rotation button click"""
        dialog = RestoreAutoRotationDialog(self)
        if dialog.exec() == QDialog.Accepted:
            selection = dialog.getSelection()
            self._restoreAutoRotation(selection)
            
            # Process current image immediately for 'current' or 'all' selections
            if selection == 'current' or selection == 'all':
                self.quadFold.setBaseRotation(None)
                self.processImage()
            
            QMessageBox.information(self, "Auto Rotation Restored", 
                f"Auto rotation restored for {selection} images.")
    
    def _applyManualCenter(self, center, scope):
        """Apply manual center to images based on scope"""
        if not self.file_manager:
            return
        
        current_index = self.file_manager.current
        total_images = len(self.file_manager.names)
        
        if scope == 'all':
            indices = range(total_images)
        elif scope == 'subsequent':
            indices = range(current_index, total_images)
        elif scope == 'previous':
            indices = range(0, current_index + 1)
        else:
            return
        
        # Apply manual center setting to selected images
        for idx in indices:
            filename = self.file_manager.names[idx]
            self.imageCenterSettings[filename] = {
                'center': list(center),
                'source': 'propagated'
            }
        
        # Save to file
        self.saveCenterSettings()
        
        # Update mode display
        self.updateApplyCenterMode()
    
    def _restoreAutoCenter(self, scope):
        """Restore auto center for images based on scope"""
        if not self.file_manager:
            return
        
        current_index = self.file_manager.current
        total_images = len(self.file_manager.names)
        
        if scope == 'current':
            indices = [current_index]
        elif scope == 'all':
            indices = range(total_images)
        elif scope == 'subsequent':
            indices = range(current_index, total_images)
        elif scope == 'previous':
            indices = range(0, current_index + 1)
        else:
            return
        
        # Restore auto center mode for selected images
        for idx in indices:
            filename = self.file_manager.names[idx]
            if filename in self.imageCenterSettings:
                # Remove the entry to use auto mode
                del self.imageCenterSettings[filename]
        
        # Save to file
        self.saveCenterSettings()
        
        # Update mode display
        self.updateApplyCenterMode()

    def _applyManualRotation(self, rotation, scope):
        """Apply manual rotation to images based on scope"""
        if not self.file_manager:
            return
        
        current_index = self.file_manager.current
        total_images = len(self.file_manager.names)
        
        if scope == 'all':
            indices = range(total_images)
        elif scope == 'subsequent':
            indices = range(current_index, total_images)
        elif scope == 'previous':
            indices = range(0, current_index + 1)
        else:
            return
        
        # Apply manual rotation setting to selected images
        for idx in indices:
            filename = self.file_manager.names[idx]
            self.imageRotationSettings[filename] = {
                'rotation': rotation,
                'source': 'propagated'
            }
        
        # Save to file
        self.saveRotationSettings()
        
        # Update mode display
        self.updateApplyRotationMode()
    
    def _restoreAutoRotation(self, scope):
        """Restore auto rotation for images based on scope"""
        if not self.file_manager:
            return
        
        current_index = self.file_manager.current
        total_images = len(self.file_manager.names)
        
        if scope == 'current':
            indices = [current_index]
        elif scope == 'all':
            indices = range(total_images)
        elif scope == 'subsequent':
            indices = range(current_index, total_images)
        elif scope == 'previous':
            indices = range(0, current_index + 1)
        else:
            return
        
        # Restore auto rotation mode for selected images
        for idx in indices:
            filename = self.file_manager.names[idx]
            if filename in self.imageRotationSettings:
                # Remove the entry to use auto mode
                del self.imageRotationSettings[filename]
        
        # Save to file
        self.saveRotationSettings()
        
        # Update mode display
        self.updateApplyRotationMode()

    def drawPerpendiculars(self):
        """
        Draw perpendiculars on the image
        """
        ax = self.imageAxes
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

    def setRotation(self):
        """
        Trigger when set rotation angle button is pressed.
        Now using the new tool system with auto-completion.
        """
        if self.setRotationButton.isChecked():
            # Activate the rotation tool
            self.imgPathOnStatusBar.setText(
                "Rotate the line to the pattern equator (click to set angle)")
            self.image_viewer.tool_manager.activate_tool('rotation')
        else:
            # User manually cancelled - just deactivate without applying
            self.image_viewer.tool_manager.deactivate_tool('rotation')
            self.resetStatusbar()
    
    def _get_current_center(self):
        """
        Helper method to get current center for tools that need it.
        Returns None if no center is set.
        """
        if self.quadFold is None:
            return None
        return self.quadFold.center
    
    def _apply_zoom_immediately(self, zoom_bounds):
        """
        Callback to immediately apply zoom when ZoomRectangleTool completes selection.
        This is called automatically when user releases mouse after dragging.
        
        Args:
            zoom_bounds: [(x_min, x_max), (y_min, y_max)]
        """
        print(f"Zoom applied: {zoom_bounds}")
        # First deactivate the tool to clear the selection rectangle
        self.image_viewer.tool_manager.deactivate_tool('zoom_rectangle')
        # Then apply the zoom and refresh
        self.img_zoom = zoom_bounds
        self.refreshImageTab()
        # Finally reset UI state
        self.imgZoomInB.setChecked(False)
        self.resetStatusbar()
    
    # ===== ImageViewerWidget Signal Handlers =====
    
    def _on_tool_completed(self, tool_name, result):
        """
        Unified handler for tool completion events from ImageViewerWidget.
        
        This is called automatically when a tool completes its interaction.
        The tool has already been deactivated by ImageViewerWidget.
        
        Args:
            tool_name: String identifier of the completed tool
            result: The tool's result (varies by tool type)
        """
        print(f"Tool '{tool_name}' completed with result: {result}")
        
        if tool_name == 'rotation':
            # RotationTool: result is the rotation angle
            angle = result
            self.setAngle(angle, "RotationTool")
            self.setRotationButton.setChecked(False)
            self.processImage()
            self.resetStatusbar()
        
        elif tool_name == 'center_rotate':
            # CenterRotateTool: result is {'center': (x, y), 'angle': angle}
            center = result['center']
            angle = result['angle']
            self.setCenter(center, "CenterRotate")
            self.setAngle(angle, "CenterRotate")
            self.deleteInfo(['avg_fold'])
            self.newImgDimension = None
            self.setCenterRotationButton.setChecked(False)
            self.processImage()
            self.resetStatusbar()
        
        elif tool_name == 'chords':
            # ChordsCenterTool: result is center (x, y)
            center = result
            self.setCenter(center, "ChordsCenter")
            self.setCentByChords.setChecked(False)
            self.processImage()
            self.resetStatusbar()
        
        elif tool_name == 'perpendiculars':
            # PerpendicularsCenterTool: result is center (x, y)
            center = result
            self.setCenter(center, "PerpendicularCenter")
            self.setCentByPerp.setChecked(False)
            self.processImage()
            self.resetStatusbar()
        
        # Note: zoom_rectangle is handled by immediate callback (_apply_zoom_immediately)
        # so it doesn't emit toolCompleted signal
    
    def _on_image_coordinates_changed(self, x, y, value):
        """
        Handler for coordinate changes (mouse movement over image).
        Updates the status bar with current mouse position and pixel value.
        
        Args:
            x, y: Coordinates in image space
            value: Pixel value at that position
        """
        self.imgCoordOnStatusBar.setText(f"  X: {x:.1f}, Y: {y:.1f}, Intensity: {value:.2f}")
    
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
        
        menu.popup(QCursor.pos())
    
    def _on_precise_coordinates(self, x, y):
        """
        Handler for precise coordinates from DoubleZoom.
        
        Args:
            x, y: Precise coordinates selected through DoubleZoom
        """
        print(f"Precise coordinates selected: ({x:.2f}, {y:.2f})")
        # The event has already been modified and passed to tools
        # This is just for logging or additional processing if needed

    def setAngleBtnClicked(self):
        if self.quadFold:
            start_img = self.quadFold.start_img
            curr_img = self.quadFold.orig_img
            # Get current center and transform info
            center = self.quadFold.center
            base_rotation = self.quadFold.rotation if self.quadFold.rotation is not None else 0.0
            transform = self.quadFold.info.get("transform")

            if (start_img is not None) and (curr_img is not None) and center and (transform is not None):
                self.setAngleDialog = SetAngleDialog(self,
                    start_img.copy(),
                    curr_img.copy(),
                    center,
                    base_rotation,
                    transform,
                    isLogScale=self.logScaleIntChkBx.isChecked(),
                    vmin=self.spminInt.value(),
                    vmax=self.spmaxInt.value()
                )
                dialogCode = self.setAngleDialog.exec()

                # print(f"setAngleDialog dialogCode: {dialogCode}")

                if dialogCode == QDialog.Accepted:
                    angle = self.setAngleDialog.get_angle()
                    self.setAngle(angle, "SetAngleDialog")
                    self.processImage()
                else:
                    assert dialogCode == QDialog.Rejected, f"SetAngleDialog closed with unexpected code:{dialogCode}"
    
    def setAutoOrientationClicked(self):
        """
        Handle when Set Auto Orientation button is clicked.
        Shows dialog to configure orientation finding method and mode orientation.
        """
        dialog = AutoOrientationDialog(self, 
                                       current_orientation_model=self.orientationModel,
                                       mode_orientation_enabled=self.modeOrientation is not None)
        
        if dialog.exec() == QDialog.Accepted:
            # Update orientation model
            new_orientation_model = dialog.getOrientationModel()
            if new_orientation_model != self.orientationModel:
                self.orientationModel = new_orientation_model
                if self.quadFold:
                    # Reset rotation to force recalculation with new orientation model
                    self.quadFold.setBaseRotation(None)
                    self.processImage()
            
            # Update mode orientation
            mode_enabled = dialog.getModeOrientationEnabled()
            if mode_enabled:
                # Calculate mode orientation if not already done
                if self.modeOrientation is None:
                    self.modeOrientation = self.getModeRotation()
            else:
                self.modeOrientation = None


    def calibrationClicked(self):
        """
        Handle when Calibration Settings button is clicked
        :return:
        """

        success = self.setCalibrationImage(force=True)

        if success:
            # Reset rotation to force recalculation
            self.quadFold.setBaseRotation(None)
            self.deleteImgCache(['BgSubFold'])
            self.processImage()


    def setCalibrationImage(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or force to open
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        if self.calSettingsDialog is None:
            if self.quadFold is None or self.quadFold.orig_image_center is None:
                self.calSettingsDialog = CalibrationSettings(self.filePath)
            else:
                self.calSettingsDialog =  CalibrationSettings(self.filePath, center=self.quadFold.orig_image_center)
        self.calSettings = None

        self.calSettingsDialog.recalculate = False

        cal_setting = self.calSettingsDialog.calSettings
        if cal_setting is not None or force:
            result = self.calSettingsDialog.exec_()
            if result == 1:
                self.calSettings = self.calSettingsDialog.getValues()

                if self.calSettings is not None:
                    if 'center' in self.calSettings:
                        # Use setCenter to handle everything (imageCenterSettings, quadFold.center, etc.)
                        self.setCenter(self.calSettings['center'], "calibration")
                    else:
                        # Remove calibration center if unchecked
                        if self.file_manager:
                            filename = self.file_manager.current_image_name
                            if filename in self.imageCenterSettings:
                                del self.imageCenterSettings[filename]
                                self.saveCenterSettings()
                                self.updateApplyCenterMode()
                        # Reset center to None to allow auto calculation
                        if self.quadFold is not None:
                            self.quadFold.setBaseCenter(None)

                return True
        return False

    def setCenterRotation(self):
        """
        Trigger when set center and rotation angle button is pressed.
        Now using the new tool system with auto-completion.
        """
        if self.setCenterRotationButton.isChecked():
            # Activate the center-rotate tool
            self.imgPathOnStatusBar.setText("Click on 2 corresponding reflection peaks along the equator (click to set)")
            self.image_viewer.tool_manager.activate_tool('center_rotate')
        else:
            # User manually cancelled - just deactivate without applying
            self.image_viewer.tool_manager.deactivate_tool('center_rotate')
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

    def imageZoomIn(self):
        """
        Trigger when set zoom in button is pressed (image tab).
        Now using ZoomRectangleTool managed by tool_manager.
        """
        if self.imgZoomInB.isChecked():
            # Activate the zoom rectangle tool
            self.imgPathOnStatusBar.setText(
                "Draw a rectangle on the image to zoom in (drag to select)")
            self.image_viewer.tool_manager.activate_tool('zoom_rectangle')
        else:
            # User manually cancelled - just deactivate without applying
            self.image_viewer.tool_manager.deactivate_tool('zoom_rectangle')
            self.resetStatusbar()

    def imageZoomOut(self):
        """
        Trigger when set zoom out button is pressed (image tab)
        """
        self.imgZoomInB.setChecked(False)
        self.zoomOutClicked = True
        self.default_img_zoom = None
        self.default_result_img_zoom = None
        self.img_zoom = None
        self.refreshImageTab()

    def imageClicked(self, event):
        """
        Triggered when mouse presses on image in image tab.
        
        NOTE: This receives events from ImageViewerWidget after:
        - DoubleZoom processing
        - ToolManager handling
        - Right-click handling (via _on_image_right_click signal)
        
        So this only handles legacy im_move functionality.
        """
        if not self.ableToProcess():
            return

        x = event.xdata
        y = event.ydata
        
        if x is None or y is None:
            return

        # Clear ignorefold state if set
        if self.function is not None and self.function[0] == 'ignorefold':
            self.function = None
            self.display_points = None

        # Only handle im_move for left-click when no tool is active
        if self.function is None:
            # Don't set im_move if a tool is active or DoubleZoom is enabled
            if self.image_viewer.tool_manager.has_active_tool() or self.image_viewer.is_double_zoom_enabled():
                return
            
            # Right-click is handled by _on_image_right_click signal
            # So this only handles left-click (button == 1)
            if event.button == 1:
                self.function = ["im_move", (x, y)]
                self.display_points = ["im_move", (x, y)]


    def calcMouseMovement(self):
        "Determines relatively how fast the mouse is moving around"

        mph = self.mousePosHist
        if len(mph) < 2:
            return 0

        diffs = len(self.mousePosHist) - 1
        total = 0
        for i in range(diffs):
            total += np.sqrt(((mph[i][0] - mph[i+1][0]) ** 2) + ((mph[i][1] - mph[i+1][1]) ** 2))

        return total / diffs

    def imageOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab.
        
        NOTE: This receives events from ImageViewerWidget after:
        - DoubleZoom processing (handled in ImageViewerWidget)
        - ToolManager motion handling (handled in ImageViewerWidget)
        
        This updates the status bar with detailed information about mouse position.
        """
        current_time = time.time()

        #Wrapped in try block becasue this throws an error for missing last_executed,
        #even though it's set in constructor.  Investigate this further.
        try:
            if not current_time - self.last_executed >= self.min_interval:
                return
            else:
                self.last_executed = current_time
        except:
            pass

        if not self.ableToProcess():
            return

        x = event.xdata
        y = event.ydata

        if self.quadFold is None or self.quadFold.orig_img is None:
            return

        img = self.quadFold.orig_img

        # If mouse is not moving inside main image, do nothing.
        if event.inaxes != self.imageAxes:
            return

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:

            x = int(round(x))
            y = int(round(y))
            unit = "px"

            extent, center = self.getExtentAndCenter()

            if self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
                mouse_distance = np.sqrt((center[0] - x) ** 2 + (center[1] - y) ** 2)
                scale = self.calSettings['scale']
                d = mouse_distance / scale
                if (d > 0.01):
                    q = 1.0/d
                    unit = "nm^-1"
                else:
                    q = mouse_distance
                # constant = self.calSettings["silverB"] * self.calSettings["radius"]
                # calib_distance = mouse_distance * 1.0/constant
                # calib_distance = f"{calib_distance:.4f}"
            if x < img.shape[1] and y < img.shape[0]:
                #extent = self.extent
                sx = x + extent[0]
                sy = y + extent[1]

                image_height = img.shape[0]
                image_wdith = img.shape[1]
                int_x = min(max(int(round(x)), 0), image_wdith - 1)
                int_y = min(max(int(round(y)), 0), image_height - 1)
                pixel_value = img[int_x, int_y]

                if self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
                    self.imgCoordOnStatusBar.setText("Cursor (Current coords): x={x:.2f}, y={y:.2f}, value={pixel_value:.2f}, distance={q:.2f} {unit}")
                else:
                    mouse_distance = np.sqrt((self.quadFold.center[0] - x) ** 2 + (self.quadFold.center[1] - y) ** 2)
                    self.imgCoordOnStatusBar.setText(f"Cursor (Current coords): x={x:.2f}, y={y:.2f}, value={pixel_value:.2f}, distance={mouse_distance:.2f} {unit}")

                o_x, o_y = self.getOrigCoordsCenter(x, y)
                self.left_status.setText(f"Cursor (Original coords): x={o_x:.2f}, y={o_y:.2f}")

        ax = self.imageAxes
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
        # im_zoomin now handled by matplotlib RectangleSelector
        if func[0] == "im_move":
            # Don't execute im_move if DoubleZoom is enabled (to prevent pan during DoubleZoom)
            if not self.doubleZoom.is_enabled():
                if self.img_zoom is not None:
                    move = (func[1][0] - x, func[1][1] - y)
                    self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[1], img.shape[0])
                    ax.set_xlim(self.img_zoom[0])
                    ax.set_ylim(self.img_zoom[1])
                    #ax.invert_yaxis()
                    self.imageCanvas.draw_idle()
        # Deprecated tool-specific code blocks removed - now handled by:
        # - ChordsCenterTool
        # - PerpendicularsCenterTool
        # - CenterRotateTool
        # - RotationTool

    def imageReleased(self, event):
        """
        Triggered when mouse released from image
        
        NOTE: This receives events from ImageViewerWidget after:
        - DoubleZoom coordinate precision (handled in ImageViewerWidget)
        - ToolManager release handling (handled in ImageViewerWidget)
        - Tool completion detection (handled via _on_tool_completed signal)
        
        This only handles legacy im_move cleanup.
        """
        if not self.ableToProcess():
            return
        
        # Clean up im_move state
        if self.function is not None and self.function[0] == "im_move":
            self.function = None
            self.display_points = None

    def imgScrolled(self, event):
        """
        This function is called when a mouse scrolled on the image in image tab. This will affect zoom-in and zoom-out
        """
        if self.quadFold is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img_size = self.quadFold.orig_img.shape

        if self.img_zoom is None:
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
        ax = self.imageAxes
        ax.set_xlim(self.img_zoom[0])
        ax.set_ylim(self.img_zoom[1])
        #ax.invert_yaxis()
        self.imageCanvas.draw_idle()

    def RminChanged(self):
        """
        Triggered when R-min spinboxe changes
        :return:
        """
        if  self.rminSpnBx.value() > 0 and not self.uiUpdating:
            self.setRmin(self.rminSpnBx.value())

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
        self.highlightApply()

    def TranRChanged(self):
        self.quadFold.info['transition_radius'] = self.tranRSpnBx.value()
        self.highlightApply()

    def TranDeltaChanged(self):
        self.quadFold.info['transition_delta'] = self.tranDeltaSpnBx.value()
        self.highlightApply()

    def highlightApply(self):
        self.applyBGButton.setStyleSheet("background-color: yellow; color: black;")


    def highlightApplyUndo(self):
        self.applyBGButton.setStyleSheet("background-color: white; color: black;")

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
            self.display_points = ["im_move", (x, y)]
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

                    self.setRmin(rmin)
                    self.function = None
                    self.display_points = None
                    self.setRminButton.setChecked(False)

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
        ax.invert_yaxis()
        self.resultCanvas.draw_idle()

    def setManualRmin(self):
        """
        Prepare for R-min settings after button clicked
        """
        if self.setRminButton.isChecked():
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
            self.setRminButton.setChecked(False)
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
        self.highlightApply()

    def bgChoiceOutChanged(self):
        """
        Trigger when background subtraction method 2 is changed
        Available Choices : 'None', '2D Convexhull', 'Circularly-symmetric', 'Roving Window', 'White-top-hats', 'Smoothed-Gaussian', 'Smoothed-BoxCar'
        """
        choice = self.bgChoiceOut.currentText()

        self.tophat2SpnBx.setHidden(not choice == 'White-top-hats')
        self.tophat2Label.setHidden(not choice == 'White-top-hats')
        self.windowSize2Label.setHidden(not choice == 'Roving Window')
        self.winSize2X.setHidden(not choice == 'Roving Window')
        self.winSize2Y.setHidden(not choice == 'Roving Window')
        self.windowSep2Label.setHidden(True)
        self.winSep2X.setHidden(True)
        self.winSep2Y.setHidden(True)
        self.maxPixRange2.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.minPixRange2.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.pixRange2Label.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.gaussFWHM2Label.setHidden(not choice == 'Smoothed-Gaussian')
        self.gaussFWHM2.setHidden(not choice == 'Smoothed-Gaussian')
        self.boxcar2Label.setHidden(not choice == 'Smoothed-BoxCar')
        self.boxcar2X.setHidden(not choice == 'Smoothed-BoxCar')
        self.boxcar2Y.setHidden(not choice == 'Smoothed-BoxCar')
        self.deg2Label.setHidden(not choice == '2D Convexhull')
        self.deg2CB.setHidden(not choice == '2D Convexhull')
        self.cycle2Label.setHidden(not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        self.cycle2.setHidden(not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        self.thetaBin2Label.setHidden(True)
        self.thetabinCB2.setHidden(True)
        self.radialBin2SpnBx.setHidden(not choice == 'Circularly-symmetric')
        self.radialBin2Label.setHidden(not choice == 'Circularly-symmetric')
        self.smooth2Label.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.smooth2SpnBx.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.tension2Label.setHidden(not choice in ('Roving Window'))
        self.tension2SpnBx.setHidden(not choice in ('Roving Window'))

        self.highlightApply()


    def bgChoiceInChanged(self):
        """
        Trigger when background subtraction method is changed
        Available Choices : 'None', '2D Convexhull', 'Circularly-symmetric', 'Roving Window', 'White-top-hats', 'Smoothed-Gaussian', 'Smoothed-BoxCar'
        """
        choice = self.bgChoiceIn.currentText()

        self.rrangeSettingFrame.setHidden(choice=='None')

        self.tophat1SpnBx.setHidden(not choice == 'White-top-hats')
        self.tophat1Label.setHidden(not choice == 'White-top-hats')
        self.windowSizeLabel.setHidden(not choice == 'Roving Window')
        self.winSizeX.setHidden(not choice == 'Roving Window')
        self.winSizeY.setHidden(not choice == 'Roving Window')
        self.windowSepLabel.setHidden(True)
        self.winSepX.setHidden(True)
        self.winSepY.setHidden(True)
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


        hide_outBG = (choice == 'None')
        for w in self.outBGWidgets:
            w.setHidden(hide_outBG)

        self.applyBGButton.setHidden(choice == 'None')

        self.highlightApply()


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

    def applyBGSub(self):
        """
        Reprocess about background subtraction when some parameters are changed
        """
        QApplication.processEvents()
        if self.ableToProcess():
            self.deleteInfo(['bgimg1']) # delete bgimg1 to make QuadrantFolder reproduce background subrtacted image
            self.deleteInfo(['bgimg2']) # delete bgimg2 to make QuadrantFolder reproduce background subrtacted image
            self.deleteImgCache(['BgSubFold'])
            self.processImage()

        self.highlightApplyUndo()

    def minIntChanged(self):
        """
        Trigger when min intensity is changed
        """
        QApplication.processEvents()
        if self.ableToProcess():
            if self.spmaxInt.value() <= self.spminInt.value():
                self.uiUpdating = True
                self.spmaxInt.setValue(self.spminInt.value() + 1)
                self.uiUpdating = False
            self.refreshImageTab()

    def maxIntChanged(self):
        """
        Trigger when min intensity is changed
        """
        QApplication.processEvents()
        if self.ableToProcess():
            if self.spmaxInt.value() <= self.spminInt.value():
                self.uiUpdating = True
                self.spminInt.setValue(self.spmaxInt.value() - 1)
                self.uiUpdating = False
            self.refreshImageTab()

    def updateApplyCenterMode(self):
        """
        Update the apply center mode
        """
        self.applyCenterMode.setText(f"{len(self.file_manager.names) - len(self.imageCenterSettings)}/{len(self.file_manager.names)} images have auto center settings")

    def updateApplyRotationMode(self):
        """
        Update the apply rotation mode
        """
        if hasattr(self, 'applyRotationMode') and self.file_manager:
            self.applyRotationMode.setText(f"{len(self.file_manager.names) - len(self.imageRotationSettings)}/{len(self.file_manager.names)} images have auto rotation settings")

    def updateCenterModeIndicator(self):
        """
        Update the Set Center group box title to show (Auto) or (Manual) for current image
        """
        if self.file_manager:
            filename = self.file_manager.current_image_name
            if filename in self.imageCenterSettings:
                self.setCenterGroup.setTitle("Set Center  (Current Image Mode:Manual)")
            else:
                self.setCenterGroup.setTitle("Set Center  (Current Image Mode:Auto)")

    def updateRotationModeIndicator(self):
        """
        Update the Set Rotation Angle group box title to show (Auto) or (Manual) for current image
        """
        if self.file_manager:
            filename = self.file_manager.current_image_name
            if filename in self.imageRotationSettings:
                self.rotationAngleGroup.setTitle("Set Rotation Angle  (Current Image Mode:Manual)")
            else:
                self.rotationAngleGroup.setTitle("Set Rotation Angle  (Current Image Mode:Auto)")

    def orientationModelChanged(self):
        """
        Triggered when the orientation model is changed
        NOTE: This is now handled by setAutoOrientationClicked dialog
        """
        if self.quadFold is None:
            return
        # Reset rotation to force recalculation with new orientation model
        self.quadFold.setBaseRotation(None)
        self.processImage()

    def getModeRotation(self):
        """
        open images and calculate the mode orientation
        :param file_list: list of image path (str)
        :return: mode of orientation of all images in the folder
        """
        if self.modeOrientation is not None:
            return self.modeOrientation
        print("Calculating mode of angles of images in directory")
        angles = []
        for f in self.file_manager.names:
            img = self.file_manager.current_image
            quadFold = QuadrantFolder(img, self.filePath, f, self)
            print(f'Getting angle {f}')

            if 'auto_rotation' not in quadFold.info:
                return None
            angle = quadFold.info['auto_rotation']
            angles.append(angle)
        self.modeOrientation = max(set(angles), key=angles.count)
        return self.modeOrientation

    def ableToProcess(self):
        """
        Check if image can be processed
        """
        return self.quadFold is not None and not self.uiUpdating

    def resetAllManual(self):
        """
        Remove center from QuadrantFolder to make it recalculate everything from finding center
        """
        # Reset center to force recalculation (use None to trigger auto mode)
        self.quadFold.setBaseCenter(None)
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
        Initial some widgets values which depends on current image
        :param img: selected image
        :param previnfo: info of the last image
        """
        self.uiUpdating = True
        min_val = img.min()
        max_val = img.max()
        
        if not self.persistIntensity.isChecked():
            # Only update values when NOT persisting (range is already set to allow any value)
            self.spmaxInt.setValue(max_val * .5)
            self.spminInt.setValue(min_val)
        # When persist is checked: don't touch range or values at all
        
        self.spmaxInt.setSingleStep(max_val * .05)
        self.spminInt.setSingleStep(max_val * .05)

        self.minIntLabel.setText(f"Min Intensity ({min_val:.2f})")
        self.maxIntLabel.setText(f"Max Intensity ({max_val:.2f})")

        if 'float' in str(img.dtype):
            self.spmaxInt.setDecimals(2)
            self.spminInt.setDecimals(2)
            self.spResultmaxInt.setDecimals(2)
            self.spResultminInt.setDecimals(2)
        else:
            self.spmaxInt.setDecimals(2)
            self.spminInt.setDecimals(2)
            self.spResultmaxInt.setDecimals(2)
            self.spResultminInt.setDecimals(2)

        info = self.quadFold.info
        if "bgsub" in info:
            self.bgChoiceIn.setCurrentIndex(self.allBGChoices.index(info['bgsub']))
            if info['bgsub'] != 'None':

                try:
                    self.tranRSpnBx.setValue(info['transition_radius'])
                    self.tranDeltaSpnBx.setValue(info['transition_delta'])

                    self.tophat1SpnBx.setValue(info['tophat1'])
                    self.maxPixRange.setValue(info["cirmax"])
                    self.minPixRange.setValue(info["cirmin"])

                    self.radialBinSpnBx.setValue(info['radial_bin'])
                    self.smoothSpnBx.setValue(info['smooth'])
                    self.tensionSpnBx.setValue(info['tension'])

                    if previnfo is None or not self.fixedRadiusRangeChkBx.isChecked():
                        self.rminSpnBx.setValue(info['rmin'])
                    else:
                        self.rminSpnBx.setValue(previnfo['rmin'])

                    self.winSizeX.setValue(info['win_size_x'])
                    self.winSizeY.setValue(info['win_size_y'])
                    self.winSepX.setValue(info['win_sep_x'])
                    self.winSepY.setValue(info['win_sep_y'])
                    self.gaussFWHM.setValue(info['fwhm'])
                    self.boxcarX.setValue(info['boxcar_x'])
                    self.boxcarY.setValue(info['boxcar_y'])
                    self.cycle.setValue(info['cycles'])
                    self.deg1CB.setCurrentIndex(1)

                except:

                    pass


        if "bgsub2" in info:
            self.bgChoiceOut.setCurrentIndex(self.allBGChoices.index(info['bgsub2']))
            if info['bgsub2'] != 'None':
                self.tophat2SpnBx.setValue(info['tophat2'])
                self.maxPixRange2.setValue(info["cirmax2"])
                self.minPixRange2.setValue(info["cirmin2"])

                self.radialBin2SpnBx.setValue(info['radial_bin2'])
                self.smooth2SpnBx.setValue(info['smooth2'])
                self.tension2SpnBx.setValue(info['tension2'])

                self.winSize2X.setValue(info['win_size_x2'])
                self.winSize2Y.setValue(info['win_size_y2'])
                self.winSep2X.setValue(info['win_sep_x2'])
                self.winSep2Y.setValue(info['win_sep_y2'])

                self.gaussFWHM2.setValue(info['fwhm2'])
                self.boxcar2X.setValue(info['boxcar_x2'])
                self.boxcar2Y.setValue(info['boxcar_y2'])

                self.cycle2.setValue(info['cycles2'])
                self.deg2CB.setCurrentIndex(2)



        # if 'blank_mask' in info:
        #     self.blankImageGrp.setChecked(info['blank_mask'])

        # Range is already set to allow any value at spinbox creation
        if not self.resPersistIntensity.isChecked():
            self.spResultmaxInt.setValue(max_val * .1)
            self.spResultminInt.setValue(min_val)
        self.spResultmaxInt.setSingleStep(max_val * .05)
        self.spResultminInt.setSingleStep(max_val * .05)

        if 'rotate' in info:
            self.rotate90Chkbx.setChecked(info['rotate'])

        self.uiUpdating = False

    def onImageChanged(self, reprocess=False):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new QuadrantFolder object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        previnfo = None if self.quadFold is None else self.quadFold.info
        fileName = self.file_manager.current_image_name
        self.navControls.filenameLineEdit.setText(fileName)
        self.navControls.setNavMode(self.file_manager.current_file_type)
        if reprocess:
            # Don't clear info - instead mark for reprocess
            # This allows cache to work while forcing recalculation
            self.quadFold.info['reprocess'] = True
            # Remove cached center to force recalculation
            if 'auto_center' in self.quadFold.info:
                del self.quadFold.info['auto_center']
        if 'saveCroppedImage' not in self.quadFold.info:
            self.quadFold.info['saveCroppedImage'] = self.cropFoldedImageChkBx.isChecked()
        self.markFixedInfo(self.quadFold.info, previnfo)
        original_image = self.quadFold.orig_img
        # if self.calSettings is not None and not self.calSettings:
        #     self.imgDetailOnStatusBar.setText(str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype))
        # elif self.calSettings is not None and self.calSettings:
        #     self.imgDetailOnStatusBar.setText(str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype) + " (Image Calibrated)")
        self.imgDetailOnStatusBar.setText(str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype))
        self.initialWidgets(original_image, previnfo)
        if 'ignore_folds' in self.quadFold.info:
            self.ignoreFolds = self.quadFold.info['ignore_folds']
        if 'folded' in self.quadFold.info:
            # print(self.quadFold.info['folded'])
            if self.quadFold.info['folded'] != self.toggleFoldImage.isChecked():
                self.quadFold.deleteFromDict(self.quadFold.info, 'avg_fold')
                self.quadFold.deleteFromDict(self.quadFold.imgCache, 'BgSubFold')

        # Update blank image and mask checkbox states
        self.updateBlankMaskCheckboxStates()
        
        # Update center and rotation mode indicators
        self.updateCenterModeIndicator()
        self.updateRotationModeIndicator()

        self.processImage()


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
            # Show floating toggle button in Image tab
            self.right_panel.toggle_btn.show()
            self._position_toggle_button()
        elif index == 1:  # Results tab
            self.buttonsLayout2.addWidget(self.navControls, 0, 0, 1, 1)
            # Hide floating toggle button in Results tab
            self.right_panel.toggle_btn.hide()
        
        # Trigger UI update
        self.updateUI()
    
    def _position_toggle_button(self):
        """Position the floating toggle button in the top-right corner of the image tab."""
        if hasattr(self, 'right_panel') and hasattr(self, 'imageTab'):
            # Get imageTab dimensions
            tab_width = self.imageTab.width()
            # Position button in top-right corner
            # Place it above the panel, with proper spacing
            button_x = tab_width - self.right_panel.toggle_btn.width() - 10
            button_y = 5  # Small top margin
            self.right_panel.toggle_btn.move(button_x, button_y)
    
    def resizeEvent(self, event):
        """Handle window resize to reposition floating toggle button."""
        super().resizeEvent(event)
        # Reposition toggle button when window is resized
        self._position_toggle_button()

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
        if not self.updated['img']:
            self.uiUpdating = True

            ax = self.imageAxes
            ax.cla()

            if self.quadFold is None or self.quadFold.orig_img is None:
                return

            img = self.quadFold.orig_img

            extent = [0,0]
            center = self.quadFold.center

            self.extent = extent

            # Get current colormap from display panel
            current_cmap = self.image_viewer.display_panel.get_color_map()
            
            if self.logScaleIntChkBx.isChecked():
                #ax.imshow(img, cmap=current_cmap, norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0]-extent[1], 0 - extent[1]])
                ax.imshow(img, cmap=current_cmap, norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()))
            else:
                #ax.imshow(img, cmap=current_cmap, norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0]-extent[1], 0 - extent[1]])
                ax.imshow(img, cmap=current_cmap, norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()))
            ax.set_facecolor('black')

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

            #Show the masked image in the image tab
            #NICKAA


            # Set Zoom in location
            if self.img_zoom is not None and len(self.img_zoom) == 2:
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
            elif self.default_img_zoom is not None and len(self.default_img_zoom) == 2:
                ax.set_xlim(self.default_img_zoom[0])
                ax.set_ylim(self.default_img_zoom[1])
            else:
                ax.set_xlim((0-extent[0], img.shape[1] - extent[0]))
                ax.set_ylim((0-extent[1], img.shape[0] - extent[1]))

            self.img_zoom = [ax.get_xlim(), ax.get_ylim()]
            #ax.invert_yaxis()
            self.imageFigure.tight_layout()
            self.imageCanvas.draw()

            self.updated['img'] = True
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

    def setCenter(self, center, source):
        """Set center for current image and enable Apply Center button (user action - saves to settings)"""
        if self.quadFold:
            # Use QuadrantFolder's method to set base_center and center
            self.quadFold.setBaseCenter(center)
            
            # GUI responsibility: Save to settings
            if self.file_manager:
                filename = self.file_manager.current_image_name
                self.imageCenterSettings[filename] = {
                    'center': list(center),
                    'source': source  # Track how this center was set
                }
                # Save to file immediately
                self.saveCenterSettings()
            
            # GUI responsibility: Update UI
            self.updateCurrentCenter(center)
            self.updateApplyCenterMode()
            self.updateCenterModeIndicator()
            
            print(f"Center set to {center} from source: {source}")

    def setAngle(self, angle, source):
        """
        Set rotation angle for current image.
        
        The angle parameter is treated as an increment (delta) to be added to the current base_rotation.
        This is because user rotations are performed on the already-transformed (displayed) image.
        
        Args:
            angle: Rotation angle increment in degrees (relative to current displayed image)
            source: String describing the source of the angle setting
        """
        if self.quadFold:
            # Get current rotation (may be None for first time)
            current_base_rotation = self.quadFold.rotation
            if current_base_rotation is None:
                current_base_rotation = 0.0
            
            # Calculate new absolute rotation relative to original image
            # User's angle is relative to the currently displayed (transformed) image,
            # so we accumulate it
            new_base_rotation = current_base_rotation + angle
            
            # Set the accumulated rotation
            self.quadFold.setBaseRotation(new_base_rotation)
            
            # Store in imageRotationSettings for current image
            # Presence in this dict means manual mode, absence means auto mode
            if self.file_manager:
                filename = self.file_manager.current_image_name
                self.imageRotationSettings[filename] = {
                    'rotation': new_base_rotation,
                    'source': source  # Track how this rotation was set
                }
                # Save to file immediately
                self.saveRotationSettings()
            
            # Update mode display
            self.updateApplyRotationMode()
            self.updateRotationModeIndicator()
            
            # Update rotation angle display immediately (preview)
            self.rotationAngleLabel.setText(
                f"Rotation Angle (Original Coords): {new_base_rotation % 360:.2f} °"
            )
            
            print(f"Rotation increment: {angle:.2f}° from {source}, new base_rotation: {new_base_rotation:.2f}°")

    def updateResultTab(self):
        """
        Display result image in result tab
        """
        if not self.updated['result']:
            self.uiUpdating = True
            img = self.quadFold.imgCache['resultImg']

            ## Update Widgets
            self.resultminIntLabel.setText("Min intensity (" + str(round(img.min(), 2)) + ") : ")
            self.resultmaxIntLabel.setText("Max intensity (" + str(round(img.max(), 2)) + ") : ")
            # Range is already set to allow any value at spinbox creation
            self.rminSpnBx.setValue(self.quadFold.info['rmin'])

            self.tranRSpnBx.setValue(self.quadFold.info['transition_radius'])
            self.tranDeltaSpnBx.setValue(self.quadFold.info['transition_delta'])

            self.fixedRoiChkBx.setChecked('fixed_roi_rad' in self.quadFold.info)
            self.fixedRoi.setEnabled('fixed_roi_rad' in self.quadFold.info)
            if 'fixed_roi_rad' in self.quadFold.info:
                self.fixedRoi.setValue(int(self.quadFold.info['fixed_roi_rad']))

            # convert image for displaying
            # img = getBGR(get8bitImage(img, max=self.spResultmaxInt.value(), min=self.spResultminInt.value()))
            ax = self.resultAxes
            ax.cla()
            
            # Get current colormap from display panel
            current_cmap = self.image_viewer.display_panel.get_color_map()
            
            if self.resLogScaleIntChkBx.isChecked():
                ax.imshow(img, cmap=current_cmap, norm=LogNorm(vmin=max(1, self.spResultminInt.value()), vmax=self.spResultmaxInt.value()))
            else:
                ax.imshow(img, cmap=current_cmap, norm=Normalize(vmin=self.spResultminInt.value(), vmax=self.spResultmaxInt.value()))
            ax.set_facecolor('black')

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
            #ax.invert_yaxis()
            self.resultFigure.tight_layout()
            self.resultCanvas.draw()

            self.toggleCircleTransition()
            self.toggleCircleRmin()

            self.updated['result'] = True
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
        if self.ableToProcess():
            QApplication.setOverrideCursor(Qt.WaitCursor)
            flags = self.getFlags()
            # self.quadFold.expandImg = 2.8 if self.expandImage.isChecked() else 1
            # quadFold_copy = copy.copy(self.quadFold)
            try:
                # Center is already set in quadFold.center (if manual mode)
                # by setCenter() or _navigate_and_update()
                self.quadFold.process(flags)
            except Exception:
                QApplication.restoreOverrideCursor()
                errMsg = QMessageBox()
                errMsg.setText('Unexpected error')
                msg = 'Please report the problem with error message below and the input image\n\n'
                msg += "Image : "+str(self.quadFold.img_name)
                msg += "\n\nError : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
                errMsg.setInformativeText(msg)
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.setFixedWidth(300)
                errMsg.exec_()
                raise

            self.updateParams()
            self.refreshAllTabs()
            self.csvManager.writeNewData(self.quadFold)

            self.toggleCircleTransition()
            self.toggleCircleRmin()
            
            # Update center display with transformed coordinates
            self.updateCurrentCenter(self.quadFold.center)
            
            # Update rotation angle display (rotation is the angle relative to original image)
            base_rotation = self.quadFold.rotation
            if base_rotation is not None:
                self.rotationAngleLabel.setText(
                    f"Rotation Angle (Original Coords): {base_rotation % 360:.2f} °"
                )
            else:
                self.rotationAngleLabel.setText(
                    f"Rotation Angle (Original Coords): 0.00 °"
                )

            self.saveResults()
            QApplication.restoreOverrideCursor()


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

    # placeholder method
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
        while not self.tasksQueue.empty() and self.threadPool.activeThreadCount() < self.threadPool.maxThreadCount() / 2:
            params = self.tasksQueue.get()
            self.currentTask = Worker(params, self.imageCenterSettings, self.imageRotationSettings,
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

        QApplication.restoreOverrideCursor()
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
        resultImg = self.quadFold.makeFullImage(background)

        if 'rotate' in info and info['rotate']:
            #pass
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
            #self.csv_bg.loc[filename] = pd.Series({'Sum':total_inten})
            self.csv_bg.to_csv(csv_path)

    def updateParams(self):
        """
        Update the parameters
        """
        try:
            info = self.quadFold.info
            if 'orientation_model' in info:
                self.orientationModel = info['orientation_model']
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

    def resetStatusbar(self):
        """
        Reset the status bar
        """
        fileFullPath = fullPath(self.filePath, self.file_manager.current_image_name)
        total = str(len(self.file_manager.names)) + ('*' if self._provisionalCount else '')
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(self.file_manager.current + 1) + '/' + total + ') : ' + fileFullPath)
        
    def resetStatusbar2(self):
        """
        Reset the status bar, but search using self.quadFold.info
        """
        
        index = self.file_manager.names.index(self.quadFold.img_name)
        #DOES NOT GET HERE
        fileFullPath = fullPath(self.filePath, self.file_manager.names[index])
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(index + 1) + '/' + str(len(self.file_manager.names)) + ') : ' + fileFullPath)
        self.navControls.filenameLineEdit.setText(self.quadFold.img_name)

    def _checkScanDone(self):
        """
        Check if background directory scan is complete.
        Updates the image layer with full HDF5 frame expansion for accurate count.
        """
        if not self.file_manager:
            return
        
        # Show HDF5 processing progress
        h5_done, h5_total = self.file_manager.get_h5_progress()
        if h5_total > 0:
            if not self.progressBar.isVisible():
                self.progressBar.setVisible(True)
                self.progressBar.setRange(0, h5_total)
            self.progressBar.setValue(h5_done)
            self.progressBar.setFormat(f"Processing HDF5 files: {h5_done}/{h5_total}")
        
        # When FileManager finishes, it has already updated names/specs
        if not self.file_manager.is_scan_done():
            return
        
        # Hide progress bar when done
        self.progressBar.setVisible(False)
        self.progressBar.setFormat("%p%")  # Reset format to default
        
        self._provisionalCount = False
        self._scan_timer.stop()
        self.resetStatusbar()
        self.updateApplyCenterMode()
        self.updateApplyRotationMode()

    def getFlags(self):
        """
        Get all flags for QuadrantFolder process() from widgets
        :return: flags (dict)
        """
        flags = {}

        # image
        flags['orientation_model'] = self.orientationModel
        flags["ignore_folds"] = self.ignoreFolds

        # Check if blank image settings exist and is enabled
        settings_dir = Path(self.filePath) / "settings"
        blank_config_path = settings_dir / "blank_image_settings.json"
        blank_disabled_flag = settings_dir / ".blank_image_disabled"
        flags['blank_mask'] = blank_config_path.exists() and not blank_disabled_flag.exists()
        
        # Check if mask settings exist and is enabled
        mask_file_path = settings_dir / "mask.tif"
        mask_disabled_flag = settings_dir / ".mask_disabled"
        flags['apply_mask'] = mask_file_path.exists() and not mask_disabled_flag.exists()
        
        flags['fold_image'] = self.toggleFoldImage.isChecked()

        flags["transition_radius"] = self.tranRSpnBx.value()
        flags["transition_delta"] = self.tranDeltaSpnBx.value()

        # bg rm (in)
        flags['bgsub'] = self.bgChoiceIn.currentText()
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
        flags["tophat1"] = self.tophat1SpnBx.value()
        flags['fwhm'] = self.gaussFWHM.value()
        flags['boxcar_x'] = self.boxcarX.value()
        flags['boxcar_y'] = self.boxcarY.value()
        flags['cycles'] = self.cycle.value()
        flags['deg1'] = float(self.deg1CB.currentText())

        # bg rm (out)
        flags['bgsub2'] = self.bgChoiceOut.currentText()
        flags["cirmin2"] = self.minPixRange2.value()
        flags["cirmax2"] = self.maxPixRange2.value()
        flags['win_size_x2'] = self.winSize2X.value()
        flags['win_size_y2'] = self.winSize2Y.value()
        flags['win_sep_x2'] = self.winSep2X.value()
        flags['win_sep_y2'] = self.winSep2Y.value()
        flags['radial_bin2'] = self.radialBin2SpnBx.value()
        flags['smooth2'] = self.smooth2SpnBx.value()
        flags['tension2'] = self.tension2SpnBx.value()
        flags["tophat2"] = self.tophat2SpnBx.value()
        flags['fwhm2'] = self.gaussFWHM2.value()
        flags['boxcar_x2'] = self.boxcar2X.value()
        flags['boxcar_y2'] = self.boxcar2Y.value()
        flags['cycles2'] = self.cycle2.value()
        flags['deg2'] = float(self.deg2CB.currentText())


        # Apply mode orientation if enabled
        if self.modeOrientation is not None:
            self.setAngle(self.modeOrientation, "ModeAngle")
            flags["mode_angle"] = self.modeOrientation

        if self.rminSpnBx.value() > 0:
            flags['fixed_rmin'] = self.rminSpnBx.value()

        if self.tranRSpnBx.value() > 0:
            flags['transition_radius'] = self.tranRSpnBx.value()

        if self.tranDeltaSpnBx.value() > 0:
            flags['transition_delta'] = self.tranDeltaSpnBx.value()

        if self.fixedRoiChkBx.isChecked():
            flags['fixed_roi_rad'] = self.fixedRoi.value()

        flags['rotate'] = self.rotate90Chkbx.isChecked()

        if self.calSettings is not None and 'detector' in self.calSettings:
            flags['detector'] = self.calSettings['detector']

        return flags

    def onNewFileSelected(self, newFile):
        """
        Preprocess folder of the file and process current image
        :param newFile: full name of selected file
        """
        QApplication.setOverrideCursor(Qt.WaitCursor)
        if not self.file_manager:
            self.file_manager = FileManager()
        
        self.file_manager.set_from_file(str(newFile))
        self.filePath = self.file_manager.dir_path
        
        # Load center and rotation settings for this folder
        self.loadCenterSettings()
        self.loadRotationSettings()
        
        if self.file_manager.dir_path and self.file_manager.names:
            try:
                self.csvManager = QF_CSVManager(self.filePath)
            except Exception as e:
                print("Exception occurred:", e)
                tb_str = traceback.format_exc()
                print(f"Full traceback: {tb_str}\n")

                msg = QMessageBox()
                msg.setInformativeText(
                    "Permission denied when creating a folder at " + self.filePath + ". Please check the folder permissions.")
                msg.setStandardButtons(QMessageBox.Ok)
                msg.setWindowTitle("Error Creating CSVManager")
                msg.setStyleSheet("QLabel{min-width: 500px;}")
                msg.exec_()
                return "Retry"
            if self.csvManager is not None:
                self.ignoreFolds = set()
                self.selectImageButton.setHidden(True)
                self.selectFolder.setHidden(True)
                self.imageCanvas.setHidden(False)
                self.updateLeftWidgetWidth()

                self.setCentByChords.setCheckable(True)
                self.setCentByPerp.setCheckable(True)

                self.setCenterRotationButton.setCheckable(True)

                self.setRotationButton.setCheckable(True)

                self.resetWidgets()
                QApplication.restoreOverrideCursor()

                imageProcessed = False

                if self.h5List == []:
                    fileName = self.file_manager.current_image_name
                    try:
                        # Load ndarray via spec and construct QuadrantFolder
                        img = self.file_manager.current_image
                        self.quadFold = QuadrantFolder(img, self.filePath, fileName, self)

                        success = self.setCalibrationImage(force=True)

                        if success:
                            # Reset rotation to force recalculation
                            self.quadFold.setBaseRotation(None)
                            self.deleteImgCache(['BgSubFold'])

                    except Exception as e:
                        print("Exception occurred:", e)
                        tb_str = traceback.format_exc()
                        print(f"Full traceback: {tb_str}\n")

                        infMsg = QMessageBox()
                        infMsg.setText("Error trying to open " + str(fileName))
                        infMsg.setInformativeText("This usually means that the image is corrupted or missing.")
                        infMsg.setStandardButtons(QMessageBox.Ok)
                        infMsg.setIcon(QMessageBox.Information)
                        infMsg.exec_()
                        return "Retry"
                self.h5List = []
                self.onImageChanged()

                # Start background scan to populate full directory list using FileManager
                self._scan_result = None
                self._scan_timer.start()
                self.file_manager.start_async_scan(self.filePath)
            else:
                QApplication.restoreOverrideCursor()
                return "Retry"
        else:
            QApplication.restoreOverrideCursor()
            return "Retry"

        return "Success"


    def resetWidgets(self):
        """
        Reset the widgets
        """
        self.uiUpdating = True
        self.rminSpnBx.setValue(-1)
        self.tranRSpnBx.setValue(-1)
        self.tranDeltaSpnBx.setValue(-1)
        self.uiUpdating = False

    def loadCenterSettings(self):
        """Load image center settings from settings/center_settings.json"""
        if not self.filePath:
            return
        
        settings_path = Path(self.filePath) / "settings" / "center_settings.json"
        if settings_path.exists():
            try:
                with open(settings_path, 'r') as f:
                    self.imageCenterSettings = json.load(f)
                print(f"Loaded center settings for {len(self.imageCenterSettings)} images")
            except Exception as e:
                print(f"Error loading center settings: {e}")
                self.imageCenterSettings = {}
        else:
            print("No center settings file found, starting fresh")
        
        # Update mode display
        if self.file_manager and self.file_manager.names:
            self.updateApplyCenterMode()

    def saveCenterSettings(self):
        """Save image center settings to settings/center_settings.json"""
        if not self.filePath:
            return
        
        settings_dir = Path(self.filePath) / "settings"
        settings_dir.mkdir(exist_ok=True)
        
        settings_path = settings_dir / "center_settings.json"
        try:
            with open(settings_path, 'w') as f:
                json.dump(self.imageCenterSettings, f, indent=2)
            print(f"Saved center settings for {len(self.imageCenterSettings)} images")
        except Exception as e:
            print(f"Error saving center settings: {e}")

    def loadRotationSettings(self):
        """Load image rotation settings from settings/rotation_settings.json"""
        if not self.filePath:
            return
        
        settings_path = Path(self.filePath) / "settings" / "rotation_settings.json"
        if settings_path.exists():
            try:
                with open(settings_path, 'r') as f:
                    self.imageRotationSettings = json.load(f)
                print(f"Loaded rotation settings for {len(self.imageRotationSettings)} images")
            except Exception as e:
                print(f"Error loading rotation settings: {e}")
                self.imageRotationSettings = {}
        else:
            print("No rotation settings file found, starting fresh")
        
        # Update mode display
        if self.file_manager and self.file_manager.names:
            self.updateApplyRotationMode()

    def saveRotationSettings(self):
        """Save image rotation settings to settings/rotation_settings.json"""
        if not self.filePath:
            return
        
        settings_dir = Path(self.filePath) / "settings"
        settings_dir.mkdir(exist_ok=True)
        
        settings_path = settings_dir / "rotation_settings.json"
        try:
            with open(settings_path, 'w') as f:
                json.dump(self.imageRotationSettings, f, indent=2)
            print(f"Saved rotation settings for {len(self.imageRotationSettings)} images")
        except Exception as e:
            print(f"Error saving rotation settings: {e}")

    def browseFolder(self):
        """
        Process all images in current folder
        Basically, it just process the first image, and push next button automatically until it comes back to the first image
        """
        # popup folder selection dialog
        dir_path = getAFolder()
        if dir_path != "":
            self.filePath = str(dir_path)
            self.selectImageButton.setHidden(True)
            self.selectFolder.setHidden(True)
            self.imageCanvas.setHidden(False)
            self.updateLeftWidgetWidth()
            self.ignoreFolds = set()
            # Load center and rotation settings for this folder
            self.loadCenterSettings()
            self.loadRotationSettings()
            self.onImageChanged()
            self.processFolder()

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
        text += "\nCurrent Settings"

        if len(self.ignoreFolds) > 0:
            text += "\n  - Ignore Folds : " + str(list(self.ignoreFolds))
        
        # Show orientation finding method
        orientation_methods = ["Max Intensity", "GMM", "Herman Factor (Half Pi)", "Herman Factor (Pi)"]
        orientation_text = orientation_methods[self.orientationModel] if self.orientationModel is not None else "Max Intensity"
        text += "\n  - Orientation Finding : " + orientation_text
        
        if self.modeOrientation is not None:
            text += "\n  - Mode Orientation : Enabled"
        
        # Show blank image configuration if exists
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
        
        text += "\n  - Background Subtraction Method (In): "+ str(self.bgChoiceIn.currentText())

        if flags['bgsub'] != 'None':
            if 'fixed_rmin' in flags:
                text += "\n  - R-min : " + str(flags["fixed_rmin"])

            if flags['bgsub'] in ['Circularly-symmetric', 'Roving Window']:
                text += "\n  - Pixel Range (Percentage) : " + str(flags["cirmin"]) + "% - "+str(flags["cirmax"])+"%"

            if flags['bgsub'] == 'Circularly-symmetric':
                text += "\n  - Radial Bin : " + str(flags["radial_bin"])
                text += "\n  - Smooth : " + str(flags["smooth"])
            elif flags['bgsub'] == '2D Convexhull':
                text += "\n  - Step (deg) : " + str(flags["deg1"])
            elif flags['bgsub'] == 'White-top-hats':
                text += "\n  - Tophat (inside R-max) : " + str(flags["tophat1"])
            elif flags['bgsub'] == 'Smoothed-Gaussian':
                text += "\n  - FWHM : " + str(flags["fwhm"])
                text += "\n  - Number of cycle : " + str(flags["cycles"])
            elif flags['bgsub'] == 'Smoothed-BoxCar':
                text += "\n  - Box car width : " + str(flags["boxcar_x"])
                text += "\n  - Box car height : " + str(flags["boxcar_y"])
                text += "\n  - Number of cycle : " + str(flags["cycles"])

        text += "\n  - Background Subtraction Method (Out): "+ str(self.bgChoiceOut.currentText())
        if flags['bgsub2'] != 'None':
            if flags['bgsub2'] in ['Circularly-symmetric', 'Roving Window']:
                text += "\n  - Pixel Range (Percentage) : " + str(flags["cirmin2"]) + "% - "+str(flags["cirmax2"])+"%"

            if flags['bgsub2'] == 'Circularly-symmetric':
                text += "\n  - Radial Bin : " + str(flags["radial_bin2"])
                text += "\n  - Smooth : " + str(flags["smooth2"])
            elif flags['bgsub2'] == '2D Convexhull':
                text += "\n  - Step (deg) : " + str(flags["deg2"])
            elif flags['bgsub2'] == 'White-top-hats':
                text += "\n  - Tophat : " + str(flags["tophat2"])
            elif flags['bgsub2'] == 'Smoothed-Gaussian':
                text += "\n  - FWHM : " + str(flags["fwhm2"])
                text += "\n  - Number of cycle : " + str(flags["cycles2"])
            elif flags['bgsub2'] == 'Smoothed-BoxCar':
                text += "\n  - Box car width : " + str(flags["boxcar_x2"])
                text += "\n  - Box car height : " + str(flags["boxcar_y2"])
                text += "\n  - Number of cycle : " + str(flags["cycles2"])


            text += "\n  - Merge Transition Radius : " + str(flags["transition_radius"])
            text += "\n  - Merge Transition Delta : " + str(flags["transition_delta"])

        text += '\n\nAre you sure you want to process ' + str(len(img_ids)) + ' image(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
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
        
        self.highlightApplyUndo()

    def processH5File(self):
        """
        Triggered when a folder with multiple H5 files has been selected to process it
        """
        start_idx, end_idx = self.file_manager.get_current_h5_range()
        self._process_image_list(range(start_idx, end_idx + 1), text="Process Current H5 File")

    def browseFile(self):
        """
        Popup input dialog and set file selection
        """
        self.newProcess = True

        success = False

        while not success:
            file_name = getAFile()
            if file_name != "":
                result = self.onNewFileSelected(str(file_name))

                success = result != "Retry"

                if success:
                    self.centralWidget.setMinimumSize(700, 500)
            else:
                # If the user presses Cancel, it returns a null string "".
                break

    def saveSettings(self):
        """
        save settings to json
        """
        settings = self.calSettings
        if self.quadFold is not None:
            if settings is None:
                settings = {}
            settings['compressed'] = self.compressFoldedImageChkBx.isChecked()
        if self.quadFold is not None and 'fixed_roi_rad' in self.quadFold.info:
            settings['fixed_roi_rad'] = self.quadFold.info['fixed_roi_rad']
        if self.quadFold is not None and 'bgsub' in self.quadFold.info:
            settings['bgsub'] = self.quadFold.info['bgsub']
        filename = getSaveFile(os.path.join("musclex", "settings", "qfsettings.json"), None)
        if filename != "":
            with open(filename, 'w') as f:
                json.dump(settings, f)

    def prevClicked(self, reprocess=False):
        """
        Going to the previous image
        """
        self.file_manager.prev_frame()
        self._navigate_and_update(reprocess=reprocess)

    def nextClicked(self, reprocess=False):
        """
        Going to the next image
        """
        self.file_manager.next_frame()
        self._navigate_and_update(reprocess=reprocess)


    def prevFileClicked(self, reprocess=False):
        """
        Going to the previous h5 file
        """
        self.file_manager.prev_file()
        self._navigate_and_update(reprocess=reprocess)


    def nextFileClicked(self, reprocess=False):
        """
        Going to the next h5 file
        """
        self.file_manager.next_file()
        self._navigate_and_update(reprocess=reprocess)


    def _navigate_and_update(self, reprocess=False):
        """
            Helper method for navigation: creates QuadrantFolder and applies settings
        """
        filename = self.file_manager.current_image_name
        self.quadFold = QuadrantFolder(self.file_manager.current_image, self.file_manager.dir_path, filename, self)
        
        # Don't clear info - let cache work!
        # Apply image-specific center settings if available
        # Presence in imageCenterSettings means manual mode
        if filename in self.imageCenterSettings:
            settings = self.imageCenterSettings[filename]
            center = tuple(settings['center'])
            # Restore center from settings (no need to save again)
            self.quadFold.setBaseCenter(center)
        
        # Apply image-specific rotation settings if available
        # Presence in imageRotationSettings means manual mode
        if filename in self.imageRotationSettings:
            settings = self.imageRotationSettings[filename]
            # Set base_rotation before processing
            self.quadFold.setBaseRotation(settings['rotation'])
        
        self.onImageChanged(reprocess=reprocess)

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        self.statusReport.setText(text) # will fix later with different threads
        print(text)
        QApplication.processEvents()

    def fileNameChanged(self):
        """
        Triggered when the name of the current file is changed
        """
        fileName = self.navControls.filenameLineEdit.text().strip()
        if fileName not in self.file_manager.names:
            return
        self.file_manager.switch_image_by_name(fileName)
        self._navigate_and_update()

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