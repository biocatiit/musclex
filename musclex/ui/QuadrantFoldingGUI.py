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
import copy
from os.path import split, splitext
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
import matplotlib.pyplot as plt
import pandas as pd
from PIL import Image
from musclex import __version__
from PyQt5.QtCore import QRunnable, QThreadPool, QEventLoop, pyqtSignal
from queue import Queue
import fabio
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.QuadrantFolder import QuadrantFolder
from ..csv_manager.QF_CSVManager import QF_CSVManager
from .pyqt_utils import *
from .BlankImageSettings import BlankImageSettings
from ..CalibrationSettings import CalibrationSettings
from threading import Lock

class QuadFoldParams:
    def __init__(self, flags, fileName, filePath, ext, fileList, parent):
        self.flags = flags
        self.fileName = fileName
        self.filePath = filePath
        self.ext = ext
        self.fileList = fileList
        self.parent = parent

class WorkerSignals(QObject):
    
    finished = pyqtSignal()
    error = pyqtSignal(tuple)
    result = pyqtSignal(object)


class Worker(QRunnable):

    def __init__(self, params):
        super().__init__()
        self.flags = params.flags
        self.params = params
        self.signals = WorkerSignals()
        self.lock = Lock()
        
    @pyqtSlot()
    def run(self):
        try:
            self.quadFold = QuadrantFolder(self.params.filePath, self.params.fileName, self.params.parent, self.params.fileList, self.params.ext)
            self.quadFold.info = {}
            self.quadFold.process(self.flags)
            if self.lock is not None:
                self.lock.acquire()
            with open(self.quadFold.img_path + "/qf_results/tasks_done.txt", "a") as file:
                file.write(self.quadFold.img_name + " saving image"+ "\n")
            if self.lock is not None:
                self.lock.release()
        except:
            traceback.print_exc()
            self.signals.error.emit((traceback.format_exc()))
        else:
            self.signals.result.emit(self.quadFold)
        finally:
            self.signals.finished.emit()

class QuadrantFoldingGUI(QMainWindow):
    """
    A class for window displaying all information of a selected image.
    This window contains 2 tabs : image, and result
    """
    def __init__(self):
        """
        Initial window
        """
        QWidget.__init__(self)
        self.imgList = [] # all images name in current directory
        self.h5List = [] # if the file selected is an H5 file, regroups all the other h5 files names
        self.h5index = 0
        self.filePath = "" # current directory
        self.extent = None
        self.img = None
        self.numberOfFiles = 0
        self.currentFileNumber = 0
        self.quadFold = None # QuadrantFolder object
        self.img_zoom = None # zoom location of original image (x,y range)
        self.default_img_zoom = None # default zoom calculated after processing image
        self.default_result_img_zoom = None # default result image zoom calculated after processing image
        self.zoomOutClicked = False # to check whether zoom out is clicked for using default zoom value
        self.result_zoom = None # zoom location of result image (x,y range)
        self.function = None # current active function
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
        self.masked = False
        self.csvManager = None
        
        self.threadPool = QThreadPool()
        self.tasksQueue = Queue()
        self.currentTask = None
        self.worker = None
        self.tasksDone = 0
        self.totalFiles = 1
        self.lock = Lock()
        
        self.rotationAngle = None

        self.calSettingsDialog = None
        self.doubleZoomMode = False
        self.dontShowAgainDoubleZoomMessageResult = False
        self.doubleZoomPt = (0, 0)
        self.doubleZoomAxes = None

        self.initUI() # initial all GUI
        self.setConnections() # set triggered function for widgets
        self.setMinimumHeight(900)
        self.newImgDimension = None
        self.browseFile()

    def initUI(self):
        """
        Open a file finder and return the name of the file selected
        """
        self.setWindowTitle("Muscle X Quadrant Folding v." + __version__)

        self.centralWidget = QWidget(self)
        self.setCentralWidget(self.centralWidget)
        self.mainHLayout = QHBoxLayout(self.centralWidget)

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 200px; }")
        self.mainHLayout.addWidget(self.tabWidget)

        ##### Image Tab #####
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.tabWidget.addTab(self.imageTab, "Original Image")

        self.verImgLayout = QVBoxLayout()
        # self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)
        self.selectImageButton = QPushButton('Click Here to Select an Image...')
        self.selectImageButton.setFixedHeight(100)
        self.selectImageButton.setFixedWidth(300)

        self.selectFolder = QPushButton('Click Here to Select a Folder...')
        self.selectFolder.setFixedHeight(100)
        self.selectFolder.setFixedWidth(300)

        self.bgWd = QWidget()
        self.verImgLayout.addWidget(self.selectImageButton)
        self.imageFigure = plt.figure()
        self.imageAxes = self.imageFigure.add_subplot(111)
        self.imageCanvas = FigureCanvas(self.imageFigure)

        self.imageCanvas.setHidden(True)
        self.imageTabLayout.addLayout(self.verImgLayout)
        self.imageTabLayout.addWidget(self.imageCanvas)

        self.displayOptGrpBx = QGroupBox()
        self.displayOptGrpBx.setTitle("Display Options")
        # self.displayOptGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self.dispOptLayout = QGridLayout()

        self.spminInt = QDoubleSpinBox()
        self.spminInt.setToolTip("Reduction in the maximal intensity shown to allow for more details in the image.")
        self.spminInt.setKeyboardTracking(False)
        self.spminInt.setSingleStep(5)
        self.spminInt.setDecimals(0)
        self.spmaxInt = QDoubleSpinBox()
        self.spmaxInt.setToolTip("Increase in the minimal intensity shown to allow for more details in the image.")
        self.spmaxInt.setKeyboardTracking(False)
        self.spmaxInt.setSingleStep(5)
        self.spmaxInt.setDecimals(0)
        self.logScaleIntChkBx = QCheckBox("Log scale intensity")
        self.persistIntensity = QCheckBox("Persist intensities")

        self.showSeparator = QCheckBox()
        self.showSeparator.setText("Show Quadrant Separator")
        self.showSeparator.setChecked(True)

        self.imgZoomInB = QPushButton("Zoom in")
        self.imgZoomInB.setCheckable(True)
        self.imgZoomOutB = QPushButton("Full")
        self.checkableButtons.append(self.imgZoomInB)

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')
        self.dispOptLayout.addWidget(self.showSeparator, 0, 0, 1, 4)
        self.dispOptLayout.addWidget(self.minIntLabel, 1, 0, 1, 2)
        self.dispOptLayout.addWidget(self.spminInt, 2, 0, 1, 2)
        self.dispOptLayout.addWidget(self.maxIntLabel, 1, 2, 1, 2)
        self.dispOptLayout.addWidget(self.spmaxInt, 2, 2, 1, 2)
        self.dispOptLayout.addWidget(self.logScaleIntChkBx, 3, 0, 1, 2)
        self.dispOptLayout.addWidget(self.persistIntensity, 3, 2, 1, 2)
        self.dispOptLayout.addWidget(self.imgZoomInB, 4, 0, 1, 2)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 4, 2, 1, 2)

        self.displayOptGrpBx.setLayout(self.dispOptLayout)

        self.optionsLayout = QVBoxLayout()
        # self.optionsLayout.setAlignment(Qt.AlignCenter)
        self.settingsGroup = QGroupBox("Image Processing")
        self.settingsLayout = QGridLayout()
        self.settingsGroup.setLayout(self.settingsLayout)

        self.calibrationButton = QPushButton("Calibration Settings")
        self.setCenterRotationButton = QPushButton("Set Rotation Angle and Center")
        self.setCenterRotationButton.setCheckable(True)
        self.checkableButtons.append(self.setCenterRotationButton)
        self.setCentByChords = QPushButton("Set Center by Chords")
        self.setCentByChords.setCheckable(True)
        self.checkableButtons.append(self.setCentByChords)
        self.setCentByPerp = QPushButton("Set Center by Perpendiculars")
        self.setCentByPerp.setCheckable(True)
        self.checkableButtons.append(self.setCentByPerp)
        self.setRotationButton = QPushButton("Set Rotation Angle")
        self.setRotationButton.setCheckable(True)
        self.checkableButtons.append(self.setRotationButton)
        
        self.persistRotations = QCheckBox("Persist Rotations")
        self.persistRotations.setVisible(False)

        self.maskThresSpnBx = QDoubleSpinBox()
        self.maskThresSpnBx.setMinimum(-999)
        self.maskThresSpnBx.setMaximum(999)
        self.maskThresSpnBx.setValue(-999)
        self.maskThresSpnBx.setKeyboardTracking(False)

        self.orientationCmbBx = QComboBox()
        self.orientationCmbBx.addItem("Max Intensity")
        self.orientationCmbBx.addItem("GMM")
        self.orientationCmbBx.addItem("Herman Factor (Half Pi)")
        self.orientationCmbBx.addItem("Herman Factor (Pi)")

        self.modeAngleChkBx = QCheckBox("Mode orientation")
        self.modeAngleChkBx.setChecked(False)

        # self.expandImage = QCheckBox("Expand the Image")
        # self.expandImage.setChecked(False)
        # self.expandImage.setToolTip("Expand the size of the image, for images with an offset center")

        self.compressFoldedImageChkBx = QCheckBox("Save Compressed Image")
        self.compressFoldedImageChkBx.setChecked(True)
        self.compressFoldedImageChkBx.setToolTip("Saves the images as compressed tifs (might not be compatible with fit2d, but works with imagej)")

        self.cropFoldedImageChkBx = QCheckBox("Save Cropped Image (Original Size)")
        self.cropFoldedImageChkBx.setChecked(False)

        self.doubleZoom = QCheckBox("Double Zoom")
        self.dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")

        self.toggleFoldImage = QCheckBox("Fold Image")
        self.toggleFoldImage.setChecked(True)

        self.settingsLayout.addWidget(self.calibrationButton, 0, 0, 1, 4)
        self.settingsLayout.addWidget(self.setCentByChords, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.setCentByPerp, 1, 2, 1, 2)
        self.settingsLayout.addWidget(self.setCenterRotationButton, 2, 0, 1, 2)
        self.settingsLayout.addWidget(self.setRotationButton, 2, 2, 1, 2)
        self.settingsLayout.addWidget(self.persistRotations, 3, 0, 1, 4)
        self.settingsLayout.addWidget(QLabel("Mask Threshold : "), 4, 0, 1, 2)
        self.settingsLayout.addWidget(self.maskThresSpnBx, 4, 2, 1, 2)
        self.settingsLayout.addWidget(QLabel("Orientation Finding: "), 5, 0, 1, 2)
        self.settingsLayout.addWidget(self.orientationCmbBx, 5, 2, 1, 2)
        self.settingsLayout.addWidget(self.modeAngleChkBx, 6, 0, 1, 4)
        # self.settingsLayout.addWidget(self.expandImage, 7, 0, 1, 4)
        self.settingsLayout.addWidget(self.compressFoldedImageChkBx, 7, 0, 1, 4)
        self.settingsLayout.addWidget(self.cropFoldedImageChkBx, 8, 0, 1, 4)
        self.settingsLayout.addWidget(self.doubleZoom, 9, 0, 1, 4)
        self.settingsLayout.addWidget(self.toggleFoldImage, 10, 0, 1, 4)

        # Blank Image Settings
        self.blankImageGrp = QGroupBox("Enable Blank Image and Mask")
        self.blankImageGrp.setCheckable(True)
        self.blankImageGrp.setChecked(False)
        self.blankImageLayout = QVBoxLayout(self.blankImageGrp)
        self.blankSettingButton = QPushButton("Set Blank Image and Mask")
        self.blankImageLayout.addWidget(self.blankSettingButton)

        # Result processing and background Subtraction
        self.resProcGrpBx = QGroupBox()
        self.resProcGrpBx.setTitle("Result Processing")
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

        self.bgChoice = QComboBox()
        self.bgChoice.setCurrentIndex(0)
        # self.bgChoice.setFixedHeight(40)
        self.allBGChoices = ['None','Circularly-symmetric', 'Roving Window', 'White-top-hats', 'Smoothed-Gaussian', 'Smoothed-BoxCar'] # '2D Convexhull',
        for c in self.allBGChoices:
            self.bgChoice.addItem(c)

        self.setRminmaxButton = QPushButton("Set Manual R-min and R-max")
        self.setRminmaxButton.setCheckable(True)
        self.checkableButtons.append(self.setRminmaxButton)

        self.rminSpnBx = QSpinBox()
        self.rminSpnBx.setSingleStep(2)
        self.rminSpnBx.setValue(-1)
        self.rminSpnBx.setRange(-1, 3000)
        self.rminSpnBx.setKeyboardTracking(False)
        self.rminLabel = QLabel("R-min")

        self.rmaxSpnBx = QSpinBox()
        self.rmaxSpnBx.setSingleStep(10)
        self.rmaxSpnBx.setValue(-1)
        self.rmaxSpnBx.setRange(-1, 3000)
        self.rmaxSpnBx.setKeyboardTracking(False)
        self.rmaxLabel = QLabel("R-max")
        self.radiusLabel = QLabel("Radius Range : ")
        self.fixedRadiusRangeChkBx = QCheckBox("Fixed Radius Range")

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
        self.tophat1Label = QLabel("Top-hat inside R-max : ")

        self.tophat2SpnBx = QSpinBox()
        self.tophat2SpnBx.setRange(1, 100)
        self.tophat2SpnBx.setValue(20)
        self.tophat2SpnBx.setKeyboardTracking(False)
        self.tophat2Label = QLabel("Top-hat outside R-max : ")

        self.applyBGButton = QPushButton("Apply")

        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        separator.setLineWidth(1)
        self.mergeGradientLabel = QLabel("Merge Gradient :")
        self.sigmoidSpnBx = QDoubleSpinBox()
        self.sigmoidSpnBx.setDecimals(5)
        self.sigmoidSpnBx.setSingleStep(2)
        self.sigmoidSpnBx.setValue(0.1)
        self.sigmoidSpnBx.setRange(0, 100)
        self.sigmoidSpnBx.setKeyboardTracking(False)

        self.tophat2Widgets = [self.tophat2SpnBx, self.tophat2Label, self.mergeGradientLabel, self.sigmoidSpnBx, separator]

        self.bgLayout = QGridLayout()
        self.bgLayout.addWidget(self.setFitRoi, 0, 0, 1, 3)
        self.bgLayout.addWidget(self.unsetRoi, 0, 3, 1, 1)
        self.bgLayout.addWidget(self.fixedRoiChkBx, 1, 0, 1, 2)
        self.bgLayout.addWidget(self.fixedRoi, 1, 2, 1, 2)
        self.bgLayout.addWidget(QLabel("Background Subtraction :"), 2, 0, 1, 2)
        self.bgLayout.addWidget(self.bgChoice, 2, 2, 1, 2)

        # R-min R-max settings
        self.rrangeSettingFrame = QFrame()
        self.rrangeSettingLayout = QGridLayout(self.rrangeSettingFrame)
        self.rrangeSettingLayout.setContentsMargins(0, 0, 0, 0)
        self.rrangeSettingLayout.addWidget(self.setRminmaxButton, 0, 0, 1, 4)
        self.rrangeSettingLayout.addWidget(self.radiusLabel, 1, 0, 2, 2)
        self.rrangeSettingLayout.addWidget(self.rminLabel, 1, 2, 1, 1)
        self.rrangeSettingLayout.addWidget(self.rmaxLabel, 1, 3, 1, 1)
        self.rrangeSettingLayout.addWidget(self.rminSpnBx, 2, 2, 1, 1)
        self.rrangeSettingLayout.addWidget(self.rmaxSpnBx, 2, 3, 1, 1)
        self.rrangeSettingLayout.addWidget(self.fixedRadiusRangeChkBx, 3, 0, 1, 4)
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
        self.bgLayout.addWidget(self.smoothLabel, 14, 0, 1, 2)
        self.bgLayout.addWidget(self.smoothSpnBx, 14, 2, 1, 2)

        # Tension
        self.bgLayout.addWidget(self.tensionLabel, 15, 0, 1, 2)
        self.bgLayout.addWidget(self.tensionSpnBx, 15, 2, 1, 2)

        # White top hat (inside R-max)
        self.bgLayout.addWidget(self.tophat1Label, 16, 0, 1, 2)
        self.bgLayout.addWidget(self.tophat1SpnBx, 16, 2, 1, 2)
        self.bgLayout.addWidget(separator, 17, 0, 1, 4)

        # White top hat (outside R-max)
        self.bgLayout.addWidget(self.tophat2Label, 18, 0, 1, 2)
        self.bgLayout.addWidget(self.tophat2SpnBx, 18, 2, 1, 2)

        # Merging params
        self.bgLayout.addWidget(self.mergeGradientLabel, 19, 0, 1, 2)
        self.bgLayout.addWidget(self.sigmoidSpnBx, 19, 2, 1, 2)

        # Apply button
        self.bgLayout.addWidget(self.applyBGButton, 20, 0, 1, 4)

        # self.bgLayout.setColumnStretch(0, 2)

        self.resProcGrpBx.setLayout(self.bgLayout)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet(pfss)
        self.processFolderButton.setCheckable(True)
        self.processH5FolderButton = QPushButton("Process All H5 Files")
        self.processH5FolderButton.setStyleSheet(pfss)
        self.processH5FolderButton.setCheckable(True)

        self.nextButton = QPushButton(">")
        self.prevButton = QPushButton("<")
        self.nextFileButton = QPushButton(">>>")
        self.prevFileButton = QPushButton("<<<")
        self.nextButton.setToolTip('Next Frame')
        self.prevButton.setToolTip('Previous Frame')
        self.nextFileButton.setToolTip('Next H5 File in this Folder')
        self.prevFileButton.setToolTip('Previous H5 File in this Folder')
        self.filenameLineEdit = QLineEdit()
        self.buttonsLayout = QGridLayout()
        self.buttonsLayout.addWidget(self.processFolderButton,0,0,1,4)
        self.buttonsLayout.addWidget(self.processH5FolderButton,1,0,1,4)
        self.buttonsLayout.addWidget(self.prevButton,2,0,1,2)
        self.buttonsLayout.addWidget(self.nextButton,2,2,1,2)
        self.buttonsLayout.addWidget(self.prevFileButton,3,0,1,2)
        self.buttonsLayout.addWidget(self.nextFileButton,3,2,1,2)
        self.buttonsLayout.addWidget(self.filenameLineEdit,4,0,1,4)

        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.blankImageGrp)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingsGroup)

        self.optionsLayout.addStretch()
        self.optionsLayout.addLayout(self.buttonsLayout)
        self.frameOfKeys = QFrame()
        self.frameOfKeys.setFixedWidth(500)
        self.frameOfKeys.setLayout(self.optionsLayout)
        self.imageTabLayout.addWidget(self.frameOfKeys)

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
        self.resultVLayout = QVBoxLayout()
        self.resultCanvas = FigureCanvas(self.resultFigure)
        self.resultTabLayout.addWidget(self.resultCanvas)

        self.rightLayout = QVBoxLayout()
        self.rightFrame = QFrame()
        self.rightFrame.setFixedWidth(500)
        self.rightFrame.setLayout(self.rightLayout)
        self.resultTabLayout.addWidget(self.rightFrame)

        # Display Options
        self.resultDispOptGrp = QGroupBox("Display Options")
        self.resultDispOptLayout = QGridLayout(self.resultDispOptGrp)

        self.rotate90Chkbx = QCheckBox("Rotate 90 degree")

        self.spResultmaxInt = QDoubleSpinBox()
        self.spResultmaxInt.setToolTip(
            "Reduction in the maximal intensity shown to allow for more details in the image.")
        self.spResultmaxInt.setKeyboardTracking(False)
        self.spResultmaxInt.setSingleStep(5)
        self.spResultmaxInt.setDecimals(0)

        self.spResultminInt = QDoubleSpinBox()
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

        self.rightLayout.addWidget(self.resultDispOptGrp)
        self.rightLayout.addSpacing(10)
        self.rightLayout.addWidget(self.resProcGrpBx)
        self.rightLayout.addStretch()

        self.processFolderButton2 = QPushButton("Process Current Folder")
        self.processFolderButton2.setStyleSheet(pfss)
        self.processFolderButton2.setCheckable(True)
        self.processH5FolderButton2 = QPushButton("Process All H5 Files")
        self.processH5FolderButton2.setStyleSheet(pfss)
        self.processH5FolderButton2.setCheckable(True)
        self.nextButton2 = QPushButton(">")
        self.prevButton2 = QPushButton("<")
        self.nextFileButton2 = QPushButton(">>>")
        self.prevFileButton2 = QPushButton("<<<")
        self.nextButton2.setToolTip('Next Frame')
        self.prevButton2.setToolTip('Previous Frame')
        self.nextFileButton2.setToolTip('Next H5 File in this Folder')
        self.prevFileButton2.setToolTip('Previous H5 File in this Folder')
        self.filenameLineEdit2 = QLineEdit()
        self.buttonsLayout2 = QGridLayout()
        self.buttonsLayout2.addWidget(self.processFolderButton2, 0, 0, 1, 4)
        self.buttonsLayout2.addWidget(self.processH5FolderButton2, 1, 0, 1, 4)
        self.buttonsLayout2.addWidget(self.prevButton2, 2, 0, 1, 2)
        self.buttonsLayout2.addWidget(self.nextButton2, 2, 2, 1, 2)
        self.buttonsLayout2.addWidget(self.prevFileButton2, 3, 0, 1, 2)
        self.buttonsLayout2.addWidget(self.nextFileButton2, 3, 2, 1, 2)
        self.buttonsLayout2.addWidget(self.filenameLineEdit2, 4, 0, 1, 4)
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
        self.setStatusBar(self.statusBar)


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

        self.bgChoiceChanged()
        self.show()

    def setConnections(self):
        """
        Set all triggered functions for widgets
        """
        self.tabWidget.currentChanged.connect(self.updateUI)

        ##### Image Tab #####
        self.selectFolder.clicked.connect(self.browseFolder)
        self.spminInt.valueChanged.connect(self.refreshImageTab)
        self.spmaxInt.valueChanged.connect(self.refreshImageTab)
        self.logScaleIntChkBx.stateChanged.connect(self.refreshImageTab)
        self.showSeparator.stateChanged.connect(self.refreshAllTabs)
        self.orientationCmbBx.currentIndexChanged.connect(self.orientationModelChanged)
        self.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.processFolderButton2.toggled.connect(self.batchProcBtnToggled)
        self.processH5FolderButton.toggled.connect(self.h5batchProcBtnToggled)
        self.processH5FolderButton2.toggled.connect(self.h5batchProcBtnToggled)
        self.nextButton.clicked.connect(self.nextClicked)
        self.prevButton.clicked.connect(self.prevClicked)
        self.nextFileButton.clicked.connect(self.nextFileClicked)
        self.prevFileButton.clicked.connect(self.prevFileClicked)
        self.filenameLineEdit.editingFinished.connect(self.fileNameChanged)
        self.nextButton2.clicked.connect(self.nextClicked)
        self.prevButton2.clicked.connect(self.prevClicked)
        self.nextFileButton2.clicked.connect(self.nextFileClicked)
        self.prevFileButton2.clicked.connect(self.prevFileClicked)
        self.filenameLineEdit2.editingFinished.connect(self.fileNameChanged)
        self.spResultmaxInt.valueChanged.connect(self.refreshResultTab)
        self.spResultminInt.valueChanged.connect(self.refreshResultTab)
        self.resLogScaleIntChkBx.stateChanged.connect(self.refreshResultTab)
        self.modeAngleChkBx.clicked.connect(self.modeAngleChecked)
        self.doubleZoom.stateChanged.connect(self.doubleZoomChecked)
        self.toggleFoldImage.stateChanged.connect(self.onFoldChkBoxToggled)
        self.cropFoldedImageChkBx.stateChanged.connect(self.cropFoldedImageChanged)
        self.compressFoldedImageChkBx.stateChanged.connect(self.compressFoldedImageChanged)
        # self.expandImage.stateChanged.connect(self.expandImageChecked)

        self.selectImageButton.clicked.connect(self.browseFile)
        self.imgZoomInB.clicked.connect(self.imageZoomIn)
        self.imgZoomOutB.clicked.connect(self.imageZoomOut)
        self.calibrationButton.clicked.connect(self.calibrationClicked)
        self.setCenterRotationButton.clicked.connect(self.setCenterRotation)
        self.setRotationButton.clicked.connect(self.setRotation)
        self.setCentByChords.clicked.connect(self.setCenterByChordsClicked)
        self.setCentByPerp.clicked.connect(self.setCenterByPerpClicked)
        self.maskThresSpnBx.valueChanged.connect(self.ignoreThresChanged)
        self.imageFigure.canvas.mpl_connect('button_press_event', self.imageClicked)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)
        self.imageFigure.canvas.mpl_connect('button_release_event', self.imageReleased)
        self.imageFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)
        
        self.persistRotations.stateChanged.connect(self.persistRotationsChecked)

        ##### Result Tab #####
        self.rotate90Chkbx.stateChanged.connect(self.processImage)
        self.resultZoomInB.clicked.connect(self.resultZoomIn)
        self.resultZoomOutB.clicked.connect(self.resultZoomOut)
        self.resultFigure.canvas.mpl_connect('button_press_event', self.resultClicked)
        self.resultFigure.canvas.mpl_connect('motion_notify_event', self.resultOnMotion)
        self.resultFigure.canvas.mpl_connect('button_release_event', self.resultReleased)
        self.resultFigure.canvas.mpl_connect('scroll_event', self.resultScrolled)

        # Blank image and mask
        self.blankSettingButton.clicked.connect(self.blankSettingClicked)

        # Background Subtraction
        self.setFitRoi.clicked.connect(self.setFitRoiClicked)
        self.unsetRoi.clicked.connect(self.unsetRoiClicked)
        self.fixedRoiChkBx.stateChanged.connect(self.fixedRoiChecked)
        self.fixedRoi.editingFinished.connect(self.fixedRoiChanged)
        self.bgChoice.currentIndexChanged.connect(self.bgChoiceChanged)
        self.minPixRange.valueChanged.connect(self.pixRangeChanged)
        self.maxPixRange.valueChanged.connect(self.pixRangeChanged)
        self.setRminmaxButton.clicked.connect(self.setManualRminmax)
        self.rminSpnBx.valueChanged.connect(self.RminRmaxChanged)
        self.rmaxSpnBx.valueChanged.connect(self.RminRmaxChanged)
        self.sigmoidSpnBx.valueChanged.connect(self.sigmoidChanged)
        self.applyBGButton.clicked.connect(self.applyBGSub)

        self.blankImageGrp.clicked.connect(self.blankChecked)
        
    def persistRotationsChecked(self):
        if self.persistRotations.isChecked():
            self.rotationAngle = self.quadFold.info['rotationAngle']

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
            fileName = self.imgList[self.currentFileNumber]
            self.quadFold = QuadrantFolder(self.filePath, fileName, self, self.fileList, self.ext)
            self.masked = False
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
        dlg = BlankImageSettings(self.filePath)
        result = dlg.exec_()
        if result == 1 and self.quadFold is not None:
            self.quadFold.delCache()
            fileName = self.imgList[self.currentFileNumber]
            self.quadFold = QuadrantFolder(self.filePath, fileName, self, self.fileList, self.ext)
            self.masked = False
            self.processImage()

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
        else:
            self.function = None
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
        Prepare for manual center selection using perpendicular peaks
        :return:
        """
        if self.quadFold is None:
            return
        if self.setCentByPerp.isChecked():
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.imageCanvas.draw_idle()
            self.function = ["perp_center"]  # set current active function
        else:
            QApplication.restoreOverrideCursor()

            func = self.function
            horizontalLines = []
            verticalLines = []
            intersections = []
            for i in range(1, len(func) - 1, 2):
                slope = calcSlope(func[i], func[i + 1])
                if abs(slope) > 1:
                    verticalLines.append((func[i], func[i + 1]))
                else:
                    horizontalLines.append((func[i], func[i + 1]))
            for line1 in verticalLines:
                for line2 in horizontalLines:
                    cx, cy = getIntersectionOfTwoLines(line2, line1)
                    print("Intersection ", (cx, cy))
                    intersections.append((cx, cy))
            cx = int(sum([intersections[i][0] for i in range(0, len(intersections))]) / len(intersections))
            cy = int(sum([intersections[i][1] for i in range(0, len(intersections))]) / len(intersections))

            print("Center calc ", (cx, cy))

            extent, _ = self.getExtentAndCenter()
            new_center = [cx, cy]  # np.dot(invM, homo_coords)
            # Set new center and rotaion angle , re-calculate R-min
            print("New Center ", new_center)
            self.quadFold.info['manual_center'] = (
            int(round(new_center[0])) + extent[0], int(round(new_center[1])) + extent[1])
            if 'center' in self.quadFold.info:
                del self.quadFold.info['center']
            print("New center after extent ", self.quadFold.info['manual_center'])
            self.deleteInfo(['avg_fold'])
            self.newImgDimension = None
            self.setCentByPerp.setChecked(False)
            self.processImage()

    def setCenterByChordsClicked(self):
        """
        Prepare for manual rotation center setting by selecting chords
        """
        if self.quadFold is None:
            return

        if self.setCentByChords.isChecked():
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.chordpoints=[]
            self.chordLines = []
            self.imageCanvas.draw_idle()
            self.function = ["chords_center"]  # set current active function
        else:
            QApplication.restoreOverrideCursor()
            print("Finding Chords center ...")
            centers = []
            for i, line1 in enumerate(self.chordLines):
                for line2 in self.chordLines[i + 1:]:
                    if line1[0] == line2[0]:
                        continue  # parallel lines
                    if line1[0] == float('inf'):
                        xcent = line1[1]
                        ycent = line2[0] * xcent + line2[1]
                    elif line2[0] == float('inf'):
                        xcent = line2[1]
                        ycent = line1[0] * xcent + line1[1]
                    else:
                        xcent = (line2[1] - line1[1]) / (line1[0] - line2[0])
                        ycent = line1[0] * xcent + line1[1]
                    center = [xcent, ycent]
                    print("CenterCalc ", center)

                    centers.append(center)

            extent, center = self.getExtentAndCenter()

            cx = int(sum([centers[i][0] for i in range(0, len(centers))]) / len(centers))
            cy = int(sum([centers[i][1] for i in range(0, len(centers))]) / len(centers))
            new_center = [cx, cy] #np.dot(invM, homo_coords)
            print("New center ", new_center)
            # Set new center and rotaion angle , re-calculate R-min
            self.quadFold.info['manual_center'] = (int(round(new_center[0])) + extent[0], int(round(new_center[1])) + extent[1])
            if 'center' in self.quadFold.info:
                del self.quadFold.info['center']
            print("New center after extent ", self.quadFold.info['manual_center'])
            self.deleteInfo(['avg_fold'])
            self.newImgDimension = None
            self.setCentByChords.setChecked(False)
            self.processImage()

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
        Trigger when set center and rotation angle button is pressed
        """
        if self.setRotationButton.isChecked():
            # clear plot
            self.imgPathOnStatusBar.setText(
                "Rotate the line to the pattern equator (ESC to cancel)")
            # ax = self.imageAxes
            # for i in range(len(ax.lines)-1,-1,-1):
            #     ax.lines[i].remove()
            # for i in range(len(ax.patches)-1,-1,-1):
            #     ax.patches[i].remove()
            _, center = self.getExtentAndCenter()
            self.quadFold.info['center'] = center
            self.imageCanvas.draw_idle()
            self.function = ["im_rotate"]
        else:
            self.function = None
            self.resetStatusbar()

    def calibrationClicked(self):
        """
        Handle when Calibration Settings button is clicked
        :return:
        """
        sucess = self.setCalibrationImage(force=True)
        if sucess:
            self.deleteInfo(['rotationAngle'])
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
        cal_setting = self.calSettingsDialog.calSettings
        if cal_setting is not None or force:
            result = self.calSettingsDialog.exec_()
            if result == 1:
                self.calSettings = self.calSettingsDialog.getValues()

                if self.calSettings is not None:
                    if self.calSettingsDialog.fixedCenter.isChecked():
                        self.quadFold.info['calib_center'] = self.calSettings['center']
                        self.setCenterRotationButton.setEnabled(False)
                        self.setCenterRotationButton.setToolTip(
                            "Please uncheck fixed center in calibration settings first")
                        if 'manual_center' in self.quadFold.info:
                            del self.quadFold.info['manual_center']
                        if 'center' in self.quadFold.info:
                            del self.quadFold.info['center']
                    else:
                        self.setCenterRotationButton.setEnabled(True)
                        self.setCenterRotationButton.setToolTip("")
                        if self.quadFold is not None and 'calib_center' in self.quadFold.info:
                            del self.quadFold.info['calib_center']
                        if self.quadFold is not None and 'center' in self.quadFold.info:
                            del self.quadFold.info['center']

                return True
        return False

    def setCenterRotation(self):
        """
        Trigger when set center and rotation angle button is pressed
        """
        if self.setCenterRotationButton.isChecked():
            # clear plot
            self.imgPathOnStatusBar.setText("Click on 2 corresponding reflection peaks along the equator (ESC to cancel)")
            # ax = self.imageAxes
            # for i in range(len(ax.lines)-1,-1,-1):
            #     ax.lines[i].remove()
            # for i in range(len(ax.patches)-1,-1,-1):
            #     ax.patches[i].remove()
            self.imageCanvas.draw_idle()
            self.function = ["im_center_rotate"]
        else:
            self.function = None
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
        else:
            self.function = None
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
        Trigger when set zoom in button is pressed (image tab)
        """
        if self.imgZoomInB.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a rectangle on the image to zoom in (ESC to cancel)")
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.imageCanvas.draw_idle()
            self.function = ["im_zoomin"]
        else:
            self.function = None
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

    def imageClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return
        x = event.xdata
        y = event.ydata
        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.imgCoordOnStatusBar.setText("")
            ax = self.imageAxes
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
        elif self.doubleZoomMode:
            # If x, y is inside figure and image is clicked for first time in double zoom mode
            print(x,y)
            if not self.dontShowAgainDoubleZoomMessageResult:
                msg = QMessageBox()
                msg.setInformativeText(
                    "Please click on zoomed window on the top right")
                dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")
                msg.setStandardButtons(QMessageBox.Ok)
                msg.setWindowTitle("Double Zoom Guide")
                msg.setStyleSheet("QLabel{min-width: 500px;}")
                msg.setCheckBox(dontShowAgainDoubleZoomMessage)
                msg.exec_()
                self.dontShowAgainDoubleZoomMessageResult = dontShowAgainDoubleZoomMessage.isChecked()
            self.doubleZoomMode = False
            return

        if self.function is not None and self.function[0] == 'ignorefold':
            self.function = None

        if self.doubleZoom.isChecked() and not self.doubleZoomMode:
            x, y = self.doubleZoomToOrigCoord(x, y)
            self.doubleZoomMode = True

        # Provide different behavior depending on current active function
        if self.function is None:
            if event.button == 3:
                # If the click is left-click, popup a ignore quadrant
                menu = QMenu(self)
                fold_number = self.quadFold.getFoldNumber(x, y)
                self.function = ["ignorefold", (x, y)]
                if fold_number not in self.quadFold.info["ignore_folds"]:
                    ignoreThis = QAction('Ignore This Quadrant', self)
                    ignoreThis.triggered.connect(self.addIgnoreQuadrant)
                    menu.addAction(ignoreThis)
                else:
                    unignoreThis = QAction('Unignore This Quadrant', self)
                    unignoreThis.triggered.connect(self.removeIgnoreQuadrant)
                    menu.addAction(unignoreThis)
                menu.popup(QCursor.pos())
            else:
                self.function = ["im_move", (x, y)]
        else:
            func = self.function
            if func[0] == "im_zoomin":
                # zoom in image
                func.append((x, y))
                if len(func) == 3:
                    p1 = func[1]
                    p2 = func[2]
                    self.img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.imgZoomInB.setChecked(False)
                    self.refreshImageTab()
            elif func[0] == "chords_center":
                ax = self.imageAxes
                axis_size = 5
                self.chordpoints.append([x, y])
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                if len(self.chordpoints) >= 3:
                    self.drawPerpendiculars()
                self.imageCanvas.draw_idle()
            elif func[0] == "perp_center":
                ax = self.imageAxes
                axis_size = 5
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                if self.doubleZoom.isChecked() and len(func) > 1 and len(func) % 2 == 0:
                    start_pt = func[len(func) - 1]
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                self.imageCanvas.draw_idle()
                func.append((x, y))
            elif func[0] == "im_center_rotate":
                # set center and rotation angle
                ax = self.imageAxes
                axis_size = 5
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                self.imageCanvas.draw_idle()
                func.append((x, y))
                if len(func) == 3:
                    if func[1][0] < func[2][0]:
                        x1, y1 = func[1]
                        x2, y2 = func[2]
                    else:
                        x1, y1 = func[2]
                        x2, y2 = func[1]

                    if abs(x2 - x1) == 0:
                        new_angle = -90
                    else:
                        new_angle = -180. * np.arctan((y1 - y2) / abs(x1 - x2)) / np.pi

                    extent, center = self.getExtentAndCenter()

                    cx = int(round((x1 + x2) / 2.) + extent[0])
                    cy = int(round((y1 + y2) / 2.) + extent[1])
                    # M = cv2.getRotationMatrix2D(tuple(self.quadFold.info['center']), self.quadFold.info['rotationAngle'], 1)
                    new_center = [cx, cy]
                    cx = int(round(new_center[0]))
                    cy = int(round(new_center[1]))
                    self.quadFold.info['manual_center'] = (cx, cy)
                    if 'center' in self.quadFold.info:
                        del self.quadFold.info['center']
                    self.quadFold.info['manual_rotationAngle'] = self.quadFold.info['rotationAngle'] + new_angle
                    self.deleteInfo(['avg_fold'])
                    self.newImgDimension = None
                    self.setCenterRotationButton.setChecked(False)
                    self.persistRotations.setVisible(True)
                    self.processImage()
            elif func[0] == "im_rotate":
                # set rotation angle
                extent, center = self.getExtentAndCenter()

                if center[0] < x:
                    x1 = center[0]
                    y1 = center[1]
                    x2 = x
                    y2 = y
                else:
                    x1 = x
                    y1 = y
                    x2 = center[0]
                    y2 = center[1]

                if abs(x2 - x1) == 0:
                    new_angle = -90
                else:
                    new_angle = -180. * np.arctan((y1 - y2) / abs(x1 - x2)) / np.pi

                self.quadFold.info['manual_rotationAngle'] = self.quadFold.info['rotationAngle'] + new_angle
                self.deleteInfo(['avg_fold'])
                self.setRotationButton.setChecked(False)
                self.persistRotations.setVisible(True)
                self.processImage()

    def imageOnMotion(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return

        x = event.xdata
        y = event.ydata
        img = self.img

        if img is None:
            return

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < img.shape[1] and y < img.shape[0]:
                extent = self.extent
                sx = x + extent[0]
                sy = y + extent[1]
                self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[int(sy)][int(sx)]))
                if self.doubleZoom.isChecked() and self.doubleZoomMode and sx>10 and sx<img.shape[1]-10 and sy>10 and sy<img.shape[0]-10:
                    ax1 = self.doubleZoomAxes
                    imgCropped = img[int(sy - 10):int(sy + 10), int(sx - 10):int(sx + 10)]
                    if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                        imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                        self.doubleZoomPt = (x,y)
                        ax1.imshow(imgScaled)
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        for i in range(len(ax1.patches)-1,-1,-1):
                            ax1.patches[i].remove()
                        self.imageCanvas.draw_idle()

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
        if func[0] == "im_zoomin" and len(self.function) == 2:
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
            self.imageCanvas.draw_idle()
        elif func[0] == "im_move":
            # change zoom-in location (x,y ranges) to move around image
            if self.img_zoom is not None:
                move = (func[1][0] - x, func[1][1] - y)
                self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[1], img.shape[0])
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
                ax.invert_yaxis()
                self.imageCanvas.draw_idle()

        elif func[0] == "im_center_rotate":
            # draw X on points and a line between points
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    # first_cross = ax.lines[:2]
                    for i in range(len(ax.lines)-1,1,-1):
                        ax.lines[i].remove()
                    # ax.lines = first_cross
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.imageCanvas.draw_idle()

        elif func[0] == "perp_center":
            # draw X on points and a line between points
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5

            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines) - 1, -1, -1):
                        ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    for i in range(len(ax.lines) - 1, 1, -1):
                        ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) % 2 != 0:
                if len(ax.lines) > 0:
                    n = (len(func)-1)*5//2 + 2
                    for i in range(len(ax.lines) - 1, n - 1, -1):
                        ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) % 2 == 0:
                start_pt = func[-1]
                if len(ax.lines) > 3:
                    n = len(func) * 5 // 2 - 1
                    for i in range(len(ax.lines) - 1, n - 1, -1):
                        ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.imageCanvas.draw_idle()

        elif func[0] == "chords_center":
            if self.doubleZoom.isChecked():
                if (not self.doubleZoomMode) and x < 200 and y < 200:
                    axis_size = 1
                    ax1 = self.doubleZoomAxes
                    if len(ax1.lines) > 0:
                        for i in range(len(ax1.lines)-1,-1,-1):
                            ax1.lines[i].remove()
                    ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.imageCanvas.draw_idle()

        elif func[0] == "im_rotate":
            # draw line as angle
            if self.calSettings is None or 'center' not in self.calSettings:
                self.calSettings = {}
                extent, self.calSettings['center'] = self.getExtentAndCenter()
            center = self.calSettings['center']
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            if not self.doubleZoom.isChecked():
                for i in range(len(ax.lines)-1,-1,-1):
                    ax.lines[i].remove()
                ax.plot([x, x2], [y, y2], color="g")
            else:
                if (not self.doubleZoomMode) and x < 200 and y < 200:
                    axis_size = 1
                    ax1 = self.doubleZoomAxes
                    if len(ax1.lines) > 0:
                        for i in range(len(ax1.lines)-1,-1,-1):
                            ax1.lines[i].remove()
                    ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                elif self.doubleZoomMode:
                    for i in range(len(ax.lines)-1,-1,-1):
                        ax.lines[i].remove()
                    ax.plot([x, x2], [y, y2], color="g")
            self.imageCanvas.draw_idle()

    def imageReleased(self, event):
        """
        Triggered when mouse released from image
        """
        if self.function is not None and self.function[0] == "im_move":
            self.function = None

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
        ax.invert_yaxis()
        self.imageCanvas.draw_idle()

    def RminRmaxChanged(self):
        """
        Triggered when R-min or R-max spinboxes changed
        :return:
        """
        if self.rmaxSpnBx.value() > self.rminSpnBx.value() > 0 and not self.uiUpdating:
            self.setRminRmax(self.rminSpnBx.value(), self.rmaxSpnBx.value())

    def setRminRmax(self, rmin, rmax):
        """
        Manual set R-max R-min
        :param rmin: r-min value in pixel
        :param rmax: r-max value in pixel
        :return:
        """
        self.quadFold.info['rmin'] = rmin
        self.quadFold.info['rmax'] = rmax

        self.uiUpdating = True
        self.rminSpnBx.setValue(rmin)
        self.rmaxSpnBx.setValue(rmax)
        self.uiUpdating = False

        self.deleteInfo(['bgimg1'])  # delete bgimg1 to make QuadrantFolder recalculate
        self.processImage()

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
        else:
            func = self.function
            if func[0] == "r_zoomin":
                # Set new zoom in location
                func.append((x, y))
                if len(func) == 3:
                    p1 = func[1]
                    p2 = func[2]
                    self.result_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.resultZoomInB.setChecked(False)
                    self.refreshResultTab()
            elif func[0] == "rminmax":
                # Set new R-min and R-max
                img = self.quadFold.info['avg_fold']
                center = (img.shape[1], img.shape[0])
                radius = distance((x, y), center)
                func.append(radius)
                ax = self.resultAxes
                ax.add_patch(
                    patches.Circle(center, radius, linewidth=2, edgecolor='r', facecolor='none', linestyle='solid'))
                if len(func) == 3:
                    rmin = int(round(min(func[1:])))
                    rmax = int(round(max(func[1:])))
                    self.setRminRmax(rmin, rmax)
                    self.function = None
                    self.setRminmaxButton.setChecked(False)

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
                ax.invert_yaxis()
                self.resultCanvas.draw_idle()

    def resultReleased(self, event):
        """
        Triggered when mouse released from image in result tab
        """
        if self.function is not None and self.function[0] == "r_move":
            self.function = None

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

    def setManualRminmax(self):
        """
        Prepare for R-min and R-max settings after button clicked
        """
        if self.setRminmaxButton.isChecked():
            self.imgPathOnStatusBar.setText(
                "Select R-min and R-max on the image (ESC to cancel)")
            self.function = ['rminmax'] # set active function
            ax = self.resultAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            self.resultCanvas.draw_idle()
        else:
            self.function = None
            self.setRminmaxButton.setChecked(False)
            self.refreshResultTab()
            self.resetStatusbar()

    def ignoreThresChanged(self):
        """
        Delete Average fold and reproduce it by current flags
        """
        if self.quadFold is None or self.uiUpdating:
            return
        self.deleteInfo(['avg_fold'])
        self.processImage()

    def pixRangeChanged(self):
        """
        Trigger when pixel range is changed
        """
        if self.minPixRange.value() > self.maxPixRange.value():
            # Check value
            self.minPixRange.setValue(self.maxPixRange.value())
            return
        # self.applyBGSub()

    def bgChoiceChanged(self):
        """
        Trigger when background subtraction method is changed
        Available Choices : 'None', '2D Convexhull', 'Circularly-symmetric', 'Roving Window', 'White-top-hats', 'Smoothed-Gaussian', 'Smoothed-BoxCar'
        """
        choice = self.bgChoice.currentText()

        self.rrangeSettingFrame.setHidden(choice=='None')

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
        self.cycleLabel.setHidden(not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        self.cycle.setHidden(not choice in ('Smoothed-Gaussian', 'Smoothed-BoxCar'))
        self.thetaBinLabel.setHidden(True)
        self.thetabinCB.setHidden(True)
        self.radialBinSpnBx.setHidden(not choice == 'Circularly-symmetric')
        self.radialBinLabel.setHidden(not choice == 'Circularly-symmetric')
        self.smoothLabel.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.smoothSpnBx.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.tensionLabel.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))
        self.tensionSpnBx.setHidden(not choice in ('Roving Window', 'Circularly-symmetric'))

        hide_tophat2 = (choice == 'None')

        for w in self.tophat2Widgets:
            w.setHidden(hide_tophat2)

        self.applyBGButton.setHidden(choice == 'None')

        if self.quadFold is not None and not self.uiUpdating:
            self.applyBGSub()

    def sigmoidChanged(self):
        """
        Trigger when sigmoid param (merge gradient) is changed
        """
        if self.ableToProcess():
            self.deleteImgCache(['BgSubFold'])
            self.processImage()

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

    def orientationModelChanged(self):
        """
        Triggered when the orientation model is changed
        """
        self.orientationModel = self.orientationCmbBx.currentIndex()
        if self.quadFold is None:
            return
        self.deleteInfo(['rotationAngle'])
        self.processImage()

    def doubleZoomChecked(self):
        """
        Triggered when double zoom is checked
        """
        if self.doubleZoom.isChecked():
            print("Double zoom checked")
            self.doubleZoomAxes = self.imageFigure.add_subplot(333)
            self.doubleZoomAxes.axes.xaxis.set_visible(False)
            self.doubleZoomAxes.axes.yaxis.set_visible(False)
            self.doubleZoomMode = True

            img = self.quadFold.getRotatedImage()
            ax1 = self.doubleZoomAxes
            x,y = self.quadFold.info['center']
            imgCropped = img[y - 10:y + 10, x - 10:x + 10]
            if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                self.doubleZoomPt = (x, y)
                ax1.imshow(imgScaled)
                y, x = imgScaled.shape
                # cy, cx = y // 2, x // 2
                if len(ax1.lines) > 0:
                    for i in range(len(ax1.lines)-1,-1,-1):
                        ax1.lines[i].remove()
                for i in range(len(ax1.patches)-1,-1,-1):
                    ax1.patches[i].remove()
        else:
            self.imageFigure.delaxes(self.doubleZoomAxes)
            self.doubleZoomMode = False
        self.imageCanvas.draw_idle()

    def modeAngleChecked(self):
        """
        Triggered when mode angle is checked or unchecked
        """
        print("Function executed", flush=True)

        if self.quadFold is not None:

            modeOrientation = self.getModeRotation()
            if modeOrientation is not None:
                if not self.modeAngleChkBx.isChecked():
                    self.quadFold.deleteFromDict(self.quadFold.info, 'mode_angle')
                    self.processImage()
                else:
                    self.processImage()
            else:
                self.modeAngleChkBx.setCheckState(Qt.Unchecked) # executes twice, setChecked executes once but button becomes unresponsive for one click

                msg = QMessageBox()
                msg.setInformativeText("All images in folder must be processed first, use Process Folder to process all images")
                msg.setStandardButtons(QMessageBox.Ok)
                msg.setWindowTitle("Mode Orientation Failed")
                msg.setStyleSheet("QLabel{min-width: 500px;}")
                msg.exec_()

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
        for f in self.imgList:
            quadFold = QuadrantFolder(self.filePath, f, self, self.fileList, self.ext)
            print(f'Getting angle {f}')

            if 'rotationAngle' not in quadFold.info:
                return None
            angle = quadFold.info['rotationAngle']
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
        self.deleteInfo(['center'])
        self.processImage()

    def addIgnoreQuadrant(self):
        """
        Trigger when a quadrant is ignored
        """
        fold_number = self.quadFold.getFoldNumber(self.function[1][0], self.function[1][1])
        self.function = None
        self.ignoreFolds.add(fold_number)
        self.deleteInfo(['avg_fold'])
        self.processImage()

    def removeIgnoreQuadrant(self):
        """
        Trigger when a quadrant is unignored
        """
        fold_number = self.quadFold.getFoldNumber(self.function[1][0], self.function[1][1])
        self.function = None
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
        self.spmaxInt.setRange(min_val, max_val)
        self.spminInt.setRange(min_val, max_val)
        if not self.persistIntensity.isChecked():
            self.spmaxInt.setValue(max_val * .5)
            self.spminInt.setValue(min_val)
        self.spmaxInt.setSingleStep(max_val * .05)
        self.spminInt.setSingleStep(max_val * .05)

        self.minIntLabel.setText("Min Intensity ("+str(min_val)+")")
        self.maxIntLabel.setText("Max Intensity (" + str(max_val) + ")")

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
            self.bgChoice.setCurrentIndex(self.allBGChoices.index(info['bgsub']))
            if info['bgsub'] != 'None':
                # self.bgChoice.setCurrentIndex(self.allBGChoices.index(info['bgsub']))
                self.tophat1SpnBx.setValue(info['tophat1'])
                self.tophat2SpnBx.setValue(info['tophat2'])
                self.sigmoidSpnBx.setValue(info["sigmoid"])
                self.maxPixRange.setValue(info["cirmax"])
                self.minPixRange.setValue(info["cirmin"])
                self.radialBinSpnBx.setValue(info['radial_bin'])
                self.smoothSpnBx.setValue(info['smooth'])
                self.tensionSpnBx.setValue(info['tension'])
                if previnfo is None or not self.fixedRadiusRangeChkBx.isChecked():
                    self.rminSpnBx.setValue(info['rmin'])
                    self.rmaxSpnBx.setValue(info['rmax'])
                else:
                    self.rminSpnBx.setValue(previnfo['rmin'])
                    self.rmaxSpnBx.setValue(previnfo['rmax'])
                self.winSizeX.setValue(info['win_size_x'])
                self.winSizeY.setValue(info['win_size_y'])
                self.winSepX.setValue(info['win_sep_x'])
                self.winSepY.setValue(info['win_sep_y'])
                self.gaussFWHM.setValue(info['fwhm'])
                self.boxcarX.setValue(info['boxcar_x'])
                self.boxcarY.setValue(info['boxcar_y'])
                self.cycle.setValue(info['cycles'])

        if 'blank_mask' in info:
            self.blankImageGrp.setChecked(info['blank_mask'])

        if 'mask_thres' in info.keys():
            self.maskThresSpnBx.setValue(info['mask_thres'])
        elif self.maskThresSpnBx.value() == -999:
            self.maskThresSpnBx.setValue(getMaskThreshold(img))
        self.maskThresSpnBx.setRange(min_val, max_val)

        self.spResultmaxInt.setRange(min_val + 1, max_val)
        self.spResultminInt.setRange(min_val, max_val - 1)
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
        fileName = self.imgList[self.currentFileNumber]
        self.filenameLineEdit.setText(fileName)
        self.filenameLineEdit2.setText(fileName)
        if self.quadFold is not None and 'saveCroppedImage' in self.quadFold.info and self.quadFold.info['saveCroppedImage'] != self.cropFoldedImageChkBx.isChecked():
            self.quadFold.delCache()
        self.quadFold = QuadrantFolder(self.filePath, fileName, self, self.fileList, self.ext)
        if reprocess:
            self.quadFold.info = {}
            self.quadFold.info['reprocess'] = True
        if 'saveCroppedImage' not in self.quadFold.info:
            self.quadFold.info['saveCroppedImage'] = self.cropFoldedImageChkBx.isChecked()
        self.markFixedInfo(self.quadFold.info, previnfo)
        original_image = self.quadFold.orig_img
        self.imgDetailOnStatusBar.setText(
            str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype))
        self.initialWidgets(original_image, previnfo)
        if 'ignore_folds' in self.quadFold.info:
            self.ignoreFolds = self.quadFold.info['ignore_folds']
        if 'folded' in self.quadFold.info:
            # print(self.quadFold.info['folded'])
            if self.quadFold.info['folded'] != self.toggleFoldImage.isChecked():
                self.quadFold.deleteFromDict(self.quadFold.info, 'avg_fold')
                self.quadFold.deleteFromDict(self.quadFold.imgCache, 'BgSubFold')  
        if self.persistRotations.isChecked():
            self.quadFold.info['manual_rotationAngle'] = self.rotationAngle
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
        Deleting the center for appropriate recalculation
        """

        if 'center' in currentInfo:
            del currentInfo['center']

        if self.calSettingsDialog.fixedCenter.isChecked() and prevInfo is not None:
            currentInfo['calib_center'] = prevInfo['calib_center']
            if 'manual_center' in currentInfo:
                del currentInfo['manual_center']
        else:
            if 'calib_center' in currentInfo:
                del currentInfo['calib_center']
        if not self.calSettingsDialog.manDetector.isChecked() and prevInfo is not None:
            if 'detector' in currentInfo:
                del currentInfo['detector']

    def refreshAllTabs(self):
        """
        Set all tab update status to be not update, and Refresh (Redraw) all tab
        """
        self.updated['img'] = False
        self.updated['result'] = False
        self.function = None
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
            img = self.quadFold.getRotatedImage()
            extent, center = self.getExtentAndCenter()
            self.img = img
            self.extent = extent
            # img = getBGR(get8bitImage(img, min=self.spminInt.value(), max=self.spmaxInt.value()))
            if self.logScaleIntChkBx.isChecked():
                ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
            else:
                ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
            ax.set_facecolor('black')

            self.orientationCmbBx.setCurrentIndex(0 if self.orientationModel is None else self.orientationModel)

            if self.showSeparator.isChecked():
                # Draw quadrant separator
                ax.axvline(center[0], color='y')
                ax.axhline(center[1], color='y')

            self.calSettingsDialog.centerX.setValue(center[0])
            self.calSettingsDialog.centerY.setValue(center[1])

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
            ax.invert_yaxis()
            self.imageFigure.tight_layout()
            self.imageCanvas.draw()

            self.updated['img'] = True
            self.uiUpdating = False

    def getExtentAndCenter(self):
        """
        Give the extent and the center of the image
        """
        if self.quadFold is None:
            return [0,0], (0,0)
        if self.quadFold.orig_image_center is None:
            self.quadFold.findCenter()
            self.statusPrint("Done.")
        if 'calib_center' in self.quadFold.info:
            center = self.quadFold.info['calib_center']
        elif 'manual_center' in self.quadFold.info:
            center = self.quadFold.info['manual_center']
        else:
            center = self.quadFold.orig_image_center
            
        extent = [self.quadFold.info['center'][0] - center[0], self.quadFold.info['center'][1] - center[1]]
        return extent, center

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
            self.spResultminInt.setRange(img.min(), img.max())
            self.spResultmaxInt.setRange(img.min(), img.max())
            self.rminSpnBx.setValue(self.quadFold.info['rmin'])
            self.rmaxSpnBx.setValue(self.quadFold.info['rmax'])
            self.fixedRoiChkBx.setChecked('fixed_roi_rad' in self.quadFold.info)
            self.fixedRoi.setEnabled('fixed_roi_rad' in self.quadFold.info)
            if 'fixed_roi_rad' in self.quadFold.info:
                self.fixedRoi.setValue(int(self.quadFold.info['fixed_roi_rad']))

            # convert image for displaying
            # img = getBGR(get8bitImage(img, max=self.spResultmaxInt.value(), min=self.spResultminInt.value()))
            ax = self.resultAxes
            ax.cla()
            if self.resLogScaleIntChkBx.isChecked():
                ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.spResultminInt.value()), vmax=self.spResultmaxInt.value()))
            else:
                ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.spResultminInt.value(), vmax=self.spResultmaxInt.value()))
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
            ax.invert_yaxis()
            self.resultFigure.tight_layout()
            self.resultCanvas.draw()

            self.updated['result'] = True
            self.uiUpdating = False

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

            self.saveResults()
            QApplication.restoreOverrideCursor()
            
    
    def addTask(self, i):
        # def __init__(self, flags, fileName, filePath, ext, fileList, parent):
        params = QuadFoldParams(self.getFlags(), self.imgList[i], self.filePath, self.ext, self.fileList, self)
        self.tasksQueue.put(params)

        # If there's no task currently running, start the next task
        self.startNextTask()
            
    def thread_done(self, quadFold):
        
        if self.lock is not None:
            self.lock.acquire()
            
        self.quadFold = quadFold
            
        self.onProcessingFinished()
        
        if self.lock is not None:
            self.lock.release()
    
    # placeholder method
    def thread_finished(self):
        
        self.tasksDone += 1
        self.progressBar.setValue(int(100. / self.numberOfFiles * self.tasksDone))
        
        if not self.tasksQueue.empty():
            self.startNextTask()
        else:
            if self.threadPool.activeThreadCount() == 0:
                print("All threads are complete")
                self.currentFileNumber = 0
                self.progressBar.setVisible(False)
                self.filenameLineEdit.setEnabled(True)
                self.filenameLineEdit2.setEnabled(True)
                self.csvManager.sortCSV()
            
    def startNextTask(self):
        self.progressBar.setVisible(True)
        self.filenameLineEdit.setEnabled(False)
        self.filenameLineEdit2.setEnabled(False)
        while not self.tasksQueue.empty() and self.threadPool.activeThreadCount() < self.threadPool.maxThreadCount() / 2:
            params = self.tasksQueue.get()
            self.currentTask = Worker(params)
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
                cx,cy = self.quadFold.info['center']
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
            # plt.imsave(fullPath(result_path, self.imgList[self.currentFileNumber])+".result2.tif", img)
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
            resultImg = np.rot90(resultImg)

        method = info['bgsub']
        print(method)
        if method != 'None':
            
            filename = self.imgList[self.currentFileNumber]
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

            self.csv_bg.loc[filename] = pd.Series({'Sum':total_inten})
            self.csv_bg.to_csv(csv_path)

    def updateParams(self):
        """
        Update the parameters
        """
        info = self.quadFold.info
        if 'orientation_model' in info:
            self.orientationModel = info['orientation_model']
        if self.calSettings is not None and 'center' in self.calSettings and 'calib_center' in info:
            # Update cal settings center with the corresponding coordinate in original (or initial) image
            # so that it persists correctly on moving to next image
            self.calSettings['center'] = info['calib_center']
        if not self.zoomOutClicked:
            _, center = self.getExtentAndCenter()
            cx, cy = center
            cxr, cyr = self.quadFold.info['center']
            xlim, ylim = self.quadFold.initImg.shape
            xlim, ylim = int(xlim/2), int(ylim/2)
            self.default_img_zoom = [(cx-xlim, cx+xlim), (cy-ylim, cy+ylim)]
            self.default_result_img_zoom = [(cxr-xlim, cxr+xlim), (cyr-ylim, cyr+ylim)]

    def resetStatusbar(self):
        """
        Reset the status bar
        """
        fileFullPath = fullPath(self.filePath, self.imgList[self.currentFileNumber])
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(self.currentFileNumber + 1) + '/' + str(self.numberOfFiles) + ') : ' + fileFullPath)
        
    def resetStatusbar2(self):
        """
        Reset the status bar, but search using self.quadFold.info
        """
        index = self.imgList.index(self.quadFold.img_name)
        fileFullPath = fullPath(self.filePath, self.imgList[index])
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(index + 1) + '/' + str(self.numberOfFiles) + ') : ' + fileFullPath)
        self.filenameLineEdit.setText(self.quadFold.img_name)
        self.filenameLineEdit2.setText(self.quadFold.img_name)

    def getFlags(self):
        """
        Get all flags for QuadrantFolder process() from widgets
        :return: flags (dict)
        """
        flags = {}

        flags['orientation_model'] = self.orientationModel
        flags["ignore_folds"] = self.ignoreFolds
        flags['bgsub'] = self.bgChoice.currentText()
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
        flags["tophat2"] = self.tophat2SpnBx.value()
        flags['mask_thres'] = self.maskThresSpnBx.value()
        flags['sigmoid'] = self.sigmoidSpnBx.value()
        flags['fwhm'] = self.gaussFWHM.value()
        flags['boxcar_x'] = self.boxcarX.value()
        flags['boxcar_y'] = self.boxcarY.value()
        flags['cycles'] = self.cycle.value()
        flags['blank_mask'] = self.blankImageGrp.isChecked()
        flags['fold_image'] = self.toggleFoldImage.isChecked()

        if self.modeAngleChkBx.isChecked():
            modeOrientation = self.getModeRotation()
            if modeOrientation is not None:
                flags["mode_angle"] = modeOrientation

        if self.rmaxSpnBx.value() > self.rminSpnBx.value() > 0:
            flags['fixed_rmin'] = self.rminSpnBx.value()
            flags['fixed_rmax'] = self.rmaxSpnBx.value()

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
        self.filePath, self.imgList, self.currentFileNumber, self.fileList, self.ext = getImgFiles(str(newFile))
        if self.filePath is not None and self.imgList is not None and self.imgList:
            try:
                self.csvManager = QF_CSVManager(self.filePath)
            except Exception:
                msg = QMessageBox()
                msg.setInformativeText(
                    "Permission denied when creating a folder at " + self.filePath + ". Please check the folder permissions.")
                msg.setStandardButtons(QMessageBox.Ok)
                msg.setWindowTitle("Error Creating CSVManager")
                msg.setStyleSheet("QLabel{min-width: 500px;}")
                msg.exec_()
            if self.csvManager is not None:
                self.numberOfFiles = len(self.imgList)
                self.ignoreFolds = set()
                self.selectImageButton.setHidden(True)
                self.selectFolder.setHidden(True)
                self.imageCanvas.setHidden(False)
                self.resetWidgets()
                QApplication.restoreOverrideCursor()
                if self.h5List == []:
                    fileName = self.imgList[self.currentFileNumber]
                    self.quadFold = QuadrantFolder(self.filePath, fileName, self, self.fileList, self.ext)
                    self.setCalibrationImage()
                self.h5List = []
                self.setH5Mode(str(newFile))
                self.onImageChanged()
            else:
                QApplication.restoreOverrideCursor()
                self.browseFile()
        else:
            QApplication.restoreOverrideCursor()
            self.browseFile()

    def setH5Mode(self, file_name):
        """
        Sets the H5 list of file and displays the right set of buttons depending on the file selected
        """
        if self.ext in ['.h5', '.hdf5']:
            for file in os.listdir(self.filePath):
                if file.endswith(".h5") or file.endswith(".hdf5"):
                    self.h5List.append(file)
            self.h5index = self.h5List.index(os.path.split(file_name)[1])
            self.nextFileButton.show()
            self.prevFileButton.show()
            self.nextFileButton2.show()
            self.prevFileButton2.show()
            self.processH5FolderButton.show()
            self.processH5FolderButton2.show()
            self.processFolderButton.setText("Process Current H5 File")
            self.processFolderButton2.setText("Process Current H5 File")
        else:
            self.nextFileButton.hide()
            self.prevFileButton.hide()
            self.nextFileButton2.hide()
            self.prevFileButton2.hide()
            self.processH5FolderButton.hide()
            self.processH5FolderButton2.hide()
            self.processFolderButton.setText("Process Current Folder")
            self.processFolderButton2.setText("Process Current Folder")

    def resetWidgets(self):
        """
        Reset the widgets
        """
        self.uiUpdating = True
        self.rminSpnBx.setValue(-1)
        self.rmaxSpnBx.setValue(-1)
        self.uiUpdating = False

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
            self.ignoreFolds = set()
            self.currentFileNumber = 0
            self.onImageChanged()
            self.processFolder()

    def batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.processFolderButton.isChecked():
            if not self.progressBar.isVisible():
                self.processFolderButton.setText("Stop")
                self.processFolder()
        elif self.processFolderButton2.isChecked():
            if not self.progressBar.isVisible():
                self.processFolderButton2.setText("Stop")
                self.processFolder()
        else:
            self.stop_process = True
    
    def h5batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.processH5FolderButton.isChecked():
            if not self.progressBar.isVisible():
                self.processH5FolderButton.setText("Stop")
                self.processH5Folder()
        elif self.processH5FolderButton2.isChecked():
            if not self.progressBar.isVisible():
                self.processH5FolderButton2.setText("Stop")
                self.processH5Folder()
        else:
            self.stop_process = True

    def processFolder(self):
        """
        Triggered when a folder has been selected to process it
        """
        # fileList = os.listdir(self.filePath)
        # self.imgList = []
        # for f in fileList:
        #     if isImg(fullPath(self.filePath, f)):
        #         self.imgList.append(f)

        # self.imgList.sort()
        # self.numberOfFiles = len(self.imgList)

        errMsg = QMessageBox()
        errMsg.setText('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'

        flags = self.getFlags()
        text += "\nCurrent Settings"
        if 'center' in flags:
            text += "\n  - Center : " + str(flags["center"])
        if len(self.ignoreFolds) > 0:
            text += "\n  - Ignore Folds : " + str(list(self.ignoreFolds))
        text += "\n  - Orientation Finding : " + str(self.orientationCmbBx.currentText())
        text += "\n  - Mask Threshold : " + str(flags["mask_thres"])
        text += "\n  - Background Subtraction Method : "+ str(self.bgChoice.currentText())

        if flags['bgsub'] != 'None':
            if 'fixed_rmin' in flags:
                text += "\n  - R-min : " + str(flags["fixed_rmin"])
                text += "\n  - R-max : " + str(flags["fixed_rmax"])

            if flags['bgsub'] in ['Circularly-symmetric', 'Roving Window']:
                text += "\n  - Pixel Range (Percentage) : " + str(flags["cirmin"]) + "% - "+str(flags["cirmax"])+"%"

            if flags['bgsub'] == 'Circularly-symmetric':
                text += "\n  - Radial Bin : " + str(flags["radial_bin"])
                text += "\n  - Smooth : " + str(flags["smooth"])
            elif flags['bgsub'] == 'White-top-hats':
                text += "\n  - Tophat (inside R-max) : " + str(flags["tophat1"])
            elif flags['bgsub'] == 'Smoothed-Gaussian':
                text += "\n  - FWHM : " + str(flags["fwhm"])
                text += "\n  - Number of cycle : " + str(flags["cycles"])
            elif flags['bgsub'] == 'Smoothed-BoxCar':
                text += "\n  - Box car width : " + str(flags["boxcar_x"])
                text += "\n  - Box car height : " + str(flags["boxcar_y"])
                text += "\n  - Number of cycle : " + str(flags["cycles"])

            text += "\n  - Tophat (outside R-max) : " + str(flags["tophat2"])
            text += "\n  - Merge Gradient : " + str(flags["sigmoid"])

        text += '\n\nAre you sure you want to process ' + str(self.numberOfFiles) + ' image(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            # self.progressBar.setVisible(True)
            # self.progressBar.setValue(0)
            self.stop_process = False
            self.totalFiles = self.numberOfFiles
            self.tasksDone = 0
            for i in range(self.numberOfFiles):
                if self.stop_process:
                    break
                # self.progressBar.setValue(int(100. / self.numberOfFiles * i))
                # QApplication.processEvents()
                # self.nextClicked(reprocess=True)
                self.addTask(i)
                
            # self.progressBar.setVisible(False)

        self.processFolderButton.setChecked(False)
        self.processFolderButton2.setChecked(False)
        if self.ext in ['.h5', '.hdf5']:
            self.processFolderButton.setText("Process Current H5 File")
            self.processFolderButton2.setText("Process Current H5 File")
        else:
            self.processFolderButton.setText("Process Current Folder")
            self.processFolderButton2.setText("Process Current Folder")

    def processH5Folder(self):
        """
        Triggered when a folder with multiple H5 files has been selected to process it
        """
        errMsg = QMessageBox()
        errMsg.setText('Process Current H5 Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'

        flags = self.getFlags()
        text += "\nCurrent Settings"
        if 'center' in flags:
            text += "\n  - Center : " + str(flags["center"])
        if len(self.ignoreFolds) > 0:
            text += "\n  - Ignore Folds : " + str(list(self.ignoreFolds))
        text += "\n  - Orientation Finding : " + str(self.orientationCmbBx.currentText())
        text += "\n  - Mask Threshold : " + str(flags["mask_thres"])
        text += "\n  - Background Subtraction Method : "+ str(self.bgChoice.currentText())

        if flags['bgsub'] != 'None':
            if 'fixed_rmin' in flags:
                text += "\n  - R-min : " + str(flags["fixed_rmin"])
                text += "\n  - R-max : " + str(flags["fixed_rmax"])

            if flags['bgsub'] in ['Circularly-symmetric', 'Roving Window']:
                text += "\n  - Pixel Range (Percentage) : " + str(flags["cirmin"]) + "% - "+str(flags["cirmax"])+"%"

            if flags['bgsub'] == 'Circularly-symmetric':
                text += "\n  - Radial Bin : " + str(flags["radial_bin"])
                text += "\n  - Smooth : " + str(flags["smooth"])
            elif flags['bgsub'] == 'White-top-hats':
                text += "\n  - Tophat (inside R-max) : " + str(flags["tophat1"])
            elif flags['bgsub'] == 'Smoothed-Gaussian':
                text += "\n  - FWHM : " + str(flags["fwhm"])
                text += "\n  - Number of cycle : " + str(flags["cycles"])
            elif flags['bgsub'] == 'Smoothed-BoxCar':
                text += "\n  - Box car width : " + str(flags["boxcar_x"])
                text += "\n  - Box car height : " + str(flags["boxcar_y"])
                text += "\n  - Number of cycle : " + str(flags["cycles"])

            text += "\n  - Tophat (outside R-max) : " + str(flags["tophat2"])
            text += "\n  - Merge Gradient : " + str(flags["sigmoid"])

        text += '\n\nAre you sure you want to process ' + str(len(self.h5List)) + ' H5 file(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            self.progressBar.setValue(0)
            self.progressBar.setVisible(True)
            self.stop_process = False
            self.totalFiles = len(self.h5List) * self.numberOfFiles
            for _ in range(len(self.h5List)):
                for i in range(self.numberOfFiles):
                    if self.stop_process:
                        break
                    self.progressBar.setValue(int(100. / self.numberOfFiles * i ))
                    QApplication.processEvents()
                    self.nextClicked(reprocess=True)
                if self.stop_process:
                    break
                self.nextFileClicked()
            self.progressBar.setVisible(False)

        self.processH5FolderButton.setChecked(False)
        self.processH5FolderButton2.setChecked(False)
        self.processH5FolderButton.setText("Process All H5 Files")
        self.processH5FolderButton2.setText("Process All H5 Files")

    def browseFile(self):
        """
        Popup input dialog and set file selection
        """
        self.newProcess = True
        file_name = getAFile()
        if file_name != "":
            self.onNewFileSelected(str(file_name))
            self.centralWidget.setMinimumSize(700, 500)

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

    def prevClicked(self):
        """
        Going to the previous image
        """
        if self.numberOfFiles > 0:
            self.currentFileNumber = (self.currentFileNumber - 1) % self.numberOfFiles
            self.onImageChanged()

    def nextClicked(self, reprocess=False):
        """
        Going to the next image
        """
        if self.numberOfFiles > 0:
            self.currentFileNumber = (self.currentFileNumber + 1) % self.numberOfFiles
            self.onImageChanged(reprocess=reprocess)

    def prevFileClicked(self):
        """
        Going to the previous h5 file
        """
        if len(self.h5List) > 1:
            self.h5index = (self.h5index - 1) % len(self.h5List)
            self.onNewFileSelected(os.path.join(self.filePath, self.h5List[self.h5index]))

    def nextFileClicked(self):
        """
        Going to the next h5 file
        """
        if len(self.h5List) > 1:
            self.h5index = (self.h5index + 1) % len(self.h5List)
            self.onNewFileSelected(os.path.join(self.filePath, self.h5List[self.h5index]))

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
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            fileName = self.filenameLineEdit.text().strip()
        elif selected_tab == 1:
            fileName = self.filenameLineEdit2.text().strip()
        if fileName not in self.imgList:
            return
        self.currentFileNumber = self.imgList.index(fileName)
        self.onImageChanged()

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
        
