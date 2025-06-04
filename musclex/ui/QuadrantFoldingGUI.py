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
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize, ListedColormap
import matplotlib.pyplot as plt
import pandas as pd
from PIL import Image
from musclex import __version__
from PySide6.QtCore import QRunnable, QThreadPool, QEventLoop, Signal
from queue import Queue
import fabio
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.QuadrantFolder import QuadrantFolder
from ..csv_manager.QF_CSVManager import QF_CSVManager
from .pyqt_utils import *
from .BlankImageSettings import BlankImageSettings
from .ImageMaskTool import ImageMaskerWindow
from .DoubleZoomGUI import DoubleZoom
from ..CalibrationSettings import CalibrationSettings
from threading import Lock
from scipy.ndimage import rotate

import time
import random

class QuadFoldParams:
    def __init__(self, flags, fileName, filePath, ext, fileList, parent):
        self.flags = flags
        self.fileName = fileName
        self.filePath = filePath
        self.ext = ext
        self.fileList = fileList
        self.parent = parent

class WorkerSignals(QObject):
    
    finished = Signal()
    error = Signal(tuple)
    result = Signal(object)


class Worker(QRunnable):

    def __init__(self, params, fixed_center_checked, 
                 persist_center, persist_rot, bgsub = 'Circularly-symmetric',
                 bgDict = None, bg_lock=None):
        
        super().__init__()
        self.flags = params.flags
        self.params = params
        self.signals = WorkerSignals()
        self.lock = Lock()

        #NA
        self.fixedCenterChecked = fixed_center_checked
        self.persist_center = persist_center

        #NA
        self.persist_rot = persist_rot

        self.bgsub = bgsub

        self.bgDict = bgDict

        #NA
        #self.qf_lock = qf_lock
        self.qf_lock = Lock()
        
    @Slot()
    def run(self):
        try:
            self.quadFold = QuadrantFolder(self.params.filePath, self.params.fileName, 
                                           self.params.parent, self.params.fileList, 
                                           self.params.ext)
            self.quadFold.info = {}
            self.quadFold.info['bgsub'] = self.bgsub
            print("WORKER RUN: info-bgsub=",self.bgsub) #NICKA DEBUG

            #NICK ALLISON
            #pass persisted center data to quadfold object
            if self.fixedCenterChecked:
                self.quadFold.fixedCenterX = self.persist_center[0]
                self.quadFold.fixedCenterY = self.persist_center[1]

            #Pass the persisted rotation to the quadfold object
            if self.persist_rot is not None:
                self.quadFold.fixedRot = self.persist_rot


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


        print("Avg_fold total: ", np.sum(avg_fold)) #NICKA DEBUG
        print("Result Shape: ", np.sum(result)) 


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
                print("READING CSV_BG from CSV") #NICKA DEBUG
                self.csv_bg = pd.DataFrame(columns=['Name', 'Sum'])
                self.csv_bg.loc[filename] = pd.Series({'Name': filename, 'Sum': total_inten})
                self.csv_bg.to_csv(csv_path, mode='a', header=not os.path.exists(csv_path), index=False)
                print("CSV DF: ", self.csv_bg) #NICKA DEBUG
            else:
                self.csv_bg = pd.DataFrame(columns=['Name', 'Sum'])
                self.csv_bg.loc[filename] = pd.Series({'Name': filename, 'Sum': total_inten})
                self.csv_bg.to_csv(csv_path, mode='a')
                print("ELSE CASE") #NICKA DEBUG
                print("SELF>CSV_GB IN ELSE: ", self.csv_bg)

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
        self.qf_lock = Lock()
        self.imageMaskingTool = None
        
        self.rotationAngle = None

        self.calSettingsDialog = None

        #NA
        #Used for when the same center/rotation needs to be used to process a folder
        self.persistedCenter = None
        self.persistedRotation = None

        self.thresh_mask = None

        self.initUI() # initial all GUI

        self.doubleZoomGUI = DoubleZoom(self.imageFigure)

        self.setConnections() # set triggered function for widgets
        # self.setMinimumHeight(900)
        self.resize(1200, 900)
        self.newImgDimension = None
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
        self.leftWidget.setMinimumWidth(650)

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
        self.imageTabLayout.addWidget(self.leftWidget)
        self.imageTabLayout.addWidget(self.imageCanvas)
        #self.imageTabLayout.addStretch()


        self.rightImageFrame = QFrame()
        self.rightImageLayout = QVBoxLayout(self.rightImageFrame)
        
        #self.rightImageFrame.setFixedWidth(500)
        #self.rightImageFrame.setLayout(self.rightImageLayout)

        self.displayOptGrpBx = QGroupBox("Display Options")
        self.dispOptLayout = QGridLayout(self.displayOptGrpBx)

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
        #self.persistIntensity.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)

        self.showSeparator = QCheckBox()
        self.showSeparator.setText("Show Quadrant Separator")
        self.showSeparator.setChecked(True)

        self.imgZoomInB = QPushButton("Zoom in")
        self.imgZoomInB.setCheckable(True)
        self.imgZoomOutB = QPushButton("Full")
        self.checkableButtons.append(self.imgZoomInB)

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')

        self.doubleZoom = QCheckBox("Double Zoom")
        self.cropFoldedImageChkBx = QCheckBox("Save Cropped Image (Original Size)")
        self.cropFoldedImageChkBx.setChecked(False)

        self.dispOptLayout.addWidget(self.showSeparator, 0, 0, 1, 4)
        self.dispOptLayout.addWidget(self.minIntLabel, 1, 0, 1, 2)
        self.dispOptLayout.addWidget(self.spminInt, 2, 0, 1, 2)
        self.dispOptLayout.addWidget(self.maxIntLabel, 1, 2, 1, 2)
        self.dispOptLayout.addWidget(self.spmaxInt, 2, 2, 1, 2)
        self.dispOptLayout.addWidget(self.logScaleIntChkBx, 3, 0, 1, 2)
        self.dispOptLayout.addWidget(self.persistIntensity, 3, 2, 1, 2)
        self.dispOptLayout.addWidget(self.imgZoomInB, 4, 0, 1, 2)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 4, 2, 1, 2)
        self.dispOptLayout.addWidget(self.doubleZoom, 5, 0, 1, 2)
        self.dispOptLayout.addWidget(self.cropFoldedImageChkBx, 5, 2, 1, 2)

        self.rightImageLayout.addWidget(self.displayOptGrpBx)
        self.rightImageLayout.addSpacing(10)

        #self.displayOptGrpBx.setFixedHeight(160)

        #self.displayOptGrpBx.setLayout(self.dispOptLayout)
        #self.rightImageLayout.addStretch(1)

        self.optionsLayout = QVBoxLayout()
        # self.optionsLayout.setAlignment(Qt.AlignCenter)
        self.settingsGroup = QGroupBox("Image Processing")
        self.settingsLayout = QGridLayout(self.settingsGroup)
        #self.settingsLayout.setScaledContents(False)
        #self.settingsLayout.setWidgetResizable(False)
        #self.settingsGroup.setLayout(self.settingsLayout)
        #self.settingsGroup.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        #self.settingsGroup.setFixedHeight(300)
        #self.settingsGroup.setMinimumSize(400, 200)

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

        self.modeAngleChkBx = QCheckBox("Mode Orientation")
        self.modeAngleChkBx.setChecked(False)

        # self.expandImage = QCheckBox("Expand the Image")
        # self.expandImage.setChecked(False)
        # self.expandImage.setToolTip("Expand the size of the image, for images with an offset center")

        self.compressFoldedImageChkBx = QCheckBox("Save Compressed Image")
        self.compressFoldedImageChkBx.setChecked(True)
        self.compressFoldedImageChkBx.setToolTip("Saves the images as compressed tifs (might not be compatible with fit2d, but works with imagej)")

        #self.doubleZoom.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Fixed)

        #self.dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")

        self.toggleFoldImage = QCheckBox("Fold Image")
        self.toggleFoldImage.setChecked(True)

        self.fixedOrientationChkBx = QCheckBox("Persistent Orientation")
        self.fixedOrientationChkBx.setChecked(False)

        self.settingsLayout.addWidget(self.calibrationButton, 0, 0, 1, 4)
        self.settingsLayout.addWidget(self.setCentByChords, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.setCentByPerp, 1, 2, 1, 2)
        self.settingsLayout.addWidget(self.setCenterRotationButton, 2, 0, 1, 2)
        self.settingsLayout.addWidget(self.setRotationButton, 2, 2, 1, 2)
        self.settingsLayout.addWidget(self.persistRotations, 3, 0, 1, 4)
        #self.settingsLayout.addWidget(QLabel("Lower Bound : "), 4, 0, 1, 2)
        #self.settingsLayout.addWidget(self.minThreshField, 4, 2, 1, 2)
        #self.settingsLayout.addWidget(QLabel("Upper Bound : "), 5, 0, 1, 2)
        #self.settingsLayout.addWidget(self.maxThreshField, 5, 2, 1, 2)
        self.settingsLayout.addWidget(QLabel("Mask Threshold : "), 6, 0, 1, 2)
        self.settingsLayout.addWidget(self.maskThresSpnBx, 6, 2, 1, 2)
        self.settingsLayout.addWidget(QLabel("Orientation Finding: "), 7, 0, 1, 2)
        self.settingsLayout.addWidget(self.orientationCmbBx, 7, 2, 1, 2)
        self.settingsLayout.addWidget(self.modeAngleChkBx, 8, 0, 1, 4)
        self.settingsLayout.addWidget(self.fixedOrientationChkBx, 8, 2, 1, 4)
        

        self.settingsLayout.addWidget(self.toggleFoldImage, 14, 0, 1, 4)
        self.settingsLayout.addWidget(self.compressFoldedImageChkBx, 14, 2, 1, 4)

        # Blank Image Settings
        self.blankImageGrp = QGroupBox("Enable Blank Image and Mask")
        self.blankImageGrp.setCheckable(True)
        self.blankImageGrp.setChecked(False)
        self.blankImageLayout = QGridLayout(self.blankImageGrp)
        self.blankSettingButton = QPushButton("Set Blank Image and Mask")
        self.blankImageLayout.addWidget(self.blankSettingButton, 1, 0, 1, 4)

        self.rightImageLayout.addWidget(self.blankImageGrp)
        self.rightImageLayout.addWidget(self.settingsGroup)

        #self.blankImageGrp.setFixedHeight(200)

        self.rightImageLayout.addStretch()


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
        #self.imageTabLayout.addWidget(self.frameOfKeys)

        self.scroll_areaImg = QScrollArea()
        self.scroll_areaImg.setWidgetResizable(True)
        self.scroll_areaImg.setWidget(self.frameOfKeys)

        self.scroll_areaImg.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        self.imageTabLayout.addWidget(self.scroll_areaImg)

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

        self.res_scroll_areaImg = QScrollArea()
        self.res_scroll_areaImg.setWidgetResizable(True)
        self.res_scroll_areaImg.setWidget(self.rightFrame)

        self.scroll_areaImg.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        self.resultTabLayout.addWidget(self.res_scroll_areaImg)

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
        self.fixedOrientationChkBx.stateChanged.connect(self.onFixedRotationChkBxToggled)
        self.cropFoldedImageChkBx.stateChanged.connect(self.cropFoldedImageChanged)
        self.compressFoldedImageChkBx.stateChanged.connect(self.compressFoldedImageChanged)

        self.showRminChkBx.stateChanged.connect(self.toggleCircleRmin)
        self.rminSpnBx.valueChanged.connect(self.toggleCircleRmin)

        self.showTranRadDeltaChkBx.stateChanged.connect(self.toggleCircleTransition)
        self.tranRSpnBx.valueChanged.connect(self.toggleCircleTransition)
        self.tranDeltaSpnBx.valueChanged.connect(self.toggleCircleTransition)

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
        self.bgChoiceIn.currentIndexChanged.connect(self.bgChoiceInChanged)
        self.bgChoiceOut.currentIndexChanged.connect(self.bgChoiceOutChanged)
        self.minPixRange.valueChanged.connect(self.pixRangeChanged)
        self.maxPixRange.valueChanged.connect(self.pixRangeChanged)

        self.setRminButton.clicked.connect(self.setManualRmin)
        self.rminSpnBx.valueChanged.connect(self.RminChanged)

        self.tranRSpnBx.valueChanged.connect(self.TranRChanged)
        self.tranDeltaSpnBx.valueChanged.connect(self.TranDeltaChanged)

        self.applyBGButton.clicked.connect(self.applyBGSub)

        self.blankImageGrp.clicked.connect(self.blankChecked)


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




        

    def updateLeftWidgetWidth(self):
        if self.imageCanvas.isVisible():
            # Remove the minimum width constraint
            self.leftWidget.setMinimumWidth(0)
        else:
            # Set the minimum width for when the canvas is hidden
            self.leftWidget.setMinimumWidth(650)


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
            center = self.quadFold.info['center']

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
            center = self.quadFold.info['center']

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
            fileName = self.imgList[self.currentFileNumber]
            fix_x, fix_y = self.quadFold.fixedCenterX, self.quadFold.fixedCenterY
            self.quadFold = QuadrantFolder(self.filePath, fileName, self, self.fileList, self.ext)

            if fix_x is not None and fix_y is not None:
                self.quadFold.fixedCenterX = fix_x
                self.quadFold.fixedCenterY = fix_y
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
        # dlg = BlankImageSettings(self.filePath)
        # result = dlg.exec_()
        # if result == 1 and self.quadFold is not None:
        #     self.quadFold.delCache()
        #     fileName = self.imgList[self.currentFileNumber]
        #     self.quadFold = QuadrantFolder(self.filePath, fileName, self, self.fileList, self.ext)
        #     self.masked = False
        #     self.processImage()
        
        try:
            fabio.tifimage.tifimage(data=self.img).write(join(self.filePath,'settings/tempMaskFile.tif'))
        except:
            print("ERROR WITH SAVING THE IMAGE")

        rot_ang = None if 'rotationAngle' not in self.quadFold.info else self.quadFold.info['rotationAngle']

        isH5 = False
        if self.h5List:
            fileName = self.h5List[self.h5index]
            isH5 = True
        else:
            fileName = self.imgList[self.currentFileNumber]
            
        max_val = np.max(np.ravel(self.img))
        trans_mat = self.quadFold.centImgTransMat if self.quadFold.centImgTransMat is not None else None  
        orig_size = self.quadFold.origSize if self.quadFold.origSize is not None else None

        self.imageMaskingTool = ImageMaskerWindow(self.filePath, 
                                                  join(self.filePath, "settings/tempMaskFile.tif"), 
                                                  self.spminInt.value(), 
                                                  self.spmaxInt.value(), 
                                                  max_val, 
                                                  orig_size,
                                                  trans_mat,                                                    
                                                  rot_ang, 
                                                  isH5)
            
        if self.imageMaskingTool is not None and self.imageMaskingTool.exec_():
            if os.path.exists(join(join(self.filePath, 'settings'), 'blank_image_settings.json')):
                with open(join(join(self.filePath, 'settings'), 'blank_image_settings.json'), 'r') as f:
                    info = json.load(f)
                    if 'path' in info:
                        img = fabio.open(info['path']).data
                        fabio.tifimage.tifimage(data=img).write(join(join(self.filePath, 'settings'),'blank.tif'))    
            else:
                if os.path.exists(join(join(self.filePath, 'settings'), 'mask.tif')):
                    os.rename(join(join(self.filePath, 'settings'), 'mask.tif'), join(join(self.filePath, 'settings'), 'maskonly.tif'))
                    
            self.quadFold.delCache()
            fileName = self.imgList[self.currentFileNumber]
            self.quadFold = QuadrantFolder(self.filePath, fileName, self, self.fileList, self.ext)
            self.masked = False
            self.processImage()
                
    def getOrigCoordsCenter(self, x, y):
        """
        Calculate the center in original image coordinates
        """
        _, center = self.getExtentAndCenter()
        center = self.quadFold.info['center']
        #rotation angle in radians
        #print("CALCULATING ORIGINAL IMAGE COORDINATES") #Debug
        #print("Display coords: ", (x, y)) #Debug
        #print("Center: ", center) #Debug
        angle = -self.quadFold.info['rotationAngle'] * math.pi / 180
        #print("Angle: ", angle) #Debug
        cos_a = math.cos(angle)
        sin_a = math.sin(angle)
        #print("Cos: ", cos_a) #Debug
        #print("Sin: ", sin_a) #Debug
        #mouse pos in center-as-origin points
        #Get the scale factor
        s = 1 if 'scale' not in self.quadFold.info else self.quadFold.info['scale']
        #print("Scale: ", s)
        dx =  (x - center[0]) #coordinate of x relative to center
        dy = (y - center[1]) #coordinate of y relative to center
        #print("dx, dy: ", (dx, dy)) #Debug
        #apply rotation
        x1 =  dx * cos_a + dy * sin_a
        y1 = -dx * sin_a + dy * cos_a
        #print("Rotated coords: ", (x1, y1)) #Debug

        #Apply the inverse translation to the point
        """new_tx, new_ty = self.quadFold.new_tx, self.quadFold.new_ty
        x2 = x1 - new_tx
        y2 = y1 - new_ty
        print("Translated coords in image: ", (x2, y2)) #Debug

        #Apply scaling
        x3 = x2 / s
        y3 = y2 / s
        print("Scaled coords in image: ", (x3, y3)) #Debug

        #Move the origin back to the bottom left
        co_x, co_y = self.quadFold.old_center
        print("Old center: ", (co_x, co_y)) #Debug
        x4, y4 = x3 + co_x, y3 + co_y
        print("Final original coords: ", (x4, y4)) #Debug

        return x4, y4"""

                # … your existing rotation code, unchanged …
        x1 =  dx * cos_a + dy * sin_a
        y1 = -dx * sin_a + dy * cos_a
        #print("Rotated coords: ", (x1, y1))

        # 1) bring x1,y1 back into absolute cent_img coordinates
        center_x, center_y = center
        x1 += center_x
        y1 += center_y
        #print("After re-adding center:", (x1, y1))

        # 2) undo the same tx,ty you applied in transformImage
        new_tx, new_ty = self.quadFold.new_tx, self.quadFold.new_ty
        x2 = x1 - new_tx
        y2 = y1 - new_ty
        #print("After inverting translation:", (x2, y2))

        # 3) undo the scale
        s = self.quadFold.info.get('scale', 1.0)
        x3 = x2 / s
        y3 = y2 / s
        #print("After inverting scale:", (x3, y3))

        # That IS your original-image coords; no further shifting needed
        return x3, y3


    #IS THIS USED?
    def getNewCoordsCenter(self, x, y):
        """
        Calculate the center in new image coordinates
        given original image coordinates
        """
        _, center = self.getExtentAndCenter()
        #rotation angle in radians
        #print("CALCULATING ORIGINAL IMAGE COORDINATES") #Debug
        #print("Display coords: ", (x, y)) #Debug
        #print("Center: ", center) #Debug
        angle = -self.quadFold.info['rotationAngle'] * math.pi / 180
        #print("Angle: ", angle) #Debug
        cos_a = math.cos(angle)
        sin_a = math.sin(angle)
        #print("Cos: ", cos_a) #Debug
        #print("Sin: ", sin_a) #Debug
        #mouse pos in center-as-origin points
        dx =  x - center[0]
        dy = y - center[1]
        #print("dx, dy: ", (dx, dy)) #Debug
        #apply rotation
        x1 =  dx * cos_a + dy * sin_a
        y1 = -dx * sin_a + dy * cos_a
        #print("Rotated coords: ", (x1, y1)) #Debug
        #Move the origin back to the bottom left
        x_ir = x1 + center[0]
        y_ir = y1 + center[1]
        #print("Rotated coords in image: ", (x_ir, y_ir)) #Debug
        #Apply inverse translation to the point
        #print("translation: ", (self.quadFold.dl, self.quadFold.db)) #Debug
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
                    cx_o, cy_o = self.getOrigCoordsCenter(cx, cy)
                    print("Intersection in original coords ", (cx_o, cy_o))
                    intersections.append((cx_o, cy_o))
            if len(intersections) != 0:
                cx = int(sum([intersections[i][0] for i in range(0, len(intersections))]) / len(intersections))
                cy = int(sum([intersections[i][1] for i in range(0, len(intersections))]) / len(intersections))

            else:
                print("Can't Calculate Center Yet; no intersections found")
                return

            print("Center calc ", (cx, cy))

            extent, _ = self.getExtentAndCenter()
            extent = [0,0] #Remove the extent because it moves the center out of place.

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
                    cx_o, cy_o = self.getOrigCoordsCenter(xcent, ycent)
                    print("Center in original coords ", (cx_o, cy_o))

                    centers.append([cx_o, cy_o])

            extent, center = self.getExtentAndCenter()
            extent = [0, 0]  # Remove the extent because it moves the center out of place.

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
            #_, center = self.getExtentAndCenter()
            center = self.quadFold.info['center']
            if self.quadFold.fixedCenterX is None and self.quadFold.fixedCenterY is None:
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

        print("info dict before calsettings opened: ") #NICKA DEBUG
        print(self.quadFold.info) #NICKA DEBUG

        success = self.setCalibrationImage(force=True)
        """
        if success:

            self.quadFold.fixedCenterX = None
            self.quadFold.fixedCenterY = None

            print("Self.calsettings dialoge center: ", self.calSettingsDialog.centerX.value(), ",", self.calSettingsDialog.centerY.value()) #NICKA DEBUG
            if 'center' in self.quadFold.info:
                print("Quadfold info center: ", self.quadFold.info['center']) #NICKA DEBUG
            else:
                print("Center not in qf info") #NICKA DEBUG

            self.deleteInfo(['rotationAngle'])
            self.deleteImgCache(['BgSubFold'])
            self.processImage()

        print("info dict AFTER calsettings object: ")
        print(self.quadFold.info)
        """
        
        if success:
            print("Recalculate") #NICKA DEBUG
            self.deleteInfo(['rotationAngle'])
            self.deleteImgCache(['BgSubFold'])
            self.quadFold.info['manual_center'] = [self.calSettingsDialog.centerX.value(), self.calSettingsDialog.centerY.value()]
            self.processImage()


    def setCalibrationImage(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or force to open
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        print("Set Calibration Image") #NICKA DEBUG
        if self.calSettingsDialog is None:
            if self.quadFold is None or self.quadFold.orig_image_center is None:
                print("Cal Setting Constructor call with no center") #NICKA DEBUG 
                self.calSettingsDialog = CalibrationSettings(self.filePath)
            else:
                print("Cal Setting Constructor call with center = ", self.quadFold.orig_image_center) #NICKA DEBUG
                self.calSettingsDialog =  CalibrationSettings(self.filePath, center=self.quadFold.orig_image_center)
        self.calSettings = None

        self.calSettingsDialog.recalculate = False

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
        elif self.doubleZoomGUI.doubleZoomMode:
            self.doubleZoomGUI.mouseClickBehavior(x, y)
            return

        if self.function is not None and self.function[0] == 'ignorefold':
            self.function = None

        if self.doubleZoom.isChecked() and not self.doubleZoomGUI.doubleZoomMode:
            x, y = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
            self.doubleZoomGUI.doubleZoomMode = True

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
                x_o, y_o = self.getOrigCoordsCenter(x, y)
                func.append((x_o, y_o))
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
                    extent = [0, 0]  # Remove the extent because it moves the center out of place.

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
                center = self.quadFold.info['center']

                x_o, y_o = self.getOrigCoordsCenter(x, y)
                cx_o, cy_o = self.getOrigCoordsCenter(center[0], center[1])

                print("CALCULATING ROTATION ANGLE") #Debug
                print("x_o, y_o: ", (x_o, y_o)) #Debug
                print("Center: ", (cx_o, cy_o)) #Debug

                if cx_o < x:
                    x1 = cx_o
                    y1 = cy_o
                    x2 = x_o
                    y2 = y_o
                else:
                    x1 = x_o
                    y1 = y_o
                    x2 = cx_o
                    y2 = cy_o

                if abs(x2 - x1) == 0:
                    new_angle = -90
                else:
                    new_angle = -180. * np.arctan((y1 - y2) / abs(x1 - x2)) / np.pi


                print("New angle: ", new_angle) #Debug

                #self.quadFold.info['manual_rotationAngle'] = self.quadFold.info['rotationAngle'] + new_angle
                self.quadFold.info['manual_rotationAngle'] = new_angle


                
                self.deleteInfo(['avg_fold'])
                self.setRotationButton.setChecked(False)
                self.persistRotations.setVisible(True)

                #Put the center (in original image coordinates) into the manual center entry of the key so that it will be used during processing.
                self.quadFold.info['manual_center'] = (int(round(x_o)), int(round(y_o)))
                if 'center' in self.quadFold.info:
                    del self.quadFold.info['center']

                self.processImage()

    
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
        Triggered when mouse hovers on image in image tab
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
        img = self.img

        if img is None:
            return

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            
            if self.doubleZoomGUI.doubleZoomMode:
                self.doubleZoomGUI.beginImgMotion(x, y, len(img[0]), len(img), self.extent, self.imageAxes)

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
                q = f"{q:.4f}"
                # constant = self.calSettings["silverB"] * self.calSettings["radius"]
                # calib_distance = mouse_distance * 1.0/constant
                # calib_distance = f"{calib_distance:.4f}"
            if x < img.shape[1] and y < img.shape[0]:
                #extent = self.extent
                sx = x + extent[0]
                sy = y + extent[1]
                if self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
                    try:
                        self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[int(sy)][int(sx)]) + ", distance=" + str(q) + unit)
                    except:
                        self.imgCoordOnStatusBar.setText("x=NaN" + ', y=NaN'+ ", value=" + "NaN" + ", distance=NaN" + unit)
                else:
                    mouse_distance = np.sqrt((self.quadFold.info['center'][0] - x) ** 2 + (self.quadFold.info['center'][1] - y) ** 2)
                    mouse_distance = f"{mouse_distance:.4f}"
                    try:
                        self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[int(sy)][int(sx)]) + ", distance=" + str(mouse_distance) + unit)
                    except:
                        pass

                o_x, o_y = self.getOrigCoordsCenter(x, y)
                self.left_status.setText("Original Image Coordinates: x=" + str(o_x) + ', y=' + str(o_y))

                self.doubleZoomGUI.mouseHoverBehavior(sx, sy, img, self.imageCanvas, self.doubleZoom.isChecked())

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
        if func[0] == "im_zoomin" and len(self.function) == 1 and self.doubleZoom.isChecked():
            if not self.doubleZoomGUI.doubleZoomMode:
                self.doubleZoomGUI.updateAxes(x, y)
                self.imageCanvas.draw_idle()
        if func[0] == "im_zoomin" and len(self.function) == 2:
            # draw rectangle            
            if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                if len(ax.patches) > 0:
                    ax.patches[0].remove()
                start_pt = func[1]    
                w = abs(start_pt[0] - x)
                h = abs(start_pt[1] - y)
                x = min(start_pt[0], x)
                y = min(start_pt[1], y)
                ax.add_patch(patches.Rectangle((x, y), w, h,
                                            linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            else:
                self.doubleZoomGUI.updateAxes(x, y)
            self.imageCanvas.draw_idle()
        elif func[0] == "im_move":
            if self.img_zoom is not None:
                move = (func[1][0] - x, func[1][1] - y)
                self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[1], img.shape[0])
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
                #ax.invert_yaxis()
                self.imageCanvas.draw_idle()

        elif func[0] == "im_center_rotate":
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    # first_cross = ax.lines[:2]
                    for i in range(len(ax.lines)-1,2,-1):
                        ax.lines[i].remove()
                    # ax.lines = first_cross
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)
            self.imageCanvas.draw_idle()

        elif func[0] == "perp_center":
            # draw X on points and a line between points
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5

            if len(func) == 1:
                if len(ax.lines) > 0:

                    for i in range(len(ax.lines) - 1, 0, -1):
                        ax.lines[i].remove()


                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)
            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:

                    for i in range(len(ax.lines) - 1, 2, -1):
                       # print("LEN = 2") #NICKA DEBUG
                        #print("removing: ", ax.lines[i].get_label()) #NICKA DEBUG
                        ax.lines[i].remove()

                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)

            elif len(func) % 2 != 0:
                #print("LEN FUNC % 2 != 0") #NICKA DEBUG
                if len(ax.lines) > 0:
                    n = (len(func)-1)*5//2 + 2

                    for i in range(len(ax.lines) - 1, n - 1, -1):
                        #print("LEN % 2 != 0") #NICKA DEBUG
                        #print("removing: ", ax.lines[i].get_label()) #NICKA DEBUG
                        ax.lines[i].remove()


                if not self.doubleZoom.isChecked():
                    #print("NOT DOUBLE ZOOM 44") #NICKA DEBUG
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    #print("DOUBLE ZOOM 44") #NICKA DEBUG
                    self.doubleZoomGUI.updateAxes(x, y)

            elif len(func) % 2 == 0:
                #print("LEN FUNC % 2 == 0") #NICKA DEBUG
                start_pt = func[-1]
                if len(ax.lines) > 3:
                    n = len(func) * 5 // 2 - 1

                    for i in range(len(ax.lines) - 1, n - 1, -1):
                        #print("LEN % 2 == 0") #NICKA DEBUG
                        #print("removing: ", ax.lines[i].get_label()) #NICKA DEBUG
                        ax.lines[i].remove()


                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)

            self.imageCanvas.draw_idle()

        elif func[0] == "chords_center":
            #print("Function is chords_center") #NICKA DEBUG
            if self.doubleZoom.isChecked():
                print("DOUBLE ZOOM IS CHECKED") #NICKA DEBUG
                self.doubleZoomGUI.updateAxes(x, y)
            self.imageCanvas.draw_idle()

        elif func[0] == "im_rotate":
            #print("Function is im_rotate") #NICKA DEBUG
            # draw line as angle
            """if self.calSettings is None or 'center' not in self.calSettings:
            self.calSettings = {}
            extent, self.calSettings['center'] = self.getExtentAndCenter()"""
            center = self.quadFold.info['center']
            print("Center: ", center) #NICKA DEBUG

            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            if not self.doubleZoom.isChecked():
                #print("NOT DOUBLE ZOOM") #NICKA DEBUG
                for i in range(len(ax.lines)-1,-1,-1):
                    ax.lines[i].remove()
                ax.plot([x, x2], [y, y2], color="g")
            else:
                #print("DOUBLE ZOOM") #NICKA DEBUG
                if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                    self.doubleZoomGUI.updateAxesInner(x, y)
                elif self.doubleZoomGUI.doubleZoomMode:
                    for i in range(len(ax.lines) - 1, -1, -1):
                        print("IN IMAGE ROTATE FUNC: ", ax.lines[i].get_label()) #NICKA DEBUG
                    for i in range(len(ax.lines)-1,-1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
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

                    self.setRmin(rmin)
                    self.function = None
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
            ax = self.resultAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            self.resultCanvas.draw_idle()
        else:
            self.function = None
            self.setRminButton.setChecked(False)
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
        print("Apply BGSub Function") #NICKA DEBUG
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
        self.doubleZoomGUI.doubleZoomChecked(img=self.quadFold.getRotatedImage() if self.quadFold is not None else None, 
                                                                                  canv=self.imageCanvas, 
                                                                                  center=self.quadFold.info['center'] if self.quadFold is not None else (0,0),
                                                                                  is_checked=self.doubleZoom.isChecked())

    def modeAngleChecked(self):
        """
        Triggered when mode angle is checked or unchecked
        """
        print("Function executed", flush=True)

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
        if reprocess:
            self.quadFold.info = {}
            self.quadFold.info['reprocess'] = True
        if 'saveCroppedImage' not in self.quadFold.info:
            self.quadFold.info['saveCroppedImage'] = self.cropFoldedImageChkBx.isChecked()
        self.markFixedInfo(self.quadFold.info, previnfo)
        original_image = self.quadFold.orig_img
        if self.calSettings is not None and not self.calSettings:
            self.imgDetailOnStatusBar.setText(str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype))
        elif self.calSettings is not None and self.calSettings:
            self.imgDetailOnStatusBar.setText(str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype) + " (Image Calibrated)")
        self.imgDetailOnStatusBar.setText(str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype))
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


    def onFixedRotationChkBxToggled(self):
        """
        Toggles whether the current rotation is persisted in the QuadrantFolderGUI object.
        If there is no current rotation angle known, calculates it.
        """

        if self.fixedRotationChkBx.isChecked():
            try:
                if 'rotationAngle' not in self.quadFold.info:
                    self.processImage()

                self.persistedRotation = self.quadFold.info['rotationAngle']
            except:
                print("Error trying to fix rotation")
        else:
            self.persistedRotation = None


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

        if self.calSettingsDialog.fixedCenter.isChecked() and prevInfo is not None and 'calib_center' in prevInfo:
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
            self.img = img
            img = self.quadFold.orig_img

            extent = [0,0]
            center = self.quadFold.info['center']

            self.extent = extent
            print("EXTENT IN GUI SHOW: ", extent) #NICKA DEBUG
            print("IMG SHAPE IN GUI SHOW: ", img.shape) #NICKA DEBUG
            print("PLUGGED IN TO IMSHOW", [0-extent[0], img.shape[1] - extent[0], img.shape[0]-extent[1], 0 - extent[1]]) #NICKA DEBUG
            # img = getBGR(get8bitImage(img, min=self.spminInt.value(), max=self.spmaxInt.value()))

            print("Center: ", center) #NICK ADNEUG


            if self.logScaleIntChkBx.isChecked():
                #ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0]-extent[1], 0 - extent[1]])
                ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()))
            else:
                #ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0]-extent[1], 0 - extent[1]])
                ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()))
            ax.set_facecolor('black')

            self.orientationCmbBx.setCurrentIndex(0 if self.orientationModel is None else self.orientationModel)

            if self.showSeparator.isChecked():
                # Draw quadrant separator
                ax.axvline(center[0], color='y')
                ax.axhline(center[1], color='y')


            o_x, o_y = self.getOrigCoordsCenter(center[0], center[1])
            self.calSettingsDialog.centerX.setValue(o_x)
            self.calSettingsDialog.centerY.setValue(o_y)

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
                print("Zoom case 1") #NICKA DEBUG
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
            elif self.default_img_zoom is not None and len(self.default_img_zoom) == 2:
                print("Zoom Case 2") #NICKA DEBUG
                ax.set_xlim(self.default_img_zoom[0])
                ax.set_ylim(self.default_img_zoom[1])
            else:
                print("Zoom case 3") #NICKA DEBGU
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
        center = [self.quadFold.fixedCenterX, self.quadFold.fixedCenterY]
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
            return [0,0], (0,0)
        if self.quadFold.orig_image_center is None and (self.quadFold.fixedCenterX is None or self.quadFold.fixedCenterY is None):
            self.quadFold.findCenter()
            self.statusPrint("Done.")
        if self.quadFold.fixedCenterX is not None and self.quadFold.fixedCenterY is not None:
            center = []
            center.append(self.quadFold.fixedCenterX)
            center.append(self.quadFold.fixedCenterY)
        elif 'calib_center' in self.quadFold.info:
            center = self.quadFold.info['calib_center']
        elif 'manual_center' in self.quadFold.info:
            center = self.quadFold.info['manual_center']
        else:
            center = self.quadFold.orig_image_center
        if 'center' not in self.quadFold.info:
            extent = [0, 0]
        else:
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
                if self.calSettingsDialog.fixedCenter.isChecked() and self.calSettings is not None and 'center' in self.calSettings:
                    self.quadFold.fixedCenterX, self.quadFold.fixedCenterY = self.calSettings['center']
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
            if self.threadPool.activeThreadCount() == 0 and self.tasksDone == self.numberOfFiles:
                print("All threads are complete")
                self.currentFileNumber = 0
                self.progressBar.setVisible(False)
                self.filenameLineEdit.setEnabled(True)
                self.filenameLineEdit2.setEnabled(True)
                self.csvManager.sortCSV()
                print("bgasyncdict: ", len(self.bgAsyncDict)) #NICKA DEBUG
                with open(join(self.filePath, 'qf_results/bg/background_sum.csv'), 'w', newline='') as csvfile:
                    writer = csv.writer(csvfile)

                    writer.writerow(['Name', 'Sum'])

                    for name, sum in self.bgAsyncDict.items():
                        writer.writerow([name, sum])

                self.showProcessingFinishedMessage()

    def startNextTask(self):
        self.progressBar.setVisible(True)
        self.filenameLineEdit.setEnabled(False)
        self.filenameLineEdit2.setEnabled(False)
        bg_csv_lock = Lock()
        while not self.tasksQueue.empty() and self.threadPool.activeThreadCount() < self.threadPool.maxThreadCount() / 2:
            params = self.tasksQueue.get()
            self.currentTask = Worker(params, self.calSettingsDialog.fixedCenter.isChecked(), 
                                      self.persistedCenter, self.persistedRotation, self.bgChoiceIn.currentText(), 
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
            #pass
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
            if self.calSettings is not None and 'center' in self.calSettings and 'calib_center' in info:
                # Update cal settings center with the corresponding coordinate in original (or initial) image
                # so that it persists correctly on moving to next image
                self.calSettings['center'] = info['calib_center']
            if not self.zoomOutClicked and self.quadFold.initImg is not None:
                _, center = self.getExtentAndCenter()
                print(center)
                cx, cy = center
                cxr, cyr = self.quadFold.info['center']
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
        fileFullPath = fullPath(self.filePath, self.imgList[self.currentFileNumber])
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(self.currentFileNumber + 1) + '/' + str(self.numberOfFiles) + ') : ' + fileFullPath)
        
    def resetStatusbar2(self):
        """
        Reset the status bar, but search using self.quadFold.info
        """
        
        index = self.imgList.index(self.quadFold.img_name)
        #DOES NOT GET HERE
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

        # image
        flags['orientation_model'] = self.orientationModel
        flags["ignore_folds"] = self.ignoreFolds
        flags['mask_thres'] = self.maskThresSpnBx.value()

        flags['blank_mask'] = self.blankImageGrp.isChecked()
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


        if self.modeAngleChkBx.isChecked():
            modeOrientation = self.getModeRotation()
            if modeOrientation is not None:
                flags["mode_angle"] = modeOrientation

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
                self.updateLeftWidgetWidth()
                self.resetWidgets()
                QApplication.restoreOverrideCursor()
                if self.h5List == []:
                    fileName = self.imgList[self.currentFileNumber]
                    try:
                        self.quadFold = QuadrantFolder(self.filePath, fileName, self, self.fileList, self.ext)

                        success = self.setCalibrationImage(force=True)

                        if success:
                            self.deleteInfo(['rotationAngle'])
                            self.deleteImgCache(['BgSubFold'])
                            self.quadFold.info['manual_center'] = [self.calSettingsDialog.centerX.value(), self.calSettingsDialog.centerY.value()]
                            self.processImage()

                    except Exception as e:
                        infMsg = QMessageBox()
                        infMsg.setText("Error trying to open " + str(fileName))
                        infMsg.setInformativeText("This usually means that the image is corrupted or missing.")
                        infMsg.setStandardButtons(QMessageBox.Ok)
                        infMsg.setIcon(QMessageBox.Information)
                        infMsg.exec_()
                        self.browseFile()
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
        self.tranRSpnBx.setValue(-1)
        self.tranDeltaSpnBx.setValue(-1)
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
            self.updateLeftWidgetWidth()
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

        #NICK ALLISON
        #If the fixed center box is checked, then:
        #Print message
        #store the current center in the quadfoldgui object
        #Display Center on popup window
        if self.calSettingsDialog.fixedCenter.isChecked() and self.calSettings['center'] is not None:
            print("USING PERSISTED CENTER")
            self.persistedCenter = self.calSettings['center']
            text += "\n  - Center : " + str(self.persistedCenter)

        #Same thing for rotation
        if self.persistedRotation is not None:
            print("USING PERSISTED ROTATION ANGLE")
            text += "\n  - Rotation Angle : " + str(self.persistedRotation)

        if len(self.ignoreFolds) > 0:
            text += "\n  - Ignore Folds : " + str(list(self.ignoreFolds))
        text += "\n  - Orientation Finding : " + str(self.orientationCmbBx.currentText())
        text += "\n  - Mask Threshold : " + str(flags["mask_thres"])
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
        self.highlightApplyUndo()
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
                text += "\n  - Tophat : " + str(flags["tophat1"])
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
        self.highlightApplyUndo()
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
        
