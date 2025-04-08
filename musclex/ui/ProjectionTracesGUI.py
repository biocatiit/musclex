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
from musclex import __version__
from ..utils.misc_utils import inverseNmFromCenter
from ..utils.file_manager import fullPath, getImgFiles, createFolder
from ..utils.image_processor import getPerpendicularLineHomogenous, calcSlope, getIntersectionOfTwoLines, getBGR, get8bitImage, getNewZoom, getCenter, rotateImageAboutPoint, rotatePoint, processImageForIntCenter, getMaskThreshold
from ..modules.ProjectionProcessor import ProjectionProcessor
from ..ui.ProjectionBoxTab import ProjectionBoxTab
from ..CalibrationSettings import CalibrationSettings
from ..csv_manager import PT_CSVManager
from .ImageMaskTool import ImageMaskerWindow
from .DoubleZoomGUI import DoubleZoom
from .pyqt_utils import *

class ProjectionParams:
    def __init__(self, dir_path, img_name, fileList, ext, settings):
        self.settings = settings
        self.dir_path = dir_path
        self.img_name = img_name
        self.fileList = fileList
        self.ext = ext

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
                self.projProc = ProjectionProcessor(self.params.dir_path, self.params.img_name, self.params.fileList, self.params.ext)
            self.projProc.process(self.settings)
        except:
            traceback.print_exc()
            self.signals.error.emit((traceback.format_exc()))
            infMsg = QMessageBox()
            infMsg.setText("Error trying to open " + str(self.params.img_name))
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
        

class ProjectionTracesGUI(QMainWindow):
    """
    This class is for Projection Traces GUI Object
    """
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Muscle X Projection Traces v." + __version__)
        self.current_file = 0
        self.dir_path = ""
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
        self.center_func = None
        self.rotated = True
        self.rotationAngle = 0
        self.calSettingsDialog = None
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

        self.browseFile()

    def initUI(self):
        """
        Initial all GUI
        """
        #### Image Tab ####
        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.centralWidget = QWidget(self)

        self.scrollArea.setWidget(self.centralWidget)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.scrollArea)

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(True)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 20px; width: 200px; }")

        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayer = QHBoxLayout(self.imageTab)

        self.displayImgFigure = plt.figure()
        self.displayImgAxes = self.displayImgFigure.add_subplot(111)
        #self.imageVLayout = QVBoxLayout()
        self.displayImgCanvas = FigureCanvas(self.displayImgFigure)
        #self.imageVLayout.addWidget(self.displayImgCanvas)

        self.imageLeftFrame = QFrame()
        self.imageLeftFrame.setFixedWidth(300)
        self.leftFrameLayout = QVBoxLayout(self.imageLeftFrame)

        # Image selection
        self.selectImageGrp = QGroupBox("1. Select an image")
        self.selectImageLayout = QVBoxLayout(self.selectImageGrp)
        self.browseImageButton = QPushButton("Browse")
        self.selectImageLayout.addWidget(self.browseImageButton)

        # Pattern Properties
        self.propGrp = QGroupBox("2. Pattern Settings (Optional)")
        self.propGrp.setEnabled(False)
        self.propLayout = QGridLayout(self.propGrp)

        self.calibrateButton = QPushButton("Calibration Settings")
        self.calSettingsDialog = None
        self.setRotAndCentB = QPushButton("Set Rotation Angle and Center")
        self.setRotAndCentB.setCheckable(True)
        self.setCentByChords = QPushButton("Set Center by Chords")
        self.setCentByChords.setCheckable(True)
        self.checkableButtons.append(self.setCentByChords)
        self.setCentByPerp = QPushButton("Set Center by Perpendiculars")
        self.setCentByPerp.setCheckable(True)
        self.checkableButtons.append(self.setCentByPerp)
        self.setRotationButton = QPushButton("Set Rotation Angle")
        self.setRotationButton.setCheckable(True)
        self.checkableButtons.append(self.setRotationButton)
        self.qfChkBx = QCheckBox("Quadrant Folded?")
        self.qfChkBx.setChecked(True)
        self.doubleZoom = QCheckBox("Double Zoom")
        self.maskThresSpnBx = QDoubleSpinBox()
        self.maskThresSpnBx.setMinimum(-10000)
        self.maskThresSpnBx.setMaximum(10000)
        self.maskThresSpnBx.setValue(-999)
        self.maskThresSpnBx.setKeyboardTracking(False)

        self.propLayout.addWidget(self.calibrateButton, 0, 0, 1, 4)
        self.propLayout.addWidget(self.setCentByChords, 1, 0, 1, 4)
        self.propLayout.addWidget(self.setCentByPerp, 2, 0, 1, 4)
        self.propLayout.addWidget(self.setRotAndCentB, 3, 0, 1, 4)
        self.propLayout.addWidget(self.setRotationButton, 4, 0, 1, 4)
        self.propLayout.addWidget(self.qfChkBx, 5, 0, 1, 2)
        self.propLayout.addWidget(self.doubleZoom, 5, 2, 1, 2)
        self.propLayout.addWidget(QLabel('Mask Threshold:'), 6, 0, 1, 2)
        self.propLayout.addWidget(self.maskThresSpnBx, 6, 2, 1, 2)

        # Box selection
        self.boxGrp = QGroupBox("3. Add boxes")
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

        # Peaks Selection
        self.selectPeaksGrp = QGroupBox("4. Peaks")
        self.selectPeaksGrp.setEnabled(False)
        self.selectPeaksLayout = QVBoxLayout(self.selectPeaksGrp)
        self.selectPeaksButton = QPushButton("Select Approximate Peak Locations")
        self.selectPeaksButton.setCheckable(True)
        self.checkableButtons.append(self.selectPeaksButton)
        self.selectPeaksLayout.addWidget(self.selectPeaksButton)

        self.leftFrameLayout.addWidget(self.selectImageGrp)
        self.leftFrameLayout.addSpacing(10)
        self.leftFrameLayout.addWidget(self.propGrp)
        self.leftFrameLayout.addSpacing(10)
        self.leftFrameLayout.addWidget(self.boxGrp)
        self.leftFrameLayout.addSpacing(10)
        self.leftFrameLayout.addWidget(self.selectPeaksGrp)
        self.leftFrameLayout.addStretch()

        self.imageRightFrame = QFrame()
        self.imageRightFrame.setFixedWidth(250)
        self.rightFrameLayout = QVBoxLayout(self.imageRightFrame)

        # Display Options
        self.dispOptGrp = QGroupBox("Display Options")
        self.dispOptLayout = QGridLayout(self.dispOptGrp)

        self.boxesChkBx = QCheckBox("Boxes")
        self.boxesChkBx.setChecked(True)
        self.peaksChkBx = QCheckBox("Peaks")
        self.peaksChkBx.setChecked(True)
        self.centerChkBx = QCheckBox("Center")
        self.centerChkBx.setChecked(False)

        self.imgZoomInB = QPushButton("Zoom In")
        self.imgZoomInB.setCheckable(True)
        self.checkableButtons.append(self.imgZoomInB)
        self.imgZoomOutB = QPushButton("Full")

        self.minIntLabel = QLabel("Min Intensity")
        self.minIntSpnBx = QDoubleSpinBox()
        self.minIntSpnBx.setKeyboardTracking(False)
        self.minIntSpnBx.setDecimals(2)

        self.maxIntLabel = QLabel("Max Intensity")
        self.maxIntSpnBx = QDoubleSpinBox()
        self.maxIntSpnBx.setValue(0)
        self.maxIntSpnBx.setDecimals(2)
        self.maxIntSpnBx.setKeyboardTracking(False)

        self.persistIntensity = QCheckBox("Persist intensities")
        self.logScaleIntChkBx = QCheckBox("Log scale intensity")

        self.dispOptLayout.addWidget(self.minIntLabel, 0, 0, 1, 2)
        self.dispOptLayout.addWidget(self.minIntSpnBx, 1, 0, 1, 2)
        self.dispOptLayout.addWidget(self.maxIntLabel, 0, 2, 1, 2)
        self.dispOptLayout.addWidget(self.maxIntSpnBx, 1, 2, 1, 2)
        self.dispOptLayout.addWidget(self.persistIntensity, 2, 0, 1, 4)
        self.dispOptLayout.addWidget(self.logScaleIntChkBx, 3, 0, 1, 4)
        self.dispOptLayout.addWidget(self.imgZoomInB, 4, 0, 1, 2)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 4, 2, 1, 2)
        self.dispOptLayout.addWidget(self.centerChkBx, 5, 0, 1, 4)
        self.dispOptLayout.addWidget(self.boxesChkBx, 6, 0, 1, 4)
        self.dispOptLayout.addWidget(self.peaksChkBx, 7, 0, 1, 4)
        # Blank Image Settings
        self.blankImageGrp = QGroupBox("Enable Blank Image and Mask")
        self.blankImageGrp.setCheckable(True)
        self.blankImageGrp.setChecked(False)
        self.blankImageLayout = QVBoxLayout(self.blankImageGrp)
        self.blankSettingButton = QPushButton("Set Blank Image and Mask")
        self.blankImageLayout.addWidget(self.blankSettingButton)

        # Process Folder Button
        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet(pfss)
        self.processFolderButton.setCheckable(True)
        self.processH5FolderButton = QPushButton("Process All H5 Files")
        self.processH5FolderButton.setStyleSheet(pfss)
        self.processH5FolderButton.setCheckable(True)

        # Export 1-D Projections
        self.exportChkBx = QCheckBox("Export all 1-D Projections")
        self.exportChkBx.setChecked(True)

        # next previos buttons
        self.nextButton = QPushButton(">")
        self.prevButton = QPushButton("<")
        self.nextFileButton = QPushButton(">>>")
        self.prevFileButton = QPushButton("<<<")
        self.nextButton.setToolTip('Next Frame')
        self.prevButton.setToolTip('Previous Frame')
        self.nextFileButton.setToolTip('Next H5 File in this Folder')
        self.prevFileButton.setToolTip('Previous H5 File in this Folder')
        self.bottomLayout = QGridLayout()
        self.bottomLayout.addWidget(self.exportChkBx, 0, 0, 1, 4)
        self.bottomLayout.addWidget(self.processFolderButton, 1, 0, 1, 4)
        self.bottomLayout.addWidget(self.processH5FolderButton, 2, 0, 1, 4)
        self.bottomLayout.addWidget(self.prevButton, 3, 0, 1, 2)
        self.bottomLayout.addWidget(self.nextButton, 3, 2, 1, 2)
        self.bottomLayout.addWidget(self.prevFileButton, 4, 0, 1, 2)
        self.bottomLayout.addWidget(self.nextFileButton, 4, 2, 1, 2)

        self.rightFrameLayout.addWidget(self.dispOptGrp)
        self.rightFrameLayout.addSpacing(10)
        self.rightFrameLayout.addWidget(self.blankImageGrp)
        self.rightFrameLayout.addStretch()
        self.rightFrameLayout.addLayout(self.bottomLayout)

        self.imageTabLayer.addWidget(self.imageLeftFrame)
        self.imageTabLayer.addWidget(self.displayImgCanvas)
        self.imageTabLayer.addWidget(self.imageRightFrame)

        self.tabWidget.addTab(self.imageTab, "Image")
        self.tabWidget.tabBar().setTabButton(0, QTabBar.LeftSide, None)
        self.tabWidget.tabBar().setTabButton(0, QTabBar.RightSide, None)

        ### Status Bars ###
        self.upperStatusBar = QStatusBar()
        
        self.right_status = QLabel()
        self.pixel_detail = QLabel()
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        
        self.upperStatusBar.addPermanentWidget(self.pixel_detail)
        self.upperStatusBar.addPermanentWidget(self.right_status)
        self.upperStatusBar.addPermanentWidget(self.progressBar)

        self.lowerStatusBar = QStatusBar()
        self.left_status = QLabel()
        self.lowerStatusBar.addWidget(self.left_status)

        self.mainLayout.addWidget(self.tabWidget)
        self.mainLayout.addWidget(self.upperStatusBar)
        self.mainLayout.addWidget(self.lowerStatusBar)

        #### Menu Bar #####
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

        self.show()
        self.resize(1300, 700)

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.tabWidget.currentChanged.connect(self.updateUI)
        self.tabWidget.tabCloseRequested.connect(self.removeTab)

        # Image seletion
        self.browseImageButton.clicked.connect(self.browseFile)

        # # Pattern Properties
        self.calibrateButton.clicked.connect(self.calibrationClicked)
        self.setRotationButton.clicked.connect(self.setRotation)
        self.setCentByChords.clicked.connect(self.setCenterByChordsClicked)
        self.setCentByPerp.clicked.connect(self.setCenterByPerpClicked)
        self.qfChkBx.stateChanged.connect(self.qfChkBxClicked)
        self.setRotAndCentB.clicked.connect(self.setAngleAndCenterClicked)
        self.doubleZoom.stateChanged.connect(self.doubleZoomChecked)

        # Display options
        self.maxIntSpnBx.valueChanged.connect(self.updateImage)
        self.minIntSpnBx.valueChanged.connect(self.updateImage)
        self.boxesChkBx.stateChanged.connect(self.updateImage)
        self.peaksChkBx.stateChanged.connect(self.updateImage)
        self.centerChkBx.stateChanged.connect(self.updateImage)
        self.imgZoomInB.clicked.connect(self.imgZoomIn)
        self.imgZoomOutB.clicked.connect(self.imgZoomOut)
        self.logScaleIntChkBx.stateChanged.connect(self.updateImageTab)

        # Blank Image
        self.blankImageGrp.clicked.connect(self.blankChecked)
        self.blankSettingButton.clicked.connect(self.blankSettingClicked)

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

        # Process Folder button
        self.processFolderButton.clicked.connect(self.batchProcBtnToggled)
        self.processH5FolderButton.clicked.connect(self.h5batchProcBtnToggled)

        self.prevButton.clicked.connect(self.prevClicked)
        self.nextButton.clicked.connect(self.nextClicked)
        self.prevFileButton.clicked.connect(self.prevFileClicked)
        self.nextFileButton.clicked.connect(self.nextFileClicked)

        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)
        self.displayImgFigure.canvas.mpl_connect('button_release_event', self.imgReleased)
        self.displayImgFigure.canvas.mpl_connect('figure_leave_event', self.leaveImage)
        self.displayImgFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)

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

    def blankChecked(self):
        """
        Handle when the Blank image and mask is checked or unchecked
        """
        if self.projProc is not None and not self.syncUI:
            self.projProc = ProjectionProcessor(self.dir_path, self.imgList[self.current_file], self.fileList, self.ext)
            self.projProc.info['hists'] = {}
            self.masked = False
            print("Blank checked")
            self.processImage()
            self.updateImage

    def blankSettingClicked(self):
        """
        Trigger when Set Blank Image and Mask clicked
        """

        img = self.projProc.getRotatedImage()

        try:
            fabio.tifimage.tifimage(data=img).write(join(self.dir_path,'settings/tempMaskFile_pt.tif'))
        except:
            print("ERROR WITH SAVING THE IMAGE")

        max_val = np.max(np.ravel(img))

        ext = self.projProc.filename.split('.')[-1]

        rot_ang = None if 'rotationAngle' not in self.projProc.info else self.projProc.info['rotationAngle']

        trans_x = (img.shape[0] - self.projProc.orig_img.shape[0]) / 2
        trans_y = (img.shape[1] - self.projProc.orig_img.shape[1]) / 2

        trans_mat = np.float32([[1,0,trans_x],[0,1,trans_y]])

        imageMaskingTool = ImageMaskerWindow(self.dir_path, 
                                             os.path.join(self.dir_path, "settings/tempMaskFile_pt.tif"), 
                                             self.minIntSpnBx.value(),
                                             self.maxIntSpnBx.value(),
                                             max_val,
                                             orig_size=self.projProc.orig_img.shape,
                                             trans_mat=trans_mat,                                            
                                             rot_angle=rot_ang,
                                             isHDF5= ext in ('hdf5', 'h5'),
                                             )

        if imageMaskingTool is not None and imageMaskingTool.exec_():
            if os.path.exists(join(join(self.dir_path, 'settings'), 'blank_image_settings.json')):
                with open(join(join(self.dir_path, 'settings'), 'blank_image_settings.json'), 'r') as f:
                    info = json.load(f)
                    if 'path' in info:
                        img = fabio.open(info['path']).data
                        fabio.tifimage.tifimage(data=img).write(join(join(self.dir_path, 'settings'),'blank.tif'))    
            else:
                if os.path.exists(join(join(self.dir_path, 'settings'), 'mask.tif')):
                    os.rename(join(join(self.dir_path, 'settings'), 'mask.tif'), join(join(self.dir_path, 'settings'), 'maskonly.tif'))

        if self.projProc is not None:
            self.masked = False
            print("Blank setting clicked")
            self.processImage()

    def maskThresChanged(self):
        """
        Trigger when Mask threshold is changed
        """
        if self.projProc is not None:
            self.projProc.info['hists'] = {}
            print("Mask threshold changed")
            self.processImage()

    def calibrationClicked(self):
        """
        Triggered when calibration settings button pressed
        """
        success = self.launchCalibrationSettings(force=True)
        if self.projProc is not None and success:
            self.center_func = 'automatic'
            self.rotated = False
            self.projProc.rotMat = None
            self.updateImage()
            print("calibration clicked")
            self.processImage()

    def launchCalibrationSettings(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or calibration.tif in the folder
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """

        if self.calSettingsDialog is None:
            self.calSettingsDialog = CalibrationSettings(self.dir_path) if self.projProc is None else \
                CalibrationSettings(self.dir_path, center=self.projProc.info['orig_center'])
        self.calSettings = None
        cal_setting = self.calSettingsDialog.calSettings
        if cal_setting is not None or force:
            result = self.calSettingsDialog.exec_()
            if result == 1:
                self.calSettings = self.calSettingsDialog.getValues()
                return True
        return False

    def setCenterByChordsClicked(self):
        """
        Prepare for manual rotation center setting by selecting chords
        """
        if self.projProc is None:
            return

        if self.setCentByChords.isChecked():
            self.rotated = False
            self.updateImage()
            ax = self.displayImgAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.displayImgCanvas.draw_idle()
            self.chordpoints=[]
            self.chordLines = []
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

            cx = int(sum([centers[i][0] for i in range(0, len(centers))]) / len(centers))
            cy = int(sum([centers[i][1] for i in range(0, len(centers))]) / len(centers))
            new_center = [cx, cy] #np.dot(invM, homo_coords)
            print("New center ", new_center)
            self.centerx = int(round(new_center[0]))
            self.centery = int(round(new_center[1]))
            self.projProc.info['centerx'] = self.centerx
            self.projProc.info['centery'] = self.centery
            self.projProc.info['orig_center'] = (self.centerx, self.centery)
            self.setCentByChords.setChecked(False)
            self.center_func = 'manual'
            self.rotated = True
            self.updateCenter()
            self.removeAllTabs()
            print("set center by chords")
            self.processImage()
            self.addBoxTabs()
            self.updateImage()

    def setCenterByPerpClicked(self):
        """
        Prepare for manual center selection using perpendicular peaks
        :return:
        """
        if self.projProc is None:
            return
        if self.setCentByPerp.isChecked():
            self.rotated = False
            self.updateImage()
            ax = self.displayImgAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.displayImgCanvas.draw_idle()
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

            new_center = [cx, cy]  # np.dot(invM, homo_coords)
            # Set new center and rotaion angle , re-calculate R-min
            print("New Center ", new_center)
            self.centerx = int(round(new_center[0]))
            self.centery = int(round(new_center[1]))
            self.projProc.info['centerx'] = int(round(new_center[0]))
            self.projProc.info['centery'] = int(round(new_center[1]))
            self.projProc.info['orig_center'] = (self.centerx, self.centery)
            
            self.setCentByPerp.setChecked(False)
            self.center_func = 'manual'
            self.rotated = True
            self.updateCenter()
            self.removeAllTabs()
            self.processImage()
            self.addBoxTabs()
            self.updateImage()

    def setRotation(self):
        """
        Trigger when set center and rotation angle button is pressed
        """
        if self.setRotationButton.isChecked():
            # clear plot
            self.left_status.setText(
                "Rotate the line to the pattern equator (ESC to cancel)")
            self.rotated = False
            self.updateImage()
            ax = self.displayImgAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.displayImgCanvas.draw_idle()
            self.function = ["im_rotate"]
        else:
            self.function = None
            self.rotated = True
            self.removeAllTabs()
            self.addBoxTabs()
            self.updateImage()
            self.refreshStatusbar()

    def setAngleAndCenterClicked(self):
        """
        Triggered when the Set angle and center button is clicked
        """
        if len(self.imgList) == 0:
            return

        if self.setRotAndCentB.isChecked():
            self.setLeftStatus("Click on 2 corresponding reflection peaks along the equator (ESC to cancel)")
            self.rotated = False
            self.updateImage()
            ax = self.displayImgAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.displayImgCanvas.draw_idle()
            self.function = ["angle_center"]
        else:
            self.function = None
            self.rotated = True
            self.removeAllTabs()
            self.addBoxTabs()
            self.updateImage()
            self.refreshStatusbar()

    def doubleZoomChecked(self):
        """
        Triggered when double zoom is checked
        """
        
        self.doubleZoomGUI.doubleZoomChecked(img=self.projProc.getRotatedImage(),
                                             canv=self.displayImgCanvas,
                                             is_checked=self.doubleZoom.isChecked())

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

    def imgZoomIn(self):
        """
        Triggered when Zoom in image is pressed
        """
        if self.imgZoomInB.isChecked():
            self.setLeftStatus("Please select zoom-in area by clicking 2 points to make a rectangle (ESC to cancel)")
            self.clearImage()
            self.function = ["im_zoomin"]
        else:
            self.function = None

    def imgZoomOut(self):
        """
        Triggered when Zoom out image is pressed
        """
        self.imgZoomInB.setChecked(False)
        self.img_zoom = None
        self.updateImage()

    def updateImage(self):
        """
        Refresh image tab
        """
        self.update_plot['img'] = True
        self.updateUI()

    def qfChkBxClicked(self):
        """
        Triggered when the quandrant fold button is clicked
        """
        if self.qfChkBx.isChecked():
            self.center_func = 'quadrant_fold'
            self.rotated = False
        elif self.center_func == 'init':
            self.rotated = False
            pass
        else:
            self.center_func = 'automatic'
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
        if self.processFolderButton.isChecked():
            if not self.progressBar.isVisible():
                self.processFolderButton.setText("Stop")
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
        
        self.processFolderButton.setChecked(False)
        if self.ext in ['.h5', '.hdf5']:
            self.processFolderButton.setText("Process Current H5 File")
        else:
            self.processFolderButton.setText("Process Current Folder")

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

        self.processH5FolderButton.setChecked(False)
        self.processH5FolderButton.setText("Process All H5 Files")

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
                self.function.append((self.projProc.info['centerx'], self.projProc.info['centery']))
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

        if key == Qt.Key_Right:
            self.nextClicked()
        elif key == Qt.Key_Left:
            self.prevClicked()
        elif key == Qt.Key_Escape:
            
            self.resetUI()
        elif key == Qt.Key_D:
            self.tabWidget.setCurrentIndex((self.tabWidget.currentIndex() + 1) % self.tabWidget.count())
        elif key == Qt.Key_A:
            self.tabWidget.setCurrentIndex((self.tabWidget.currentIndex() - 1) % self.tabWidget.count())
        elif key == Qt.Key_S:
            self.maxIntSpnBx.stepDown()
        elif key == Qt.Key_W:
            self.maxIntSpnBx.stepUp()
        elif key == Qt.Key_Q:
            self.close()

    def mousePressEvent(self, event):
        """
        Clear focus when mouse pressed
        """
        focused_widget = QApplication.focusWidget()
        if focused_widget is not None:
            focused_widget.clearFocus()

    def prevClicked(self):
        """
        Going to the previous image
        """
        self.current_file = (self.current_file - 1) % len(self.imgList)
        self.onImageChanged()

    def nextClicked(self):
        """
        Going to the next image
        """
        self.current_file = (self.current_file + 1) % len(self.imgList)
        self.onImageChanged()

    def prevFileClicked(self):
        """
        Going to the previous h5 file
        """
        QApplication.setOverrideCursor(Qt.WaitCursor)
        if len(self.h5List) > 1:
            self.h5index = (self.h5index - 1) % len(self.h5List)
            self.dir_path, self.imgList, self.currentImg, self.fileList, self.ext = getImgFiles(join(self.dir_path, self.h5List[self.h5index]))
            self.onImageChanged()
        QApplication.restoreOverrideCursor()

    def nextFileClicked(self):
        """
        Going to the next h5 file
        """
        QApplication.setOverrideCursor(Qt.WaitCursor)
        if len(self.h5List) > 1:
            self.h5index = (self.h5index + 1) % len(self.h5List)
            self.dir_path, self.imgList, self.currentImg, self.fileList, self.ext = getImgFiles(join(self.dir_path, self.h5List[self.h5index]))
            self.onImageChanged()
        QApplication.restoreOverrideCursor()

    def setH5Mode(self, file_name):
        """
        Sets the H5 list of file and displays the right set of buttons depending on the file selected
        """
        if self.ext in ['.h5', '.hdf5']:
            for file in os.listdir(self.dir_path):
                if file.endswith(".h5") or file.endswith(".hdf5"):
                    self.h5List.append(file)
            self.h5index = self.h5List.index(os.path.split(file_name)[1])
            self.nextFileButton.show()
            self.prevFileButton.show()
            self.processH5FolderButton.show()
            self.processFolderButton.setText("Process Current H5 File")
        else:
            self.nextFileButton.hide()
            self.prevFileButton.hide()
            self.processH5FolderButton.hide()
            self.processFolderButton.setText("Process Current Folder")

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
            self.pixel_detail.setText("")
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
        elif self.doubleZoomGUI.doubleZoomMode:
            self.doubleZoomGUI.mouseClickBehavior(x, y)
            return

        if self.doubleZoom.isChecked() and not self.doubleZoomGUI.doubleZoomMode:
            x, y = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
            self.doubleZoomGUI.doubleZoomMode = True

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

        elif func[0] == "angle_center":
            axis_size = 5
            if self.doubleZoom.isChecked() and not self.doubleZoomGUI.doubleZoomMode:
                x, y = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
                self.doubleZoomGUI.doubleZoomMode = True
            ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.displayImgCanvas.draw_idle()
            func.append((x, y))
            if len(func) == 3:
                QApplication.restoreOverrideCursor()

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

                cx = int(round((x1 + x2) / 2.))
                cy = int(round((y1 + y2) / 2.))
                # self.projProc.info['rotationAngle'] = 0
                # M = cv2.getRotationMatrix2D((self.projProc.info['centerx'], self.projProc.info['centery']), self.projProc.info['rotationAngle'], 1)
                # invM = cv2.invertAffineTransform(M)
                # homo_coords = [cx, cy, 1.]
                # new_center = np.dot(invM, homo_coords)
                new_center = [cx, cy]
                self.centerx = int(round(new_center[0]))
                self.centery = int(round(new_center[1]))
                self.projProc.info['centerx'] = self.centerx
                self.projProc.info['centery'] = self.centery
                self.projProc.info['orig_center'] = (self.centerx, self.centery)
                self.rotationAngle = new_angle
                self.projProc.info['rotationAngle'] = new_angle
                self.setRotAndCentB.setChecked(False)
                self.center_func = 'manual'
                self.rotated = True
                self.updateCenter()
                self.removeAllTabs()
                self.processImage()
                self.addBoxTabs()
                self.updateImage()

        elif func[0] == "perp_center":
            axis_size = 5
            ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            if self.doubleZoom.isChecked() and len(func) > 1 and len(func) % 2 == 0:
                start_pt = func[len(func) - 1]
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            self.displayImgCanvas.draw_idle()
            func.append((x, y))

        elif func[0] == "chords_center":
            axis_size = 5
            self.chordpoints.append([x, y])
            ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            if len(self.chordpoints) >= 3:
                self.drawPerpendiculars()
            self.displayImgCanvas.draw_idle()

        elif func[0] == "im_rotate":
            # set rotation angle
            center = self.projProc.info['orig_center']

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

            self.rotationAngle = new_angle
            self.projProc.info['rotationAngle'] = new_angle
            self.setRotationButton.setChecked(False)
            self.rotated = True
            self.removeAllTabs()
            self.processImage()
            self.addBoxTabs()
            self.updateImage()

        elif func[0] == "im_zoomin":
                func.append((x, y))
                if len(func) == 3:
                    p1 = func[1]
                    p2 = func[2]
                    # Set zoom-in location ( x,y, ranges) and update image tab
                    self.img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.imgZoomInB.setChecked(False)
                    self.updateImage()

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
            if self.doubleZoomGUI.doubleZoomMode:
                self.doubleZoomGUI.beginImgMotion(x, y, len(img[0]), len(img), (0,0), ax)

            x = int(round(x))
            y = int(round(y))
            unit = "px"
            if self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
                if 'center' in self.calSettings and self.calSettings['center'] is not None:
                    center = self.calSettings['center']
                else:
                    center = (self.projProc.info['centerx'], self.projProc.info['centery'])
                q, unit = inverseNmFromCenter([x, y], center, self.calSettings['scale'])
                # constant = self.calSettings["silverB"] * self.calSettings["radius"]
                # calib_distance = mouse_distance * 1.0/constant
                # calib_distance = f"{calib_distance:.4f}"
            if x < img.shape[1] and y < img.shape[0]:
                if self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
                    self.pixel_detail.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x])+ ", distance=" + str(q) + unit)
                else:
                    mouse_distance = np.sqrt((self.projProc.info['centerx'] - x) ** 2 + (self.projProc.info['centery'] - y) ** 2)
                    mouse_distance = f"{mouse_distance:.4f}"
                    self.pixel_detail.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]) + ", distance=" + str(mouse_distance) + unit)

                self.doubleZoomGUI.mouseHoverBehavior(x, y, img, self.displayImgCanvas, self.doubleZoom.isChecked())

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.pixel_detail.setText("")
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
        if func[0] == "im_zoomin" and len(self.function) == 1 and self.doubleZoom.isChecked():
            if not self.doubleZoomGUI.doubleZoomMode:
                self.doubleZoomGUI.updateAxes(x, y)
                self.displayImgCanvas.draw_idle()
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
            self.displayImgCanvas.draw_idle()
        elif func[0] == 'box':
            if len(func) == 1:
                # cross lines
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                    for line in list(ax.lines):
                        if line.get_label() != "Blue Dot":
                            line.remove()
                    ax.axhline(y, color='y', linestyle='dotted')
                    ax.axvline(x, color='y', linestyle='dotted')
                else:
                    self.doubleZoomGUI.updateAxes(x,y)
                self.displayImgCanvas.draw_idle()
            elif len(func) == 2:
                # draw rectangle
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
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
                else:
                    self.doubleZoomGUI.updateAxes(x,y)
                self.displayImgCanvas.draw_idle()

        elif func[0] == 'oriented_box' or func[0] == 'center_oriented_box':
            if len(func) == 1:
                axis_size = 5
                for line in list(ax.lines):
                    if line.get_label() != "Blue Dot":
                        line.remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)
                    """
                    if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                        if len(ax.lines) > 0:
                            for i in range(len(ax.lines)-1,-1,-1):
                                ax.lines[i].remove()
                        ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                        """
                self.displayImgCanvas.draw_idle()
            if len(func) == 2:
                # draw line as angle
                pivot = func[1]
                deltax = x - pivot[0]
                deltay = y - pivot[1]
                x2 = pivot[0] - deltax
                y2 = pivot[1] - deltay
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                    for line in list(ax.lines):
                        if line.get_label() != "Blue Dot":
                            line.remove()
                    ax.plot([x, x2], [y, y2], color="r")
                else:
                    self.doubleZoomGUI.updateAxes(x, y)
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

                    if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode: 
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
                    else:
                        self.doubleZoomGUI.updateAxes(x, y)
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

        elif func[0] == "perp_center":
            # draw X on points and a line between points
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
                        ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)

            elif len(func) % 2 != 0:
                if len(ax.lines) > 0:
                    n = (len(func)-1)*5//2 + 2
                    for i in range(len(ax.lines) - 1, n - 1, -1):
                        ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)
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
                    self.doubleZoomGUI.updateAxes(x, y)
            self.displayImgCanvas.draw_idle()

        elif func[0] == "chords_center":
            if self.doubleZoom.isChecked():
                self.doubleZoomGUI.updateAxes(x, y)
            self.displayImgCanvas.draw_idle()

        elif func[0] == "angle_center":
            # draw X on points and a line between points
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
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
                    for i in range(len(ax.lines)-1,1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                    # ax.lines = first_cross
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)
            self.displayImgCanvas.draw_idle()
        
        elif func[0] == "im_rotate":
            # draw line as angle
            center = self.projProc.info['orig_center']
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            if not self.doubleZoom.isChecked():
                for i in range(len(ax.lines)-1,-1,-1):
                    if ax.lines[i].get_label() != "Blue Dot":
                        ax.lines[i].remove()
                ax.plot([x, x2], [y, y2], color="g")
            else:
                if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                    self.doubleZoomGUI.updateAxesInner(x, y)
                elif self.doubleZoomGUI.doubleZoomMode:
                    for i in range(len(ax.lines)-1,-1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                    ax.plot([x, x2], [y, y2], color="g")
            self.displayImgCanvas.draw_idle()

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
        self.pixel_detail.setText("")

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

    def browseFile(self):
        """
        Popup an input file dialog. Users can select an image or .txt for failed cases list
        """
        file_name = getAFile()
        if file_name != "":
            self.onImageSelect(file_name)

    def onImageSelect(self, fullfilename):
        """
        Triggered when a new image is selected
        :param fullfilename: path for the image selected
        :return:
        """
        self.dir_path, self.imgList, self.current_file, self.fileList, self.ext = getImgFiles(fullfilename)
        if self.dir_path is not None and self.imgList is not None and self.imgList:
            self.propGrp.setEnabled(True)
            self.boxGrp.setEnabled(True)
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
                self.center_func = cache['center_func']
                for name, box in self.allboxes.items():
                    self.boxes_on_img[name] = self.genBoxArtists(name, box, self.boxtypes[name])
            else:
                self.allboxes = {}
                self.peaks = {}
            self.csvManager = PT_CSVManager(self.dir_path, self.allboxes, self.peaks)
            self.addBoxTabs()
            self.selectPeaksGrp.setEnabled(False)
            self.launchCalibrationSettings()
            self.setH5Mode(fullfilename)
            self.onImageChanged()

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new ProjectionProcessor object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        try:
            self.projProc = ProjectionProcessor(self.dir_path, self.imgList[self.current_file], self.fileList, self.ext)
        except Exception as e:
            infMsg = QMessageBox()
            infMsg.setText("Error")
            infMsg.setInformativeText(str(e))
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
        # self.initSpinBoxes(self.projProc.info)
        self.initMinMaxIntensities(self.projProc)
        self.img_zoom = None
        self.refreshStatusbar()
        if self.center_func is None:
            self.center_func = 'init'
        self.updateCenter() # do not update fit results
        # Process new image
        self.processImage()

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
        if 'blank_mask' in self.projProc.info:
            self.blankImageGrp.setChecked(self.projProc.info['blank_mask'])
        self.syncUI = False

    def updateCenter(self, refit=False):
        """
        Update the image center
        :return:
        """
        if self.center_func == 'automatic':
            self.projProc.orig_img, center = processImageForIntCenter(self.projProc.orig_img, getCenter(self.projProc.orig_img))
            self.centerx, self.centery = center
            if self.qfChkBx.isChecked():
                self.qfChkBx.stateChanged.disconnect(self.qfChkBxClicked) # Avoid second runs at launch
                self.qfChkBx.setChecked(False)
                self.qfChkBx.stateChanged.connect(self.qfChkBxClicked)
        elif self.center_func == 'quadrant_fold': # default to quadrant folded
            self.centerx = self.projProc.orig_img.shape[1] / 2. - 0.5
            self.centery = self.projProc.orig_img.shape[0] / 2. - 0.5
        elif self.center_func == 'init': # loading from the cache if it exists
            self.centerx = self.projProc.info['centerx']
            self.centery = self.projProc.info['centery']
            if self.centerx != self.projProc.orig_img.shape[1] / 2. - 0.5 and \
                self.centery != self.projProc.orig_img.shape[0] / 2. - 0.5:
                self.qfChkBx.stateChanged.disconnect(self.qfChkBxClicked)
                self.qfChkBx.setChecked(False)
                self.qfChkBx.stateChanged.connect(self.qfChkBxClicked)
        elif self.center_func == 'manual':
            self.qfChkBx.stateChanged.disconnect(self.qfChkBxClicked)
            self.qfChkBx.setChecked(False)
            self.qfChkBx.stateChanged.connect(self.qfChkBxClicked)

        self.projProc.info['centerx'] = self.centerx
        self.projProc.info['centery'] = self.centery

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
        print(self.projProc.info['centerx'], self.projProc.info['centery'])
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
        params = ProjectionParams(self.dir_path, self.imgList[i], self.fileList, self.ext, self.getSettings())
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
            'center_func' : self.center_func,
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

        # add blank image and mask
        settings['blank_mask'] = self.blankImageGrp.isChecked()

        settings['mask_thres'] = self.maskThresSpnBx.value()

        if self.refit:
            settings['refit'] = self.refit
            self.refit = False

        if self.center_func == 'manual' or self.rotated:
            settings['rotated'] = True
            self.rotated = True
            if self.rotationAngle != 0:
                settings['rotationAngle'] = self.rotationAngle

        if self.calSettings is not None:
            if 'type' in self.calSettings:
                if self.calSettings["type"] == "img":
                    settings["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
                elif self.calSettings["type"] == "cont":
                    settings["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings["pixel_size"]
            if "center" in self.calSettings and self.center_func != 'manual':
                settings["center"] = self.calSettings["center"]
                self.projProc.info['orig_center'] = self.calSettings["center"]
                self.projProc.info['centery'] = self.calSettings["center"][1]
                self.projProc.info['centerx'] = self.calSettings["center"][0]
            else:
                if 'centerx' in self.projProc.info and self.center_func != 'manual':
                    del self.projProc.info['centerx']
                if 'centery' in self.projProc.info and self.center_func != 'manual':
                    del self.projProc.info['centery']
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
            self.right_status.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype))
        elif self.calSettings is not None and self.calSettings:
            self.right_status.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype) + " " + "(Image Calibrated)")
        self.pixel_detail.setText("")
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
        Draw all UI in image tab
        """

        if self.projProc is None or self.syncUI or not self.update_plot['img']:
            return
        if self.rotated:
            img = self.projProc.getRotatedImage()
        else:
            img = self.projProc.orig_img
        img = np.flipud(img)
        img = get8bitImage(copy.copy(img), min=self.minIntSpnBx.value(), max=self.maxIntSpnBx.value())
        ax = self.displayImgAxes
        ax.cla()
        if self.logScaleIntChkBx.isChecked():
            ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.minIntSpnBx.value()), vmax=self.maxIntSpnBx.value()))
        else:
            ax.imshow(getBGR(img))
        
        if len(self.allboxes.keys()) > 0:
            self.selectPeaksGrp.setEnabled(True)
            if self.boxesChkBx.isChecked():
                for name, aritists in self.boxes_on_img.items():
                    ax.add_patch(aritists['rect'])
                    ax.add_artist(aritists['text'])

            if self.peaksChkBx.isChecked():
                for name in self.peaks.keys():
                    centerx = self.projProc.info['centerx']
                    centery = self.projProc.info['centery']
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

        if self.centerChkBx.isChecked():
            centerx = self.projProc.info['centerx']
            centery = self.projProc.info['centery']
            circle = plt.Circle((centerx, centery), 10, color='g')
            ax.add_patch(circle)

        # Zoom
        if self.img_zoom is not None and len(self.img_zoom) == 2:
            ax.set_xlim(self.img_zoom[0])
            ax.set_ylim(self.img_zoom[1])
        else:
            ax.set_xlim((0, img.shape[1]))
            ax.set_ylim((0, img.shape[0]))

        self.calSettingsDialog.centerX.setValue(self.projProc.info['centerx'])
        self.calSettingsDialog.centerY.setValue(self.projProc.info['centery'])

        self.img_zoom = [ax.get_xlim(), ax.get_ylim()]
        ax.invert_yaxis()
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
