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

import os
import time
import json
import collections
from os.path import join
from datetime import datetime
import gc
import copy
import pickle
import numpy as np
import cv2
import csv
import concurrent.futures
import hdf5plugin # for some reason this is needed even though never coded
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
from PIL import Image
import fabio
#from .image_masker import image_masker

from .AISEImageSelectionWindow import AISEImageSelectionWindow
from .UnalignedImagesDialog import UnalignedImagesDialog
from .ImageMaskTool import ImageMaskerWindow
from .CalibrationDialog import CalibrationDialog
from musclex import __version__
from .pyqt_utils import *
from ..utils.file_manager import ifHdfReadConvertless, createFolder, getFilesAndHdf, fullPath, getBlankImageAndMask, getMaskOnly, validateImage, isImg
from ..utils.image_processor import calcSlope, getIntersectionOfTwoLines, getPerpendicularLineHomogenous, processImageForIntCenter, getRotationAngle, getCenter, getNewZoom, rotateImage, averageImages
from ..CalibrationSettings import CalibrationSettings
from ..utils.detect_unaligned_images import *

class SelectionWindow(QWidget):
    closed = Signal(list)  # Signal to emit when the window is closed
    
    def __init__(self, files):
        super().__init__()
        self.files = files
        self.selected_files = []
        self.initUI()
        
    def initUI(self):
        layout = QVBoxLayout()
        self.list_widget = QListWidget()
        self.list_widget.setSelectionMode(QListWidget.MultiSelection)
        for file in self.files:
            print(file)
            QListWidgetItem(file, self.list_widget)
            
        self.confirmButton = QPushButton("Confirm")
        layout.addWidget(self.list_widget)
        layout.addWidget(self.confirmButton)
        self.confirmButton.clicked.connect(self.saveSelections)
        self.setLayout(layout)
        self.setWindowTitle("Select Experiments")
        # self.show()
        
    def saveSelections(self):
        self.selected_files = [(self.list_widget.row(item), item.text()) for item in self.list_widget.selectedItems()]
        if len(self.selected_files) < 2:
            QMessageBox.critical(self, "Error", "Please select at least 2 files.")
        else:
            self.close()

    def closeEvent(self, event):
        self.closed.emit(self.selected_files)  # Emit the signal with the selected files
        event.accept()  # Accept the close event

class AIStartWindow(QWidget):
    def __init__(self):
        super().__init__()
        self.initUI()

    def initUI(self):
        layout = QGridLayout()
        self.aime = QCheckBox("Add Intensities Multiple Experiments")
        self.aise = QCheckBox("Add Intensities Single Experiment")
        
        # Create a button group and add checkboxes to it
        self.button_group = QButtonGroup()
        self.button_group.addButton(self.aime)
        self.button_group.addButton(self.aise)
        self.button_group.setExclusive(True)  # Make checkboxes exclusive
        
        button = QPushButton("Open Main Window")
        button.clicked.connect(self.openMainWindow)
        layout.addWidget(self.aime, 0, 0)
        layout.addWidget(self.aise, 0, 1)
        layout.addWidget(button, 1, 0, 1, 2)
        self.setLayout(layout)
        self.setWindowTitle('Select Experiment Type')
        self.show()

    def openMainWindow(self):
        checked_boxes = {
            'aime': self.aime.isChecked(),
            'aise': self.aise.isChecked()
        }
        self.main_window = AddIntensitiesExp(checked_boxes)
        self.main_window.show()
        self.close()
        
class AddIntensitiesExp(QMainWindow):
    def __init__(self, checked_boxes):
        super().__init__()
        self.checked_boxes = checked_boxes
        if self.checked_boxes['aime'] == True:
            self.mode = 'aime'
            self.fileList = []
            self.numberToFilesMap = None
            self.orig_img_names = []
            
            self.experimentSelectionDialog = None
            self.customSelection = False
            self.nbOfExposures = 2
            self.imageAxes2 = None
            self.imageAxes3 = None
            self.imageAxes4 = None
            self.imageAxes5 = None
            self.imageAxes6 = None
            self.imageAxes7 = None
            self.imageAxes8 = None
            self.axClicked = None
            self.index = 0
            
            self.info = {'manual_rotationAngle': [None] * 8,
                        'rotationAngle': [0] * 8,
                        'center': [None] * 8,
                        'manual_center': [None] * 8}
            self.orig_image_center = None
        else:
            self.mode = 'aise'
            self.img_list = []
            self.img_grps = []
            self.img_grps_copy = []
            
            self.file_name = '' 
            self.orig_avg_img = None
            self.currentFrameNumber = 0
            self.currentGroupNumber = 0
            self.imageSequenceDialog = None
            
            self.customImageSequence = False  # If we are using a custom sequence by the user himself
            self.nbOfFrames = 2 # This controls binning factor 
            self.nbOfGroups = 1
            
            
            self.info = {}
            self.orig_image_center = None
            self.maskData = None
            self.blankImageSettings = None
            self.drawnMask = False
            self.computedMask = False
            self.csvInfo = []
            self.calibrationDialog = None
        
        # Shared Variables
        self.orig_imgs = []
        self.init_imgs = None
        self.sum_img = None # AISE names this avg_img
        self.updated = {'img' : False, 'res': False}
        self.uiUpdating = False # update ui status flag (prevent recursive)
        self.calSettings = None
        self.calSettingsDialog = None
        self.dir_path = ""
        self.stop_process = False
        self.unalignedImagesDialog = None
        self.imageAxes = None
        self.resultAxes = None
        
        self.imageMaskingTool = None
        self.misaligned_images = []
        self.function = None
        self.currentFileNumber = 0
        self.center_before_rotation = None
        self.img_zoom = None # zoom location of original image (x,y range)
        self.default_img_zoom = None # default zoom calculated after processing image
        self.doubleZoomMode = False
        self.dontShowAgainDoubleZoomMessageResult = False
        self.doubleZoomPt = (0, 0)
        self.doubleZoomAxes = None
        self.isHdf5 = False
        self.chordLines = []      
        
        self.initUI()
        self.setConnections()

    def initUI(self):
        if self.mode == 'aime':
            self.setWindowTitle("Muscle X Add Intensities Multiple Experiment v." + __version__)
        else:
            self.setWindowTitle("Muscle X Add Intensities Single Experiment v." + __version__)
            
        """
        Initialize the UI.
        """
        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.centralWidget = QWidget(self)

        self.scrollArea.setWidget(self.centralWidget)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.scrollArea)
        
        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 200px; }")
        self.mainLayout.addWidget(self.tabWidget)

        ##### Image Tab #####
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.tabWidget.addTab(self.imageTab, "Original Images")

        ## display browse folder buttons when program started
        self.verImgLayout = QVBoxLayout()
        self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)

        self.browseFileButton = QPushButton("Select an H5 File...")
        self.verImgLayout.addWidget(self.browseFileButton)
        self.browseFileButton.setFixedHeight(100)
        self.browseFileButton.setFixedWidth(300)

        self.browseFolderButton = QPushButton("Select a Folder...")
        self.verImgLayout.addWidget(self.browseFolderButton)
        self.browseFolderButton.setFixedHeight(100)
        self.browseFolderButton.setFixedWidth(300)

        self.imageFigure = plt.figure()
        # self.imageAxes = self.imageFigure.add_subplot(111)
        self.imageCanvas = FigureCanvas(self.imageFigure)

        self.imageCanvas.setHidden(True)
        self.imageTabLayout.addLayout(self.verImgLayout)
        self.imageTabLayout.addWidget(self.imageCanvas)

        # Options Menu on the Right

        # Main Layout to add GrpBoxes To
        self.optionsLayout = QVBoxLayout()
        self.optionsLayout.setAlignment(Qt.AlignCenter)

        # Display Options
        self.displayOptGrpBx = QGroupBox()
        self.displayOptGrpBx.setTitle("Display Options")
        self.displayOptGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self.displayOptGrpBx.setStyleSheet("QGroupBox {font-weight: bold;}")
        self.dispOptLayout = QGridLayout()

        self.spminInt = QDoubleSpinBox()
        self.spminInt.setToolTip("Reduction in the maximal intensity shown \
            to allow for more details in the image.")
        self.spminInt.setKeyboardTracking(False)
        self.spminInt.setSingleStep(5)
        self.spminInt.setDecimals(0)
        self.spmaxInt = QDoubleSpinBox()
        self.spmaxInt.setToolTip("Increase in the minimal intensity shown \
            to allow for more details in the image.")
        self.spmaxInt.setKeyboardTracking(False)
        self.spmaxInt.setSingleStep(5)
        self.spmaxInt.setDecimals(0)
        self.logScaleIntChkBx = QCheckBox("Log scale intensity")
        self.persistIntensity = QCheckBox("Persist intensities")

        self.imgZoomInB = QPushButton("Zoom in")
        self.imgZoomInB.setCheckable(True)
        self.imgZoomOutB = QPushButton("Full")
        
        self.doubleZoom = QCheckBox("Double Zoom")
        self.dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')
        self.showSeparator = QCheckBox("Show Quadrant Separator")
        self.dispOptLayout.addWidget(self.minIntLabel, 1, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spminInt, 1, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntLabel, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spmaxInt, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomInB, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 3, 1, 1, 1)
        self.dispOptLayout.addWidget(self.logScaleIntChkBx, 4, 0, 1, 2)
        self.dispOptLayout.addWidget(self.persistIntensity, 5, 0, 1, 2)
        self.dispOptLayout.addWidget(self.doubleZoom, 6, 0, 1, 2)
        self.dispOptLayout.addWidget(self.showSeparator, 7, 0, 1, 2)
        self.displayOptGrpBx.setLayout(self.dispOptLayout)

        # Image Operations Group Box

        self.imgOperationGrp = QGroupBox("Image Operations")
        self.imgOperationGrp.setStyleSheet("QGroupBox {font-weight: bold;}")
        self.imgOperationLayout = QGridLayout()
        self.imgOperationGrp.setLayout(self.imgOperationLayout)
        
        self.frameNb = QSpinBox()
        self.frameNb.setKeyboardTracking(False)
        self.frameNb.setValue(2)
        self.frameNb.setMinimum(2)
        
        if self.mode == 'aise':
            self.factorBx = QCheckBox('Binning Factor')
            self.factorBx.setChecked(True)
            self.frameNb.setToolTip("Choose the number of frames per group you would like.")
            self.selectImageChkBx = QCheckBox('Sum Image Ranges')
            self.sumImagesButton = QPushButton("Select Image Subsets")
        else:
            self.frameNb.setToolTip("Choose the number of exposures (images) you would like to add.")
            self.frameNb.setMaximum(8)
            self.factorBx = QLabel('Experiment Runs')
            self.selectImageChkBx = QCheckBox("Custom Sequence")
            self.sumImagesButton = QPushButton("Select Experiments")
            
        
        self.sumImagesButton.setEnabled(False)
        self.useMaskChkBx = QCheckBox('Use Mask')
        self.useMaskChkBx.setEnabled(False)
        self.maskImageButton = QPushButton("Specify Empty Cell and Mask Images")
        self.maskImageButton.setEnabled(False)
        self.calibrationChkBx = QCheckBox("Align Images")
        self.specifyCenterAndOrientationButton = QPushButton("Correct Center and Orientation")
        self.specifyCenterAndOrientationButton.setEnabled(False)
        
        self.checkImagesButton = QPushButton("Detect Misaligned Images")
        self.checkImagesButton.setToolTip("Check all images for misalignment")
        self.checkImagesButton.setEnabled(False)
        
        self.imgOperationLayout.addWidget(self.factorBx, 0, 0, 1, 2)
        self.imgOperationLayout.addWidget(self.frameNb, 0, 3, 1, 2)
        self.imgOperationLayout.addWidget(self.selectImageChkBx, 1, 0, 1, 2)
        self.imgOperationLayout.addWidget(self.sumImagesButton, 1, 3, 1, 1)
        self.imgOperationLayout.addWidget(self.useMaskChkBx, 2, 0, 1, 2)
        self.imgOperationLayout.addWidget(self.maskImageButton, 3, 0, 1, 4)
        self.imgOperationLayout.addWidget(self.calibrationChkBx, 4, 0, 1, 2)
        self.imgOperationLayout.addWidget(self.specifyCenterAndOrientationButton, 5, 0, 1, 4)
        self.imgOperationLayout.addWidget(self.checkImagesButton, 6, 0, 1, 4)
        
        # Operation Options Group Box

        self.operationGroup = QGroupBox("Operation Options")
        self.operationGroup.setStyleSheet("QGroupBox {font-weight: bold;}")
        self.operationGroupLayout = QGridLayout()
        self.operationGroup.setLayout(self.operationGroupLayout)

        self.avgInsteadOfSum = QCheckBox("Compute Average Instead of Sum")
        self.compressChkBx = QCheckBox('Compress the Resulting Images')

        self.operationGroupLayout.addWidget(self.avgInsteadOfSum, 1, 0, 1, 4)
        self.operationGroupLayout.addWidget(self.compressChkBx, 2, 0, 1, 4)
        
        # if self.mode == 'aime':
        #     self.calibrationChkBx = QGroupBox("Calibrate images")
        #     self.calibrationChkBx.setCheckable(True)
        #     self.calibrationChkBx.setChecked(False)
        #     self.calibrationLayout = QGridLayout(self.calibrationChkBx)
        #     self.calibrationButton = QPushButton("Calibration Settings")
        #     self.setCenterRotationButton = QPushButton("Set Manual Center and Rotation")
        #     self.setCenterRotationButton.setCheckable(True)
        #     self.setCentByChords = QPushButton("Set Center by Chords")
        #     self.setCentByChords.setCheckable(True)
        #     self.setCentByPerp = QPushButton("Set Center by Perpendiculars")
        #     self.setCentByPerp.setCheckable(True)
        #     self.setRotationButton = QPushButton("Set Manual Rotation")
        #     self.setRotationButton.setCheckable(True)
        #     self.setFitRegion = QPushButton("Set Region of Interest")
        #     self.setFitRegion.setCheckable(True)

            
        #     self.calibrationLayout.addWidget(self.calibrationButton, 1, 0, 1, 2)
        #     self.calibrationLayout.addWidget(self.setCenterRotationButton, 2, 0, 1, 2)
        #     self.calibrationLayout.addWidget(self.setRotationButton, 3, 0, 1, 2)
        #     self.calibrationLayout.addWidget(self.setCentByChords, 4, 0, 1, 2)
        #     self.calibrationLayout.addWidget(self.setCentByPerp, 5, 0, 1, 2)
        #     self.calibrationLayout.addWidget(self.setFitRegion, 6, 0, 1, 2)
        #     self.calibrationLayout.addWidget(self.centerWoRotateChkBx, 7, 0, 1, 2)
        #     self.calibrationLayout.addWidget(self.showSeparator, 8, 0, 1, 2)

        #     self.calibrationButton.setEnabled(False)
        #     self.setCenterRotationButton.setEnabled(False)
        #     self.setRotationButton.setEnabled(False)
        #     self.setCentByChords.setEnabled(False)
        #     self.setCentByPerp.setEnabled(False)
        #     self.setFitRegion.setEnabled(False)
        #     self.showSeparator.setEnabled(False)
        #     self.centerWoRotateChkBx.setEnabled(False)

        # Review Images Group
        self.reviewImageGroup = QGroupBox("Review Images")
        self.reviewImageGroup.setStyleSheet("QGroupBox {font-weight: bold;}")
        self.reviewImageLayout = QGridLayout()
        self.reviewImageGroup.setLayout(self.reviewImageLayout)

        if self.mode == 'aise':

            self.frameSteppingSelection = QComboBox()
            self.frameSteppingSelection.addItems(['Step through selected images', 'Step through all images', 'Step through badly correlated images'])
            self.nextButton = QPushButton("Frame >")
            self.prevButton = QPushButton("< Frame")

            self.filenameLineEdit = QSpinBox()
            self.filenameLineEdit.setMinimum(1)
            self.filenameLineEdit.setKeyboardTracking(False)
            self.filenameLineEdit.setVisible(False)
            
            self.reviewImageLayout.addWidget(self.frameSteppingSelection, 0, 0, 1, 2)
            self.reviewImageLayout.addWidget(self.filenameLineEdit, 1, 0, 1, 1) 
            self.reviewImageLayout.addWidget(self.prevButton, 2, 0, 1, 1)
            self.reviewImageLayout.addWidget(self.nextButton, 2, 1, 1, 1)
            
        else:
            self.nextButton = QPushButton(">>>")
            self.prevButton = QPushButton("<<<")
            self.filenameLineEdit = QSpinBox()
            self.filenameLineEdit.setMinimum(0)
            self.filenameLineEdit.setKeyboardTracking(False)
            self.reviewImageLayout.addWidget(self.prevButton, 1, 0, 1, 1)
            self.reviewImageLayout.addWidget(self.nextButton, 1, 1, 1, 1)
            self.reviewImageLayout.addWidget(QLabel('Images #'), 2, 0, 1, 1)
            self.reviewImageLayout.addWidget(self.filenameLineEdit, 2, 1, 1, 1)
        
        self.processFolderButton = QPushButton("Sum Images")
        self.processFolderButton.setStyleSheet(pfss)
        self.processFolderButton.setCheckable(True)

        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addSpacing(5)
        self.optionsLayout.addWidget(self.imgOperationGrp)
        self.optionsLayout.addSpacing(5)
        self.optionsLayout.addWidget(self.operationGroup)
        self.optionsLayout.addSpacing(5)
        self.optionsLayout.addWidget(self.reviewImageGroup)
        self.optionsLayout.addSpacing(5)
        self.optionsLayout.addWidget(self.processFolderButton)

        self.frameOfKeys = QFrame()
        self.frameOfKeys.setFixedWidth(350)
        self.frameOfKeys.setLayout(self.optionsLayout)
        self.imageTabLayout.addWidget(self.frameOfKeys)

        ##### Result Tab #####
        self.resultTab = QWidget()
        self.resultTab.setContentsMargins(0, 0, 0, 0)
        self.resultTabLayout = QHBoxLayout(self.resultTab)
        self.tabWidget.addTab(self.resultTab, "Results")

        self.resultFigure = plt.figure()
        self.resultAxes = self.resultFigure.add_subplot(111)
        self.resultVLayout = QVBoxLayout()
        self.resultCanvas = FigureCanvas(self.resultFigure)
        self.resultTabLayout.addWidget(self.resultCanvas)

        self.leftLayout = QVBoxLayout()
        self.leftFrame = QFrame()
        self.leftFrame.setFixedWidth(300)
        self.leftFrame.setLayout(self.leftLayout)
        self.resultTabLayout.addWidget(self.leftFrame)

        # Display Options
        self.resultDispOptGrp = QGroupBox()
        self.resultDispOptGrp.setTitle("Display Options")
        self.resultDispOptGrp.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self.resultDispOptLayout = QGridLayout()

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

        self.resultminIntLabel = QLabel("Min intensity : ")
        self.resultmaxIntLabel = QLabel("Max intensity : ")
        self.resLogScaleIntChkBx = QCheckBox("Log scale intensity")
        self.resPersistIntensity = QCheckBox("Persist intensities")

        self.resultDispOptLayout.addWidget(self.resultminIntLabel, 1, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.spResultminInt, 1, 1, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultmaxIntLabel, 2, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.spResultmaxInt, 2, 1, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultZoomInB, 3, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultZoomOutB, 3, 1, 1, 1)
        self.resultDispOptLayout.addWidget(self.resLogScaleIntChkBx, 4, 0, 1, 2)
        self.resultDispOptLayout.addWidget(self.resPersistIntensity, 5, 0, 1, 2)

        self.resultDispOptGrp.setLayout(self.resultDispOptLayout)

        self.leftLayout.addWidget(self.resultDispOptGrp)
        self.leftLayout.addStretch()

        if self.mode == 'aise':
            self.processFolderButton2 = QPushButton("Process All Groups")
            self.nextGrpButton2 = QPushButton("Group >>>")
            self.prevGrpButton2 = QPushButton("<<< Group")
            self.resultLabel = QLabel("Group #")
        else:
            self.processFolderButton2 = QPushButton("Process Current Folder")
            self.nextGrpButton2 = QPushButton(">>>")
            self.prevGrpButton2 = QPushButton("<<<")
            self.resultLabel = QLabel("Image #")
        self.processFolderButton2.setStyleSheet(pfss)
        self.processFolderButton2.setCheckable(True)
        self.processFolderButton2.setVisible(False)
        self.filenameLineEdit2 = QSpinBox()
        self.filenameLineEdit2.setMinimum(0)
        self.filenameLineEdit2.setKeyboardTracking(False)
        self.buttonsLayout2 = QGridLayout()
        self.buttonsLayout2.addWidget(self.processFolderButton2, 0, 0, 1, 2)
        self.buttonsLayout2.addWidget(self.prevGrpButton2, 1, 0, 1, 1)
        self.buttonsLayout2.addWidget(self.nextGrpButton2, 1, 1, 1, 1)
        self.buttonsLayout2.addWidget(self.resultLabel, 2, 0, 1, 1)
        self.buttonsLayout2.addWidget(self.filenameLineEdit2, 2, 1, 1, 1)
        self.leftLayout.addLayout(self.buttonsLayout2)

        #### Status bar #####
        self.statusBar = QStatusBar()
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusReport = QLabel()
        self.imgDetailOnStatusBar = QLabel()
        self.imgCoordOnStatusBar = QLabel()
        self.imgPathOnStatusBar = QLabel("  Please select a folder to process")
        self.statusBar.addPermanentWidget(self.statusReport)
        self.statusBar.addPermanentWidget(self.imgCoordOnStatusBar)
        self.statusBar.addPermanentWidget(self.imgDetailOnStatusBar)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.statusBar.addWidget(QLabel("    "))
        self.statusBar.addWidget(self.imgPathOnStatusBar)
        self.setStatusBar(self.statusBar)

        # Menubar
        if self.mode == 'aise':
            selectFileAction = QAction('Select an H5 File...', self)
            selectFileAction.setShortcut('Ctrl+H')
            selectFileAction.triggered.connect(self.browseFile)
        selectFolderAction = QAction('Select a Folder...', self)
        selectFolderAction.setShortcut('Ctrl+F')
        selectFolderAction.triggered.connect(self.browseFolder)
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectFolderAction)
        aboutAct = QAction('About', self)
        aboutAct.triggered.connect(self.showAbout)
        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)
        
        # AISE/AIME Differences here
        if self.mode == 'aime':
            self.browseFileButton.setVisible(False)
            self.compressChkBx.setVisible(False)
        else:
            self.imageAxes = self.imageFigure.add_subplot(111)
        

        self.show()
        # self.setMinimumHeight(900)
        # self.setMinimumWidth(1500)
        self.resize(1500, 900)
        
    def setConnections(self):
        self.browseFileButton.clicked.connect(self.browseFile)
        self.browseFolderButton.clicked.connect(self.browseFolder)
        self.imgZoomInB.clicked.connect(self.imageZoomIn)
        self.imgZoomOutB.clicked.connect(self.imageZoomOut)
        self.spminInt.valueChanged.connect(self.refreshImageTab)
        self.spmaxInt.valueChanged.connect(self.refreshImageTab)
        self.logScaleIntChkBx.stateChanged.connect(self.refreshImageTab)
        self.doubleZoom.stateChanged.connect(self.doubleZoomChecked)
        self.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.nextButton.clicked.connect(self.nextClicked)
        self.prevButton.clicked.connect(self.prevClicked)
        self.avgInsteadOfSum.stateChanged.connect(self.avgInsteadOfSumChanged)
        self.compressChkBx.stateChanged.connect(self.avgInsteadOfSumChanged)
        self.calibrationChkBx.clicked.connect(self.setCalibrationActive)
        self.checkImagesButton.clicked.connect(self.checkImages)
        self.showSeparator.stateChanged.connect(self.refreshImageTab)
        self.selectImageChkBx.clicked.connect(self.selectImageChecked)
        self.sumImagesButton.clicked.connect(self.selectImageSequence)
        self.maskImageButton.clicked.connect(self.maskImageButtonClicked)
        self.specifyCenterAndOrientationButton.clicked.connect(self.specifyCenterAndOrientationClicked)
        if self.mode == 'aise':
            self.factorBx.clicked.connect(self.binFactorChecked)
            self.frameNb.valueChanged.connect(self.frameNbChanged)
            self.frameSteppingSelection.currentTextChanged.connect(self.stepComboBoxChanged)
        else:
            self.filenameLineEdit.valueChanged.connect(self.fileNameChanged)
            self.frameNb.valueChanged.connect(self.exposureNbChanged)
            
        self.imageFigure.canvas.mpl_connect('button_press_event', self.imageClicked)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)
        self.imageFigure.canvas.mpl_connect('button_release_event', self.imageReleased)
        self.imageFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)
        
        self.resultZoomInB.clicked.connect(self.resultZoomIn)
        self.resultZoomOutB.clicked.connect(self.imageZoomOut)
        self.spResultminInt.valueChanged.connect(self.refreshResultTab)
        self.spResultmaxInt.valueChanged.connect(self.refreshResultTab)
        self.resLogScaleIntChkBx.stateChanged.connect(self.refreshResultTab)
        self.processFolderButton2.toggled.connect(self.batchProcBtnToggled)
        self.nextGrpButton2.clicked.connect(self.nextClicked)
        self.prevGrpButton2.clicked.connect(self.prevClicked)
        self.filenameLineEdit2.valueChanged.connect(self.fileName2Changed)
        
        self.resultFigure.canvas.mpl_connect('button_press_event', self.resultClicked)
        self.resultFigure.canvas.mpl_connect('motion_notify_event', self.resultOnMotion)
        self.resultFigure.canvas.mpl_connect('button_release_event', self.resultReleased)
        self.resultFigure.canvas.mpl_connect('scroll_event', self.resultScrolled)
        
            
    def drawPerpendiculars(self, ax):
        """
        Draw perpendiculars on the image
        """
        points = self.function[1:]
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
        
    def setCalibrationActive(self):
        """
        Checkbox to decide if we want to calibrate the images or not
        """
        if self.mode == 'aise':
            self.info = {'manual_rotationAngle': [None] * self.nbOfFrames,
                'rotationAngle': [0] * self.nbOfFrames,
                'center': [None] * self.nbOfFrames,
                'manual_center': [None] * self.nbOfFrames}
            if self.calibrationChkBx.isChecked():
                self.specifyCenterAndOrientationButton.setEnabled(True)
                _, self.orig_image_center = self.getExtentAndCenter(self.orig_imgs[0])
            else:
                self.specifyCenterAndOrientationButton.setEnabled(False)
        else:
            if self.calibrationChkBx.isChecked():
                self.specifyCenterAndOrientationButton.setEnabled(True)
                _, self.orig_image_center = self.getExtentAndCenter(self.orig_imgs[0])
            else:
                self.info = {'manual_rotationAngle': [None] * 8,
                            'rotationAngle': [0] * 8,
                            'center': [None] * 8,
                            'manual_center': [None] * 8}
                self.specifyCenterAndOrientationButton.setEnabled(False)
            
    def batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        
        if (self.processFolderButton.isChecked() or self.processFolderButton2.isChecked()) and self.processFolderButton.text() == "Sum Images":
            if not self.progressBar.isVisible():
                self.processFolderButton.setText("Stop")
                self.processFolderButton.setChecked(True)
                self.processFolderButton2.setText("Stop")
                self.processFolderButton2.setChecked(True)
                self.processFolder()
        else:
            self.stop_process = True
        
    def processFolder(self):
        """
        Triggered when a folder has been selected to process it
        """
        errMsg = QMessageBox()
        errMsg.setText('Start')
        text = 'The current folder will be processed using current settings. \
            Make sure to adjust them before processing the folder. \n\n'
        text += ""
        if self.orig_image_center is not None:
            text += "\n - Center : " + str(self.orig_image_center)
        if self.mode == 'aise':
            text += '\n\nAre you sure you want to process ' + str(self.nbOfGroups) + ' groups of ' + str(self.nbOfFrames) + ' frames in this Folder? \nThis might take a long time.'
        else:
            text += '\n\nAre you sure you want to process ' + str(len(self.numberToFilesMap)) + ' exposures in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            self.progressBar.setVisible(True)
            self.stop_process = False
            if self.mode == 'aise':
                self.progressBar.setRange(0, self.nbOfGroups)
                output = os.path.join(self.dir_path, 'aise_results')

                if self.frameSteppingSelection.currentIndex() != 0:
                    self.img_grps = self.img_grps_copy

                print('Start merging...')
                for i, imgs in enumerate(self.img_grps):
                    if len(imgs) > 0 and not self.stop_process:
                        self.progressBar.setValue(i)
                        self.processGroup()
                        QApplication.processEvents()
                        self.nextGrpClicked()
                    else:
                        break
                
                self.progressBar.setValue(self.nbOfGroups)
                self.progressBar.setVisible(False)
                self.statusPrint("")
                print("writing to csv file..")
                self.writeToCSV()
                print("Done. All result images have been saved to "+output)
            else:
                for i in range(len(self.numberToFilesMap)):
                    if self.stop_process:
                        break
                    self.progressBar.setValue(int(100. / len(self.numberToFilesMap) * i))
                    QApplication.processEvents()
                    self.nextClicked()
                self.progressBar.setVisible(False)

        
        self.processFolderButton.setChecked(False)
        self.processFolderButton.setText("Sum Images")
        self.processFolderButton2.setChecked(False)
        self.processFolderButton2.setText("Sum Images")
        
    def browseFile(self):
        """
        Popup input dialog and set file selection
        """
        self.img_list = []
        file_name = getAFile(filtr='Pattern ( *.h5 *.hdf5)')
        if file_name != "":
            if file_name.split('.')[1] in ['h5', 'hdf5']:
                self.isHdf5 = True
                self.dir_path, _ = os.path.split(str(file_name))
                self.file_name = file_name
                try:
                    with fabio.open(file_name) as series:
                        for frame in series.frames():
                            namefile = os.path.split(frame.file_container.filename)[1].split('.')
                            temp_filename = namefile[0] + '_%05i.' %(frame.index + 1) + namefile[1]
                            self.img_list.append(temp_filename)
                    self.img_list.sort()
                    self.updateImageGroups()
                    self.onNewFileSelected()
                    self.refreshAllTab()
                except:
                    infMsg = QMessageBox()
                    infMsg.setText('Error opening file: ' + file_name)
                    infMsg.setInformativeText("File is not a valid HDF5 file or corrupted.")
                    infMsg.setStandardButtons(QMessageBox.Ok)
                    infMsg.setIcon(QMessageBox.Information)
                    infMsg.exec_()
            else:
                errMsg = QMessageBox()
                errMsg.setText('Wrong file type')
                msg = 'Please select a file ending with .h5 or .hdf5\n\n'
                msg += "Error : Unexpected file type"
                errMsg.setInformativeText(msg)
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.setFixedWidth(300)
                errMsg.exec_()
                print(msg)
                
    def nextClicked(self):
        """
        Going to the next image
        """
        if self.mode == 'aime':
            if len(self.numberToFilesMap) > 0:
                self.currentFileNumber = (self.currentFileNumber + 1) % len(self.numberToFilesMap)
                self.filenameLineEdit.setValue(self.currentFileNumber + 1)
        else:
            if len(self.orig_imgs) > 0:
                if self.img_list != [] and self.currentGroupNumber + 1 == self.nbOfGroups and len(self.img_list)%self.nbOfFrames != 0:
                    max_frame = len(self.img_list)%self.nbOfFrames
                else:
                    max_frame = self.nbOfFrames
                if self.frameSteppingSelection.currentIndex() == 0:
                    if self.currentFrameNumber + 1 >= max_frame:
                        self.nextGrpClicked()
                        return
                self.currentFrameNumber = (self.currentFrameNumber + 1) % max_frame
                self.currentFileNumber = self.currentFrameNumber + (self.currentGroupNumber) * self.nbOfFrames
                self.refreshImageTab()
                self.initialWidgets(self.orig_imgs[self.currentFrameNumber], self.sum_img)
    
    def prevClicked(self):
        """
        Going to the previous image
        """
        if self.mode == 'aime':
            if len(self.numberToFilesMap) > 0:
                self.currentFileNumber = (self.currentFileNumber - 1) % len(self.numberToFilesMap)
                self.filenameLineEdit.setValue(self.currentFileNumber + 1)
        else:
            if len(self.orig_imgs) > 0:
                if self.img_list != [] and self.currentGroupNumber + 1 == self.nbOfGroups and len(self.img_list)%self.nbOfFrames != 0:
                    max_frame = len(self.img_list)%self.nbOfFrames
                else:
                    max_frame = self.nbOfFrames
                if self.frameSteppingSelection.currentIndex() == 0:
                    if self.currentFrameNumber - 1 < 0:
                        self.prevGrpClicked()
                        return
                self.currentFrameNumber = (self.currentFrameNumber - 1) % max_frame
                self.currentFileNumber = self.currentFrameNumber + (self.currentGroupNumber) * self.nbOfFrames
                self.refreshImageTab()
                
    def prevGrpClicked(self):
        """
        Going to the previous image
        """
        if len(self.img_grps) > 0:
            self.currentGroupNumber = (self.currentGroupNumber - 1) % self.nbOfGroups
            self.currentFrameNumber = 0
            self.currentFileNumber = self.currentFrameNumber + (self.currentGroupNumber) * self.nbOfFrames
            self.filenameLineEdit.setValue(self.currentGroupNumber + 1)
            self.onGroupChanged()

    def nextGrpClicked(self):
        """
        Going to the next image
        """
        if len(self.img_grps) > 0:
            self.currentGroupNumber = (self.currentGroupNumber + 1) % self.nbOfGroups
            self.currentFrameNumber = 0
            self.currentFileNumber = self.currentFrameNumber + (self.currentGroupNumber) * self.nbOfFrames
            self.filenameLineEdit.setValue(self.currentGroupNumber + 1)
            self.onGroupChanged()
                
    def avgInsteadOfSumChanged(self):
        """
        Change the resulting image from sum if unchecked to avg if checked
        """
        self.onGroupChanged()
                
    def frameNbChanged(self):
        """
        Change the number of images to add. Will also change the display consequently
        """
        self.nbOfFrames = self.frameNb.value()
        self.currentFrameNumber = 0
        self.currentGroupNumber = 0
        self.currentFileNumber = 0
        if self.customImageSequence is False:
            self.updateImageGroups()
        self.nbOfGroups = len(self.img_grps)
        self.filenameLineEdit.setMaximum(self.nbOfGroups)
        self.filenameLineEdit2.setMaximum(self.nbOfGroups)
        self.info['manual_rotationAngle'] = [None] * self.nbOfFrames
        self.info['rotationAngle'] = [0] * self.nbOfFrames
        self.info['manual_center'] = [None] * self.nbOfFrames
        self.info['center'] = [None] * self.nbOfFrames
        self.onGroupChanged()
        self.resetStatusbar()
    
    def showAbout(self):    
        
        text = "Single"
        if self.mode == 'aime':
            text = "Multiple"
        
        """
        Display About Dialog
        """
        msgBox = QMessageBox()
        msgBox.setWindowTitle("About")
        msgBox.setTextFormat(Qt.RichText)
        msgBox.setText("<br><br><br>" +
                       "Add Intensities " + text + " Experiment (former Image Merger) is running under" +
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

    def browseFolder(self):
        """
        Same as browse files but allow the user to select a folder instead of a file.
        """
        self.dir_path = getAFolder()
        self.folder_paths = []
        self.ignore_files = ['_results', 'settings']
        self.fileList = []
        if self.dir_path != "":
            self.imgPathOnStatusBar.setText("Preprocessing Folder...")
            if self.mode == 'aime':
                self.progressBar.setVisible(True)
                self.progressBar.setRange(0, len(os.listdir(self.dir_path)))
                self.numberToFilesMap = collections.defaultdict(list)
                self.isHdf5 = False
                for fname in os.listdir(self.dir_path):
                    isDataFile = True
                    f = os.path.join(self.dir_path, fname)
                    if isImg(f):
                        self.folder_paths.append(fname)
                        i = -1
                        name = fname.split('.')[0]
                        number = name.split('_')[i]
                        if fname.split('.')[1] in ['h5', 'hdf5']:
                            self.isHdf5 = True
                            isDataFile = False
                        while not number.isnumeric():
                            i -= 1
                            number = name.split('_')[i]
                        if self.isHdf5:
                            for n in name.split('_'):
                                if n == 'data':
                                    isDataFile = True
                        if isDataFile:
                            if self.isHdf5:
                                self.fileList.append(f)
                                with fabio.open(f) as series:
                                    for frame in series.frames():
                                        namefile = os.path.split(frame.file_container.filename)[1].split('.')
                                        temp_filename = namefile[0] + '_%05i.' %(frame.index + 1) + namefile[1]
                                        self.numberToFilesMap[frame.index].append(temp_filename)
                                        QApplication.processEvents()
                            else:
                                self.numberToFilesMap[int(number) - 1].append(f)
                            self.numberToFilesMap[int(number) - 1].sort()
                    else:
                        if not any(ignore_str in fname for ignore_str in self.ignore_files):
                            temp_path = os.path.join(self.dir_path, fname)
                            self.folder_paths.append(temp_path)
                            if os.path.isdir(temp_path):
                                i = 0
                                for fname2 in os.listdir(temp_path):
                                    if isImg(fname2):
                                        self.numberToFilesMap[i].append(fname2)
                                    i += 1
                                QApplication.processEvents()
                    self.progressBar.setValue(self.progressBar.value() + 1)
                self.progressBar.setVisible(False)
                self.onNewFileSelected()
            else:
                self.isHdf5 = False
                self.preprocessfolder()
                self.onNewFileSelected()
                self.refreshAllTab()
            

            # addIntensities(self.numberToFilesMap, dir_path)
            # msg = QMessageBox()
            # msg.setInformativeText(
            #     "Completed Adding intensities, results saved in folder aime_results")
            # msg.setStandardButtons(QMessageBox.Ok)
            # msg.setWindowTitle("Finished Adding Intensities")
            # msg.setStyleSheet("QLabel{min-width: 500px;}")
            # msg.exec_()
            
    def onNewFileSelected(self):
        """
        Preprocess folder of the file and process current image
        :param newFile: full name of selected file
        """
        
        self.browseFolderButton.setHidden(True)
        self.calibrationChkBx.setEnabled(True)
        self.imageCanvas.setHidden(False)
        
        if (self.mode == 'aime'):
            createFolder(os.path.join(self.dir_path, 'aime_results'))
            self.frameNb.setMaximum(len(self.numberToFilesMap[1]) if len(self.numberToFilesMap[1]) <= 8 else 8)
            self.maskImageButton.setEnabled(True)
            self.checkImagesButton.setEnabled(True)
            self.filenameLineEdit.setMaximum(len(self.numberToFilesMap))
            self.filenameLineEdit2.setMaximum(len(self.numberToFilesMap))
            self.exposureNbChanged()
        else:
            createFolder(os.path.join(self.dir_path, 'aise_results'))
            self.maskImageButton.setEnabled(True)
            self.checkImagesButton.setEnabled(True)
            self.browseFileButton.setHidden(True)
            self.frameNb.setMaximum(len(self.img_list))
            self.nbOfGroups = len(self.img_grps)
            self.filenameLineEdit.setMaximum(self.nbOfGroups)
            self.filenameLineEdit2.setMaximum(self.nbOfGroups)
            self.info['manual_rotationAngle'] = [None] * self.nbOfFrames
            self.info['rotationAngle'] = [0] * self.nbOfFrames
            self.onGroupChanged()
            
    def preprocessfolder(self):
        """
        Get all image names in the selected folder
        """
        imgs, _ = getFilesAndHdf(self.dir_path)
        self.img_list = sorted(imgs)
        self.validateFolder()
        self.updateImageGroups()
        
    def validateFolder(self):
        for img in self.img_list:
            if not validateImage(os.path.join(self.dir_path, img), showDialog=False):
                self.img_list.remove(img)
                infMsg = QMessageBox()
                infMsg.setText('Error opening file: ' + os.path.join(self.dir_path, img))
                infMsg.setInformativeText("File is not a valid .TIFF file. The file will be skipped.")
                infMsg.setStandardButtons(QMessageBox.Ok)
                infMsg.setIcon(QMessageBox.Information)
                infMsg.exec_()
                
    def updateImageGroups(self):
        """
        Group images and update details
        """
        self.img_grps = self.splitImages()
        grps = self.img_grps
        detail = "Available Groups : \n"
        if len(grps) >= 1:
            for (i, grp) in enumerate(grps):
                detail += "Group "+ str(i+1)+ " : " + str(grp[0]) + " ... " + str(grp[-1]) + '\n'

        print(detail)
        QApplication.processEvents()

    def splitImages(self):
        """
        Split images into groups
        :return: list of group
        """
        imgs = self.img_list
        frames = self.nbOfFrames
        grps = []
        for i in range(0, len(imgs), frames):
            grps.append(imgs[i:i + frames])

        return grps
    
    def exposureNbChanged(self):
        """
        Change the number of images to add. Will also change the display consequently
        """
        if not self.customSelection:
            self.nbOfExposures = self.frameNb.value()
        self.imageFigure.clear()
        if self.nbOfExposures == 2:
            self.imageAxes = self.imageFigure.add_subplot(121)
            self.imageAxes2 = self.imageFigure.add_subplot(122)
        elif self.nbOfExposures == 3:
            self.imageAxes = self.imageFigure.add_subplot(221)
            self.imageAxes2 = self.imageFigure.add_subplot(222)
            self.imageAxes3 = self.imageFigure.add_subplot(223)
        elif self.nbOfExposures == 4:
            self.imageAxes = self.imageFigure.add_subplot(221)
            self.imageAxes2 = self.imageFigure.add_subplot(222)
            self.imageAxes3 = self.imageFigure.add_subplot(223)
            self.imageAxes4 = self.imageFigure.add_subplot(224)
        elif self.nbOfExposures == 5:
            self.imageAxes = self.imageFigure.add_subplot(231)
            self.imageAxes2 = self.imageFigure.add_subplot(232)
            self.imageAxes3 = self.imageFigure.add_subplot(233)
            self.imageAxes4 = self.imageFigure.add_subplot(234)
            self.imageAxes5 = self.imageFigure.add_subplot(235)
        elif self.nbOfExposures == 6:
            self.imageAxes = self.imageFigure.add_subplot(231)
            self.imageAxes2 = self.imageFigure.add_subplot(232)
            self.imageAxes3 = self.imageFigure.add_subplot(233)
            self.imageAxes4 = self.imageFigure.add_subplot(234)
            self.imageAxes5 = self.imageFigure.add_subplot(235)
            self.imageAxes6 = self.imageFigure.add_subplot(236)
        elif self.nbOfExposures == 7:
            self.imageAxes = self.imageFigure.add_subplot(241)
            self.imageAxes2 = self.imageFigure.add_subplot(242)
            self.imageAxes3 = self.imageFigure.add_subplot(243)
            self.imageAxes4 = self.imageFigure.add_subplot(244)
            self.imageAxes5 = self.imageFigure.add_subplot(245)
            self.imageAxes6 = self.imageFigure.add_subplot(246)
            self.imageAxes7 = self.imageFigure.add_subplot(247)
        elif self.nbOfExposures == 8:
            self.imageAxes = self.imageFigure.add_subplot(241)
            self.imageAxes2 = self.imageFigure.add_subplot(242)
            self.imageAxes3 = self.imageFigure.add_subplot(243)
            self.imageAxes4 = self.imageFigure.add_subplot(244)
            self.imageAxes5 = self.imageFigure.add_subplot(245)
            self.imageAxes6 = self.imageFigure.add_subplot(246)
            self.imageAxes7 = self.imageFigure.add_subplot(247)
            self.imageAxes8 = self.imageFigure.add_subplot(248)
        if self.numberToFilesMap is not None:
            self.onGroupChanged()
            
    def onGroupChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        """
        self.statusPrint("Processing...")
        self.orig_imgs = []
        self.orig_img_names = []
        if (self.mode == 'aise'):
            self.filenameLineEdit.setValue(self.currentGroupNumber + 1)
            start = self.nbOfFrames*(self.currentGroupNumber)
            end = min(len(self.img_list), start + self.nbOfFrames)
            for i in range(start, end):
                if self.isHdf5:
                    with fabio.open(self.file_name) as series:
                        frame = series.get_frame(i).data
                    self.orig_imgs.append(ifHdfReadConvertless(self.img_list[i], frame).astype(np.float32))
                else:
                    filePath = os.path.join(self.dir_path, self.img_list[i])
                    self.orig_imgs.append(fabio.open(filePath).data.astype(np.float32))
        else:
            self.filenameLineEdit.setValue(self.currentFileNumber + 1)
            if self.customSelection:
                if self.isHdf5:
                    for line in self.selected_files:
                        i = line[0]
                        file = line[1]
                        path = os.path.join(self.dir_path, file)
                        with fabio.open(path) as series:
                            frame = series.get_frame(self.currentFileNumber).data
                        image = ifHdfReadConvertless(self.numberToFilesMap[self.currentFileNumber][i], frame)
                        self.orig_imgs.append(image.astype(np.float32))
                        self.orig_img_names.append(self.numberToFilesMap[self.currentFileNumber][i])
                else:
                    for line in self.selected_files:
                        i = line[0]
                        folder = line[1]
                        image = fabio.open(os.path.join(folder, self.numberToFilesMap[self.currentFileNumber][i])).data.astype(np.float32)
                        self.orig_imgs.append(image)
                        self.orig_img_names.append(os.path.basename(folder) + "/" + self.numberToFilesMap[self.currentFileNumber][i])
            else:
                for i in range(self.nbOfExposures):
                    if self.isHdf5:
                        with fabio.open(self.fileList[i]) as series:
                            frame = series.get_frame(self.currentFileNumber).data
                        image = ifHdfReadConvertless(self.numberToFilesMap[self.currentFileNumber][i], frame)
                        self.orig_imgs.append(image.astype(np.float32))
                        self.orig_img_names.append(self.numberToFilesMap[self.currentFileNumber][i])
                    else:
                        folder = self.folder_paths[i]
                        image = fabio.open(os.path.join(folder, self.numberToFilesMap[self.currentFileNumber][i])).data.astype(np.float32)
                        self.orig_imgs.append(image)
                        self.orig_img_names.append(os.path.basename(folder) + "/" + self.numberToFilesMap[self.currentFileNumber][i])
          
        self.init_imgs = copy.copy(self.orig_imgs)
        self.imgDetailOnStatusBar.setText(
            str(self.orig_imgs[0].shape[0]) + 'x' + str(self.orig_imgs[0].shape[1]) + ' : ' + str(self.orig_imgs[0].dtype))
        
        if (self.mode == 'aime'):
            self.processImage()
        else:
            self.sum_img = self.generateSumImg()
            self.refreshAllTab()
    
        self.initialWidgets(self.orig_imgs[0], self.sum_img)
        self.statusPrint("")
        gc.collect()
        
    
    def processGroup(self):
        """
        Process Image by getting all flags and call process() of QuadrantFolder object
        Then, write data and update UI
        """
        if self.ableToProcess():
            print('Processing...')
            self.statusPrint('Processing...')
            if self.calibrationChkBx.isChecked():
                self.getExtentAndCenter(self.orig_imgs[0])
                if any(mra is not None for mra in self.info['manual_rotationAngle']):
                    for i in range(self.nbOfFrames):
                        self.rotateImg(i)
                        self.orig_imgs[i] = self.getRotatedImage(i)

            output = os.path.join(self.dir_path, 'aise_results')
            imgs = self.img_grps[self.currentGroupNumber]
            if len(imgs) > 0:
                first = imgs[0]
                last = imgs[-1]
                f_ind1 = first.rfind('_')
                f_ind2 = first.rfind('.')
                l_ind1 = last.rfind('_')
                l_ind2 = last.rfind('.')

                if f_ind1 == -1 or f_ind2 == -1 or l_ind1 == -1 or l_ind2 == -1 or first[:f_ind1] != last[:l_ind1]:
                    filename = "group_"+str(self.currentGroupNumber + 1).zfill(5)+'.tif'
                else:
                    filename = first[:f_ind1] + "_"
                    filename += first[f_ind1 + 1:f_ind2]
                    filename += "_"
                    filename += last[l_ind1 + 1:l_ind2]
                    filename += '.tif'

                # Update details
                details = "- Merging Group "+ str(self.currentGroupNumber + 1)+" : \n"
                for (j, img) in enumerate(imgs):
                    details += str(j+1)+". "+img+"\n"
                details += "To : " + filename +'\n\n'

                print(details)
                self.statusPrint('Merging...')
                # self.parallel_process_and_add()
                self.sum_img = self.generateSumImg()
                if self.blankImageSettings is not None:
                    if self.blankImageSettings['subtractBlank'] == True:
                        raw_filepath = r"{}".format(self.blankImageSettings['path'])
                        blank_image = fabio.open(raw_filepath).data
                        weight = self.blankImageSettings['weight']
                        self.sum_img = self.sum_img - weight * blank_image * len(imgs)
                        if self.blankImageSettings['clampNegativeValues'] == True:
                            self.sum_img = np.clip(self.sum_img, 0, None) 
                
                if self.useMaskChkBx.isChecked():
                    self.maskData = fabio.open(self.maskPath).data
                    self.orig_avg_img = self.sum_img
                    self.sum_img = self.sum_img* self.maskData
                    avg_intensity = np.sum(self.orig_avg_img) / np.sum(self.sum_img)
                    print("Average masked intensity: ", avg_intensity)
                    index = filename.rfind('.')
                    masked_filename = filename[:index] + "_masked" + filename[index:]
                                   
                self.generateCSVLine(filename)
                print('Saving merged image...')
                self.statusPrint('Saving merged image...')
                # Saving images
                # If use mask is checked, then we save the masked image and the unmasked image
                if self.compressChkBx.isChecked():
                    if self.useMaskChkBx.isChecked():
                        mask_tif_img = Image.fromarray(self.sum_img)
                        mask_tif_img.save(os.path.join(output, masked_filename), compression='tiff_lzw')
                        tif_img = Image.fromarray(self.orig_avg_img)
                        tif_img.save(os.path.join(output, filename), compression='tiff_lzw')
                    else:
                        tif_img = Image.fromarray(self.sum_img)
                        tif_img.save(os.path.join(output, filename), compression='tiff_lzw')
                else:
                    if self.useMaskChkBx.isChecked():
                        fabio.tifimage.tifimage(data=self.sum_img).write(os.path.join(output, masked_filename))
                        fabio.tifimage.tifimage(data=self.orig_avg_img).write(os.path.join(output, filename))
                    else:
                        fabio.tifimage.tifimage(data=self.sum_img).write(os.path.join(output, filename))

            self.refreshAllTab()
            print("Done. Result image have been saved to "+output)
            
    def generateCSVLine(self, filename):
        nonmaskedPixels = 0
        orig_img_intensity = 0
        img_with_mask_intensity = 0
        weight = 0
            
        if self.orig_avg_img is not None:
            orig_img_intensity = np.sum(self.orig_avg_img)
        else:
            orig_img_intensity = np.sum(self.sum_img)
            
        if self.maskData is not None:
            nonmaskedPixels = np.sum(self.maskData == 1)
            img_with_mask_intensity = np.sum(self.sum_img)
        else:
            img_with_mask_intensity = orig_img_intensity
        
        dateString = datetime.now().strftime("%m/%d/%Y %H:%M:%S")
        
        # Masked Image Intensity (average) = Masked Image Intensity (Total) / Number of Pixels Not Masked
        average_mask = img_with_mask_intensity / nonmaskedPixels
        
        if self.blankImageSettings is not None:
            weight = self.blankImageSettings['weight']
        
        info = [filename, dateString, orig_img_intensity, img_with_mask_intensity, nonmaskedPixels, average_mask, weight, self.nbOfFrames, self.drawnMask, self.computedMask]
        self.csvInfo.append(info)
        
    def writeToCSV(self):
        file_path = join(join(self.dir_path, 'aise_results'), 'intensities.csv')
        # Write header line if file doesn't exist
        if not os.path.exists(file_path):
            with open(file_path, 'w') as f:
                writer = csv.writer(f)
                writer.writerow(['Filename', 'Date', 'Original Image Intensity (Total)', 'Masked Image Intensity (Total)', 'Number of Pixels Not Masked', 'Masked Image Intensity (Average)', 'Blank Image Weight', 'Binning Factor', 'Drawn Mask', 'Computed Mask'])
        
        # Write all the other lines
        with open(file_path, 'a' ) as f:
            writer = csv.writer(f)
            for line in self.csvInfo:
                writer.writerow(line)
        
        self.csvInfo = []
        
    def initialWidgets(self, img, result):
        """
        Initial some widgets values which depends on current image
        :param img: selected image
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

        min_val = result.min()
        max_val = result.max()
        self.spResultmaxInt.setRange(min_val, max_val)
        self.spResultminInt.setRange(min_val, max_val)
        if not self.resPersistIntensity.isChecked():
            self.spResultmaxInt.setValue(max_val * .5)
            self.spResultminInt.setValue(min_val)
        self.spResultmaxInt.setSingleStep(max_val * .05)
        self.spResultminInt.setSingleStep(max_val * .05)

        self.resultminIntLabel.setText("Min Intensity ("+str(min_val)+")")
        self.resultmaxIntLabel.setText("Max Intensity (" + str(max_val) + ")")

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

        self.uiUpdating = False
        
    def plotImages(self, imageAxes, img, index, name=None):
        """
        Displays the image
        """
        ax = imageAxes
        ax.cla()

        if self.calibrationChkBx.isChecked():
            extent, center = self.getExtentAndCenter(img, index)
        else:
            extent, center = [0, 0], (0, 0)

        if self.logScaleIntChkBx.isChecked():
            ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
        else:
            ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
        ax.set_facecolor('black')
        
        if self.mode == 'aime':
            ax.set_xlabel(name)

            if self.showSeparator.isChecked() and self.orig_image_center is not None:
                # Draw quadrant separator
                ax.axvline(center[0], color='y')
                ax.axhline(center[1], color='y')

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
    
    def ableToProcess(self):
        """
        Check if image can be processed
        """
        return self.orig_imgs != [] and not self.uiUpdating
        
    def processImage(self):
        """
        Process Image by getting all flags and call process() of QuadrantFolder object
        Then, write data and update UI
        """
        if self.ableToProcess():
            if self.calibrationChkBx.isChecked():
                self.getExtentAndCenter(self.orig_imgs[0])
                if any(mra is not None for mra in self.info['manual_rotationAngle']):
                    for i in range(self.nbOfExposures):
                        self.rotateImg(i)
                        self.orig_imgs[i] = self.getRotatedImage(i)

            
            self.sum_img = self.addIntensity(self.orig_imgs, self.dir_path, self.currentFileNumber + 1)
            self.refreshAllTab()
            
    def addIntensity(self, imgs, dir_path, key):
        """
        Add Intensity of one set of files (main function).
        :param imgs, dir_path, key:
        """
        createFolder(fullPath(dir_path, "aime_results")) 
        sum_img = self.generateSumImg(imgs)

        first = self.orig_img_names[0].split('.')[0]
        last = self.orig_img_names[-1].split('.')[0]
        ind = 1
        filename = ""
        while last.startswith(first[:ind]) and ind < len(first):
            ind += 1
        if ind != 1:
            filename = first[:ind]
            if filename[-1] != '_':
                filename += '_'
        filename += 'res_' + str(key).zfill(5) + '.tif'
        result_file = os.path.join(dir_path, 'aime_results', filename)
        fabio.tifimage.tifimage(data=sum_img).write(result_file)
        print('Saved ', result_file)
        print('Resulting image shape ', sum_img.shape)
        return sum_img
    
    def generateSumImg(self, imgs=None):
        if imgs is None:
            if self.calibrationChkBx.isChecked():
                images = self.matchCenters(self.orig_imgs)
            else:
                images = self.orig_imgs
        else:
            images = imgs
        if self.avgInsteadOfSum.isChecked():
            # WARNING: in averageImages, we are using preprocessed instead of rotate because rotate is a black box and we already calibrated the images
            ##todo homogenize
            sum_img = 0
            if 'detector' in self.info:
                sum_img = averageImages(images, preprocessed=True, man_det=self.info['detector'])
            else:
                sum_img = averageImages(images, preprocessed=True)
            return sum_img
        else:
            sum_img = 0
            for img in images:
                if not isinstance(sum_img, int) and (img.shape[0] > sum_img.shape[0] or img.shape[1] > sum_img.shape[1]):
                    sum_img = resizeImage(sum_img, img.shape)
                elif not isinstance(sum_img, int):
                    img = resizeImage(img, sum_img.shape)
                sum_img += img
            return sum_img
        
    def fileNameChanged(self):
        """
        Triggered when the name of the current file is changed
        """
        fileName = self.filenameLineEdit.value()
        self.filenameLineEdit2.blockSignals(True)
        self.filenameLineEdit2.setValue(fileName)
        self.filenameLineEdit2.blockSignals(False)
        if self.numberToFilesMap[fileName - 1] == []:
            print('Error in the number entered')
            return
        self.currentFileNumber = fileName - 1
        self.onGroupChanged()
        
    def fileName2Changed(self):
        """
        Triggered when the name of the current file is changed
        """
        fileName = self.filenameLineEdit2.value()
        self.filenameLineEdit.blockSignals(True)
        self.filenameLineEdit.setValue(fileName)
        self.filenameLineEdit.blockSignals(False)
        if self.numberToFilesMap[fileName - 1] == []:
            print('Error in the number entered')
            return
        self.currentFileNumber = fileName - 1
        self.onGroupChanged()
            
    def getExtentAndCenter(self, orig_img, index=0):
        """
        Give the extent and the center of the image
        """
        if self.orig_imgs == []:
            return [0, 0], (0, 0)
        if self.orig_image_center is None:
            self.findCenter(orig_img, index)
            self.statusPrint("Done.")
        if 'calib_center' in self.info:
            center = self.info['calib_center']
        elif self.info['manual_center'][index] is not None:
            center = self.info['manual_center'][index]
        else:
            center = self.orig_image_center

        for d, c in enumerate(self.info['center']):
            if c is None:
                self.info['center'][d] = center
        extent = [self.info['center'][index][0] - center[0], self.info['center'][index][1] - center[1]]
        return extent, center
    
    def findCenter(self, orig_img, index=0):
        """
        Find center of the diffraction. The center will be kept in self.info['center'].
        Once the center is calculated, the rotation angle will be re-calculated, so self.info["rotationAngle"] is deleted
        """
        self.statusPrint("Finding Center...")
        if self.info['center'][index] is not None:
            return
        if 'calib_center' in self.info:
            self.info['center'][index] = self.info['calib_center']
            return
        if self.info['manual_center'][index] is not None:
            self.info['center'][index] = self.info['manual_center'][index]
            return
        print("Center is being calculated ... ")
        self.orig_imgs[index], self.info['center'][index] = processImageForIntCenter(orig_img, getCenter(orig_img))
        if self.orig_image_center is None:
            self.orig_image_center = self.info['center'][index]
        print("Done. Center = "+str(self.info['center'][index]))
        
    def matchCenters(self, imgs):
        images = []
        b, l = imgs[0].shape
        size = 0
        for img in imgs:
            b, l = img.shape
            dim = max(b,l)
            if size < dim:
                size = dim
        for i, img in enumerate(imgs):
            new_image = np.zeros((size,size))
            new_image[0:img.shape[0],0:img.shape[1]] = img
            transx = size/2-self.info['center'][i][0]
            transy = size/2-self.info['center'][i][1]
            M = np.float32([[1,0,transx],[0,1,transy]])
            cv2.setNumThreads(1) # Added to prevent segmentation fault due to cv2.warpAffine
            translated_Img = cv2.warpAffine(new_image, M, (size,size))
            images.append(translated_Img)
        return images
        
    def rotateImg(self, index):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal. The angle will be kept in self.info["rotationAngle"]
        Once the rotation angle is calculated, the average fold will be re-calculated, so self.info["avg_fold"] is deleted
        """
        self.statusPrint("Finding Rotation Angle...")
        if self.info['manual_rotationAngle'][index] is not None:
            self.info['rotationAngle'][index] = self.info['manual_rotationAngle'][index]
            # self.info['manual_rotationAngle'][index] = None
        else:
            if all(v is None for v in self.info['manual_rotationAngle']):
                print("Rotation Angle is being calculated ... ")
                # Selecting disk (base) image and corresponding center for determining rotation as for larger images (formed from centerize image) rotation angle is wrongly computed
                img = copy.copy(self.orig_imgs[index])
                _, center = self.getExtentAndCenter(img, index)
                if 'detector' in self.info:
                    self.info['rotationAngle'][index] = getRotationAngle(img, center, 0, man_det=self.info['detector'])
                else:
                    self.info['rotationAngle'][index] = getRotationAngle(img, center, 0)
        print("Done. Rotation Angle is " + str(self.info['rotationAngle'][index]) +" degree")
        self.statusPrint("")
        
    def getRotatedImage(self, index):
        """
        Get rotated image by angle while image = original input image, and angle = self.info["rotationAngle"]
        """
        img = np.array(self.init_imgs[index], dtype="float32")
        center = self.info['center'][index]
        if self.center_before_rotation is not None:
            center = self.center_before_rotation
        else:
            self.center_before_rotation = center
        if self.info['manual_center'][index] is not None:
            center = self.info['manual_center'][index]

        b, l = img.shape
        rotImg, newCenter, _ = rotateImage(img, center, self.info["rotationAngle"][index])

        # Cropping off the surrounding part since we had already expanded the image to maximum possible extent in centerize image
        bnew, lnew = rotImg.shape
        db, dl = (bnew - b)//2, (lnew-l)//2
        final_rotImg = rotImg[db:bnew-db, dl:lnew-dl]
        self.info['center'][index] = (newCenter[0]-dl, newCenter[1]-db)

        return final_rotImg
    
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

            if self.mode == 'aise':
                img = self.orig_imgs[self.currentFrameNumber]
            else:
                img = self.orig_imgs[self.currentFileNumber]
            ax1 = self.doubleZoomAxes
            x,y = (0, 0)
            imgCropped = img[y - 10:y + 10, x - 10:x + 10]
            if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                self.doubleZoomPt = (x, y)
                ax1.imshow(imgScaled)
                # y, x = imgScaled.shape
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

    def updateUI(self):
        """
        Update current all widget in current tab , spinboxes, and refresh status bar
        """
        if self.ableToProcess():
            self.updateImageTab()
            self.updateResultTab()
    
    def refreshAllTab(self):
        """
        Set all tab update status to be not update, and Refresh (Redraw) all tab
        """
        self.updated['img'] = False
        self.updated['res'] = False
        self.function = None
        self.axClicked = None
        self.updateUI()
        self.resetStatusbar()
        
    def resetStatusbar(self):
        """
        Reset the status bar
        """
        if self.mode == 'aise':
            if self.img_list != [] and self.currentGroupNumber + 1 == self.nbOfGroups and len(self.img_list)%self.nbOfFrames != 0:
                max_frame = len(self.img_list)%self.nbOfFrames
            else:
                max_frame = self.nbOfFrames
            self.imgPathOnStatusBar.setText(
                'Current Frame (' + str(self.currentFrameNumber + 1) + '/' + str(max_frame) + '), Current Group ('
                + str(self.currentGroupNumber + 1) + '/' + str(self.nbOfGroups) + ') : ' + self.img_list[self.currentFileNumber])
        else:
            self.imgPathOnStatusBar.setText(
            'Current Set (' + str(self.currentFileNumber + 1) + '/' + str(len(self.numberToFilesMap)) + ') : ' + self.dir_path)
            
    def refreshImageTab(self):
        """
        Refresh (Redraw) image tab
        """
        self.updated['img'] = False
        self.updateUI()
        self.resetStatusbar()
        
    def updateImageTab(self):
        """
        Display image in image tab, and draw lines
        """
        if not self.updated['img']:
            self.uiUpdating = True

            if self.mode == 'aime':
                if self.calibrationChkBx.isChecked():
                    _, center = self.getExtentAndCenter(self.orig_imgs[0])
                else:
                    _, center = [0, 0], (0, 0)

                self.plotImages(self.imageAxes, self.orig_imgs[0], 0, self.orig_img_names[0])
                self.plotImages(self.imageAxes2, self.orig_imgs[1], 1, self.orig_img_names[1])

                if self.nbOfExposures >= 3:
                    self.plotImages(self.imageAxes3, self.orig_imgs[2], 2, self.orig_img_names[2])
                if self.nbOfExposures >= 4:
                    self.plotImages(self.imageAxes4, self.orig_imgs[3], 3, self.orig_img_names[3])
                if self.nbOfExposures >= 5:
                    self.plotImages(self.imageAxes5, self.orig_imgs[4], 4, self.orig_img_names[4])
                if self.nbOfExposures >= 6:
                    self.plotImages(self.imageAxes6, self.orig_imgs[5], 5, self.orig_img_names[5])
                if self.nbOfExposures >= 7:
                    self.plotImages(self.imageAxes7, self.orig_imgs[6], 6, self.orig_img_names[6])
                if self.nbOfExposures >= 8:
                    self.plotImages(self.imageAxes8, self.orig_imgs[7], 7, self.orig_img_names[7])
            else:
                self.plotImages(self.imageAxes, self.orig_imgs[self.currentFrameNumber], self.currentFrameNumber)

            if self.calSettingsDialog is not None:
                self.calSettingsDialog.centerX.setValue(center[0])
                self.calSettingsDialog.centerY.setValue(center[1])

            self.imageFigure.tight_layout()
            self.imageCanvas.draw()


            self.updated['img'] = True
            self.uiUpdating = False
            
    def updateResultTab(self):
        """
        Display image in image tab, and draw lines
        """
        if not self.updated['res']:
            self.uiUpdating = True

            ax = self.resultAxes
            img = self.sum_img
            ax.cla()

            if self.calibrationChkBx.isChecked():
                extent, _ = self.getExtentAndCenter(img)
            else:
                extent, _ = [0, 0], (0, 0)

            if self.resLogScaleIntChkBx.isChecked():
                ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.spResultminInt.value()), vmax=self.spResultmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
            else:
                ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.spResultminInt.value(), vmax=self.spResultmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
            ax.set_facecolor('black')

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

            self.resultFigure.tight_layout()
            self.resultCanvas.draw()

            self.updated['res'] = True
            self.uiUpdating = False
            
    def refreshResultTab(self):
        """
        Refresh (Redraw) image tab
        """
        self.updated['res'] = False
        self.updateUI()
        self.resetStatusbar()
        
    def imgScrolled(self, event):
        """
        This function is called when a mouse scrolled on the image in image tab. This will affect zoom-in and zoom-out
        """
        if event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        if self.mode == 'aime':
            img_size = self.orig_imgs[0].shape
        else:
            img_size = self.orig_imgs[self.currentFrameNumber].shape

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
            
    def imageZoomIn(self):
        """
        Trigger when set zoom in button is pressed (image tab)
        """
        if self.imgZoomInB.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a rectangle on the image to zoom in (ESC to cancel)")
            self.imageCanvas.draw_idle()
            self.function = ["im_zoomin"]
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
            self.resultCanvas.draw_idle()
            self.function = ["r_zoomin"]
        else:
            self.function = None
            self.resetStatusbar()
    
    def imageZoomOut(self):
        """
        Trigger when set zoom out button is pressed (image tab)
        """
        self.imgZoomInB.setChecked(False)
        self.resultZoomInB.setChecked(False)
        self.default_img_zoom = None
        self.img_zoom = None
        self.refreshAllTab()
        
    def imageClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return
            
        x = event.xdata
        y = event.ydata
        
        if self.mode == 'aise':
            ax = self.imageAxes
        else:
            if event.inaxes == self.imageAxes.axes:
                ax = self.imageAxes
                self.index = 0
            elif event.inaxes == self.imageAxes2.axes:
                ax = self.imageAxes2
                self.index = 1
            elif self.imageAxes3 is not None and event.inaxes == self.imageAxes3.axes:
                ax = self.imageAxes3
                self.index = 2
            elif self.imageAxes4 is not None and event.inaxes == self.imageAxes4.axes:
                ax = self.imageAxes4
                self.index = 3
            elif self.imageAxes5 is not None and event.inaxes == self.imageAxes5.axes:
                ax = self.imageAxes5
                self.index = 4
            elif self.imageAxes6 is not None and event.inaxes == self.imageAxes6.axes:
                ax = self.imageAxes6
                self.index = 5
            elif self.imageAxes7 is not None and event.inaxes == self.imageAxes7.axes:
                ax = self.imageAxes7
                self.index = 6
            elif self.imageAxes8 is not None and event.inaxes == self.imageAxes8.axes:
                ax = self.imageAxes8
                self.index = 7
            elif self.axClicked is not None:
                ax = self.axClicked
                
        if self.doubleZoomMode:
            progress = False
            # If x, y is inside figure and image is clicked for first time in double zoom mode
            if self.mode == 'aime' and self.axClicked is not None:
                progress = True
            elif self.mode == 'aise':
                progress = True
                
            if progress:
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
        
        if x is None or y is None:
            self.imgCoordOnStatusBar.setText("")
            bounds = ax.get_window_extent().get_points()  ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[0] - ylim[1]) / (bounds[0][1] - bounds[1][1])
            cy = ylim[1] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])
            x = int(round(x))
            y = int(round(y))
            
        if self.doubleZoom.isChecked() and not self.doubleZoomMode:
            x, y = self.doubleZoomToOrigCoord(x, y)
            self.doubleZoomMode = True

        # Provide different behavior depending on current active function
        if self.function is None:
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
                    self.refreshAllTab()
            elif func[0] == 'specify_image':
                self.specifyCenterAndOrientationClicked()
            elif func[0] == 'specify_image_mask':
                self.maskImageButtonClicked()
                
        if self.mode == 'aime':
            self.axClicked = ax
            
    def imageOnMotion(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return
        x = event.xdata
        y = event.ydata
        
        if self.mode == 'aise':
            img = self.orig_imgs[self.currentFrameNumber]
            ax = self.imageAxes
        else:
            if self.axClicked is not None:
                ax = self.axClicked
            else:
                if event.inaxes == self.imageAxes.axes:
                    ax = self.imageAxes
                elif event.inaxes == self.imageAxes2.axes:
                    ax = self.imageAxes2
                elif self.imageAxes3 is not None and event.inaxes == self.imageAxes3.axes:
                    ax = self.imageAxes3
                elif self.imageAxes4 is not None and event.inaxes == self.imageAxes4.axes:
                    ax = self.imageAxes4
                elif self.imageAxes5 is not None and event.inaxes == self.imageAxes5.axes:
                    ax = self.imageAxes5
                elif self.imageAxes6 is not None and event.inaxes == self.imageAxes6.axes:
                    ax = self.imageAxes6
                elif self.imageAxes7 is not None and event.inaxes == self.imageAxes7.axes:
                    ax = self.imageAxes7
                elif self.imageAxes8 is not None and event.inaxes == self.imageAxes8.axes:
                    ax = self.imageAxes8
                else:
                    ax = self.imageAxes

            img = self.orig_imgs[self.index]

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < img.shape[1] and y < img.shape[0]:
                self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]))
                if self.doubleZoom.isChecked() and self.doubleZoomMode and x>10 and x<img.shape[1]-10 and y>10 and y<img.shape[0]-10:
                    ax1 = self.doubleZoomAxes
                    imgCropped = img[int(y - 10):int(y + 10), int(x - 10):int(x + 10)]
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

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.imgCoordOnStatusBar.setText("")
            bounds = ax.get_window_extent().get_points()  ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[0] - ylim[1]) / (bounds[0][1] - bounds[1][1])
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

            
    def imageReleased(self, event):
        """
        Triggered when mouse released from image
        """
        if self.function is not None and self.function[0] == "im_move":
            self.function = None
            
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
                    self.img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.resultZoomInB.setChecked(False)
                    self.refreshAllTab()

    def resultOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """
        if not self.ableToProcess():
            return

        x = event.xdata
        y = event.ydata
        img = self.sum_img
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

        if self.function is None:
            return

        func = self.function

        if func[0] == "r_zoomin" and len(self.function) == 2:
            # draw rectangle
            ax = self.resultAxes
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
        elif func[0] == "r_move":
            # move zoom in location when image dragged
            if self.img_zoom is not None:
                ax = self.resultAxes
                move = (func[1][0] - x, func[1][1] - y)
                self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[1], img.shape[0])
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
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
        if event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img = self.sum_img
        img_size = img.shape

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

        self.img_zoom = [(x1, x2), (y1, y2)]
        ax = self.resultAxes
        ax.set_xlim(self.img_zoom[0])
        ax.set_ylim(self.img_zoom[1])
        ax.invert_yaxis()
        self.resultCanvas.draw_idle()
        
    def maskImageButtonClicked(self):
        
        if self.imageMaskingTool is None:
            if self.mode == 'aise':
                if self.isHdf5:
                    file_name = self.file_name
                else:
                    file_name = self.dir_path + '/' + self.img_list[self.currentFileNumber]
                self.imageMaskingTool = ImageMaskerWindow(self.dir_path , file_name, self.spminInt.value(), self.spmaxInt.value(), self.isHdf5)
            else:
                if self.function is None:
                    self.function = ["specify_image_mask"]
                    QMessageBox.information(self, "Select Image", "Select Image to Mask")
                else:
                    self.function = None
                    file_name = self.numberToFilesMap[self.currentFileNumber][self.index]
                    path_name = os.path.join(self.dir_path, self.folder_paths[self.index] + '/' + file_name)
                    self.imageMaskingTool = ImageMaskerWindow(self.dir_path , path_name, self.spminInt.value(), self.spmaxInt.value(), self.isHdf5)
                
        if self.imageMaskingTool is not None and self.imageMaskingTool.exec_():
            if os.path.exists(join(join(self.dir_path, 'settings'), 'mask.tif')):
                print("mask found!!")
                self.maskPath = join(join(self.dir_path, 'settings'), 'mask.tif')
                self.useMaskChkBx.setChecked(True)
                self.useMaskChkBx.setEnabled(True)
            if os.path.exists(join(join(self.dir_path, 'settings'), 'blank_image_settings.json')):
                print("blank image found!")
                with open(join(join(self.dir_path, 'settings'), 'blank_image_settings.json'), 'r') as f:
                    self.blankImageSettings = json.load(f)
            self.drawnMask = self.imageMaskingTool.drawnMask
            self.computedMask = self.imageMaskingTool.computedMask
            self.imageMaskingTool = None

    def specifyCenterAndOrientationClicked(self):
        if self.mode == 'aise':
            if self.isHdf5:
                file_name = self.file_name
            else:
                file_name = self.dir_path + '/' + self.img_list[self.currentFileNumber]
            
            self.calibrationDialog = CalibrationDialog(self.dir_path, file_name, self.mode)
            self.calibrationDialog.show()
        else:
            if self.function is None:
                self.function = ["specify_image"]
                QMessageBox.information(self, "Select Image", "Select Image to Calibrate")
            else:
                self.function = None
                file_name = self.numberToFilesMap[self.currentFileNumber][self.index]
                path_name = os.path.join(self.dir_path, self.folder_paths[self.index] + '/' + file_name)
                self.calibrationDialog = CalibrationDialog(self.dir_path, path_name, self.mode)
                self.calibrationDialog.show()
                
        
    def setCalibrationImage(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or force to open
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        if self.calSettingsDialog is None:
            self.calSettingsDialog = CalibrationSettings(self.dir_path)
        self.calSettings = None
        if self.mode == 'aise':
            # cal_setting = self.calSettingsDialog.calSettings
            result = self.calSettingsDialog.exec_()
            if result == 1:
                self.calSettings = self.calSettingsDialog.getValues()
                if self.calSettings is not None and self.calSettings:
                    print("not empty")
                    if self.calSettingsDialog.fixedCenter.isChecked():
                        self.info['calib_center'] = self.calSettings['center']
                    else:
                        print("fix this")
                    if "detector" in self.calSettings:
                        self.info["detector"] = self.calSettings["detector"]
                    return True
                else:
                    print("empty")
                    return False
        else:
            cal_setting = self.calSettingsDialog.calSettings
            if cal_setting is not None or force:
                result = self.calSettingsDialog.exec_()
                if result == 1:
                    self.calSettings = self.calSettingsDialog.getValues()
                    if self.calSettings is not None:
                        if self.calSettingsDialog.fixedCenter.isChecked():
                            self.info['calib_center'] = self.calSettings['center']
                            self.setCenterRotationButton.setEnabled(False)
                            self.setCenterRotationButton.setToolTip(
                                "Please uncheck fixed center in calibration settings first")
                        else:
                            self.setCenterRotationButton.setEnabled(True)
                            self.setCenterRotationButton.setToolTip("")
                        if "detector" in self.calSettings:
                            self.info["detector"] = self.calSettings["detector"]
                    return True
            return False
    
    def calibrationClicked(self):
        """
        Handle when Calibration Settings button is clicked
        :return:
        
        """
        sucess = self.setCalibrationImage(force=False)
        if sucess:
            self.info['manual_rotationAngle'] = [None] * self.nbOfFrames
            self.info['rotationAngle'] = [0] * self.nbOfFrames
            self.info['center'] = [None] * self.nbOfFrames
            self.info['manual_center'] = [None] * self.nbOfFrames
            self.onGroupChanged()
            self.refreshAllTab()
        
    def binFactorChecked(self):
        if self.factorBx.isChecked():
            self.frameNb.setEnabled(True)
            self.selectImageChkBx.setChecked(False)
            self.sumImagesButton.setEnabled(False)
    
    def selectImageChecked(self):
        if self.selectImageChkBx.isChecked():
            if self.mode == 'aise':
                self.factorBx.setChecked(False)
            self.frameNb.setEnabled(False)
            self.sumImagesButton.setEnabled(True)

    def selectImageSequence(self):
        if self.mode == 'aise':
            if self.imageSequenceDialog is None:
                if self.isHdf5:
                    file_path = self.file_name
                else:
                    file_path = self.dir_path
                self.imageSequenceDialog = AISEImageSelectionWindow(file_path, self.img_list, self.img_grps, self.misaligned_images, self.isHdf5)
            self.imageSequenceLists = None
            if self.imageSequenceDialog.exec_():
                self.customImageSequence = True
                if self.imageSequenceDialog.img_grps:
                    self.img_grps = self.imageSequenceDialog.img_grps
                    self.nbOfGroups = len(self.img_grps)
                    self.frameNb.setValue(len(self.img_grps[0]))
        else:
            self.experimentSelectionDialog = SelectionWindow(self.folder_paths)
            self.experimentSelectionDialog.closed.connect(self.handleSelection)
            self.experimentSelectionDialog.show()
                
    
    def handleSelection(self, selected_files):
        self.selected_files = sorted(selected_files)
        if len(self.selected_files) == 0:
            return
        self.nbOfExposures = len(self.selected_files)
        self.customSelection = True
        self.exposureNbChanged()
        self.onGroupChanged()
        
    def stepComboBoxChanged(self, value):
        if (value == 'Step through all images'):
            self.img_grps_copy = self.img_grps
            self.frameNb.setValue(len(self.img_list))
            self.filenameLineEdit.setVisible(False)
        elif (value == 'Step through selected images'):
            self.img_grps = self.img_grps_copy
            self.nbOfGroups = len(self.img_grps)
            self.frameNb.setValue(len(self.img_grps[0]))
            self.filenameLineEdit.setVisible(False)
        elif (value == 'Step through badly correlated images'):
            self.img_grps_copy = self.img_grps
            self.filenameLineEdit.setVisible(False)
            if not self.misaligned_images:
                QMessageBox.information(self, "No Misaligned Images", "No misaligned images found")
                self.frameSteppingSelection.setCurrentIndex(0)
            else:
                self.img_grps = []
                self.img_grps.append(self.misaligned_images)
                self.nbOfGroups = len(self.img_grps)
                self.frameNb.setValue(len(self.img_grps[0]))
        # elif (value == 'Step through group images'):
        #     self.filenameLineEdit.setVisible(True)
        
    def loadImagesCache(self):
        cache_file = fullPath(fullPath(self.dir_path, "aise_cache"), "imageresultscache"+".info")
        if os.path.isfile(cache_file):
            with open(cache_file, "rb") as c:
                info = pickle.load(c)
            if info is not None:
                return info
            print("Cache didnt work or soething")

    def saveImagesCache(self, infoCache):
        cache_file = fullPath(fullPath(self.dir_path, "aise_cache"), "imageresultscache" + ".info")
        createFolder(fullPath(self.dir_path, "aise_cache"))
        with open(cache_file, "wb") as c:
            pickle.dump(infoCache, c)

    def searchInfoCache(self, tiff_file, infoCache):
        for info in infoCache:
            if info[0] == tiff_file:
                return info
        return None
        
    def checkImages(self):
        if self.unalignedImagesDialog is None:
            self.unalignedImagesDialog = UnalignedImagesDialog()
        if self.unalignedImagesDialog.exec_():
            inconsistent_images = []
            imageCo = self.unalignedImagesDialog.image
            centerCo = self.unalignedImagesDialog.center
            angleCo = self.unalignedImagesDialog.angle
            
            processData = False
            infoCache = self.loadImagesCache()
            if infoCache is None:
                processData = True
                infoCache = []
            
            if self.mode == 'aise':
                self.progressBar.setRange(0, len(self.img_list)+1)
            else:
                self.progressBar.setRange(0, len(self.numberToFilesMap)+1)
            self.progressBar.setVisible(True)
            
            if not self.isHdf5:
                if self.mode == 'aise':
                    tiff_files = glob.glob(os.path.join(self.dir_path, '*.tif'))
                else:
                    tiff_files = []
                    for folder in self.folder_paths:
                        tiff_files.extend(glob.glob(os.path.join(self.dir_path, folder, '*.tif')))
                max_intensities = []
                for i, tiff_file in enumerate(tiff_files):
                    image = fabio.open(tiff_file).data.astype(np.float32)
                    max_intensities.append(np.max(image))
                    if self.unalignedImagesDialog.distance_mode != 1: # Don't calculate if image mode has been selected
                        if processData == True:
                            center = getCenter(image)
                            angle = getRotationAngle(image, center)
                        else:
                            info = self.searchInfoCache(tiff_file, infoCache)
                            center = info[1]
                            angle = info[2]
                        addImages(image, center, angle, tiff_file)
                        currentList = [tiff_file, center, angle]
                        infoCache.append(currentList)
                    else:
                        center = 0
                        angle = 0
                        addImages(image, center, angle, tiff_file)
                    self.progressBar.setValue(i)
            else:
                max_intensities = []
                file_names = []
                if self.mode == 'aise':
                    with fabio.open(self.file_name) as series:
                        for frame in series.frames():
                            image = frame.data.astype(np.float32)
                            max_intensities.append(np.max(image))
                            namefile = os.path.split(frame.file_container.filename)[1].split('.')
                            temp_filename = namefile[0] + '_%05i.' %(frame.index + 1) + namefile[1]
                            file_names.append(temp_filename)
                            if self.unalignedImagesDialog.distance_mode != 1: # Don't calculate if image mode has been selected
                                if processData == True:
                                    center = getCenter(image)
                                    angle = getRotationAngle(image, center)
                                else:
                                    info = self.searchInfoCache(temp_filename, infoCache)
                                    center = info[1]
                                    angle = info[2]
                                addImages(image, center, angle, tiff_file)
                                currentList = [temp_filename, center, angle]
                                infoCache.append(currentList)
                            else:
                                center = 0
                                angle = 0
                                addImages(image, center, angle, temp_filename)
                            self.progressBar.setValue(frame.index)
                else:
                    for i in range(self.nbOfExposures):
                        with fabio.open(self.fileList[i]) as series:
                            for frame in series.frames():
                                image = frame.data.astype(np.float32)
                                max_intensities.append(np.max(image))
                                namefile = os.path.split(frame.file_container.filename)[1].split('.')
                                temp_filename = namefile[0] + '_%05i.' %(frame.index + 1) + namefile[1]
                                file_names.append(temp_filename)
                                if self.unalignedImagesDialog.distance_mode != 1: # Don't calculate if image mode has been selected
                                    if processData == True:
                                        center = getCenter(image)
                                        angle = getRotationAngle(image, center)
                                    else:
                                        info = self.searchInfoCache(temp_filename, infoCache)
                                        center = info[1]
                                        angle = info[2]
                                    addImages(image, center, angle, tiff_file)
                                    currentList = [temp_filename, center, angle]
                                    infoCache.append(currentList)
                                else:
                                    center = 0
                                    angle = 0
                                    addImages(image, center, angle, temp_filename)
                                self.progressBar.setValue(frame.index)

            
            inconsistent_images = detectImages(self.dir_path, int(np.percentile(max_intensities, 95)), self.unalignedImagesDialog.distance_mode, { 'image': imageCo, 'center': centerCo, 'angle': angleCo})
            if self.mode == 'aise':
                self.progressBar.setValue(len(self.img_list)+1)
            else:
                self.progressBar.setValue(len(self.numberToFilesMap)+1)
            self.progressBar.setVisible(False)

            if self.unalignedImagesDialog.distance_mode != 1: # Dont save cache as distance_mode 1 (image) does not calculate anything so no info needs to be saved
                self.saveImagesCache(infoCache)

            if inconsistent_images is None or inconsistent_images == []:
                QMessageBox.information(self, "No Unaligned Images", "No unaligned images found")
            else:
                for image in inconsistent_images:
                    self.misaligned_images.append(image.getName())
                if self.mode == 'aise':
                    items = ['Review selected images', 'Ignore all misaligned images']
                    item, ok = QInputDialog.getItem(self, "Select Option", "Misaligned images found, what would you like to do?",  items, 0, False)
                    if ok and item:
                        if item == 'Review selected images':
                            self.selectImageSequence()
                                
                        else:
                            for grps in self.img_grps:
                                grps = list(set(grps).difference(self.misaligned_images)) #don't think this works right now
                            self.onGroupChanged()
                else:
                    misaligned_images_str = "\n".join(self.misaligned_images)
                    QMessageBox.information(self, "Misaligned Images", f"The following images are misaligned:\n{misaligned_images_str}")
                    
                    
                        
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
            self.refreshAllTab()
    
    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        self.statusReport.setText(text)
        QApplication.processEvents()
        
def resizeImage(img, res_size):
    """
    Resize the image.
    """
    if img.shape == res_size:
        return img
    print("Size mismatched, resizing image")
    h, b = img.shape
    resH, resB = res_size
    dH = resH - h
    dB = resB - b
    extraH = dH//2
    extraB = dB//2
    res_img = np.zeros((res_size))
    res_img[extraH:extraH+h, extraB:extraB+b] = img
    return res_img

def getRectanglePatch(center, w, h):
    """
    Give the rectangle patch
    """
    leftTopCorner = (center[0] - w//2, center[1] - h//2)
    sq = patches.Rectangle(leftTopCorner, w, h, linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted')
    return sq
        
