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
import gc
import copy
import numpy as np
import cv2
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
from PIL import Image
import fabio
from musclex import __version__
from .pyqt_utils import *
from ..utils.file_manager import ifHdfReadConvertless, createFolder, getFilesAndHdf
from ..utils.image_processor import processImageForIntCenter, getRotationAngle, getCenter, getNewZoom, rotateImage, averageImages
from ..CalibrationSettings import CalibrationSettings

class AddIntensitiesSingleExp(QMainWindow):
    """
    A class for GUI of Image Merger
    """
    def __init__(self):
        QWidget.__init__(self)
        self.orig_imgs = []
        self.img_list = []
        self.img_grps = []
        self.file_name = ''
        self.initImg = None
        self.avg_img = None
        self.currentFileNumber = 0
        self.currentFrameNumber = 0
        self.currentGroupNumber = 0
        self.center_before_rotation = None
        self.updated = {'img' : False, 'res': False}
        self.uiUpdating = False # update ui status flag (prevent recursive)
        self.calSettings = None
        self.calSettingsDialog = None
        self.img_zoom = None # zoom location of original image (x,y range)
        self.default_img_zoom = None # default zoom calculated after processing image
        self.dir_path = ""
        self.newImgDimension = None
        self.stop_process = False
        self.nbOfFrames = 3
        self.nbOfGroups = 1
        self.function = None
        self.imageAxes = None
        self.isHdf5 = False
        self.fileList = None
        self.chordpoints = []
        self.chordLines = []
        self.info = {}
        self.orig_image_center = None
        self.doubleZoomMode = False
        self.dontShowAgainDoubleZoomMessageResult = False
        self.doubleZoomPt = (0, 0)
        self.doubleZoomAxes = None
        self.initUI()
        self.setConnections()

    def initUI(self):
        """
        Initialize the UI.
        """
        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.setWindowTitle("Muscle X Add Intensities Single Experiment v." + __version__)
        self.centralWidget = QWidget(self)
        self.setCentralWidget(self.centralWidget)
        self.mainLayout = QHBoxLayout(self.centralWidget)

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
        self.imageAxes = self.imageFigure.add_subplot(111)
        self.imageCanvas = FigureCanvas(self.imageFigure)

        self.imageCanvas.setHidden(True)
        self.imageTabLayout.addLayout(self.verImgLayout)
        self.imageTabLayout.addWidget(self.imageCanvas)

        self.displayOptGrpBx = QGroupBox()
        self.displayOptGrpBx.setTitle("Display Options")
        self.displayOptGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
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
        self.persistMaxIntensity = QCheckBox("Persist Max intensity")

        self.imgZoomInB = QPushButton("Zoom in")
        self.imgZoomInB.setCheckable(True)
        self.imgZoomOutB = QPushButton("Full")

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')
        self.dispOptLayout.addWidget(self.minIntLabel, 1, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spminInt, 1, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntLabel, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spmaxInt, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomInB, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 3, 1, 1, 1)
        self.dispOptLayout.addWidget(self.logScaleIntChkBx, 4, 0, 1, 2)
        self.dispOptLayout.addWidget(self.persistMaxIntensity, 5, 0, 1, 2)

        self.displayOptGrpBx.setLayout(self.dispOptLayout)

        self.optionsLayout = QVBoxLayout()
        self.optionsLayout.setAlignment(Qt.AlignCenter)
        self.settingsGroup = QGroupBox("Image Processing")
        self.settingsLayout = QGridLayout()
        self.settingsGroup.setLayout(self.settingsLayout)

        self.frameNb = QSpinBox()
        self.frameNb.setToolTip("Choose the number of frames per group you would like.")
        self.frameNb.setKeyboardTracking(False)
        self.frameNb.setValue(3)
        self.frameNb.setMinimum(2)

        self.avgInsteadOfSum = QCheckBox("Compute Average Instead of Sum")

        self.calibrationChkBx = QCheckBox("Calibrate images")
        self.calibrationButton = QPushButton("Calibration Settings")
        self.setCenterRotationButton = QPushButton("Set Manual Center and Rotation")
        self.setCenterRotationButton.setCheckable(True)
        self.setCentByChords = QPushButton("Set Center by Chords")
        self.setCentByChords.setCheckable(True)
        self.setCentByPerp = QPushButton("Set Center by Perpendiculars")
        self.setCentByPerp.setCheckable(True)
        self.setRotationButton = QPushButton("Set Manual Rotation")
        self.setRotationButton.setCheckable(True)
        self.setFitRegion = QPushButton("Set Region of Interest")
        self.setFitRegion.setCheckable(True)
        self.doubleZoom = QCheckBox("Double Zoom")
        self.dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")

        self.showSeparator = QCheckBox("Show Quadrant Separator")

        self.centerWoRotateChkBx = QCheckBox("Calibrate Center Without Rotation")
        self.centerWoRotateChkBx.setChecked(True)

        self.compressChkBx = QCheckBox('Compress the Resulting Images')

        self.settingsLayout.addWidget(QLabel('Frames/Group'), 0, 0, 1, 2)
        self.settingsLayout.addWidget(self.frameNb, 0, 1, 1, 2)
        self.settingsLayout.addWidget(self.avgInsteadOfSum, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.compressChkBx, 2, 0, 1, 2)
        self.settingsLayout.addWidget(self.calibrationChkBx, 3, 0, 1, 2)
        self.settingsLayout.addWidget(self.calibrationButton, 4, 0, 1, 2)
        self.settingsLayout.addWidget(self.setCenterRotationButton, 5, 0, 1, 2)
        self.settingsLayout.addWidget(self.setRotationButton, 6, 0, 1, 2)
        self.settingsLayout.addWidget(self.setCentByChords, 7, 0, 1, 2)
        self.settingsLayout.addWidget(self.setCentByPerp, 8, 0, 1, 2)
        self.settingsLayout.addWidget(self.setFitRegion, 9, 0, 1, 2)
        self.settingsLayout.addWidget(self.doubleZoom, 10, 0, 1, 2)
        self.settingsLayout.addWidget(self.centerWoRotateChkBx, 11, 0, 1, 2)
        self.settingsLayout.addWidget(self.showSeparator, 12, 0, 1, 2)

        self.calibrationButton.setEnabled(False)
        self.setCenterRotationButton.setEnabled(False)
        self.setRotationButton.setEnabled(False)
        self.setCentByChords.setEnabled(False)
        self.setCentByPerp.setEnabled(False)
        self.setFitRegion.setEnabled(False)
        self.showSeparator.setEnabled(False)
        self.centerWoRotateChkBx.setEnabled(False)
        self.doubleZoom.setEnabled(False)

        self.processFolderButton = QPushButton("Process All Groups")
        self.processFolderButton.setStyleSheet(pfss)
        self.processFolderButton.setCheckable(True)

        self.nextButton = QPushButton("Frame >")
        self.prevButton = QPushButton("< Frame")
        self.nextGrpButton = QPushButton("Group >>>")
        self.prevGrpButton = QPushButton("<<< Group")
        self.filenameLineEdit = QSpinBox()
        self.filenameLineEdit.setMinimum(0)
        self.filenameLineEdit.setKeyboardTracking(False)
        self.buttonsLayout = QGridLayout()
        self.buttonsLayout.addWidget(self.processFolderButton, 0, 0, 1, 2)
        self.buttonsLayout.addWidget(self.prevButton, 1, 0, 1, 1)
        self.buttonsLayout.addWidget(self.nextButton, 1, 1, 1, 1)
        self.buttonsLayout.addWidget(self.prevGrpButton, 2, 0, 1, 1)
        self.buttonsLayout.addWidget(self.nextGrpButton, 2, 1, 1, 1)
        self.buttonsLayout.addWidget(QLabel('Group #'), 3, 0, 1, 1)
        self.buttonsLayout.addWidget(self.filenameLineEdit, 3, 1, 1, 1)

        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingsGroup)

        self.optionsLayout.addStretch()
        self.optionsLayout.addLayout(self.buttonsLayout)
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
        self.resPersistMaxIntensity = QCheckBox("Persist Max intensity")

        self.resultDispOptLayout.addWidget(self.resultminIntLabel, 1, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.spResultminInt, 1, 1, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultmaxIntLabel, 2, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.spResultmaxInt, 2, 1, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultZoomInB, 3, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultZoomOutB, 3, 1, 1, 1)
        self.resultDispOptLayout.addWidget(self.resLogScaleIntChkBx, 4, 0, 1, 2)
        self.resultDispOptLayout.addWidget(self.resPersistMaxIntensity, 5, 0, 1, 2)

        self.resultDispOptGrp.setLayout(self.resultDispOptLayout)

        self.leftLayout.addWidget(self.resultDispOptGrp)
        self.leftLayout.addStretch()

        self.processFolderButton2 = QPushButton("Process All Groups")
        self.processFolderButton2.setStyleSheet(pfss)
        self.processFolderButton2.setCheckable(True)
        self.nextGrpButton2 = QPushButton("Group >>>")
        self.prevGrpButton2 = QPushButton("<<< Group")
        self.filenameLineEdit2 = QSpinBox()
        self.filenameLineEdit2.setMinimum(0)
        self.filenameLineEdit2.setKeyboardTracking(False)
        self.buttonsLayout2 = QGridLayout()
        self.buttonsLayout2.addWidget(self.processFolderButton2, 0, 0, 1, 2)
        self.buttonsLayout2.addWidget(self.prevGrpButton2, 1, 0, 1, 1)
        self.buttonsLayout2.addWidget(self.nextGrpButton2, 1, 1, 1, 1)
        self.buttonsLayout2.addWidget(QLabel('Group #'), 2, 0, 1, 1)
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

        self.show()
        self.setMinimumHeight(900)
        self.setMinimumWidth(1500)

    def setConnections(self):
        """
        Set all triggered functions for widgets
        """
        self.browseFileButton.clicked.connect(self.browseFile)
        self.browseFolderButton.clicked.connect(self.browseFolder)
        self.imgZoomInB.clicked.connect(self.imageZoomIn)
        self.imgZoomOutB.clicked.connect(self.imageZoomOut)
        self.spminInt.valueChanged.connect(self.refreshImageTab)
        self.spmaxInt.valueChanged.connect(self.refreshImageTab)
        self.logScaleIntChkBx.stateChanged.connect(self.refreshImageTab)
        self.doubleZoom.stateChanged.connect(self.doubleZoomChecked)
        self.showSeparator.stateChanged.connect(self.refreshImageTab)
        self.centerWoRotateChkBx.stateChanged.connect(self.centerWoRotateChanged)
        self.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.nextButton.clicked.connect(self.nextClicked)
        self.prevButton.clicked.connect(self.prevClicked)
        self.nextGrpButton.clicked.connect(self.nextGrpClicked)
        self.prevGrpButton.clicked.connect(self.prevGrpClicked)
        self.filenameLineEdit.valueChanged.connect(self.fileNameChanged)

        self.frameNb.valueChanged.connect(self.frameNbChanged)
        self.avgInsteadOfSum.stateChanged.connect(self.avgInsteadOfSumChanged)
        self.compressChkBx.stateChanged.connect(self.compressChanged)
        self.calibrationChkBx.stateChanged.connect(self.setCalibrationActive)
        self.calibrationButton.clicked.connect(self.calibrationClicked)
        self.setCenterRotationButton.clicked.connect(self.setCenterRotation)
        self.setRotationButton.clicked.connect(self.setRotation)
        self.setCentByChords.clicked.connect(self.setCenterByChordsClicked)
        self.setCentByPerp.clicked.connect(self.setCenterByPerpClicked)
        self.setFitRegion.clicked.connect(self.setFitRegionClicked)
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
        self.nextGrpButton2.clicked.connect(self.nextGrpClicked)
        self.prevGrpButton2.clicked.connect(self.prevGrpClicked)
        self.filenameLineEdit2.valueChanged.connect(self.fileName2Changed)

        self.resultFigure.canvas.mpl_connect('button_press_event', self.resultClicked)
        self.resultFigure.canvas.mpl_connect('motion_notify_event', self.resultOnMotion)
        self.resultFigure.canvas.mpl_connect('button_release_event', self.resultReleased)
        self.resultFigure.canvas.mpl_connect('scroll_event', self.resultScrolled)

    def compressChanged(self):
        """
        Triggered when the button is clicked
        """
        self.onGroupChanged()
    
    def avgInsteadOfSumChanged(self):
        """
        Change the resulting image from sum if unchecked to avg if checked
        """
        self.onGroupChanged()

    def centerWoRotateChanged(self):
        """
        Triggered when the button is clicked
        """
        if self.centerWoRotateChkBx.isChecked():
            self.info['manual_rotationAngle'] = [None] * self.nbOfFrames
            self.info['rotationAngle'] = [0] * self.nbOfFrames
        self.deleteInfo(['center'])
        self.onGroupChanged()

    def fileNameChanged(self):
        """
        Triggered when the name of the current file is changed
        """
        fileName = self.filenameLineEdit.value()
        self.filenameLineEdit2.blockSignals(True)
        self.filenameLineEdit2.setValue(fileName)
        self.filenameLineEdit2.blockSignals(False)
        self.currentGroupNumber = fileName - 1
        self.onGroupChanged()

    def fileName2Changed(self):
        """
        Triggered when the name of the current file is changed
        """
        fileName = self.filenameLineEdit2.value()
        self.filenameLineEdit.blockSignals(True)
        self.filenameLineEdit.setValue(fileName)
        self.filenameLineEdit.blockSignals(False)
        self.currentGroupNumber = fileName - 1
        self.onGroupChanged()

    def prevClicked(self):
        """
        Going to the previous image
        """
        if len(self.orig_imgs) > 0:
            if self.img_list != [] and self.currentGroupNumber + 1 == self.nbOfGroups and len(self.img_list)%self.nbOfFrames != 0:
                max_frame = len(self.img_list)%self.nbOfFrames
            else:
                max_frame = self.nbOfFrames
            self.currentFrameNumber = (self.currentFrameNumber - 1) % max_frame
            self.currentFileNumber = self.currentFrameNumber + (self.currentGroupNumber) * self.nbOfFrames
            self.refreshImageTab()

    def nextClicked(self):
        """
        Going to the next image
        """
        if len(self.orig_imgs) > 0:
            if self.img_list != [] and self.currentGroupNumber + 1 == self.nbOfGroups and len(self.img_list)%self.nbOfFrames != 0:
                max_frame = len(self.img_list)%self.nbOfFrames
            else:
                max_frame = self.nbOfFrames
            self.currentFrameNumber = (self.currentFrameNumber + 1) % max_frame
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

    def nextGrpClicked(self):
        """
        Going to the next image
        """
        if len(self.img_grps) > 0:
            self.currentGroupNumber = (self.currentGroupNumber + 1) % self.nbOfGroups
            self.currentFrameNumber = 0
            self.currentFileNumber = self.currentFrameNumber + (self.currentGroupNumber) * self.nbOfFrames
            self.filenameLineEdit.setValue(self.currentGroupNumber + 1)

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

            img = self.orig_imgs[self.currentFrameNumber]
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
                        ax1.lines.pop(i)
                for i in range(len(ax1.patches)-1,-1,-1):
                    ax1.patches.pop(i)
        else:
            self.imageFigure.delaxes(self.doubleZoomAxes)
            self.doubleZoomMode = False
        self.imageCanvas.draw_idle()

    def frameNbChanged(self):
        """
        Change the number of images to add. Will also change the display consequently
        """
        self.nbOfFrames = self.frameNb.value()
        self.currentFrameNumber = 0
        self.currentGroupNumber = 0
        self.currentFileNumber = 0
        self.updateImageGroups()
        self.nbOfGroups = len(self.img_grps)
        self.filenameLineEdit.setMaximum(self.nbOfGroups)
        self.filenameLineEdit2.setMaximum(self.nbOfGroups)
        self.info['manual_rotationAngle'] = [None] * self.nbOfFrames
        self.info['rotationAngle'] = [0] * self.nbOfFrames
        self.onGroupChanged()
        self.resetStatusbar()

    def batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if (self.processFolderButton.isChecked() or self.processFolderButton2.isChecked()) and self.processFolderButton.text() == "Process All Groups":
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
        text += "\nCurrent Settings"
        if self.orig_image_center is not None:
            text += "\n - Center : " + str(self.orig_image_center)
        text += '\n\nAre you sure you want to process ' + str(self.nbOfGroups) + ' groups of ' + str(self.nbOfFrames) + ' frames in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            self.progressBar.setVisible(True)
            self.progressBar.setRange(0, self.nbOfGroups)
            self.stop_process = False
            output = os.path.join(self.dir_path, 'aise_results')
            print('Start merging...')
            for i, imgs in enumerate(self.img_grps):
                if len(imgs) > 0 and not self.stop_process:
                    self.progressBar.setValue(i)
                    self.nextGrpClicked()
                else:
                    break
            self.progressBar.setValue(self.nbOfGroups)
            self.progressBar.setVisible(False)
            self.statusPrint("")
            print("Done. All result images have been saved to "+output)

        self.processFolderButton.setChecked(False)
        self.processFolderButton.setText("Process All Groups")
        self.processFolderButton2.setChecked(False)
        self.processFolderButton2.setText("Process All Groups")

    def processGroup(self):
        """
        Process Image by getting all flags and call process() of QuadrantFolder object
        Then, write data and update UI
        """
        if self.ableToProcess():
            print('Processing...')
            self.statusPrint('Processing...')
            if self.calibrationChkBx.isChecked():
                _, self.orig_image_center = self.getExtentAndCenter(self.orig_imgs[0])
                if not self.centerWoRotateChkBx.isChecked():
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
                    filename = "Group"+str(self.currentGroupNumber + 1)+'.tif'
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
                if self.avgInsteadOfSum.isChecked():
                    # WARNING: in averageImages, we are using preprocessed instead of rotate because rotate is a black box and we already calibrated the images
                    self.avg_img = averageImages(self.orig_imgs, preprocessed=True) ##todo homogenize
                else:
                    sum_img = 0
                    for img in self.orig_imgs:
                        if not isinstance(sum_img, int) and img.shape[0] > sum_img.shape[0]:
                            sum_img = resizeImage(sum_img, img.shape)
                        elif not isinstance(sum_img, int):
                            img = resizeImage(img, sum_img.shape)
                        sum_img += img
                    self.avg_img = sum_img
                print('Saving merged image...')
                self.statusPrint('Saving merged image...')
                if self.compressChkBx.isChecked():
                    tif_img = Image.fromarray(self.avg_img)
                    tif_img.save(os.path.join(output, filename), compression='tiff_lzw')
                else:
                    fabio.tifimage.tifimage(data=self.avg_img).write(os.path.join(output, filename))

            self.refreshAllTab()
            print("Done. Result image have been saved to "+output)

    def refreshAllTab(self):
        """
        Set all tab update status to be not update, and Refresh (Redraw) all tab
        """
        self.updated['img'] = False
        self.updated['res'] = False
        self.function = None
        self.updateUI()
        self.resetStatusbar()

    def refreshImageTab(self):
        """
        Refresh (Redraw) image tab
        """
        self.updated['img'] = False
        self.updateUI()
        self.resetStatusbar()

    def refreshResultTab(self):
        """
        Refresh (Redraw) image tab
        """
        self.updated['res'] = False
        self.updateUI()
        self.resetStatusbar()

    def resetStatusbar(self):
        """
        Reset the status bar
        """
        if self.img_list != [] and self.currentGroupNumber + 1 == self.nbOfGroups and len(self.img_list)%self.nbOfFrames != 0:
            max_frame = len(self.img_list)%self.nbOfFrames
        else:
            max_frame = self.nbOfFrames
        self.imgPathOnStatusBar.setText(
            'Current Frame (' + str(self.currentFrameNumber + 1) + '/' + str(max_frame) + '), Current Group ('
            + str(self.currentGroupNumber + 1) + '/' + str(self.nbOfGroups) + ') : ' + self.img_list[self.currentFileNumber])

    def ableToProcess(self):
        """
        Check if image can be processed
        """
        return self.orig_imgs != [] and not self.uiUpdating

    def updateUI(self):
        """
        Update current all widget in current tab , spinboxes, and refresh status bar
        """
        if self.ableToProcess():
            self.updateImageTab()
            self.updateResultTab()

    def imageClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return
        x = event.xdata
        y = event.ydata

        ax = self.imageAxes

        if self.doubleZoomMode:
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
            elif func[0] == "fit_region":
                if len(func) == 1:
                    # width selected
                    func.append(abs(int(x)))
                    self.imgPathOnStatusBar.setText(
                        "Drag mouse pointer to select height, click on the image to accept (ESC to cancel)")
                else:
                    # both width and height selected
                    extent, center = self.getExtentAndCenter(self.orig_imgs[0])
                    half_height = abs(func[1] - center[0])
                    half_width = abs(abs(int(y)) - center[1])
                    print("Selected Fit Reg W/2 x H/2 ", (half_width, half_height))

                    initImg = self.orig_imgs[self.currentFrameNumber]
                    b, l = initImg.shape
                    if self.newImgDimension is None:
                        dim = int(2.8*max(l, b))
                        self.newImgDimension = dim
                    else:
                        dim = self.newImgDimension
                    scaleX = initImg.shape[0]/dim
                    scaleY = initImg.shape[1]/dim
                    half_width = int(half_width*scaleX)
                    half_height = int(half_height*scaleY)
                    croppedImage = initImg[int(center[1] - half_height):int(center[1] + half_height), int(center[0] - half_width):int(center[0] + half_width)]
                    new_img = np.zeros(initImg.shape)
                    # Placing cropped image in new image such that size of original image matches new image
                    new_img[int(center[1] - half_height):int(center[1] + half_height), int(center[0] - half_width):int(center[0] + half_width)] = croppedImage
                    print("Cropped Image shape ", croppedImage.shape)
                    print("New Image shape ", new_img.shape)
                    self.initImg = None
                    self.newImgDimension = None
                    # self.deleteInfo(['center', 'rotationAngle', 'manual_center', 'manual_rotationAngle'])
                    self.setFitRegion.setChecked(False)
                    self.onGroupChanged()

            elif func[0] == "chords_center":
                self.imgPathOnStatusBar.setText(
                    "Click to place a point (need 3 points), then click on the button to process (ESC to cancel)")
                axis_size = 5
                self.chordpoints.append([x, y])
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                if len(self.chordpoints) >= 3:
                    self.drawPerpendiculars(ax)
                self.imageCanvas.draw_idle()
            elif func[0] == "perp_center":
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

                    extent, center = self.getExtentAndCenter(self.orig_imgs[0])

                    cx = int(round((x1 + x2) / 2.) + extent[0])
                    cy = int(round((y1 + y2) / 2.) + extent[1])
                    new_center = [cx, cy]
                    cx = int(round(new_center[0]))
                    cy = int(round(new_center[1]))
                    self.info['manual_center'] = (cx, cy)
                    #self.orig_image_center = self.info['manual_center']
                    self.info['manual_rotationAngle'][self.currentFrameNumber] = self.info['rotationAngle'][self.currentFrameNumber] + new_angle
                    self.setCenterRotationButton.setChecked(False)
                    self.onGroupChanged()
            elif func[0] == "im_rotate":
                # set rotation angle
                extent, center = self.getExtentAndCenter(self.orig_imgs[0])

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
                self.info['manual_rotationAngle'][self.currentFrameNumber] = self.info['rotationAngle'][self.currentFrameNumber] + new_angle
                self.setRotationButton.setChecked(False)
                self.onGroupChanged()

    def imageOnMotion(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return
        x = event.xdata
        y = event.ydata
        img = self.orig_imgs[self.currentFrameNumber]
        ax = self.imageAxes

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
                                ax1.lines.pop(i)
                        for i in range(len(ax1.patches)-1,-1,-1):
                            ax1.patches.pop(i)
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
                ax.patches.pop(0)
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
        elif func[0] == "fit_region":
            self.imgPathOnStatusBar.setText(
                "Drag mouse pointer to select width, click on the image to accept (ESC to cancel)")
            if self.calSettings is None or 'center' not in self.calSettings:
                self.calSettings = {}
                _, self.calSettings['center'] = self.getExtentAndCenter(self.orig_imgs[0])
            center = self.calSettings['center']
            if len(func) == 2:
                # width selected, change height as cursor moves
                if not self.doubleZoom.isChecked():
                    if len(ax.patches) > 0:
                        for i in range(len(ax.patches) - 1, -1, -1):
                            ax.patches.pop(i)
                    hei = 2*abs(y-center[1])
                    wei = 2*abs(func[1] - center[0])
                    sq = getRectanglePatch(center, wei, hei)
                    ax.add_patch(sq)
                else: 
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines.pop(i)
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    elif self.doubleZoomMode:
                        if len(ax.patches) > 0:
                            for i in range(len(ax.patches) - 1, -1, -1):
                                ax.patches.pop(i)
                        hei = 2*abs(y-center[1])
                        wei = 2*abs(func[1] - center[0])
                        sq = getRectanglePatch(center, wei, hei)
                        ax.add_patch(sq)
            else:
                # nothing is selected, start by changing width
                if not self.doubleZoom.isChecked():
                    if len(ax.patches) > 0:
                        for i in range(len(ax.patches) - 1, -1, -1):
                            ax.patches.pop(i)
                    wei = 2 * abs(x - center[0])
                    sq = getRectanglePatch(center, wei, 50)
                    ax.add_patch(sq)
                else: 
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines.pop(i)
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    elif self.doubleZoomMode:
                        if len(ax.patches) > 0:
                            for i in range(len(ax.patches) - 1, -1, -1):
                                ax.patches.pop(i)
                        wei = 2 * abs(x - center[0])
                        sq = getRectanglePatch(center, wei, 50)
                        ax.add_patch(sq)
            self.imageCanvas.draw_idle()

        elif func[0] == "im_center_rotate":
            self.imgPathOnStatusBar.setText("Click on 2 corresponding reflection peaks along the equator (ESC to cancel)")
            # draw X on points and a line between points
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines) - 1, -1, -1):
                        ax.lines.pop(i)
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines.pop(i)
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    for i in range(len(ax.lines) - 1, 1, -1):
                        ax.lines.pop(i)
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
                                ax1.lines.pop(i)
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.imageCanvas.draw_idle()

        elif func[0] == "perp_center":
            self.imgPathOnStatusBar.setText("Click to create a point (2 points for a line), then click on the button to process (ESC to cancel)")
            # draw X on points and a line between points
            axis_size = 5

            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines) - 1, -1, -1):
                        ax.lines.pop(i)
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines.pop(i)
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    for i in range(len(ax.lines) - 1, 1, -1):
                        ax.lines.pop(i)
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
                                ax1.lines.pop(i)
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) % 2 != 0:
                if len(ax.lines) > 0:
                    n = (len(func)-1)*5//2 + 2
                    for i in range(len(ax.lines) - 1, n - 1, -1):
                        ax.lines.pop(i)
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomAxes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines.pop(i)
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) % 2 == 0:
                start_pt = func[-1]
                if len(ax.lines) > 3:
                    n = len(func) * 5 // 2 - 1
                    for i in range(len(ax.lines) - 1, n - 1, -1):
                        ax.lines.pop(i)
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
                                ax1.lines.pop(i)
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
                            ax1.lines.pop(i)
                    ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.imageCanvas.draw_idle()

        elif func[0] == "im_rotate":
            self.imgPathOnStatusBar.setText(
                "Rotate the line to the pattern equator (ESC to cancel)")
            # draw line as angle
            if self.calSettings is None or 'center' not in self.calSettings:
                self.calSettings = {}
                _, self.calSettings['center'] = self.getExtentAndCenter(self.orig_imgs[0])
            center = self.calSettings['center']
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            if not self.doubleZoom.isChecked():
                for i in range(len(ax.lines)-1,-1,-1):
                    ax.lines.pop(i)
                ax.plot([x, x2], [y, y2], color="g")
            else:
                if (not self.doubleZoomMode) and x < 200 and y < 200:
                    axis_size = 1
                    ax1 = self.doubleZoomAxes
                    if len(ax1.lines) > 0:
                        for i in range(len(ax1.lines)-1,-1,-1):
                            ax1.lines.pop(i)
                    ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                elif self.doubleZoomMode:
                    for i in range(len(ax.lines)-1,-1,-1):
                        ax.lines.pop(i)
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
        if event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img_size = self.orig_imgs[0].shape

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
        img = self.avg_img
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
                ax.patches.pop(0)
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
        img = self.avg_img
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

    def setCalibrationActive(self):
        """
        Checkbox to decide if we want to calibrate the images or not
        """
        if self.calibrationChkBx.isChecked():
            self.calibrationButton.setEnabled(True)
            self.setCenterRotationButton.setEnabled(True)
            self.setRotationButton.setEnabled(True)
            self.setCentByChords.setEnabled(True)
            self.setCentByPerp.setEnabled(True)
            self.setFitRegion.setEnabled(True)
            self.showSeparator.setEnabled(True)
            self.centerWoRotateChkBx.setEnabled(True)
            self.doubleZoom.setEnabled(True)
            _, self.orig_image_center = self.getExtentAndCenter(self.orig_imgs[0])
            self.showSeparator.setChecked(True)
        else:
            self.calibrationButton.setEnabled(False)
            self.setCenterRotationButton.setEnabled(False)
            self.setRotationButton.setEnabled(False)
            self.setCentByChords.setEnabled(False)
            self.setCentByPerp.setEnabled(False)
            self.setFitRegion.setEnabled(False)
            self.showSeparator.setEnabled(False)
            self.showSeparator.setChecked(False)
            self.doubleZoom.setEnabled(False)
            self.doubleZoom.setChecked(False)
            self.centerWoRotateChkBx.setEnabled(False)

    def setFitRegionClicked(self):
        """
        Triggered when the Set fit region button is clicked
        """
        if self.setFitRegion.isChecked():
            ax = self.imageAxes
            for i in range(len(ax.lines) - 1, -1, -1):
                ax.lines.pop(i)
            for i in range(len(ax.patches) - 1, -1, -1):
                ax.patches.pop(i)
            self.imageCanvas.draw_idle()
            self.function = ['fit_region']
        else:
            self.function = None
            self.resetStatusbar()

    def setCenterByPerpClicked(self):
        """
        Prepare for manual center selection using perpendicular peaks
        :return:
        """
        if self.setCentByPerp.isChecked():
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

            extent, _ = self.getExtentAndCenter(self.orig_imgs[0])
            new_center = [cx, cy]  # np.dot(invM, homo_coords)
            # Set new center and rotaion angle , re-calculate R-min
            print("New Center ", new_center)
            self.info['manual_center'] = (
                int(round(new_center[0])) + extent[0], int(round(new_center[1])) + extent[1])
            print("New center after extent ", self.info['manual_center'])
            self.setCentByPerp.setChecked(False)
            self.onGroupChanged()

    def setCenterByChordsClicked(self):
        """
        Prepare for manual rotation center setting by selecting chords
        """
        if self.setCentByChords.isChecked():
            self.chordpoints = []
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

            cx = int(sum([centers[i][0] for i in range(0, len(centers))]) / len(centers))
            cy = int(sum([centers[i][1] for i in range(0, len(centers))]) / len(centers))
            if self.info['rotationAngle'][self.currentFrameNumber] is None:
                M = cv2.getRotationMatrix2D(tuple(self.info['center']), 0, 1)
            else:
                M = cv2.getRotationMatrix2D(tuple(self.info['center']), self.info['rotationAngle'][self.currentFrameNumber], 1)
            invM = cv2.invertAffineTransform(M)
            homo_coords = [cx, cy, 1.]
            new_center = np.dot(invM, homo_coords)
            print("New center ", new_center)
            # Set new center
            self.info['center'] = (int(round(new_center[0])), int(round(new_center[1])))
            self.setCentByChords.setChecked(False)
            self.onGroupChanged()

    def drawPerpendiculars(self, ax):
        """
        Draw perpendiculars on the image
        """
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
            _, center = self.getExtentAndCenter(self.orig_imgs[0])
            self.info['center'] = center
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
            self.onGroupChanged()
            self.refreshAllTab()

    def getExtentAndCenter(self, orig_img):
        """
        Give the extent and the center of the image
        """
        if self.orig_imgs == []:
            return [0, 0], (0, 0)
        if self.orig_image_center is None:
            self.findCenter(orig_img)
            self.statusPrint("Done.")
        if 'calib_center' in self.info:
            center = self.info['calib_center']
        elif 'manual_center' in self.info:
            center = self.info['manual_center']
        else:
            center = self.orig_image_center
        if 'center' not in self.info:
            self.info['center'] = self.orig_image_center
        extent = [self.info['center'][0] - center[0], self.info['center'][1] - center[1]]
        return extent, center

    def findCenter(self, orig_img):
        """
        Find center of the diffraction. The center will be kept in self.info["center"].
        Once the center is calculated, the rotation angle will be re-calculated, so self.info["rotationAngle"] is deleted
        """
        self.statusPrint("Finding Center...")
        if 'center' in self.info:
            return
        if 'calib_center' in self.info:
            self.info['center'] = self.info['calib_center']
            return
        if 'manual_center' in self.info:
            self.info['center'] = self.info['manual_center']
            return
        print("Center is being calculated ... ")
        self.orig_imgs[0], self.info['center'] = processImageForIntCenter(orig_img, getCenter(orig_img))
        if self.orig_image_center is None:
            self.orig_image_center = self.info['center']
        print("Done. Center = "+str(self.info['center']))

    def setCalibrationImage(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or force to open
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        _, center = self.getExtentAndCenter(self.orig_imgs[0])
        if self.calSettingsDialog is None:
            self.calSettingsDialog = CalibrationSettings(self.dir_path) if self.orig_imgs == [] else \
                CalibrationSettings(self.dir_path, center=center)
        self.calSettings = None
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
                return True
        return False

    def setCenterRotation(self):
        """
        Trigger when set center and rotation angle button is pressed
        """
        if self.setCenterRotationButton.isChecked():
            # clear plot
            self.imageCanvas.draw_idle()
            self.function = ["im_center_rotate"]
        else:
            self.function = None
            self.resetStatusbar()

    def onNewFileSelected(self):
        """
        Preprocess folder of the file and process current image
        :param newFile: full name of selected file
        """
        createFolder(os.path.join(self.dir_path, 'aise_results'))
        self.browseFolderButton.setHidden(True)
        self.browseFileButton.setHidden(True)
        self.imageCanvas.setHidden(False)
        self.frameNb.setMaximum(len(self.img_list))
        self.nbOfGroups = len(self.img_grps)
        self.filenameLineEdit.setMaximum(self.nbOfGroups)
        self.filenameLineEdit2.setMaximum(self.nbOfGroups)
        self.info['manual_rotationAngle'] = [None] * self.nbOfFrames
        self.info['rotationAngle'] = [0] * self.nbOfFrames
        self.onGroupChanged()

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
                with fabio.open(file_name) as series:
                    for frame in series.frames():
                        namefile = os.path.split(frame.file_container.filename)[1].split('.')
                        temp_filename = namefile[0] + '_%05i.' %(frame.index + 1) + namefile[1]
                        self.img_list.append(temp_filename)
                self.img_list.sort()
                self.updateImageGroups()
                self.onNewFileSelected()
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

    def browseFolder(self):
        """
        Same as browse files but allow the user to select a folder instead of a file.
        """
        self.dir_path = getAFolder()
        if len(self.dir_path) > 0:
            self.isHdf5 = False
            self.preprocessfolder()
            self.onNewFileSelected()

    def preprocessfolder(self):
        """
        Get all image names in the selected folder
        """
        imgs, _ = getFilesAndHdf(self.dir_path)
        self.img_list = sorted(imgs)
        self.updateImageGroups()

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

    def onGroupChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new QuadrantFolder object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        self.statusPrint("Processing...")
        self.filenameLineEdit.setValue(self.currentGroupNumber + 1)
        self.orig_imgs = []
        start = self.nbOfFrames*(self.currentGroupNumber)
        end = min(len(self.img_list), start + self.nbOfFrames)
        for i in range(start, end):
            if self.isHdf5:
                with fabio.open(self.file_name) as series:
                    frame = series.get_frame(i).data
                self.orig_imgs.append(ifHdfReadConvertless(self.img_list[i], frame).astype(np.float32))
            else:
                self.orig_imgs.append(fabio.open(os.path.join(self.dir_path, self.img_list[i])).data.astype(np.float32))
        self.imgDetailOnStatusBar.setText(
            str(self.orig_imgs[0].shape[0]) + 'x' + str(self.orig_imgs[0].shape[1]) + ' : ' + str(self.orig_imgs[0].dtype))
        self.processGroup()
        self.initialWidgets(self.orig_imgs[0], self.avg_img)
        self.statusPrint("")
        gc.collect()

    def initialWidgets(self, img, result):
        """
        Initial some widgets values which depends on current image
        :param img: selected image
        """
        self.uiUpdating = True
        min_val = img.min()
        max_val = img.max()
        self.spmaxInt.setRange(min_val, max_val)
        if not self.persistMaxIntensity.isChecked():
            self.spmaxInt.setValue(max_val * .5)
        self.spmaxInt.setSingleStep(max_val * .05)
        self.spminInt.setRange(min_val, max_val)
        self.spminInt.setValue(min_val)
        self.spminInt.setSingleStep(max_val * .05)

        self.minIntLabel.setText("Min Intensity ("+str(min_val)+")")
        self.maxIntLabel.setText("Max Intensity (" + str(max_val) + ")")

        min_val = result.min()
        max_val = result.max()
        self.spResultmaxInt.setRange(min_val, max_val)
        if not self.resPersistMaxIntensity.isChecked():
            self.spResultmaxInt.setValue(max_val * .5)
        self.spResultmaxInt.setSingleStep(max_val * .05)
        self.spResultminInt.setRange(min_val, max_val)
        self.spResultminInt.setValue(min_val)
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

    def updateImageTab(self):
        """
        Display image in image tab, and draw lines
        """
        if not self.updated['img']:
            self.uiUpdating = True

            if self.calibrationChkBx.isChecked():
                extent, center = self.getExtentAndCenter(self.orig_imgs[0])
            else:
                extent, center = [0, 0], (0, 0)

            self.plotImages(self.imageAxes, self.orig_imgs[self.currentFrameNumber], extent)

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

            if self.calibrationChkBx.isChecked():
                extent, _ = self.getExtentAndCenter(self.orig_imgs[0])
            else:
                extent, _ = [0, 0], (0, 0)

            ax = self.resultAxes
            img = self.avg_img
            ax.cla()

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

    def plotImages(self, imageAxes, img, extent):
        """
        Displays the image
        """
        ax = imageAxes
        ax.cla()

        if self.logScaleIntChkBx.isChecked():
            ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
        else:
            ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
        ax.set_facecolor('black')

        if self.showSeparator.isChecked() and self.orig_image_center is not None:
            # Draw quadrant separator
            ax.axvline(self.orig_image_center[0], color='y')
            ax.axhline(self.orig_image_center[1], color='y')

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

    def deleteInfo(self, delList):
        """
        Remove input keys from info dict of current QuadrantFolder object
        :param delList: list of keys
        """
        if self.ableToProcess():
            for inf in delList:
                if inf in self.info.keys():
                    del self.info[inf]

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
                # _, center = self.getExtentAndCenter(img)
                self.info['rotationAngle'][index] = getRotationAngle(img, self.orig_image_center, 0)
        print("Done. Rotation Angle is " + str(self.info['rotationAngle'][index]) +" degree")
        self.statusPrint("")

    def getRotatedImage(self, index):
        """
        Get rotated image by angle while image = original input image, and angle = self.info["rotationAngle"]
        """
        img = np.array(self.orig_imgs[index], dtype="float32")
        center = self.info["center"]
        if 'manual_center' in self.info:
            center = self.info["manual_center"]
        if self.center_before_rotation is not None:
            center = self.center_before_rotation

        b, l = img.shape
        rotImg, newCenter, _ = rotateImage(img, center, self.info["rotationAngle"][index])

        # Cropping off the surrounding part since we had already expanded the image to maximum possible extent in centerize image
        bnew, lnew = rotImg.shape
        db, dl = (bnew - b)//2, (lnew-l)//2
        final_rotImg = rotImg[db:bnew-db, dl:lnew-dl]
        self.info["center"] = (newCenter[0]-dl, newCenter[1]-db)

        return final_rotImg

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        self.statusReport.setText(text)
        QApplication.processEvents()

    def showAbout(self):
        """
        Display About Dialog
        """
        msgBox = QMessageBox()
        msgBox.setWindowTitle("About")
        msgBox.setTextFormat(Qt.RichText)
        msgBox.setText("<br><br><br>" +
                       "Add Itensities Single Experiment (former Image Merger) is running under" +
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

def getRectanglePatch(center, w, h):
    """
    Give the rectangle patch
    """
    leftTopCorner = (center[0] - w//2, center[1] - h//2)
    sq = patches.Rectangle(leftTopCorner, w, h, linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted')
    return sq

def calcSlope(pt1, pt2):
    """
    Compute the slope using 2 points.
    :param pt1, pt2: 2 points
    :return: slope
    """
    if pt1[0] == pt2[0]:
        return float('inf')
    return (pt2[1] - pt1[1]) / (pt2[0] - pt1[0])

def getPerpendicularLineHomogenous(p1, p2):
    """
    Give the perpendicular line homogeneous
    """
    b1 = (p2[1] - p1[1]) / (p2[0] - p1[0]) if p1[0] != p2[0] else float('inf')
    chord_cent = [(p2[0] + p1[0]) / 2, (p2[1] + p1[1]) / 2, 1]
    print("Chord_cent1 ", chord_cent)
    if b1 == 0:
        return float('inf'), chord_cent
    if b1 == float('inf'):
        return 0, chord_cent
    return -1 / b1, chord_cent

def getIntersectionOfTwoLines(line1, line2):
    """
    Finds intersection of lines line1 = [p1,p2], line2 = [p3,p4]
    :param line1:
    :param line2:
    :return:
    """
    p1, p2 = line1
    p3, p4 = line2
    slope1 = (p2[1] - p1[1]) / (p2[0] - p1[0])
    if p4[0] != p3[0]:
        slope2 = (p4[1] - p3[1]) / (p4[0] - p3[0])
        x = (p3[1] - p1[1] + slope1*p1[0] - slope2*p3[0]) / (slope1 - slope2)
        y = slope1*(x - p1[0]) + p1[1]
    else:
        # Slope2 is inf
        x = p4[0]
        y = slope1 * (x - p1[0]) + p1[1]

    return (int(x), int(y))

def resizeImage(img, res_size):
    """
    Resize the image.
    """
    print("Size mismatched, resizing image")
    if img.shape == res_size:
        return img
    h, b = img.shape
    resH, resB = res_size
    dH = resH - h
    dB = resB - b
    extraH = dH//2
    extraB = dB//2
    res_img = np.zeros((res_size))
    res_img[extraH:extraH+h, extraB:extraB+b] = img
    return res_img
