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
import os
from os.path import isfile, abspath
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
import argparse
import collections
import fabio
import cv2
import musclex
from .pyqt_utils import *
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.ScanningDiffraction import *
from .DIImageWindow import DIImageWindow
from ..CalibrationSettings import CalibrationSettings

class AddIntensities(QMainWindow):
    """
    Add Intensities is a program which is designed to be used with a series
    of images placed across multiple folders. It takes the sum of the images
    in each folder which have the same number (For example, F_001.tif in Folder A
    will map to FP_001.tif in folder B). The matched images are then summed
    together and the resultant sum image is stored in ai results folder
    in the selected directory.
    """
    resizeCompleted = pyqtSignal()
    def __init__(self):
        QWidget.__init__(self)
        self.widgetList = []
        self.numberToFilesMap = None
        self.orig_imgs = []
        self.initImg = None
        self.currentFileNumber = 1
        self.updated = False
        self.uiUpdating = False # update ui status flag (prevent recursive)
        self.calSettingsDialog = None
        self.img_zoom = None # zoom location of original image (x,y range)
        self.default_img_zoom = None # default zoom calculated after processing image
        self.dir_path = ""
        self.newImgDimension = None
        self.function = None
        self.imageAxes = None
        self.imageAxes2 = None
        self.imageAxes3 = None
        self.imageAxes4 = None
        self.imageAxes5 = None
        self.imageAxes6 = None
        self.imageAxes7 = None
        self.imageAxes8 = None
        self.axClicked = None
        self.centImgTransMat = None
        self.info = {}
        self.orig_image_center = None
        self.initUI()
        self.setConnections()

    def initUI(self):
        """
        Initialize the UI.
        """
        self.setWindowTitle("Muscle X Add Intensities v." + musclex.__version__)
        self.centralWidget = QWidget(self)
        self.setCentralWidget(self.centralWidget)
        self.mainLayout = QHBoxLayout(self.centralWidget)

        ## display browse folder buttons when program started
        self.verImgLayout = QVBoxLayout()
        self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)

        self.browseFolderButton = QPushButton("Select a Folder...")
        self.verImgLayout.addWidget(self.browseFolderButton)
        self.browseFolderButton.setFixedHeight(100)
        self.browseFolderButton.setFixedWidth(300)
        self.mainLayout.addLayout(self.verImgLayout)

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
        self.imgPathOnStatusBar.setText("  Please select a folder to process")
        self.statusBar.addPermanentWidget(self.statusReport)
        self.statusBar.addPermanentWidget(self.imgCoordOnStatusBar)
        self.statusBar.addPermanentWidget(self.imgDetailOnStatusBar)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.statusBar.addWidget(QLabel("    "))
        self.statusBar.addWidget(self.imgPathOnStatusBar)
        self.setStatusBar(self.statusBar)

        # Menubar
        selectFolderAction = QAction('Select a Folder...', self)
        selectFolderAction.setShortcut('Ctrl+F')
        selectFolderAction.triggered.connect(self.browseFolder)
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectFolderAction)

        self.verImgLayout.addWidget(self.browseFolderButton)
        self.imageFigure = plt.figure()
        self.imageCanvas = FigureCanvas(self.imageFigure)

        self.imageCanvas.setHidden(True)
        self.mainLayout.addWidget(self.imageCanvas)

        self.displayOptGrpBx = QGroupBox()
        self.displayOptGrpBx.setTitle("Display Options")
        self.displayOptGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
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

        self.exposureNb = QSpinBox()
        self.exposureNb.setToolTip("Choose the number of exposures (images) you would like to add.")
        self.exposureNb.setKeyboardTracking(False)
        self.exposureNb.setValue(2)
        self.exposureNb.setMaximum(8)
        self.exposureNb.setMinimum(2)

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

        self.settingsLayout.addWidget(QLabel('Exposures'), 0, 0, 1, 2)
        self.settingsLayout.addWidget(self.exposureNb, 0, 1, 1, 2)
        self.settingsLayout.addWidget(self.calibrationChkBx, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.calibrationButton, 2, 0, 1, 2)
        self.settingsLayout.addWidget(self.setCenterRotationButton, 3, 0, 1, 2)
        self.settingsLayout.addWidget(self.setRotationButton, 4, 0, 1, 2)
        self.settingsLayout.addWidget(self.setCentByChords, 5, 0, 1, 2)
        self.settingsLayout.addWidget(self.setCentByPerp, 6, 0, 1, 2)
        self.settingsLayout.addWidget(self.setFitRegion, 7, 0, 1, 2)

        self.calibrationButton.setEnabled(False)
        self.setCenterRotationButton.setEnabled(False)
        self.setRotationButton.setEnabled(False)
        self.setCentByChords.setEnabled(False)
        self.setCentByPerp.setEnabled(False)
        self.setFitRegion.setEnabled(False)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet(pfss)
        self.processFolderButton.setCheckable(True)

        self.nextButton = QPushButton()
        self.nextButton.setText(">>>")
        self.prevButton = QPushButton()
        self.prevButton.setText("<<<")
        self.filenameLineEdit = QSpinBox()
        self.filenameLineEdit.setMinimum(0)
        self.filenameLineEdit.setKeyboardTracking(False)
        self.buttonsLayout = QGridLayout()
        self.buttonsLayout.addWidget(self.processFolderButton,0,0,1,2)
        self.buttonsLayout.addWidget(self.prevButton,1,0,1,1)
        self.buttonsLayout.addWidget(self.nextButton,1,1,1,1)
        self.buttonsLayout.addWidget(QLabel('Images #'),2,0,1,1)
        self.buttonsLayout.addWidget(self.filenameLineEdit,2,1,1,1)

        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingsGroup)

        self.optionsLayout.addStretch()
        self.optionsLayout.addLayout(self.buttonsLayout)
        self.frameOfKeys = QFrame()
        self.frameOfKeys.setFixedWidth(350)
        self.frameOfKeys.setLayout(self.optionsLayout)
        self.mainLayout.addWidget(self.frameOfKeys)

        self.show()
        self.setMinimumHeight(800)
        self.setMinimumWidth(1500)

    def setConnections(self):
        """
        Set all triggered functions for widgets
        """
        self.browseFolderButton.clicked.connect(self.browseFolder)
        self.spminInt.valueChanged.connect(self.refreshImageTab)
        self.spmaxInt.valueChanged.connect(self.refreshImageTab)
        self.logScaleIntChkBx.stateChanged.connect(self.refreshImageTab)
        self.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.nextButton.clicked.connect(self.nextClicked)
        self.prevButton.clicked.connect(self.prevClicked)
        self.filenameLineEdit.valueChanged.connect(self.fileNameChanged)

        self.exposureNb.valueChanged.connect(self.exposureNbChanged)
        self.imgZoomInB.clicked.connect(self.imageZoomIn)
        self.imgZoomOutB.clicked.connect(self.imageZoomOut)
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

    def fileNameChanged(self):
        """
        Triggered when the name of the current file is changed
        """
        fileName = self.filenameLineEdit.value()
        if self.numberToFilesMap[fileName] == []:
            return
        self.currentFileNumber = fileName
        self.onImageChanged()

    def prevClicked(self):
        """
        Going to the previous image
        """
        self.addIntensity(self.orig_imgs, self.dir_path, self.currentFileNumber)
        if len(self.numberToFilesMap) > 0:
            if self.currentFileNumber == 1:
                self.currentFileNumber = len(self.numberToFilesMap)
            else:
                self.currentFileNumber = (self.currentFileNumber - 1) % len(self.numberToFilesMap)
            self.filenameLineEdit.setValue(self.currentFileNumber)
            # self.onImageChanged()

    def nextClicked(self):
        """
        Going to the next image
        """
        self.addIntensity(self.orig_imgs, self.dir_path, self.currentFileNumber)
        if len(self.numberToFilesMap) > 0:
            if self.currentFileNumber == len(self.numberToFilesMap) - 1:
                self.currentFileNumber += 1
            else:
                self.currentFileNumber = (self.currentFileNumber + 1) % len(self.numberToFilesMap)
            self.filenameLineEdit.setValue(self.currentFileNumber)
            # self.onImageChanged()

    def exposureNbChanged(self):
        """
        Change the number of images to add. Will also change the display consequently
        """
        self.nbOfExposures = self.exposureNb.value()
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
        self.onImageChanged()

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

    def processFolder(self):
        """
        Triggered when a folder has been selected to process it
        """
        errMsg = QMessageBox()
        errMsg.setText('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'

        text += "\nCurrent Settings"
        if self.orig_image_center is not None:
            text += "\n  - Center : " + str(self.orig_image_center)

        text += '\n\nAre you sure you want to process ' + str(len(self.numberToFilesMap)) + ' exposures in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            self.progressBar.setVisible(True)
            self.stop_process = False
            for i in range(len(self.numberToFilesMap)):
                if self.stop_process:
                    break
                self.progressBar.setValue(int(100. / len(self.numberToFilesMap) * i))
                QApplication.processEvents()
                self.nextClicked()
            self.progressBar.setVisible(False)

        self.processFolderButton.setChecked(False)
        self.processFolderButton.setText("Process Current Folder")

    def refreshImageTab(self):
        """
        Refresh (Redraw) image tab
        """
        self.updated = False
        self.function = None
        self.axClicked = None
        self.updateUI()
        self.resetStatusbar()
    
    def resetStatusbar(self):
        """
        Reset the status bar
        """
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(self.currentFileNumber) + '/' + str(len(self.numberToFilesMap)) + ') : ' + self.dir_path)

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

    def imageClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return
        x = event.xdata
        y = event.ydata

        if event.inaxes == self.imageAxes.axes:
            ax = self.imageAxes
            index = 1
        elif event.inaxes == self.imageAxes2.axes:
            ax = self.imageAxes2
            index = 2
        elif self.imageAxes3 is not None and event.inaxes == self.imageAxes3.axes:
            ax = self.imageAxes3
            index = 3
        elif self.imageAxes4 is not None and event.inaxes == self.imageAxes4.axes:
            ax = self.imageAxes4
            index = 4
        elif self.imageAxes5 is not None and event.inaxes == self.imageAxes5.axes:
            ax = self.imageAxes5
            index = 5
        elif self.imageAxes6 is not None and event.inaxes == self.imageAxes6.axes:
            ax = self.imageAxes6
            index = 6
        elif self.imageAxes7 is not None and event.inaxes == self.imageAxes7.axes:
            ax = self.imageAxes7
            index = 7
        elif self.imageAxes8 is not None and event.inaxes == self.imageAxes8.axes:
            ax = self.imageAxes8
            index = 8
        else:
            return

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
                    self.refreshImageTab()
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

                    dim = self.newImgDimension
                    initImg = self.initImg
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
                    self.orig_img = new_img
                    self.initImg = None
                    self.newImgDimension = None
                    self.deleteInfo(['center', 'rotationAngle', 'manual_center', 'manual_rotationAngle'])
                    self.setFitRegion.setChecked(False)
                    self.centImgTransMat = None
                    self.processImage()

            elif func[0] == "chords_center":
                axis_size = 1
                self.chordpoints.append([x, y])
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                if len(self.chordpoints) >= 3:
                    self.drawPerpendiculars()
                self.imageCanvas.draw_idle()
            elif func[0] == "perp_center":
                axis_size = 5
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                self.imageCanvas.draw_idle()
                func.append((x, y))
            elif func[0] == "im_center_rotate" and self.axClicked is not None:
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
                    if 'rotationAngle' not in self.info:
                        self.info['rotationAngle'] = 0
                    self.info['manual_rotationAngle'] = self.info['rotationAngle'] + new_angle
                    self.setCenterRotationButton.setChecked(False)
                    self.processImage()
            elif func[0] == "im_rotate" and self.axClicked is not None:
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
                if 'rotationAngle' not in self.info:
                    self.info['rotationAngle'] = 0
                self.info['manual_rotationAngle'] = self.info['rotationAngle'] + new_angle
                self.setRotationButton.setChecked(False)
                self.processImage()
        self.axClicked = ax

    def imageOnMotion(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return
        x = event.xdata
        y = event.ydata
        img = self.orig_imgs[0]

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
                return

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < img.shape[1] and y < img.shape[0]:
                self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]))

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
            if self.calSettings is None or 'center' not in self.calSettings:
                self.calSettings = {}
                extent, self.calSettings['center'] = self.getExtentAndCenter(self.orig_imgs[0])
            center = self.calSettings['center']
            if len(func) == 2:
                # width selected, change height as cursor moves
                if len(ax.patches) > 0:
                    for i in range(len(ax.patches)-1,-1,-1):
                        ax.patches.pop(i)
                hei = 2*abs(y-center[1])
                wei = 2*abs(func[1] - center[0])
                sq = self.getRectanglePatch(center, wei, hei)
                ax.add_patch(sq)
            else:
                # nothing is selected, start by changing width
                if len(ax.patches) > 0:
                    for i in range(len(ax.patches)-1,-1,-1):
                        ax.patches.pop(i)
                if self.calSettings is None or 'center' not in self.calSettings:
                    self.calSettings = {}
                    _, self.calSettings['center'] = self.getExtentAndCenter(self.orig_imgs[0])
                center = self.calSettings['center']
                wei = 2 * abs(x - center[0])
                sq = self.getRectanglePatch(center, wei, 50)
                ax.add_patch(sq)
            self.imageCanvas.draw_idle()

        elif func[0] == "im_center_rotate" and self.axClicked is not None:
            # draw X on points and a line between points
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        ax.lines.pop(i)
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    # first_cross = ax.lines[:2]
                    for i in range(len(ax.lines)-1,1,-1):
                        ax.lines.pop(i)
                    # ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            self.imageCanvas.draw_idle()

        elif func[0] == "perp_center":
            # draw X on points and a line between points
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5

            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        ax.lines.pop(i)
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    # first_cross = ax.lines[:2]
                    for i in range(len(ax.lines)-1,1,-1):
                        ax.lines.pop(i)
                    # ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')

            elif len(func) % 2 != 0:
                if len(ax.lines) > 0:
                    n = (len(func)-1)*5//2 + 2
                    # first_cross = ax.lines[:n]
                    for i in range(len(ax.lines)-1,n-1,-1):
                        ax.lines.pop(i)
                    # ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) % 2 == 0:
                start_pt = func[-1]
                if len(ax.lines) > 3:
                    n = len(func) * 5 // 2 - 1
                    # first_cross = ax.lines[:n]
                    for i in range(len(ax.lines)-1,n-1,-1):
                        ax.lines.pop(i)
                    # ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            self.imageCanvas.draw_idle()

        elif func[0] == "im_rotate" and self.axClicked is not None:
            # draw line as angle
            if self.calSettings is None or 'center' not in self.calSettings:
                self.calSettings = {}
                extent, self.calSettings['center'] = self.getExtentAndCenter(self.orig_imgs[0])
            center = self.calSettings['center']
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines.pop(i)
            ax.plot((x, x2), (y, y2), color='g')
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
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines.pop(i)
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches.pop(i)
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
            self.refreshImageTab()

    def getRectanglePatch(self, center, w, h):
        """
        Give the rectangle patch
        """
        leftTopCorner = (center[0] - w//2, center[1] - h//2)
        sq = patches.Rectangle(leftTopCorner, w, h, linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted')
        return sq

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
        else:
            self.calibrationButton.setEnabled(False)
            self.setCenterRotationButton.setEnabled(False)
            self.setRotationButton.setEnabled(False)
            self.setCentByChords.setEnabled(False)
            self.setCentByPerp.setEnabled(False)
            self.setFitRegion.setEnabled(False)

    def setFitRegionClicked(self):
        """
        Triggered when the Set fit region button is clicked
        """
        if self.setFitRegion.isChecked():
            self.imgPathOnStatusBar.setText(
                "Drag mouse pointer to select width, click on the image to accept (ESC to cancel)")
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines.pop(i)
            for i in range(len(ax.patches)-1,-1,-1):
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
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines.pop(i)
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches.pop(i)
            self.imageCanvas.draw_idle()
            self.function = ["perp_center"]  # set current active function
        else:
            QApplication.restoreOverrideCursor()

            func = self.function
            horizontalLines = []
            verticalLines = []
            intersections = []
            for i in range(1, len(func) - 1, 2):
                slope = self.calcSlope(func[i], func[i + 1])
                if abs(slope) > 1:
                    verticalLines.append((func[i], func[i + 1]))
                else:
                    horizontalLines.append((func[i], func[i + 1]))
            for line1 in verticalLines:
                for line2 in horizontalLines:
                    cx, cy = self.getIntersectionOfTwoLines(line2, line1)
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
            if 'center' in self.info:
                del self.info['center']
            print("New center after extent ", self.info['manual_center'])
            self.setCentByPerp.setChecked(False)
            self.processImage()

    def calcSlope(self, pt1, pt2):
        """
        Compute the slope using 2 points.
        :param pt1, pt2: 2 points
        :return: slope
        """
        if pt1[0] == pt2[0]:
            return float('inf')
        return (pt2[1] - pt1[1]) / (pt2[0] - pt1[0])

    def setCenterByChordsClicked(self):
        """
        Prepare for manual rotation center setting by selecting chords
        """
        if self.setCentByChords.isChecked():
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines.pop(i)
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches.pop(i)
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

            extent, center = self.getExtentAndCenter(self.orig_imgs[0])

            cx = int(sum([centers[i][0] for i in range(0, len(centers))]) / len(centers))
            cy = int(sum([centers[i][1] for i in range(0, len(centers))]) / len(centers))
            new_center = [cx, cy] #np.dot(invM, homo_coords)
            print("New center ", new_center)
            # Set new center and rotaion angle , re-calculate R-min
            self.info['manual_center'] = (int(round(new_center[0])) + extent[0], int(round(new_center[1])) + extent[1])
            if 'center' in self.info:
                del self.info['center']
            print("New center after extent ", self.info['manual_center'])
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
                slope, cent = self.getPerpendicularLineHomogenous(p1, p2)
                if slope == float('inf'):
                    y_vals = np.array(ax.get_ylim())
                    x_vals = cent[0] + np.zeros(y_vals.shape)
                    self.chordLines.append([slope, cent[0]])
                else:
                    x_vals = np.array(ax.get_xlim())
                    y_vals = (x_vals - cent[0]) * slope + cent[1]
                    self.chordLines.append([slope, cent[1] - slope * cent[0]])
                ax.plot(x_vals, y_vals, linestyle='dashed', color='b')

    def getPerpendicularLineHomogenous(self, p1, p2):
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

    def getIntersectionOfTwoLines(self, line1, line2):
        """
        Finds intersection of lines line1 = [p1,p2], line2 = [p3,p4]
        :param line1:
        :param line2:
        :return:
        """
        p1,p2 = line1
        p3,p4 = line2
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
            #     ax.lines.pop(i)
            # for i in range(len(ax.patches)-1,-1,-1):
            #     ax.patches.pop(i)
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
            self.deleteInfo(['rotationAngle'])
            self.processImage()
            self.refreshImageTab()

    def centerizeImage(self, img, index):
        """
        Create an enlarged image such that image center is at the center of new image
        """
        self.statusPrint("Centererizing image...")
        center = self.orig_image_center
        """
        if self.centImgTransMat is not None and 'calib_center' not in self.info:
            # convert center in initial img coordinate system
            M = self.centImgTransMat
            M[0,2] = -1*M[0,2]
            M[1,2] = -1*M[1,2]
            center = [center[0], center[1], 1]
            center = np.dot(M, center)
            if 'manual_center' in self.info:
                self.info['manual_center'] = (int(center[0]), int(center[1]))
            if 'calib_center' in self.info:
                self.info['calib_center'] = (int(center[0]), int(center[1]))
        """
        center = (int(center[0]), int(center[1]))

        print("Dimension of image before centerize ", img.shape)

        b, l = img.shape
        if self.newImgDimension is None:
            dim = int(2.8*max(l, b))
            self.newImgDimension = dim
        else:
            dim = self.newImgDimension
        new_img = np.zeros((dim,dim)).astype("float32")
        new_img[0:b,0:l] = img

        #Translate image to appropriate position
        transx = int(((dim/2) - center[0]))
        transy = int(((dim/2) - center[1]))
        M = np.float32([[1,0,transx],[0,1,transy]])
        self.centImgTransMat = M
        rows,cols = new_img.shape
        mask_thres = -1
        print(center)

        if self.img_type == "PILATUS":
            mask = np.zeros((new_img.shape[0], new_img.shape[1]), dtype=np.uint8)
            mask[new_img <= mask_thres] = 255
            cv2.setNumThreads(1) # Added to prevent segmentation fault due to cv2.warpAffine
            translated_Img = cv2.warpAffine(new_img, M, (cols, rows))
            translated_mask = cv2.warpAffine(mask, M, (cols, rows))
            translated_mask[translated_mask > 0.] = 255
            translated_Img[translated_mask > 0] = mask_thres
        else:
            cv2.setNumThreads(1) # Added to prevent segmentation fault due to cv2.warpAffine
            translated_Img = cv2.warpAffine(new_img,M,(cols,rows))

        self.orig_imgs[index] = translated_Img
        self.info['center'] = (int(dim / 2), int(dim / 2))
        self.center_before_rotation = (int(dim / 2), int(dim / 2))
        print("Dimension of image after centerize ", self.orig_imgs[index].shape)
        self.statusPrint("")

    def getExtentAndCenter(self, orig_img):
        """
        Give the extent and the center of the image
        """
        if self.orig_imgs == []:
            return [0,0], (0,0)
        if self.orig_image_center is None:
            self.findCenter(orig_img)
            self.statusPrint("Done.")
        if 'calib_center' in self.info:
            center = self.info['calib_center']
        elif 'manual_center' in self.info:
            center = self.info['manual_center']
        else:
            center = self.orig_image_center
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
        self.orig_imgs[0], self.info['center'] = processImageForIntCenter(orig_img, getCenter(orig_img), self.img_type, -1)
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
            self.axClicked = None
            # clear plot
            self.imgPathOnStatusBar.setText("Click on 2 corresponding reflection peaks along the equator (ESC to cancel)")
            # ax = self.imageAxes
            # for i in range(len(ax.lines)-1,-1,-1):
            #     ax.lines.pop(i)
            # for i in range(len(ax.patches)-1,-1,-1):
            #     ax.patches.pop(i)
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
        self.browseFolderButton.setHidden(True)
        self.imageCanvas.setHidden(False)
        self.exposureNb.setMaximum(len(self.numberToFilesMap[1]))
        self.filenameLineEdit.setMaximum(len(self.numberToFilesMap))
        self.exposureNbChanged()
        # self.onImageChanged()

    def resizeImage(self, img, res_size):
        """
        Resize the image.
        """
        print("Size mismatched, resizing image")
        if img.shape == res_size:
            return img
        h,b = img.shape
        resH, resB = res_size
        dH = resH - h
        dB = resB - b
        extraH = dH//2
        extraB = dB//2
        res_img = np.zeros((res_size))
        res_img[extraH:extraH+h, extraB:extraB+b] = img
        return res_img

    def addIntensity(self, imgs, dir_path, key):
        """
        Add Intensity of one set of files (main function).
        :param imgs, dir_path, key:
        """
        createFolder(fullPath(dir_path, "ai_results"))
        sum_img = 0
        for img in imgs:
            if not isinstance(sum_img, int) and img.shape[0] > sum_img.shape[0]:
                sum_img = self.resizeImage(sum_img, img.shape)
            elif not isinstance(sum_img, int):
                img = self.resizeImage(img, sum_img.shape)
            sum_img += img
        result_file = os.path.join(dir_path, 'ai_results/res_' + str(key) + '.tif')
        fabio.tifimage.tifimage(data=sum_img).write(result_file)
        print('Saved ', result_file)
        print('Resulting image shape ', sum_img.shape)

    def addIntensities(self, numberToFilesMap, dir_path):
        """
        Add Intensities of the different files (main function).
        :param numberToFilesMap, dir_path:
        """
        createFolder(fullPath(dir_path, "ai_results"))
        for key in numberToFilesMap:
            sum_img = 0
            for fname in numberToFilesMap[key]:
                img = fabio.open(fname).data
                img = ifHdfReadConvertless(fname, img)
                if not isinstance(sum_img, int) and img.shape[0] > sum_img.shape[0]:
                    sum_img = self.resizeImage(sum_img, img.shape)
                elif not isinstance(sum_img, int):
                    img = self.resizeImage(img, sum_img.shape)
                sum_img += img
            result_file = os.path.join(dir_path, 'ai_results/res_' + str(key) + '.tif')
            fabio.tifimage.tifimage(data=sum_img).write(result_file)
            print('Saved ', result_file)
            print('Resulting image shape ', sum_img.shape)

    def browseFolder(self):
        """
        Same as browse files but allow the user to select a folder instead of a file.
        """
        self.dir_path = QFileDialog.getExistingDirectory(self, "Select a Folder")
        if self.dir_path != "":
            self.numberToFilesMap = collections.defaultdict(list)
            for fname in os.listdir(self.dir_path):
                f = os.path.join(self.dir_path, fname)
                if os.path.isfile(f):
                    i = -1
                    name = fname.split('.')[0]
                    number = name.split('_')[i]
                    while not number.isnumeric():
                        i -= 1
                        number = name.split('_')[i]
                    # number = int(re.sub(r'[^0-9]', '', fname))
                    self.numberToFilesMap[int(number)].append(f)
                    self.numberToFilesMap[int(number)].sort()
            self.onNewFileSelected()
            # self.onImageChanged()
            """
            self.addIntensities(self.numberToFilesMap, dir_path)
            msg = QMessageBox()
            msg.setInformativeText(
                "Completed Adding intensities, results saved in folder ai_results")
            msg.setStandardButtons(QMessageBox.Ok)
            msg.setWindowTitle("Finished Adding Intensities")
            msg.setStyleSheet("QLabel{min-width: 500px;}")
            msg.exec_()
            """

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new QuadrantFolder object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        self.filenameLineEdit.setValue(self.currentFileNumber)
        self.orig_imgs = []
        for i in range(self.exposureNb.value()):
            self.orig_imgs.append(fabio.open(self.numberToFilesMap[self.currentFileNumber][i]).data)
        # self.orig_image_center = None
        if self.orig_imgs[0].shape == (1043, 981):
            self.img_type = "PILATUS"
        else:
            self.img_type = "NORMAL"
        self.imgDetailOnStatusBar.setText(
            str(self.orig_imgs[0].shape[0]) + 'x' + str(self.orig_imgs[0].shape[1]) + ' : ' + str(self.orig_imgs[0].dtype))
        self.initialWidgets(self.orig_imgs[0])
        self.processImage()

    def initialWidgets(self, img):
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

        if 'float' in str(img.dtype):
            self.spmaxInt.setDecimals(2)
            self.spminInt.setDecimals(2)
        else:
            self.spmaxInt.setDecimals(2)
            self.spminInt.setDecimals(2)

        self.uiUpdating = False

    def updateImageTab(self):
        """
        Display image in image tab, and draw lines
        """
        if not self.updated:
            self.uiUpdating = True

            if self.calibrationChkBx.isChecked():
                extent, center = self.getExtentAndCenter(self.orig_imgs[0])
            else:
                extent, center = [0, 0], (0, 0)

            self.plotImages(self.imageAxes, self.orig_imgs[0], extent)
            self.plotImages(self.imageAxes2, self.orig_imgs[1], extent)

            if self.nbOfExposures >= 3:
                self.plotImages(self.imageAxes3, self.orig_imgs[2], extent)
            if self.nbOfExposures >= 4:
                self.plotImages(self.imageAxes4, self.orig_imgs[3], extent)
            if self.nbOfExposures >= 5:
                self.plotImages(self.imageAxes5, self.orig_imgs[4], extent)
            if self.nbOfExposures >= 6:
                self.plotImages(self.imageAxes6, self.orig_imgs[5], extent)
            if self.nbOfExposures >= 7:
                self.plotImages(self.imageAxes7, self.orig_imgs[6], extent)
            if self.nbOfExposures >= 8:
                self.plotImages(self.imageAxes8, self.orig_imgs[7], extent)

            if self.calSettingsDialog is not None:
                self.calSettingsDialog.centerX.setValue(center[0])
                self.calSettingsDialog.centerY.setValue(center[1])

            self.imageFigure.tight_layout()
            self.imageCanvas.draw()

            self.updated = True
            self.uiUpdating = False

    def plotImages(self, imageAxes, img, extent):
        """
        Displays the image for each exposure
        """
        ax = imageAxes
        ax.cla()

        if self.logScaleIntChkBx.isChecked():
            ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
        else:
            ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()), extent=[0-extent[0], img.shape[1] - extent[0], img.shape[0] - extent[1], 0-extent[1]])
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

    def deleteInfo(self, delList):
        """
        Remove input keys from info dict of current QuadrantFolder object
        :param delList: list of keys
        """
        if self.ableToProcess():
            for inf in delList:
                if inf in self.info.keys():
                    del self.info[inf]

    def rotateImg(self):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal. The angle will be kept in self.info["rotationAngle"]
        Once the rotation angle is calculated, the average fold will be re-calculated, so self.info["avg_fold"] is deleted
        """
        self.statusPrint("Finding Rotation Angle...")
        if 'manual_rotationAngle' in self.info:
            self.info['rotationAngle'] = self.info['manual_rotationAngle']
            del self.info['manual_rotationAngle']
        elif 'rotationAngle' not in self.info.keys():
            print("Rotation Angle is being calculated ... ")
            # Selecting disk (base) image and corresponding center for determining rotation as for larger images (formed from centerize image) rotation angle is wrongly computed
            img = copy.copy(self.orig_imgs[0])
            # _, center = self.getExtentAndCenter(img)
            self.info['rotationAngle'] = getRotationAngle(img, self.orig_image_center, 0)
        print("Done. Rotation Angle is " + str(self.info['rotationAngle']) +" degree")
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
        rotImg, newCenter, self.rotMat = rotateImage(img, center, self.info["rotationAngle"], self.img_type, -1)

        # Cropping off the surrounding part since we had already expanded the image to maximum possible extent in centerize image
        bnew, lnew = rotImg.shape
        db, dl = (bnew - b)//2, (lnew-l)//2
        final_rotImg = rotImg[db:bnew-db, dl:lnew-dl]
        self.info["center"] = (newCenter[0]-dl, newCenter[1]-db)
        self.dl, self.db = dl, db # storing the cropped off section to recalculate coordinates when manual center is given

        return final_rotImg

    def processImage(self):
        """
        Process Image by getting all flags and call process() of QuadrantFolder object
        Then, write data and update UI
        """
        if self.ableToProcess():
            if self.calibrationChkBx.isChecked():
                _, self.orig_image_center = self.getExtentAndCenter(self.orig_imgs[0])
                for i in range(self.exposureNb.value()):
                    self.rotateImg()
                    self.centerizeImage(self.orig_imgs[i], i)
                    self.orig_imgs[i] = self.getRotatedImage(i)
            self.refreshImageTab()

    def removeWidget(self, win):
        """
        Remove a widget from the current window.
        :param win: the widget to remove
        """
        if win in self.widgetList:
            idx = self.widgetList.index(win)
            del self.widgetList[idx]

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        self.statusReport.setText(text)
        QApplication.processEvents()
