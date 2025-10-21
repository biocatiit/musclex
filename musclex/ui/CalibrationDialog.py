
import os
import sys
import fabio
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import cv2
import json

from os.path import join
from ..utils.file_manager import createFolder
from matplotlib.colors import LogNorm, Normalize
from .pyqt_utils import *
from ..CalibrationSettings import CalibrationSettings
from ..utils.image_processor import *

class CalibrationDialog(QMainWindow):
    def __init__(self, dir_path, imagePath, mode):
        super().__init__()
        self.dir_path = dir_path
        self.orig_image = None
        self.image = None
        self.img_zoom = None
        self.default_img_zoom = None
        self.imagePath = imagePath
        self.imageAxes = None
        self.function = None
        self.activeFunction = False
        self.image_center = None
        self.newImgDimension = None
        self.center_before_rotation = None
        self.centImgTransMat = None
        self.expandImg = 1
        self.mode = mode
        
        self.info = {
            'center' : None,
            'rotationAngle' : 0,
            'calib_center' : None,
            'manual_center': None,
            'detector' : None,
            'applyAll': False,
        }
        self.calSettingsDialog = None
        
        
        self.doubleZoomMode = False
        self.dontShowAgainDoubleZoomMessageResult = False
        self.doubleZoomPt = (0, 0)
        self.doubleZoomAxes = None
        
        self.initUI()
        self.setConnections()
        self.loadImage()

    def initUI(self):
        self.centralWidget = QWidget(self)
        self.setCentralWidget(self.centralWidget)
        self.mainLayout = QHBoxLayout(self.centralWidget)

        # Image Display
        self.imageFigure = plt.figure()
        self.imageAxes = self.imageFigure.add_subplot(111)
        self.imageCanvas = FigureCanvas(self.imageFigure)
        # self.imageCanvas.setHidden(True)
        
        # Options Layout
        self.optionsWidget = QWidget()
        self.optionsLayout = QVBoxLayout()
        self.optionsLayout.setAlignment(Qt.AlignCenter)
        self.optionsWidget.setLayout(self.optionsLayout)
        
        # Mode Group
        self.modeGroup = QGroupBox("Mode")
        self.modeLayout = QHBoxLayout()
        self.applyCurrentImage = QCheckBox("Apply to Current Image")
        self.applyAllImage = QCheckBox("Apply to All Images")
        self.modeButtonGroup = QButtonGroup()
        self.modeButtonGroup.setExclusive(True)
        self.modeButtonGroup.addButton(self.applyCurrentImage)
        self.modeButtonGroup.addButton(self.applyAllImage)
        self.applyCurrentImage.setChecked(True)
        self.modeLayout.addWidget(self.applyCurrentImage)
        self.modeLayout.addWidget(self.applyAllImage)
        if self.mode == 'aime':
            self.applyToSequence = QCheckBox("Apply to Sequence")
            self.modeButtonGroup.addButton(self.applyToSequence)
            self.modeLayout.addWidget(self.applyToSequence)
        self.modeGroup.setLayout(self.modeLayout)
        
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

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')
        self.dispOptLayout.addWidget(self.minIntLabel, 1, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spminInt, 1, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntLabel, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spmaxInt, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomInB, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 3, 1, 1, 1)
        self.dispOptLayout.addWidget(self.logScaleIntChkBx, 4, 0, 1, 2)
        self.dispOptLayout.addWidget(self.persistIntensity, 5, 0, 1, 2)
        self.dispOptLayout.addWidget(self.doubleZoom, 6, 0, 1, 2)
        self.displayOptGrpBx.setLayout(self.dispOptLayout)
        
        # Display Group Checkboxes
        self.optionsDisplay = QGroupBox()
        self.optionsDisplayLayout = QHBoxLayout()
        self.optionsDisplay.setLayout(self.optionsDisplayLayout)
        
        self.displayCompute = QCheckBox("Compute Center and Orientation")
        self.displayCenter = QCheckBox("Set Center and Orientation")
        
        self.optionsDisplayLayout.addWidget(self.displayCompute)
        self.optionsDisplayLayout.addWidget(self.displayCenter)
        
        self.optionsDisplayButtonGroup = QButtonGroup()
        self.optionsDisplayButtonGroup.setExclusive(True)
        self.optionsDisplayButtonGroup.addButton(self.displayCompute)
        self.optionsDisplayButtonGroup.addButton(self.displayCenter)
        
        # New Layout
        self.testLayout = QGridLayout()
        self.testLayoutGrp = QGroupBox()
        self.testLayoutGrp.setLayout(self.testLayout)
        self.selectCalibrationImageButton = QPushButton("Use Calibration Image Center")
        self.setCalibrationCenter = QCheckBox("Set Calibration Center")
        self.calibrationCenterDropdown = QComboBox()
        self.calibrationCenterDropdown.addItems(["(Select function)", "Set Center by Chord", "Set Center by Perpendicular"])
        self.calibrationCenterDropdown.setEnabled(False)
        self.computeCenter = QCheckBox("Compute Center")
        self.setOrientationChkBx = QCheckBox("Set Orientation")
        self.orientationDrpDown = QComboBox()
        self.orientationDrpDown.addItems(["(Select function)", "Set Rotation Angle", "Set Rotation Angle and Center"])
        self.orientationDrpDown.setEnabled(False)
        self.computeOrientation = QCheckBox("Compute Orientation")
        self.computeOrientationDrpDown = QComboBox()
        self.computeOrientationDrpDown.addItems(["Max Intensity", "GMM", "Herman Factor (Half Pi)", "Herman Factor (Pi)"])
        self.computeOrientationDrpDown.setEnabled(False)
        
        self.calibrationButtonGroup = QButtonGroup()
        self.calibrationButtonGroup.setExclusive(True)
        self.calibrationButtonGroup.addButton(self.setCalibrationCenter)
        self.calibrationButtonGroup.addButton(self.computeCenter)
        
        self.orientationButtonGroup = QButtonGroup()
        self.orientationButtonGroup.setExclusive(True)
        self.orientationButtonGroup.addButton(self.setOrientationChkBx)
        self.orientationButtonGroup.addButton(self.computeOrientation)
        
        self.testLayout.addWidget(self.selectCalibrationImageButton, 0, 0, 1, 5)
        self.testLayout.addWidget(self.setCalibrationCenter, 1, 0, 1, 2)
        self.testLayout.addWidget(self.calibrationCenterDropdown, 1, 3, 1, 2)
        self.testLayout.addWidget(self.computeCenter, 2, 0, 1, 5)
        self.testLayout.addWidget(self.setOrientationChkBx, 3, 0, 1, 2)
        self.testLayout.addWidget(self.orientationDrpDown, 3, 3, 1, 2)
        self.testLayout.addWidget(self.computeOrientation, 4, 0, 1, 2)
        self.testLayout.addWidget(self.computeOrientationDrpDown, 4, 3, 1, 2)
        
        # Compute Group
        # self.computeGroup = QGroupBox("Compute Center and Orientation")
        # self.computeGrpLayout = QGridLayout()
        # self.selectCalibrationImageButton = QPushButton("Select Calibration Image")
        # self.useComputedCenter = QCheckBox("Use Computed Center")
        # self.useComputedCenter.setChecked(True)
        # self.useCalibrationCenter = QCheckBox("Use Calibration Center")
        
        
        # self.centerCheckButtonGroup = QButtonGroup()
        # self.centerCheckButtonGroup.setExclusive(True)
        # self.centerCheckButtonGroup.addButton(self.useComputedCenter)
        # self.centerCheckButtonGroup.addButton(self.useCalibrationCenter)
        
        # self.useCalibrationCenter.setEnabled(False)
        # self.useComputedOrientation = QCheckBox("Use Computed Orientation")
        
        # self.computeGrpLayout.addWidget(self.selectCalibrationImageButton, 0, 0, 1, 5)
        # self.computeGrpLayout.addWidget(self.useComputedCenter, 1, 0, 1, 2)
        # self.computeGrpLayout.addWidget(self.useCalibrationCenter, 1, 3, 1, 2)
        # self.computeGrpLayout.addWidget(self.useComputedOrientation, 2, 0, 1, 2)
        # self.computeGroup.setLayout(self.computeGrpLayout)
        # self.computeGroup.setVisible(False)
        
        # # Set Center Group
        # self.setCenterGroup = QGroupBox("Set Center and Orientation")
        # self.setCenterLayout = QGridLayout()
        # self.setCenterGroup.setLayout(self.setCenterLayout)
        
        # self.setCenterByChord = QCheckBox("Set Center by Chord")
        # self.setCenterByPerp = QCheckBox("Set Center by Perpendicular")
        # self.setRotationCenter = QCheckBox("Set Rotation Angle and Center")
        # self.setRotationOnly = QCheckBox("Set Rotation Angle")
        
        # self.centerButtonGroup = QButtonGroup()
        # self.centerButtonGroup.setExclusive(True)
        # self.centerButtonGroup.addButton(self.setCenterByChord)
        # self.centerButtonGroup.addButton(self.setCenterByPerp)
        # self.centerButtonGroup.addButton(self.setRotationCenter)
        # self.centerButtonGroup.addButton(self.setRotationOnly)
        
        # self.setButton = QPushButton("Set Center/Orientation")
        
        # self.setCenterLayout.addWidget(self.setCenterByChord, 0, 0, 1, 2)
        # self.setCenterLayout.addWidget(self.setCenterByPerp, 0, 3, 1, 2)
        # self.setCenterLayout.addWidget(self.setRotationCenter, 1, 0, 1, 2)
        # self.setCenterLayout.addWidget(self.setRotationOnly, 1, 3, 1, 2)
        # self.setCenterLayout.addWidget(self.setButton, 2, 0, 1, 5)
        # self.setCenterGroup.setVisible(False)
        
        self.saveButton = QPushButton("Correct Center and Orientation")
        # self.saveButton.setStyleSheet("background-color: green; color: white;")
        self.saveButton.setIcon(self.style().standardIcon(QStyle.SP_DialogOkButton))
        
        self.cancelButton = QPushButton("Cancel")
        self.cancelButton.setStyleSheet("background-color: red; color: white;")
        self.cancelButton.setIcon(self.style().standardIcon(QStyle.SP_DialogCancelButton))
        
        # Add Widgets to Options Layout
        
        self.optionsLayout.addWidget(self.modeGroup)
        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addWidget(self.testLayoutGrp)
        # self.optionsLayout.addWidget(self.optionsDisplay)
        # self.optionsLayout.addWidget(self.computeGroup)
        # self.optionsLayout.addWidget(self.setCenterGroup)
        self.optionsLayout.addWidget(self.saveButton)
        self.optionsLayout.addWidget(self.cancelButton)
        
        # Add Widgets to Main Layout
        self.mainLayout.addWidget(self.imageCanvas)
        self.mainLayout.addWidget(self.optionsWidget)
        self.mainLayout.setStretchFactor(self.imageCanvas, 2)
        self.mainLayout.setStretchFactor(self.optionsWidget, 1)
        
        # Status Bar
        self.statusBar = QStatusBar()
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusReport = QLabel()
        self.imgDetailOnStatusBar = QLabel()
        self.imgCoordOnStatusBar = QLabel()
        self.statusBar.addPermanentWidget(self.statusReport)
        self.statusBar.addPermanentWidget(self.imgCoordOnStatusBar)
        self.statusBar.addPermanentWidget(self.imgDetailOnStatusBar)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.statusBar.addWidget(QLabel("    "))
        self.setStatusBar(self.statusBar)
        
        self.show()
        self.setMinimumHeight(900)
        self.setMinimumWidth(1500)
        
        
    def setConnections(self):
        self.spminInt.valueChanged.connect(self.refreshImage)
        self.spmaxInt.valueChanged.connect(self.refreshImage)
        self.doubleZoom.stateChanged.connect(self.doubleZoomChecked)
        self.logScaleIntChkBx.stateChanged.connect(self.refreshImage)
        
        self.selectCalibrationImageButton.clicked.connect(self.selectCalibrationImage)
        self.setCalibrationCenter.stateChanged.connect(self.setCenterChecked)
        self.setOrientationChkBx.stateChanged.connect(self.setOrientationChecked)
        self.orientationDrpDown.currentIndexChanged.connect(self.orientationComboChanged)
        self.computeOrientation.stateChanged.connect(self.computeOrientationChecked)
        self.computeOrientationDrpDown.currentIndexChanged.connect(self.computeOrientationChanged)
        self.calibrationCenterDropdown.currentIndexChanged.connect(self.setCenterDropDownChanged)
        self.computeCenter.stateChanged.connect(self.useComputedCenterClicked)
        # self.useComputedCenter.clicked.connect(self.useComputedCenterClicked)
        
        self.imageFigure.canvas.mpl_connect('button_press_event', self.imageClicked)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)
        self.imageFigure.canvas.mpl_connect('button_release_event', self.imageReleased)
        self.imageFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)
        
        # self.displayCompute.stateChanged.connect(self.displayGroupChecked)
        # self.displayCenter.stateChanged.connect(self.displayGroupChecked)
        
        # self.setButton.clicked.connect(self.setButtonClicked)
        
        self.saveButton.clicked.connect(self.saveClicked)
        self.cancelButton.clicked.connect(self.cancelClicked)
        
    # def displayGroupChecked(self):
    #     self.computeGroup.setVisible(self.displayCompute.isChecked())
    #     self.setCenterGroup.setVisible(self.displayCenter.isChecked())
    
    def setCenterChecked(self):
        if self.setCalibrationCenter.isChecked():
            self.calibrationCenterDropdown.setEnabled(True)
        else:
            self.calibrationCenterDropdown.setEnabled(False)
            
    def setOrientationChecked(self):
        if self.setOrientationChkBx.isChecked():
            self.orientationDrpDown.setEnabled(True)
        else:
            self.orientationDrpDown.setEnabled(False)
    
    def computeOrientationChecked(self):
        if self.computeOrientation.isChecked():
            self.computeOrientationDrpDown.setEnabled(True)
        else:
            self.computeOrientationDrpDown.setEnabled(False)
            
    def setCenterDropDownChanged(self):
        self.activeFunction = False
        self.function = None
        if self.calibrationCenterDropdown.currentText() == "Set Center by Chord":
            print("Set Center by Chords Mode")
            self.setCenterByChordsActive()
        elif self.calibrationCenterDropdown.currentText() == "Set Center by Perpendicular":
            print("Set Center by Perpendicular Mode")
            self.setCenterByPerpActive()
        
    def orientationComboChanged(self):
        if self.orientationDrpDown.currentText() == "Set Rotation Angle":
            print("Set Rotation Angle Mode")
            self.setRotation()
        elif self.orientationDrpDown.currentText() == "Set Rotation Angle and Center":
            print("Set Rotation Angle and Center Mode")
            self.setCenterRotation()
            
    def computeOrientationChanged(self):
        method = self.computeOrientationDrpDown.currentIndex()
        self.info['rotationAngle'] = None
        if self.info['center'] is not None:
            center = self.info['center']
        else:
            center = self.findCenter()
        self.info['rotationAngle'] = getRotationAngle(self.orig_image, center, method)
        self.centerizeImage()
        self.image, _, _ = rotateImage(self.orig_image, center, self.info['rotationAngle'])
        self.newImgDimension = None
        self.refreshImage()
        
    def saveClicked(self):
        path = join(self.dir_path, 'settings')
        createFolder(path)
        path = join(path, 'calibrationDialog.json')
        if self.mode == 'aime':
            if self.applyToSequence.isChecked():
                self.info['applyToSequence'] = True
            elif self.applyAllImage.isChecked():
                self.info['applyAll'] = True
            elif self.applyCurrentImage.isChecked():
                self.info['applyAll'] = False
        elif self.applyAllImage.isChecked():
            self.info['applyAll'] = True
        elif self.applyCurrentImage.isChecked():
            self.info['applyAll'] = False
        
        with open(path, 'w') as f:
            json.dump(self.info, f)
        self.close()
    
    def useComputedCenterClicked(self):
        if self.computeCenter.isChecked():
            self.info['center'] = None
            self.findCenter()
            self.plotCenter()
            self.refreshImage()
            
    
    def cancelClicked(self):
        self.close()
        
    def selectCalibrationImage(self):
        if self.calSettingsDialog is None:
            self.calSettingsDialog = CalibrationSettings(self.dir_path)
        self.calSettings = None
        result = self.calSettingsDialog.exec_()
        if result == 1:
            self.calSettings = self.calSettingsDialog.getValues()
            # self.useCalibrationCenter.setEnabled(True)

            if self.calSettings is not None and self.calSettings:
                print("not empty")
                # Always save calibrated center if available
                # Whether to USE it is controlled by Persistent Center in main GUI
                if 'center' in self.calSettings:
                    self.info['calib_center'] = self.calSettings['center']
                if "detector" in self.calSettings:
                    self.info["detector"] = self.calSettings["detector"]
                return True
            else:
                print("empty")
                return False
            
    def plotCenter(self):
        if self.image_center is not None:
            self.imageAxes.plot(self.image_center[0], self.image_center[1], 'bo')
        
    # Redraw the Image
    def refreshImage(self):
        self.plotImages(self.imageAxes, self.image)
        if self.computeCenter.isChecked() or self.setCalibrationCenter.isChecked():
            self.plotCenter()
        self.imageFigure.tight_layout()
        self.imageCanvas.draw()
        
    # Load the image provided and set default values then call refreshImage to redraw
        
    def loadImage(self):
        raw_filepath = r"{}".format(self.imagePath)
        self.image = fabio.open(raw_filepath).data
        self.orig_image = self.image
        
        min_val = self.image.min()
        max_val = self.image.max()
        self.spmaxInt.setRange(min_val, max_val)
        self.spminInt.setRange(min_val, max_val)
        if not self.persistIntensity.isChecked():
            self.spmaxInt.setValue(max_val * .5)
            self.spminInt.setValue(min_val)
        self.spmaxInt.setSingleStep(max_val * .05)
        self.spminInt.setSingleStep(max_val * .05)
        
        self.spmaxInt.setDecimals(2)
        self.spminInt.setDecimals(2)

        self.minIntLabel.setText("Min Intensity ("+str(min_val)+")")
        self.maxIntLabel.setText("Max Intensity (" + str(max_val) + ")")
        
        self.findCenter()
        
        self.imgDetailOnStatusBar.setText(str(self.image.shape[0]) + 'x' + str(self.image.shape[1]) + ' : ' + str(self.image.dtype))
        
        self.refreshImage()
    
    def plotImages(self, imageAxes, img):
        """
        Displays the image
        """
        ax = imageAxes
        ax.cla()

        extent, center = [0, 0], (0, 0)

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

            img = self.image
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
        
    def imageOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """

        x = event.xdata
        y = event.ydata

        img = self.image

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < img.shape[1] and y < img.shape[0]:
                self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]))
                if self.doubleZoom.isChecked() and self.doubleZoomMode and x > 10 and x < img.shape[1]-10 and y > 10 and y < img.shape[0]-10:
                    ax1 = self.doubleZoomAxes
                    imgCropped = img[y - 10:y + 10, x - 10:x + 10]
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

        func = self.function

        if func is None:
            return
        elif func[0] == "im_zoomin":
            # draw rectangle
            if len(func) < 2:
                return
            ax = self.imageAxes
            ax.patches[-1].remove()
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.imageCanvas.draw_idle()

        elif func[0] == "im_rotate":
            # draw line as angle
            center = self.info["center"]
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            ax = self.imageAxes
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
        elif func[0] == "im_center_rotate":
            # draw X on points and a line between points
            axis_size = 5
            ax = self.imageAxes
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
            # self.imageCanvas.flush_events()
        elif func[0] == "perp_center":
            # draw X on points and a line between points
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
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

            elif len(func) % 2 != 0:
                if len(ax.lines) > 0:
                    n = (len(func)-1)*5//2 + 2
                    # first_cross = ax.lines[:n]
                    for i in range(len(ax.lines)-1,n-1,-1):
                        ax.lines[i].remove()
                    # ax.lines = first_cross
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
                    # first_cross = ax.lines[:n]
                    for i in range(len(ax.lines)-1,n-1,-1):
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

        elif func[0] == "im_move":
            # change zoom-in location (x,y ranges) to move around image
            if self.img_zoom is not None:
                ax = self.imageAxes
                move = (func[1][0] - x, func[1][1] - y)
                self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[1], img.shape[0])
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
                ax.invert_yaxis()
                self.imageCanvas.draw_idle()

        # self.doubleZoomCanvas.draw_idle()
                        
                        
    def imageZoomOut(self):
        """
        Trigger when set zoom out button is pressed (image tab)
        """
        self.imgZoomInB.setChecked(False)
        self.default_img_zoom = None
        self.img_zoom = None
        self.refreshImage()
    
    def imageZoomIn(self):
        """
        Trigger when set zoom in button is pressed (image tab)
        """
        if self.imgZoomInB.isChecked():
            msg = QMessageBox()
            msg.setText("Draw a rectangle on the image to zoom in (ESC to cancel)")
            msg.setStandardButtons(QMessageBox.Ok)
            msg.setIcon(QMessageBox.Warning)
            msg.setFixedWidth(300)
            msg.exec_()
            self.imageCanvas.draw_idle()
            self.function = ["im_zoomin"]
        else:
            self.function = None
            
    def imageClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
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
                    self.refreshImage()
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
                    self.info['manual_center'] = (cx, cy)
                    if 'center' in self.info:
                        self.info['center'] = None
                    self.info['manual_rotationAngle'] = self.info['rotationAngle'] + new_angle
                    self.setCenterRotation()
                    self.centerizeImage()
                    self.image, _, _ = rotateImage(self.image, new_center, self.info['manual_rotationAngle'])
                    self.newImgDimension = None
                    # self.setRotationCenter.setChecked(False)
                    self.refreshImage()
            elif func[0] == "im_rotate":
                # draw line as angle
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

                self.info['manual_rotationAngle'] = self.info['rotationAngle'] + new_angle
                print(self.info['manual_rotationAngle'])
                self.image = rotateImageAboutPoint(self.image, center, self.info['manual_rotationAngle'])
                self.setRotation()
                # self.setRotationOnly.setChecked(False)
                self.refreshImage()
            
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
        img_size = self.image.shape

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
        
    def setButtonClicked(self):
        if self.setCenterByChord.isChecked():
            print("Set Center by Chords Mode")
            self.setCenterByChordsActive()
        elif self.setCenterByPerp.isChecked():
            print("Set Center by Perpendicular Mode")
            self.setCenterByPerpActive()
        elif self.setRotationCenter.isChecked():
            print("Set Center and Rotation Angle Mode")
            self.setCenterRotation()
        elif self.setRotationOnly.isChecked():
            print("Set Rotation Angle Mode")
            self.setRotation()
            
    def setRotation(self):
        """
        Trigger when set rotation angle button is pressed
        """
        if self.activeFunction is False:
            # clear plot
            QMessageBox.information(self, "Set Rotation Angle", "Rotate the line to the pattern equator (ESC to cancel)")
            _, center = self.getExtentAndCenter()
            self.info['center'] = center
            self.function = ["im_rotate"]
            self.activeFunction = True
            self.imageCanvas.draw_idle()
        else:
            self.function = None
            self.activeFunction = False
            
    def setCenterRotation(self):
        """
        Trigger when set center and rotation angle button is pressed
        """
        if self.activeFunction is False:
            QMessageBox.information(self, "Set Center and Rotation Angle", "Click on 2 corresponding reflection peaks along the equator (ESC to cancel)")
            self.imageCanvas.draw_idle()
            self.function = ["im_center_rotate"]
            self.activeFunction = True
        else:
            self.function = None
            self.activeFunction = False
            
    def setCenterByPerpActive(self):
        """
        Prepare for manual center selection using perpendicular peaks
        :return:
        """
        if self.activeFunction is False:
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            QMessageBox.information(self, "Set Center by Perpendicular", "Click on 2 perpendicular peaks of the image, then press Enter to compute. Esc to cancel")
            self.imageCanvas.draw_idle()
            self.function = ["perp_center"]  # set current active function
            self.activeFunction = True
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
            self.info['manual_center'] = (
            int(round(new_center[0])) + extent[0], int(round(new_center[1])) + extent[1])
            if 'center' in self.info:
                self.info['center'] = None
            print("New center after extent ", self.info['manual_center'])
            self.newImgDimension = None
            self.activeFunction = False
            self.function = None
            self.centerizeImage()
            self.refreshImage()
        
    def setCenterByChordsActive(self):
        """
        Prepare for manual rotation center setting by selecting chords
        """

        if self.activeFunction is False:
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4, 4, 13)
            # ax2.imshow(getBGR(get8bitImage(self.bioImg.getRotatedImage(), self.minIntSpnBx.value(), self.maxIntSpnBx.value())))
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            
            QMessageBox.information(self, "Set Center by Chords", "Click on 3 chords of the image, then press Enter to compute. Esc to cancel")
            self.chordpoints = []
            self.chordLines = []
            self.imageCanvas.draw_idle()
            self.function = ["chords_center"]  # set current active function
            self.activeFunction = True
        else:
            QApplication.restoreOverrideCursor()
            print("Finding Chords center ...")
            centers = []
            for i, line1 in enumerate(self.chordLines):
                for line2 in self.chordLines[i+1:]:
                    if line1[0] == line2[0]:
                        continue #parallel lines
                    if line1[0] == float('inf'):
                        xcent = line1[1]
                        ycent = line2[0] * xcent + line2[1]
                    elif line2[0] == float('inf'):
                        xcent = line2[1]
                        ycent = line1[0] * xcent + line1[1]
                    else:
                        xcent = (line2[1] - line1[1]) / (line1[0] - line2[0])
                        ycent = line1[0]*xcent + line1[1]
                    center = [xcent, ycent]
                    print("CenterCalc ", center)

                    centers.append(center)

            cx = int(sum([centers[i][0] for i in range(0, len(centers))]) / len(centers))
            cy = int(sum([centers[i][1] for i in range(0, len(centers))]) / len(centers))
            M = cv2.getRotationMatrix2D(tuple(self.info['center']), self.info['rotationAngle'], 1)
            invM = cv2.invertAffineTransform(M)
            homo_coords = [cx, cy, 1.]
            new_center = np.dot(invM, homo_coords)
            print("New center ", new_center)
            # Set new center and rotaion angle , re-calculate R-min
            self.info['center'] = (int(round(new_center[0])), int(round(new_center[1])))
            # self.log_changes('center', varName='center', newValue=self.info['center'])
            self.newImgDimension = None
            self.activeFunction = False
            self.function = None
            self.centerizeImage()
            self.refreshImage()
            
    def drawPerpendiculars(self):
        """
        Draw perpendiculars on the image
        """
        ax = self.imageAxes
        points = self.chordpoints
        self.chordLines = []
        for i,p1 in enumerate(points):
            for p2 in points[i+1:]:
                slope, cent = getPerpendicularLineHomogenous(p1, p2)
                if slope == float('inf'):
                    y_vals = np.array(ax.get_ylim())
                    x_vals = cent[0] + np.zeros(y_vals.shape)
                    self.chordLines.append([slope, cent[0]])
                else:
                    x_vals = np.array(ax.get_xlim())
                    y_vals = (x_vals - cent[0])*slope + cent[1]
                    self.chordLines.append([slope, cent[1] - slope*cent[0]])
                ax.plot(x_vals, y_vals, linestyle='dashed', color='b')
                
    def getExtentAndCenter(self):
        """
        Give the extent and the center of the image
        """
        if self.image_center is None:
            self.findCenter()
        if 'calib_center' in self.info and self.info['calib_center'] is not None:
            center = self.info['calib_center']
        elif self.info['manual_center'] is not None:
            center = self.info['manual_center']
        else:
            center = self.image_center

        for d, c in enumerate(self.info['center']):
            if c is None:
                self.info['center'][d] = center
        extent = [self.info['center'][0] - center[0], self.info['center'][1] - center[1]]
        return extent, center

    def findCenter(self):
        """
        Find center of the diffraction. The center will be kept in self.info['center'].
        Once the center is calculated, the rotation angle will be re-calculated, so self.info["rotationAngle"] is deleted
        """
        print("Finding Center...")
        if self.info['center'] is not None:
            return
        if 'calib_center' in self.info and self.info['calib_center'] is not None:
            self.info['center'] = self.info['calib_center']
            return
        if self.info['manual_center'] is not None:
            self.info['center'] = self.info['manual_center']
            return
        print("Center is being calculated ... ")
        self.image, self.info['center']= processImageForIntCenter(self.orig_image, getCenter(self.orig_image))
        self.image_center = self.info['center']
        print("Done. Center = "+str(self.info['center']))
        
        
    def centerizeImage(self):
        
        """
        Create an enlarged image such that image center is at the center of new image
        """
        print("Centererizing image...")
        if self.info['center'] is not None:
            center = self.info['center']
        elif self.info['manual_center'] is not None:
            center = self.info['manual_center']
            
        if self.centImgTransMat is not None and 'calib_center' not in self.info:
            # convert center in initial img coordinate system
            print("here!!!!!!!!!!!!!!!!!???")
            M = self.centImgTransMat
            M[0,2] = -1*M[0,2]
            M[1,2] = -1*M[1,2]
            center = [center[0], center[1], 1]
            center = np.dot(M, center)
            if 'manual_center' in self.info and self.info['manual_center'] is not None:
                self.info['manual_center'] = (int(center[0]), int(center[1]))
            if 'calib_center' in self.info and self.info['calib_center'] is not None:
                self.info['calib_center'] = (int(center[0]), int(center[1]))

        center = (int(center[0]), int(center[1]))
        print("Dimension of initial image before centerize ", self.image.shape)
        img = self.orig_image
        print("Dimension of image before centerize ", img.shape)

        b, l = img.shape
        if self.newImgDimension is None:
            # This is the max dimension in the case beamline is in a corner and image rotated to 45 degrees
            qf_w, qf_h = 2.8*(l-center[0]), 2.8*(b-center[1])
            max_side = max(max(l,b), max(qf_w, qf_h))
            dim = int(self.expandImg*max_side)
            self.newImgDimension = dim
        else:
            dim = self.newImgDimension
        new_img = np.zeros((dim,dim)).astype("float32")
        try:
            new_img[0:b,0:l] = img
        except:
            print("Centerize Image : Dimension mismatched. Please report error and the steps leading up to it.")
        

        #Translate image to appropriate position
        transx = int(((dim/2) - center[0]))
        transy = int(((dim/2) - center[1]))
        M = np.float32([[1,0,transx],[0,1,transy]])
        self.centImgTransMat = M
        rows, cols = new_img.shape
        # mask_thres = self.info["mask_thres"]

        # if self.img_type == "PILATUS":
        #     if mask_thres == -999:
        #         mask_thres = getMaskThreshold(img, self.img_type)
        #     mask = np.zeros((new_img.shape[0], new_img.shape[1]), dtype=np.uint8)
        #     mask[new_img <= mask_thres] = 255
        #     cv2.setNumThreads(1) # Added to prevent segmentation fault due to cv2.warpAffine
        #     translated_Img = cv2.warpAffine(new_img, M, (cols, rows))
        #     translated_mask = cv2.warpAffine(mask, M, (cols, rows))
        #     translated_mask[translated_mask > 0.] = 255
        #     translated_Img[translated_mask > 0] = mask_thres
        # else:
        cv2.setNumThreads(1) # Added to prevent segmentation fault due to cv2.warpAffine
        translated_Img = cv2.warpAffine(new_img,M,(cols,rows))

        self.image = translated_Img
        self.info['center'] = (int(dim / 2), int(dim / 2))
        self.image_center = self.info['center']
        self.center_before_rotation = (int(dim / 2), int(dim / 2))
        print("Dimension of image after centerize ", self.image.shape)
        
            
    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard
        """
        key = event.key()

        if key == Qt.Key_Escape:
            self.function = None
            self.activeFunction = False
            self.refreshImage()
        elif key == Qt.Key_Return:
            if self.function[0] == 'chords_center':
                self.setCenterByChordsActive()
            elif self.function[0] == 'perp_center':
                self.setCenterByPerpActive()