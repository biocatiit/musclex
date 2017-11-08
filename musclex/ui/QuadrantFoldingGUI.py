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
__author__ = 'Jiranun.J'

import sys
from tifffile import imsave
import matplotlib.patches as patches
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.QuadrantFolder import QuadrantFolder
from pyqt_utils import *
import matplotlib.pyplot as plt
import musclex
import traceback

class QuadrantFoldingGUI(QMainWindow):
    """
    A class forwindow displaying all information of a selected image.
    This window contains 2 tabs : image, and result
    """

    def __init__(self):
        """
        Initial window
        """
        QWidget.__init__(self)
        self.imgList = [] # all images name in current directory
        self.filePath = "" # current directory
        self.quadFold = None # QuadrantFolder object
        self.img_zoom = None # zoom location of original image (x,y range)
        self.result_zoom = None # zoom location of result image (x,y range)
        self.function = None # current active function
        self.uiUpdating = False # update ui status flag (prevent recursive)
        self.checkableButtons = [] # list of checkable buttons
        self.updated = {'img': False, 'result': False} # update state of 2 tabs
        self.ignoreFolds = set()
        self.initUI() # initial all GUI
        self.setConnections() # set triggered function for widgets
        self.setMinimumHeight(700)

    def initUI(self):
        self.setStyleSheet(getStyleSheet())
        self.setWindowTitle("Quadrant Folding v." + musclex.__version__)

        self.centralWidget = QWidget(self)
        self.setCentralWidget(self.centralWidget)
        self.mainHLayout = QHBoxLayout(self.centralWidget)

        self.tabWidget = QTabWidget()
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
        self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)
        self.selectImageButton = QPushButton('Click Here to Select an Image...')
        self.selectImageButton.setFixedHeight(100)
        self.selectImageButton.setFixedWidth(300)

        self.selectFolder = QPushButton('Click Here to Select a Folder...')
        self.selectFolder.setFixedHeight(100)
        self.selectFolder.setFixedWidth(300)

        self.bgWd = QWidget()
        self.verImgLayout.addWidget(self.selectImageButton)
        self.verImgLayout.addWidget(self.selectFolder)
        self.imageFigure = plt.figure(facecolor='#606060')
        self.imageAxes = self.imageFigure.add_subplot(111)
        self.imageCanvas = FigureCanvas(self.imageFigure)

        self.imageCanvas.setHidden(True)
        self.imageTabLayout.addLayout(self.verImgLayout)
        self.imageTabLayout.addWidget(self.imageCanvas)

        self.displayOptGrpBx = QGroupBox()
        self.displayOptGrpBx.setTitle("Display Options")
        self.displayOptGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        # self.displayOptGrpBx.setFixedHeight(125)
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

        self.showSeparator = QCheckBox()
        self.showSeparator.setText("Show Quadrant Separator")
        self.showSeparator.setChecked(True)

        self.imgZoomInB = QPushButton("Zoom in")
        self.imgZoomInB.setCheckable(True)
        self.imgZoomOutB = QPushButton("Full")
        self.checkableButtons.append(self.imgZoomInB)

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')
        self.dispOptLayout.addWidget(self.showSeparator, 0, 0, 1, 2)
        self.dispOptLayout.addWidget(self.minIntLabel, 1, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spminInt, 1, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntLabel, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spmaxInt, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomInB, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 3, 1, 1, 1)

        self.displayOptGrpBx.setLayout(self.dispOptLayout)

        self.optionsLayout = QVBoxLayout()
        self.optionsLayout.setAlignment(Qt.AlignCenter)
        self.settingsGroup = QGroupBox("Image Processing")
        self.settingsLayout = QGridLayout()
        self.settingsGroup.setLayout(self.settingsLayout)

        self.setCenterRotationButton = QPushButton("Set Manual Center and Rotation")
        self.setCenterRotationButton.setCheckable(True)
        self.checkableButtons.append(self.setCenterRotationButton)
        self.setRotationButton = QPushButton("Set Manual Rotation")
        self.setRotationButton.setCheckable(True)
        self.checkableButtons.append(self.setRotationButton)

        self.maskThresSpnBx = QDoubleSpinBox()
        self.maskThresSpnBx.setValue(-999)
        self.maskThresSpnBx.setMinimum(-999)
        self.maskThresSpnBx.setMaximum(999)
        self.maskThresSpnBx.setKeyboardTracking(False)

        self.settingsLayout.addWidget(self.setCenterRotationButton, 0, 0, 1, 2)
        self.settingsLayout.addWidget(self.setRotationButton, 1, 0, 1, 2)
        self.settingsLayout.addWidget(QLabel("Mask Threshold : "), 2, 0, 1, 1)
        self.settingsLayout.addWidget(self.maskThresSpnBx, 2, 1, 1, 1)

        self.fixCenterGrpBx = QGroupBox()
        self.fixCenterGrpBx.setCheckable(True)
        self.fixCenterGrpBx.setTitle("Fix Center")
        self.fixCenterGrpBx.setChecked(False)
        self.fixCX = QLineEdit()
        self.fixCX.setValidator(QIntValidator())
        self.fixCY = QLineEdit()
        self.fixCY.setValidator(QIntValidator())

        self.fixCenterLayout = QGridLayout()
        self.fixCenterLayout.addWidget(QLabel('X'), 0, 0, Qt.AlignCenter)
        self.fixCenterLayout.addWidget(QLabel('Y'), 0, 2, Qt.AlignCenter)
        self.fixCenterLayout.addWidget(self.fixCX, 0, 1)
        self.fixCenterLayout.addWidget(self.fixCY, 0, 3)
        self.fixCenterGrpBx.setLayout(self.fixCenterLayout)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet(pfss)

        self.nextButton = QPushButton()
        self.nextButton.setText(">>>")
        self.prevButton = QPushButton()
        self.prevButton.setText("<<<")
        self.buttonsLayout = QGridLayout()
        self.buttonsLayout.addWidget(self.processFolderButton,0,0,1,2)
        self.buttonsLayout.addWidget(self.prevButton,1,0,1,1)
        self.buttonsLayout.addWidget(self.nextButton,1,1,1,1)

        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingsGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.fixCenterGrpBx)

        self.optionsLayout.addStretch()
        self.optionsLayout.addLayout(self.buttonsLayout)
        self.frameOfKeys = QFrame()
        self.frameOfKeys.setFixedWidth(370)
        self.frameOfKeys.setLayout(self.optionsLayout)
        self.imageTabLayout.addWidget(self.frameOfKeys)


        ##### Result Tab #####
        self.resultTab = QWidget()
        self.resultTab.setContentsMargins(0, 0, 0, 0)
        self.resultTabLayout = QHBoxLayout(self.resultTab)
        self.tabWidget.addTab(self.resultTab, "Results")

        self.resultFigure = plt.figure(facecolor='#606060')
        self.resultAxes = self.resultFigure.add_subplot(111)
        self.resultVLayout = QVBoxLayout()
        self.resultCanvas = FigureCanvas(self.resultFigure)
        self.resultTabLayout.addWidget(self.resultCanvas)

        self.resultOptLayout = QVBoxLayout()

        self.resultOptFrame = QFrame()
        self.resultOptFrame.setFixedWidth(370)
        self.resultOptFrame.setLayout(self.resultOptLayout)
        self.resultTabLayout.addWidget(self.resultOptFrame)

        # background Subtraction
        self.resultDispOptGrp = QGroupBox("Display Options")
        self.resultDispOptLayout = QGridLayout(self.resultDispOptGrp)

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

        self.resultDispOptLayout.addWidget(self.resultminIntLabel, 0, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.spResultminInt, 0, 1, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultmaxIntLabel, 1, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.spResultmaxInt, 1, 1, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultZoomInB, 2, 0, 1, 1)
        self.resultDispOptLayout.addWidget(self.resultZoomOutB, 2, 1, 1, 1)

        self.bgSubGrpBx = QGroupBox()
        self.bgSubGrpBx.setTitle("Background Subtraction")
        self.bgSubGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)

        self.bgChoice = QComboBox()
        self.bgChoice.setCurrentIndex(0)
        # self.bgChoice.setFixedHeight(40)
        self.bgChoice.addItem("None")
        self.bgChoice.addItem("2D Convexhull")
        self.bgChoice.addItem("Circularly-symmetric")
        # self.bgChoice.addItem("Angular")
        self.bgChoice.addItem("White-top-hats")

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

        self.rminmaxWidgets = [self.setRminmaxButton, self.rminSpnBx, self.rminLabel, self.rmaxSpnBx, self.rmaxLabel, self.radiusLabel]

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
        self.smoothSpnBx.setValue(0)
        self.smoothSpnBx.setKeyboardTracking(False)
        self.smoothLabel = QLabel("Smoothing factor : ")

        self.tensionSpnBx = QDoubleSpinBox()
        self.tensionSpnBx.setRange(0, 100)
        self.tensionSpnBx.setValue(0)
        self.tensionSpnBx.setKeyboardTracking(False)
        self.tensionLabel = QLabel("Tension factor : ")

        self.tophat1SpnBx = QSpinBox()
        self.tophat1SpnBx.setRange(1, 100)
        self.tophat1SpnBx.setValue(5)
        self.tophat1SpnBx.setKeyboardTracking(False)
        self.tophat1Label = QLabel("Top-hat inside : ")

        self.tophat2SpnBx = QSpinBox()
        self.tophat2SpnBx.setRange(1, 100)
        self.tophat2SpnBx.setValue(20)
        self.tophat2SpnBx.setKeyboardTracking(False)
        self.tophat2Label = QLabel("Top-hat outside : ")

        self.tophat1SpnBx.setHidden(True)
        self.tophat1Label.setHidden(True)
        self.maxPixRange.setHidden(False)
        self.minPixRange.setHidden(False)
        self.pixRangeLabel.setHidden(False)

        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        separator.setLineWidth(1)
        # self.mergeRadiusButton = QPushButton("Set Manual Merge Radius")
        # self.mergeRadiusButton.setCheckable(True)
        # self.checkableButtons.append(self.mergeRadiusButton)
        self.mergeGradientLabel = QLabel("Merge Gradient :")
        self.sigmoidSpnBx = QDoubleSpinBox()
        self.sigmoidSpnBx.setDecimals(5)
        self.sigmoidSpnBx.setSingleStep(2)
        self.sigmoidSpnBx.setValue(0.1)
        self.sigmoidSpnBx.setRange(0, 100)
        self.sigmoidSpnBx.setKeyboardTracking(False)

        self.tophat2Widgets = [self.tophat2SpnBx, self.tophat2Label, self.mergeGradientLabel, self.sigmoidSpnBx, separator]

        self.bgLayout = QGridLayout()
        self.bgLayout.addWidget(QLabel("Method : "), 0, 0, 1, 1)
        self.bgLayout.addWidget(self.bgChoice, 0, 1, 1, 2)
        self.bgLayout.addWidget(self.setRminmaxButton, 2, 0, 1, 3)
        self.bgLayout.addWidget(self.radiusLabel, 3, 0, 2, 1)
        self.bgLayout.addWidget(self.rminLabel, 3, 1, 1, 1)
        self.bgLayout.addWidget(self.rmaxLabel, 3, 2, 1, 1)
        self.bgLayout.addWidget(self.rminSpnBx, 4, 1, 1, 1)
        self.bgLayout.addWidget(self.rmaxSpnBx, 4, 2, 1, 1)
        self.bgLayout.addWidget(self.thetaBinLabel, 5, 0, 1, 2)
        self.bgLayout.addWidget(self.thetabinCB, 5, 1, 1, 2)
        self.bgLayout.addWidget(self.radialBinLabel, 6, 0, 1, 2)
        self.bgLayout.addWidget(self.radialBinSpnBx, 6, 1, 1, 2)
        self.bgLayout.addWidget(self.pixRangeLabel, 7, 0, 1, 1)
        self.bgLayout.addWidget(self.minPixRange, 7, 1, 1, 1)
        self.bgLayout.addWidget(self.maxPixRange, 7, 2, 1, 1)
        self.bgLayout.addWidget(self.smoothLabel, 8, 0, 1, 2)
        self.bgLayout.addWidget(self.smoothSpnBx, 8, 2, 1, 1)
        self.bgLayout.addWidget(self.tensionLabel, 9, 0, 1, 2)
        self.bgLayout.addWidget(self.tensionSpnBx, 9, 2, 1, 1)
        self.bgLayout.addWidget(self.tophat1Label, 10, 0, 1, 2)
        self.bgLayout.addWidget(self.tophat1SpnBx, 10, 2, 1, 1)
        self.bgLayout.addWidget(separator, 11, 0, 1, 3)
        self.bgLayout.addWidget(self.tophat2Label, 12, 0, 1, 2)
        self.bgLayout.addWidget(self.tophat2SpnBx, 12, 2, 1, 1)
        self.bgLayout.addWidget(self.mergeGradientLabel, 13, 0, 1, 2)
        self.bgLayout.addWidget(self.sigmoidSpnBx, 13, 2, 1, 1)

        # self.bgLayout.setColumnStretch(0, 2)

        self.bgSubGrpBx.setLayout(self.bgLayout)

        self.resultOptLayout.addWidget(self.resultDispOptGrp)
        self.resultOptLayout.addSpacing(10)
        self.resultOptLayout.addWidget(self.bgSubGrpBx)
        self.resultOptLayout.addStretch()

        self.processFolderButton2 = QPushButton("Process Current Folder")
        self.processFolderButton2.setStyleSheet(pfss)
        self.nextButton2 = QPushButton()
        self.nextButton2.setText(">>>")
        self.prevButton2 = QPushButton()
        self.prevButton2.setText("<<<")
        self.buttonsLayout2 = QGridLayout()
        self.buttonsLayout2.addWidget(self.processFolderButton2, 0, 0, 1, 2)
        self.buttonsLayout2.addWidget(self.prevButton2, 1, 0, 1, 1)
        self.buttonsLayout2.addWidget(self.nextButton2, 1, 1, 1, 1)
        self.resultOptLayout.addLayout(self.buttonsLayout2)


        #### Status bar #####
        self.statusBar = QStatusBar()
        self.progressBar = QProgressBar()
        self.progressBar.setMaximum(100)
        self.progressBar.setMinimum(0)
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.imgDetailOnStatusBar = QLabel()
        self.imgCoordOnStatusBar = QLabel()
        self.imgPathOnStatusBar = QLabel()
        self.imgPathOnStatusBar.setText("  Please select an image or a folder to process")
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

        menubar = self.menuBar()
        # menubar.setNativeMenuBar(False)
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addAction(selectFolderAction)
        # fileMenu.addAction(caliSettingsAction)
        # fileMenu.addAction(exit)
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
        self.showSeparator.stateChanged.connect(self.refreshAllTabs)
        self.fixCX.returnPressed.connect(self.fixcxEntered)
        self.fixCY.returnPressed.connect(self.fixcyEntered)
        self.processFolderButton.clicked.connect(self.processFolder)
        self.processFolderButton2.clicked.connect(self.processFolder)
        self.nextButton.clicked.connect(self.nextClicked)
        self.prevButton.clicked.connect(self.prevClicked)
        self.nextButton2.clicked.connect(self.nextClicked)
        self.prevButton2.clicked.connect(self.prevClicked)
        self.spResultmaxInt.valueChanged.connect(self.refreshResultTab)
        self.spResultminInt.valueChanged.connect(self.refreshResultTab)

        self.selectImageButton.clicked.connect(self.browseFile)
        self.imgZoomInB.clicked.connect(self.imageZoomIn)
        self.imgZoomOutB.clicked.connect(self.imageZoomOut)
        self.setCenterRotationButton.clicked.connect(self.setCenterRotation)
        self.setRotationButton.clicked.connect(self.setRotation)
        self.maskThresSpnBx.valueChanged.connect(self.ignoreThresChanged)
        self.imageFigure.canvas.mpl_connect('button_press_event', self.imageClicked)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)
        self.imageFigure.canvas.mpl_connect('button_release_event', self.imageReleased)
        self.imageFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)

        ##### Result Tab #####
        self.resultZoomInB.clicked.connect(self.resultZoomIn)
        self.resultZoomOutB.clicked.connect(self.resultZoomOut)
        self.resultFigure.canvas.mpl_connect('button_press_event', self.resultClicked)
        self.resultFigure.canvas.mpl_connect('motion_notify_event', self.resultOnMotion)
        self.resultFigure.canvas.mpl_connect('button_release_event', self.resultReleased)
        self.resultFigure.canvas.mpl_connect('scroll_event', self.resultScrolled)

        self.bgChoice.currentIndexChanged.connect(self.bgChoiceChanged)
        self.bgSubGrpBx.clicked.connect(self.applyBGSub)
        self.thetabinCB.currentIndexChanged.connect(self.applyBGSub)
        self.radialBinSpnBx.valueChanged.connect(self.applyBGSub)
        self.smoothSpnBx.valueChanged.connect(self.applyBGSub)
        self.tensionSpnBx.valueChanged.connect(self.applyBGSub)
        self.minPixRange.valueChanged.connect(self.pixRangeChanged)
        self.maxPixRange.valueChanged.connect(self.pixRangeChanged)
        self.tophat2SpnBx.valueChanged.connect(self.applyBGSub)
        self.tophat1SpnBx.valueChanged.connect(self.applyBGSub)
        self.setRminmaxButton.clicked.connect(self.setManualRminmax)
        self.rminSpnBx.valueChanged.connect(self.RminRmaxChanged)
        self.rmaxSpnBx.valueChanged.connect(self.RminRmaxChanged)
        self.sigmoidSpnBx.valueChanged.connect(self.sigmoidChanged)
        # self.mergeRadiusButton.clicked.connect(self.mergeRadiusClicked)

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
    
    def setRotation(self):
        """
        Trigger when set center and rotation angle button is pressed
        """
        if self.setRotationButton.isChecked():
            # clear plot
            self.imgPathOnStatusBar.setText(
                "Rotate the line to the pattern equator (ESC to cancel)")
            ax = self.imageAxes
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            self.imageCanvas.draw_idle()
            self.function = ["im_rotate"]
        else:
            self.function = None
            self.resetStatusbar()
            
    def setCenterRotation(self):
        """
        Trigger when set center and rotation angle button is pressed
        """
        if self.setCenterRotationButton.isChecked():
            # clear plot
            self.imgPathOnStatusBar.setText("Click on 2 corresponding reflection peaks along the equator (ESC to cancel)")
            ax = self.imageAxes
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
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
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
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
        self.refreshResultTab()

    def imageZoomIn(self):
        """
        Trigger when set zoom in button is pressed (image tab)
        """
        if self.imgZoomInB.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a rectangle on the image to zoom in (ESC to cancel)")
            ax = self.imageAxes
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
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

        if self.function is not None and self.function[0] == 'ignorefold':
            self.function = None

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

                    cx = int(round((x1 + x2) / 2.))
                    cy = int(round((y1 + y2) / 2.))
                    M = cv2.getRotationMatrix2D(tuple(self.quadFold.info['center']),
                                                self.quadFold.info['rotationAngle'], 1)
                    invM = cv2.invertAffineTransform(M)
                    homo_coords = [cx, cy, 1.]
                    new_center = np.dot(invM, homo_coords)
                    self.quadFold.info['center'] = (int(round(new_center[0])), int(round(new_center[1])))
                    self.quadFold.info['rotationAngle'] = self.quadFold.info['rotationAngle'] + new_angle
                    self.deleteInfo(['avg_fold'])
                    self.setCenterRotationButton.setChecked(False)
                    self.processImage()
            elif func[0] == "im_rotate":
                # set rotation angle
                center = self.quadFold.info['center']

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

                self.quadFold.info['rotationAngle'] = self.quadFold.info['rotationAngle'] + new_angle
                self.deleteInfo(['avg_fold'])
                self.setRotationButton.setChecked(False)
                self.processImage()

    def imageOnMotion(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return

        x = event.xdata
        y = event.ydata
        img = self.quadFold.getRotatedImage()

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < img.shape[1] and y < img.shape[0]:
                self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]))

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
        elif func[0] == "im_center_rotate":
            # draw X on points and a line between points
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    del ax.lines
                    ax.lines = []
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    first_cross = ax.lines[:2]
                    del ax.lines
                    ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            self.imageCanvas.draw_idle()
        elif func[0] == "im_rotate":
            # draw line as angle
            center = self.quadFold.info["center"]
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            del ax.lines
            ax.lines = []
            ax.plot([x,x2],[y,y2], color = "g")
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
            # elif func[0] == "mrad":
            #     # Set new merge radius
            #     img = self.quadFold.info['avg_fold']
            #     center = (img.shape[1], img.shape[0])
            #     self.quadFold.info["merge_rad"] = int(round(distance((x, y), center)))
            #     self.deleteImgCache(['BgSubFold'])
            #     self.function = None
            #     self.processImage()
            #     self.mergeRadiusButton.setChecked(False)

    def resultOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """
        if not self.ableToProcess():
            return

        x = event.xdata
        y = event.ydata
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
        elif func[0] == "rminmax":
            # draw circles
            img = self.quadFold.info['avg_fold']
            center = (img.shape[1] - 1, img.shape[0] - 1)
            radius = distance((x, y), center)
            ax = self.resultAxes
            if len(ax.patches) > len(self.function) - 1:
                ax.patches.pop()
            ax.add_patch(
                patches.Circle(center, radius, linewidth=2, edgecolor='r', facecolor='none', linestyle='solid'))
            self.resultCanvas.draw_idle()
        elif func[0] == "mrad":
            # draw circle
            img = self.quadFold.info['avg_fold']
            center = (img.shape[1] - 1, img.shape[0] - 1)
            radius = distance((x, y), center)
            ax = self.resultAxes
            if len(ax.patches) > 0:
                ax.patches.pop()
            ax.add_patch(
                patches.Circle(center, radius, linewidth=2, edgecolor='r', facecolor='none', linestyle='solid'))
            self.resultCanvas.draw_idle()
        elif func[0] == "r_move":
            # move zoom in location when image dragged
            if self.result_zoom is not None:
                ax = self.resultAxes
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
            del ax.lines
            ax.lines = []
            self.resultCanvas.draw_idle()
        else:
            self.function = None
            self.setRminmaxButton.setChecked(False)
            self.refreshResultTab()
            self.resetStatusbar()
    #
    # def mergeRadiusClicked(self):
    #     """
    #     Prepare for merge radius settings after button clicked
    #     """
    #     if self.mergeRadiusButton.isChecked():
    #         self.imgPathOnStatusBar.setText(
    #             "Select merge radius on the image (ESC to cancel)")
    #         self.function = ['mrad']
    #         ax = self.resultAxes
    #         del ax.lines
    #         ax.lines = []
    #         self.resultCanvas.draw_idle()
    #     else:
    #         self.function = None
    #         self.mergeRadiusButton.setChecked(False)
    #         self.refreshResultTab()
    #         self.resetStatusbar()

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

        self.applyBGSub()

    def bgChoiceChanged(self):
        """
        Trigger when background subtraction method is changed
        """
        ind = self.bgChoice.currentIndex()
        cir = 2
        wth = 3
        angular = 4

        for w in self.rminmaxWidgets:
            w.setHidden(ind==0)

        self.tophat1SpnBx.setHidden(not ind == wth)
        self.tophat1Label.setHidden(not ind == wth)
        self.maxPixRange.setHidden(not (ind == angular or ind == cir))
        self.minPixRange.setHidden(not (ind == angular or ind == cir))
        self.pixRangeLabel.setHidden(not (ind == angular or ind == cir))
        self.thetaBinLabel.setHidden(not ind == angular)
        self.thetabinCB.setHidden(not ind == angular)
        self.radialBinSpnBx.setHidden(not ind == cir)
        self.radialBinLabel.setHidden(not ind == cir)
        self.smoothLabel.setHidden(not ind == cir)
        self.smoothSpnBx.setHidden(not ind == cir)
        self.tensionLabel.setHidden(not ind == cir)
        self.tensionSpnBx.setHidden(not ind == cir)

        hide_tophat2 = (ind == 0)

        for w in self.tophat2Widgets:
            w.setHidden(hide_tophat2)

        if self.quadFold is not None and not self.uiUpdating:
            self.applyBGSub()

    def sigmoidChanged(self):
        """
        Trigger when sigmoid param (merge gradient) is changed
        """
        if self.ableToProcess():
            self.deleteImgCache(['BgSubFold'])
            self.processImage()

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

    def fixcxEntered(self):
        """
        Trigger when fixed center X is changed
        """
        self.fixCY.setFocus() # set focus to Y
        if self.ableToProcess():
            self.resetAllManual()

    def fixcyEntered(self):
        """
        Trigger when fixed center Y is changed
        """
        self.fixCY.clearFocus()
        if self.ableToProcess():
            self.resetAllManual()

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

    def ableToProcess(self):
        # Check if image can be processed
        return (self.quadFold is not None and not self.uiUpdating)

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
                if inf in self.quadFold.imgCache.keys():
                    del self.quadFold.imgCache[inf]

    def initialWidgets(self, img):
        """
        Initial some widgets values which depends on current image
        :param img: selected image
        """
        self.uiUpdating = True
        min_val = img.min()
        max_val = img.max()
        self.spmaxInt.setRange(min_val, max_val)
        self.spmaxInt.setValue(max_val * .5)
        self.spmaxInt.setSingleStep(max_val * .05)
        self.spminInt.setRange(min_val, max_val)
        self.spminInt.setValue(min_val)
        self.spminInt.setSingleStep(max_val * .05)
        self.maskThresSpnBx.setRange(min_val, max_val)

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

        if self.quadFold.info.has_key("bgsub"):
            self.bgSubGrpBx.setChecked(self.quadFold.info['bgsub'] != -1)
            if self.quadFold.info['bgsub'] != -1:
                self.bgChoice.setCurrentIndex(self.quadFold.info['bgsub'])
                self.tophat1SpnBx.setValue(self.quadFold.info['tophat1'])
                self.tophat2SpnBx.setValue(self.quadFold.info['tophat2'])
                self.sigmoidSpnBx.setValue(self.quadFold.info["sigmoid"])
                self.maxPixRange.setValue(self.quadFold.info["cirmax"])
                self.minPixRange.setValue(self.quadFold.info["cirmin"])
                self.radialBinSpnBx.setValue(self.quadFold.info['radial_bin'])
                self.smoothSpnBx.setValue(self.quadFold.info['smooth'])
                self.tensionSpnBx.setValue(self.quadFold.info['tension'])

        if 'mask_thres' in self.quadFold.info.keys():
            self.maskThresSpnBx.setValue(self.quadFold.info['mask_thres'])
        elif self.maskThresSpnBx.value() == -999:
            if self.orig_img.shape == (1043, 981):  # Pilatus
                self.maskThresSpnBx.setValue(getMaskThreshold(img))
            else:
                self.maskThresSpnBx.setValue(img.min())

        self.spResultmaxInt.setRange(min_val + 1, max_val)
        self.spResultmaxInt.setValue(max_val * .1)
        self.spResultmaxInt.setSingleStep(max_val * .05)
        self.spResultminInt.setRange(min_val, max_val - 1)
        self.spResultminInt.setValue(min_val)
        self.spResultminInt.setSingleStep(max_val * .05)

        self.uiUpdating = False

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new QuadrantFolder object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        self.img_zoom = None
        self.result_zoom = None
        self.quadFold = QuadrantFolder(self.filePath, self.imgList[self.currentFileNumber])
        original_image = self.quadFold.orig_img
        self.imgDetailOnStatusBar.setText(
            str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype))
        self.initialWidgets(original_image)
        if self.quadFold.info.has_key('ignore_folds'):
            self.ignoreFolds = self.quadFold.info['ignore_folds']
        self.processImage()

    def closeEvent(self, ev):
        self.close()

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
            center = self.quadFold.info['center']

            self.fixCX.setText(str(center[0]))
            self.fixCY.setText(str(center[1]))

            ax = self.imageAxes
            ax.cla()
            img = self.quadFold.getRotatedImage()
            img = getBGR(get8bitImage(img, min=self.spminInt.value(), max=self.spmaxInt.value()))
            ax.imshow(img)

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
                        ax.plot([center[0], img.shape[1]], [center[1], 0], color="w")
                        ax.plot([center[0], img.shape[1]], [0, center[1]], color="w")
                    if fold == 2:
                        ax.plot([0, center[0]], [center[1], img.shape[0]], color="w")
                        ax.plot([0, center[0]], [img.shape[0], center[1]], color="w")
                    if fold == 3:
                        ax.plot([center[0], img.shape[1]], [center[1], img.shape[0]], color="w")
                        ax.plot([center[0], img.shape[1]], [img.shape[0], center[1]], color="w")

            # Set Zoom in location
            if self.img_zoom is not None and len(self.img_zoom) == 2:
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
            else:
                ax.set_xlim((0, img.shape[1]))
                ax.set_ylim((0, img.shape[0]))

            self.img_zoom = [ax.get_xlim(), ax.get_ylim()]
            ax.invert_yaxis()
            self.imageFigure.tight_layout()
            self.imageCanvas.draw()

            self.updated['img'] = True
            self.uiUpdating = False

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

            # convert image for displaying
            img = getBGR(get8bitImage(img, max=self.spResultmaxInt.value(), min=self.spResultminInt.value()))

            ax = self.resultAxes
            ax.cla()
            ax.imshow(img)

            # if self.showSeparator.isChecked():
            #     ax.axvline(center[0]-1, color='y')
            #     ax.axhline(center[1]-1, color='y')

            # Set Zoom in location
            if self.result_zoom is not None and len(self.result_zoom) == 2:
                ax.set_xlim(self.result_zoom[0])
                ax.set_ylim(self.result_zoom[1])
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

            try:
                self.quadFold.process(flags)
            except Exception, e:
                QApplication.restoreOverrideCursor()
                errMsg = QMessageBox()
                errMsg.setText('Unexpected error')
                msg = 'Please report the problem with error message below and the input image (.tif)\n\n'
                msg += "Image : "+str(self.quadFold.img_name)
                msg += "\n\nError : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
                errMsg.setInformativeText(msg)
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.setFixedWidth(300)
                errMsg.exec_()
                raise

            self.refreshAllTabs()

            # Save result to folder qf_results
            if 'resultImg' in self.quadFold.imgCache.keys():
                result_path = fullPath(self.filePath, 'qf_results')
                createFolder(result_path)

                result_file = fullPath(result_path, self.imgList[self.currentFileNumber]) + ".result.tif"
                img = self.quadFold.imgCache['resultImg']

                # if self.quadFold.info['imgType'] == 'float32':
                #     img = get16bitImage(img)
                # else:
                #     img = img.astype(self.quadFold.info['imgType'])
                img = img.astype("float32")

                # cv2.imwrite(result_file, img)
                imsave(result_file, img)
                # plt.imsave(fullPath(result_path, self.imgList[self.currentFileNumber])+".result2.tif", img)

            QApplication.restoreOverrideCursor()

    def resetStatusbar(self):
        fileFullPath = fullPath(self.filePath, self.imgList[self.currentFileNumber])
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(self.currentFileNumber + 1) + '/' + str(self.numberOfFiles) + ') : ' + fileFullPath)

    def getFlags(self):
        """
        Get all flags for QuadrantFolder process() from widgets
        :return: flags (dict)
        """
        flags = {}

        if self.fixCenterGrpBx.isChecked():
            flags['center'] = [int(self.fixCX.text()), int(self.fixCY.text())]

        flags["ignore_folds"] = self.ignoreFolds

        flags['bgsub'] = self.bgChoice.currentIndex()
        flags["cirmin"] = self.minPixRange.value()
        flags["cirmax"] = self.maxPixRange.value()
        flags["bin_theta"] = int(self.thetabinCB.currentText())
        flags['radial_bin'] = self.radialBinSpnBx.value()
        flags['smooth'] = self.smoothSpnBx.value()
        flags['tension'] = self.tensionSpnBx.value()
        flags["tophat1"] = self.tophat1SpnBx.value()
        flags["tophat2"] = self.tophat2SpnBx.value()
        flags['mask_thres'] = self.maskThresSpnBx.value()
        flags['sigmoid'] = self.sigmoidSpnBx.value()

        if self.rmaxSpnBx.value() > self.rminSpnBx.value() > 0:
            flags['fixed_rmin'] = self.rminSpnBx.value()
            flags['fixed_rmax'] = self.rmaxSpnBx.value()

        return flags

    def onNewFileSelected(self, newFile):
        """
        Preprocess folder of the file and process current image
        :param newFile: full name of selected file
        """
        self.filePath, self.imgList, self.currentFileNumber = getImgFiles(str(newFile))
        self.numberOfFiles = len(self.imgList)

        self.ignoreFolds = set()
        self.selectImageButton.setHidden(True)
        self.selectFolder.setHidden(True)
        self.imageCanvas.setHidden(False)
        self.resetWidgets()
        self.onImageChanged()

    def resetWidgets(self):
        self.uiUpdating = True
        self.rminSpnBx.setValue(-1)
        self.rmaxSpnBx.setValue(-1)
        self.fixCenterGrpBx.setChecked(False)
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

    def processFolder(self):
        fileList = os.listdir(self.filePath)
        self.imgList = []
        for f in fileList:
            if isImg(fullPath(self.filePath, f)):
                self.imgList.append(f)

        self.imgList.sort()
        self.numberOfFiles = len(self.imgList)

        errMsg = QMessageBox()
        errMsg.setText('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'

        flags = self.getFlags()
        text += "\nCurrent Settings"
        if 'center' in flags.keys():
            text += "\n  - Center : " + str(flags["center"])
        if len(self.ignoreFolds) > 0:
            text += "\n  - Ignore Folds : " + str(list(self.ignoreFolds))
        text += "\n  - Mask Threshold : " + str(flags["mask_thres"])
        text += "\n  - Background Subtraction Method : "+ str(self.bgChoice.currentText())

        if flags['bgsub'] != 0:
            if 'fixed_rmin' in flags.keys() and flags['bgsub'] in [1,2,3]:
                text += "\n  - R-min : " + str(flags["fixed_rmin"])
                text += "\n  - R-max : " + str(flags["fixed_rmax"])

            if flags['bgsub'] in [2,3]:
                text += "\n  - Pixel Range (Percentage) : " + str(flags["cirmin"]) + "% - "+str(flags["cirmax"])+"%"

            if flags['bgsub'] == 2:
                text += "\n  - Radial Bin : " + str(flags["radial_bin"])
                text += "\n  - Smooth : " + str(flags["smooth"])
            elif flags['bgsub'] == 3:
                text += "\n  - Theta Bin : " + str(flags["bin_theta"])
            elif flags['bgsub'] == 4:
                text += "\n  - Tophat (inside merge radius) : " + str(flags["tophat1"])

            text += "\n  - Tophat (outside merge radius) : " + str(flags["tophat2"])
            text += "\n  - Merge Gradient : " + str(flags["sigmoid"])

        text += '\n\nAre you sure you want to process ' + str(self.numberOfFiles) + ' image(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            self.progressBar.setVisible(True)
            for i in range(self.numberOfFiles):
                self.progressBar.setValue(100. / self.numberOfFiles * i)
                QApplication.processEvents()
                self.nextClicked()
            self.progressBar.setVisible(False)

    def browseFile(self):
        """
        Popup input dialog and set file selection
        """
        file_name = getAFile()
        if file_name != "":
            self.onNewFileSelected(str(file_name))
            self.centralWidget.setMinimumSize(700, 500)

    def prevClicked(self):
        """
        Going to the previous image
        """
        if self.numberOfFiles > 0:
            self.currentFileNumber = (self.currentFileNumber - 1) % self.numberOfFiles
            self.onImageChanged()

    def nextClicked(self):
        """
        Going to the next image
        """
        if self.numberOfFiles > 0:
            self.currentFileNumber = (self.currentFileNumber + 1) % self.numberOfFiles
            self.onImageChanged()


if __name__ == "__main__":
    app = QApplication(sys.argv)
    myapp = QuadrantFoldingGUI()
    sys.exit(app.exec_())