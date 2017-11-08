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
import os, shutil
from pyqt_utils import *
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from os.path import split
import traceback
import webbrowser
from ..CalibrationSettings import CalibrationSettings
from ..utils.file_manager import fullPath, getImgFiles, getStyleSheet
from ..modules.BioImage import BioImage, getCardiacGraph
from ..utils.image_processor import *
from ..csv_manager import BM_CVSManager
from ..ui.BM_FittingTab import BM_FittingTab
import musclex

class BioMuscleWindow(QMainWindow):
    """
    Window displaying all information of a selected image.
    This window contains 3 tabs : image, fitting, results
    """
    def __init__(self, mainWin, filename):
        """
        Init window with main window object and selected file name
        :param mainWin: main window object
        :param filename: selected file name
        """
        QWidget.__init__(self)
        self.mainWindow = mainWin
        self.version = musclex.__version__
        self.bioImg = None  # Current BioImage object
        self.img_zoom = None  # Params for x and y ranges of displayed image in image tab
        self.graph_zoom = None # Params for x and y ranges of displayed graph in fitting tab
        self.function = None  # Current active function
        self.syncUI = False  # boolean status for UI sync. Prevent recursive infinite processing
        self.update_plot = {'img': True, 'graph' :True, 'results': True}  # update status of each tab
        self.fixedIntArea = None
        self.dir_path, self.imgList, self.currentImg = getImgFiles(str(filename))
        if len(self.imgList) == 0:
            self.inputerror()
            return
        self.csvManager = BM_CVSManager(self.dir_path)  # Create a CSV Manager object
        self.setWindowTitle("Bio-Muscle v." + self.version)
        self.setStyleSheet(getStyleSheet())
        self.initUI()  # Initial all UI
        self.setAllToolTips()  # Set tooltips for widgets
        self.setConnections()  # Set interaction for widgets
        self.setCalibrationImage()
        self.onImageChanged() # Toggle window to process current image
        self.show()
        self.resize(1000, 100)
        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

    def inputerror(self):
        # Display input error to screen
        errMsg = QMessageBox()
        errMsg.setText('Invalid Input')
        errMsg.setInformativeText("Please select only failedcases.txt or .tif image\n\n")
        errMsg.setStandardButtons(QMessageBox.Ok)
        errMsg.setIcon(QMessageBox.Warning)
        errMsg.exec_()
        self.close()

    def mousePressEvent(self, event):
        # Clear focus when mouse pressed
        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

    def initUI(self):
        """
        Initial all GUIs including : image tab, fitting tab, results tab, menu bar, and status bar
        """
        self.centralWidget = QWidget(self)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 20px; width: 200px; }")
        self.checkableButtons = []

        #
        ### Image Tab ###
        #
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.displayImgFigure = plt.figure(facecolor='#606060')
        self.displayImgAxes = self.displayImgFigure.add_subplot(111)
        self.imageVLayout = QVBoxLayout()
        self.displayImgCanvas = FigureCanvas(self.displayImgFigure)
        self.imageVLayout.addWidget(self.displayImgCanvas)

        self.imgDispOptionGrp = QGroupBox('Display Options')
        self.imgDispOptLayout = QVBoxLayout()
        self.centerChkBx = QCheckBox('Center')
        self.centerChkBx.setChecked(True)
        self.rminChkBx = QCheckBox('R-min')
        self.rminChkBx.setChecked(True)
        self.histChkBx = QCheckBox('Histogram')
        self.histChkBx.setChecked(True)
        self.intChkBx = QCheckBox('Integrated Area')
        self.intChkBx.setChecked(True)
        self.imgPeakChkBx = QCheckBox('Peaks')
        self.imgPeakChkBx.setChecked(True)
        self.minIntLabel = QLabel()
        self.maxIntLabel = QLabel()
        self.minmaxLabelLayout = QHBoxLayout()
        self.minmaxLabelLayout.addWidget(self.minIntLabel)
        self.minmaxLabelLayout.addWidget(self.maxIntLabel)
        self.minIntSpnBx = QDoubleSpinBox()
        self.minIntSpnBx.setKeyboardTracking(False)
        self.maxIntSpnBx = QDoubleSpinBox()
        self.maxIntSpnBx.setKeyboardTracking(False)
        self.minmaxLayout = QHBoxLayout()
        self.minmaxLayout.addWidget(self.minIntSpnBx)
        self.minmaxLayout.addWidget(self.maxIntSpnBx)
        self.imgZoomInB = QPushButton('Zoom In')
        self.imgZoomInB.setCheckable(True)
        self.checkableButtons.append(self.imgZoomInB)
        self.imgZoomOutB = QPushButton('Full')
        self.checkableButtons.append(self.imgZoomOutB)
        self.imgDispOptLayout.addWidget(self.centerChkBx)
        self.imgDispOptLayout.addWidget(self.intChkBx)
        self.imgDispOptLayout.addWidget(self.rminChkBx)
        self.imgDispOptLayout.addWidget(self.histChkBx)
        self.imgDispOptLayout.addWidget(self.imgPeakChkBx)
        self.imgDispOptLayout.addLayout(self.minmaxLabelLayout)
        self.imgDispOptLayout.addLayout(self.minmaxLayout)
        self.imgDispOptLayout.addWidget(self.imgZoomInB)
        self.imgDispOptLayout.addWidget(self.imgZoomOutB)
        self.imgDispOptionGrp.setLayout(self.imgDispOptLayout)

        self.imgProcGrp = QGroupBox("Image Processing")
        self.imgProcLayout = QGridLayout()
        self.imgProcGrp.setLayout(self.imgProcLayout)
        self.calibrationB = QPushButton("Calibration Settings")
        self.setRotAndCentB = QPushButton("Set Rotation Angle \nand Center")
        self.setRotAndCentB.setCheckable(True)
        self.setRotAndCentB.setFixedHeight(40)
        self.setAngleB = QPushButton("Set \nRotation Angle")
        self.setAngleB.setCheckable(True)
        self.setAngleB.setFixedHeight(40)
        self.setRminB = QPushButton("Set R-min")
        self.setRminB.setCheckable(True)
        self.setRminB.setFixedHeight(40)
        self.setIntAreaB = QPushButton("Set Box Width")
        self.setIntAreaB.setCheckable(True)
        self.setIntAreaB.setFixedHeight(40)
        self.checkableButtons.extend([self.setRotAndCentB, self.setIntAreaB, self.setRminB, self.setAngleB])
        self.fixedAngleChkBx = QCheckBox("Fixed Angle")
        self.fixedAngleChkBx.setChecked(False)
        self.fixedIntAreaChkBx = QCheckBox("Fixed Box Width")
        self.fixedIntAreaChkBx.setChecked(False)
        self.fixedAngle = QSpinBox()
        self.fixedAngle.setKeyboardTracking(False)
        self.fixedAngle.setRange(-360, 360)
        self.fixedAngle.setEnabled(False)
        self.maskThresSpnBx =QDoubleSpinBox()
        self.maskThresSpnBx.setRange(-10,10)
        self.maskThresSpnBx.setKeyboardTracking(False)

        self.resetAllB = QPushButton("Reset All")
        self.imgProcLayout.addWidget(self.calibrationB, 0, 0, 1, 2)
        self.imgProcLayout.addWidget(self.setRotAndCentB, 1, 0, 1, 1)
        self.imgProcLayout.addWidget(self.setAngleB, 1, 1, 1, 1)
        self.imgProcLayout.addWidget(self.setRminB, 2, 0, 1, 1)
        self.imgProcLayout.addWidget(self.setIntAreaB, 2, 1, 1, 1)
        self.imgProcLayout.addWidget(self.fixedAngleChkBx, 3, 0, 1, 1)
        self.imgProcLayout.addWidget(self.fixedAngle, 3, 1, 1, 1)
        self.imgProcLayout.addWidget(self.fixedIntAreaChkBx, 4, 0, 1, 2)
        self.imgProcLayout.addWidget(QLabel("Mask Threshold"), 5, 0, 1, 1)
        self.imgProcLayout.addWidget(self.maskThresSpnBx, 5, 1, 1, 1)
        self.imgProcLayout.addWidget(self.resetAllB, 6, 0, 1, 2)

        self.rejectChkBx = QCheckBox("Reject")
        self.rejectChkBx.setFixedWidth(100)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet(pfss)
        self.bottomLayout = QGridLayout()
        self.prevButton = QPushButton("<<<")
        self.nextButton = QPushButton(">>>")
        self.bottomLayout.addWidget(self.rejectChkBx, 0, 0, 1, 2)
        self.bottomLayout.addWidget(self.processFolderButton, 1, 0, 1, 2)
        self.bottomLayout.addWidget(self.prevButton, 2, 0, 1, 1)
        self.bottomLayout.addWidget(self.nextButton, 2, 1, 1, 1)
        self.bottomLayout.setAlignment(self.rejectChkBx, Qt.AlignLeft)

        self.imageOptionsFrame = QFrame()
        self.imageOptionsFrame.setFixedWidth(300)
        self.imageOptionsLayout = QVBoxLayout()

        self.imageOptionsLayout.setAlignment(Qt.AlignTop)
        self.imageOptionsLayout.addSpacing(10)
        self.imageOptionsLayout.addWidget(self.imgDispOptionGrp)
        self.imageOptionsLayout.addSpacing(10)
        self.imageOptionsLayout.addWidget(self.imgProcGrp)
        self.imageOptionsLayout.addStretch()
        self.imageOptionsLayout.addLayout(self.bottomLayout)
        self.imageOptionsFrame.setLayout(self.imageOptionsLayout)

        self.imageTabLayout.addLayout(self.imageVLayout)
        self.imageTabLayout.addWidget(self.imageOptionsFrame)
        self.tabWidget.addTab(self.imageTab, "Image")

        #
        ### Fitting Tab ###
        #
        self.fittingTab = QWidget()
        self.fittingTabLayout = QHBoxLayout(self.fittingTab)

        # Plot
        self.fittingFigure = plt.figure(facecolor='#606060')
        self.fittingAxes = self.fittingFigure.add_subplot(111)
        self.fittingVLayout = QVBoxLayout()
        self.fittingCanvas = FigureCanvas(self.fittingFigure)

        self.generalGrp = QGroupBox("General Settings")
        self.genLayout = QGridLayout(self.generalGrp)
        self.skeletalChkBx = QCheckBox("Skeletal Muscle (Z line)")
        self.skeletalChkBx.setFixedWidth(200)
        self.nPeakSpnBx = QSpinBox()
        self.nPeakSpnBx.setKeyboardTracking(False)
        self.nPeakSpnBx.setMinimum(2)
        self.nPeakSpnBx.setMaximum(40)
        self.nPeakSpnBx.setSingleStep(1)
        self.nPeakSpnBx.setValue(2)
        self.modelSelect = QComboBox()
        self.modelSelect.addItem("Voigt")
        self.modelSelect.addItem("Gaussian")
        self.modelSelect.setCurrentIndex(0)
        self.setPeaksB = QPushButton("Start Manual Peak Selection")
        self.setPeaksB.setCheckable(True)
        self.checkableButtons.append(self.setPeaksB)

        self.genLayout.addWidget(self.skeletalChkBx, 0, 0, 1, 2)
        self.genLayout.addWidget(QLabel("Number of peaks : <br/>on each side"), 1, 0, 1, 1)
        self.genLayout.addWidget(self.nPeakSpnBx, 1, 1, 1, 1)
        self.genLayout.addWidget(QLabel("Model : "), 2, 0, 1, 1)
        self.genLayout.addWidget(self.modelSelect, 2, 1, 1, 1)
        self.genLayout.addWidget(self.setPeaksB, 3, 0, 1, 2)

        self.fitDispOptionGrp = QGroupBox('Display Options')
        self.fitDispOptLayout = QGridLayout()
        self.origHistChkBx = QCheckBox('Original Histogram')
        self.hullChkBx = QCheckBox('After Convexhull')
        self.hullChkBx.setChecked(True)
        self.fitChkBx = QCheckBox('Fitting Graph')
        self.fitChkBx.setChecked(True)
        self.peakChkBx = QCheckBox('Peaks')
        self.peakChkBx.setChecked(True)
        self.dispZlineChkBx = QCheckBox("Z line")
        self.dispZlineChkBx.setChecked(False)
        self.centerXChkBx = QCheckBox('Center X')
        self.centerXChkBx.setChecked(True)
        self.graphZoomInB = QPushButton('Zoom In')
        self.graphZoomInB.setCheckable(True)
        self.checkableButtons.append(self.graphZoomInB)
        self.graphZoomOutB = QPushButton('Full')
        self.checkableButtons.append(self.graphZoomOutB)
        self.fitDispOptLayout.addWidget(self.origHistChkBx, 0, 0, 1, 1)
        self.fitDispOptLayout.addWidget(self.hullChkBx, 0, 1, 1, 1)
        self.fitDispOptLayout.addWidget(self.peakChkBx, 1, 0, 1, 1)
        self.fitDispOptLayout.addWidget(self.fitChkBx, 1, 1, 1, 1)
        self.fitDispOptLayout.addWidget(self.dispZlineChkBx, 2, 0, 1, 1)
        self.fitDispOptLayout.addWidget(self.centerXChkBx, 2, 1, 1, 1)
        self.fitDispOptLayout.addWidget(self.graphZoomInB, 3, 0, 1, 1)
        self.fitDispOptLayout.addWidget(self.graphZoomOutB, 3, 1, 1, 1)
        self.fitDispOptionGrp.setLayout(self.fitDispOptLayout)

        self.fittingTabWidget = QTabWidget()
        self.fittingTabWidget.setTabPosition(QTabWidget.North)
        self.fittingTabWidget.setDocumentMode(False)
        self.fittingTabWidget.setTabsClosable(False)
        self.fittingTabWidget.setStyleSheet("QTabBar::tab { height: 20px; width: 50px; }")

        self.left_fitting_tab = BM_FittingTab(self, "left")
        self.right_fitting_tab = BM_FittingTab(self, "right")
        self.fittingTabWidget.addTab(self.left_fitting_tab, "Left")
        self.fittingTabWidget.addTab(self.right_fitting_tab, "Right")

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton2 = QPushButton("Process Current Folder")
        self.processFolderButton2.setStyleSheet(pfss)
        self.bottomLayout2 = QGridLayout()
        self.prevButton2 = QPushButton("<<<")
        self.nextButton2 = QPushButton(">>>")
        self.bottomLayout2.addWidget(self.processFolderButton2, 0, 0, 1, 2)
        self.bottomLayout2.addWidget(self.prevButton2, 1, 0, 1, 1)
        self.bottomLayout2.addWidget(self.nextButton2, 1, 1, 1, 1)

        self.fittingOptionsFrame = QFrame()
        self.fittingOptionsFrame.setFixedWidth(300)
        self.fittingOptionsLayout = QVBoxLayout()
        self.fittingOptionsLayout.setAlignment(Qt.AlignTop)
        self.fittingOptionsLayout.addWidget(self.generalGrp)
        self.fittingOptionsLayout.addSpacing(10)
        self.fittingOptionsLayout.addWidget(self.fitDispOptionGrp)
        self.fittingOptionsLayout.addSpacing(10)
        self.fittingOptionsLayout.addWidget(self.fittingTabWidget)
        self.fittingOptionsLayout.addStretch()
        self.fittingOptionsLayout.addLayout(self.bottomLayout2)
        self.fittingOptionsFrame.setLayout(self.fittingOptionsLayout)

        self.fittingTabLayout.addWidget(self.fittingCanvas)
        self.fittingTabLayout.addWidget(self.fittingOptionsFrame)
        self.tabWidget.addTab(self.fittingTab, "Fitting")
        #
        ### Results Tab ###
        #
        self.resultTab = QWidget()
        self.resultTab.setContentsMargins(0, 0, 0, 0)
        self.resultLayout = QGridLayout(self.resultTab)
        self.generalResults = QLabel("General Results")

        self.fiberResultTable = QTableWidget()
        self.fiberResultTable.setColumnCount(3)
        self.fiberResultTable.setHorizontalHeaderLabels(["Info", "Left", "Right"])
        self.fiberResultTable.horizontalHeader().setStretchLastSection(True)
        self.fiberResultTable.setColumnWidth(0, 150)
        self.fiberResultTable.setColumnWidth(1, 350)
        self.fiberResultTable.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self.resultLayout.addWidget(self.generalResults)
        # self.resultLayout.addWidget(self.generalResultTable)
        self.resultLayout.addWidget(QLabel("<h2>Fitting Results</h2>"))
        self.resultLayout.addWidget(self.fiberResultTable)
        self.tabWidget.addTab(self.resultTab, "Results")

        #
        ### Menu Bar ###
        #
        selectImageAction = QAction('Select a File...', self)
        selectImageAction.setShortcut('Ctrl+O')
        selectImageAction.triggered.connect(self.browseFile)
        clearCacheAction = QAction('Clear All Caches in Current Folder', self)
        clearCacheAction.setShortcut('Ctrl+D')
        clearCacheAction.triggered.connect(self.clearAllCache)
        processFolderAction = QAction('Process Current Folder', self)
        processFolderAction.setShortcut('Ctrl+F')
        processFolderAction.triggered.connect(self.processFolder)
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addAction(processFolderAction)
        fileMenu.addAction(clearCacheAction)

        launchManualAct = QAction('User Manual', self)
        # launchManualAct.setShortcut('Ctrl+M')
        launchManualAct.triggered.connect(self.launchManual)
        shortcutKeysAct = QAction('Shortcut keys', self)
        # shortcutKeysAct.setShortcut('Ctrl+K')
        shortcutKeysAct.triggered.connect(self.showKeysHelpDialog)
        aboutAct = QAction('About', self)
        # aboutAct.setShortcut('Ctrl+A')
        aboutAct.triggered.connect(self.showAbout)
        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)
        helpMenu.addAction(launchManualAct)
        helpMenu.addAction(shortcutKeysAct)
        # helpMenu.addAction(aboutAct)

        #
        ### Status Bar ###
        #
        self.statusBar = QStatusBar()
        self.left_status = QLabel()
        self.right_status = QLabel()
        self.pixel_detail = QLabel()
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusBar.addWidget(self.left_status)
        self.statusBar.addPermanentWidget(self.pixel_detail)
        self.statusBar.addPermanentWidget(self.right_status)
        self.statusBar.addPermanentWidget(self.progressBar)

        self.mainLayout.addWidget(self.tabWidget)
        self.mainLayout.addWidget(self.statusBar)

    def setAllToolTips(self):
        """
        Set Tooltips for widgets
        """
        #
        ### image tab ###
        #
        self.centerChkBx.setToolTip("Show the detected projection center")
        self.intChkBx.setToolTip("Show the detected Integrated area ")
        self.rminChkBx.setToolTip("Show the detected R-min")
        self.histChkBx.setToolTip("Show the background subtracted histogram obtained using convex hull operators")
        self.imgPeakChkBx.setToolTip("Show the detected peaks")
        self.minIntSpnBx.setToolTip("Increase in the minimal intensity shown to allow for more details in the image")
        self.maxIntSpnBx.setToolTip("Reduction in the maximal intensity shown to allow for more details in the image")
        self.imgZoomInB.setToolTip("Activate zoom-in operation, click on the image to select zoom-in area")
        self.imgZoomOutB.setToolTip("Activate zoom-in operation")
        self.calibrationB.setToolTip("Launch Calibration Setting Window")
        self.setRotAndCentB.setToolTip(
            "Activate rotation and center adjustment.\n To adjust, please click 2 center locations of coresponding diffraction on the image")
        self.setAngleB.setToolTip(
            "Activate rotation adjustment.\n To adjust, please click when the line is on the equator of diffraction")
        self.setRminB.setToolTip("Activate R-min adjustment.\n To adjust, please click location of R-min on the image")
        self.setIntAreaB.setToolTip(
            "Activate Integrated Area adjustment.\n To adjust, please click start and end position of the Integrated area on the image")
        self.maskThresSpnBx.setToolTip("Pixel values to discard")
        self.resetAllB.setToolTip("Reset all manual settings, and process image again with default detection")
        self.rejectChkBx.setToolTip(
            "Reject the case when model cannot be fitted. The word \"REJECTED\" will appear in summary.csv")
        self.nextButton.setToolTip("Go to the next image in this folder")
        self.prevButton.setToolTip("Go to the previous image in this folder")
        self.processFolderButton.setToolTip("Process all images in the same directory as the current file")

        #
        ### Fitting tab ###
        #
        self.nPeakSpnBx.setToolTip("Select number of peaks on each side that will be fitted by model")
        self.modelSelect.setToolTip("Select the fitting model")
        self.setPeaksB.setToolTip(
            "Activate manual peak selections. Click on the graph to select the position of the peaks. Click Done when you finish")
        self.origHistChkBx.setToolTip("Show original histogram")
        self.hullChkBx.setToolTip("Show the background subtracted histogram obtained using convex hull operators.")
        self.peakChkBx.setToolTip("Show the detected peaks")
        self.dispZlineChkBx.setToolTip("Show the detected skeletal peaks (Skeletal Muscle needs to be checked)")
        self.centerXChkBx.setToolTip("Show the fitting model center X")
        self.fitChkBx.setToolTip("Show the fitting graph")
        self.graphZoomInB.setToolTip("Activate zoom-in operation, click on the graph to select zoom-in area")
        self.graphZoomOutB.setToolTip("Activate zoom-in operation")
        self.nextButton2.setToolTip("Go to the next image in this folder")
        self.prevButton2.setToolTip("Go to the previous image in this folder")
        self.processFolderButton2.setToolTip("Process all images in the same directory as the current file")

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        #
        ### Tab Widget ###
        #
        self.tabWidget.currentChanged.connect(self.updateUI)
        self.fittingTabWidget.currentChanged.connect(self.updateUI)

        #
        ### Image Tab ###
        #
        self.centerChkBx.stateChanged.connect(self.updateImage)
        self.histChkBx.stateChanged.connect(self.updateImage)
        self.intChkBx.stateChanged.connect(self.updateImage)
        self.rminChkBx.stateChanged.connect(self.updateImage)
        self.imgPeakChkBx.stateChanged.connect(self.updateImage)
        self.minIntSpnBx.valueChanged.connect(self.intensityChanged)
        self.maxIntSpnBx.valueChanged.connect(self.intensityChanged)
        self.imgZoomInB.clicked.connect(self.imgZoomIn)
        self.imgZoomOutB.clicked.connect(self.imgZoomOut)

        self.calibrationB.clicked.connect(self.calibrationClicked)
        self.setRotAndCentB.clicked.connect(self.setAngleAndCenterClicked)
        self.setAngleB.clicked.connect(self.setAngleClicked)
        self.setRminB.clicked.connect(self.setRminClicked)
        self.setIntAreaB.clicked.connect(self.setIntAreaClicked)
        self.fixedAngleChkBx.stateChanged.connect(self.fixedAngleChecked)
        self.fixedIntAreaChkBx.stateChanged.connect(self.fixedIntAreaChecked)
        self.fixedAngle.valueChanged.connect(self.fixedAngleChanged)
        self.maskThresSpnBx.valueChanged.connect(self.maskThresChanged)
        self.resetAllB.clicked.connect(self.resetAll)

        self.prevButton.clicked.connect(self.prevClicked)
        self.nextButton.clicked.connect(self.nextClicked)
        self.processFolderButton.clicked.connect(self.processFolder)
        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)
        self.displayImgFigure.canvas.mpl_connect('button_release_event', self.imgReleased)
        self.displayImgFigure.canvas.mpl_connect('figure_leave_event', self.leaveImage)
        self.displayImgFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)
        self.rejectChkBx.stateChanged.connect(self.rejectClicked)

        #### Fitting Tab
        self.skeletalChkBx.stateChanged.connect(self.refreshAllFittingParams)
        self.nPeakSpnBx.valueChanged.connect(self.nPeakChanged)
        self.modelSelect.currentIndexChanged.connect(self.refreshAllFittingParams)
        self.setPeaksB.clicked.connect(self.setManualPeaks)
        self.origHistChkBx.stateChanged.connect(self.refreshGraph)
        self.hullChkBx.stateChanged.connect(self.refreshGraph)
        self.peakChkBx.stateChanged.connect(self.refreshGraph)
        self.dispZlineChkBx.stateChanged.connect(self.refreshGraph)
        self.centerXChkBx.stateChanged.connect(self.refreshGraph)
        self.fitChkBx.stateChanged.connect(self.refreshGraph)
        self.graphZoomInB.clicked.connect(self.graphZoomIn)
        self.graphZoomOutB.clicked.connect(self.graphZoomOut)

        self.processFolderButton2.clicked.connect(self.processFolder)
        self.prevButton2.clicked.connect(self.prevClicked)
        self.nextButton2.clicked.connect(self.nextClicked)
        self.fittingFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.fittingFigure.canvas.mpl_connect('motion_notify_event', self.plotOnMotion)
        self.fittingFigure.canvas.mpl_connect('button_release_event', self.plotReleased)
        self.fittingFigure.canvas.mpl_connect('figure_leave_event', self.leavePlot)
        self.fittingFigure.canvas.mpl_connect('scroll_event', self.plotScrolled)

    def graphZoomIn(self):
        """
        Triggered when Zoom in graph is pressed
        """
        if self.graphZoomInB.isChecked():
            self.function = ["g_zoomin"]
            self.setLeftStatus("Please select zoom-in area by clicking 2 points to make a rectangle (ESC to cancel)")
        else:
            self.function = None

    def graphZoomOut(self):
        """
        Triggered when Zoom out graph is pressed
        """
        self.graphZoomInB.setChecked(False)
        self.graph_zoom = None
        self.refreshGraph()

    def setManualPeaks(self):
        """
        Start or end manual peak selections
        """
        bioImg = self.bioImg
        if bioImg is None:
            return

        if self.setPeaksB.isChecked():
            # Prepare for start selection
            self.setLeftStatus("Please click location of peaks on the graph, and click Done when all peaks selected (ESC to cancel)")
            self.setPeaksB.setText("Done")
            self.function = ['peaks', []]
            # Remove all lines and draw only background subtracted histogram
            ax = self.fittingAxes
            del ax.lines
            ax.lines = []
            ax.plot(bioImg.info['hulls']['all'], color = 'b')
            self.fittingCanvas.draw_idle()
        else:
            # Finish peak selections
            self.setPeaksB.setText("Start Manual Peak Selection")
            centerX = bioImg.info['center'][0]
            bioImg.info['tmp_peaks']['left'] = [centerX - p for p in self.function[1] if p < centerX]
            bioImg.info['tmp_peaks']['right'] = [p-centerX for p in self.function[1] if p > centerX]
            bioImg.removeInfo('peaks')  # Remove peaks info before re-processing
            self.setPeaksB.setChecked(False)
            self.processImage()

    def plotClicked(self, event):
        """
        Triggered when mouse presses on the graph in fitting tab
        """
        bioImg = self.bioImg
        if bioImg is None:
            return

        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.pixel_detail.setText("")
            ax = self.fittingAxes
            bounds = ax.get_window_extent().get_points() ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[1] - ylim[0]) / (bounds[0][1] - bounds[1][1]) ### todo
            cy = ylim[0] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])

        # Provide different behavior depending on current active function
        if self.function is None:
            self.function = ["g_move", (x, y)]
        else:
            func = self.function
            if func[0] == "g_zoomin":
                func.append((x,y))
                if len(func) == 3:
                    # Set new zoom location nad update graph
                    p1 = func[1]
                    p2 = func[2]
                    self.graph_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.graphZoomInB.setChecked(False)
                    self.refreshGraph()
            elif func[0] == "peaks":
                # Draw selected peaks
                hist = bioImg.info['hulls']["all"]
                centerX = bioImg.info['center'][0]
                selected_peaks = func[1]
                x = int(round(x))
                selected_peaks.append(x)
                ax = self.fittingAxes
                ax.axvline(x, linewidth=2, color='k')
                if x < centerX:
                    ax.text(x+5, hist[x], "left", fontsize=10)
                else:
                    ax.text(x+5, hist[x], "right", fontsize=10)
                self.fittingCanvas.draw_idle()

    def plotOnMotion(self, event):
        """
        Triggered when mouse hovers on the graph in fitting tab
        """
        bioImg = self.bioImg
        if bioImg is None:
            self.pixel_detail.setText("")
            return

        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.pixel_detail.setText("")
            ax = self.fittingAxes
            bounds = ax.get_window_extent().get_points() ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[1] - ylim[0]) / (bounds[0][1] - bounds[1][1]) ### todo
            cy = ylim[0] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])
        else:
            # Display plot information if the cursor is on the graph
            self.pixel_detail.setText("x=" + str(round(x,2)) + ', y=' + str(round(y,2)))

        if self.function is None or len(self.function) < 2:
            return

        func = self.function

        if func[0] == "g_zoomin":
            # Draw rectangle
            ax = self.fittingAxes
            del ax.patches
            ax.patches = []
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.fittingCanvas.draw_idle()
        elif func[0] == "g_move":
            # Change zoom location (x, y ranges) in order to go round plot by dragging
            if self.graph_zoom is not None:
                hist = bioImg.info['hist']
                ax = self.fittingAxes
                move = (func[1][0] - x, func[1][1] - y)
                self.graph_zoom = getNewZoom(self.graph_zoom, move, len(hist), max(hist)*1.1, self.plot_min)
                ax.set_xlim(self.graph_zoom[0])
                ax.set_ylim(self.graph_zoom[1])
                self.fittingCanvas.draw_idle()

    def plotReleased(self, event):
        """
        Triggered when mouse releases from graph
        """
        if self.function is not None and self.function[0] == "g_move":
            self.function = None

    def leavePlot(self, event):
        """
        Clear plot information when mouse leaves the graph
        """
        self.pixel_detail.setText("")

    def plotScrolled(self, event):
        """
        This function is called when a mouse scrolled on the graph in fitting tab. This will affect zoom-in and zoom-out
        """
        bioImg = self.bioImg
        if bioImg is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata

        max_size = (max(bioImg.info['hist']) * 1.1, len(bioImg.info['hist']))
        ax = self.fittingAxes

        if self.graph_zoom is None:
            self.graph_zoom = [ax.get_xlim(), ax.get_ylim()]

        zoom_height = self.graph_zoom[1][1] - self.graph_zoom[1][0]
        zoom_width = self.graph_zoom[0][1] - self.graph_zoom[0][0]

        clicked_x_percentage = 1. * (x - self.graph_zoom[0][0]) / zoom_width
        clicked_y_percentage = 1. * (y - self.graph_zoom[1][0]) / zoom_height

        step_x = .1 * zoom_width
        step_y = .1 * zoom_height
        if direction == 'up' : # zoom in
            step_x *= -1
            step_y *= -1
        zoom_width = min(max_size[1], max(zoom_width + step_x, 5))
        zoom_height = min(max_size[0], max(zoom_height + step_y, 20))

        x1 = x - clicked_x_percentage * zoom_width
        x2 = x1 + zoom_width
        y1 = y - clicked_y_percentage * zoom_height
        y2 = y1 + zoom_height

        if x1 < 0:
            x1 = 0
            x2 = zoom_width

        if y1 < self.plot_min:
            y1 = self.plot_min
            y2 = self.plot_min + zoom_height

        if x2 > max_size[1]:
            x2 = max_size[1]
            x1 = max_size[1] - zoom_width

        if y2 > max_size[0]:
            y2 = max_size[0]
            y1 = max_size[0] - zoom_height

        # Set new x,y ranges for graph
        self.graph_zoom = [(x1,x2), (y1,y2)]
        ax.set_xlim(self.graph_zoom[0])
        ax.set_ylim(self.graph_zoom[1])
        self.fittingCanvas.draw_idle()

    def showKeysHelpDialog(self):
        """
        Display help dialog
        """
        keysDialog = QMessageBox()
        keysDialog.setText('Shortcut keys')
        keysDialog.setInformativeText('Arrow Right > : Go to the next image\n'
                                      'Arrow Left < : Go to the previous image\n'
                                      'W : Increase the max intensity\n'
                                      'S : Decrease the max intensity\n'
                                      'A : Go to the left tab\n'
                                      'D : Go to the right tab\n'
                                      'N / O : Open a new image\n'
                                      'F : Process current folder\n'
                                      'M : See User Manual\n'
                                      'Q : close window\n')
        keysDialog.setStandardButtons(QMessageBox.Ok)
        keysDialog.exec_()

    def showAbout(self):
        """
        Display About Dialog
        """
        msgBox = QMessageBox()
        msgBox.setWindowTitle("About")
        msgBox.setTextFormat(Qt.RichText)
        msgBox.setText("<br><br><br>" +
                       "This Bio-Muscle is running under" +
                       "<h2>Muscle X v" +
                       self.version +
                       "</h2><br><br>" +
                       "&copy;2017 BioCAT <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("http://www.bio.aps.anl.gov/") +
                       "Wiki Page : <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://github.com/biocatiit/musclex/wiki") +
                       "Send Feedback or Issues : <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://github.com/biocatiit/musclex/issues"))
        msgBox.setStandardButtons(QMessageBox.Ok)
        msgBox.exec_()

    def launchManual(self):
        """
        Launch manual document in an internet browser
        """
        webbrowser.open_new(
            'https://docs.google.com/document/d/1G_5d4Zpk9HifMAk-D3UHMS-9fSKo7ZpnVWmAXGoaQUE/edit?usp=sharing')

    def imgScrolled(self, event):
        """
        This function is called when a mouse scrolled on the image in image tab. This will affect zoom-in and zoom-out
        """
        if self.bioImg is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img_size = self.bioImg.orig_img.shape

        if self.img_zoom is None:
            # init values of it's None
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
        Popup an input file dialog. Users can select .tif for image or .txt for failed cases list
        """
        file_name = getAFile('Images (*.tif);;Failed cases (*.txt)')
        _, ext = os.path.splitext(str(file_name))
        _, name = split(str(file_name))
        if file_name != "":
            if ext == ".txt" and not name == "failedcases.txt":
                errMsg = QMessageBox()
                errMsg.setText('Invalid Input')
                errMsg.setInformativeText("Please select only failedcases.txt or .tif image\n\n")
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
            else:
                self.mainWindow.runBioMuscle(str(file_name))

    def clearAllCache(self):
        """
        Delete all caches in a directory
        """
        cache_path = fullPath(self.dir_path, "bm_cache")
        shutil.rmtree(cache_path)
    
    def nPeakChanged(self):
        """
        Triggered when number of peaks are changed
        :return:
        """
        if self.bioImg is not None and not self.syncUI:
            self.bioImg.removeInfo("peaks")  # Remove peaks info before re-processing
            self.processImage()

    def processFolder(self):
        """
        Process current folder
        """
        ## Popup confirm dialog with settings
        nImg = len(self.imgList)
        errMsg = QMessageBox()
        errMsg.setText('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'
        settings = self.getSettings()
        text += "\nCurrent Settings"

        if settings.has_key('fixed_angle'):
            text += "\n  - Fixed Angle : " + str(settings["fixed_angle"])
        if settings.has_key('fixed_int_area'):
            text += "\n  - Fixed Box Width : " + str(settings["fixed_int_area"])

        text += "\n  - Skeletal Muscle : " + str(settings["isSkeletal"])
        text += "\n  - Number of Peaks on each side : " + str(settings["nPeaks"])
        text += "\n  - Model : " + str(settings["model"])

        for side in ['left', 'right']:
            text += "\n  - "+side+" Sigma C : " + str(settings[side+'_sigmac'])
            if side+'_fix_sigmad' in settings.keys():
                text += "\n  - "+side+" Fixed Sigma D : " + str(settings[side+'_fix_sigmad'])
            if side+'_fix_sigmas' in settings.keys():
                text += "\n  - "+side+" Fixed Sigma S : " + str(settings[side+'_fix_sigmas'])
            if side+'_fix_gamma' in settings.keys():
                text += "\n  - "+side+" Fixed Gamma : " + str(settings[side+'_fix_gamma'])
            if side+'_fix_zline' in settings.keys():
                text += "\n  - "+side+" Fixed Z line Center: " + str(settings[side+'_fix_zline'])
            if side+'_fix_intz' in settings.keys():
                text += "\n  - "+side+" Fixed Z line Intensity : " + str(settings[side+'_fix_intz'])
            if side+'_fix_sigz' in settings.keys():
                text += "\n  - "+side+" Fixed Z line Sigma : " + str(settings[side+'_fix_sigz'])
            if side+'_fix_gammaz' in settings.keys():
                text += "\n  - "+side+" Fixed Z line Gamma : " + str(settings[side+'_fix_gammaz'])


        if self.calSettings is not None:
            if self.calSettings.has_key("center"):
                text += "\n  - Calibration Center : " + str(self.calSettings["center"])
            if self.calSettings["type"] == "img":
                text += "\n  - Silver Behenate : " + str(self.calSettings["silverB"]) + " nm"
                text += "\n  - Sdd : " + str(self.calSettings["radius"]) + " pixels"
            else:
                text += "\n  - Lambda : " + str(self.calSettings["lambda"]) + " nm"
                text += "\n  - Sdd : " + str(self.calSettings["sdd"]) + " mm"
                text += "\n  - Pixel Size : " + str(self.calSettings["pixel_size"]) + " nm"

        text += '\n\nAre you sure you want to process ' + str(
            nImg) + ' image(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:

            # Display progress bar
            self.progressBar.setMaximum(nImg)
            self.progressBar.setMinimum(0)
            self.progressBar.setVisible(True)

            ## Process all images and update progress bar
            for i in range(nImg):
                self.progressBar.setValue(i)
                QApplication.processEvents()
                self.nextClicked()

        self.progressBar.setVisible(False)

    def setCalibrationImage(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or calibration.tif in the folder
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        settingDialog = CalibrationSettings(self.dir_path)
        self.calSettings = None
        cal_setting = settingDialog.calSettings
        if cal_setting is not None or force:
            result = settingDialog.exec_()
            if result == 1:
                self.calSettings = settingDialog.getValues()
                return True
        return False

    def rejectClicked(self):
        """
        Mark BioImage object as rejected. Save to cache and write data tto summary file
        """
        if self.bioImg is None or self.syncUI:
            return
        self.bioImg.info["reject"] = self.rejectChkBx.isChecked()
        self.bioImg.saveCache()
        self.csvManager.writeNewData(self.bioImg)

    def maskThresChanged(self):
        """
        Re-process and start from apply convexhull
        """
        if self.bioImg is not None and not self.syncUI:
            self.bioImg.removeInfo('hulls')
            self.processImage()

    def resetAll(self):
        """
        Remove all processing info from BioImage object and re-process with current settings
        """
        if self.bioImg is not None:
            self.bioImg.removeInfo()
            self.bioImg.delCache()
            self.processImage()

    def refreshAllFittingParams(self):
        if self.bioImg is None or self.syncUI:
            return

        self.left_fitting_tab.hideGamma(str(self.modelSelect.currentText()) != 'Voigt')
        self.right_fitting_tab.hideGamma(str(self.modelSelect.currentText()) != 'Voigt')

        if self.bioImg is not None:
            self.bioImg.removeInfo('fit_results')
            self.refreshFittingParams('left')
            self.refreshFittingParams('right')
        self.processImage()

    def refreshFittingParams(self, side):
        if self.bioImg is not None:
            self.bioImg.removeInfo(side + '_fix_sigmad')
            self.bioImg.removeInfo(side + '_fix_sigmas')
            self.bioImg.removeInfo(side + '_fix_gamma')
            self.bioImg.removeInfo(side + '_fix_sigz')
            self.bioImg.removeInfo(side + '_fix_intz')
            self.bioImg.removeInfo(side + '_fix_zline')
            self.bioImg.removeInfo(side + '_fix_gammaz')

    def prevClicked(self):
        """
        Going to the previous image
        """
        self.currentImg = (self.currentImg - 1) % len(self.imgList)
        self.onImageChanged()

    def nextClicked(self):
        """
        Going to the next image
        """
        self.currentImg = (self.currentImg + 1) % len(self.imgList)
        self.onImageChanged()

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
        elif key == Qt.Key_N:
            self.browseFile()
        elif key == Qt.Key_F:
            self.processFolder()
        elif key == Qt.Key_O:
            self.browseFile()
        elif key == Qt.Key_M:
            self.launchManual()

    def calibrationClicked(self):
        """
        Triggered when calibration settings button pressed
        """
        success = self.setCalibrationImage(force=True)
        if self.bioImg is not None and success:
            self.bioImg.removeInfo()
            self.processImage()

    def setAngleAndCenterClicked(self):
        """
        Prepare for manual rotation angle and center setting
        """
        if self.bioImg is None:
            return

        if self.setRotAndCentB.isChecked():
            self.setLeftStatus("Click on 2 corresponding reflection peaks along the equator (ESC to cancel)")
            ax = self.displayImgAxes
            # ax2 = self.displayImgFigure.add_subplot(4, 4, 13)
            # ax2.imshow(getBGR(get8bitImage(self.bioImg.getRotatedImage(), self.minIntSpnBx.value(), self.maxIntSpnBx.value())))
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            self.displayImgCanvas.draw_idle()
            self.function = ["angle_center"]  # set current active function
        else:
            self.resetUI()

    def setAngleClicked(self):
        """
        Prepare for manual rotation angle setting
        """
        if self.bioImg is None:
            return

        if self.setAngleB.isChecked():
            self.setLeftStatus("Click on image to select the angle of the equator (ESC to cancel)")
            ax = self.displayImgAxes
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            self.displayImgCanvas.draw_idle()
            self.function = ["angle"]  # set current active function
        else:
            self.resetUI()

    def setRminClicked(self):
        """
        Prepare for manual R-min setting
        """
        if self.setRminB.isChecked():
            self.setLeftStatus("Please select R-min size (ESC to cancel)")
            ax = self.displayImgAxes
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            self.displayImgCanvas.draw_idle()
            self.function = ["rmin"]  # set current active function
        else:
            self.resetUI()

    def setIntAreaClicked(self):
        """
        Prepare for manual integrated area (Box width) setting
        """
        if self.setIntAreaB.isChecked():
            self.setLeftStatus("Please select Integrated area by select start line and end line (ESC to cancel)")
            ax = self.displayImgAxes
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            self.displayImgCanvas.draw_idle()
            self.function = ["int_area"]  # set current active function
        else:
            self.resetUI()

    def fixedAngleChecked(self):
        """
        Triggered when fixed angle is checked or unchecked
        """
        self.fixedAngle.setEnabled(self.fixedAngleChkBx.isChecked())
        if not self.fixedAngleChkBx.isChecked() and self.bioImg is not None:
            self.bioImg.removeInfo("fixed_angle")

    def fixedIntAreaChecked(self):
        """
        Triggered when fixed integrated area is checked or unchecked
        """
        if self.bioImg is not None:
            if not self.fixedIntAreaChkBx.isChecked():
                self.fixedIntArea = None
                self.bioImg.removeInfo("fixed_int_area")
            else:
                self.fixedIntArea = self.bioImg.info['int_area']

    def fixedAngleChanged(self):
        """
        Triggered when fixed angle spinbox value is changed
        """
        if self.bioImg is not None and not self.syncUI:
            self.bioImg.removeInfo("rotationAngle")
            self.processImage()

    def imgClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if self.bioImg is None:
            return

        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.pixel_detail.setText("")
            ax = self.displayImgAxes
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
            self.function = ["im_move", (x, y)]
        elif func[0] == "angle_center":
            # draw X at points and a line between points
            ax = self.displayImgAxes
            axis_size = 5
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
                # new_angle += 90.

                cx = int(round((x1 + x2) / 2.))
                cy = int(round((y1 + y2) / 2.))
                M = cv2.getRotationMatrix2D(tuple(self.bioImg.info['center']), self.bioImg.info['rotationAngle'], 1)
                invM = cv2.invertAffineTransform(M)
                homo_coords = [cx, cy, 1.]
                new_center = np.dot(invM, homo_coords)
                # Set new center and rotaion angle , re-calculate R-min
                self.bioImg.info['center'] = (int(round(new_center[0])), int(round(new_center[1])))
                self.bioImg.info['rotationAngle'] = self.bioImg.info['rotationAngle'] + new_angle
                self.bioImg.removeInfo('rmin')
                self.setRotAndCentB.setChecked(False)
                self.processImage()
        elif func[0] == "angle":
            center = self.bioImg.info['center']
            x1 = center[0]
            y1 = center[1]
            if abs(x - x1) == 0:
                new_angle = -90
            else:
                new_angle = -180. * np.arctan((y1 - y) / (x1 - x)) / np.pi

            # Set new rotaion angle , re-calculate from R-min calculation process
            self.bioImg.info['rotationAngle'] = self.bioImg.info['rotationAngle'] - new_angle
            self.bioImg.removeInfo('rmin')
            self.setAngleB.setChecked(False)
            self.processImage()

        elif func[0] == "int_area":
            # draw 2 horizontal lines
            func.append(y)
            ax = self.displayImgAxes
            ax.axhline(y=y, color='y')
            self.displayImgCanvas.draw_idle()
            if len(func) == 3:
                int_area = [int(round(func[1])), int(round(func[2]))]
                t = min(int_area)
                b = max(int_area)
                # Set new integrated area, re-calculate from getting histogram process
                self.bioImg.info['int_area'] = (t, b)
                self.bioImg.removeInfo('hist')
                if self.fixedIntAreaChkBx.isChecked():
                    self.fixedIntArea = (t, b)
                self.function = None
                self.setIntAreaB.setChecked(False)
                self.processImage()
        elif func[0] == "rmin":
            # Set new R-min, re-calculate from getting integrated area process
            self.bioImg.info['rmin'] = int(np.round(distance(self.bioImg.info['center'], (x, y))))
            self.bioImg.removeInfo('int_area')
            self.function = None
            self.setRminB.setChecked(False)
            self.processImage()
        else:
            if func[0] == "im_zoomin":
                func.append((x, y))
                if len(func) == 3:
                    p1 = func[1]
                    p2 = func[2]
                    # Set zoom-in location ( x,y, ranges) and update image tab
                    self.img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.imgZoomInB.setChecked(False)
                    self.updateImage()

    def imgOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """
        if self.bioImg is None or 'rotationAngle' not in self.bioImg.info.keys():
            return

        x = event.xdata
        y = event.ydata

        img = self.bioImg.getRotatedImage()

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < img.shape[1] and y < img.shape[0]:
                self.pixel_detail.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]))

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.pixel_detail.setText("")
            ax = self.displayImgAxes
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
            ax = self.displayImgAxes
            ax.patches.pop()
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.displayImgCanvas.draw_idle()

        elif func[0] == "angle":
            # draw line as angle
            center = self.bioImg.info["center"]
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            ax = self.displayImgAxes
            ax.lines = []
            ax.plot([x, x2], [y, y2], color="g")
            self.displayImgCanvas.draw_idle()

        elif func[0] == "int_area":
            # draw horizontal lines
            ax = self.displayImgAxes
            if len(ax.lines) > len(func) - 1:
                line = ax.lines[:len(func) - 1]
                ax.lines = line
            ax.axhline(y, color='g')
            self.displayImgCanvas.draw_idle()
        elif func[0] == "rmin":
            # draw R-min circle
            center = self.bioImg.info['center']
            dis = int(np.round(distance(center, (x, y))))
            ax = self.displayImgAxes
            ax.patches = []
            ax.add_patch(
                patches.Circle(tuple(center), dis, linewidth=2, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.displayImgCanvas.draw_idle()
        elif func[0] == "angle_center":
            # draw X on points and a line between points
            ax = self.displayImgAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
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

            # bound = 10
            # ax2.set_xlim((x-bound, x+bound))
            # ax2.set_ylim((y-bound, y+bound))
            # del ax2.lines
            # ax2.lines = []
            # ax2.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            # ax2.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            self.displayImgCanvas.draw_idle()
            # self.displayImgCanvas.flush_events()
        elif func[0] == "im_move":
            # change zoom-in location (x,y ranges) to move around image
            if self.img_zoom is not None:
                ax = self.displayImgAxes
                move = (func[1][0] - x, func[1][1] - y)
                self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[1], img.shape[0])
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
                ax.invert_yaxis()
                self.displayImgCanvas.draw_idle()

    def imgReleased(self, event):
        """
        Triggered when mouse released from image
        """
        if self.function is not None and self.function[0] == "im_move":
            self.function = None

    def leaveImage(self, event):
        """
        Set pixel information is empty if mouse leaves figure
        """
        self.pixel_detail.setText("")

    def imgZoomIn(self):
        """
        Triggered when Zoom in image is pressed
        """
        if self.imgZoomInB.isChecked():
            self.setLeftStatus("Please select zoom-in area by clicking 2 points to make a rectangle (ESC to cancel)")
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

    def closeEvent(self, ev):
        """
        Trigger when window is closed
        """
        # delete window object from main window
        self.mainWindow.childWindowClosed(self)

    def initWidgets(self, info):
        """
        Update GUI to sync with BioImage info
        :param info: BioImage info
        """
        self.syncUI = True

        # Initial UI for each fitting tab : left, right
        self.left_fitting_tab.initSpinBoxes(info)
        self.right_fitting_tab.initSpinBoxes(info)

        # Initial UI for general settings
        if info.has_key('isSkeletal'):
            self.skeletalChkBx.setChecked(info['isSkeletal'])

        self.maskThresSpnBx.setValue(info['mask_thres'])

        if info.has_key('fit_results'):
            fit_results = info['fit_results']
            self.nPeakSpnBx.setValue(len(fit_results['left_areas']))
            self.modelSelect.setCurrentIndex(self.modelSelect.findText(fit_results["model"]))

        # self.fixedAngleChkBx.setChecked(info.has_key('fixed_angle'))
        # if info.has_key('fixed_angle'):
        #     self.fixedAngle.setValue(info['fixed_angle'])
        #
        # self.fixedIntAreaChkBx.setChecked(info.has_key('fixed_int_area'))
        # if info.has_key('fixed_int_area'):
        #     self.fixedIntArea = info['fixed_int_area']

        # Initital reject check box
        if "reject" in info.keys():
            self.rejectChkBx.setChecked(self.bioImg.info["reject"])
        else:
            self.rejectChkBx.setChecked(False)

        self.syncUI = False

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new BioImage object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        self.bioImg = BioImage(self.dir_path, self.imgList[self.currentImg])
        self.initWidgets(self.bioImg.info)
        self.initMinMaxIntensities(self.bioImg)
        self.img_zoom = None
        self.refreshStatusbar()

        # Process new image
        self.processImage()

    def processImage(self):
        """
        Process Image by getting all settings and call process() of BioImage object
        Then, write data and update UI
        """
        if self.bioImg is None:
            return
        QApplication.setOverrideCursor(Qt.WaitCursor)
        QApplication.processEvents()
        settings = self.getSettings()
        try:
            self.bioImg.process(settings)
        except Exception, e:
            QApplication.restoreOverrideCursor()
            errMsg = QMessageBox()
            errMsg.setText('Unexpected error')
            msg = 'Please report the problem with error message below and the input image (.tif)\n\n'
            msg += "Error : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
            errMsg.setInformativeText(msg)
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.setFixedWidth(300)
            errMsg.exec_()
            raise

        self.csvManager.writeNewData(self.bioImg)
        self.resetUI()
        self.refreshStatusbar()
        QApplication.restoreOverrideCursor()

    def setLeftStatus(self, s):
        """
        Set text on status bar on the left
        :param s: input text (str)
        """
        self.left_status.setText(s)
        QApplication.processEvents()

    def refreshStatusbar(self):
        """
        Set Left status bar to be image detail
        Set Right status bar to by image shape and type
        Clear pixel detail
        """
        if self.bioImg is None:
            return
        self.setLeftStatus(
            "(" + str(self.currentImg + 1) + "/" + str(len(self.imgList)) + ") " + fullPath(self.dir_path,
                                                                                            self.bioImg.filename))
        img = self.bioImg.orig_img
        self.right_status.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype))
        self.pixel_detail.setText("")
        QApplication.processEvents()

    def getSettings(self):
        """
        Get all settings for BioImage process() from widgets
        :return: settings (dict)
        """
        settings = {}
        settings.update(self.left_fitting_tab.getFittingSettings())
        settings.update(self.right_fitting_tab.getFittingSettings())

        settings['nPeaks'] = self.nPeakSpnBx.value()
        settings['model'] = str(self.modelSelect.currentText())
        settings['isSkeletal'] = self.skeletalChkBx.isChecked()
        settings['mask_thres'] = self.maskThresSpnBx.value()

        if self.calSettings is not None:
            if self.calSettings["type"] == "img":
                settings["center"] = self.calSettings["center"]
                settings["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
            else:
                settings["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings[
                    "pixel_size"]
                if self.calSettings.has_key("center"):
                    settings["center"] = self.calSettings["center"]

        if self.fixedAngleChkBx.isChecked():
            settings["fixed_angle"] = self.fixedAngle.value()

        if self.fixedIntAreaChkBx.isChecked() and self.fixedIntArea is not None:
            settings["fixed_int_area"] = self.fixedIntArea

        return settings

    def initMinMaxIntensities(self, bioImg):
        """
        Set preference for image min & max intesity spinboxes, and initial their value
        :param bioImg: current BioImage object
        :return:
        """
        img = bioImg.orig_img
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

        if img.shape == (1043, 981):
            self.minIntSpnBx.setDecimals(2)
            self.maxIntSpnBx.setDecimals(2)
        else:
            self.minIntSpnBx.setDecimals(0)
            self.maxIntSpnBx.setDecimals(0)

        # use cached values if they're available
        if "minInt" in self.bioImg.info and "maxInt" in self.bioImg.info:
            self.minIntSpnBx.setValue(self.bioImg.info["minInt"])
            self.maxIntSpnBx.setValue(self.bioImg.info["maxInt"])
        else:
            self.minIntSpnBx.setValue(img.min())  # init min intensity as min value
            self.maxIntSpnBx.setValue(img.max() * 0.20)  # init max intensity as 20% of max value
        self.syncUI = False

    def refreshGraph(self):
        self.update_plot['graph'] = True
        self.updateUI()

    def resetUI(self):
        """
        Refresh all tabs
        """
        self.function = None
        self.graph_zoom = None
        QApplication.restoreOverrideCursor()
        for b in self.checkableButtons:
            b.setChecked(False)
        for k in self.update_plot.keys():
            self.update_plot[k] = True

        # change seleck peak button's text
        self.setPeaksB.setText("Start Manual Peak Selection")

        self.updateUI()

    def intensityChanged(self):
        """
        Triggered when min or max intensity value is changed from spinbox
        """
        if self.bioImg is None or self.syncUI:
            return
        self.bioImg.info["minInt"] = self.minIntSpnBx.value()
        self.bioImg.info["maxInt"] = self.maxIntSpnBx.value()
        self.bioImg.saveCache()
        self.updateImage()

    def updateImage(self):
        """
        Refresh image tab
        """
        self.update_plot['img'] = True
        self.updateUI()

    def syncSpinBoxes(self):
        """
        Update Spinboxes values by using current BioImage info (after processing)
        """
        self.syncUI = True
        info = self.bioImg.info
        self.left_fitting_tab.syncSpinBoxes(info)
        self.right_fitting_tab.syncSpinBoxes(info)
        self.fixedAngle.setValue(info["rotationAngle"])
        self.syncUI = False

    def updateUI(self):
        """
        Update current all widget in current tab , spinboxes, and refresh status bar
        """
        if self.bioImg is None:
            return

        self.function = None
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            self.updateImageTab()
        elif selected_tab == 1:
            self.updateFittingTab()
        elif selected_tab == 2:
            self.updateResultsTab()

        self.syncSpinBoxes()
        self.refreshStatusbar()

        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

        QApplication.processEvents()

    def updateImageTab(self):
        """
        Draw all UI in image tab
        """
        info = copy.copy(self.bioImg.info)
        img = self.bioImg.getRotatedImage()
        disp_img = getBGR(get8bitImage(img, self.minIntSpnBx.value(), self.maxIntSpnBx.value()))
        hulls = info['hulls']['all']
        center = info['center']
        rmin = info['rmin']
        int_area = info['int_area']
        ax = self.displayImgAxes
        ax.cla()
        ax.imshow(disp_img)  # Display selected image

        if self.centerChkBx.isChecked():
            # Draw center
            ax.plot([center[0]], [center[1]], 'bo')

        if self.rminChkBx.isChecked():
            # Draw R-min
            ax.add_patch(
                patches.Circle(tuple(center), rmin, linewidth=2, edgecolor='r', facecolor='none', linestyle='dotted'))

        if self.intChkBx.isChecked():
            # Draw Integrated area
            ax.plot([0, img.shape[1]], [int_area[0], int_area[0]], color='g')
            ax.plot([0, img.shape[1]], [int_area[1], int_area[1]], color='g')

        if self.imgPeakChkBx.isChecked():
            # Draw peaks line
            if 'fit_results' in info.keys():
                for p in info['fit_results']['model_peaks']:
                    ax.axvline(p, color='y', alpha=0.3)

        if self.histChkBx.isChecked():
            # Draw background subtracted histogram
            norm = float(disp_img.shape[0] - center[1]) * .8 / max(hulls)
            hulls = np.array([disp_img.shape[0] - p * norm for p in hulls])
            ax.fill(hulls, facecolor='white')

            xs = np.linspace(0, len(hulls), len(hulls))
            if 'fit_results' in info.keys():
                cardiac = (np.array(getCardiacGraph(xs, info['fit_results'])) * norm - disp_img.shape[0]) * -1
                ax.plot(cardiac, color='r')


        # Zoom
        if self.img_zoom is not None and len(self.img_zoom) == 2:
            ax.set_xlim(self.img_zoom[0])
            ax.set_ylim(self.img_zoom[1])
        else:
            ax.set_xlim((0, img.shape[1]))
            ax.set_ylim((0, img.shape[0]))

        self.img_zoom = [ax.get_xlim(), ax.get_ylim()]
        ax.invert_yaxis()
        self.displayImgFigure.tight_layout()
        self.displayImgCanvas.draw()

    def updateFittingTab(self):
        """
        Draw all UI in fitting tab
        """
        if not self.update_plot['graph']:
            return

        info = copy.copy(self.bioImg.info)
        hist = info['hist']
        hull = info['hulls']['all']
        hull = np.array(hull)

        ax = self.fittingAxes
        ax.cla()

        if self.origHistChkBx.isChecked():
            # Draw original histogram
            ax.plot(hist, color = 'k')
        if self.hullChkBx.isChecked():
            # Draw background subtracted histogram
            ax.plot(hull, color = 'g')

        if 'fit_results' in info:
            fit_result = info['fit_results']

            if fit_result['isSkeletal'] and self.dispZlineChkBx.isChecked():
                # Draw z line
                ax.axvline(fit_result['centerX'] + fit_result['right_zline'], color='y', alpha=0.5)
                ax.axvline(fit_result['centerX'] - fit_result['left_zline'], color='y', alpha=0.5)

            if self.centerXChkBx.isChecked():
                ax.axvline(fit_result['centerX'], color='m', alpha=0.5)

            # draw fitting model
            if self.fitChkBx.isChecked():
                x = np.linspace(0, len(hull), len(hull))
                ax.plot(getCardiacGraph(x, fit_result), color = 'b')

            if fit_result.has_key('model_peaks') and self.peakChkBx.isChecked():
                # Draw peak lines
                peaks = fit_result['model_peaks']
                for p in peaks:
                    ax.axvline(p, color='r', alpha=0.3)

        # Zoom
        if self.graph_zoom is not None and len(self.graph_zoom) == 2:
            ax.set_xlim(self.graph_zoom[0])
            ax.set_ylim(self.graph_zoom[1])
        else:
            self.plot_min = ax.get_ylim()[0]
            ax.set_xlim(0, len(hull))

        self.fittingFigure.tight_layout()
        self.fittingCanvas.draw()
        self.update_plot['fit'] = False


    def updateResultsTab(self):
        """
        Update values in the table in results tab
        """
        info = copy.copy(self.bioImg.info)

        genResults = "<h2>General Information</h2>"

        if "reject" in info and info["reject"]:
            genResults += "<b>THIS IMAGE IS REJECTED</b><br/><br/>"

        if "fit_results" in info:
            fit_results = info['fit_results']
            genResults += "<b>Model : </b>" + str(fit_results["model"]) + '<br/><br/>'
            genResults += "<b>Skeletal Muscle : </b>" + str(fit_results["isSkeletal"]) + '<br/><br/>'
            genResults += "<b>CenterX : </b>" + str(fit_results["centerX"]) + '<br/><br/>'
            genResults += "<b>S10 : </b>" + str(fit_results["S10"]) + '<br/><br/>'
            if 'd10' in fit_results.keys():
                genResults += "<b>d10 : </b>" + str(fit_results["d10"]) + '<br/><br/>'
            genResults += "<b>Average I11/I10 per fiber : </b>" + str(fit_results["avg_ratio"]) + '<br/><br/>'
            genResults += "<b>Fitting Error : </b>" + str(fit_results["fiterror"])
            if fit_results['fiterror'] > 0.2:
                genResults += " <b>(High Error)</b>"
        else:
            genResults +=  "<b>Model cannot be fit</b>"


        if self.calSettings is not None:
            genResults += "<h2>Calibration Settings</h2>"
            if self.calSettings.has_key("center"):
                genResults += "<b>Calibration Center : </b>" + str(self.calSettings["center"]) + '<br/><br/>'

            if self.calSettings["type"] == "img":
                genResults += "<b>S<sub>dd</sub> (in pixel) : </b>" + str(self.calSettings["radius"]) + '<br/><br/>'
                genResults += "<b>Silver Behenate : </b>" + str(self.calSettings["silverB"]) + '<br/><br/>'
            else:
                genResults += "<b>Lambda : </b>" + str(self.calSettings["lambda"]) + '<br/><br/>'
                genResults += "<b>S<sub>dd</sub> : </b>" + str(self.calSettings["sdd"]) + '<br/><br/>'
                genResults += "<b>Pixel Size : </b>" + str(self.calSettings["pixel_size"]) + '<br/><br/>'

        if len(genResults) > 0:
            self.generalResults.setText(genResults)

        self.fiberResultTable.setRowCount(200)
        ind = 0

        # Display all peaks found in table
        if 'tmp_peaks' in info:
            self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("All Peaks Found"))
            self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(info['tmp_peaks']['left'])))
            self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(info['tmp_peaks']['right'])))
            ind += 1
        if 'peaks' in info:
            self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Initial Peaks"))
            self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(info['peaks']['left'])))
            self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(info['peaks']['right'])))
            ind += 1

        if 'fit_results' in info:
            fit_results = info['fit_results']

            # Display all areas in table
            for i in range(len(fit_results['left_areas'])):
                self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Areas " + str(i + 1)))
                self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_areas'][i])))
                self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_areas'][i])))
                ind += 1

            # Display ratio I11/I10 in table
            self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("I11/I10"))
            self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_ratio'])))
            self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_ratio'])))
            ind += 1

            # Display Sigma C in table
            self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Sigma C"))
            self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_sigmac'])))
            self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_sigmac'])))
            ind += 1

            # Display Sigma D in table
            self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Sigma D"))
            self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_sigmad'])))
            self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_sigmad'])))
            ind += 1

            # Display Sigma S in table
            self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Sigma S"))
            self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_sigmas'])))
            self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_sigmas'])))
            ind += 1

            if fit_results['model'] == 'Voigt':
                # Display gamma in table if model is Voigt
                self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Gamma"))
                self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_gamma'])))
                self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_gamma'])))
                ind += 1

            # Display all skeletal parameters in table
            if fit_results['isSkeletal']:

                # Display Z line center in table
                self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Z line"))
                self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_zline'])))
                self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_zline'])))
                ind += 1

                # Display Z line sigma in table
                self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Sigma Z"))
                self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_sigmaz'])))
                self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_sigmaz'])))
                ind += 1

                # Display Z line intensity in table
                self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Iz"))
                self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_intz'])))
                self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_intz'])))
                ind += 1


                # Display gamma in table if model is Voigt
                self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("gamma"))
                self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_gamma'])))
                self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_gamma'])))
                ind += 1

                if fit_results['model'] == 'Voigt':

                    # Display gamma in table if model is Voigt
                    self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Gamma Z"))
                    self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_gammaz'])))
                    self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_gammaz'])))
                    ind += 1

        self.fiberResultTable.setRowCount(ind)
        QApplication.processEvents()