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
import os, shutil
from .pyqt_utils import *
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
from os.path import split
import traceback
import webbrowser
from ..CalibrationSettings import CalibrationSettings
from ..utils.file_manager import fullPath, getImgFiles, getStyleSheet, getBlankImageAndMask
from ..modules.EquatorImage import EquatorImage, getCardiacGraph
from ..utils.image_processor import *
from ..csv_manager import EQ_CVSManager, EQ_CSVManager2
from ..ui.EQ_FittingTab import EQ_FittingTab
import musclex
from .BlankImageSettings import BlankImageSettings
from ..utils import logger

class EquatorWindow(QMainWindow):
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
        self.logger = None
        self.editableVars = {}
        self.bioImg = None  # Current EquatorImage object
        self.img_zoom = None  # Params for x and y ranges of displayed image in image tab
        self.graph_zoom = None # Params for x and y ranges of displayed graph in fitting tab
        self.function = None  # Current active function
        self.syncUI = False  # boolean status for UI sync. Prevent recursive infinite processing
        self.update_plot = {'img': True, 'graph' :True, 'results': True}  # update status of each tab
        self.in_batch_process = False
        self.fixedIntArea = None
        self.orientationModel = None
        self.modeOrientation = None
        self.dir_path, self.imgList, self.currentImg = getImgFiles(str(filename))
        if len(self.imgList) == 0:
            self.inputerror()
            return
        self.csvManager = EQ_CVSManager(self.dir_path)  # Create a CSV Manager object
        self.csvManager2 = EQ_CSVManager2(self.dir_path)
        self.setWindowTitle("Muscle X Equator v." + self.version)
        # self.setStyleSheet(getStyleSheet())
        self.initUI()  # Initial all UI
        self.setAllToolTips()  # Set tooltips for widgets
        self.setConnections()  # Set interaction for widgets
        self.setCalibrationImage()
        self.onImageChanged() # Toggle window to process current image
        self.show()
        #self.resize(1000, 100)
        self.init_logging()
        # focused_widget = QApplication.focusWidget()
        # if focused_widget != None:
        #     focused_widget.clearFocus()

    def inputerror(self):
        # Display input error to screen
        errMsg = QMessageBox()
        errMsg.setText('Invalid Input')
        errMsg.setInformativeText("Please select non empty failedcases.txt or an image\n\n")
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
        self.displayImgFigure = plt.figure()
        self.displayImgAxes = self.displayImgFigure.add_subplot(111)
        self.imageVLayout = QVBoxLayout()
        self.displayImgCanvas = FigureCanvas(self.displayImgFigure)
        self.imageVLayout.addWidget(self.displayImgCanvas)

        self.imgDispOptionGrp = QGroupBox('Display Options')
        self.imgDispOptLayout = QGridLayout()
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
        self.minIntSpnBx = QDoubleSpinBox()
        self.minIntSpnBx.setObjectName('minIntSpnBx')
        self.editableVars[self.minIntSpnBx.objectName()] = None
        self.minIntSpnBx.setKeyboardTracking(False)
        self.maxIntSpnBx = QDoubleSpinBox()
        self.maxIntSpnBx.setObjectName('maxIntSpnBx')
        self.editableVars[self.maxIntSpnBx.objectName()] = None
        self.maxIntSpnBx.setKeyboardTracking(False)
        self.logScaleIntChkBx = QCheckBox("Log scale intensity")
        self.persistMaxIntensity = QCheckBox("Persist Max intensity")
        self.imgZoomInB = QPushButton('Zoom In')
        self.imgZoomInB.setCheckable(True)
        self.checkableButtons.append(self.imgZoomInB)
        self.imgZoomOutB = QPushButton('Full')
        self.checkableButtons.append(self.imgZoomOutB)
        self.imgDispOptLayout.addWidget(self.centerChkBx,1,0,1,2)
        self.imgDispOptLayout.addWidget(self.intChkBx,1,2,1,2)
        self.imgDispOptLayout.addWidget(self.rminChkBx,2,0,1,2)
        self.imgDispOptLayout.addWidget(self.histChkBx,2,2,1,2)
        self.imgDispOptLayout.addWidget(self.imgPeakChkBx,3,0,1,2)
        self.imgDispOptLayout.addWidget(self.minIntLabel,4,0,1,2)
        self.imgDispOptLayout.addWidget(self.maxIntLabel,4,2,1,2)
        self.imgDispOptLayout.addWidget(self.minIntSpnBx, 5, 0, 1, 2)
        self.imgDispOptLayout.addWidget(self.maxIntSpnBx, 5, 2, 1, 2)
        self.imgDispOptLayout.addWidget(self.logScaleIntChkBx,6,0,1,2)
        self.imgDispOptLayout.addWidget(self.persistMaxIntensity,6,2,1,2)
        self.imgDispOptLayout.addWidget(self.imgZoomInB,7,0,1,2)
        self.imgDispOptLayout.addWidget(self.imgZoomOutB,7,2,1,2)
        self.imgDispOptionGrp.setLayout(self.imgDispOptLayout)

        self.imgProcGrp = QGroupBox("Image Processing")
        self.imgProcLayout = QGridLayout()
        self.imgProcGrp.setLayout(self.imgProcLayout)
        self.calibrationB = QPushButton("Calibration Settings")
        self.calibSettingDialog = None
        self.setRotAndCentB = QPushButton("Set Rotation Angle \nand Center")
        self.setRotAndCentB.setCheckable(True)
        self.setRotAndCentB.setFixedHeight(45)
        self.setAngleB = QPushButton("Set \nRotation Angle")
        self.setAngleB.setCheckable(True)
        self.setAngleB.setFixedHeight(45)
        self.setRminB = QPushButton("Set R-min")
        self.setRminB.setCheckable(True)
        self.setRminB.setFixedHeight(45)
        self.setIntAreaB = QPushButton("Set Box Width")
        self.setIntAreaB.setCheckable(True)
        self.setIntAreaB.setFixedHeight(45)
        self.checkableButtons.extend([self.setRotAndCentB, self.setIntAreaB, self.setRminB, self.setAngleB])
        self.fixedAngleChkBx = QCheckBox("Fixed Angle")
        self.fixedAngleChkBx.setChecked(False)
        self.fixedRminChkBx = QCheckBox("Fixed R-min")
        self.fixedRminChkBx.setChecked(False)
        self.fixedIntAreaChkBx = QCheckBox("Fixed Box Width")
        self.fixedIntAreaChkBx.setChecked(False)
        self.modeAngleChkBx = QCheckBox("Mode orientation")
        self.modeAngleChkBx.setChecked(False)
        self.fixedAngle = QSpinBox()
        self.fixedAngle.setObjectName('fixedAngle')
        self.editableVars[self.fixedAngle.objectName()] = None
        self.fixedAngle.setKeyboardTracking(False)
        self.fixedAngle.setRange(-360, 360)
        self.fixedAngle.setEnabled(False)
        self.fixedRmin = QSpinBox()
        self.fixedRmin.setObjectName('fixedRmin')
        self.editableVars[self.fixedRmin.objectName()] = None
        self.fixedRmin.setKeyboardTracking(False)
        self.fixedRmin.setRange(1, 1000)
        self.fixedRmin.setEnabled(False)
        self.applyBlank = QCheckBox("Apply Blank Image and Mask")
        self.applyBlank.setChecked(False)
        self.blankSettings = QPushButton("Set")
        self.blankSettings.setEnabled(False)
        self.maskThresSpnBx = QDoubleSpinBox()
        self.maskThresSpnBx.setObjectName('maskThresSpnBx')
        self.editableVars[self.maskThresSpnBx.objectName()] = None
        self.maskThresSpnBx.setRange(-10,10)
        self.maskThresSpnBx.setKeyboardTracking(False)
        self.orientationCmbBx = QComboBox()
        self.orientationCmbBx.addItem("Max Intensity")
        self.orientationCmbBx.addItem("GMM")
        self.orientationCmbBx.addItem("Herman Factor (Half Pi)")
        self.orientationCmbBx.addItem("Herman Factor (Pi)")
        self.rotation90ChkBx = QCheckBox("Rotate 90")
        self.forceRot90ChkBx = QCheckBox("Persist Rotation")

        self.resetAllB = QPushButton("Reset All")
        self.imgProcLayout.addWidget(self.calibrationB, 0, 0, 1, 4)
        self.imgProcLayout.addWidget(self.setRotAndCentB, 1, 0, 1, 2)
        self.imgProcLayout.addWidget(self.setAngleB, 1, 2, 1, 2)
        self.imgProcLayout.addWidget(self.setRminB, 2, 0, 1, 2)
        self.imgProcLayout.addWidget(self.setIntAreaB, 2, 2, 1, 2)
        self.imgProcLayout.addWidget(self.applyBlank, 3, 0, 1, 3)
        self.imgProcLayout.addWidget(self.blankSettings, 3, 3, 1, 1)
        self.imgProcLayout.addWidget(QLabel("Mask Threshold"), 4, 0, 1, 2)
        self.imgProcLayout.addWidget(self.maskThresSpnBx, 4, 2, 1, 2)
        self.imgProcLayout.addWidget(self.fixedAngleChkBx, 5, 0, 1, 2)
        self.imgProcLayout.addWidget(self.fixedAngle, 5, 2, 1, 2)
        self.imgProcLayout.addWidget(self.fixedRminChkBx, 6, 0, 1, 2)
        self.imgProcLayout.addWidget(self.fixedRmin, 6, 2, 1, 2)
        self.imgProcLayout.addWidget(self.fixedIntAreaChkBx, 7, 0, 1, 4)
        self.imgProcLayout.addWidget(self.modeAngleChkBx, 7, 2, 1, 2)
        self.imgProcLayout.addWidget(QLabel("Orientation Finding: "), 8, 0, 1, 4)
        self.imgProcLayout.addWidget(self.orientationCmbBx, 9, 0, 1, 4)
        self.imgProcLayout.addWidget(self.rotation90ChkBx, 10, 0, 1, 2)
        self.imgProcLayout.addWidget(self.forceRot90ChkBx, 10, 2, 1, 2)

        self.imgProcLayout.addWidget(self.resetAllB, 11, 0, 1, 4)

        self.rejectChkBx = QCheckBox("Reject")
        self.rejectChkBx.setFixedWidth(100)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Reprocess and Refit current folder")
        self.processFolderButton.setStyleSheet(pfss)
        self.processFolderButton.setCheckable(True)
        self.bottomLayout = QGridLayout()
        self.prevButton = QPushButton("<<<")
        self.nextButton = QPushButton(">>>")
        self.filenameLineEdit = QLineEdit()
        self.bottomLayout.addWidget(self.rejectChkBx, 0, 0, 1, 2)
        self.bottomLayout.addWidget(self.processFolderButton, 1, 0, 1, 2)
        self.bottomLayout.addWidget(self.prevButton, 2, 0, 1, 1)
        self.bottomLayout.addWidget(self.nextButton, 2, 1, 1, 1)
        self.bottomLayout.addWidget(self.filenameLineEdit, 3, 0, 1, 2)
        self.bottomLayout.setAlignment(self.rejectChkBx, Qt.AlignLeft)

        self.imageOptionsFrame = QFrame()
        self.imageOptionsFrame.setFixedWidth(500)
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
        self.fittingTabLayout = QGridLayout(self.fittingTab)

        # Plot
        self.fittingFigure = plt.figure()
        self.fittingAxes = self.fittingFigure.add_subplot(111)
        self.fittingVLayout = QVBoxLayout()
        self.fittingCanvas = FigureCanvas(self.fittingFigure)

        self.generalGrp = QGroupBox("General Settings")
        self.genLayout = QGridLayout(self.generalGrp)
        self.skeletalChkBx = QCheckBox("Skeletal Muscle (Z line)")
        self.skeletalChkBx.setFixedWidth(200)
        self.nPeakSpnBx = QSpinBox()
        self.nPeakSpnBx.setObjectName('nPeakSpnBx')
        self.editableVars[self.nPeakSpnBx.objectName()] = None
        self.nPeakSpnBx.setKeyboardTracking(False)
        self.nPeakSpnBx.setMinimum(2)
        self.nPeakSpnBx.setMaximum(40)
        self.nPeakSpnBx.setSingleStep(1)
        self.nPeakSpnBx.setValue(2)
        self.modelSelect = QComboBox()
        self.modelSelect.addItem("Gaussian")
        self.modelSelect.addItem("Voigt")
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

        self.left_fitting_tab = EQ_FittingTab(self, "left")
        self.right_fitting_tab = EQ_FittingTab(self, "right")
        self.fittingTabWidget.addTab(self.left_fitting_tab, "Left")
        self.fittingTabWidget.addTab(self.right_fitting_tab, "Right")
        self.fittingTabWidget.setStyleSheet("QTabBar::tab { width: 100px; }")

        self.k_chkbx = QCheckBox("Background K : ")
        self.k_spnbx = QDoubleSpinBox()
        self.k_spnbx.setObjectName('k_spnbx')
        self.editableVars[self.k_spnbx.objectName()] = None
        self.k_spnbx.setDecimals(4)
        self.k_spnbx.setRange(0, 99999999)
        self.k_spnbx.setEnabled(False)
        self.k_layout = QHBoxLayout()
        self.use_previous_fit_chkbx = QCheckBox("Use Previous Fit")
        self.k_layout.addWidget(self.k_chkbx)
        self.k_layout.addWidget(self.k_spnbx)

        self.refittingB = QPushButton("Refit current image")
        self.refitAllButton = QPushButton("Refit current folder")
        self.refitAllButton.setCheckable(True)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton2 = QPushButton("Reprocess and Refit current folder")
        self.processFolderButton2.setStyleSheet(pfss)
        self.processFolderButton2.setCheckable(True)
        self.bottomLayout2 = QGridLayout()
        self.prevButton2 = QPushButton("<<<")
        self.nextButton2 = QPushButton(">>>")
        self.filenameLineEdit2 = QLineEdit()
        self.bottomLayout2.addWidget(self.processFolderButton2, 0, 0, 1, 2)
        self.bottomLayout2.addWidget(self.prevButton2, 1, 0, 1, 1)
        self.bottomLayout2.addWidget(self.nextButton2, 1, 1, 1, 1)
        self.bottomLayout2.addWidget(self.filenameLineEdit2, 2, 0, 1, 2)

        self.fittingOptionsFrame1 = QFrame()
        self.fittingOptionsFrame1.setFixedWidth(300)
        self.fittingOptionsLayout = QVBoxLayout(self.fittingOptionsFrame1)
        self.fittingOptionsLayout.setAlignment(Qt.AlignLeft)
        self.fittingOptionsLayout.addWidget(self.generalGrp)
        self.fittingOptionsLayout.addSpacing(10)
        self.fittingOptionsLayout.addWidget(self.fitDispOptionGrp)
        self.fittingOptionsLayout.addStretch()


        self.fittingOptionsFrame2 = QFrame()
        self.fittingOptionsFrame2.setFixedWidth(300)
        self.fittingOptionsLayout2 = QVBoxLayout(self.fittingOptionsFrame2)
        self.fittingOptionsLayout2.addWidget(self.fittingTabWidget)
        self.fittingOptionsLayout2.addLayout(self.k_layout)
        self.fittingOptionsLayout2.addWidget(self.use_previous_fit_chkbx)
        self.fittingOptionsLayout2.addWidget(self.refittingB)
        self.fittingOptionsLayout2.addWidget(self.refitAllButton)
        self.fittingOptionsLayout2.addStretch()
        self.fittingOptionsLayout2.addLayout(self.bottomLayout2)

        self.fittingTabLayout.addWidget(self.fittingOptionsFrame1, 1, 0, 1, 1)
        self.fittingTabLayout.addWidget(self.fittingCanvas, 0, 1, 2, 2)
        self.fittingTabLayout.addWidget(self.fittingOptionsFrame2, 0, 0, 1, 1)
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
        ### Parameter Editor ###
        #
        self.parameterEditorTab = QWidget()
        self.parameterEditorTab.setContentsMargins(0, 0, 0, 0)
        self.parameterEditorLayout = QGridLayout(self.parameterEditorTab)
        self.paramEditorTitlebox = QGroupBox()
        self.paramEditorTitleboxLayout = QGridLayout()

        self.parameterEditorTable = QTableWidget()
        self.parameterEditorTable.setColumnCount(5)
        self.parameterEditorTable.setHorizontalHeaderLabels(["Fixed", "Parameter", "Value", "Min", "Max"])
        self.parameterEditorTable.horizontalHeader().setStretchLastSection(True)
        self.parameterEditorTable.setColumnWidth(0, 50)
        self.parameterEditorTable.setColumnWidth(1, 250)
        self.parameterEditorTable.setColumnWidth(2, 250)
        self.parameterEditorTable.setColumnWidth(3, 250)
        self.parameterEditorTable.setColumnWidth(4, 250)
        # self.parameterEditorTable.setEditTriggers(QAbstractItemView.NoEditTriggers)
        # self.resultLayout.addWidget(self.generalResultTable)
        self.refitParamsBtn = QPushButton("Re-fit Parameters")
        self.addSPeakBtn = QPushButton("Add 'S' peak parameter")
        self.enableExtraGaussBtn = QPushButton("Enable Extra Gaussian")
        self.paramEditorTitleboxLayout.addWidget(QLabel("<h2>Parameter Editor (lmfit values - does not persist)</h2>"), 1, 0, 1, 2)
        self.paramEditorTitleboxLayout.addWidget(self.refitParamsBtn, 1, 2, 1, 1)
        self.paramEditorTitleboxLayout.addWidget(self.addSPeakBtn, 1, 3, 1, 1)
        self.paramEditorTitleboxLayout.addWidget(self.enableExtraGaussBtn, 1, 4, 1, 1)
        self.paramEditorTitlebox.setLayout(self.paramEditorTitleboxLayout)
        self.parameterEditorLayout.addWidget(self.paramEditorTitlebox)
        self.parameterEditorLayout.addWidget(self.parameterEditorTable)
        self.tabWidget.addTab(self.parameterEditorTab, "Parameter Editor")

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
        self.statusReport = QLabel()
        self.right_status = QLabel()
        self.pixel_detail = QLabel()
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusBar.addWidget(self.left_status)
        self.statusBar.addPermanentWidget(self.statusReport)
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
        self.processFolderButton.setToolTip("Process all images in the same directory as the current file with current fitting parameters and image settings")

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
        self.processFolderButton2.setToolTip("Process all images in the same directory as the current file with current fitting parameters and image settings")
        self.refittingB.setToolTip("Refit current image again with current fitting parameters")
        self.refitAllButton.setToolTip("Refit all images in the directory again with current fitting parameters")

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        #
        ### Tab Widget ###
        #
        self.tabWidget.currentChanged.connect(self.updateUI)
        # self.fittingTabWidget.currentChanged.connect(self.updateUI)

        #
        ### Image Tab ###
        #
        self.centerChkBx.stateChanged.connect(self.updateImage)
        self.histChkBx.stateChanged.connect(self.updateImage)
        self.intChkBx.stateChanged.connect(self.updateImage)
        self.rminChkBx.stateChanged.connect(self.updateImage)
        self.imgPeakChkBx.stateChanged.connect(self.updateImage)
        self.minIntSpnBx.editingFinished.connect(self.intensityChanged)
        self.maxIntSpnBx.editingFinished.connect(self.intensityChanged)
        self.logScaleIntChkBx.stateChanged.connect(self.updateImage)
        self.imgZoomInB.clicked.connect(self.imgZoomIn)
        self.imgZoomOutB.clicked.connect(self.imgZoomOut)

        self.calibrationB.clicked.connect(self.calibrationClicked)
        self.setRotAndCentB.clicked.connect(self.setAngleAndCenterClicked)
        self.setAngleB.clicked.connect(self.setAngleClicked)
        self.setRminB.clicked.connect(self.setRminClicked)
        self.setIntAreaB.clicked.connect(self.setIntAreaClicked)
        self.fixedAngleChkBx.stateChanged.connect(self.fixedAngleChecked)
        self.fixedRminChkBx.stateChanged.connect(self.fixedRminChecked)
        self.fixedIntAreaChkBx.stateChanged.connect(self.fixedIntAreaChecked)
        self.modeAngleChkBx.clicked.connect(self.modeAngleChecked)
        self.fixedAngle.editingFinished.connect(self.fixedAngleChanged)
        self.fixedRmin.editingFinished.connect(self.fixedRminChanged)
        self.maskThresSpnBx.editingFinished.connect(self.maskThresChanged)
        self.applyBlank.stateChanged.connect(self.applyBlankChecked)
        self.blankSettings.clicked.connect(self.blankSettingClicked)
        self.orientationCmbBx.currentIndexChanged.connect(self.orientationModelChanged)
        self.rotation90ChkBx.stateChanged.connect(self.rotation90Checked)
        self.forceRot90ChkBx.stateChanged.connect(self.forceRot90Checked)
        self.resetAllB.clicked.connect(self.resetAll)

        self.prevButton.clicked.connect(self.prevClicked)
        self.nextButton.clicked.connect(self.nextClicked)
        #self.processFolderButton.clicked.connect(self.processFolder)
        self.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.filenameLineEdit.editingFinished.connect(self.fileNameChanged)
        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)
        self.displayImgFigure.canvas.mpl_connect('button_release_event', self.imgReleased)
        self.displayImgFigure.canvas.mpl_connect('figure_leave_event', self.leaveImage)
        self.displayImgFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)
        self.rejectChkBx.stateChanged.connect(self.rejectClicked)

        #### Fitting Tab
        # self.skeletalChkBx.stateChanged.connect(self.refreshAllFittingParams)
        self.nPeakSpnBx.editingFinished.connect(self.nPeakChanged)
        # self.modelSelect.currentIndexChanged.connect(self.refreshAllFittingParams)
        self.setPeaksB.clicked.connect(self.setManualPeaks)
        self.origHistChkBx.stateChanged.connect(self.refreshGraph)
        self.hullChkBx.stateChanged.connect(self.refreshGraph)
        self.peakChkBx.stateChanged.connect(self.refreshGraph)
        self.dispZlineChkBx.stateChanged.connect(self.refreshGraph)
        self.centerXChkBx.stateChanged.connect(self.refreshGraph)
        self.fitChkBx.stateChanged.connect(self.refreshGraph)
        self.graphZoomInB.clicked.connect(self.graphZoomIn)
        self.graphZoomOutB.clicked.connect(self.graphZoomOut)

        #self.processFolderButton2.clicked.connect(self.processFolder)
        self.processFolderButton2.toggled.connect(self.batchProcBtnToggled)
        self.prevButton2.clicked.connect(self.prevClicked)
        self.nextButton2.clicked.connect(self.nextClicked)
        self.filenameLineEdit2.editingFinished.connect(self.fileNameChanged)
        self.fittingFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.fittingFigure.canvas.mpl_connect('motion_notify_event', self.plotOnMotion)
        self.fittingFigure.canvas.mpl_connect('button_release_event', self.plotReleased)
        self.fittingFigure.canvas.mpl_connect('figure_leave_event', self.leavePlot)
        self.fittingFigure.canvas.mpl_connect('scroll_event', self.plotScrolled)

        self.k_chkbx.stateChanged.connect(self.k_checked)
        self.k_spnbx.editingFinished.connect(self.kChanged)
        self.refittingB.clicked.connect(self.refitting)
        self.refitAllButton.toggled.connect(self.refitAllBtnToggled)

        #### Parameter Editor Tab
        self.parameterEditorTable.itemClicked.connect(self.onRowFixed)
        self.refitParamsBtn.clicked.connect(self.refitParamEditor)
        self.addSPeakBtn.clicked.connect(self.addSPeak)
        self.enableExtraGaussBtn.clicked.connect(self.enableExtraGauss)

    def k_checked(self):
        """
        Handle when bias k is checked or unchecked
        """
        self.k_spnbx.setEnabled(self.k_chkbx.isChecked())

    def kChanged(self):
        self.log_changes('backgroudK', obj=self.k_spnbx)

    def refitting(self):
        """
        Fixed Value Changed. Remove fit_results from info dict to make it be re-calculated and Recalculate
        :return:
        """
        self.refreshAllFittingParams()
        if self.use_previous_fit_chkbx.isChecked() and self.bioImg is not None:
            print("Using previous fit")
            self.updateFittingParamsInParamInfo()
            self.processImage(self.bioImg.info['paramInfo'])
            return
        self.processImage()

    def updateFittingParamsInParamInfo(self):
        paramInfo = self.bioImg.info['paramInfo']
        settings = self.getSettings()
        paramInfo['isSkeletal']['val'] = settings['isSkeletal']

        if 'fix_k' in settings:
            paramInfo['k']['fixed'] = True
            paramInfo['k']['val'] = settings['fix_k']
        else:
            paramInfo['k']['fixed'] = False
            paramInfo['k']['val'] = 0

        for side in ['left', 'right']:
            fitting_tab = self.left_fitting_tab if side == 'left' else self.right_fitting_tab
            fitparams = fitting_tab.getFittingSettings()
            self.updateParamInfo('sigmac', side, fitparams)
            self.updateParamInfo('sigmad', side, fitparams)
            self.updateParamInfo('sigmas', side, fitparams)
            self.updateParamInfo('gamma', side, fitparams)
            self.updateParamInfo('intz', side, fitparams)
            self.updateParamInfo('sigz', side, fitparams)
            self.updateParamInfo('zline', side, fitparams)
            self.updateParamInfo('gammaz', side, fitparams)

    def updateParamInfo(self, param, side, fitparams):
        paramInfo = self.bioImg.info['paramInfo']
        p = param if param != 'sigz' else 'sigmaz' #Handling sigz and sigmaz discrepancy, side_fix_sigz vs side_sigmaz
        pInfo = paramInfo[side + '_' + p]
        if side+'_fix_' + param in fitparams:
            pInfo['fixed'] = True
            pInfo['val'] = fitparams[side+'_fix_'+param]
        else:
            pInfo['fixed'] = False
            if side + '_' + param in fitparams:
                pInfo['val'] = fitparams[side + '_' + param]

        
    def refitAllBtnToggled(self):
        if self.refitAllButton.isChecked():
            if not self.in_batch_process:
                self.refitAllButton.setText("Stop")
                self.refitAll()
        else:
            self.stop_process = True
    
    def refitAll(self):
        """
        Refit current folder
        """
        ## Popup confirm dialog with settings
        nImg = len(self.imgList)
        errMsg = QMessageBox()
        errMsg.setText('Refitting All')
        text = 'The current folder will be refitted using current settings. Make sure to adjust them before refitting the folder. \n\n'
        settings = self.getSettings()
        text += "\nCurrent Settings"

        if 'fixed_angle' in settings:
            text += "\n  - Fixed Angle : " + str(settings["fixed_angle"])
        if 'fixed_rmin' in settings:
            text += "\n  - Fixed R-min : " + str(settings["fixed_rmin"])
        if 'fixed_int_area' in settings:
            text += "\n  - Fixed Box Width : " + str(settings["fixed_int_area"])

        text += "\n  - Orientation Finding : " + str(self.orientationCmbBx.currentText())
        text += "\n  - Skeletal Muscle : " + str(settings["isSkeletal"])
        text += "\n  - Number of Peaks on each side : " + str(settings["nPeaks"])
        text += "\n  - Model : " + str(settings["model"])

        for side in ['left', 'right']:
            if side+'_fix_sigmac' in settings.keys():
                text += "\n  - "+side+" Fixed Sigma C : " + str(settings[side+'_fix_sigmac'])
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
            if "center" in self.calSettings:
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

            # Refit current image
            self.refitting()

            ## Process all images and update progress bar
            self.in_batch_process = True
            self.stop_process = False
            for i in range(nImg):
                if self.stop_process:
                    break
                self.progressBar.setValue(i)
                QApplication.processEvents()
                self.nextImageFitting()
            self.in_batch_process = False

        self.progressBar.setVisible(False)
        self.refitAllButton.setChecked(False)
        self.refitAllButton.setText("Refit current folder")

    def applyBlankChecked(self):
        """
        Trigger when Apply Blank Image and Mask is checked or unchecked
        :return:
        """
        self.blankSettings.setEnabled(self.applyBlank.isChecked())
        if self.bioImg is not None and not self.syncUI:
            if self.applyBlank.isChecked():
                self.blankSettingClicked()
            else:
                self.resetAll()

    def blankSettingClicked(self):
        """
        Trigger when Set Blank Image and Mask clicked
        """
        dlg = BlankImageSettings(self.dir_path)
        result = dlg.exec_()
        if result == 1 and self.bioImg is not None:
            self.resetAll()


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
        Popup an input file dialog. Users can select an image or .txt for failed cases list
        """
        file_name = getAFile(add_txt=True)
        _, ext = os.path.splitext(str(file_name))
        _, name = split(str(file_name))
        if file_name != "":
            if ext == ".txt" and not name == "failedcases.txt":
                errMsg = QMessageBox()
                errMsg.setText('Invalid Input')
                errMsg.setInformativeText("Please select only failedcases.txt or an image\n\n")
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
            else:
                self.mainWindow.runBioMuscle(str(file_name))

    def clearAllCache(self):
        """
        Delete all caches in a directory
        """
        cache_path = fullPath(self.dir_path, "eq_cache")
        shutil.rmtree(cache_path)

    def nPeakChanged(self):
        """
        Triggered when number of peaks are changed
        :return:
        """
        if self.bioImg is not None and not self.syncUI:
            self.bioImg.removeInfo("peaks")  # Remove peaks info before re-processing
            self.log_changes('nPeaks', self.nPeakSpnBx)

    def batchProcBtnToggled(self):
        if self.processFolderButton.isChecked():
            if not self.in_batch_process:
                self.processFolderButton.setText("Stop")
                self.processFolder()
        elif self.processFolderButton2.isChecked():
            if not self.in_batch_process:
                self.processFolderButton2.setText("Stop")
                self.processFolder()
        else:
            self.stop_process = True

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

        if 'fixed_angle' in settings:
            text += "\n  - Fixed Angle : " + str(settings["fixed_angle"])
        if 'fixed_rmin' in settings:
            text += "\n  - Fixed R-min : " + str(settings["fixed_rmin"])
        if 'fixed_int_area' in settings:
            text += "\n  - Fixed Box Width : " + str(settings["fixed_int_area"])

        text += "\n  - Orientation Finding : " + str(self.orientationCmbBx.currentText())
        text += "\n  - Skeletal Muscle : " + str(settings["isSkeletal"])
        text += "\n  - Number of Peaks on each side : " + str(settings["nPeaks"])
        text += "\n  - Model : " + str(settings["model"])

        for side in ['left', 'right']:
            if side+'_fix_sigmac' in settings.keys():
                text += "\n  - "+side+" Fixed Sigma C : " + str(settings[side+'_fix_sigmac'])
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
            if "center" in self.calSettings:
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
            self.in_batch_process = True
            self.stop_process = False
            for i in range(nImg):
                if self.stop_process:
                    break
                self.progressBar.setValue(i)
                QApplication.processEvents()
                self.nextClicked()
            self.in_batch_process = False

        self.progressBar.setVisible(False)
        self.processFolderButton.setChecked(False)
        self.processFolderButton.setText("Reprocess and Refit current folder")
        self.processFolderButton2.setChecked(False)
        self.processFolderButton2.setText("Reprocess and Refit current folder")

    def setCalibrationImage(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or calibration.tif in the folder
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        if self.calibSettingDialog is None:
            self.calibSettingDialog = CalibrationSettings(self.dir_path) if self.bioImg is None else \
                CalibrationSettings(self.dir_path, center=self.bioImg.info['center'])
        self.calSettings = None
        cal_setting = self.calibSettingDialog.calSettings
        if cal_setting is not None or force:
            result = self.calibSettingDialog.exec_()
            logMsgs = self.calibSettingDialog.logMsgs
            if result == 1:
                self.calSettings = self.calibSettingDialog.getValues()
                for msg in logMsgs:
                    self.write_log('(calib) ' + msg)
                del logMsgs[:]
                return True
            del logMsgs[:]
        return False

    def rejectClicked(self):
        """
        Mark EquatorImage object as rejected. Save to cache and write data tto summary file
        """
        if self.bioImg is None or self.syncUI:
            return
        self.bioImg.info["reject"] = self.rejectChkBx.isChecked()
        self.bioImg.saveCache()
        self.csvManager.writeNewData(self.bioImg)
        self.csvManager2.writeNewData(self.bioImg)

    def maskThresChanged(self):
        """
        Re-process and start from apply convexhull
        """
        if self.bioImg is not None and not self.syncUI:
            self.log_changes('maskThres', obj=self.maskThresSpnBx)
            self.bioImg.removeInfo('hulls')
            self.processImage()

    def resetAll(self):
        """
        Remove all processing info from EquatorImage object and re-process with current settings
        """
        if self.bioImg is not None:
            self.fixedAngleChkBx.setChecked(False)
            self.fixedRminChkBx.setChecked(False)
            self.fixedIntAreaChkBx.setChecked(False)
            self.bioImg.removeInfo()
            self.bioImg.delCache()
            self.processImage()

    def refreshAllFittingParams(self):
        """
        Clear fit results
        :return:
        """
        if self.bioImg is None or self.syncUI:
            return

        self.left_fitting_tab.hideGamma(str(self.modelSelect.currentText()) != 'Voigt')
        self.right_fitting_tab.hideGamma(str(self.modelSelect.currentText()) != 'Voigt')

        if self.bioImg is not None:
            self.bioImg.removeInfo('fit_results')
            self.bioImg.removeInfo('peaks')
            self.refreshFittingParams('left')
            self.refreshFittingParams('right')

    def refreshFittingParams(self, side):
        if self.bioImg is not None:
            self.bioImg.removeInfo(side + '_fix_sigmac')
            self.bioImg.removeInfo(side + '_fix_sigmad')
            self.bioImg.removeInfo(side + '_fix_sigmas')
            self.bioImg.removeInfo(side + '_fix_gamma')
            self.bioImg.removeInfo(side + '_fix_sigz')
            self.bioImg.removeInfo(side + '_fix_intz')
            self.bioImg.removeInfo(side + '_fix_zline')
            self.bioImg.removeInfo(side + '_fix_gammaz')
            self.bioImg.removeInfo('fix_k')

    def prevClicked(self):
        """
        Going to the previous image
        """
        self.currentImg = (self.currentImg - 1) % len(self.imgList)
        self.onImageChanged()
        
    def nextImageFitting(self):
        self.currentImg = (self.currentImg + 1) % len(self.imgList)
        
        fileName = self.imgList[self.currentImg]
        self.filenameLineEdit.setText(fileName)
        self.filenameLineEdit2.setText(fileName)
        self.bioImg = EquatorImage(self.dir_path, fileName, self)
        settings = None
        #if len(self.bioImg.info) < 2: # use settings of the previous image
        settings = self.getSettings()
        nPeaks = settings['nPeaks'] if 'nPeaks' in settings else None
        isSkeletal = settings['isSkeletal'] if 'isSkeletal' in settings else None
        if self.calSettings is not None:
            self.bioImg.removeInfo()
        # settings.update(self.bioImg.info)
        
        if nPeaks != None:
            settings['nPeaks'] = nPeaks
        if isSkeletal != None:
            settings['isSkeletal'] = isSkeletal
        self.initWidgets(settings)
        self.initMinMaxIntensities(self.bioImg)
        self.img_zoom = None
        self.refreshStatusbar()
        self.refitting()

    def nextClicked(self):
        """
        Going to the next image
        """
        self.currentImg = (self.currentImg + 1) % len(self.imgList)
        self.onImageChanged()

    def fileNameChanged(self):
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            fileName = str(self.filenameLineEdit.text()).strip()
        elif selected_tab == 1:
            fileName = str(self.filenameLineEdit2.text()).strip()
        if fileName not in self.imgList:
            return
        self.currentImg = self.imgList.index(fileName)
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
            self.bioImg.delCache()
            self.processImage()

    def fixedRminChecked(self):
        """
        Triggered when fixed R-min is checked or unchecked
        """
        self.fixedRmin.setEnabled(self.fixedRminChkBx.isChecked())
        if not self.fixedRminChkBx.isChecked() and self.bioImg is not None:
            self.bioImg.removeInfo("fixed_rmin")
            self.bioImg.removeInfo("rmin")
            self.bioImg.delCache()
            self.processImage()

    def fixedIntAreaChecked(self):
        """
        Triggered when fixed integrated area is checked or unchecked
        """
        if self.bioImg is not None:
            if not self.fixedIntAreaChkBx.isChecked():
                self.fixedIntArea = None
                self.bioImg.removeInfo("fixed_int_area")
                self.bioImg.delCache()
                self.processImage()
            else:
                self.fixedIntArea = self.bioImg.info['int_area']

    def modeAngleChecked(self):
        """
        Triggered when mode angle is checked or unchecked
        """
        print("FUnction executed", flush=True)

        if self.bioImg is not None:

            modeOrientation = self.getModeRotation()
            if modeOrientation != None:
                if not self.modeAngleChkBx.isChecked():
                    self.bioImg.removeInfo('rmin')  # Remove R-min from info dict to make it be re-calculated
                    self.bioImg.removeInfo("mode_angle")
                    self.processImage()
                else:
                    self.bioImg.removeInfo('rmin')  # Remove R-min from info dict to make it be re-calculated
                    self.bioImg.info["mode_angle"] = modeOrientation
                    self.processImage()
            else:
                f=1
                self.modeAngleChkBx.setCheckState(Qt.Unchecked) # executes twice, setChecked executes once but button becomes unresponsive for one click

                msg = QMessageBox()
                msg.setInformativeText("All images in folder must be processed first, use Process Folder to process all images")
                msg.setStandardButtons(QMessageBox.Ok)
                msg.setWindowTitle("Mode Orientation Failed")
                msg.setStyleSheet("QLabel{min-width: 500px;}")
                msg.exec_()
                return

    def getModeRotation(self):
        """
        open images and calculate the mode orientation
        :param file_list: list of image path (str)
        :return: mode of orientation of all images in the folder
        """
        if self.modeOrientation != None:
            return self.modeOrientation
        print("Calculating mode of angles of images in directory")
        angles = []
        for f in self.imgList:
            bioImg = EquatorImage(self.dir_path, f, self)
            print("Getting angle {}".format(f))

            if 'rotationAngle' not in bioImg.info:
                return
            angle = bioImg.info['rotationAngle']
            angles.append(angle)
        self.modeOrientation = max(set(angles), key=angles.count)
        return self.modeOrientation

    def fixedAngleChanged(self):
        """
        Triggered when fixed angle spinbox value is changed
        """
        if self.bioImg is not None and not self.syncUI:
            self.bioImg.removeInfo("rotationAngle")
            self.processImage()

    def fixedRminChanged(self):
        """
        Triggered when fixed R-min spinbox value is changed
        """
        if self.bioImg is not None and not self.syncUI:
            self.log_changes('fixedRmin', obj=self.fixedRmin)
            self.bioImg.info['fixed_rmin'] = self.fixedRmin.value()
            self.processImage()

    def orientationModelChanged(self):
        self.orientationModel = self.orientationCmbBx.currentIndex()
        self.bioImg.removeInfo('rotationAngle')
        self.processImage()

    def rotation90Checked(self):
        self.bioImg.removeInfo('rmin')
        self.processImage()

    def forceRot90Checked(self):
        if self.forceRot90ChkBx.isChecked():
            self.rotation90ChkBx.setChecked(True)
            self.rotation90ChkBx.setEnabled(False)
        else:
            self.rotation90ChkBx.setEnabled(True)

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
                self.log_changes('center', varName='center', newValue=self.bioImg.info['center'])
                self.bioImg.info['rotationAngle'] = self.bioImg.info['rotationAngle'] + new_angle
                self.fixedAngle.setValue(self.bioImg.info['rotationAngle'])
                self.log_changes('rotationAngle', obj=self.fixedAngle)
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
            self.fixedAngle.setValue(self.bioImg.info['rotationAngle'])
            self.log_changes('rotationAngle', obj=self.fixedAngle)
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
                self.log_changes('intArea', varName='int_area', newValue=(t, b))
                self.bioImg.removeInfo('hist')
                if self.fixedIntAreaChkBx.isChecked():
                    self.fixedIntArea = (t, b)
                self.function = None
                self.setIntAreaB.setChecked(False)
                self.processImage()
        elif func[0] == "rmin":
            # Set new R-min, re-calculate from getting integrated area process
            self.bioImg.info['rmin'] = int(np.round(distance(self.bioImg.info['center'], (x, y))))
            self.fixedRmin.setValue(self.bioImg.info['rmin'])
            self.log_changes('Rmin', obj=self.fixedRmin)
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
        if self.logger is not None:
            self.logger.popup()
            self.logger.close()
        self.mainWindow.childWindowClosed(self)

    def initWidgets(self, info):
        """
        Update GUI to sync with EquatorImage info
        :param info: EquatorImage info
        """
        self.syncUI = True

        # Initial UI for each fitting tab : left, right
        self.left_fitting_tab.initSpinBoxes(info)
        self.right_fitting_tab.initSpinBoxes(info)

        # Initial UI for general settings
        if 'isSkeletal' in info:
            self.skeletalChkBx.setChecked(info['isSkeletal'])

        self.maskThresSpnBx.setValue(info['mask_thres'])

        if 'fit_results' in info:
            fit_results = info['fit_results']
            self.modelSelect.setCurrentIndex(self.modelSelect.findText(fit_results["model"]))
            self.k_spnbx.setValue(fit_results['k'])

        # init bias k
        if 'fix_k' in info:
            self.k_chkbx.setChecked(True)
            self.k_spnbx.setValue(info['fix_k'])
                
        if 'nPeaks' in info:
            self.nPeakSpnBx.setValue(info['nPeaks'])

        # Initital reject check box
        if "reject" in info.keys():
            self.rejectChkBx.setChecked(info["reject"])
        else:
            self.rejectChkBx.setChecked(False)

        if 'blank_mask' in info:
            self.applyBlank.setChecked(info['blank_mask'])

        if 'fixed_angle' in info:
            self.fixedAngle.setValue(info['fixed_angle'])
        self.fixedAngleChkBx.setChecked('fixed_angle' in info)
        self.fixedAngle.setEnabled('fixed_angle' in info)

        self.fixedRminChkBx.setChecked('fixed_rmin' in info)
        self.fixedRmin.setEnabled('fixed_rmin' in info)
        if 'fixed_rmin' in info:
            self.fixedRmin.setValue(info['fixed_rmin'])

        if 'fixed_max_intensity' in info:
            self.maxIntSpnBx.setValue(info['fixed_max_intensity'])

        if self.rotation90ChkBx.isEnabled():
            self.rotation90ChkBx.setChecked('90rotation' in info and info['90rotation'])

        self.syncUI = False

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new EquatorImage object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        if self.fixedFittingParamChanged(self.getSettings()):
            print("Refitting current image first")
            self.refitting()

        fileName = self.imgList[self.currentImg]
        self.filenameLineEdit.setText(fileName)
        self.filenameLineEdit2.setText(fileName)
        prevInfo = self.bioImg.info if self.bioImg is not None else None
        self.bioImg = EquatorImage(self.dir_path, fileName, self)
        settings = None
        #if len(self.bioImg.info) < 2: # use settings of the previous image
        settings = self.getSettings()
        print("Settings in onImageChange before update")
        print(settings)
        if self.calSettings is not None:
            self.bioImg.removeInfo()
        # settings.update(self.bioImg.info)
        self.initWidgets(settings)
        self.initMinMaxIntensities(self.bioImg)
        self.img_zoom = None
        self.refreshStatusbar()

        if self.fixedParamChanged(prevInfo):
            print("Refitting next image")
            self.refreshAllFittingParams()

        if self.use_previous_fit_chkbx.isChecked():
            print("Using previous fit")
            self.updateFittingParamsInParamInfo()
            self.processImage(self.bioImg.info['paramInfo'])

        # Process new image
        self.processImage()

    def fixedParamChanged(self, prevInfo):
        '''
        Checks whether any of the fixed paramters have been changed
        :param prevInfo: info dict of previous image
        :return: bool True if any fixed param is changed else false
        '''
        if prevInfo is None:
            return False

        currentInfo = self.bioImg.info

        # Check fixed rmin
        if self.fixedRminChkBx.isChecked() and self.paramChanged(prevInfo, currentInfo, 'rmin'):
            self.bioImg.removeInfo('int_area')  # Remove integrated area from info dict to make it be re-calculated
            return True

        # Check fixed Angle
        if self.fixedAngleChkBx.isChecked() and self.paramChanged(prevInfo, currentInfo, 'rotationAngle'):
            self.bioImg.removeInfo('rmin')  # Remove R-min from info dict to make it be re-calculated
            return True

        # Check Mode Angle
        if (self.modeAngleChkBx.isChecked() and 'mode_angle' not in currentInfo):
            self.bioImg.removeInfo('rmin')  # Remove R-min from info dict to make it be re-calculated
            return True

        # Check fixed int area
        if self.fixedIntAreaChkBx.isChecked() and self.paramChanged(prevInfo, currentInfo, 'int_area'):
            self.bioImg.removeInfo('int_area')  # Remove integrated area from info dict to make it be re-calculated
            return True

        return self.fixedFittingParamChanged(prevInfo)

    def fixedFittingParamChanged(self, prevInfo):
        '''
        Check left and right fitting params
        :param prevInfo:
        :return:
        '''

        if prevInfo is None or self.bioImg is None:
            return False

        currentInfo = self.bioImg.info

        # Check Background k
        if self.k_chkbx.isChecked() and self.paramChanged(prevInfo, currentInfo, 'fix_k'):
            return True

        # Check if number of peaks has changed
        if self.paramChanged(prevInfo, currentInfo, 'nPeaks'):
            return True

        #Check if skeletal z line is checked
        if self.paramChanged(prevInfo, currentInfo, 'isSkeletal'):
            return True

        for side in ['left', 'right']:
            fitting_tab = self.left_fitting_tab if side == 'left' else self.right_fitting_tab

            if fitting_tab.fixSigmaC.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_sigmac'):
                return True

            if fitting_tab.fixSigmaD.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_sigmad'):
                return True

            if fitting_tab.fixSigmaS.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_sigmas'):
                return True

            if fitting_tab.fixGamma.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_gamma'):
                return True

            if fitting_tab.parent.skeletalChkBx.isChecked():
                if fitting_tab.fixedIntZ.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_intz'):
                    return True
                if fitting_tab.fixedSigZ.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_sigz'):
                    return True
                if fitting_tab.fixedZline.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_zline'):
                    return True
                if fitting_tab.fixedGammaZ.isChecked() and self.paramChanged(prevInfo, currentInfo,
                                                                             side + '_fix_gammaz'):
                    return True
        return False

    def paramChanged(self, prevInfo, currentInfo, param):
        '''
        Check is the parameter is changed in current vs previous image
        :param prevInfo: info of prev image
        :param currentInfo: info of current image
        :param param: parameter to be checked
        :return:
        '''
        if param not in currentInfo and param not in prevInfo:
            #Paramter not fixed
            return False
        if param not in prevInfo or param not in currentInfo or prevInfo[param] != currentInfo[param]:
            return True
        return False

    def processImage(self, paramInfo=None):
        """
        Process Image by getting all settings and call process() of EquatorImage object
        Then, write data and update UI
        """
        if self.bioImg is None:
            return
        QApplication.setOverrideCursor(Qt.WaitCursor)
        QApplication.processEvents()
        settings = self.getSettings()
        print("Settings in processImage:")
        print(settings)
        try:
            self.bioImg.process(settings, paramInfo)
        except Exception as e:
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

        self.updateParams()
        self.csvManager.writeNewData(self.bioImg)
        self.csvManager2.writeNewData(self.bioImg)
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

    def updateParams(self):
        info = self.bioImg.info
        if 'orientation_model' in info:
            self.orientationModel = info['orientation_model']

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
        Get all settings for EquatorImage process() from widgets
        :return: settings (dict)
        """
        settings = {}
        settings.update(self.left_fitting_tab.getFittingSettings())
        settings.update(self.right_fitting_tab.getFittingSettings())

        settings['orientation_model'] = self.orientationModel
        settings['nPeaks'] = self.nPeakSpnBx.value()
        settings['model'] = str(self.modelSelect.currentText())
        settings['isSkeletal'] = self.skeletalChkBx.isChecked()
        settings['mask_thres'] = self.maskThresSpnBx.value()
        settings['90rotation'] = self.rotation90ChkBx.isChecked()

        if self.calSettings is not None:
            if 'type' in self.calSettings:
                if self.calSettings["type"] == "img":
                    settings["center"] = self.calSettings["center"]
                    settings["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
                else:
                    settings["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings[
                        "pixel_size"]
            if "center" in self.calSettings:
                settings["center"] = self.calSettings["center"]

        if self.fixedAngleChkBx.isChecked():
            settings['fixed_angle'] = self.fixedAngle.value()

        if self.fixedRminChkBx.isChecked():
            settings['fixed_rmin'] = self.fixedRmin.value()

        if self.persistMaxIntensity.isChecked():
            settings['fixed_max_intensity'] = self.maxIntSpnBx.value()

        if self.fixedIntAreaChkBx.isChecked() and self.fixedIntArea is not None:
            settings["fixed_int_area"] = self.fixedIntArea

        if self.modeAngleChkBx.isChecked():
            modeOrientation = self.getModeRotation()
            if modeOrientation != None:
                settings["mode_angle"] = modeOrientation

        if self.applyBlank.isChecked():
            settings['blank_mask'] = True
        else:
            settings['blank_mask'] = False

        if self.k_chkbx.isChecked():
            settings['fix_k'] = self.k_spnbx.value()

        return settings

    def initMinMaxIntensities(self, bioImg):
        """
        Set preference for image min & max intesity spinboxes, and initial their value
        :param bioImg: current EquatorImage object
        :return:
        """
        img = bioImg.orig_img
        self.syncUI = True
        self.minIntSpnBx.setMinimum(img.min())
        self.minIntSpnBx.setMaximum(img.max())
        self.maxIntSpnBx.setMinimum(img.min())
        if self.persistMaxIntensity.isChecked():
            self.maxIntSpnBx.setMaximum(self.maxIntSpnBx.value())
        else:
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
        if self.editableVars[self.minIntSpnBx.objectName()] != self.minIntSpnBx.value():
            self.log_changes('minIntensity', obj=self.minIntSpnBx)
        else:
            self.log_changes('maxIntensity', obj=self.maxIntSpnBx)
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
        Update Spinboxes values by using current EquatorImage info (after processing)
        """
        self.syncUI = True
        info = self.bioImg.info
        self.left_fitting_tab.syncSpinBoxes(info)
        self.right_fitting_tab.syncSpinBoxes(info)
        self.fixedAngle.setValue(info["rotationAngle"])
        self.fixedRmin.setValue(info['rmin'])

        if 'fit_results' in info:
            self.k_spnbx.setValue(info['fit_results']['k'])

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
        elif selected_tab == 3:
            self.updateParameterEditorTab()

        self.syncSpinBoxes()
        self.refreshStatusbar()

        # focused_widget = QApplication.focusWidget()
        # if focused_widget != None:
        #     focused_widget.clearFocus()

        QApplication.processEvents()

    def updateImageTab(self):
        """
        Draw all UI in image tab
        """
        info = copy.copy(self.bioImg.info)
        img = self.bioImg.getRotatedImage()
        #disp_img = getBGR(get8bitImage(img, self.minIntSpnBx.value(), self.maxIntSpnBx.value()))
        hulls = info['hulls']['all']
        center = info['center']
        rmin = info['rmin']
        int_area = info['int_area']
        ax = self.displayImgAxes
        ax.cla()
        #ax.imshow(disp_img)  # Display selected image
        if self.logScaleIntChkBx.isChecked():
            ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.minIntSpnBx.value()), vmax=self.maxIntSpnBx.value()))
        else:
            ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.minIntSpnBx.value(), vmax=self.maxIntSpnBx.value()))
        ax.set_facecolor('black')

        self.orientationCmbBx.setCurrentIndex(0 if self.orientationModel is None else self.orientationModel)
        if self.rotation90ChkBx.isEnabled():
            self.rotation90ChkBx.setChecked('90rotation' in info and info['90rotation'])

        self.calibSettingDialog.centerX.setValue(center[0])
        self.calibSettingDialog.centerY.setValue(center[1])
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
            norm = float(img.shape[0] - center[1]) * .8 / max(hulls)
            hulls = np.array([img.shape[0] - p * norm for p in hulls])
            ax.fill(hulls, facecolor='white')

            xs = np.linspace(0, len(hulls), len(hulls))
            if 'fit_results' in info.keys():
                cardiac = (np.array(getCardiacGraph(xs, info['fit_results'])) * norm - img.shape[0]) * -1
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

            if 'model_peaks' in fit_result and self.peakChkBx.isChecked():
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
            genResults += "<b>S0 : </b>" + str(fit_results["S0"]) + '<br/><br/>'
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
            if "center" in self.calSettings:
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


                if fit_results['model'] == 'Voigt':

                    # Display gamma in table if model is Voigt
                    self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("gamma"))
                    self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_gamma'])))
                    self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_gamma'])))
                    ind += 1

                    # Display gamma in table if model is Voigt
                    self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Gamma Z"))
                    self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_gammaz'])))
                    self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_gammaz'])))
                    ind += 1

        self.fiberResultTable.setRowCount(ind)
        QApplication.processEvents()

    def updateParameterEditorTab(self):
        '''
        Updates Parameter Editor Tab
        :return:
        '''
        self.parameterEditorTable.clearContents()
        if 'paramInfo' not in self.bioImg.info:
            print("Parameter editor information missing")
            return
        paramInfo = self.bioImg.info['paramInfo']
        ind=0
        self.parameterEditorTable.setRowCount(200)
        if paramInfo is not None:
            for k in paramInfo.keys():
                if 'model' in paramInfo and paramInfo['model']['val'] =='Gaussian' and 'gamma' in k:
                    continue

                self.parameterEditorTable.setItem(ind, 1, QTableWidgetItem(k))
                v = paramInfo[k]['val']
                if not isinstance(v, bool) and (isinstance(v, float) or isinstance(v, int)):

                    if not self.isDynamicParameter(k):
                        # Parameter cannot be fixed if it is dynamically being handled like left_areas
                        chkBoxItem = QTableWidgetItem()
                        chkBoxItem.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
                        chkBoxItem.setCheckState(Qt.Checked if paramInfo[k]['fixed'] else Qt.Unchecked)

                        self.parameterEditorTable.setItem(ind, 0, chkBoxItem)

                    valueItem = QDoubleSpinBox(self.parameterEditorTable)
                    valueItem.setDecimals(6)
                    valueItem.setRange(float('-inf'), 100000000000000000000)
                    valueItem.setValue(v)
                    self.parameterEditorTable.setCellWidget(ind, 2, valueItem)

                    valueItem = QDoubleSpinBox(self.parameterEditorTable)
                    valueItem.setDecimals(6)
                    valueItem.setRange(float('-inf'), 100000000000000000000)
                    valueItem.setValue(paramInfo[k]['min'])
                    self.parameterEditorTable.setCellWidget(ind, 3, valueItem)

                    valueItem = QDoubleSpinBox(self.parameterEditorTable)
                    valueItem.setDecimals(6)
                    valueItem.setRange(float('-inf'), 100000000000000000000)
                    valueItem.setValue(paramInfo[k]['max'])
                    self.parameterEditorTable.setCellWidget(ind, 4, valueItem)

                    self.adjustMinMaxColumn(chkBoxItem)
                else:
                    valueItem = QTableWidgetItem(str(v))
                    valueItem.setFlags(Qt.ItemIsEditable)
                    self.parameterEditorTable.setItem(ind, 2, valueItem)
                ind+=1
        self.parameterEditorTable.setRowCount(ind)
        QApplication.processEvents()

    def isDynamicParameter(self, paramName):
        '''
        Checks whether parameter is dynamically handelled by fitting mechanism
        :param paramName: Name of the parameter to be checked
        :return: bool True if it is in the dynamic parameter list
        '''

        dynamicParams = ['Speak', 'left_area', 'right_area']
        for p in dynamicParams:
            if p in paramName:
                return True
        return False

    def adjustMinMaxColumn(self,item):
        '''
        Toggles the min and max items of the row appropriately
        :param item:
        :return:
        '''
        if item == None:
            return
        if item.column() == 0:
            row = item.row()
            table = self.parameterEditorTable
            minItem = table.cellWidget(row, 3)
            maxItem = table.cellWidget(row, 4)
            if item.checkState() == Qt.Checked:
                minItem.setEnabled(False)
                maxItem.setEnabled(False)
            else:
                minItem.setEnabled(True)
                maxItem.setEnabled(True)

    def onRowFixed(self, item):
        self.adjustMinMaxColumn(item)
        QApplication.processEvents()

    def getInfoFromParameterEditor(self):
        '''
        To get information from parameter editor
        :return: data from parameter editor as dictionary
        '''
        paramInfo = {}
        table = self.parameterEditorTable
        for row in range(0, table.rowCount()):
            c0 = table.item(row, 0)
            c1 = table.item(row, 1).text()
            paramInfo[c1] = {}
            if table.cellWidget(row, 2) != None:
                c2 = table.cellWidget(row, 2).value()
                c3 = table.cellWidget(row, 3).value()
                c4 = table.cellWidget(row, 4).value()
                paramInfo[c1]['fixed'] = c0.checkState() == Qt.Checked if c0 is not None else False
                paramInfo[c1]['val'] = c2
                paramInfo[c1]['min'] = c3
                paramInfo[c1]['max'] = c4
            else:
                c2 = table.item(row, 2).text()
                paramInfo[c1]['fixed'] = True
                if c2 == 'True' or c2 == 'False':
                    c2 = bool(c2)
                paramInfo[c1]['val'] = c2

        # Adding remaining parameters as fixed to avoid complaining about missing parameters
        oldParamInfo = self.bioImg.info['paramInfo']
        for k in oldParamInfo:
            if k not in paramInfo:
                paramInfo[k] = oldParamInfo[k]
        return paramInfo

    def addSPeak(self):
        '''
        Adds Speak parameter to parameter editor
        :return:
        '''
        if self.bioImg is not None and 'peaks' in self.bioImg.info:
            left_peaks = self.bioImg.info['peaks']['left'] if 'left' in self.bioImg.info['peaks'] else 0
            right_peaks = self.bioImg.info['peaks']['right'] if 'right' in self.bioImg.info['peaks'] else 0
            num_peaks = max(len(left_peaks), len(right_peaks))
            num, ok = QInputDialog.getInt(self, "Peak Number", "Please provide peak number between 1 and " + str(num_peaks))
            if ok:
                if num > num_peaks or num<1:
                    msg = QMessageBox()
                    msg.setInformativeText(
                        "Please provide peak number between 1 and " + str(num_peaks))
                    msg.setStandardButtons(QMessageBox.Ok)
                    msg.setWindowTitle("Incorrect Peak Number")
                    msg.setStyleSheet("QLabel{min-width: 500px;}")
                    msg.exec_()
                else:
                    ind = self.parameterEditorTable.rowCount()
                    self.parameterEditorTable.insertRow(ind)

                    self.parameterEditorTable.setItem(ind, 1, QTableWidgetItem('Speak' + str(num)))

                    valueItem = QDoubleSpinBox(self.parameterEditorTable)
                    valueItem.setDecimals(6)
                    valueItem.setRange(float('-inf'), 100000000000000000000)
                    valueItem.setValue(0)
                    self.parameterEditorTable.setCellWidget(ind, 2, valueItem)

                    valueItem = QDoubleSpinBox(self.parameterEditorTable)
                    valueItem.setDecimals(6)
                    valueItem.setRange(float('-inf'), 100000000000000000000)
                    valueItem.setValue(-1)
                    self.parameterEditorTable.setCellWidget(ind, 3, valueItem)

                    valueItem = QDoubleSpinBox(self.parameterEditorTable)
                    valueItem.setDecimals(6)
                    valueItem.setRange(float('-inf'), 100000000000000000000)
                    valueItem.setValue(1)
                    self.parameterEditorTable.setCellWidget(ind, 4, valueItem)
                print(num)

    def enableExtraGauss(self):
        '''
        Function to enable extra gaussian
        :return:
        '''
        paramInfo = self.bioImg.info['paramInfo']
        if 'extraGaussCenter' in paramInfo:
            for p in ['extraGaussCenter', 'extraGaussArea', 'extraGaussSig']:
                paramInfo[p]['fixed'] = True
                paramInfo[p]['val'] = 0.0
                paramInfo[p]['min'] = 0.0
                paramInfo[p]['max'] = 10.0
        self.updateParameterEditorTab()

    def refitParamEditor(self):
        '''
        Function to refit parameter editor changes
        :return:
        '''
        if self.bioImg is None:
            return
        QApplication.setOverrideCursor(Qt.WaitCursor)
        QApplication.processEvents()
        paramInfo = self.getInfoFromParameterEditor()
        try:
            self.bioImg.processParameters(paramInfo)
        except Exception as e:
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

        self.updateParams()
        self.csvManager.writeNewData(self.bioImg)
        self.csvManager2.writeNewData(self.bioImg)
        self.resetUI()
        self.refreshStatusbar()
        QApplication.restoreOverrideCursor()

    def init_logging(self):
        for objName in self.editableVars:
            self.editableVars[objName] = self.findChild(QAbstractSpinBox, objName).value()
        self.editableVars['int_area'] = self.bioImg.info['int_area']
        self.editableVars['center'] = self.bioImg.info['center']
        self.left_fitting_tab.init_logging()
        self.right_fitting_tab.init_logging()
        self.calibSettingDialog.init_logging()
        #print(self.editableVars)

    def statusPrint(self, text):
        self.statusReport.setText(text)
        QApplication.processEvents()

    def write_log(self, msg):
        if self.logger is None:
            self.logger = logger.Logger('equator', self.bioImg.dir_path)
        img_name = os.path.join(os.path.split(self.bioImg.dir_path)[-1], self.bioImg.filename)
        self.logger.write('[%s] %s' % (img_name, msg))

    def log_changes(self, name, obj=None, varName='', newValue=None):
        if obj is not None:
            varName = obj.objectName()
            newValue = obj.value()
        if self.editableVars[varName] == newValue:
            return
        self.write_log('{0}Changed: {1} -> {2}'.format(name, self.editableVars[varName], newValue))
        self.editableVars[varName] = newValue
