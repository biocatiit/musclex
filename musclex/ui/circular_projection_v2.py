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

__author__ = 'Miguel Menendez, Jiranun.J'

from pyqt_utils import *
import sys
from os.path import isfile, abspath
from matplotlib.patches import Ellipse
import matplotlib.patches as patches
from matplotlib.collections import PatchCollection
import h5py
import logging
import argparse
from ..utils.file_manager import *
from ..modules.CircularProjection import *
from ..CalibrationSettings import CalibrationSettings
from ..csv_manager import CP_CSVManager
import musclex
import pandas as pd
import BlankImageSettings as BI

class CPImageWindow(QMainWindow):
    def __init__(self, mainWin = None, image_name = "", dir_path = "", process_folder = False, imgList = None):
        # QDialog.__init__(self, parent)
        QWidget.__init__(self)
        self.setWindowTitle(image_name)
        self.fileName = image_name
        self.filePath = dir_path
        self.csvManager = CP_CSVManager(dir_path)
        self.imgList = []
        self.numberOfFiles = 0
        self.currentFileNumber = 0
        self.img_zoom = [0, 0, 0, 0]
        self.currentImgSize = (0, 0)
        self.cirProj = None
        self.calSettings = None
        self.mask = None
        self.function = None
        self.checkable_buttons = []
        self.fixed_hull_range = None

        self.m1_selected_range = 0
        self.update_plot = {'m1_partial_hist': True,
                            'm1_hist': True,
                            'm2_diff': True,
                            'image_result': True,
                            'results_text': True
                            }
        self.intesityRange = [0, 1, 1, 2]
        self.mainWin = mainWin
        self.logger = None

        self.generateRingColors()
        self.initUI()
        self.setConnections()
        self.setCalibrationImage()
        self.onNewFileSelected(imgList)
        if process_folder and len(self.imgList) > 0:
            self.processFolder()
        elif len(self.imgList) > 0:
            self.onImageChanged()

    def generateRingColors(self):
        possible_vals = [0, 255]
        self.ring_colors = []
        for b in possible_vals:
            for g in possible_vals:
                for r in possible_vals:
                    if b==0 and g==0 and r==0:
                        continue
                    self.ring_colors.append([b,g,r])

    def initUI(self):
        self.setWindowTitle("Circular Projection v." + musclex.__version__)
        self.setStyleSheet(getStyleSheet())
        self.centralWidget = QWidget(self)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        ### Image mode tabs
        ## IMAGE MODE 1 : Image tab
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.displayedImgLayout = QHBoxLayout()
        self.displayedImgLayout.setAlignment(Qt.AlignCenter)
        self.displayImgFigure = plt.figure(facecolor='#606060')
        self.displayImgAxes = self.displayImgFigure.add_subplot(111)
        self.displayImgCanvas = FigureCanvas(self.displayImgFigure)
        self.imageOptionsFrame = QFrame()
        self.imageOptionsFrame.setFixedWidth(300)
        self.imageOptionsLayout = QVBoxLayout()
        self.imageOptionsLayout.setAlignment(Qt.AlignTop)
        self.centerChkbx = QCheckBox("Display Center")
        self.displayRingsChkbx = QCheckBox("Display Rings")
        self.displayRingsChkbx.setChecked(True)
        self.intensityGrp = QGroupBox()
        self.intensityGrp.setTitle("Image Intensity")
        self.intensityGrp.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Maximum)
        self.intensityLayout = QGridLayout()
        self.intensityGrp.setLayout(self.intensityLayout)
        self.maxInt = QDoubleSpinBox()
        self.maxInt.setValue(1)
        self.maxInt.setKeyboardTracking(False)
        self.minInt = QDoubleSpinBox()
        self.minInt.setValue(0)
        self.minInt.setKeyboardTracking(False)
        self.minIntLabel = QLabel("Min intensity")
        self.maxIntLabel = QLabel("Max intensity")
        self.intensityLayout.addWidget(self.minIntLabel, 0, 0)
        self.intensityLayout.addWidget(self.minInt, 0, 1)
        self.intensityLayout.addWidget(self.maxIntLabel, 1, 0)
        self.intensityLayout.addWidget(self.maxInt, 1, 1)
        self.noBGImgChkBx = QCheckBox("Backgound Subtracted Image")
        self.blankChkBx = QCheckBox("Subtract with Blank Image")
        self.blankChkBx.setChecked(True)
        self.noBGImgSpinbx = QSpinBox()
        self.noBGImgSpinbx.setMinimum(3)
        self.noBGImgSpinbx.setMaximum(100)
        self.noBGImgSpinbx.setValue(3)
        self.noBGImgSpinbx.setEnabled(False)
        self.angleChkBx = QCheckBox("Display Angle lines")
        self.angleChkBx.setChecked(True)
        self.rminmaxChkBx = QCheckBox("Display R-min and R-max")
        self.rminmaxChkBx.setChecked(False)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet(pfss)
        self.pnButtons = QGridLayout()
        self.prevButton = QPushButton('<')
        self.prevButton.clearFocus()
        self.nextButton = QPushButton('>')
        self.pnButtons.addWidget(self.processFolderButton, 0, 0, 1, 2)
        self.pnButtons.addWidget(self.prevButton, 1, 0, 1, 1)
        self.pnButtons.addWidget(self.nextButton, 1, 1, 1, 1)

        self.displayOptionGrp = QGroupBox()
        self.displayOptionGrp.setTitle('Display Options')
        self.displayOptionsLayout = QVBoxLayout()
        self.displayOptionsLayout.addSpacing(15)
        self.displayOptionsLayout.addWidget(self.intensityGrp)
        self.displayOptionsLayout.addSpacing(10)
        self.displayOptionsLayout.addWidget(self.centerChkbx)
        self.displayOptionsLayout.addSpacing(10)
        self.displayOptionsLayout.addWidget(self.displayRingsChkbx)
        self.displayOptionsLayout.addSpacing(10)
        # self.displayOptionsLayout.addWidget(self.noBGImgChkBx)
        # self.displayOptionsLayout.addSpacing(10)
        self.displayOptionsLayout.addWidget(self.blankChkBx)
        self.displayOptionsLayout.addSpacing(10)
        self.displayOptionsLayout.addWidget(self.angleChkBx)
        self.displayOptionsLayout.addSpacing(10)
        self.displayOptionsLayout.addWidget(self.rminmaxChkBx)
        self.displayOptionGrp.setLayout(self.displayOptionsLayout)

        self.setCaliButton = QPushButton("Calibration Settings")
        self.setBlankImageButton = QPushButton("Blank Image and Mask")
        self.setHullRange = QPushButton("Set Fixed R-min and R-max")
        self.setHullRange.setCheckable(True)
        self.checkable_buttons.append(self.setHullRange)
        self.selectRings = QPushButton("Select Rings Manually")
        self.selectRings.setCheckable(True)
        self.checkable_buttons.append(self.selectRings)

        self.settingGrp = QGroupBox("Settings")
        self.settingLayout = QVBoxLayout(self.settingGrp)
        self.settingLayout.addWidget(self.setCaliButton)
        self.settingLayout.addWidget(self.setBlankImageButton)
        self.settingLayout.addWidget(self.setHullRange)
        self.settingLayout.addWidget(self.selectRings)

        self.imageOptionsLayout.addWidget(self.settingGrp)
        self.imageOptionsLayout.addSpacing(10)
        self.imageOptionsLayout.addWidget(self.displayOptionGrp)
        self.imageOptionsLayout.addStretch()
        self.imageOptionsLayout.addLayout(self.pnButtons)
        self.imageOptionsLayout.addSpacing(10)
        self.imageOptionsFrame.setLayout(self.imageOptionsLayout)

        self.imageTabLayout.addWidget(self.displayImgCanvas)
        self.imageTabLayout.addWidget(self.imageOptionsFrame)

        ## IMAGE MODE 2 : Method 1 Tab Multiple conical integrations
        self.method1Tab = QWidget()
        self.method1Tab.setContentsMargins(0, 0, 0, 0)
        self.method1TabLayout = QGridLayout(self.method1Tab)
        self.m1_key_group = QGroupBox()
        # self.m1_key_group.setTitle()
        self.m1_keys_layout = QGridLayout(self.m1_key_group)
        self.skipFirstPeakChkBx = QCheckBox()
        self.skipFirstPeakChkBx.setText("Zoom")
        self.skipFirstPeakChkBx.setChecked(False)

        self.m1OriginalHistChkbx = QCheckBox()
        self.m1OriginalHistChkbx.setText("Original Histogram")
        self.m1OriginalHistChkbx.setChecked(False)

        self.partialRange = QSpinBox()
        self.partialRange.setMinimum(30)
        self.partialRange.setMaximum(180)
        self.partialRange.setSingleStep(30)
        self.partialRange.setValue(90)
        self.partialRange.setKeyboardTracking(False)
        self.next_range = QPushButton('>>')
        self.prev_range = QPushButton('<<')

        self.m1_keys_layout.addWidget(self.skipFirstPeakChkBx, 0, 0, 1, 1)
        self.m1_keys_layout.addWidget(self.m1OriginalHistChkbx, 0, 1, 1, 1)
        self.m1_keys_layout.addWidget(QLabel('Range Angle (degree) : '), 1, 0, 1, 1)
        self.m1_keys_layout.addWidget(self.partialRange, 1, 1, 1, 1)
        change_label = QLabel("Change Range")
        self.m1_keys_layout.addWidget(change_label, 0, 2, 1, 2)
        self.m1_keys_layout.setAlignment(change_label, Qt.AlignCenter)
        self.m1_keys_layout.addWidget(self.prev_range, 1, 2, 1, 1)
        self.m1_keys_layout.addWidget(self.next_range, 1, 3, 1, 1)
        self.m1_keys_layout.setAlignment(Qt.AlignTop)
        self.m1_key_group.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Maximum)
        self.m1_key_group.setAlignment(Qt.AlignTop)

        self.m1_partial_hist_figure = plt.figure(facecolor='#606060')
        self.m1_partial_hist_figure.subplots_adjust(top=0.90, bottom=0.20)
        self.m1_partial_hist_axes = self.m1_partial_hist_figure.add_subplot(111)
        self.m1_partial_hist_canvas = FigureCanvas(self.m1_partial_hist_figure)
        # self.toolbar = NavigationToolbar(self.m1_partial_hist_canvas, self)
        # self.method1TabLayout.addWidget(self.toolbar)
        self.m1_hist_figure = plt.figure(facecolor='#606060')
        self.m1_hist_axes = self.m1_hist_figure.add_subplot(111)
        self.m1_hist_canvas = FigureCanvas(self.m1_hist_figure)
        self.m1_img_fig = plt.figure(facecolor='#606060')
        self.m1_img_axes = self.m1_img_fig.add_subplot(111)
        self.m1_img_canvas = FigureCanvas(self.m1_img_fig)
        self.method1TabLayout.addWidget(self.m1_img_canvas, 0, 0, 3, 1)
        self.method1TabLayout.addWidget(self.m1_partial_hist_canvas, 0, 1)
        self.method1TabLayout.addWidget(self.m1_key_group, 1, 1)
        self.method1TabLayout.addWidget(self.m1_hist_canvas, 2, 1)

        ## IMAGE MODE 3 : Method 2 Tab
        self.method2Tab = QWidget()
        self.method2Tab.setContentsMargins(0, 0, 0, 0)
        self.method2Tablayout = QGridLayout(self.method2Tab)
        self.method2ComboBx = QComboBox()
        self.method2ComboBx.addItem("2D Integration")
        self.method2ComboBx.addItem("Central Differences")
        self.method2ComboBx.addItem("Log Central Differences")
        self.method2ComboBx.setCurrentIndex(2)
        self.runsChkBx = QCheckBox("Display Runs")
        self.ringsChkBx = QCheckBox("Display Rings")

        self.m2_cent_diff_fig = plt.figure(facecolor='#606060')
        self.m2_cent_diff_axes = self.m2_cent_diff_fig.add_subplot(111)
        self.m2_cent_diff_canvas = FigureCanvas(self.m2_cent_diff_fig)
        self.method2Tablayout.addWidget(self.method2ComboBx, 0, 0, 1, 1)
        self.method2Tablayout.addWidget(self.runsChkBx, 0, 1, 1, 1)
        # self.method2Tablayout.setAlignment(self.runsChkBx, Qt.AlignCenter)
        self.method2Tablayout.addWidget(self.ringsChkBx, 0, 2, 1, 1)
        # self.method2Tablayout.setAlignment(self.ringsChkBx, Qt.AlignCenter)
        self.method2Tablayout.addWidget(self.m2_cent_diff_canvas, 1, 0, 1, 3)

        ## IMAGE MODE 4 : Result Tab
        self.resultTab = QWidget()
        self.resultTab.setContentsMargins(0, 0, 0, 0)
        self.resultTabLayout = QGridLayout(self.resultTab)
        self.graph_cmbbx = QComboBox()
        self.graph_cmbbx.addItem("2D Integration and Fitting Information")
        self.graph_cmbbx.addItem("Angle distribution")
        self.graph_cmbbx.setCurrentIndex(0)

        self.skip_first_peak_chkbx = QCheckBox("Zoom")
        self.skip_first_peak_chkbx.setChecked(False)
        self.original_hist_chkbx = QCheckBox("Original Histogram")
        self.original_hist_chkbx.setChecked(False)
        self.hull_hist_chkbx = QCheckBox("No Background Histogram")
        self.hull_hist_chkbx.setChecked(True)
        self.fit_hist_chkbx = QCheckBox("Fit Model")
        self.fit_hist_chkbx.setChecked(True)
        self.selectPeaks = QPushButton("Select Peaks Manually")
        self.selectPeaks.setCheckable(True)
        self.checkable_buttons.append(self.selectPeaks)
        self.rings_chkbx = QCheckBox("Model Peaks")
        self.rings_chkbx.setChecked(True)
        self.ring_hists_chkbx = QCheckBox("All Rings")
        self.ring_hists_chkbx.setChecked(True)
        self.ring_hists_chkbx.setHidden(True)
        self.average_ring_chkbx = QCheckBox("Average Model")
        self.average_ring_chkbx.setChecked(False)
        self.average_ring_chkbx.setHidden(True)
        self.g_model_chkbx = QCheckBox("Gaussian Models")
        self.g_model_chkbx.setChecked(True)
        self.g_model_chkbx.setHidden(True)
        self.graph_options_frame = QFrame()
        self.graph_options_layout = QVBoxLayout()
        self.graph_options_layout.addWidget(self.selectPeaks)
        self.graph_options_layout.addWidget(self.skip_first_peak_chkbx)
        self.graph_options_layout.addWidget(self.original_hist_chkbx)
        self.graph_options_layout.addWidget(self.hull_hist_chkbx)
        self.graph_options_layout.addWidget(self.rings_chkbx)
        self.graph_options_layout.addWidget(self.fit_hist_chkbx)
        self.graph_options_layout.addWidget(self.ring_hists_chkbx)
        self.graph_options_layout.addWidget(self.g_model_chkbx)
        self.graph_options_layout.addWidget(self.average_ring_chkbx)
        self.graph_options_frame.setLayout(self.graph_options_layout)

        # self.graph_options_frame.setFixedWidth(200)
        self.result_graph_figure = plt.figure(facecolor='#606060')
        self.result_graph_axes = self.result_graph_figure.add_subplot(111)
        self.result_graph_canvas = FigureCanvas(self.result_graph_figure)
        self.processing_results = QTextEdit()
        self.processing_results.setReadOnly(True)
        self.processing_results.setText("Angular results : N/A")
        self.rings_results = QTextEdit()
        self.rings_results.setReadOnly(True)
        # self.rings_results.setLineWidth(2)
        self.rings_results.setText("Rings Information : N/A")
        self.resultTabLayout.addWidget(self.graph_cmbbx, 0, 0, 1, 4)
        self.resultTabLayout.addWidget(self.result_graph_canvas, 1, 0, 1, 3)
        self.resultTabLayout.addWidget(self.graph_options_frame, 1, 3, 1, 1)
        self.resultTabLayout.addWidget(self.processing_results, 2, 0, 1, 2)
        self.resultTabLayout.addWidget(self.rings_results, 2, 2, 1, 2)
        self.resultTabLayout.setRowStretch(0, 2)
        self.resultTabLayout.setRowStretch(1, 1)
        self.resultTabLayout.setColumnStretch(0, 1)
        self.resultTabLayout.setColumnStretch(1, 1)
        self.resultTabLayout.setColumnStretch(2, 1)
        self.resultTabLayout.setColumnStretch(3, 1)

        ## tabs
        self.tabWidget = QTabWidget()
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 300px; }")
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)

        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 300px; }")
        self.tabWidget.addTab(self.imageTab, "Image")
        self.tabWidget.addTab(self.method1Tab, "Method 1\nMultiple Conical Integration")
        self.tabWidget.addTab(self.method2Tab, "Method 2\nRuns on Central Difference Image")
        self.tabWidget.addTab(self.resultTab, "Results")
        self.mainLayout.addWidget(self.tabWidget)

        # status bar
        self.statusBar = QStatusBar()
        self.imagePathLabel = QLabel('')
        self.statusLabel = QLabel('')
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusBar.addWidget(QLabel('   '))
        self.statusBar.addWidget(self.imagePathLabel)
        self.statusBar.addPermanentWidget(self.statusLabel)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.mainLayout.addWidget(self.statusBar)

        self.show()

    def setConnections(self):

        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)

        self.centerChkbx.stateChanged.connect(self.updateUI)
        self.displayRingsChkbx.stateChanged.connect(self.updateUI)
        self.maxInt.valueChanged.connect(self.maxIntChanged)
        self.minInt.valueChanged.connect(self.minIntChanged)
        self.noBGImgChkBx.stateChanged.connect(self.updateUI)
        self.blankChkBx.stateChanged.connect(self.updateUI)
        self.angleChkBx.stateChanged.connect(self.updateUI)
        self.rminmaxChkBx.stateChanged.connect(self.updateUI)
        self.setCaliButton.clicked.connect(self.calibrationClicked)
        self.setBlankImageButton.clicked.connect(self.setBlankAndMask)
        self.setHullRange.clicked.connect(self.setHullRangeClicked)
        self.selectRings.clicked.connect(self.selectRingsClicked)
        self.processFolderButton.clicked.connect(self.processFolder)
        self.prevButton.clicked.connect(self.prevImage)
        self.nextButton.clicked.connect(self.nextImage)

        self.skipFirstPeakChkBx.stateChanged.connect(self.m1_update_plots)
        self.m1OriginalHistChkbx.stateChanged.connect(self.m1_update_plots)
        self.partialRange.valueChanged.connect(self.partialRangeChanged)
        self.next_range.clicked.connect(self.next_range_pushed)
        self.prev_range.clicked.connect(self.prev_range_pushed)
        self.runsChkBx.stateChanged.connect(self.refreshMethod2Tab)
        self.ringsChkBx.stateChanged.connect(self.refreshMethod2Tab)
        self.method2ComboBx.currentIndexChanged.connect(self.refreshMethod2Tab)
        self.graph_cmbbx.currentIndexChanged.connect(self.updateUI)

        self.result_graph_figure.canvas.mpl_connect('button_press_event', self.result_graph_clicked)
        self.selectPeaks.clicked.connect(self.selectPeaksClicked)
        self.skip_first_peak_chkbx.stateChanged.connect(self.updateUI)
        self.original_hist_chkbx.stateChanged.connect(self.updateUI)
        self.hull_hist_chkbx.stateChanged.connect(self.updateUI)
        self.fit_hist_chkbx.stateChanged.connect(self.updateUI)
        self.rings_chkbx.stateChanged.connect(self.updateUI)
        self.ring_hists_chkbx.stateChanged.connect(self.updateUI)
        self.average_ring_chkbx.stateChanged.connect(self.updateUI)
        self.g_model_chkbx.stateChanged.connect(self.updateUI)

        self.tabWidget.currentChanged.connect(self.updateUI)

    def calibrationClicked(self):
        """
        Triggered when calibration settings button pressed
        """
        success = self.setCalibrationImage(force=True)
        if self.cirProj is not None and success:
            self.cirProj.removeInfo()
            self.processImage()

    def setBlankAndMask(self):
        dialog = BI.BlankImageSettings(self.filePath)
        self.mask = None
        result = dialog.exec_()
        if result == 1 and self.cirProj is not None:
            self.cirProj.removeInfo('2dintegration')
            self.processImage()

    def selectPeaksClicked(self):
        """
        Triggered when select peaks manually button pressed (Result tab)
        """
        if self.cirProj is None:
            return

        if self.selectPeaks.isChecked():
            self.function = ["peaks"]
            self.selectPeaks.setText("Done")
            ax = self.result_graph_axes
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            hull = self.cirProj.info['hull_hist']
            ax.plot(hull)
            self.result_graph_canvas.draw_idle()
        else:
            self.cirProj.info['merged_peaks'] = sorted(self.function[1:])
            self.selectPeaks.setText("Select Peaks Manually")
            self.cirProj.removeInfo('fitResult')
            self.function = None
            self.processImage()

    def setHullRangeClicked(self):
        """
        Triggered when select R-min and R-max button pressed (Image tab)
        """
        if self.cirProj is None:
            return

        if self.setHullRange.isChecked():
            self.function = ['hull']
            ax = self.displayImgAxes
            ax.lines = []
            ax.patches = []
            self.displayImgCanvas.draw_idle()
        else:
            self.function = None
            self.updateImageTab()

    def selectRingsClicked(self):
        """
        Triggered when select rings manually button pressed (Image tab)
        """
        if self.cirProj is None:
            return

        if self.selectRings.isChecked():
            self.function = ["rings"]
            self.selectRings.setText("Done")
            ax = self.displayImgAxes
            ax.lines = []
            ax.patches = []
            self.displayImgCanvas.draw_idle()
        else:
            self.cirProj.info['merged_peaks'] = sorted(self.function[1:])
            self.selectRings.setText("Select Rings Manually")
            self.cirProj.removeInfo('fitResult')
            self.function = None
            self.processImage()

    def result_graph_clicked(self, event):
        """
        Triggered when mouse presses on graph in result tab
        """
        if self.cirProj is None:
            return

        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
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

        if self.function is None:
            return

        func = self.function

        # Provide different behavior depending on current active function
        if func[0] == "peaks":
            # Add rings to list and draw circles
            ax = self.result_graph_axes
            ax.axvline(x, color='b')
            ax.text(x,0,'Peak#'+str(len(self.function)),fontsize=10, horizontalalignment='center')
            self.function.append(int(round(x)))
            self.result_graph_canvas.draw_idle()

    def imgClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if self.cirProj is None:
            return

        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            ax = self.displayImgAxes
            bounds = ax.get_window_extent().get_points() ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[0] - ylim[1]) / (bounds[0][1] - bounds[1][1]) ### todo
            cy = ylim[1] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])

        if self.function is None:
            return

        func = self.function

        # Provide different behavior depending on current active function
        if func[0] == "rings":
            # Add rings to list and draw circles
            center = self.cirProj.info['center']
            dis = distance(center, (x,y))
            ax = self.displayImgAxes
            ax.add_patch(
                patches.Circle(tuple(center), dis, linewidth=2, edgecolor='y', facecolor='none'))
            self.function.append(int(round(dis)))
            self.displayImgCanvas.draw_idle()
        elif func[0] == "hull":
            center = self.cirProj.info['center']
            dis = distance(center, (x, y))
            self.function.append(int(round(dis)))
            if len(self.function) == 3:
                rs = self.function[1:]
                rmin = min(rs)
                rmax = max(rs)
                self.fixed_hull_range = (rmin, rmax)
                self.cirProj.removeInfo('2dintegration')
                self.processImage()
            else:
                ax = self.displayImgAxes
                ax.add_patch(
                    patches.Circle(tuple(center), dis, linewidth=2, edgecolor='y', facecolor='none'))
                self.displayImgCanvas.draw_idle()

    def imgOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """
        if self.cirProj is None:
            return

        x = event.xdata
        y = event.ydata

        if x is not None and y is not None:
            orig_img = self.cirProj.original_image
            ix = int(round(x))
            iy = int(round(y))
            self.statusLabel.setText('x='+str(round(x,2))+', y='+str(round(y,2))+', val='+str(round(orig_img[iy,ix],2)))

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.statusLabel.setText('')
            ax = self.displayImgAxes
            bounds = ax.get_window_extent().get_points() ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[0] - ylim[1]) / (bounds[0][1] - bounds[1][1]) ### todo
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

        if func[0] == "rings":
            # draw circle
            ax = self.displayImgAxes
            patch_list = ax.patches[0:len(self.function)-1]
            del ax.patches
            ax.patches = patch_list
            center = self.cirProj.info['center']
            dis = distance(center, (x, y))
            ax = self.displayImgAxes
            ax.add_patch(
                patches.Circle(tuple(center), dis, linewidth=2, edgecolor='y', facecolor='none'))
            self.displayImgCanvas.draw_idle()
        if func[0] == 'hull':
            # draw circle
            ax = self.displayImgAxes
            patch_list = ax.patches[0:len(self.function) - 1]
            ax.patches = patch_list
            center = self.cirProj.info['center']
            dis = distance(center, (x, y))
            ax = self.displayImgAxes
            ax.add_patch(
                patches.Circle(tuple(center), dis, linewidth=2, edgecolor='y', facecolor='none'))
            self.displayImgCanvas.draw_idle()

    def setCalibrationImage(self, force = False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or calibration.tif in the folder
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        settingDialog = CalibrationSettings(self.filePath)
        self.calSettings = None
        cal_setting = settingDialog.calSettings
        if cal_setting is not None or force:
            result = settingDialog.exec_()
            if result == 1:
                self.calSettings = settingDialog.getValues()
                return True
        return False

    def processFolder(self):
        """
        Process current folder
        """
        ## Popup confirm dialog with settings
        nImg = len(self.imgList)
        errMsg = QMessageBox()
        errMsg.setText('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'
        flags = self.getFlags()
        text += "\nCurrent Settings"
        text += "\n - Partial integration angle range : "+ str(flags['partial_angle'])
        if self.calSettings is not None:
            if self.calSettings.has_key("center"):
                text += "\n  - Calibration Center : " + str(self.calSettings["center"])
            if self.calSettings["type"] == "img":
                text += "\n  - Silver Behenate : " + str(self.calSettings["silverB"]) +" nm"
                text += "\n  - Sdd : " + str(self.calSettings["radius"]) + " pixels"
            else:
                text += "\n  - Lambda : " + str(self.calSettings["lambda"]) +" nm"
                text += "\n  - Sdd : " + str(self.calSettings["sdd"]) + " mm"
                text += "\n  - Pixel Size : " + str(self.calSettings["pixel_size"]) + " nm"
        text += '\n\nAre you sure you want to process ' + str(nImg) + ' image(s) in this Folder? \nThis might take a long time.'
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

            log_path = fullPath(self.filePath, 'log')
            if not exists(log_path):
                os.makedirs(log_path)

            current = time.localtime()
            filename = "CirProj_""%02d" % current.tm_year + "%02d" % current.tm_mon + "%02d" % current.tm_mday + \
                       "_" + "%02d" % current.tm_hour + "%02d" % current.tm_min + "%02d" % current.tm_sec + ".log"
            filename = fullPath(log_path, filename)
            self.logger = logging.getLogger('cp')
            self.logger.setLevel(logging.DEBUG)
            self.logger.propagate = False

            # create a file handler
            handler = logging.FileHandler(filename)
            handler.setLevel(logging.DEBUG)

            # create a logging format
            formatter = logging.Formatter('%(asctime)s: %(message)s')
            handler.setFormatter(formatter)

            # add the handlers to the self.logger
            self.logger.addHandler(handler)
            self.logger.addFilter(logging.Filter(name='cp'))

            ## Process all images and update progress bar
            for i in range(nImg):
                self.nextImage()
                self.progressBar.setValue(i)
                QApplication.processEvents()
            self.folder_processed = True
        else:
            self.folder_processed = False

        self.progressBar.setVisible(False)

    def keyPressEvent(self, event):
        key = event.key()

        if key == Qt.Key_Right:
            self.nextImage()
        elif key == Qt.Key_Left:
            self.prevImage()
        elif key == Qt.Key_Escape:
            self.refreshAllTabs()

    def closeEvent(self, ev):
        if self.mainWin is not None:
            self.mainWin.removeWidget(self)

    def getFlags(self):
        flags = {}
        flags['partial_angle'] = self.partialRange.value()

        if self.calSettings is not None:
            if self.calSettings["type"] == "img":
                flags["center"] = self.calSettings["center"]
                flags["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
            else:
                flags["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings["pixel_size"]
                if self.calSettings.has_key("center"):
                    flags["center"] = self.calSettings["center"]

        if self.fixed_hull_range is not None:
            flags['fixed_hull'] = self.fixed_hull_range

        return flags

    def maxIntChanged(self):
        if self.cirProj is not None and not self.updatingUI:
            if self.maxInt.value() < self.minInt.value():
                self.maxInt.setValue(self.minInt.value()+1)
            self.update_plot['m1_partial_hist'] = True
            self.updateUI()

    def minIntChanged(self):
        if self.cirProj is not None and not self.updatingUI:
            if self.maxInt.value() < self.minInt.value():
                self.minInt.setValue(self.maxInt.value()-1)
            self.update_plot['m1_partial_hist'] = True
            self.updateUI()

    def onNewFileSelected(self, imgList):
        self.resize(600, 600)

        if imgList is not None:
            self.imgList = imgList
        else:
            self.imgList, _ = getFilesAndHdf(self.filePath)

        self.imgList.sort()
        self.numberOfFiles = len(self.imgList)
        if len(self.fileName) > 0:
            self.currentFileNumber = self.imgList.index(self.fileName)
        else:
            self.currentFileNumber = 0

    def onImageChanged(self):
        fileName = self.imgList[self.currentFileNumber]
        fileFullPath = fullPath(self.filePath, fileName)
        self.updateStatusBar(fileFullPath+' ('+str(self.currentFileNumber+1)+'/'+str(self.numberOfFiles)+') is processing ...')
        self.cirProj = CircularProjection(self.filePath, fileName, logger=self.logger)
        self.setMinMaxIntensity(self.cirProj.original_image, self.minInt, self.maxInt, self.minIntLabel, self.maxIntLabel)
        self.processImage()
        self.updateStatusBar(fileFullPath + ' (' + str(self.currentFileNumber + 1) + '/' + str(
            self.numberOfFiles) + ') is processed.')

    def processImage(self):
        if self.cirProj is not None:
            QApplication.setOverrideCursor(Qt.WaitCursor)
            flags = self.getFlags()
            self.cirProj.process(flags)
            QApplication.restoreOverrideCursor()
            self.csvManager.write_new_data(self.cirProj)
            self.refreshAllTabs()
            self.updateUI()

    def setMinMaxIntensity(self, img, minInt, maxInt, minIntLabel, maxIntLabel):
        min_val = img.min()
        max_val = img.max()
        self.intensityRange = [min_val, max_val-1, min_val+1, max_val]
        minInt.setMinimum(self.intensityRange[0])
        minInt.setMaximum(self.intensityRange[1])
        maxInt.setMinimum(self.intensityRange[2])
        maxInt.setMaximum(self.intensityRange[3])
        step = max(1., (max_val-min_val)/100)
        minInt.setSingleStep(step)
        maxInt.setSingleStep(step)
        minIntLabel.setText("Min intensity (" + str(min_val) + ")")
        maxIntLabel.setText("Max intensity (" + str(max_val) + ")")

        if img.dtype == 'float32':
            decimal = 2
        else:
            decimal = 0

        maxInt.setDecimals(decimal)
        minInt.setDecimals(decimal)

        if maxInt.value() == 1. and minInt.value() == 0.:
            self.updatingUI = True
            minInt.setValue(min_val)
            maxInt.setValue(max_val*0.1)
            self.updatingUI = False

    def updateStatusBar(self, text):
        QApplication.processEvents()
        self.imagePathLabel.setText(text)
        QApplication.processEvents()

    def updateUI(self):
        if self.cirProj is None:
            return

        selected_tab = self.tabWidget.currentIndex()

        if selected_tab == 0:
            self.updateImageTab()
        elif selected_tab == 1:
            self.updateMethod1Tab()
        elif selected_tab == 2:
            self.updateMethod2Tab()
        elif selected_tab == 3:
            self.updateResultsTab()

    def partialRangeChanged(self):
        if self.updatingUI or self.cirProj is None:
            return
        self.cirProj.info['partial_angle'] = self.partialRange.value()
        self.cirProj.removeInfo('m1_rings')
        self.processImage()

    def prevImage(self):
        self.currentFileNumber = (self.currentFileNumber - 1) % self.numberOfFiles
        self.onImageChanged()

    def nextImage(self):
        self.currentFileNumber = (self.currentFileNumber + 1) % self.numberOfFiles
        self.onImageChanged()

    def zoomFigure(self, figure, canvas, direction, x, y):
        if self.cirProj is None:
            return

        #
        # display_size = figure.get_size_inches() * figure.dpi
        # display_height = display_size[0]
        # display_width = display_size[1]
        # ax = figure.add_subplot(111)
        # original_height = ax.dataLim.height
        # original_width = ax.dataLim.width
        # current_xlim = ax.get_xlim()
        # current_ylim = ax.get_ylim()
        # current_y_size = max(current_ylim) - min(current_ylim)
        # current_x_size = max(current_xlim) - min(current_xlim)
        # ratioY = float(display_height) / float(current_y_size)
        # ratioX = float(display_width) / float(current_x_size)
        # py = y / ratioY + min(current_ylim)
        # px = x / ratioX + min(current_xlim)
        # new_width = int(current_y_size * (1.0 - (direction * 0.1)))
        # new_height = int(current_x_size * (1.0 - (direction * 0.1)))
        # new_width = min(new_width, original_width)
        # new_height = min(new_height, original_height)
        # new_width = max(new_width, 50)
        # new_height = max(new_height, new_width * original_height / original_width)
        # clicked_x_percentage = x / float(display_width)
        # clicked_y_percentage = y / float(display_height)
        # new_xlim = (int(px - (clicked_x_percentage * new_width)), int(self.img_zoom[0] + new_width))
        # new_ylim = (int(py - (clicked_y_percentage * new_height)), int(self.img_zoom[1] + new_height))
        # ax.set_xlim(new_xlim)
        # ax.set_ylim(new_ylim)
        # canvas.draw()

    def wheelOnImg(self, ev):
        direction = ev.delta() / 120
        x = ev.pos().x()
        y = ev.pos().y()
        self.zoomFigure(self.displayImgFigure, self.displayImgCanvas, direction, x, y)

    def refreshMethod2Tab(self):
        self.update_plot['m2_diff'] = True
        self.updateUI()

    def refreshAllTabs(self):
        self.function = None
        for b in self.checkable_buttons:
            b.setChecked(False)
        for k in self.update_plot.keys():
            self.update_plot[k] = True

    def m1_update_plots(self):
        self.update_plot['m1_partial_hist'] = True
        self.update_plot['m1_hist'] = True
        self.updateUI()

    def next_range_pushed(self):
        self.m1_selected_range += 1
        self.update_plot['m1_partial_hist'] = True
        self.updateUI()

    def prev_range_pushed(self):
        self.m1_selected_range -= 1
        self.update_plot['m1_partial_hist'] = True
        self.updateUI()

    def getZoomedImage(self, img):
        if not any(self.img_zoom):
            h,w = img.shape[:2]
            self.img_zoom = [0,0,w,h]
        return img[ self.img_zoom[1]:self.img_zoom[3], self.img_zoom[0]:self.img_zoom[2]]

    def draw_angle_lines(self, img, center, angle, arange):
        scale = img.shape[1] / 2
        angle_line = [(int(center[0] - (scale * np.cos(angle))), int(center[1] - (scale * np.sin(angle)))),
                      (int(center[0] + (scale * np.cos(angle))), int(center[1] + (scale * np.sin(angle))))]
        range1 = [(int(center[0] - (scale * np.cos(arange[0]))), int(center[1] - (scale * np.sin(arange[0])))),
                  (int(center[0] + (scale * np.cos(arange[0]))), int(center[1] + (scale * np.sin(arange[0]))))]
        range2 = [(int(center[0] - (scale * np.cos(arange[1]))), int(center[1] - (scale * np.sin(arange[1])))),
                  (int(center[0] + (scale * np.cos(arange[1]))), int(center[1] + (scale * np.sin(arange[1]))))]

        cv2.line(img, angle_line[0], angle_line[1], (255, 0, 0), 5)
        cv2.line(img, range1[0], range1[1], (0, 255, 255), 5)
        cv2.line(img, range2[0], range2[1], (0, 255, 255), 5)

    def updateImageTab(self):
        img = copy.copy(self.cirProj.original_image)
        if self.blankChkBx.isChecked():
            blank, mask = getBlankImageAndMask(self.filePath)
            if blank is not None:
                img = img - blank

        img = getBGR(get8bitImage(img, min=self.minInt.value(), max=self.maxInt.value()))

        ax = self.displayImgAxes
        ax.cla()
        ax.imshow(img)

        center = (int(np.round(self.cirProj.info['center'][0])), int(np.round(self.cirProj.info['center'][1])))

        if self.displayRingsChkbx.isChecked() and 'fitResult' in self.cirProj.info.keys():
            fitResult = self.cirProj.info['fitResult']
            peaks = self.cirProj.info['model_peaks']
            num_peaks = len(peaks) + 1

            # TODO: Correction factor for sigma
            h = 2

            for i in range(1, num_peaks):
                radius = fitResult['u' + str(i)]
                sigmad = fitResult['sigmad' + str(i)]

                if radius - h * sigmad > 0:
                    ax.add_patch(
                        patches.Circle(tuple(center), int(round(radius - h*sigmad)), linewidth=2, edgecolor=tuple(np.array(self.ring_colors[(i-1)%len(self.ring_colors)])/255.), facecolor='none'))

                    ax.add_patch(
                        patches.Circle(tuple(center), int(round(radius + h*sigmad)), linewidth=2, edgecolor=tuple(np.array(self.ring_colors[(i-1)%len(self.ring_colors)])/255.), facecolor='none'))


        if self.cirProj.info.has_key('ring_models') and self.cirProj.info.has_key('ring_errors'):
            models = self.cirProj.info['ring_models']
            errors = self.cirProj.info['ring_errors']
            best_ind = min(errors.items(), key=lambda err:err[1])[0]
            model = models[best_ind]
            if model['sigma'] < 1. and errors[best_ind] < 1.:
                self.angleChkBx.setEnabled('average_ring_model' in self.cirProj.info.keys())
                if self.angleChkBx.isChecked():
                    # Draw angle lines
                    angle = model['u']
                    arange = (angle - model['sigma'], angle + model['sigma'])
                    scale = img.shape[1] / 2
                    angle_line = [
                        (int(round(center[0] - (scale * np.cos(angle)))), int(round(center[0] + (scale * np.cos(angle))))),
                         (int(round((center[1] - (scale * np.sin(angle))))), int(round((center[1] + (scale * np.sin(angle))))))]

                    range1 = [
                        (int(round(center[0] - (scale * np.cos(arange[0])))), int(round(center[0] + (scale * np.cos(arange[0]))))),
                        (int(round((center[1] - (scale * np.sin(arange[0]))))), int(round((center[1] + (scale * np.sin(arange[0]))))))]

                    range2 = [
                        (int(round(center[0] - (scale * np.cos(arange[1])))), int(round(center[0] + (scale * np.cos(arange[1]))))),
                        (int(round((center[1] - (scale * np.sin(arange[1]))))), int(round((center[1] + (scale * np.sin(arange[1]))))))]

                    ax.plot(angle_line[0], angle_line[1], color=(1,0,0))
                    ax.plot(range1[0], range1[1], color=(1,0.5,.5))
                    ax.plot(range2[0], range2[1], color=(1,0.5,.5))

        if self.centerChkbx.isChecked():
            ax.add_patch(
                patches.Circle(tuple(center), 3, linewidth=2, edgecolor='w', facecolor='r'))

        if self.rminmaxChkBx.isChecked():
            ax.add_patch(patches.Circle(tuple(center), self.cirProj.info['start_point'], linewidth=2, edgecolor='y',
                                        facecolor='none'))
            ax.add_patch(patches.Circle(tuple(center), self.cirProj.info['rmax'], linewidth=2, edgecolor='y',
                                        facecolor='none'))

        ax.set_ylim((0, img.shape[0]))
        ax.set_xlim((0, img.shape[1]))
        ax.invert_yaxis()
        self.displayImgFigure.tight_layout()
        self.displayImgCanvas.draw()

    def updateMethod1Tab(self):
        if 'm1_partial_hists' in self.cirProj.info.keys() and 'partial_ranges' in self.cirProj.info.keys() and \
                self.update_plot['m1_partial_hist']:
            partial_ranges = self.cirProj.info['partial_ranges']
            self.m1_selected_range %= len(partial_ranges)
            selected_range = partial_ranges[self.m1_selected_range]
            str_info = "Range : " + str(selected_range)
            hist = self.cirProj.info['m1_partial_hists'][self.m1_selected_range]
            hull = self.cirProj.info['m1_partial_hulls'][self.m1_selected_range]
            ax = self.m1_partial_hist_axes
            ax.cla()

            if self.m1OriginalHistChkbx.isChecked():
                ax.plot(hist, color='b')

            ax.plot(hull, color='g')

            if 'm1_partial_peaks' in self.cirProj.info.keys():
                peaks = self.cirProj.info['m1_partial_peaks'][self.m1_selected_range]
                str_info += "   Peaks : "+str(peaks)
                for p in peaks:
                    ax.plot([p, p], [0, max(hist)], color='r')

            end_plot = len(hist)
            start_plot = 0
            if self.skipFirstPeakChkBx.isChecked() and 'start_point' in self.cirProj.info.keys():

                if 'merged_peaks' in self.cirProj.info.keys() and len(self.cirProj.info['merged_peaks']) > 0:
                    merged_rings = self.cirProj.info['merged_peaks']
                    last_ring = max(merged_rings)
                    first_ring = min(merged_rings)
                    end_plot = int(last_ring * 1.4)
                    start_plot = int(first_ring * 0.4)

            if self.m1OriginalHistChkbx.isChecked():
                max_peak = max(hist[start_plot:end_plot]) * 1.1
            else:
                max_peak = max(hull[start_plot:end_plot]) * 1.1

            ax.set_xlim(start_plot, end_plot)
            ax.set_ylim(0 , max_peak)
            ax.set_title(str_info)
            ax.set_xlabel('Radial distance')
            ax.set_ylabel('Intensity')
            # self.m1_partial_hist_figure.tight_layout()
            self.m1_partial_hist_canvas.draw()

            img = copy.copy(self.cirProj.original_image)

            if self.blankChkBx.isChecked():
                blank, mask = getBlankImageAndMask(self.filePath)
                if blank is not None:
                    img = img - blank

            img = get8bitImage(img, min=self.minInt.value(), max=self.maxInt.value())

            center = (int(np.round(self.cirProj.info['center'][0])), int(np.round(self.cirProj.info['center'][1])))
            radius = int(distance((0,0),(img.shape[1],img.shape[0])))
            mask = np.zeros((img.shape[0], img.shape[1]), dtype=np.uint8)
            cv2.ellipse(mask, center, axes=(radius, radius), angle=0, startAngle=selected_range[0],
                        endAngle=selected_range[1], color=255,
                        thickness=-1)
            # img[mask > 0] += 25
            img = getBGR(img)
            r, g, b = cv2.split(img)
            red_panel = r.astype(np.int)
            red_panel[mask > 0] += 50
            red_panel[red_panel>255] = 255
            r = red_panel.astype(r.dtype)
            img = cv2.merge((r, g, b))
            ax = self.m1_img_axes
            ax.cla()
            ax.imshow(img)
            # self.m1_img_fig.tight_layout()
            self.m1_img_canvas.draw()
            self.update_plot['m1_partial_hist'] = False

        if 'orig_hists' in self.cirProj.info.keys() and 'm1_rings' in self.cirProj.info.keys() and \
                self.update_plot['m1_hist'] and 'hull_hist' in self.cirProj.info.keys():
            hist = self.cirProj.info['orig_hists']
            hull = self.cirProj.info['hull_hist']
            m1_rings = self.cirProj.info['m1_rings']
            ax = self.m1_hist_axes
            self.m1_hist_figure.subplots_adjust(top=0.90, bottom=0.20)
            ax.cla()
            for p in m1_rings:
                ax.plot([p, p], [0, max(hist)], color='r')

            if self.m1OriginalHistChkbx.isChecked():
                ax.plot(hist, color='b')
            else:
                hist = copy.copy(hull)

            ax.plot(hull, color='g')

            end_plot = len(hist)
            start_plot = 0
            if self.skipFirstPeakChkBx.isChecked() and 'start_point' in self.cirProj.info.keys():

                if 'merged_peaks' in self.cirProj.info.keys() and len(self.cirProj.info['merged_peaks']) > 0:
                    merged_rings = self.cirProj.info['merged_peaks']
                    last_ring = max(merged_rings)
                    first_ring = min(merged_rings)
                    end_plot = int(last_ring * 1.4)
                    start_plot = int(first_ring * 0.4)

            max_peak = max(hist[start_plot:end_plot]) * 1.1
            ax.set_xlim(start_plot, end_plot)
            ax.set_ylim(0, max_peak)
            ax.set_title('Peaks : '+str(m1_rings))
            ax.set_xlabel('Radial distance (Pixels)')
            ax.set_ylabel('Intensity')
            # self.m1_hist_figure.tight_layout()
            self.m1_hist_canvas.draw()
            self.update_plot['m1_hist'] = False

        if 'partial_angle' in self.cirProj.info.keys():
            self.updatingUI = True
            self.partialRange.setValue(self.cirProj.info['partial_angle'])
            self.updatingUI = False

    def updateMethod2Tab(self):
        if self.update_plot['m2_diff']:
            ax = self.m2_cent_diff_axes
            self.m2_cent_diff_fig.subplots_adjust(bottom=0.20)
            ax.cla()

            if self.method2ComboBx.currentIndex()==0 and 'tophat_2dintegration' in self.cirProj.info.keys():
                hist = self.cirProj.info['tophat_2dintegration'][0]
                ax.imshow(hist)
            elif self.method2ComboBx.currentIndex()==1 and 'm2_central_difference' in self.cirProj.info.keys():
                cent_diff = self.cirProj.info['m2_central_difference']
                ax.imshow(cent_diff)
            elif self.method2ComboBx.currentIndex()==2 and 'central_log' in self.cirProj.info.keys():
                hist = self.cirProj.info['central_log']
                ax.imshow(hist)

            x_lim = ax.get_xlim()
            y_lim = ax.get_ylim()

            if self.runsChkBx.isChecked() and 'm2_runs_dict' in self.cirProj.info.keys():
                runs_dict = self.cirProj.info['m2_runs_dict']
                for k in runs_dict.keys():
                    for run in runs_dict[k]:
                        ax.plot([run[0][1], run[1][1]], [run[0][0], run[1][0]], 'r')

            str_peak = ""
            if self.ringsChkBx.isChecked() and 'm2_rings' in self.cirProj.info.keys():
                rings = self.cirProj.info['m2_rings']
                for ring in rings.keys():
                    ax.plot([ring, ring], [0.1, 359], color = 'w' , lw = 2)
                # str_peak += '\nPeaks : '+str(rings)
            ax.set_xlim(x_lim)
            ax.set_ylim(y_lim)

            ax.set_xlabel('Radial distance (Pixels)'+str_peak)
            ax.set_ylabel('Angle')
            self.m2_cent_diff_fig.tight_layout()
            self.m2_cent_diff_canvas.draw()
            self.update_plot['m2_diff'] = False

    def swapCheckBoxes(self):
        hide = (self.graph_cmbbx.currentIndex() != 0)
        self.skip_first_peak_chkbx.setHidden(hide)
        self.original_hist_chkbx.setHidden(hide)
        self.rings_chkbx.setHidden(hide)
        self.hull_hist_chkbx.setHidden(hide)
        self.fit_hist_chkbx.setHidden(hide)
        self.selectPeaks.setHidden(hide)
        self.average_ring_chkbx.setHidden(not hide)
        self.ring_hists_chkbx.setHidden(not hide)
        self.g_model_chkbx.setHidden(not hide)

    def updateResultsTab(self):
        self.swapCheckBoxes()

        if self.graph_cmbbx.currentIndex() == 0:
            if 'model_peaks' in self.cirProj.info.keys():
                model_peaks = self.cirProj.info['model_peaks']
            else:
                model_peaks = []
            original_hist = self.cirProj.info['orig_hists']
            start_point = self.cirProj.info['start_point']
            ax = self.result_graph_axes
            ax.cla()

            # lines = []
            labels = []

            start_plot = 0
            end_plot = len(original_hist)

            if len(model_peaks) > 0:
                start_plot = int(min(model_peaks)*0.4)
                end_plot = int(max(model_peaks) * 1.4)

            max_peak = 0
            if self.original_hist_chkbx.isChecked():
                line, = ax.plot(original_hist, color='b')
                # lines.append(line)
                labels.append('Original')
                max_peak = max(original_hist[start_plot:end_plot])

            if self.hull_hist_chkbx.isChecked():
                hull_hist = self.cirProj.info['hull_hist']
                line, = ax.plot(hull_hist, color='m')
                # lines.append(line)
                labels.append('No BG')
                max_peak = max(max(hull_hist), max_peak)

            if self.fit_hist_chkbx.isChecked() and 'fitResult' in self.cirProj.info.keys():
                fit_result = self.cirProj.info['fitResult']
                x = np.array(range(start_point, len(original_hist)))
                fit_hist = GMM_any(x = x, params=fit_result)

                if fit_hist is not None:
                    line, = ax.plot(x, fit_hist, color='g')
                    # lines.append(line)
                    labels.append('Fit Model')
                    max_peak = max(max(fit_hist), max_peak)

            if self.skip_first_peak_chkbx.isChecked():
                max_peak = max_peak * 1.1
            else:
                max_peak = max(ax.get_ylim())
                start_plot = 0
                end_plot = len(original_hist)
            ax.set_ylim(0, max_peak)

            if self.rings_chkbx.isChecked() and len(model_peaks) > 0:
                for i in range(len(model_peaks)):
                    line = ax.axvline(model_peaks[i], color=tuple(np.array(self.ring_colors[i%len(self.ring_colors)])/255.))
                    # lines.append(line)
                labels.append('Merged Rings')

            ax.set_xlim(start_plot, end_plot)
            ax.set_xlabel('Radial distance')
            ax.set_ylabel('Intensity')
            # ax.legend(lines, labels)
            self.result_graph_figure.tight_layout()
            self.result_graph_canvas.draw()
            # self.update_plot['image_result'] = False

        else:
            self.g_model_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())
            self.ring_hists_chkbx.setEnabled('ring_hists' in self.cirProj.info.keys())
            self.average_ring_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())

            ax = self.result_graph_axes
            ax.cla()

            if 'ring_hists' in self.cirProj.info.keys():
                ring_hists = self.cirProj.info['ring_hists']
                x = np.arange(0, 2 * np.pi, 2 * np.pi / 360)
                if self.ring_hists_chkbx.isChecked():
                    for i in range(len(ring_hists)):
                        ax.plot(x, ring_hists[i], color = tuple(np.array(self.ring_colors[i%len(self.ring_colors)])/255.))

                if 'ring_models' in self.cirProj.info.keys() and self.g_model_chkbx.isChecked():
                    ring_models = self.cirProj.info['ring_models']
                    ring_errors = self.cirProj.info['ring_errors']
                    for i in ring_models.keys():
                        if ring_errors[i] < 1.5:
                            gauss = orientation_GMM2(x=x, **ring_models[i])
                            ax.plot(x, gauss, color='g')
                            u1 = ring_models[i]['u']
                            ax.plot((u1, u1), (0, max(gauss)), color='y')
                            ax.plot((u1+np.pi, u1+np.pi), (0, max(gauss)), color='y')

                if 'average_ring_model' in self.cirProj.info.keys() and self.average_ring_chkbx.isChecked():
                    mod = self.cirProj.info['average_ring_model']
                    gauss = orientation_GMM2(x=x, **mod)
                    u1 = mod['u']
                    ax.plot(x, gauss, color='k')
                    ax.plot((u1, u1), (0, max(gauss)), color='r')
                    ax.plot((u1 + np.pi, u1 + np.pi), (0, max(gauss)), color='r')


            self.result_graph_figure.tight_layout()
            self.result_graph_canvas.draw()

        processing_results_text = "Total Intensity : "+ str(self.cirProj.info['area'])
        processing_results_text += "\n\nFitting Results :"
        if 'fitResult' in self.cirProj.info.keys():
                fit_result = self.cirProj.info['fitResult']
                n = len(fit_result.keys())/3
                for i in range(1, n+1):
                    processing_results_text += "\nPeak "+str(i)+': '
                    processing_results_text += "\tcenter : "+str(fit_result['u'+str(i)])+':\n'
                    processing_results_text += "\talpha  : " + str(fit_result['alpha' + str(i)]) + ':\n'
                    processing_results_text += "\tsigmad : " + str(fit_result['sigmad' + str(i)]) + ':\n'

        self.processing_results.setText(processing_results_text)

        if 'ring_models' in self.cirProj.info.keys() and len(self.cirProj.info['ring_models']) > 0:
            models = self.cirProj.info['ring_models']
            errors = self.cirProj.info['ring_errors']

            rings_info = "Rings Information : \n"

            for i in models.keys():
                m = models[i]
                rings_info += "Ring " + str(i + 1) + " : \n"
                rings_info += "\tAngle : " + str(m['u']) + " rads. " + str(convertRadtoDegrees(m['u'])) + "degrees\n"
                angle_range = (m['u'] - m['sigma'], m['u'] + m['sigma'])
                rings_info += "\tRange: " + str(angle_range) + " rads"
                rings_info += " or " + str((convertRadtoDegrees(angle_range[0]), convertRadtoDegrees(angle_range[1]))) + " degrees\n"
                rings_info += "\tSigma : "+ str(m['sigma'])+ "\n"
                rings_info += "\tIntensity : "+ str(m['alpha'])+ "\n"
                rings_info += "\tFitting Error : "+ str(errors[i])+ "\n\n"

            rings_info += "\nAverage Angle : \n"
            if 'average_ring_model' in self.cirProj.info.keys():
                model = self.cirProj.info['average_ring_model']
                rings_info += " - Angle : " + str(model['u']) + " rads. " + str(
                    convertRadtoDegrees(model['u'])) + "degrees\n"
                angle_range = (model['u'] - model['sigma'], model['u'] + model['sigma'])
                rings_info += " - Standard deviation : " + str(model['sigma']) + "\n"
                rings_info += " - Range: " + str(angle_range) + " rads"
                rings_info += " or " + str(
                    (convertRadtoDegrees(angle_range[0]), convertRadtoDegrees(angle_range[1]))) + " degrees\n"
                rings_info += " - Intensity: " + str(model['alpha']) + "\n"
            else:
                if 'ring_models' in self.cirProj.info.keys() and len(self.cirProj.info['ring_models']) > 0:
                    rings_info += "Model can't be fitted. Rings are uniform\n"
                else:
                    rings_info += "N/A\n"
        else:
            rings_info = "Rings Information : N/A"

        self.rings_results.setText(rings_info)

            # self.update_plot['results_text'] = False

    def mousePressEvent(self, event):
        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

class CPBatchWindow(QMainWindow):
    def __init__(self, mainWin = None, dir_path=""):
        # QDialog.__init__(self, parent)
        QWidget.__init__(self)
        self.filePath = dir_path
        self.widgetList = []
        self.update_plot = {'intensity_maps': True,
                            'ds_maps': True,
                            'arange_maps': True,
                            'vector_maps': True,
                            'ellipse_maps': True
                            }
        self.intesityRange = [0, 1, 1, 2]
        self.mainWin = mainWin
        self.color_maps = 'jet'

        ##Batch Mode Params
        self.stopProcess = False
        self.name_dict = {}
        self.intensity_dict = {}
        self.distance_dict = {}
        self.angrange_dict = {}
        self.orientation_dict = {}
        self.fit_dict = {}
        self.fitcd_dict = {}
        self.coord_dict = {}
        self.hdf_filename = ""
        self.hdf_data = np.array([])
        self.xyIntensity = []
        self.xylim = []
        self.batchmodeImg = None
        self.batchmodeImgDetails = None
        self.batchmodeImgFilename = None
        self.updatingUI = False
        self.plots = {}

        self.vec_UV = []
        self.vec_quiver = None

        self.csvManager = CP_CSVManager(self.filePath)
        self.initUI()
        self.setConnections()
        self.processFolder(self.filePath)

    def initUI(self):
        self.setWindowTitle("Circular Projection v." + musclex.__version__)
        self.setStyleSheet(getStyleSheet())
        self.centralWidget = QWidget(self)
        self.mainLayout = QGridLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        #### IMAGE ####
        self.imgFigure = plt.figure(facecolor='#606060')
        self.imgAxes = self.imgFigure.add_subplot(111)
        self.imgCanvas = FigureCanvas(self.imgFigure)
        self.img_maxInt = QDoubleSpinBox()
        self.img_maxInt.setValue(1)
        self.img_maxInt.setKeyboardTracking(False)
        self.img_minInt = QDoubleSpinBox()
        self.img_minInt.setValue(0)
        self.img_minInt.setKeyboardTracking(False)
        self.img_minIntLabel = QLabel("Min intensity")
        self.img_maxIntLabel = QLabel("Max intensity")
        self.imgLayout = QGridLayout()
        self.imgLayout.addWidget(self.imgCanvas, 0, 0, 1, 4)
        self.imgLayout.addWidget(self.img_minIntLabel, 1, 0, 1, 1)
        self.imgLayout.addWidget(self.img_minInt, 1, 1, 1, 1)
        self.imgLayout.addWidget(self.img_maxIntLabel, 1, 2, 1, 1)
        self.imgLayout.addWidget(self.img_maxInt, 1, 3, 1, 1)

        #### BATCHMODE - tabs
        ## BATCHMODE 1 : Total Intensity Map Tab
        self.intensityTab = QWidget()
        self.intensityTab.setContentsMargins(0, 0, 0, 0)
        self.intensityTabLayout = QGridLayout(self.intensityTab)
        self.intensityMapFigure = plt.figure(facecolor='#606060')
        self.intensityMapAxes = self.intensityMapFigure.add_subplot(111)
        self.intensityMapCanvas = FigureCanvas(self.intensityMapFigure)
        self.intensityMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.int_maxIntMap = QDoubleSpinBox()
        self.int_maxIntMap.setMinimum(1)
        self.int_maxIntMap.setMaximum(100)
        self.int_maxIntMap.setValue(100.)
        self.int_maxIntMap.setSingleStep(5.0)
        self.int_maxIntMap.setSuffix("%")
        self.int_maxIntMap.setKeyboardTracking(False)

        self.int_minIntMap = QDoubleSpinBox()
        self.int_minIntMap.setMinimum(0)
        self.int_minIntMap.setMaximum(99)
        self.int_minIntMap.setValue(0)
        self.int_minIntMap.setSingleStep(5.0)
        self.int_minIntMap.setSuffix("%")
        self.int_minIntMap.setKeyboardTracking(False)

        self.int_MapIntSettings = QHBoxLayout()
        self.int_MapIntSettings.addWidget(QLabel("Max : "))
        self.int_MapIntSettings.addWidget(self.int_maxIntMap)
        self.int_MapIntSettings.addWidget(QLabel("Min : "))
        self.int_MapIntSettings.addWidget(self.int_minIntMap)
        self.intensityTabLayout.addWidget(self.intensityMapCanvas, 0, 0, 1, 1)
        self.intensityTabLayout.addLayout(self.int_MapIntSettings, 1, 0, 1, 1)

        ## BATCHMODE 2 : Distance btw rings map
        self.distanceMapTab = QWidget()
        self.distanceMapTab.setContentsMargins(0, 0, 0, 0)
        self.distanceMapTabLayout = QGridLayout(self.distanceMapTab)
        self.distanceMapFigure = plt.figure(facecolor='#606060')
        self.distanceMapAxes = self.distanceMapFigure.add_subplot(111)
        self.distanceMapCanvas = FigureCanvas(self.distanceMapFigure)
        self.distanceMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.ds_ImgFigure = plt.figure(facecolor='#606060')
        self.ds_ImgCanvas = FigureCanvas(self.ds_ImgFigure)
        
        self.ds_maxIntMap = QDoubleSpinBox()
        self.ds_maxIntMap.setMinimum(1)
        self.ds_maxIntMap.setMaximum(100)
        self.ds_maxIntMap.setValue(100.)
        self.ds_maxIntMap.setSingleStep(5.0)
        self.ds_maxIntMap.setSuffix("%")
        self.ds_maxIntMap.setKeyboardTracking(False)
        self.ds_minIntMap = QDoubleSpinBox()
        self.ds_minIntMap.setMinimum(0)
        self.ds_minIntMap.setMaximum(99)
        self.ds_minIntMap.setValue(0)
        self.ds_minIntMap.setSingleStep(5.0)
        self.ds_minIntMap.setSuffix("%")
        self.ds_minIntMap.setKeyboardTracking(False)
        self.ds_MapIntSettings = QHBoxLayout()
        self.ds_MapIntSettings.addWidget(QLabel("Max : "))
        self.ds_MapIntSettings.addWidget(self.ds_maxIntMap)
        self.ds_MapIntSettings.addWidget(QLabel("Min : "))
        self.ds_MapIntSettings.addWidget(self.ds_minIntMap)
        self.distanceMapTabLayout.addWidget(self.distanceMapCanvas, 0, 0, 1, 1)
        self.distanceMapTabLayout.addLayout(self.ds_MapIntSettings, 1, 0, 1, 1)

        ## BATCHMODE 3 : Angular range map (degrees)
        self.angularMapTab = QWidget()
        self.angularMapTab.setContentsMargins(0, 0, 0, 0)
        self.angularMapTabLayout = QGridLayout(self.angularMapTab)
        self.angularMapTab = QWidget()
        self.angularMapTab.setContentsMargins(0, 0, 0, 0)
        self.angularMapTabLayout = QGridLayout(self.angularMapTab)
        self.angularMapFigure = plt.figure(facecolor='#606060')
        self.angularMapAxes = self.angularMapFigure.add_subplot(111)
        self.angularMapCanvas = FigureCanvas(self.angularMapFigure)
        self.angularMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)

        self.ar_maxIntMap = QDoubleSpinBox()
        self.ar_maxIntMap.setMinimum(1)
        self.ar_maxIntMap.setMaximum(100)
        self.ar_maxIntMap.setValue(100.)
        self.ar_maxIntMap.setSingleStep(5.0)
        self.ar_maxIntMap.setSuffix("%")
        self.ar_maxIntMap.setKeyboardTracking(False)
        self.ar_minIntMap = QDoubleSpinBox()
        self.ar_minIntMap.setMinimum(0)
        self.ar_minIntMap.setMaximum(99)
        self.ar_minIntMap.setValue(0)
        self.ar_minIntMap.setSingleStep(5.0)
        self.ar_minIntMap.setSuffix("%")
        self.ar_minIntMap.setKeyboardTracking(False)
        self.ar_MapIntSettings = QHBoxLayout()
        self.ar_MapIntSettings.addWidget(QLabel("Max : "))
        self.ar_MapIntSettings.addWidget(self.ar_maxIntMap)
        self.ar_MapIntSettings.addWidget(QLabel("Min : "))
        self.ar_MapIntSettings.addWidget(self.ar_minIntMap)
        self.angularMapTabLayout.addWidget(self.angularMapCanvas, 0, 0, 1, 1)
        self.angularMapTabLayout.addLayout(self.ar_MapIntSettings, 1, 0, 1, 1)

        ## BATCHMODE 4 : Vector Fields
        self.vectorFieldTab = QWidget()
        self.vectorFieldTab.setContentsMargins(0, 0, 0, 0)
        self.vectorFieldTabLayout = QGridLayout(self.vectorFieldTab)
        self.vectorFieldMapFigure = plt.figure(facecolor='#606060')
        self.vectorFieldMapAxes = self.vectorFieldMapFigure.add_subplot(111)
        self.vectorFieldMapCanvas = FigureCanvas(self.vectorFieldMapFigure)
        self.vectorFieldMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.arrowLengthSlider = QSlider()
        self.arrowLengthSlider.setMinimum(5)
        self.arrowLengthSlider.setMaximum(25)
        self.arrowLengthSlider.setValue(5)
        self.arrowLengthSlider.setOrientation(Qt.Horizontal)

        self.vectorFieldTabLayout.addWidget(self.vectorFieldMapCanvas, 0, 0, 1, 2)
        self.vectorFieldTabLayout.addWidget(QLabel("Arrow Length"), 1, 0, 1, 1)
        self.vectorFieldTabLayout.addWidget(self.arrowLengthSlider, 1, 1, 1, 1)

        ## BATCHMODE 5 : Elliptical Presentation
        self.ellipticalTab = QWidget()
        self.ellipticalTab.setContentsMargins(0, 0, 0, 0)
        self.ellipticalTabLayout = QGridLayout(self.ellipticalTab)
        self.ellipticalMapFigure = plt.figure(facecolor='#606060')
        self.ellipticalMapAxes = self.ellipticalMapFigure.add_subplot(111)
        self.ellipticalMapCanvas = FigureCanvas(self.ellipticalMapFigure)
        self.ellipticalMapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.ellipticalTabLayout.addWidget(self.ellipticalMapCanvas, 0, 0, 1, 1)

        ## tabs
        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 35px; width: 200px; }")
        self.tabWidget.addTab(self.intensityTab, "Total Intensity Map")
        # self.tabWidget.addTab(self.distanceMapTab, "D-spacing Map")
        self.tabWidget.addTab(self.angularMapTab, "Angular Range Map\n(degrees)")
        self.tabWidget.addTab(self.vectorFieldTab, "Orientation and Intensity\nVector Field")
        self.tabWidget.addTab(self.ellipticalTab, "Elliptical Representation")

        # status bar
        self.statusBar = QStatusBar()
        self.imagePathLabel = QLabel('')
        self.statusLabel = QLabel('')
        self.statusBar.addWidget(QLabel('   '))
        self.statusBar.addWidget(self.imagePathLabel)
        self.progressBar = QProgressBar()
        self.progressBar.setMaximum(100)
        self.progressBar.setMinimum(0)
        self.progressBar.setHidden(True)
        # self.stopButton = QPushButton('STOP')
        # self.stopButton.clicked.connect(self.stopProcessing)
        # self.stopButton.setHidden(True)
        self.moreDetailsButton = QPushButton('More Details')
        self.moreDetailsButton.setHidden(True)
        self.rightBarLayout = QVBoxLayout()
        self.rightBarLayout.addWidget(self.statusLabel)
        self.rightBarLayout.addWidget(self.moreDetailsButton)
        self.rightBarLayout.setAlignment(Qt.AlignRight)
        self.rightBarFrame = QFrame()
        self.rightBarFrame.setLayout(self.rightBarLayout)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.statusBar.addPermanentWidget(self.rightBarFrame)

        self.refreshButton = QPushButton("Refresh Maps")
        self.refreshButton.setFixedHeight(40)

        self.mainLayout.addLayout(self.imgLayout, 0, 0, 1, 1)
        self.mainLayout.addWidget(self.refreshButton, 1, 0, 1, 1)
        self.mainLayout.addWidget(self.tabWidget, 0, 1, 2, 1)
        self.mainLayout.addWidget(self.statusBar, 2, 0, 1, 2)
        self.mainLayout.setRowStretch(0, 10)
        self.mainLayout.setRowStretch(1, 1)
        self.mainLayout.setRowStretch(2, 1)
        self.mainLayout.setColumnStretch(0, 1)
        self.mainLayout.setColumnStretch(1, 1)

        self.show()
        self.resize(1000, 1000)

    def setConnections(self):
        self.refreshButton.clicked.connect(self.processBatchmodeResults)

        self.img_maxInt.valueChanged.connect(self.maxIntChanged)
        self.img_minInt.valueChanged.connect(self.minIntChanged)

        self.int_maxIntMap.valueChanged.connect(self.sliderReleased)
        self.int_minIntMap.valueChanged.connect(self.sliderReleased)

        self.ds_maxIntMap.valueChanged.connect(self.sliderReleased)
        self.ds_minIntMap.valueChanged.connect(self.sliderReleased)

        self.ar_maxIntMap.valueChanged.connect(self.sliderReleased)
        self.ar_minIntMap.valueChanged.connect(self.sliderReleased)

        self.arrowLengthSlider.sliderReleased.connect(self.sliderReleased)

        self.tabWidget.currentChanged.connect(self.updateUI)
        self.moreDetailsButton.clicked.connect(self.popupImageDetails)

    def closeEvent(self, ev):
        if self.mainWin is not None:
            self.mainWin.removeWidget(self)
    
    def removeWidget(self, win):
        if win in self.widgetList:
            idx = self.widgetList.index(win)
            del self.widgetList[idx]
        
    def sliderReleased(self):
        QApplication.processEvents()
        if self.tabWidget.currentIndex() == 0:
            self.refreshAllTabs()
        elif self.tabWidget.currentIndex() == 1:
            self.update_plot['arange_maps'] = True
        elif self.tabWidget.currentIndex() == 2:
            self.updateVectorFieldArrow()

        self.updateUI()

    def plotClicked(self, event):
        self.moreDetailsButton.setHidden(False)
        if len(self.xyIntensity) < 3:
            return

        x = self.xyIntensity[0]
        y = self.xyIntensity[1]
        x_max = len(x)

        if x[0] <= event.xdata <= x[len(x) - 1] and y[0] <= event.ydata <= y[len(y) - 1]:
            # col = 0
            # row = 0
            indexs = list(range(0,len(x)))
            col = min(indexs, key=lambda i: abs(x[i] - event.xdata))

            if (self.tabWidget.currentIndex() == 0 or self.tabWidget.currentIndex() == 1) and event.xdata < x[col]:
                col = max(col-1, 0)

            if self.tabWidget.currentIndex() == 3:
                col = min(col+1, len(x) - 1)

            indexs = list(range(0, len(y)))
            row = min(indexs, key=lambda i: abs(y[i] - event.ydata))
            if (self.tabWidget.currentIndex() == 0 or self.tabWidget.currentIndex() == 1) and event.ydata < y[row] :
                row = max(row - 1, 0)

            ind = row * x_max + col + self.init_number

            if self.name_dict.has_key(ind):
                img_detail = "Intensity value: " + str(self.intensity_dict[ind])
                # img_detail += "\nD-spacing: " + str(self.distance_dict[ind])
                img_detail += "\nOrientation angle: " + str(self.orientation_dict[ind])
                img_detail += "\nAngular range: " + str(self.angrange_dict[ind])
                filename = self.name_dict[ind]
                full_filename = fullPath(self.filePath, filename)
                self.batchmodeImgDetails = img_detail
                self.batchmodeImgFilename = str(filename)
                self.updateRightStatusBar(filename)

                if exists(full_filename):

                    img = fabio.open(full_filename).data
                    if img is not None:
                        self.batchmodeImg = img
                        self.setMinMaxIntensity(img, self.img_minInt, self.img_maxInt, self.img_minIntLabel, self.img_maxIntLabel)
                        QApplication.processEvents()
                else:
                    self.batchmodeImg = None

                self.updateImage()
            else:
                self.batchmodeImgDetails = None
                self.batchmodeImgFilename = None
                self.batchmodeImg = None


    def stopProcessing(self):
        self.stopProcess = True

    def popupImageDetails(self):
        if self.batchmodeImg is not None:
            new_image_window = CPImageWindow(self, str(self.batchmodeImgFilename), str(self.filePath))
            self.widgetList.append(new_image_window)
        else:
            if self.batchmodeImgFilename is None:
                errMsg = QMessageBox()
                errMsg.setText('Image has not been selected')
                errMsg.setInformativeText('Please select an image from maps')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
            else:
                errMsg = QMessageBox()
                errMsg.setText('Image not found')
                errMsg.setInformativeText(str(self.batchmodeImgFilename)+' not found.\nPlease select another image')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()

    def maxIntChanged(self):
        if self.updatingUI:
            return

        if self.img_maxInt.value() < self.img_minInt.value():
            self.img_maxInt.setValue(self.img_minInt.value() + 1)
        else:
            self.updateImage()

    def minIntChanged(self):
        if self.updatingUI:
            return

        if self.img_maxInt.value() < self.img_minInt.value():
            self.img_minInt.setValue(self.img_maxInt.value() - 1)
        else:
            self.updateImage()


    def updateImage(self):
        if self.batchmodeImgDetails is not None:
            ax = self.imgAxes
            ax.cla()
            ax.set_title(self.batchmodeImgFilename)
            ax.set_xlabel(self.batchmodeImgDetails)

            if self.batchmodeImg is not None:
                img = getBGR(get8bitImage(self.batchmodeImg, min=self.img_minInt.value(), max=self.img_maxInt.value()))
                ax.imshow(img)
            else:
                xlim = ax.get_xlim()
                ylim = ax.get_ylim()
                cx = (xlim[0]+xlim[1])/2.
                cy = (ylim[0]+ylim[1])/2.
                ax.text(cx, cy, "IMAGE NOT FOUND", fontsize=15, horizontalalignment='center')

            self.imgCanvas.draw()

    def mousePressEvent(self, event):
        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

    def updateUI(self):
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            self.updateTotalIntenTab()
        elif selected_tab == 1:
            self.updateAngularRangeTab()
        elif selected_tab == 2:
            self.updateVectorFieldTab()
        elif selected_tab == 3:
            self.updateEllipticalTab()
        QApplication.processEvents()

    def updateTotalIntenTab(self):

        if self.update_plot['intensity_maps']:
            if len(self.xyIntensity) < 3:
                return

            x = self.xyIntensity[0]
            y = self.xyIntensity[1]
            intensity = copy.copy(self.xyIntensity[2])

            x_coor, y_coor = np.meshgrid(x, y)

            max_val = intensity.max()
            if self.int_maxIntMap.value() < 100:
                intensity[intensity > self.int_maxIntMap.value() * max_val/100.] = self.int_maxIntMap.value() * max_val/100.
            if self.int_minIntMap.value() > 0:
                intensity[intensity < self.int_minIntMap.value() * max_val/100.] = self.int_minIntMap.value() * max_val/100.

            ax = self.intensityMapAxes
            ax.cla()
            ax.set_title("Total Intensity Map\n")
            im = ax.pcolormesh(x_coor, y_coor, intensity, cmap=self.color_maps)
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            self.intensityMapFigure.colorbar(im)
            self.intensityMapFigure.tight_layout()
            self.intensityMapFigure.savefig(fullPath(self.filePath, 'cp_results/intensity_map.png'))
            self.intensityMapCanvas.draw()
            self.update_plot['intensity_maps'] = False

    def updateDspacingTab(self):

        if self.update_plot['ds_maps']:
            if len(self.xyIntensity) < 3:
                return

            x = self.xyIntensity[0]
            y = self.xyIntensity[1]
            x_max = len(x)

            if 'ds_' not in self.plots.keys():
                distances = [float(self.distance_dict[i]) if i in self.distance_dict and self.distance_dict[i] != '' else 0
                             for i in
                             range(self.init_number, len(self.hdf_data) + self.init_number)]
                distances = np.array([distances[i:i + x_max] for i in range(0, len(self.hdf_data), x_max)])
                self.plots['ds_'] = copy.copy(distances)
            else:
                distances = copy.copy(self.plots['ds_'])

            # z = cv2.blur(z, (4,4))
            x_coor, y_coor = np.meshgrid(x, y)

            max_val = distances.max()
            min_val = distances.min()
            if self.ds_maxIntMap.value() < 100:
                distances[distances > self.ds_maxIntMap.value() * max_val/100.] = max_val
            if self.ds_minIntMap.value() > 0:
                distances[distances < self.ds_minIntMap.value() * max_val/100.] = min_val

            ax = self.distanceMapAxes
            ax.cla()
            ax.set_title("D-spacing Map\n")
            im = ax.pcolormesh(x_coor, y_coor, distances, cmap=self.color_maps)
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            self.distanceMapFigure.colorbar(im)
            self.distanceMapFigure.tight_layout()
            self.distanceMapFigure.savefig(fullPath(self.filePath, 'cp_results/d_spacing_map.png'))
            self.distanceMapCanvas.draw()
            self.update_plot['ds_maps'] = False

    def updateAngularRangeTab(self):

        if self.update_plot['arange_maps']:
            if len(self.xyIntensity) < 3:
                return

            x = self.xyIntensity[0]
            y = self.xyIntensity[1]
            x_max = len(x)

            if 'ar_' not in self.plots.keys():
                ang_range = [
                    convertRadtoDegrees(float(self.angrange_dict[i])) if i in self.angrange_dict and self.angrange_dict[
                                                                                                         i] != '' else 0 for
                    i in range(self.init_number, len(self.hdf_data) + self.init_number)]
                ang_range = [r if 0 < r <= 180 else 0 for r in ang_range]
                ang_range = np.array([ang_range[i:i + x_max] for i in range(0, len(self.hdf_data), x_max)])
                self.plots['ar_'] = copy.copy(ang_range)
            else:
                ang_range = copy.copy(self.plots['ar_'])

            # z = cv2.blur(z, (4,4))
            x_coor, y_coor = np.meshgrid(x, y)

            max_val = ang_range.max()
            min_val = ang_range.min()
            if self.ar_maxIntMap.value() < 100:
                ang_range[ang_range > self.ar_maxIntMap.value() * max_val/100.] = max_val
            if self.ar_minIntMap.value() > 0:
                ang_range[ang_range < self.ar_minIntMap.value() * max_val/100.] = min_val

            ax = self.angularMapAxes
            ax.cla()
            ax.set_title("Angular Range Map (Degrees)\n")
            im = ax.pcolormesh(x_coor, y_coor, ang_range, cmap=self.color_maps)
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            self.angularMapFigure.colorbar(im)
            self.angularMapFigure.tight_layout()
            self.angularMapFigure.savefig(fullPath(self.filePath, 'cp_results/angular_range_map.png'))
            self.angularMapCanvas.draw()
            self.update_plot['arange_maps'] = False

    def updateVectorFieldTab(self):

        if self.update_plot['vector_maps']:
            if len(self.xyIntensity) < 3:
                return

            x = self.xyIntensity[0]
            y = self.xyIntensity[1]
            x_max = len(x)
            intensity = self.xyIntensity[2]

            orientation = np.array(
                [float(self.orientation_dict[i]) if i in self.orientation_dict and self.orientation_dict[i] != '' else 0
                 for i in
                 range(self.init_number, len(self.hdf_data) + self.init_number)])
            orientation = [np.pi - ang for ang in orientation]
            # orientation = [np.pi - ang if ang < np.pi else ang - np.pi for ang in orientation]
            orientation = np.array([orientation[i:i + x_max] for i in range(0, len(self.hdf_data), x_max)])

            U = np.cos(orientation)
            V = np.sin(orientation)
            int_display = copy.copy(intensity)

            max_val = int_display.max()
            if self.int_maxIntMap.value() < 100:
                int_display[
                    int_display > self.int_maxIntMap.value() * max_val / 100.] = self.int_maxIntMap.value() * max_val / 100.
            if self.int_minIntMap.value() > 0:
                intensity[
                    int_display < self.int_minIntMap.value() * max_val / 100.] = self.int_minIntMap.value() * max_val / 100.

            speed = int_display / intensity.max()
            UN = U * speed
            VN = V * speed
            self.vec_UV = [U, V]

            ax = self.vectorFieldMapAxes
            ax.cla()
            ax.set_facecolor('black')
            ax.set_title("Orientation (direction) and Intensity (height and color) Vector Field")
            self.vec_quiver = ax.quiver(x, y, UN, VN,  # data
                                        int_display,  # colour the arrows based on this array
                                      cmap=self.color_maps,  # colour map
                                      headlength=7, headwidth = 4)

            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            ax.set_aspect('auto')

            # self.vectorFieldMapFigure.colorbar(self.vec_quiver)
            self.vectorFieldMapFigure.tight_layout()
            self.vectorFieldMapFigure.savefig(fullPath(self.filePath, 'cp_results/vector_field.png'))
            self.vectorFieldMapCanvas.draw()

            if self.arrowLengthSlider.value() > 5:
                self.updateVectorFieldArrow()

            self.update_plot['vector_maps'] = False

    def updateVectorFieldArrow(self):
        if len(self.xyIntensity) < 3 or len(self.vec_UV) < 2 or self.vec_quiver is None:
            return

        intensity = self.xyIntensity[2]
        U = self.vec_UV[0]
        V = self.vec_UV[1]
        int_display = copy.copy(intensity)

        max_val = int_display.max()
        if self.int_maxIntMap.value() < 100:
            int_display[
                int_display > self.int_maxIntMap.value() * max_val / 100.] = self.int_maxIntMap.value() * max_val / 100.
        if self.int_minIntMap.value() > 0:
            int_display[
                int_display < self.int_minIntMap.value() * max_val / 100.] = self.int_minIntMap.value() * max_val / 100.

        speed = int_display / intensity.max() * (self.arrowLengthSlider.value()/5.)
        UN = U * speed
        VN = V * speed
        self.vec_quiver.set_UVC(UN, VN)
        self.vectorFieldMapCanvas.draw_idle()
        self.vectorFieldMapFigure.savefig(fullPath(self.filePath, 'cp_results/vector_field.png'))

    def updateEllipticalTab(self):

        if self.update_plot['ellipse_maps']:

            if len(self.xyIntensity) < 3:
                return

            x = self.xyIntensity[0]
            y = self.xyIntensity[1]

            centers = [(x[i], y[j]) for j in range(len(y)) for i in range(len(x))]
            ranges = [toFloat(self.angrange_dict[i]) if i in self.angrange_dict.keys() else 0. for i in range(self.init_number, len(self.hdf_data) + self.init_number)]
            max_width = max(ranges)
            widths = [toFloat(self.angrange_dict[i]) / max_width if i in self.angrange_dict.keys() and 0 < toFloat(self.angrange_dict[i]) / max_width else max_width/2. for i in range(self.init_number, len(self.hdf_data) + self.init_number)]

            int_display = np.array(self.intensity_dict.values())
            max_val = int_display.max()
            if self.int_maxIntMap.value() < 100:
                int_display[
                    int_display > self.int_maxIntMap.value() * max_val / 100.] = self.int_maxIntMap.value() * max_val / 100.
            if self.int_minIntMap.value() > 0:
                int_display[
                    int_display < self.int_minIntMap.value() * max_val / 100.] = self.int_minIntMap.value() * max_val / 100.

            ax = self.ellipticalMapAxes
            ax.cla()
            ax.set_title("Elliptical Representation of Orientation (direction) and Angle Range (width)")
            patches = []
            colors = []
            for i in range(len(self.hdf_data)):

                if ranges[i] == 0:
                    e = Ellipse(xy=centers[i - self.init_number], width=0.01, height=0.01)
                else:
                    e = Ellipse(xy=centers[i - self.init_number], width= 0.03 * widths[i], height=0.05,
                                angle=convertRadtoDegreesEllipse(np.pi - toFloat(self.orientation_dict[i + self.init_number])))
                patches.append(e)
                # colors.append(self.intensity_dict[i + self.init_number])
                if i < len(int_display):
                    colors.append(int_display[i])
                else:
                    colors.append(0)

            p = PatchCollection(patches, cmap=self.color_maps)
            p.set_array(np.array(colors))
            ax.add_collection(p)
            ax.set_facecolor('black')
            ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
            ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
            ax.set_aspect('auto')

            # self.ellipticalMapFigure.colorbar(p)
            self.ellipticalMapFigure.tight_layout()
            self.ellipticalMapFigure.savefig(fullPath(self.filePath, 'cp_results/direction_width.png'))
            self.ellipticalMapCanvas.draw()
            self.update_plot['ellipse_maps'] = False

    def browseHDF(self, dir_path, hdfList = []):
        hdf_filename = ""
        hdf_cache = fullPath(dir_path, 'hdf.info')
        if os.path.isfile(hdf_cache):
            hdf_filename = pickle.load(open(hdf_cache, "rb"))

        if len(hdf_filename) == 0 or not exists(hdf_filename):
            if len(hdfList) == 1:
                hdf_filename = fullPath(dir_path, hdfList[0])
            else:
                errMsg = QMessageBox()
                if len(hdfList) == 0:
                    errMsg.setText('No HDF file detected')
                else:
                    errMsg.setText('There are more than one HDF file detected')
                errMsg.setInformativeText('Please select an HDF file to process')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()

                hdf_filename = getAFile('HDF (*.hdf)')
                QApplication.processEvents()

        if hdf_filename != "":
            pickle.dump(hdf_filename, open(hdf_cache, "wb"))
            self.hdf_filename = str(hdf_filename)
            self.processBatchmodeResults()
        else:
            self.close()

    def processBatchmodeResults(self):
        QApplication.setOverrideCursor(Qt.WaitCursor)
        dir_path = self.filePath
        self.csvManager.load_all()
        hdf_filename = self.hdf_filename
        csv_filename = self.csvManager.sum_file
        self.updateStatusBar(text = 'Dir : ' + dir_path +'\nHDF : '+hdf_filename+ '\nCSV : '+csv_filename)
        df_sum = self.csvManager.df_sum.copy()
        df_sum = df_sum.sort_values(['filename'], ascending = True)
        df_rings = self.csvManager.df_rings

        # Read intensity from csv to organize the info given
        self.name_dict = {}
        self.intensity_dict = {}
        self.distance_dict = {}
        self.angrange_dict = {}
        self.orientation_dict = {}
        self.fit_dict = {}
        self.fitcd_dict = {}

        for i, row in df_sum.iterrows():
            filename = str(row['filename'])
            start_ind = filename.rfind('_')
            end_ind = filename.rfind('.')
            index = int(row['filename'][start_ind+1:end_ind])
            self.name_dict[index] = row['filename']
            self.intensity_dict[index] = row['total intensity']

            # Add ring model if its error < 1. and sigma < 1. (prevent uniform ring)
            all_rings = df_rings[df_rings['filename']==filename]
            if len(all_rings) > 0:
                all_rings = all_rings.sort_values(['angle fitting error'], ascending=True)
                best_ring = all_rings.iloc[0]
                good_model = float(best_ring['angle fitting error']) < 1. and best_ring['angle sigma'] < 1.
                self.orientation_dict[index] = best_ring['angle'] if pd.notnull(best_ring['angle']) and good_model else 0
                self.angrange_dict[index] = best_ring['angle sigma'] if pd.notnull(best_ring['angle sigma']) and good_model else 0
                self.distance_dict[index] = 0
            else:
                self.orientation_dict[index] = 0
                self.angrange_dict[index] = 0
                self.distance_dict[index] = 0

        self.init_number = min(self.name_dict.keys())

        # Read hdf5 file to get the coordinates and image shape
        hf = h5py.File(hdf_filename, 'r')
        data = hf.get('data').get('BL')
        self.hdf_data = np.array(data)
        self.coord_dict = {}

        for i in range(self.init_number, len(self.hdf_data) + self.init_number):
            self.coord_dict[i] = (self.hdf_data[i - self.init_number][0], self.hdf_data[i - self.init_number][1])

        nCols = 0
        for i in range(self.init_number+1, len(self.hdf_data) + self.init_number):
            if abs(self.coord_dict[i][1]-self.coord_dict[i-1][1]):
                nCols = i - self.init_number
                break

        nRows = len(self.hdf_data) / nCols
        all_xs = np.reshape(np.array([v[0] for k, v in self.coord_dict.items()]), (nRows, nCols))
        all_ys = np.reshape(np.array([v[1] for k, v in self.coord_dict.items()]), (nRows, nCols))

        x = np.mean(all_xs, axis=0)
        y = np.mean(all_ys, axis=1)

        # Check if any error on coordinates
        x_grad = abs(x[1] - x[0])
        y_grad = abs(y[1] - y[0])

        # Plot heatmap for intensity
        z = [float(self.intensity_dict[i]) if i in self.intensity_dict else 0 for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)]
        # z = np.array([z[i:i + x_max] for i in range(0, , x_max)])
        # z = cv2.blur(z, (4,4))
        # intensity = np.array(z)
        intensity = np.reshape(z, (len(y), len(x)))

        self.xyIntensity = [x, y, intensity]
        self.xylim = [x_grad, y_grad]
        self.refreshAllTabs()
        self.updateImage()
        self.updateUI()
        QApplication.restoreOverrideCursor()

    def refreshAllTabs(self):
        # Set all update flags to True
        self.update_plot = {'intensity_maps': True,
                            'ds_maps': True,
                            'arange_maps': True,
                            'vector_maps': True,
                            'ellipse_maps': True
                            }

    def processFolder(self, dir_path):
        imgList, hdfList = getFilesAndHdf(dir_path)
        createFolder(fullPath(dir_path,'cp_results'))

        if len(imgList) == 0:
            if exists(fullPath(dir_path, 'cp_results/summary.csv')):
                self.browseHDF(dir_path, hdfList)
            else:
                errMsg = QMessageBox()
                errMsg.setText('No image and summary.csv detected')
                errMsg.setInformativeText('Please select an image or another folder to process.')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()

        else:
            df_sum = self.csvManager.df_sum
            df_rings = self.csvManager.df_rings
            imgs1 = set(df_sum['filename'])
            imgs2 = set(df_rings['filename'])
            all_imgs = imgs1 & imgs2
            tmp_imlist = set(imgList[:])
            imgList = list(tmp_imlist-all_imgs)
            imgList.sort()
            if len(imgList) > 0:
                cp = CPImageWindow(self,"", dir_path, process_folder=True, imgList=imgList)
            self.browseHDF(dir_path, hdfList)

    def setMinMaxIntensity(self, img, minInt, maxInt, minIntLabel, maxIntLabel):
        min_val = img.min()
        max_val = img.max()
        self.intensityRange = [min_val, max_val-1, min_val+1, max_val]
        minInt.setMinimum(self.intensityRange[0])
        minInt.setMaximum(self.intensityRange[1])
        maxInt.setMinimum(self.intensityRange[2])
        maxInt.setMaximum(self.intensityRange[3])
        step = max(1., (max_val-min_val)/100)
        minInt.setSingleStep(step)
        maxInt.setSingleStep(step)
        minIntLabel.setText("Min intensity (" + str(min_val) + ")")
        maxIntLabel.setText("Max intensity (" + str(max_val) + ")")

        if img.dtype == 'float32':
            decimal = 2
        else:
            decimal = 0

        maxInt.setDecimals(decimal)
        minInt.setDecimals(decimal)

        if maxInt.value() == 1. and minInt.value() == 0.:
            self.updatingUI = True
            minInt.setValue(min_val)
            maxInt.setValue(max_val*0.6)
            self.updatingUI = False

    def updateRightStatusBar(self, text):
        QApplication.processEvents()
        self.statusLabel.setHidden(False)
        self.statusLabel.setText(text)
        QApplication.processEvents()

    def updateStatusBar(self, text, bar = None):
        QApplication.processEvents()
        self.imagePathLabel.setText(text)
        if bar is not None:
            self.progressBar.setValue(bar)
        QApplication.processEvents()

class CircularProjectionGUI(QMainWindow):
    resizeCompleted = pyqtSignal()

    def __init__(self):
        QWidget.__init__(self)
        self.widgetList = []
        self.initUI()

    def initUI(self):
        self.setStyleSheet(getStyleSheet())
        self.setWindowTitle("Circular Projection v." + musclex.__version__)
        self.centralWidget = QWidget(self)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        ## display browse file and folder buttons when program started
        self.browseFileButton = QPushButton("Select an Image...")
        self.browseFileButton.clicked.connect(self.browseFile)
        self.browseFileButton.setFixedHeight(60)
        self.browseFolderButton = QPushButton("Select a Folder...")
        self.browseFolderButton.clicked.connect(self.browseFolder)
        self.browseFolderButton.setFixedHeight(60)
        self.mainLayout.addWidget(self.browseFileButton)
        self.mainLayout.addWidget(self.browseFolderButton)

        # Menubar
        selectImageAction = QAction('Select an Image...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.triggered.connect(self.browseFile)
        selectFolderAction = QAction('Select a Folder...', self)
        selectFolderAction.setShortcut('Ctrl+F')
        selectFolderAction.triggered.connect(self.browseFolder)
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addAction(selectFolderAction)

        self.show()
        self.resize(400,150)

    def removeWidget(self, win):
        if win in self.widgetList:
            idx = self.widgetList.index(win)
            del self.widgetList[idx]

    def onNewFileSelected(self, fullfilepath):
        filePath, fileName = os.path.split(fullfilepath)
        new_image_window = CPImageWindow(self, str(fileName), str(filePath))
        self.widgetList.append(new_image_window)

    def browseFile(self):
        file_name = getAFile('Images (*.tif)')
        QApplication.processEvents()
        if file_name != "":
            self.onNewFileSelected(str(file_name))

    def browseFolder(self):
        dir_path = QFileDialog.getExistingDirectory(self, "Select a Folder")
        if dir_path != "":
            new_batch_window = CPBatchWindow(self, str(dir_path))
            self.widgetList.append(new_batch_window)

def getFilesAndHdf(dir_path):
    fileList = os.listdir(dir_path)
    imgList = []
    hdfList = []

    for f in fileList:
        full_file_name = fullPath(dir_path, f)
        if isImg(full_file_name):
            imgList.append(f)
            # if calculate_med_img:
            #     tmp_images.append(fabio.open(full_file_name).data)
        else:
            toks = f.split('.')
            if toks[-1] == 'hdf':
                hdfList.append(f)

    return imgList, hdfList

if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument('-i', help='image file')
    parser.add_argument('-f', help="handle all images in the folder")
    # parser.add_argument('-hdf', help="hdf file for the folder")
    args = parser.parse_args()

    if args.i:
        full_path = abspath(args.i)
        if isfile(full_path):
            filepath, filename = os.path.split(full_path)
            app = QApplication(sys.argv)
            myapp = CPImageWindow(mainWin=None, image_name=filename, dir_path=filepath)
            sys.exit(app.exec_())
        else:
            print "ERROR:",full_path, "does not exist. Please select another image."
    elif args.f:
        full_path = abspath(args.f)
        if exists(full_path) and not isfile(full_path):
            app = QApplication(sys.argv)
            myapp = CPImageWindow(mainWin=None, image_name="", dir_path=full_path, process_folder=True)
            sys.exit(app.exec_())
        else:
            print "ERROR:", full_path, "is not a folder."
    else:
        app = QApplication(sys.argv)
        myapp = CircularProjectionGUI()
        sys.exit(app.exec_())
