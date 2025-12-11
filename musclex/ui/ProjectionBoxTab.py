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

import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.ticker import MaxNLocator, FixedLocator
import numpy as np
from .pyqt_utils import *
from ..modules.ProjectionProcessor import layerlineModel, layerlineModelBackground, layerlineBackground, meridianBackground
from ..utils.image_processor import getNewZoom

class EditPeakDetails(QDialog):
    
    def __init__(self, info):
        super().__init__(None)
        self.info = info
        self.setWindowTitle("Edit Peak Information")
        self.initUI()
        self.loadSettings()
        
    def initUI(self):
        self.boxLayout = QGridLayout(self)
        
        self.bgsigma = QDoubleSpinBox()
        self.bgamp = QDoubleSpinBox()
        self.csigma1 = QDoubleSpinBox()
        self.camp1 = QDoubleSpinBox()
        self.csigma2 = QDoubleSpinBox()
        self.camp2 = QDoubleSpinBox()
        
        self.bgsigchk = QCheckBox("Lock Variable")
        self.bgampchk = QCheckBox("Lock Variable")
        self.csig1chk = QCheckBox("Lock Variable")
        self.camp1chk = QCheckBox("Lock Variable")
        self.csig2chk = QCheckBox("Lock Variable")
        self.camp2chk = QCheckBox("Lock Variable")
        
        self.bgsigma.setRange(-1e10, 1e10)
        self.bgamp.setRange(-1e10, 1e10)
        self.csigma1.setRange(-1e10, 1e10)
        self.camp1.setRange(-1e10, 1e10)
        self.csigma2.setRange(-1e10, 1e10)
        self.camp2.setRange(-1e10, 1e10)
        
        self.bgsigma.setDecimals(2)
        self.bgamp.setDecimals(2)
        self.csigma1.setDecimals(2)
        self.camp1.setDecimals(2)
        self.csigma2.setDecimals(2)
        self.camp2.setDecimals(2)
        
        self.bgsigma.setValue(self.info['bg_sigma'])
        self.bgamp.setValue(self.info['bg_amplitude'])
        self.csigma1.setValue(self.info['center_sigma1'])
        self.camp1.setValue(self.info['center_amplitude1'])
        self.csigma2.setValue(self.info['center_sigma2'])
        self.camp2.setValue(self.info['center_amplitude2'])   
        
        self.bottons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel,
                                              Qt.Horizontal, self)
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)
        self.bottons.setFixedWidth(200)
        
        self.boxLayout.addWidget(QLabel("Background Sigma (blue)"), 0, 0,1,1)
        self.boxLayout.addWidget(self.bgsigma, 0, 1, 1, 1)
        self.boxLayout.addWidget(self.bgsigchk, 0, 2, 1, 1)
        self.boxLayout.addWidget(QLabel("Background Amplitude (blue)"), 1, 0, 1, 1)
        self.boxLayout.addWidget(self.bgamp, 1, 1, 1, 1)
        self.boxLayout.addWidget(self.bgampchk, 1, 2, 1, 1)
        self.boxLayout.addWidget(QLabel("Meridian Background Sigma (yellow)"), 2, 0, 1, 1)
        self.boxLayout.addWidget(self.csigma1, 2, 1, 1, 1)
        self.boxLayout.addWidget(self.csig1chk, 2, 2, 1, 1)
        self.boxLayout.addWidget(QLabel("Meridian Amplitude (yellow)"), 3, 0, 1, 1)
        self.boxLayout.addWidget(self.camp1, 3, 1, 1, 1)
        self.boxLayout.addWidget(self.camp1chk, 3, 2, 1, 1)
        self.boxLayout.addWidget(QLabel("Meridian Sigma (red)"), 4, 0, 1, 1)
        self.boxLayout.addWidget(self.csigma2, 4, 1, 1, 1)
        self.boxLayout.addWidget(self.csig2chk, 4, 2, 1, 1)
        self.boxLayout.addWidget(QLabel("Meridian Amplitude (red)"), 5, 0, 1, 1)
        self.boxLayout.addWidget(self.camp2, 5, 1, 1, 1)
        self.boxLayout.addWidget(self.camp2chk, 5, 2, 1, 1)
        self.boxLayout.addWidget(self.bottons, 6, 0, 1, 2)
        
    def closeEvent(self, event):
        self.saveSettings()
        event.accept()
        
    def okClicked(self):
        """
        Triggered when OK is clicked
        """
        self.newinfo = {
            'bg_sigma': self.bgsigma.value(),
            'bg_amplitude': self.bgamp.value(),
            'center_sigma1': self.csigma1.value(),
            'center_amplitude1': self.camp1.value(),
            'center_sigma2': self.csigma2.value(),
            'center_amplitude2': self.camp2.value(),
            'bg_sigma_lock': self.bgsigchk.isChecked(),
            'bg_amplitude_lock': self.bgampchk.isChecked(),
            'center_sigma1_lock': self.csig1chk.isChecked(),
            'center_amplitude1_lock': self.camp1chk.isChecked(),
            'center_sigma2_lock': self.csig2chk.isChecked(),
            'center_amplitude2_lock': self.camp2chk.isChecked()
        }
        self.saveSettings()
        self.accept()
        
    def saveSettings(self):
        settings = QSettings("Checkboxes", "PeakDetails")
        settings.setValue("bg_sigma_lock", self.bgsigchk.isChecked())
        settings.setValue("bg_amplitude_lock", self.bgampchk.isChecked())
        settings.setValue("center_sigma1_lock", self.csig1chk.isChecked())  
        settings.setValue("center_amplitude1_lock", self.camp1chk.isChecked())
        settings.setValue("center_sigma2_lock", self.csig2chk.isChecked())
        settings.setValue("center_amplitude2_lock", self.camp2chk.isChecked())
        
    def loadSettings(self):
        settings = QSettings("Checkboxes", "PeakDetails")
        self.bgsigchk.setChecked(settings.value("bg_sigma_lock", False, type=bool))
        self.bgampchk.setChecked(settings.value("bg_amplitude_lock", False, type=bool))
        self.csig1chk.setChecked(settings.value("center_sigma1_lock", False, type=bool))
        self.camp1chk.setChecked(settings.value("center_amplitude1_lock", False, type=bool))
        self.csig2chk.setChecked(settings.value("center_sigma2_lock", False, type=bool))
        self.camp2chk.setChecked(settings.value("center_amplitude2_lock", False, type=bool))
        

class ProjectionBoxTab(QWidget):
    """
    Fitting Tabs : left or right
    Display fitting graph and providing options
    """
    def __init__(self, parent, name):
        super().__init__()
        self.parent = parent
        self.name = name
        self.function = None
        self.syncUI = False
        self.need_update = True
        self.checkableButtons = []
        self.graphMaxBound = None
        self.zoom1 = None
        self.zoom2 = None
        self.zoomRect = None
        self.centerX = None
        
        self.dragging = False
        self.dragged_line = None
        self.fixed_indices = []
        self.lines = []
        
        self.initUI()
        self.setAllToolTips()
        self.setConnections()
        self.clearSettingsFirstLaunch()
        
    def clearSettingsFirstLaunch(self):
        settings = QSettings("Checkboxes", "PeakDetails")
        if not settings.contains("initial_setup_done"):
            settings.clear()
            settings.setValue("initial_setup_done", True)

    def getCenterX(self):
        """
        Get center X
        :return: center X
        """
        if self.parent.projProc is None:
            return 0

        info = self.parent.projProc.info
        name = self.name
        box = info['boxes'][name]
        if info['types'][name] == 'h':
            start_x = box[0][0]
            if self.parent.centerx is None:
                self.centerX = self.parent.projProc.orig_img.shape[1] / 2. - 0.5 - start_x
            else:
                self.centerX = self.parent.centerx - start_x
        elif info['types'][name] == 'oriented':
            start_x = box[0][0]
            self.centerX = box[6][0] - start_x
        else:
            if info['types'][name] == 'v':
                start_y = box[1][0]
            else:
                start_y = box[0][1]
            if self.parent.centery is None:
                self.centerX = self.parent.projProc.orig_img.shape[0] / 2. - 0.5 - start_y
            else:
                self.centerX = self.parent.centery - start_y

        return self.centerX

    def initUI(self):
        """
        Initial all GUIs including : 4 plots and result table
        """
        self.setContentsMargins(0, 0, 0, 0)
        self.tabLayout = QHBoxLayout(self)
        self.graphFigure1 = plt.figure()
        self.graphAxes1 = self.graphFigure1.add_subplot(111)
        self.graphCanvas1 = FigureCanvas(self.graphFigure1)

        self.graphFigure2 = plt.figure()
        self.graphAxes2 = self.graphFigure2.add_subplot(111)
        self.graphCanvas2 = FigureCanvas(self.graphFigure2)

        self.optionsFrame = QFrame()
        self.optionsFrame.setFixedWidth(350)
        self.optionsLayout = QVBoxLayout(self.optionsFrame)

        self.displayOptionsGroup = QGroupBox("Display Options")
        self.dispOptLayout = QGridLayout(self.displayOptionsGroup)

        self.histChkBx = QCheckBox("Original Projection")
        self.histChkBx.setChecked(True)
        self.hullRangeChkBx = QCheckBox('Hull Range')
        self.hullRangeChkBx.setEnabled(self.parent.bgsubs[self.name] == 1)
        self.hullRangeChkBx.setChecked(self.parent.bgsubs[self.name]==1)
        self.fitmodelChkBx = QCheckBox("Fitting Model")
        self.fitmodelChkBx.setChecked(True)
        self.bgChkBx = QCheckBox("Background")
        self.bgChkBx.setChecked(True)
        self.maxPeaksChkBx = QCheckBox("Max Peaks")
        self.maxPeaksChkBx.setChecked(False)
        self.peaksChkBx = QCheckBox("Model Peaks")
        self.peaksChkBx.setChecked(True)
        self.centerChkBx = QCheckBox("Center")
        self.centerChkBx.setChecked(False)
        self.subHistChkBx = QCheckBox("Subtracted Projection")
        self.subHistChkBx.setChecked(True)
        self.baselineChkBx = QCheckBox("Baselines")
        self.baselineChkBx.setChecked(False)
        self.centroidChkBx = QCheckBox("Centroids")
        self.centroidChkBx.setChecked(False)

        self.zoomInButton = QPushButton("Zoom in")
        self.zoomInButton.setCheckable(True)
        self.zoomOutButton = QPushButton("Zoom out")
        self.fullButton = QPushButton("Full")
        self.checkableButtons.append(self.zoomInButton)
        self.dispOptLayout.addWidget(self.histChkBx, 0, 0, 1, 1)
        self.dispOptLayout.addWidget(self.fitmodelChkBx, 0, 1, 1, 1)
        self.dispOptLayout.addWidget(self.hullRangeChkBx, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.bgChkBx, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxPeaksChkBx, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.peaksChkBx, 3, 1, 1, 1)
        self.dispOptLayout.addWidget(self.centerChkBx, 4, 0, 1, 1)
        self.dispOptLayout.addWidget(self.centroidChkBx, 4, 1, 1, 1)
        self.dispOptLayout.addWidget(self.baselineChkBx, 5, 0, 1, 1)
        self.dispOptLayout.addWidget(self.subHistChkBx, 5, 1, 1, 1)
        self.dispOptLayout.addWidget(self.zoomInButton, 6, 0, 1, 1)
        self.dispOptLayout.addWidget(self.zoomOutButton, 6, 1, 1, 1)
        self.dispOptLayout.addWidget(self.fullButton, 7, 0, 1, 2)

        self.settingGroup = QGroupBox("Settings")
        self.settingLayout = QGridLayout(self.settingGroup)
        
        # Modified: change name to "Select Single Peak"
        self.peaksButton = QPushButton("Select Single Peak")
        self.peaksButton.setCheckable(True)
        self.checkableButtons.append(self.peaksButton)
        
        # New: Peak Cluster button for GMM
        self.peakClusterButton = QPushButton("Select Peak Cluster (GMM)")
        self.peakClusterButton.setCheckable(True)
        self.checkableButtons.append(self.peakClusterButton)
        
        # New: GMM number of Gaussians spinner
        self.gmmNumGaussSpinBox = QSpinBox()
        self.gmmNumGaussSpinBox.setRange(1, 10)
        self.gmmNumGaussSpinBox.setValue(3)
        self.gmmNumGaussSpinBox.setPrefix("N: ")
        self.gmmNumGaussSpinBox.setToolTip("Number of Gaussian peaks to fit (will be mirrored)")
        
        self.clearPeakButton = QPushButton("Clear Peaks")
        self.clearPeakButton.setVisible(True)
        
        # New: Parameter Editor button
        self.paramEditorButton = QPushButton("Open Parameter Editor")
        self.paramEditorButton.setEnabled(True)
        
        self.meridBckGrndChkBx = QCheckBox("Meridional Peak")
        self.meridBckGrndChkBx.setChecked(True)
        self.meridBckGrndChkBx.setHidden(self.parent.bgsubs[self.name]!=0)
        self.editMainPeakButton = QPushButton("Edit Meridional Peak")
        self.editMainPeakButton.setEnabled(False)
        self.refitButton = QPushButton("Refit")
        self.refitButton.setEnabled(False)
        
        self.hullRangeButton = QPushButton("Set Manual Convex Hull Range")
        self.hullRangeButton.setCheckable(True)
        self.hullRangeButton.setHidden(self.parent.bgsubs[self.name]!=1)
        box = self.parent.allboxes[self.name]
        width = int(np.ceil(abs(box[0][0]-box[0][1])/2.))

        self.startHull = QSpinBox()
        self.startHull.setRange(0, width)
        self.startHull.setKeyboardTracking(False)
        self.startHull.setHidden(self.parent.bgsubs[self.name] != 1)
        self.endHull = QSpinBox()
        self.endHull.setRange(0, width)
        self.endHull.setKeyboardTracking(False)
        self.endHull.setHidden(self.parent.bgsubs[self.name] != 1)
        self.checkableButtons.append(self.hullRangeButton)
        
        # Layout for new buttons
        self.settingLayout.addWidget(self.peaksButton, 0, 0, 1, 2)
        self.settingLayout.addWidget(self.peakClusterButton, 1, 0, 1, 1)
        self.settingLayout.addWidget(self.gmmNumGaussSpinBox, 1, 1, 1, 1)
        self.settingLayout.addWidget(self.clearPeakButton, 2, 0, 1, 2)
        self.settingLayout.addWidget(self.paramEditorButton, 3, 0, 1, 2)
        self.settingLayout.addWidget(self.hullRangeButton, 4, 0, 1, 2)
        if self.parent.bgsubs[self.name]== 1:
            self.settingLayout.addWidget(QLabel("Start"), 5, 0, 1, 1, Qt.AlignCenter)
            self.settingLayout.addWidget(QLabel("End"), 5, 1, 1, 1, Qt.AlignCenter)
        self.settingLayout.addWidget(self.startHull, 6, 0, 1, 1)
        self.settingLayout.addWidget(self.endHull, 6, 1, 1, 1)
        self.settingLayout.addWidget(self.meridBckGrndChkBx, 7, 0, 1, 2)
        self.settingLayout.addWidget(self.editMainPeakButton, 8, 0, 1, 2)
        self.settingLayout.addWidget(self.refitButton, 9, 0, 1, 2)

        self.results_text = QLabel()

        self.resultTable1 = QTableWidget()
        self.resultTable1.setColumnCount(4)
        self.resultTable1.setHorizontalHeaderLabels(["Peak Location", "Gauss Center", "Gauss Sigma", "Gauss Area"])
        tooltips1 = ["Location of the peak relative to the center of the projection",
                     "Center of the Gaussian fitted per peak", 
                     "The Gauss Sigma is the standard deviation of the distribution",
                     "Area of the Gaussian of the Peak"]
        for i, tooltip in enumerate(tooltips1):
            item = self.resultTable1.horizontalHeaderItem(i)
            if item is None:  # If the header item does not exist, create it
                item = QTableWidgetItem()
                self.resultTable1.setHorizontalHeaderItem(i, item)
            item.setToolTip(tooltip)
        self.resultTable1.horizontalHeader().setStretchLastSection(True)
        self.resultTable1.setColumnWidth(0, 75)
        self.resultTable1.setColumnWidth(1, 75)
        self.resultTable1.setColumnWidth(2, 75)
        self.resultTable1.setColumnWidth(3, 75)
        self.resultTable1.setFixedHeight(100)

        self.resultTable2 = QTableWidget()
        self.resultTable2.setColumnCount(4)
        self.resultTable2.setHorizontalHeaderLabels(["Baseline", "Centroid", "Width", "Area"])
        tooltips2 = ["By default, the baseline of the peak is the half-height of the peak",
                     "The centroid of the peak is calculated by taking the dot product of the distance from the center and the projected intensity in the range of left and right intersections.\n The result will then be divided by the projected intensity along the range",
                     "The width of the baseline",
                     "The area underneath the peak."]
        for i, tooltip in enumerate(tooltips2):
            item = self.resultTable2.horizontalHeaderItem(i)
            if item is None:  # If the header item does not exist, create it
                item = QTableWidgetItem()
                self.resultTable2.setHorizontalHeaderItem(i, item)
            item.setToolTip(tooltip)
        self.resultTable2.horizontalHeader().setStretchLastSection(True)
        self.resultTable2.setColumnWidth(0, 75)
        self.resultTable2.setColumnWidth(1, 75)
        self.resultTable2.setColumnWidth(2, 75)
        self.resultTable2.setColumnWidth(3, 75)
        self.resultTable2.setFixedHeight(100)
        self.pnButtons = QHBoxLayout()
        self.prevButton = QPushButton("<<<")
        self.nextButton = QPushButton(">>>")
        self.pnButtons.addWidget(self.prevButton)
        self.pnButtons.addWidget(self.nextButton)

        self.optionsLayout.addWidget(self.displayOptionsGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(QLabel("<h3>Model Peak Information</h3>"))
        self.optionsLayout.addWidget(self.resultTable1)
        self.optionsLayout.addWidget(QLabel("<h3>Centroid Peak Information</h3>"))
        self.optionsLayout.addWidget(self.resultTable2)
        self.optionsLayout.addStretch()
        self.optionsLayout.addLayout(self.pnButtons)

        self.graphLayout = QVBoxLayout()
        self.graphLayout.addWidget(self.graphCanvas1)
        self.graphLayout.addWidget(self.graphCanvas2)
        self.graphLayout.addWidget(QLabel("<h5>HINT: Use the 'Zoom in' and 'Zoom out' buttons in the top right to zoom in and out of either canvas.</h5>"))
        self.tabLayout.addLayout(self.graphLayout)
        self.tabLayout.addWidget(self.optionsFrame)

    def setAllToolTips(self):
        """
        Set Tooltips for widgets
        """
        return

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.histChkBx.stateChanged.connect(self.resetUI)
        self.fitmodelChkBx.stateChanged.connect(self.resetUI)
        self.hullRangeChkBx.stateChanged.connect(self.resetUI)
        self.bgChkBx.stateChanged.connect(self.resetUI)
        self.maxPeaksChkBx.stateChanged.connect(self.resetUI)
        self.peaksChkBx.stateChanged.connect(self.resetUI)
        self.centerChkBx.stateChanged.connect(self.resetUI)
        self.subHistChkBx.stateChanged.connect(self.resetUI)
        self.baselineChkBx.stateChanged.connect(self.resetUI)
        self.centroidChkBx.stateChanged.connect(self.resetUI)

        self.zoomInButton.clicked.connect(self.zoomInclicked)
        self.zoomOutButton.clicked.connect(self.zoomOutclicked)
        self.fullButton.clicked.connect(self.fullClicked)

        self.peaksButton.clicked.connect(self.addSinglePeak)
        self.peakClusterButton.clicked.connect(self.addPeakCluster)
        self.paramEditorButton.clicked.connect(self.openParameterEditor)
        self.clearPeakButton.clicked.connect(self.clearPeaks)
        self.meridBckGrndChkBx.stateChanged.connect(self.meridBckGrndChanged)
        self.hullRangeButton.clicked.connect(self.setManualHullRange)
        self.startHull.valueChanged.connect(self.hullRangeChanged)
        self.endHull.valueChanged.connect(self.hullRangeChanged)
        self.editMainPeakButton.clicked.connect(self.editMainPeak)
        self.refitButton.clicked.connect(self.refit)

        self.resultTable1.itemClicked.connect(self.handleTable1Event)
        self.resultTable2.itemClicked.connect(self.handleTable2Event)
        

        self.prevButton.clicked.connect(self.parent.prevClicked)
        self.nextButton.clicked.connect(self.parent.nextClicked)

        self.graphFigure1.canvas.mpl_connect('button_press_event', self.graphClicked)
        self.graphFigure2.canvas.mpl_connect('button_press_event', self.graphClicked2)
        self.graphFigure1.canvas.mpl_connect('motion_notify_event', self.graphOnMotion1)
        self.graphFigure2.canvas.mpl_connect('motion_notify_event', self.graphOnMotion2)
        self.graphFigure1.canvas.mpl_connect('button_release_event', self.graphReleased)
        self.graphFigure2.canvas.mpl_connect('button_release_event', self.graphReleased)
        self.graphFigure1.canvas.mpl_connect('figure_leave_event', self.graphReleased)
        self.graphFigure2.canvas.mpl_connect('figure_leave_event', self.graphReleased)
        self.graphFigure1.canvas.mpl_connect('button_press_event', self.on_press)
        self.graphFigure1.canvas.mpl_connect('motion_notify_event', self.on_motion)
        self.graphFigure1.canvas.mpl_connect('button_release_event', self.on_release)
    
    def editMainPeak(self):
        # print(self.parent.projProc.info['fit_results'][self.name])
        dialog = EditPeakDetails(self.parent.projProc.info['fit_results'][self.name])
        if dialog.exec_():
            self.newinfo = dialog.newinfo
            self.refitButton.setEnabled(True)
            self.refitButton.setStyleSheet("background-color: orange;")
            
    def refit(self):
        self.parent.projProc.removeInfo(self.name, 'fit_results')
        if 'main_peak_info' not in self.parent.projProc.info:
            self.parent.projProc.info['main_peak_info'] = {}
        self.parent.projProc.info['main_peak_info'][self.name] = self.newinfo
        self.parent.processImage() 
        self.refitButton.setEnabled(False)
        self.refitButton.setStyleSheet("") 

    def meridBckGrndChanged(self):
        """
        Trigger when meridian background is changed (on/off)
        """
        if self.parent.projProc is not None and not self.syncUI:
            self.parent.projProc.removeInfo(self.name, 'fit_results')
            self.parent.merid_bg[self.name] = self.meridBckGrndChkBx.isChecked()
            self.parent.processImage()

    def hullRangeChanged(self):
        """
        Trigger when convex hull range is changed
        """
        if self.parent.projProc is not None and not self.syncUI:
            self.parent.hull_ranges[self.name] = (self.startHull.value(), self.endHull.value())
            self.parent.projProc.removeInfo(self.name, 'hists2')
            self.parent.processImage()
            
    def handleTable1Event(self, item):
        """
        Trigger when item in resultTable1 is clicked
        """
        print("triggered click")
        self.resultTable1.itemChanged.connect(self.handleResultTable1Changed)
    
    def handleTable2Event(self, item):
        """
        Trigger when item in resultTable2 is clicked
        """
        self.resultTable1.itemChanged.connect(self.handleBaselineChanged)


    def handleResultTable1Changed(self, item):
        """
        Trigger when Gaussian Sigma or Gauss Center in table is changed
        :param item:
        :return:
        """
        self.resultTable1.itemChanged.disconnect(self.handleResultTable1Changed)
        if self.parent.projProc is not None and item.column() == 2 and not self.syncUI:
            try:
                self.parent.projProc.setGaussSig(self.name, item.row(), item.text())
                self.parent.processImage()
            except ValueError:
                errMsg = QMessageBox()
                errMsg.setText('Invalid Value')
                errMsg.setInformativeText("Please use an int or float number for the Gaussian Sigma value\n\n")
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                self.need_update = True
                self.updateUI()
        elif self.parent.projProc is not None and item.column() == 1 and not self.syncUI:
            try:
                self.parent.projProc.setGaussCenter(self.name, item.row(), item.text())
                self.parent.processImage()
            except ValueError:
                errMsg = QMessageBox()
                errMsg.setText('Invalid Value')
                errMsg.setInformativeText("Please use an int or float number for the Gaussian Center value\n\n")
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                self.need_update = True
                self.updateUI()


    def handleBaselineChanged(self, item):
        """
        Trigger when baseline in table is changed
        :param item:
        :return:
        """
        self.resultTable2.itemChanged.disconnect(self.handleBaselineChanged) 
        if self.parent.projProc is not None and item.column() == 0 and not self.syncUI:
            try:
                self.parent.projProc.setBaseline(self.name, item.row(), item.text())
                self.parent.processImage()
            except ValueError:
                errMsg = QMessageBox()
                errMsg.setText('Invalid Value')
                errMsg.setInformativeText("Please use an int/float number or a percentage (using '%') for the baseline value\n\n")
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                self.need_update = True
                self.updateUI()
        
        

    def zoomInclicked(self):
        """
        Trigger when zoom in button is pressed
        """
        if self.zoomInButton.isChecked():
            self.function = ['zoom']
        else:
            self.function = None
            self.resetUI()

    def zoomOutclicked(self):
        """
        Trigger when zoom out clicked
        :return:
        """
        if self.graphMaxBound is not None:
            xlim = self.graphMaxBound[0]
            ylim = self.graphMaxBound[1]

            def increaseLimit(canvas, ax, xlim, ylim):
                old_xlim = ax.get_xlim()
                old_ylim = ax.get_ylim()
                xscale = abs(old_xlim[0]-old_xlim[1])*0.1
                yscale = abs(old_ylim[0]-old_ylim[1])*0.1
                new_x1 = max(old_xlim[0] - xscale, xlim[0])
                new_x2 = min(old_xlim[1] + xscale, xlim[1])
                new_y1 = max(old_ylim[0] - yscale, ylim[0])
                new_y2 = min(old_ylim[1] + yscale, ylim[1])
                zoom = [(new_x1, new_x2), (new_y1,new_y2)]
                ax.set_xlim(zoom[0])
                ax.set_ylim(zoom[1])
                canvas.draw_idle()
                return zoom

            self.zoom1 = increaseLimit(self.graphCanvas1, self.graphAxes1, xlim, ylim)
            self.zoom2 = increaseLimit(self.graphCanvas2, self.graphAxes2, xlim, ylim)

    def fullClicked(self):
        """
        Triggered when Full button is pressed
        """
        self.zoom1 = None
        self.zoom2 = None
        self.resetUI()

    def setZoomIn(self, point1, point2):
        """
        Set Zoom in for plot n. If n of point1 and point2 are not the same, just refresh UI.
        :param point1: (n, (x1, y1))
        :param point2: (n, (x2, y2))
        :return:
        """
        if point1[0] == point2[0]:
            # Set zoom area for a plot
            pt1 = point1[1]
            pt2 = point2[1]
            x1 = min(pt1[0], pt2[0])
            x2 = max(pt1[0], pt2[0])
            y1 = min(pt1[1], pt2[1])
            y2 = max(pt1[1], pt2[1])
            zoom = [(x1, x2), (y1, y2)]
            if point1[0] == 1:
                self.zoom1 = zoom
            else:
                self.zoom2 = zoom
        self.resetUI()

    def graphReleased(self, event):
        """
        Triggered when mouse is released from plot
        :return:
        """
        if self.function is not None and 'move' in self.function[0]:
            self.function = None
            self.parent.pixel_detail.setText("")

    def graphClicked2(self, event):
        """
        Triggered when the second plot is clicked
        """
        x = event.xdata
        y = event.ydata

        if self.parent.projProc is None or x is None or y is None:
            return

        func = self.function

        if func is None:
            self.function = ['move2', (x, y)]
        elif func[0] == 'zoom':
            # select zoom in area for the second plot
            func.append((2, (x, y)))
            if len(func) == 3:
                self.setZoomIn(func[1], func[2])
        else:
            # call event handler for first plot
            self.graphClicked(event)

    def graphClicked(self, event):
        """
        Triggered when the first plot is clicked
        """
        x = event.xdata
        y = event.ydata
        if self.parent.projProc is None or x is None or y is None:
            return

        if self.function is None:
            self.function = ['move1', (x,y)]
            return

        func = self.function
        # box = self.parent.allboxes[self.name]
        # type = self.parent.boxtypes[self.name]
        center = self.getCenterX()

        distance = int(round(abs(center - x)))

        if func[0] == 'single_peak':
            peaks = func[1]
            
            # Limit: only one peak allowed
            if len(peaks) >= 1:
                QMessageBox.warning(self, "Peak Already Selected", 
                                  "Only one peak allowed. Click Accept or Clear first.")
                return
            
            peaks.append(distance)
            
            # Draw lines (same as original logic, both positive and negative)
            ax = self.graphAxes1
            ax.axvline(center + distance, color='#ff630a', linewidth=2)
            ax.axvline(center - distance, color='#ff630a', linewidth=2)
            ax = self.graphAxes2
            ax.axvline(center + distance, color='#ff630a', linewidth=2)
            ax.axvline(center - distance, color='#ff630a', linewidth=2)
            self.graphCanvas1.draw_idle()
            self.graphCanvas2.draw_idle()
        
        elif func[0] == 'peak_cluster':
            peaks = func[1]
            n_gauss = func[2]
            
            # Check if enough peaks selected
            if len(peaks) >= n_gauss:
                QMessageBox.warning(self, "Enough Peaks", 
                                  f"You already selected {n_gauss} peaks. Click Accept button.")
                return
            
            peaks.append(distance)
            
            # Draw with different color/style to indicate GMM cluster
            ax = self.graphAxes1
            ax.axvline(center + distance, color='#00ff00', linewidth=2, linestyle='--')
            ax.axvline(center - distance, color='#00ff00', linewidth=2, linestyle='--')
            ax.text(center + distance, ax.get_ylim()[1]*0.9, 
                   f'G{len(peaks)}', color='#00ff00', fontsize=10, ha='center')
            ax.text(center - distance, ax.get_ylim()[1]*0.9, 
                   f'G{len(peaks)}', color='#00ff00', fontsize=10, ha='center')
            
            ax = self.graphAxes2
            ax.axvline(center + distance, color='#00ff00', linewidth=2, linestyle='--')
            ax.axvline(center - distance, color='#00ff00', linewidth=2, linestyle='--')
            
            self.graphCanvas1.draw_idle()
            self.graphCanvas2.draw_idle()
            
            # Update button text to show progress
            self.peakClusterButton.setText(f"Accept {len(peaks)}/{n_gauss} Peaks")

        elif func[0] == 'hull':
            hull_range = func[1]
            hull_range.append(distance)

            ax = self.graphAxes1
            ax.axvline(center + distance, color='k', linewidth=2)
            ax.axvline(center - distance, color='k', linewidth=2)

            ax = self.graphAxes2
            ax.axvline(center + distance, color='k', linewidth=2)
            ax.axvline(center - distance, color='k', linewidth=2)

            self.graphCanvas1.draw_idle()
            self.graphCanvas2.draw_idle()

            if len(hull_range) == 2:
                hull_range = tuple(sorted(hull_range))
                self.parent.hull_ranges[self.name] = hull_range
                self.parent.projProc.removeInfo(self.name, 'hists2')
                self.parent.processImage()
        elif func[0] == 'zoom':
            # select zoom in area for the second plot
            func.append((1, (x, y)))
            if len(func) == 3:
                self.setZoomIn(func[1], func[2])

    def drawRectangle(self, canvas, ax, point1, point2):
        """
        Draw a rectangle
        :param fig:
        :param canvas:
        :param point1:
        :param point2:
        :return:
        """
        if self.zoomRect is not None and self.zoomRect in ax.patches:
            ax.patches.remove(self.zoomRect)

        x1 = min(point1[0], point2[0])
        x2 = max(point1[0], point2[0])
        y1 = min(point1[1], point2[1])
        y2 = max(point1[1], point2[1])
        w = abs(x1-x2)
        h = abs(y1-y2)

        self.zoomRect = patches.Rectangle((x1, y1), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted')
        ax.add_patch(self.zoomRect)
        canvas.draw_idle()

    def graphOnMotion1(self, event):
        """
        Trigger when mouse hovers the first plot
        """
        x = event.xdata
        y = event.ydata
        if x is not None and y is not None:
            centerX = self.getCenterX() # this should be the center in the box?
            distance = x - centerX
            hist = self.parent.projProc.info['hists'][self.name]
            if int(round(x)) < len(hist):
                self.parent.pixel_detail.setText("Distance = " + str(round(distance, 3))+", Intensity = "+str(hist[int(round(x))]))
            if self.function is not None:
                if self.function[0] == 'move1' and self.graphMaxBound is not None and self.zoom1 is not None:
                    # change zoom-in location to move around plot
                    func = self.function
                    ax = self.graphAxes1
                    move = (func[1][0] - x, func[1][1] - y)
                    self.zoom1 = getNewZoom(self.zoom1, move, self.graphMaxBound[0][1], self.graphMaxBound[1][1], self.graphMaxBound[1][0])
                    ax.set_xlim(self.zoom1[0])
                    ax.set_ylim(self.zoom1[1])
                    self.graphCanvas1.draw_idle()
                elif self.function[0] == 'zoom' and len(self.function) == 2 and self.function[1][0] == 1:
                    # Draw rectangle
                    start_pt = self.function[1][1]
                    self.drawRectangle(self.graphCanvas1, self.graphAxes1, start_pt, (x, y))

    def graphOnMotion2(self, event):
        """
        Trigger when mouse hovers the first plot
        """
        x = event.xdata
        y = event.ydata
        if x is not None and y is not None:
            centerX = self.getCenterX()
            distance = x - centerX
            all_hists =  self.parent.projProc.info['subtracted_hists']
            if self.name in all_hists:
                hist = all_hists[self.name]
                if int(round(x)) < len(hist):
                    self.parent.pixel_detail.setText(
                        "Distance = " + str(round(distance, 3)) + ", Intensity = " + str(hist[int(round(x))]))
            else:
                self.parent.pixel_detail.setText("Distance = " + str(round(distance, 3)))
            if self.function is not None:
                if self.function[0] == 'move2' and self.graphMaxBound is not None and self.zoom2 is not None:
                    # change zoom-in location to move around plot
                    func = self.function
                    ax = self.graphAxes2
                    move = (func[1][0] - x, func[1][1] - y)
                    self.zoom2 = getNewZoom(self.zoom2, move, self.graphMaxBound[0][1], self.graphMaxBound[1][1], self.graphMaxBound[1][0])
                    ax.set_xlim(self.zoom2[0])
                    ax.set_ylim(self.zoom2[1])
                    self.graphCanvas2.draw_idle()
                elif self.function[0] == 'zoom' and len(self.function) == 2 and self.function[1][0] == 2:
                    # Draw rectangle
                    start_pt = self.function[1][1]
                    self.drawRectangle(self.graphCanvas2, self.graphAxes2, start_pt, (x,y))

    def setManualHullRange(self):
        """
        Trigger when "Set Manual Convex hull ranges" is pressed
        :return:
        """
        if self.hullRangeButton.isChecked():
            self.function = ['hull', []]
        else:
            self.resetUI()

    def addSinglePeak(self):
        """
        Select single peak (limited to one, auto-mirrored)
        """
        if self.peaksButton.isChecked():
            self.peaksButton.setText("Accept Single Peak")
            self.function = ['single_peak', []]
        else:
            self.peaksButton.setText("Select Single Peak")
            peaks = self.function[1]
            
            # Limit to one peak
            if len(peaks) == 0:
                QMessageBox.warning(self, "No Peak Selected", "Please select one peak.")
                self.peaksButton.setChecked(True)
                return
            elif len(peaks) > 1:
                QMessageBox.warning(self, "Too Many Peaks", 
                                  f"Only one peak allowed. Using the first one.")
                peaks = [peaks[0]]
            
            # Auto-mirror (keep original logic)
            op_peaks = [-x for x in peaks]
            all_peaks = peaks + op_peaks  # [+dist, -dist]
            
            # Disable GMM mode for single peak selection
            if hasattr(self.parent, 'gmm_boxes'):
                self.parent.gmm_boxes[self.name] = False
            if 'gmm_mode' in self.parent.projProc.info:
                self.parent.projProc.info['gmm_mode'][self.name] = False
            
            # Clear old fit_results to force re-fitting without GMM
            if 'fit_results' in self.parent.projProc.info and \
               self.name in self.parent.projProc.info['fit_results']:
                del self.parent.projProc.info['fit_results'][self.name]
            
            self.function = None
            self.parent.addPeakstoBox(self.name, all_peaks)
    
    def addPeakCluster(self):
        """
        Select multiple peaks for GMM fitting (shared sigma, auto-mirrored)
        """
        if self.peakClusterButton.isChecked():
            n_gauss = self.gmmNumGaussSpinBox.value()
            self.peakClusterButton.setText(f"Accept {n_gauss} Peaks")
            self.function = ['peak_cluster', [], n_gauss]
            
            # Show status hint
            self.parent.setLeftStatus(
                f"Click to select {n_gauss} approximate peak positions "
                f"(will be mirrored, total {n_gauss*2} peaks for GMM fitting)")
        else:
            self.peakClusterButton.setText("Select Peak Cluster (GMM)")
            peaks = self.function[1]
            n_gauss = self.function[2]
            
            # Check count
            if len(peaks) != n_gauss:
                QMessageBox.warning(self, "Incorrect Peak Count", 
                                  f"Please select exactly {n_gauss} peaks. You selected {len(peaks)}.")
                self.peakClusterButton.setChecked(True)
                return
            
            # Auto-mirror (keep original logic)
            op_peaks = [-x for x in peaks]
            all_peaks = peaks + op_peaks  # positive and negative distances
            
            # Enable GMM mode and fit
            self.fitGaussianMixture(all_peaks, n_gauss * 2)
            self.function = None
    
    def fitGaussianMixture(self, all_peaks, total_n_gaussians):
        """
        Fit peak cluster using GMM (shared sigma)
        :param all_peaks: All peaks (including mirrored ones)
        :param total_n_gaussians: Total number of Gaussians
        """
        QApplication.setOverrideCursor(Qt.WaitCursor)
        
        try:
            # Enable GMM mode BEFORE processing
            # Store in parent's gmm_boxes dict so it persists
            if not hasattr(self.parent, 'gmm_boxes'):
                self.parent.gmm_boxes = {}
            self.parent.gmm_boxes[self.name] = True
            
            # Also set in projProc.info (will be used during fitting)
            if 'gmm_mode' not in self.parent.projProc.info:
                self.parent.projProc.info['gmm_mode'] = {}
            self.parent.projProc.info['gmm_mode'][self.name] = True
            
            # Clear old fit_results to force re-fitting with new GMM mode
            if 'fit_results' in self.parent.projProc.info and \
               self.name in self.parent.projProc.info['fit_results']:
                del self.parent.projProc.info['fit_results'][self.name]
            
            # Add peaks and process
            self.parent.addPeakstoBox(self.name, all_peaks)
            
            # Re-ensure GMM mode is set after processing
            if 'gmm_mode' not in self.parent.projProc.info:
                self.parent.projProc.info['gmm_mode'] = {}
            self.parent.projProc.info['gmm_mode'][self.name] = True
            
            # Check results after processing
            if 'fit_results' in self.parent.projProc.info and \
               self.name in self.parent.projProc.info['fit_results']:
                
                result = self.parent.projProc.info['fit_results'][self.name]
                
                # Enable parameter editor
                self.paramEditorButton.setEnabled(True)
                
                # Show results
                error = result.get('error', 0)
                common_sigma = result.get('common_sigma', None)
                
                # Debug: print actual result to verify
                print(f"DEBUG fitGaussianMixture: box={self.name}")
                print(f"  common_sigma in result: {common_sigma}")
                print(f"  Full result keys: {list(result.keys())}")
                
                if common_sigma is None:
                    common_sigma = 0
                    print("  WARNING: common_sigma not found in result!")
                
                msg = f"GMM Fitting Complete!\n\n"
                msg += f"Total Gaussians: {total_n_gaussians}\n"
                msg += f"Common Sigma: {common_sigma:.4f}\n"
                msg += f"Fitting Error (1-R²): {error:.6f}"
                
                QMessageBox.information(self, "GMM Fit Complete", msg)
            
        except Exception as e:
            import traceback
            QMessageBox.critical(self, "GMM Fitting Error", 
                               f"Error: {str(e)}\n\n{traceback.format_exc()}")
        finally:
            QApplication.restoreOverrideCursor()
    
    def openParameterEditor(self):
        """
        Open parameter editor dialog
        """
        if 'fit_results' not in self.parent.projProc.info or \
           self.name not in self.parent.projProc.info['fit_results']:
            QMessageBox.warning(self, "No Fit Results", 
                              "Please fit peaks first before opening parameter editor.")
            return
        
        
        # Open parameter editor dialog
        try:
            from .GMMParameterEditorDialog import GMMParameterEditorDialog
            dialog = GMMParameterEditorDialog(self, self.name)
            dialog.exec_()
        except ImportError as e:
            QMessageBox.critical(self, "Import Error", 
                               f"Cannot import GMMParameterEditorDialog: {str(e)}")

    def refreshUI(self):
        #clear the axes
        self.need_update = True
        self.updateUI()

        #redraw the lines
        center = self.getCenterX()

        for distance in self.function[1]:
            ax = self.graphAxes1
            ax.axvline(center + distance, color='#ff630a', linewidth=2)
            ax.axvline(center - distance, color='#ff630a', linewidth=2)

            ax = self.graphAxes2
            ax.axvline(center + distance, color='#ff630a', linewidth=2)
            ax.axvline(center - distance, color='#ff630a', linewidth=2)

        self.graphCanvas1.draw_idle()
        self.graphCanvas2.draw_idle()

    def clearPeaks(self):
        """
        Trigger when "Clear Peaks" is pressed
        :return:
        """
        if self.function is None:
            temp = self.parent.peaks[self.name]
            curr_peaks = [temp[i] for i in range(int(len(temp) / 2) - 1)]
        else:
            curr_peaks = self.function[1]

        if self.function is None:
            self.parent.addPeakstoBox(self.name, [])
            self.function = ['single_peak', curr_peaks]
            self.refreshUI()
            self.peaksButton.setText("Accept Single Peak")
            self.peaksButton.setChecked(True)
            return

        if self.function[0] in ('peaks', 'single_peak', 'peak_cluster') and curr_peaks != []:
            #Remove the most recent peak
            self.function[1].pop()
            self.refreshUI()


    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard
        """
        key = event.key()

        if key == Qt.Key_Escape:
            self.resetUI()

        self.parent.keyPressEvent(event)

    def clearFlags(self):
        """
        clear all flags
        :return:
        """
        self.need_update = True
        self.function = None
        self.peaksButton.setText("Select Single Peak")
        self.peakClusterButton.setText("Select Peak Cluster (GMM)")
        #self.clearPeakButton.setVisible(False)
        for b in self.checkableButtons:
            self.syncUI = True
            b.setChecked(False)
            self.syncUI = False

    def resetUI(self):
        """
        clear flags and update UI
        :return:
        """
        self.clearFlags()
        self.updateUI()

    def updateUI(self):
        """
        Draw plots and display results in text
        :return:
        """
        if self.parent.projProc is None or not self.need_update:
            return

        self.syncUI = True
        
        self.lines = []
        
        # Update graphs
        info = self.parent.projProc.info
        name = self.name
        if not name in info['box_names']:
            return

        hist = info['hists'][name]
        fit_results = info['fit_results']
        subtracted_hists = info['subtracted_hists']
        all_baselines = info['baselines']
        all_centroids = info['centroids']
        all_widths = info['widths']
        all_peaks = info['moved_peaks']
        all_areas = info['areas']
        bgsubs = info['bgsubs']
        merid_bgs = info['merid_bg']
        hull_ranges = info['hull_ranges']

        ax = self.graphAxes1
        ax.cla()

        ax2 = self.graphAxes2
        ax2.cla()
        
        ax.set_title("Projection")
        ax2.set_title("Background Subtracted Projection")

        if self.histChkBx.isChecked():
            ax.plot(hist, color='k')

        if name in merid_bgs:
            self.meridBckGrndChkBx.setChecked(merid_bgs[name])

        if name in fit_results:
            self.editMainPeakButton.setEnabled(True)
            xs = np.arange(0, len(hist))
            model = info['fit_results'][name]
            convex_hull = hist - info['hists2'][name]

            if self.subHistChkBx.isChecked() and name in subtracted_hists:
                ax2.plot(subtracted_hists[name], color='k')

            if self.fitmodelChkBx.isChecked():
                model_hist = layerlineModel(xs, **model)
                subtracted_model = model_hist-layerlineModelBackground(xs, **model)

                if bgsubs[name] == 1:
                    # Add convexhull background
                    model_hist = model_hist + convex_hull

                ax.plot(model_hist, color='g')
                if bgsubs[name] == 2: # if no background subtraction, use the fit from the original model
                    ax2.plot(model_hist, color='g')
                else:
                    ax2.plot(subtracted_model, color='g')

            if self.bgChkBx.isChecked():
                if bgsubs[name] == 1:
                    # Add convex hull background
                    ax.fill_between(xs, min(convex_hull), convex_hull, facecolor='b', alpha=0.3)
                else:
                    # Add 3 Gaussians
                    background = layerlineBackground(xs, **model)
                    meridian = layerlineModelBackground(xs, **model)
                    if self.meridBckGrndChkBx.isChecked():
                        meridian_bg = meridianBackground(xs, **model) + layerlineBackground(xs, **model)
                        ax.fill_between(xs, meridian, meridian_bg, facecolor='r', alpha=0.3)
                        ax.fill_between(xs, meridian_bg, background, facecolor='y', alpha=0.3)
                    else:
                        ax.fill_between(xs, meridian, background, facecolor='r', alpha=0.3)
                    ax.fill_between(xs, 0, background, facecolor='b', alpha=0.3)

            if self.centerChkBx.isChecked():
                # Add center line
                ax.axvline(model['centerX'], color='c', alpha=0.7)
                ax2.axvline(model['centerX'], color='c', alpha=0.7)

            if self.peaksChkBx.isChecked():
                # display model peaks
                i = 0
                while 'p_' + str(i) in model:
                    p = model['p_' + str(i)]
                    i += 1
                    # ax.axvline(model['centerX'] - p, color='r', alpha=0.7)
                    center_line = ax.axvline(model['centerX'] + p, color='r', alpha=0.7)
                    self.lines.append(center_line)

            if self.maxPeaksChkBx.isChecked():
                peaks = all_peaks[name]
                for p in peaks:
                    d = p - model['centerX']
                    # ax2.axvline(model['centerX'] - d, color='b', alpha=0.7)
                    ax2.axvline(model['centerX'] + d, color='b', alpha=0.7)
            # max intensity locations
            # if name in all_peaks:
            #     peaks = all_peaks[name]
            #     for p in peaks:
            #         ax2.axvline(p, color='r', alpha=0.7)

            if name in all_centroids and name in all_baselines and (self.centroidChkBx.isChecked() or self.baselineChkBx.isChecked()):
                # Add baselines and centroids
                centroids = all_centroids[name]
                baselines = all_baselines[name]
                widths = all_widths[name]
                centerX = model['centerX']
                i = 0
                while 'p_' + str(i) in model:
                    c = centroids[i]
                    b = baselines[i]
                    w = widths[i]
                    if self.centroidChkBx.isChecked():
                        ax2.axvline(centerX + c, color='#ff4732')
                        # ax2.axvline(centerX - c, color='#ff4732')
                    if self.baselineChkBx.isChecked():
                        ax2.plot(((centerX + c) - w, (centerX + c) + w), (b, b), color='y')
                        # ax2.plot(((centerX - c) - w, (centerX - c) + w), (b, b), color='y')

                    i += 1

        if self.hullRangeChkBx.isChecked() and bgsubs[name] == 1 and name in hull_ranges:
            # Color area OUTSIDE convex hull range
            centerX = self.getCenterX()

            ax.axvspan(centerX - hull_ranges[name][0], centerX + hull_ranges[name][0], alpha=0.5, color='k')
            ax.axvspan(0, centerX - hull_ranges[name][1], alpha=0.5, color='k')
            ax.axvspan(centerX + hull_ranges[name][1], len(hist), alpha=0.5, color='k')

            # Update spin box
            self.startHull.setValue(hull_ranges[name][0])
            self.endHull.setValue(hull_ranges[name][1])

        if self.zoom1 is not None:
            ax.set_xlim(self.zoom1[0])
            ax.set_ylim(self.zoom1[1])
        else:
            # print(info['boxes'][name])
            # pairs = [item for item in info['boxes'][name] if isinstance(item, tuple) and len(item) == 2]
            # min_tuple = min(pairs, key=lambda x: x[0])
            ax.set_xlim((0, len(hist)))
            # ax.set_xticks(np.arange(0, len(hist)))
            # ax.set_xticklabels(np.arange(-len(hist)/2, len(hist)/2))
            
            # locs = ax.get_xticks().tolist()
            # labels=[x.get_text() for x in ax.get_xticklabels()]
            
            # step_size = len(labels) // 10+1
            
            # reduced_labels = labels[::step_size]
            # reduced_locs = locs[::step_size]
            
            # reduced_labels.append('0.0')
            # reduced_locs.append(0.0 + len(hist)/2)
            
            # new_locs = sorted(reduced_locs)
            # new_labels = sorted(reduced_labels, key=float)
            
            # index = min(range(len(new_labels)), key=lambda i: abs(float(new_labels[i])))
            
            # left = float (new_labels[index-1])
            # right = float(new_labels[index+1])
            
            # if abs(left) < abs(right):
            #     del new_labels[index-1]
            #     del new_locs[index-1]
            # else:
            #     del new_labels[index+1]
            #     del new_locs[index+1]

            # ax.xaxis.set_major_locator(FixedLocator(new_locs))
            # ax.set_xticklabels(new_labels)
            
            # new_labels = sorted(labels, key=float)
            
            # ax.xaxis.set_major_locator(FixedLocator(locs))
            # ax.set_xticklabels(new_labels)
    

        if self.zoom2 is not None:
            ax2.set_xlim(self.zoom2[0])
            ax2.set_ylim(self.zoom2[1])
        else:
            ax2.set_xlim((0, len(hist)))
            # ax2.set_xticks(np.arange(0, len(hist)))
            # ax2.set_xticklabels(np.arange(-len(hist)/2, len(hist)/2))
            # ax2.xaxis.set_major_locator(FixedLocator(new_locs))
            # ax2.set_xticklabels(new_labels)

        if self.graphMaxBound is None:
            self.graphMaxBound = [ax.get_xlim(), ax.get_ylim()]

        self.graphFigure1.tight_layout()
        self.graphCanvas1.draw()
        self.graphFigure2.tight_layout()
        self.graphCanvas2.draw()

        # Update Table
        
        if name in all_centroids and name in all_baselines:
            centroids = all_centroids[name]
            baselines = all_baselines[name]
            widths = all_widths[name]
            areas = all_areas[name]
            nPeaks = len(centroids)
            fit_result = fit_results[name]
            peaks = np.array(all_peaks[name]) - fit_result['centerX']
            self.resultTable1.setRowCount(nPeaks)
            self.resultTable2.setRowCount(nPeaks)

            for i in range(nPeaks):
                center = round(fit_result['p_'+str(i)], 2)
                sigma = round(fit_result['sigma'+str(i)], 2)
                area = round(fit_result['amplitude' + str(i)], 2)

                item = QTableWidgetItem(str(round(peaks[i], 2)))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 0, item)

                item = QTableWidgetItem(str(center))
                #item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 1, item)

                item = QTableWidgetItem(str(sigma))
                # item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 2, item)

                item = QTableWidgetItem(str(area))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 3, item)

                centroid = round(centroids[i], 2)
                baseline = round(baselines[i], 2)
                width = round(widths[i], 2)
                area = round(areas[i], 2)

                item = QTableWidgetItem(str(baseline))
                self.resultTable2.setItem(i, 0, item)

                item = QTableWidgetItem(str(centroid))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 1, item)

                item = QTableWidgetItem(str(width))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 2, item)

                item = QTableWidgetItem(str(area))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 3, item)
        
        self.need_update = False
        self.syncUI = False
        
        
    def on_press(self, event):
        if event.inaxes != self.graphAxes1:
            return
        for idx, line in enumerate(self.lines):
            if line.contains(event)[0]:
                self.dragging = True
                self.dragged_line = line
                self.fixed_indices.append(idx)
                break

    def on_motion(self, event):

        if not self.dragging or event.inaxes != self.graphAxes1:
            return
        
        self.dragged_line.set_xdata([event.xdata, event.xdata])
        self.graphAxes1.draw_artist(self.graphAxes1.patch)
        self.graphAxes1.draw_artist(self.dragged_line)
        # self.graphFigure1.canvas.blit(self.graphAxes1.bbox)
        self.graphFigure1.canvas.draw()
        

    def on_release(self, event):
        if not self.dragging:
            return
        self.dragging = False
        self.dragged_line = None
        #fit_gaussians(fixed_indices)
        print(self.fixed_indices)
        
        hist = self.parent.projProc.info['hists'][self.name]
        
        print(event.xdata - len(hist)/2)
        
        self.parent.projProc.setGaussCenter(self.name, self.fixed_indices[-1], event.xdata - len(hist)/2)
        self.parent.processImage()
        
        self.graphFigure1.canvas.draw()  # Ensure the canvas is updated
        