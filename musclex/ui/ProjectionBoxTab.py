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
from .EditPeakDetailsDialog import EditPeakDetailsDialog
from .widgets.collapsible_right_panel import CollapsibleRightPanel
from ..modules.ProjectionProcessor import layerlineModel, layerlineModelBackground, layerlineBackground, meridianBackground
from ..utils.image_processor import getNewZoom

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
        self.auto_zoom_applied = False  # Flag to track if auto-zoom has been applied
        
        self.dragging = False
        self.dragged_line = None
        self.fixed_indices = []
        self.lines = []
        
        # Parameter editor state
        self.param_editor_active = False
        self.param_editor_dialog = None  # Store reference to dialog
        self.overlay_patches = []  # List to store overlay patches for both axes
        self.overlay_drag_start = None
        self.overlay_dragging = False
        self.dragged_edge = None  # Track which edge is being dragged ('left_inner', 'left_outer', 'right_inner', 'right_outer')
        self.edge_drag_threshold = 5  # Pixel threshold for edge detection
        
        # Preview parameters for real-time editing (used by parameter editor dialog)
        self.preview_params = None  # When not None, updateUI uses this instead of fit_results
        self.preview_hull_range = None  # When not None, updateUI uses this instead of hull_ranges
        
        self.initUI()
        self.setAllToolTips()
        self.setConnections()
        self.clearSettingsFirstLaunch()
        
    def clearSettingsFirstLaunch(self):
        settings = QSettings("Checkboxes", "PeakDetails")
        if not settings.contains("initial_setup_done"):
            settings.clear()
            settings.setValue("initial_setup_done", True)
    
    def get_box(self):
        """
        Get the ProcessingBox object for this tab.
        
        Strategy:
        1. If projProc exists → return actual processing box (projProc.boxes)
        2. If projProc doesn't exist → return folder template box (self.parent.boxes)
           (Used during UI initialization before image is loaded)
        
        Returns:
            ProcessingBox or None
        """
        if self.parent.projProc is not None:
            return self.parent.projProc.boxes.get(self.name)
        else:
            # Fallback to folder template for UI initialization
            return self.parent.boxes.get(self.name)

    def getCenterX(self):
        """
        Get center X
        :return: center X
        """
        if self.parent.projProc is None:
            return 0

        # Use the helper method to get ProcessingBox
        box_obj = self.get_box()
        if box_obj is None:
            return 0
        
        coords = box_obj.coordinates
        box_type = box_obj.type
        
        if box_type == 'h':
            start_x = coords[0][0]
            if self.parent.centerx is None:
                self.centerX = self.parent.projProc.orig_img.shape[1] / 2. - 0.5 - start_x
            else:
                self.centerX = self.parent.centerx - start_x
        elif box_type == 'oriented':
            start_x = coords[0][0]
            self.centerX = coords[6][0] - start_x
        else:
            if box_type == 'v':
                start_y = coords[1][0]
            else:
                start_y = coords[0][1]
            if self.parent.centery is None:
                self.centerX = self.parent.projProc.orig_img.shape[0] / 2. - 0.5 - start_y
            else:
                self.centerX = self.parent.centery - start_y

        return self.centerX
    
    def _getRenderParams(self):
        """
        Get parameters for rendering (supports preview mode)
        
        Returns:
            dict: Parameters to use for rendering
                  - In preview mode (dialog open): returns preview_params (temporary)
                  - In normal mode: returns fit_results (persistent)
        """
        name = self.name
        
        # Preview mode (when parameter editor dialog is open)
        if self.preview_params is not None:
            return self.preview_params
        
        # Normal mode: use fit_results from projProc
        if self.parent.projProc is not None:
            if name in self.parent.projProc.boxes:
                box = self.parent.projProc.boxes[name]
                if box.fit_results is not None:
                    return box.fit_results
        
        return None

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

        # CollapsibleRightPanel replaces the old QScrollArea + optionsFrame
        self.right_panel = CollapsibleRightPanel(
            parent=self,
            title=f"{self.name} Options",
            settings_key=f"projection_box/{self.name}/right_panel",
            start_visible=True
        )
        self.right_panel.setFixedWidth(400)

        self.displayOptionsGroup = QGroupBox("Display Options")
        self.dispOptLayout = QGridLayout(self.displayOptionsGroup)

        self.histChkBx = QCheckBox("Original Projection")
        self.histChkBx.setChecked(True)
        self.hullRangeChkBx = QCheckBox('Hull Range')
        box = self.get_box()
        self.hullRangeChkBx.setEnabled(box.bgsub == 1 if box else False)
        self.hullRangeChkBx.setChecked(box.bgsub == 1 if box else False)
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
        
        # New: Peak Tolerance label and spinner for search distance
        self.peakToleranceLabel = QLabel("Peak Tolerance")
        self.peakToleranceSpinBox = QDoubleSpinBox()
        self.peakToleranceSpinBox.setRange(0.1, 50.0)
        self.peakToleranceSpinBox.setValue(2.0)
        self.peakToleranceSpinBox.setSingleStep(0.5)
        self.peakToleranceSpinBox.setDecimals(1)
        self.peakToleranceSpinBox.setToolTip("Peak tolerance - search distance for peak fitting")

        # New: Sigma Tolerance label and spinner for sigma bounds initialization (percentage)
        self.sigmaToleranceLabel = QLabel("Sigma Tolerance (%)")
        self.sigmaToleranceSpinBox = QDoubleSpinBox()
        self.sigmaToleranceSpinBox.setRange(1.0, 200.0)
        self.sigmaToleranceSpinBox.setValue(100.0)
        self.sigmaToleranceSpinBox.setSingleStep(5.0)
        self.sigmaToleranceSpinBox.setDecimals(1)
        self.sigmaToleranceSpinBox.setSuffix("%")
        self.sigmaToleranceSpinBox.setToolTip("Sigma tolerance percentage - used to initialize sigma/common_sigma bounds (± tolerance% of value)")
        
        self.clearPeakButton = QPushButton("Clear Peaks")
        self.clearPeakButton.setVisible(True)
        
        # New: Parameter Editor button
        self.paramEditorButton = QPushButton("Open Parameter Editor")
        self.paramEditorButton.setEnabled(True)
        
        self.meridBckGrndChkBx = QCheckBox("Meridional Peak")
        self.meridBckGrndChkBx.setChecked(True)
        box = self.get_box()
        self.meridBckGrndChkBx.setHidden(box.bgsub != 0 if box else True)
        self.editMainPeakButton = QPushButton("Edit Meridional Peak")
        self.editMainPeakButton.setEnabled(False)
        self.refitButton = QPushButton("Refit")
        self.refitButton.setEnabled(False)
        
        self.hullRangeButton = QPushButton("Set Manual Convex Hull Range")
        self.hullRangeButton.setCheckable(True)
        self.hullRangeButton.setHidden(box.bgsub != 1 if box else True)
        width = int(np.ceil(abs(box.coordinates[0][0]-box.coordinates[0][1])/2.)) if box else 100

        self.startHull = QSpinBox()
        self.startHull.setRange(0, width)
        self.startHull.setKeyboardTracking(False)
        self.startHull.setHidden(box.bgsub != 1 if box else True)
        self.endHull = QSpinBox()
        self.endHull.setRange(0, width)
        self.endHull.setKeyboardTracking(False)
        self.endHull.setHidden(box.bgsub != 1 if box else True)
        self.checkableButtons.append(self.hullRangeButton)
        
        # Layout for new buttons
        self.settingLayout.addWidget(self.peaksButton, 0, 0, 1, 2)
        self.settingLayout.addWidget(self.peakClusterButton, 1, 0, 1, 2)
        self.settingLayout.addWidget(self.peakToleranceLabel, 2, 0, 1, 1)
        self.settingLayout.addWidget(self.peakToleranceSpinBox, 2, 1, 1, 1)
        self.settingLayout.addWidget(self.sigmaToleranceLabel, 3, 0, 1, 1)
        self.settingLayout.addWidget(self.sigmaToleranceSpinBox, 3, 1, 1, 1)
        self.settingLayout.addWidget(self.clearPeakButton, 4, 0, 1, 2)
        self.settingLayout.addWidget(self.paramEditorButton, 5, 0, 1, 2)
        self.settingLayout.addWidget(self.hullRangeButton, 6, 0, 1, 2)
        box = self.get_box()
        if box and box.bgsub == 1:
            self.settingLayout.addWidget(QLabel("Start"), 7, 0, 1, 1, Qt.AlignCenter)
            self.settingLayout.addWidget(QLabel("End"), 7, 1, 1, 1, Qt.AlignCenter)
        self.settingLayout.addWidget(self.startHull, 8, 0, 1, 1)
        self.settingLayout.addWidget(self.endHull, 8, 1, 1, 1)
        self.settingLayout.addWidget(self.meridBckGrndChkBx, 9, 0, 1, 2)
        self.settingLayout.addWidget(self.editMainPeakButton, 10, 0, 1, 2)
        self.settingLayout.addWidget(self.refitButton, 11, 0, 1, 2)

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

        # Add widgets to CollapsibleRightPanel content area (scrollable)
        self.right_panel.add_widget(self.displayOptionsGroup)
        self.right_panel.add_widget(self.settingGroup)
        self.right_panel.add_widget(QLabel("<h3>Model Peak Information</h3>"))
        self.right_panel.add_widget(self.resultTable1)
        self.right_panel.add_widget(QLabel("<h3>Centroid Peak Information</h3>"))
        self.right_panel.add_widget(self.resultTable2)

        # Note: Navigation controls (prev/next) are shared across all tabs.
        # The parent GUI moves the shared navControls into this right_panel's
        # bottom area when this tab becomes active (via onTabChanged).

        self.graphLayout = QVBoxLayout()
        self.graphLayout.addWidget(self.graphCanvas1)
        self.graphLayout.addWidget(self.graphCanvas2)
        self.graphLayout.addWidget(QLabel("<h5>HINT: Use the 'Zoom in' and 'Zoom out' buttons in the top right to zoom in and out of either canvas.</h5>"))
        self.tabLayout.addLayout(self.graphLayout)
        self.tabLayout.addWidget(self.right_panel)

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

        # TEMP: disable table click events (to avoid hooking itemChanged handlers on click)
        # self.resultTable1.itemClicked.connect(self.handleTable1Event)
        # self.resultTable2.itemClicked.connect(self.handleTable2Event)

        # Note: prev/next navigation is handled by shared NavigationControls
        # which is moved into this tab's right_panel by the parent GUI

        self.graphFigure1.canvas.mpl_connect('button_press_event', self.graphClicked)
        self.graphFigure2.canvas.mpl_connect('button_press_event', self.graphClicked2)
        self.graphFigure1.canvas.mpl_connect('motion_notify_event', self.graphOnMotion1)
        self.graphFigure2.canvas.mpl_connect('motion_notify_event', self.graphOnMotion2)
        self.graphFigure1.canvas.mpl_connect('button_release_event', self.graphReleased)
        self.graphFigure2.canvas.mpl_connect('button_release_event', self.graphReleased)
        self.graphFigure1.canvas.mpl_connect('figure_leave_event', self.graphReleased)
        self.graphFigure2.canvas.mpl_connect('figure_leave_event', self.graphReleased)
        # TEMP: disable draggable vertical-line behavior on graphFigure1
        # self.graphFigure1.canvas.mpl_connect('button_press_event', self.on_press)
        # self.graphFigure1.canvas.mpl_connect('motion_notify_event', self.on_motion)
        # self.graphFigure1.canvas.mpl_connect('button_release_event', self.on_release)
    
    def editMainPeak(self):
        """
        Open the Meridional Peak Parameter Editor dialog.
        Uses the new EditPeakDetailsDialog with integrated Refit & Save.
        """
        box = self.get_box()
        if box is None or box.fit_results is None:
            QMessageBox.warning(
                self,
                "No Fit Results",
                "Please fit peaks first before editing peak parameters."
            )
            return
        
        try:
            dialog = EditPeakDetailsDialog(self, self.name)
            dialog.exec_()
            # No need to handle result - dialog handles Refit & Save internally
        except ValueError as e:
            QMessageBox.warning(self, "Error", str(e))
            
    def refit(self):
        """
        Legacy refit method - kept for backward compatibility.
        The new EditPeakDetailsDialog handles refit internally.
        """
        if hasattr(self, 'newinfo') and self.newinfo:
            self.parent.projProc.removeInfo(self.name, 'fit_results')
            # Update main_peak_info in ProcessingState (not per-box)
            self.parent.projProc.state.main_peak_info[self.name] = self.newinfo
            self.parent.processImage() 
            self.refitButton.setEnabled(False)
            self.refitButton.setStyleSheet("") 

    def meridBckGrndChanged(self):
        """
        Trigger when meridian background is changed (on/off)
        """
        if self.parent.projProc is not None and not self.syncUI:
            self.parent.projProc.removeInfo(self.name, 'fit_results')
            # Directly access projProc.boxes for modifications
            box = self.parent.projProc.boxes.get(self.name)
            if box:
                box.merid_bg = self.meridBckGrndChkBx.isChecked()
            self.parent.processImage()

    def hullRangeChanged(self):
        """
        Trigger when convex hull range is changed (via spinbox)
        """
        if self.parent.projProc is not None and not self.syncUI:
            # Directly access projProc.boxes for modifications
            box = self.parent.projProc.boxes.get(self.name)
            if box:
                box.hull_range = (self.startHull.value(), self.endHull.value())
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

    def autoZoomToHullRange(self):
        """
        Auto-zoom graph1 to center the hull_range in the current display.
        
        Strategy:
        - X axis: Keep the same display width as previous, shift so hull_range center
          is in the middle of the view.
        - Y axis: Only expand if hull data range exceeds current display range;
          otherwise keep previous Y zoom.
        
        Only applies to graph1 (original projection), graph2 stays at default zoom.
        """
        if self.parent.projProc is None:
            return
        
        box = self.get_box()
        if box is None or box.hull_range is None or box.hist is None:
            return
        
        hist = box.hist
        centerX = self.getCenterX()
        start, end = box.hull_range
        
        # === Get previous zoom ranges ===
        prev_x_min, prev_x_max = self.zoom1[0]
        prev_y_min, prev_y_max = self.zoom1[1]
        
        # === X axis: Keep previous width, center hull_range in the display ===
        prev_width = prev_x_max - prev_x_min
        hull_center = centerX + (start + end) / 2
        x_min = hull_center - prev_width / 2
        x_max = hull_center + prev_width / 2
        
        # Clamp to valid range
        x_min = max(0, x_min)
        x_max = min(len(hist), x_max)
        
        # === Y axis: only expand if hull data range exceeds current display ===
        # Compute hull region data min/max
        region_start = int(max(0, centerX + start))
        region_end = int(min(len(hist), centerX + end))
        region_data = hist[region_start:region_end] if region_start < region_end else []
        
        if len(region_data) > 0:
            y_min_hull = np.min(region_data)
            y_max_hull = np.max(region_data)
            hull_y_span = y_max_hull - y_min_hull
            prev_y_span = prev_y_max - prev_y_min
            
            if hull_y_span > prev_y_span:
                # Hull data exceeds display range — expand with 10% padding
                y_padding = hull_y_span * 0.1
                y_min = y_min_hull - y_padding
                y_max = y_max_hull + y_padding
            else:
                # Hull data fits — keep previous Y width, center on hull data midpoint
                hull_y_center = (y_min_hull + y_max_hull) / 2
                y_min = hull_y_center - prev_y_span / 2
                y_max = hull_y_center + prev_y_span / 2
        else:
            # Fallback: keep previous Y range
            y_min = prev_y_min
            y_max = prev_y_max
        
        # Set zoom1 (only for graph1)
        self.zoom1 = [(x_min, x_max), (y_min, y_max)]
        
        # Note: zoom2 stays None (graph2 will use default full view)

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
        # Handle overlay drag release
        if self.overlay_dragging:
            self.overlay_dragging = False
            self.overlay_drag_start = None
            self.dragged_edge = None
            return
        
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

        # Check if parameter editor is active and we're clicking on overlay edge
        box = self.get_box()
        if self.param_editor_active and box and box.hull_range is not None:
            center = self.getCenterX()
            # Use preview hull range if available, otherwise use box hull range
            if self.preview_hull_range is not None:
                start, end = self.preview_hull_range
            else:
                start, end = box.hull_range
            
            # Calculate edge positions
            left_outer = center - end
            left_inner = center - start
            right_inner = center + start
            right_outer = center + end
            
            # Convert threshold from pixels to data coordinates
            xlim = self.graphAxes2.get_xlim()
            fig_width = self.graphFigure2.get_figwidth() * self.graphFigure2.dpi
            threshold_data = (xlim[1] - xlim[0]) * self.edge_drag_threshold / fig_width
            
            # Check which edge is being clicked (prioritize edges over regions)
            if abs(x - left_outer) < threshold_data:
                self.overlay_dragging = True
                self.dragged_edge = 'left_outer'
                self.overlay_drag_start = x
                return
            elif abs(x - left_inner) < threshold_data:
                self.overlay_dragging = True
                self.dragged_edge = 'left_inner'
                self.overlay_drag_start = x
                return
            elif abs(x - right_inner) < threshold_data:
                self.overlay_dragging = True
                self.dragged_edge = 'right_inner'
                self.overlay_drag_start = x
                return
            elif abs(x - right_outer) < threshold_data:
                self.overlay_dragging = True
                self.dragged_edge = 'right_outer'
                self.overlay_drag_start = x
                return
            
            # If not clicking on edges, check if clicking inside regions (for whole-region dragging)
            in_left_region = (left_outer < x < left_inner)
            in_right_region = (right_inner < x < right_outer)
            
            if in_left_region or in_right_region:
                self.overlay_dragging = True
                self.dragged_edge = None  # None means dragging the whole region
                self.overlay_drag_start = x
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

        # Check if parameter editor is active and we're clicking on overlay edge
        box = self.get_box()
        if self.param_editor_active and box and box.hull_range is not None:
            center = self.getCenterX()
            # Use preview hull range if available, otherwise use box hull range
            if self.preview_hull_range is not None:
                start, end = self.preview_hull_range
            else:
                start, end = box.hull_range
            
            # Calculate edge positions
            left_outer = center - end
            left_inner = center - start
            right_inner = center + start
            right_outer = center + end
            
            # Convert threshold from pixels to data coordinates
            xlim = self.graphAxes1.get_xlim()
            fig_width = self.graphFigure1.get_figwidth() * self.graphFigure1.dpi
            threshold_data = (xlim[1] - xlim[0]) * self.edge_drag_threshold / fig_width
            
            # Check which edge is being clicked (prioritize edges over regions)
            if abs(x - left_outer) < threshold_data:
                self.overlay_dragging = True
                self.dragged_edge = 'left_outer'
                self.overlay_drag_start = x
                return
            elif abs(x - left_inner) < threshold_data:
                self.overlay_dragging = True
                self.dragged_edge = 'left_inner'
                self.overlay_drag_start = x
                return
            elif abs(x - right_inner) < threshold_data:
                self.overlay_dragging = True
                self.dragged_edge = 'right_inner'
                self.overlay_drag_start = x
                return
            elif abs(x - right_outer) < threshold_data:
                self.overlay_dragging = True
                self.dragged_edge = 'right_outer'
                self.overlay_drag_start = x
                return
            
            # If not clicking on edges, check if clicking inside regions (for whole-region dragging)
            in_left_region = (left_outer < x < left_inner)
            in_right_region = (right_inner < x < right_outer)
            
            if in_left_region or in_right_region:
                self.overlay_dragging = True
                self.dragged_edge = None  # None means dragging the whole region
                self.overlay_drag_start = x
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
            
            # Add the selected peak
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
            self.peakClusterButton.setText(f"Accept {len(peaks)} Peaks")

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
                
                # CRITICAL: Must have projProc to set hull_range
                if self.parent.projProc is None:
                    return
                
                # Store hull range in ProcessingBox (directly access projProc.boxes)
                box = self.parent.projProc.boxes.get(self.name)
                if box:
                    box.hull_range = hull_range
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
            self.zoomRect.remove()

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
            # Handle overlay dragging (edge or whole region)
            if self.overlay_dragging and self.overlay_drag_start is not None:
                delta_x = x - self.overlay_drag_start
                self.overlay_drag_start = x
                
                # If dragged_edge is None, it means dragging the whole region (peaks + hull_range)
                # Otherwise, dragging a specific edge (hull_range bound only)
                if self.dragged_edge is None:
                    self.updateOverlayAndPeaks(delta_x)  # Whole region drag
                else:
                    self.updateHullRangeBound(delta_x)  # Edge drag
                return
            
            # Change cursor when hovering over edges (when parameter editor is active)
            if self.param_editor_active:
                box = self.get_box()
                if box and box.hull_range is not None:
                    center = self.getCenterX()
                    # Use preview hull range if available, otherwise use box hull range
                    if self.preview_hull_range is not None:
                        start, end = self.preview_hull_range
                    else:
                        start, end = box.hull_range
                    
                    left_outer = center - end
                    left_inner = center - start
                    right_inner = center + start
                    right_outer = center + end
                    
                    xlim = self.graphAxes1.get_xlim()
                    fig_width = self.graphFigure1.get_figwidth() * self.graphFigure1.dpi
                    threshold_data = (xlim[1] - xlim[0]) * self.edge_drag_threshold / fig_width
                    
                    # Check if hovering over any edge
                    if (abs(x - left_outer) < threshold_data or abs(x - left_inner) < threshold_data or
                        abs(x - right_inner) < threshold_data or abs(x - right_outer) < threshold_data):
                        self.graphCanvas1.setCursor(Qt.SizeHorCursor)
                    else:
                        self.graphCanvas1.setCursor(Qt.ArrowCursor)
            
            centerX = self.getCenterX() # this should be the center in the box?
            distance = x - centerX
            hist = self.parent.projProc.boxes[self.name].hist
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
        Trigger when mouse hovers the second plot
        """
        x = event.xdata
        y = event.ydata
        if x is not None and y is not None:
            # Handle overlay dragging (edge or whole region)
            if self.overlay_dragging and self.overlay_drag_start is not None:
                delta_x = x - self.overlay_drag_start
                self.overlay_drag_start = x
                
                # If dragged_edge is None, it means dragging the whole region (peaks + hull_range)
                # Otherwise, dragging a specific edge (hull_range bound only)
                if self.dragged_edge is None:
                    self.updateOverlayAndPeaks(delta_x)  # Whole region drag
                else:
                    self.updateHullRangeBound(delta_x)  # Edge drag
                return
            
            # Change cursor when hovering over edges (when parameter editor is active)
            if self.param_editor_active:
                box = self.get_box()
                if box and box.hull_range is not None:
                    center = self.getCenterX()
                    # Use preview hull range if available, otherwise use box hull range
                    if self.preview_hull_range is not None:
                        start, end = self.preview_hull_range
                    else:
                        start, end = box.hull_range
                    
                    left_outer = center - end
                    left_inner = center - start
                    right_inner = center + start
                    right_outer = center + end
                    
                    xlim = self.graphAxes2.get_xlim()
                    fig_width = self.graphFigure2.get_figwidth() * self.graphFigure2.dpi
                    threshold_data = (xlim[1] - xlim[0]) * self.edge_drag_threshold / fig_width
                    
                    # Check if hovering over any edge
                    if (abs(x - left_outer) < threshold_data or abs(x - left_inner) < threshold_data or
                        abs(x - right_inner) < threshold_data or abs(x - right_outer) < threshold_data):
                        self.graphCanvas2.setCursor(Qt.SizeHorCursor)
                    else:
                        self.graphCanvas2.setCursor(Qt.ArrowCursor)
            
            centerX = self.getCenterX()
            distance = x - centerX
            box = self.parent.projProc.boxes[self.name]
            if box.subtracted_hist is not None:
                hist = box.subtracted_hist
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
            # Hull range already stored in ProcessingBox
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
            
            # Enable GMM mode for single peak selection (update ProcessingBox)
            # Using shared sigma provides better fitting stability and physical correctness
            box = self.parent.projProc.boxes[self.name]
            box.use_common_sigma = True
            
            # Set peak and sigma tolerances from UI spinboxes
            # These control the bounds for peak positions and sigmas during fitting
            box.peak_tolerance = self.peakToleranceSpinBox.value()
            box.sigma_tolerance = self.sigmaToleranceSpinBox.value()
            
            # CRITICAL: Clear param_bounds to force recalculation based on NEW peak positions
            # If we don't clear this, fitModel will use old cached bounds from previous fit,
            # which may not match the new peak positions, causing peaks to drift outside hull_range
            box.param_bounds = {}
            
            # Clear old fit_results to force re-fitting with GMM
            box.fit_results = None
            
            self.function = None
            self.parent.addPeakstoBox(self.name, all_peaks)
    
    def addPeakCluster(self):
        """
        Select multiple peaks for GMM fitting (shared sigma, auto-mirrored)
        """
        if self.peakClusterButton.isChecked():
            # User will select peaks dynamically (no preset count)
            self.peakClusterButton.setText("Accept Peaks")
            self.function = ['peak_cluster', []]  # No preset count
            
            # Show status hint
            self.parent.setLeftStatus(
                "Click to select peak positions (minimum 2, will be mirrored)")
        else:
            self.peakClusterButton.setText("Select Peak Cluster (GMM)")
            peaks = self.function[1]
            
            # Check count - at least 2 peaks required
            if len(peaks) < 2:
                QMessageBox.warning(self, "Insufficient Peaks", 
                                  f"Please select at least 2 peaks. You selected {len(peaks)}.")
                self.peakClusterButton.setChecked(True)
                return
            
            # Auto-mirror (keep original logic)
            op_peaks = [-x for x in peaks]
            all_peaks = peaks + op_peaks  # positive and negative distances
            
            # Enable GMM mode and fit
            n_gauss = len(peaks)  # Use actual number of peaks selected
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
            # Enable GMM mode BEFORE processing (update ProcessingBox directly)
            box = self.parent.projProc.boxes[self.name]
            box.use_common_sigma = True
            
            # Set peak and sigma tolerances from UI spinboxes
            # These control the bounds for peak positions and sigmas during fitting
            box.peak_tolerance = self.peakToleranceSpinBox.value()
            box.sigma_tolerance = self.sigmaToleranceSpinBox.value()
            
            # CRITICAL: Clear param_bounds to force recalculation based on NEW peak positions
            # If we don't clear this, fitModel will use old cached bounds from previous fit,
            # which may not match the new peak positions, causing peaks to drift outside hull_range
            box.param_bounds = {}
            
            # Clear old fit_results to force re-fitting with new GMM mode
            box.fit_results = None
            
            # Add peaks and process (will use tolerances set above)
            self.parent.addPeakstoBox(self.name, all_peaks)
            
            # Re-ensure GMM mode is set after processing
            box.use_common_sigma = True
            
            # Check results after processing
            box = self.parent.projProc.boxes[self.name]
            if box.fit_results is not None:
                
                result = box.fit_results
                
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
        Open parameter editor dialog (non-modal, allows interaction with tab)
        """
        box = self.parent.projProc.boxes[self.name]
        if box.fit_results is None:
            QMessageBox.warning(self, "No Fit Results", 
                              "Please fit peaks first before opening parameter editor.")
            return
        
        # Create snapshot of manual selections before opening editor
        import copy
        box = self.get_box()
        if box:
            self.snapshot_peaks = copy.deepcopy(box.peaks)
            self.snapshot_hull_range = copy.deepcopy(box.hull_range)
        else:
            self.snapshot_peaks = []
            self.snapshot_hull_range = None
        
        # Open parameter editor dialog as non-modal
        try:
            from .GMMParameterEditorDialog import GMMParameterEditorDialog
            dialog = GMMParameterEditorDialog(self, self.name)
            
            # Store reference to dialog
            self.param_editor_dialog = dialog
            
            # Make it non-modal so tab can be interacted with
            dialog.setModal(False)
            
            # Enable dragging mode
            self.param_editor_active = True
            self.showOverlay()
            
            # Inject refit callback so folder cache gets updated
            dialog.on_refit_completed = self._on_refit_completed
            
            # Connect close signal to cleanup
            dialog.finished.connect(self.onParameterEditorClosed)
            
            dialog.show()
        except ImportError as e:
            QMessageBox.critical(self, "Import Error", 
                               f"Cannot import GMMParameterEditorDialog: {str(e)}")

    def onParameterEditorClosed(self, result):
        """
        Called when parameter editor dialog is closed
        This is the ONLY place that handles parameter editor cleanup.
        
        Execution order is critical:
        1. Disable parameter editor state first (before any drawing)
        2. Clear overlay patches list
        3. Clear preview data
        4. Restore snapshots if cancelled
        5. Redraw UI (without overlay since param_editor_active is now False)
        
        :param result: QDialog.Accepted or QDialog.Rejected
        """
        # Step 1: Disable parameter editor state FIRST
        # This prevents updateUI() from calling showOverlay()
        self.param_editor_active = False
        self.param_editor_dialog = None
        
        # Step 2: Clear overlay patches list (ax.cla() in updateUI will remove them from axes)
        self.overlay_patches = []
        
        # Step 3: Clear preview data (exit preview mode)
        self.preview_params = None
        self.preview_hull_range = None
        
        # Step 4: If user cancelled, restore snapshot
        if result == QDialog.Rejected:
            import copy
            box = self.get_box()
            if box:
                if hasattr(self, 'snapshot_peaks'):
                    box.peaks = copy.deepcopy(self.snapshot_peaks)
                if hasattr(self, 'snapshot_hull_range'):
                    box.hull_range = copy.deepcopy(self.snapshot_hull_range)
        
        # Step 5: Clean up snapshots
        if hasattr(self, 'snapshot_peaks'):
            del self.snapshot_peaks
        if hasattr(self, 'snapshot_hull_range'):
            del self.snapshot_hull_range
        
        # Step 6: Redraw UI with saved values (not working/preview values)
        # Since param_editor_active is now False, this won't trigger showOverlay()
        self.need_update = True
        self.updateUI()
    
    def _on_refit_completed(self):
        """Callback from GMMParameterEditorDialog after a successful refit."""
        self.parent._update_folder_cache_from_results()
    
    def closeParameterEditor(self):
        """
        Close parameter editor dialog if open.
        Called before tab is removed (e.g., during image switch).
        
        Returns:
            bool: True if dialog was open and is being closed, False otherwise
        """
        if self.param_editor_dialog is not None and self.param_editor_active:
            # Close the dialog - this will trigger onParameterEditorClosed via finished signal
            self.param_editor_dialog.close()
            return True
        return False
    
    def showOverlay(self):
        """
        Show semi-transparent overlay for selected hull range when parameter editor is open
        Hull range (start, end) defines the valid region: [center-end, center-start] U [center+start, center+end]
        """
        box = self.get_box()
        if not box or box.hull_range is None or self.parent.projProc is None:
            return
        
        # Clear old overlays first
        self.hideOverlay()
        
        center = self.getCenterX()
        # Use preview_hull_range during dragging, otherwise use box.hull_range
        if self.preview_hull_range is not None:
            start, end = self.preview_hull_range
        else:
            start, end = box.hull_range
        
        # Draw overlay on both axes
        # The hull range defines two regions (left and right sides)
        for ax in [self.graphAxes1, self.graphAxes2]:
            ylim = ax.get_ylim()
            height = ylim[1] - ylim[0]
            
            # Left region: center - end to center - start
            x_left_region_start = center - end
            x_left_region_end = center - start
            width_left = x_left_region_end - x_left_region_start
            
            overlay_left = patches.Rectangle(
                (x_left_region_start, ylim[0]), width_left, height,
                linewidth=2, edgecolor='cyan', facecolor='cyan', 
                alpha=0.2, linestyle='--', picker=True
            )
            ax.add_patch(overlay_left)
            self.overlay_patches.append(overlay_left)
            
            # Right region: center + start to center + end
            x_right_region_start = center + start
            x_right_region_end = center + end
            width_right = x_right_region_end - x_right_region_start
            
            overlay_right = patches.Rectangle(
                (x_right_region_start, ylim[0]), width_right, height,
                linewidth=2, edgecolor='cyan', facecolor='cyan', 
                alpha=0.2, linestyle='--', picker=True
            )
            ax.add_patch(overlay_right)
            self.overlay_patches.append(overlay_right)
        
        self.graphCanvas1.draw_idle()
        self.graphCanvas2.draw_idle()
    
    def hideOverlay(self):
        """
        Hide overlay when parameter editor is closed
        """
        # Remove patches using patch.remove() method
        for patch in self.overlay_patches:
            try:
                patch.remove()
            except (ValueError, AttributeError):
                # Patch may have already been removed or axes cleared
                pass
        
        self.overlay_patches = []
        self.graphCanvas1.draw_idle()
        self.graphCanvas2.draw_idle()
    
    def updateOverlayAndPeaks(self, delta_x):
        """
        Update overlay position and adjust peaks/hull range during drag interaction
        
        This method is ONLY called when parameter editor is open (preview mode).
        It updates preview_params and preview_hull_range for real-time preview,
        WITHOUT modifying the persistent fit_results or box.hull_range.
        
        :param delta_x: Change in x position during drag
        """
        # Sanity check: this should only be called in preview mode
        if self.preview_params is None or self.preview_hull_range is None:
            print("WARNING: updateOverlayAndPeaks called outside preview mode - this should not happen!")
            return
        
        box = self.get_box()
        if not box or box.hull_range is None:
            return
        
        # Extract current peak positions from preview_params (already shifted from previous drags)
        # instead of re-reading from box.peaks (which has original positions)
        # This makes dragging cumulative since delta_x is incremental per frame
        current_peaks = []
        i = 0
        while f'p_{i}' in self.preview_params:
            peak_val = self.preview_params[f'p_{i}']
            # Only collect positive-side peaks (peaks are mirrored: [+p1, +p2, -p1, -p2])
            # We can identify positive peaks as those that come first in the sequence
            current_peaks.append(peak_val)
            i += 1
        
        # Since peaks are mirrored, first half are positive side
        n_total = i
        if n_total > 0:
            n_positive = n_total // 2
            current_peaks = current_peaks[:n_positive]
            
            # Update peaks with drag offset (cumulative because we read from preview_params)
            new_manual_peaks = [p + delta_x for p in current_peaks]
            
            # Update preview_params (for rendering)
            for i, peak in enumerate(new_manual_peaks):
                if f'p_{i}' in self.preview_params:
                    self.preview_params[f'p_{i}'] = peak
            # Also update mirrored peaks
            for i in range(len(new_manual_peaks), len(new_manual_peaks) * 2):
                mirror_idx = i - len(new_manual_peaks)
                if f'p_{i}' in self.preview_params:
                    self.preview_params[f'p_{i}'] = -new_manual_peaks[mirror_idx]
            
            # Update dialog's working_params (for table display)
            if self.param_editor_dialog is not None and self.param_editor_active:
                for i, peak in enumerate(new_manual_peaks):
                    if f'p_{i}' in self.param_editor_dialog.working_params:
                        self.param_editor_dialog.working_params[f'p_{i}'] = peak
                # Update mirrored peaks
                for i in range(len(new_manual_peaks), len(new_manual_peaks) * 2):
                    mirror_idx = i - len(new_manual_peaks)
                    if f'p_{i}' in self.param_editor_dialog.working_params:
                        self.param_editor_dialog.working_params[f'p_{i}'] = -new_manual_peaks[mirror_idx]
        
        # Update hull_range: read from preview_hull_range (already shifted), not box.hull_range
        # This makes dragging cumulative since delta_x is incremental per frame
        old_start, old_end = self.preview_hull_range
        new_start = old_start + delta_x
        new_end = old_end + delta_x
        
        # Update preview_hull_range (for rendering)
        self.preview_hull_range = (new_start, new_end)
        
        # Update dialog's working_hull_range (for table display)
        if self.param_editor_dialog is not None and self.param_editor_active:
            self.param_editor_dialog.working_hull_range = (new_start, new_end)
            
            # Directly update spinbox display (efficient - no full table refresh)
            try:
                self.param_editor_dialog.hullStartSpinBox.blockSignals(True)
                self.param_editor_dialog.hullEndSpinBox.blockSignals(True)
                self.param_editor_dialog.hullStartSpinBox.setValue(float(new_start))
                self.param_editor_dialog.hullEndSpinBox.setValue(float(new_end))
                self.param_editor_dialog.hullStartSpinBox.blockSignals(False)
                self.param_editor_dialog.hullEndSpinBox.blockSignals(False)
            except Exception as e:
                print(f"Warning: Could not update hull range spinboxes: {e}")
        
        # Update overlay position
        self.showOverlay()
        
        # Notify parameter editor to update table and bounds
        if self.param_editor_dialog is not None and self.param_editor_active:
            try:
                # Update min/max bounds for all affected peaks (more efficient than populateParameters)
                # n_total includes both positive and negative (mirrored) peaks
                if n_total > 0:
                    peak_indices = list(range(n_total))
                    self.param_editor_dialog.updatePeakBounds(peak_indices)
            except Exception as e:
                print(f"Warning: Could not update parameter editor bounds: {e}")
        
        # Redraw with updated preview data (no refit, just visual update)
        self.need_update = True
        self.updateUI()
    
    def updateHullRangeBound(self, delta_x):
        """
        Update only the dragged hull range bound
        
        This method is called when parameter editor is open and user drags a hull range edge.
        Unlike updateOverlayAndPeaks, this ONLY updates the hull range bounds, not the peaks.
        
        :param delta_x: Change in x position during drag
        """
        # Sanity check: this should only be called in preview mode
        if self.preview_hull_range is None or self.dragged_edge is None:
            print("WARNING: updateHullRangeBound called without preview mode or dragged edge!")
            return
        
        box = self.get_box()
        if not box or box.hull_range is None:
            return
        
        # Get current hull range from preview
        start, end = self.preview_hull_range
        
        # Update the appropriate bound based on which edge is being dragged
        if self.dragged_edge == 'left_outer':
            # Dragging left outer edge (center - end)
            # Moving right increases end, moving left decreases end
            end = end - delta_x
        elif self.dragged_edge == 'left_inner':
            # Dragging left inner edge (center - start)
            # Moving right decreases start, moving left increases start
            start = start - delta_x
        elif self.dragged_edge == 'right_inner':
            # Dragging right inner edge (center + start)
            # Moving right increases start, moving left decreases start
            start = start + delta_x
        elif self.dragged_edge == 'right_outer':
            # Dragging right outer edge (center + end)
            # Moving right increases end, moving left decreases end
            end = end + delta_x
        
        # Ensure start <= end and both are positive
        start = max(0, start)
        end = max(0, end)
        if start > end:
            start, end = end, start
        
        # Update preview_hull_range (for rendering)
        self.preview_hull_range = (start, end)
        
        # Update dialog's working_hull_range (for table display)
        if self.param_editor_dialog is not None and self.param_editor_active:
            self.param_editor_dialog.working_hull_range = (start, end)
        
        # Update overlay position
        self.showOverlay()
        
        # Notify parameter editor to update table
        if self.param_editor_dialog is not None and self.param_editor_active:
            try:
                self.param_editor_dialog.populateParameters()
            except Exception as e:
                print(f"Warning: Could not update parameter editor table: {e}")
        
        # Redraw with updated preview data (no refit, just visual update)
        self.need_update = True
        self.updateUI()

    def refreshUI(self):
        #clear the axes
        self.need_update = True
        self.updateUI()

        # Only redraw lines if function exists
        if self.function is not None and len(self.function) > 1:
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
        Clears all peaks without changing button state
        :return:
        """
        if self.function is None:
            # Clear all peaks from parent (which updates box.peaks)
            self.parent.addPeakstoBox(self.name, [])
        else:
            # Clear all peaks from function
            if self.function[0] in ('peaks', 'single_peak', 'peak_cluster'):
                self.function[1].clear()
        
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
        name = self.name
        if name not in self.parent.projProc.boxes:
            return

        box = self.parent.projProc.boxes[name]
        hist = box.hist
        subtracted_hist = box.subtracted_hist
        baselines = box.baselines
        centroids = box.centroids
        widths = box.widths
        peaks = box.moved_peaks
        areas = box.areas
        bgsub = box.bgsub
        merid_bg = box.merid_bg
        hull_range = box.hull_range

        # Check if hist is available
        if hist is None:
            return

        ax = self.graphAxes1
        ax.cla()

        ax2 = self.graphAxes2
        ax2.cla()
        
        ax.set_title("Projection")
        ax2.set_title("Background Subtracted Projection")

        if self.histChkBx.isChecked():
            ax.plot(hist, color='k')

        if merid_bg is not None:
            self.meridBckGrndChkBx.setChecked(merid_bg)

        # Get render parameters (supports preview mode for parameter editor)
        model = self._getRenderParams()
        
        if model is not None:
            self.editMainPeakButton.setEnabled(True)
            xs = np.arange(0, len(hist))
            convex_hull = hist - box.hist2 if box.hist2 is not None else hist
            
            print(f"[updateUI] Drawing fit for '{name}'")
            print(f"  Fit model checkbox: {self.fitmodelChkBx.isChecked()}")
            print(f"  Model sigma0: {model.get('sigma0', 'N/A')}")
            print(f"  Model amplitude0: {model.get('amplitude0', 'N/A')}")
            print(f"  Model common_sigma: {model.get('common_sigma', 'N/A')}")

            if self.subHistChkBx.isChecked() and subtracted_hist is not None:
                ax2.plot(subtracted_hist, color='k')

            if self.fitmodelChkBx.isChecked():
                model_hist = layerlineModel(xs, **model)
                subtracted_model = model_hist-layerlineModelBackground(xs, **model)

                if bgsub == 1:
                    # Add convexhull background
                    model_hist = model_hist + convex_hull

                ax.plot(model_hist, color='g')
                print(f"  Drew fit curve on ax1, model_hist range: [{model_hist.min():.2f}, {model_hist.max():.2f}]")
                
                if bgsub == 2: # if no background subtraction, use the fit from the original model
                    ax2.plot(model_hist, color='g')
                else:
                    ax2.plot(subtracted_model, color='g')
                print(f"  Drew fit curve on ax2")

            if self.bgChkBx.isChecked():
                if bgsub == 1:
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
                while True:
                    key = f"p_{i}"
                    if key not in model:
                        break

                    p = model[key]
                    x = model['centerX'] + p

                    # ax.axvline(model['centerX'] - p, color='r', alpha=0.7)
                    center_line = ax.axvline(x, color='r', alpha=0.7)
                    self.lines.append(center_line)

                    # Label peak using the stored variable name (p_0, p_1, ...)
                    ax.text(
                        x, 0.95, key,
                        transform=ax.get_xaxis_transform(),
                        color='r', fontsize=9,
                        ha='center', va='top'
                    )

                    i += 1

            if self.maxPeaksChkBx.isChecked():
                if peaks is not None:
                    for p in peaks:
                        d = p - model['centerX']
                        # ax2.axvline(model['centerX'] - d, color='b', alpha=0.7)
                        ax2.axvline(model['centerX'] + d, color='b', alpha=0.7)
            # max intensity locations
            # if name in all_peaks:
            #     peaks = all_peaks[name]
            #     for p in peaks:
            #         ax2.axvline(p, color='r', alpha=0.7)

            if centroids is not None and baselines is not None and (self.centroidChkBx.isChecked() or self.baselineChkBx.isChecked()):
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

        # Get hull range (supports preview mode)
        current_hull_range = None
        if self.preview_hull_range is not None:
            # Preview mode (parameter editor open)
            current_hull_range = self.preview_hull_range
        else:
            # Normal mode
            current_hull_range = hull_range
        
        # Check if hull_range is valid (not None, not empty, has 2 elements)
        if (self.hullRangeChkBx.isChecked() and bgsub == 1 and 
            current_hull_range is not None and 
            isinstance(current_hull_range, (tuple, list)) and 
            len(current_hull_range) == 2):
            # Color area OUTSIDE convex hull range
            centerX = self.getCenterX()

            ax.axvspan(centerX - current_hull_range[0], centerX + current_hull_range[0], alpha=0.5, color='k')
            ax.axvspan(0, centerX - current_hull_range[1], alpha=0.5, color='k')
            ax.axvspan(centerX + current_hull_range[1], len(hist), alpha=0.5, color='k')

            # Update spin box
            self.startHull.setValue(current_hull_range[0])
            self.endHull.setValue(current_hull_range[1])

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
        
        # Redraw overlay if parameter editor is active
        if self.param_editor_active:
            self.showOverlay()

        # Update Table
        
        if centroids is not None and baselines is not None and model is not None:
            nPeaks = len(centroids)
            peaks_for_table = np.array(peaks) - model['centerX'] if peaks is not None else None
            self.resultTable1.setRowCount(nPeaks)
            self.resultTable2.setRowCount(nPeaks)

            for i in range(nPeaks):
                center = round(model['p_'+str(i)], 2)
                sigma = round(model['sigma'+str(i)], 2)
                area_gauss = round(model['amplitude' + str(i)], 2)

                item = QTableWidgetItem(str(round(peaks_for_table[i], 2)) if peaks_for_table is not None else "")
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 0, item)

                item = QTableWidgetItem(str(center))
                #item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 1, item)

                item = QTableWidgetItem(str(sigma))
                # item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 2, item)

                item = QTableWidgetItem(str(area_gauss))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 3, item)

                centroid = round(centroids[i], 2)
                baseline = round(baselines[i], 2)
                width = round(widths[i], 2) if widths is not None else 0.0
                area_cent = round(areas[i], 2) if areas is not None else 0.0

                item = QTableWidgetItem(str(baseline))
                self.resultTable2.setItem(i, 0, item)

                item = QTableWidgetItem(str(centroid))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 1, item)

                item = QTableWidgetItem(str(width))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 2, item)

                item = QTableWidgetItem(str(area_cent))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 3, item)
        
        # Auto-zoom graph1 to hull_range on first load (only if not already applied)
        if not self.auto_zoom_applied and self.zoom1 is not None and self.graphMaxBound is not None:
            box = self.get_box()
            if box and box.hull_range is not None:
                self.autoZoomToHullRange()
                self.auto_zoom_applied = True  # Mark as applied
                # Apply the zoom to graph1
                if self.zoom1 is not None:
                    self.graphAxes1.set_xlim(self.zoom1[0])
                    self.graphAxes1.set_ylim(self.zoom1[1])
                    self.graphCanvas1.draw_idle()
        
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
        
        # Check if histogram exists for this box
        if self.name not in self.parent.projProc.boxes:
            return
        
        hist = self.parent.projProc.boxes[self.name].hist
        
        print(event.xdata - len(hist)/2)
        
        self.parent.projProc.setGaussCenter(self.name, self.fixed_indices[-1], event.xdata - len(hist)/2)
        self.parent.processImage()
        
        self.graphFigure1.canvas.draw()  # Ensure the canvas is updated
        