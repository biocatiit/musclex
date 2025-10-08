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

import math
import numpy as np
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
import matplotlib.pyplot as plt
from matplotlib.text import Text
from PIL import Image
from musclex import __version__
from ..utils.misc_utils import inverseNmFromCenter
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.XRayViewer import XRayViewer
from ..csv_manager.XV_CSVManager import XV_CSVManager
from ..CalibrationSettings import CalibrationSettings
from .pyqt_utils import *
from .LogTraceViewer import LogTraceViewer
from .DoubleZoomGUI import DoubleZoom
from PySide6.QtCore import QTimer
from .widgets.NavigationControls import NavigationControls


class XRayViewerGUI(QMainWindow):
    """
    A class for window displaying all information of a selected image.
    This window contains 2 tabs : image, and graph
    """
    currSaved = Signal(int)
    def __init__(self):
        """
        Initial window
        """
        super().__init__()
        # H5 list/index no longer needed; GUI uses FileManager state directly
        self.windowList = []
        self.counter = 0
        # filePath is derived from FileManager.dir_path when needed
        self.calSettings = None
        # numberOfFiles is derived from FileManager (names/file_list) and scan status
        self._provisionalCount = False
        self.img_zoom = None # zoom location of original image (x,y range)
        self.checkableButtons = []
        self.line_coords = []
        self.xrayViewer = None
        self.default_img_zoom = None # default zoom calculated after processing image
        self.graph_zoom = None # zoom location of result image (x,y range)
        self.function = None # current active function
        self.uiUpdating = False # update ui status flag (prevent recursive)
        self.updated = {'img': False} # update state of 2 tabs
        self.plot_min = None
        self.first_slice = False
        self.first_box = False
        self.saved_slice = None
        self.stop_process = False
        self.calSettingsDialog = None
        # Scan thread/result handled by FileManager
        self._scan_timer = QTimer(self)
        self._scan_timer.setInterval(200)
        self._scan_timer.timeout.connect(self._checkScanDone)

        self.initUI() # initial all GUI
        self.setConnections() # set triggered function for widgetsF
        #self.setMinimumHeight(800)
        #self.setMinimumWidth(1000)

        self.doubleZoomGUI = DoubleZoom(self.imageFigure)

        self.browseFile()

    def initUI(self):
        """
        Open a file finder and return the name of the file selected
        """
        self.setWindowTitle("X-Ray Viewer v." + __version__)

        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)

        self.scrollArea.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        self.scrollArea.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

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
        #self.imageTabLayout.addStretch()
        self.tabWidget.addTab(self.imageTab, "Image")

        self.verImgLayout = QVBoxLayout()
        self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)

        self.leftWidget = QWidget()
        self.leftWidget.setLayout(self.verImgLayout)
        self.leftWidget.setMinimumWidth(650)

        self.selectImageButton = QPushButton('Click Here to Select an Image...')
        self.selectImageButton.setFixedHeight(100)
        self.selectImageButton.setFixedWidth(300)

        self.verImgLayout.addWidget(self.selectImageButton)
        self.imageFigure = plt.figure()
        self.imageAxes = self.imageFigure.add_subplot(111)
        self.imageCanvas = FigureCanvas(self.imageFigure)

        self.imageCanvas.setHidden(True)
        self.imageTabLayout.addWidget(self.leftWidget)
        self.imageTabLayout.addWidget(self.imageCanvas)

        self.displayOptGrpBx = QGroupBox()
        self.displayOptGrpBx.setTitle("Display Options")
        self.displayOptGrpBx.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self.dispOptLayout = QGridLayout()
        #self.dispOptLayout.setRowStretch()

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

        self.colorMapChoice = QComboBox()
        self.colorMapChoice.setCurrentIndex(0)
        self.allColorChoices = ['gray', 'viridis', 'plasma', 'inferno', 'magma', 'cividis']
        for c in self.allColorChoices:
            self.colorMapChoice.addItem(c)
        
        self.logScaleIntChkBx = QCheckBox("Log scale intensity")
        self.persistIntensity = QCheckBox("Persist intensities")

        self.imgZoomInB = QPushButton("Zoom in")
        self.imgZoomInB.setCheckable(True)
        self.imgZoomOutB = QPushButton("Full")
        self.checkableButtons.append(self.imgZoomInB)
        
        self.doubleZoom = QCheckBox("Double Zoom")

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')
        self.dispOptLayout.addWidget(self.minIntLabel, 1, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spminInt, 1, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntLabel, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.spmaxInt, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomInB, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 3, 1, 1, 1)
        self.dispOptLayout.addWidget(QLabel("Color Map:"), 4, 0, 1, 1)
        self.dispOptLayout.addWidget(self.colorMapChoice, 4, 1, 1, 1)
        self.dispOptLayout.addWidget(self.logScaleIntChkBx, 5, 0, 1, 2)
        self.dispOptLayout.addWidget(self.persistIntensity, 6, 0, 1, 2)
        self.dispOptLayout.addWidget(self.doubleZoom, 7, 0, 1, 2)
        self.displayOptGrpBx.setLayout(self.dispOptLayout)

        self.optionsLayout = QVBoxLayout()
        self.optionsLayout.setAlignment(Qt.AlignCenter)
        self.settingsGroup = QGroupBox("Image Processing")
        self.settingsLayout = QGridLayout()
        #self.settingsLayout.setRowStretch()
        self.settingsGroup.setLayout(self.settingsLayout)

        self.calibrationButton = QPushButton("Calibration Settings")
        self.openTrace = QPushButton("Open Trace Window")
        self.measureDist = QPushButton("Measure a Distance")
        self.measureDist.setCheckable(True)
        self.setSlice = QPushButton("Set Graph Slice")
        self.setSlice.setCheckable(True)
        self.setSliceBox = QPushButton("Set Graph Box")
        self.setSliceBox.setCheckable(True)
        self.saveGraphSlice = QCheckBox("Save Graph Profile")
        self.saveGraphSlice.setEnabled(False)
        self.inpaintChkBx = QCheckBox("Inpainting")
        
        self.checkableButtons.append(self.measureDist)
        self.checkableButtons.append(self.setSlice)
        self.checkableButtons.append(self.setSliceBox)
        
        self.settingsLayout.addWidget(self.calibrationButton, 0, 0, 1, 2)
        self.settingsLayout.addWidget(self.openTrace, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.measureDist, 2, 0, 1, 2)
        self.settingsLayout.addWidget(self.setSlice, 3, 0, 1, 2)
        self.settingsLayout.addWidget(self.setSliceBox, 4, 0, 1, 2)
        self.settingsLayout.addWidget(self.saveGraphSlice, 5, 0, 1, 2)
        self.settingsLayout.addWidget(self.inpaintChkBx, 6, 0, 1, 2)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        # Reusable navigation widget (Image tab)
        self.navImg = NavigationControls(primary_text="Play")
        self.navImg.primaryButton.setStyleSheet(pfss)
        # Backward-compatible attribute aliases
        self.processFolderButton = self.navImg.primaryButton
        self.nextButton = self.navImg.nextButton
        self.prevButton = self.navImg.prevButton
        self.nextFileButton = self.navImg.nextFileButton
        self.prevFileButton = self.navImg.prevFileButton
        self.filenameLineEdit = self.navImg.filenameLineEdit

        self.displayOptGrpBx.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingsGroup)
        self.optionsLayout.addStretch()
        self.optionsLayout.addWidget(self.navImg)
        self.frameOfKeys = QFrame()
        self.frameOfKeys.setFixedWidth(400)
        #self.frameOfKeys.addStretch() 
        self.frameOfKeys.setLayout(self.optionsLayout)

        self.scroll_areaImg = QScrollArea()
        self.scroll_areaImg.setWidgetResizable(True)
        self.scroll_areaImg.setWidget(self.frameOfKeys)

        self.imageTabLayout.addWidget(self.scroll_areaImg)
        
        self.measureDist2 = QPushButton("Measure a Distance")
        self.measureDist2.setCheckable(True)

        self.zoomInGraphButton = QPushButton("Zoom In")
        self.zoomInGraphButton.setCheckable(True)

        self.resetZoomButton = QPushButton("Reset Zoom")
        
        # Reusable navigation widget (Graph tab)
        self.bottomLayout2 = QGridLayout()
        self.navGraph = NavigationControls(primary_text="Play")
        self.navGraph.primaryButton.setStyleSheet(pfss)
        # Backward-compatible attribute aliases
        self.processFolderButton2 = self.navGraph.primaryButton
        self.nextButton2 = self.navGraph.nextButton
        self.prevButton2 = self.navGraph.prevButton
        self.nextFileButton2 = self.navGraph.nextFileButton
        self.prevFileButton2 = self.navGraph.prevFileButton
        self.filenameLineEdit2 = self.navGraph.filenameLineEdit

        self.bottomLayout2.addWidget(self.zoomInGraphButton, 0, 0, 1, 2)
        self.bottomLayout2.addWidget(self.resetZoomButton, 2, 0, 1, 2)
        self.bottomLayout2.addWidget(self.measureDist2, 3, 0, 1, 2)
        self.bottomLayout2.addWidget(self.navGraph, 4, 0, 4, 2)

        self.fittingOptionsFrame2 = QFrame()
        self.fittingOptionsFrame2.setFixedWidth(250) 
        self.fittingOptionsLayout2 = QVBoxLayout(self.fittingOptionsFrame2)
        self.fittingOptionsLayout2.addStretch()
        self.fittingOptionsLayout2.addLayout(self.bottomLayout2)

        # Plot
        self.fittingFigure = plt.figure()
        self.fittingAxes = self.fittingFigure.add_subplot(111)
        self.fittingCanvas = FigureCanvas(self.fittingFigure)

        ##### Graph Tab #####
        self.fittingTab = QWidget()
        self.fittingTabLayout = QGridLayout(self.fittingTab)
        self.fittingTabLayout.addWidget(self.fittingCanvas, 0, 1, 2, 2)
        self.fittingTabLayout.addWidget(self.fittingOptionsFrame2, 0, 0, 1, 1)
        self.tabWidget.addTab(self.fittingTab, "Graph")
        self.tabWidget.setTabEnabled(1, False) 

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
        self.imgPathOnStatusBar.setText("  Please select an image or a folder to process")
        self.statusBar.addPermanentWidget(self.statusReport)
        self.statusBar.addPermanentWidget(self.imgCoordOnStatusBar)
        self.statusBar.addPermanentWidget(self.imgDetailOnStatusBar)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.statusBar.addWidget(QLabel("    "))
        self.statusBar.addWidget(self.imgPathOnStatusBar)

        #Second Status bar for when graph tab is selected
        self.scrollWheelStatusBar = QStatusBar()
        self.scrollWheelStatusBar.addWidget(QLabel("    "))
        self.scrollWheelStatusBar.addWidget(QLabel("Hint: Use the scroll wheel on your mouse to zoom in and out!"))
        self.scrollWheelStatusBar.setVisible(False)

        #Add both status bars
        self.mainLayout.addWidget(self.scrollWheelStatusBar)
        self.mainLayout.addWidget(self.statusBar)

        #### Menu Bar #####
        selectImageAction = QAction('Select an Image...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.triggered.connect(self.browseFile)

        menubar = self.menuBar()
        # menubar.setNativeMenuBar(False)
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        aboutAct = QAction('About', self)
        aboutAct.triggered.connect(self.showAbout)
        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)

        self.show()
        self.resize(1150, 700)

    def setConnections(self):
        """
        Set all triggered functions for widgets
        """
        self.tabWidget.currentChanged.connect(self.updateUI)

        ##### Image Tab #####
        self.spminInt.valueChanged.connect(self.refreshImageTab)
        self.spmaxInt.valueChanged.connect(self.refreshImageTab)
        self.colorMapChoice.currentIndexChanged.connect(self.refreshImageTab)
        self.logScaleIntChkBx.stateChanged.connect(self.refreshImageTab)
        self.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.nextButton.clicked.connect(self.nextClicked)
        self.prevButton.clicked.connect(self.prevClicked)
        self.nextFileButton.clicked.connect(self.nextFileClicked)
        self.prevFileButton.clicked.connect(self.prevFileClicked)
        self.processFolderButton2.toggled.connect(self.batchProcBtnToggled)
        self.nextButton2.clicked.connect(self.nextClicked)
        self.prevButton2.clicked.connect(self.prevClicked)
        self.nextFileButton2.clicked.connect(self.nextFileClicked)
        self.prevFileButton2.clicked.connect(self.prevFileClicked)
        self.filenameLineEdit.editingFinished.connect(self.fileNameChanged)
        self.filenameLineEdit2.editingFinished.connect(self.fileNameChanged)
        self.calibrationButton.clicked.connect(self.launchCalibrationSettings)
        self.openTrace.clicked.connect(self.openTraceClicked)
        self.measureDist.clicked.connect(self.measureDistChecked)
        self.setSlice.clicked.connect(self.setSliceChecked)
        self.setSliceBox.clicked.connect(self.setSliceBoxChecked)
        self.saveGraphSlice.stateChanged.connect(self.saveGraphSliceChecked)
        self.inpaintChkBx.stateChanged.connect(self.onImageChanged)

        self.selectImageButton.clicked.connect(self.browseFile)
        self.imgZoomInB.clicked.connect(self.imageZoomIn)
        self.imgZoomOutB.clicked.connect(self.imageZoomOut)
        self.doubleZoom.stateChanged.connect(self.doubleZoomChecked)
        self.imageFigure.canvas.mpl_connect('button_press_event', self.imageClicked)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)
        self.imageFigure.canvas.mpl_connect('button_release_event', self.imageReleased)
        self.imageFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)

        ##### Graph tab #####
        self.fittingFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.fittingFigure.canvas.mpl_connect('motion_notify_event', self.plotOnMotion)
        self.fittingFigure.canvas.mpl_connect('button_release_event', self.plotReleased)
        self.fittingFigure.canvas.mpl_connect('figure_leave_event', self.leavePlot)
        self.fittingFigure.canvas.mpl_connect('scroll_event', self.plotScrolled)
        self.measureDist2.clicked.connect(self.measureDistChecked2)
        self.resetZoomButton.clicked.connect(self.resetZoomClicked)
        self.zoomInGraphButton.clicked.connect(self.zoomInGraphClicked)
        self.checkableButtons.append(self.measureDist2)


    def updateLeftWidgetWidth(self):
        if self.imageCanvas.isVisible():
            # Remove the minimum width constraint
            self.leftWidget.setMinimumWidth(0)
        else:
            # Set the minimum width for when the canvas is hidden
            self.leftWidget.setMinimumWidth(650)

    def launchCalibrationSettings(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or calibration.tif in the folder
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """

        if self.calSettingsDialog is None:
            dir_path = getattr(self.fileManager, 'dir_path', None)
            self.calSettingsDialog = CalibrationSettings(dir_path) if dir_path is None else \
                CalibrationSettings(dir_path, center=self.xrayViewer.orig_image_center)
        self.calSettings = None
        cal_setting = self.calSettingsDialog.calSettings
        if cal_setting is not None or force:
            result = self.calSettingsDialog.exec_()
            if result == 1:
                self.calSettings = self.calSettingsDialog.getValues()
                if self.calSettingsDialog.fixedCenter.isChecked():
                    self.xrayViewer.info['center'] = self.calSettings['center']
                else:
                    if 'center' in self.xrayViewer.info:
                        del self.xrayViewer.info['center']
                self.xrayViewer.findCenter()
                return True
        return False

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
            for b in self.checkableButtons:
                b.setChecked(False)

    def openTraceClicked(self):
        """
        Triggered when the Open Trace button is clicked
        """
        newWindows = LogTraceViewer(self, self.fileManager.current)
        if len(self.windowList) >= 1:
            self.windowList = []
            self.counter = 0
        self.windowList.append(newWindows)

    def send_signal(self):
        if self.windowList != []:
            signal=self.fileManager.current
            if self.counter == 0:
                self.currSaved.connect(self.windowList[0].showLine)
                self.counter = 1
            self.currSaved.emit(signal)

    def saveGraphSliceChecked(self):
        """
        Saves the first slice in xv_results
        """
        if self.saveGraphSlice.isChecked():
            self.csv_manager.writeNewData(self.xrayViewer)

    def plotClicked(self, event):
        """
        Triggered when mouse presses on the graph in fitting tab
        """
        xrayViewer = self.xrayViewer
        if xrayViewer is None:
            return

        x = event.xdata
        y = event.ydata
        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.imgCoordOnStatusBar.setText("")
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
                    self.zoomInGraphButton.setChecked(False)
                    self.refreshAllTabs()
            elif func[0] in ["dist"]:
                func.append((x, y))
                ax = self.fittingAxes
                self.updateMeasureDistBox()
                ax.plot(event.xdata, event.ydata, 'o')

                self.fittingCanvas.draw_idle()   
                if len(func) > 2 and len(func) % 2 != 0:
                    ax.add_artist(self.makeText(func[-1], func[-2], str(int((len(func) - 1) / 2)), dist=20))

    def plotOnMotion(self, event):
        """
        Triggered when mouse hovers on the graph in fitting tab
        """
        xrayViewer = self.xrayViewer
        if xrayViewer is None:
            self.imgCoordOnStatusBar.setText("")
            return

        x = event.xdata
        y = event.ydata
        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.imgCoordOnStatusBar.setText("")
            ax = self.fittingAxes
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            bounds = ax.get_window_extent().get_points() ## return [[x1,y1],[x2,y2]]
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
            self.imgCoordOnStatusBar.setText("x=" + str(round(x,2)) + ', y=' + str(round(y,2)))

        if self.function is None:
            return

        func = self.function
        if func[0] == "g_zoomin" and len(func) > 1:
            # Draw rectangle
            ax = self.fittingAxes

            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.fittingCanvas.draw_idle()
        elif func[0] == "g_move" and len(func) > 1:
            # Change zoom location (x, y ranges) in order to go round plot by dragging
            if self.graph_zoom is not None:
                hist = self.xrayViewer.hist
                ax = self.fittingAxes
                move = (func[1][0] - x, func[1][1] - y)
                self.graph_zoom = getNewZoom(self.graph_zoom, move, len(hist), max(hist)*1.1, self.plot_min)
                ax.set_xlim(self.graph_zoom[0])
                ax.set_ylim(self.graph_zoom[1])
                self.fittingCanvas.draw_idle()
        elif func[0] == "dist":
            # draw X on points and a line between points
            ax = self.fittingAxes

            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,0,-1):
                        ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            
            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    # first_cross = ax.lines[:2]
                    for i in range(len(ax.lines)-1,1,-1):
                        ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                
            elif len(func) % 2 != 0:
                if len(ax.lines) > 0:
                    n = (len(func)-1)*5//2 + 2
                
                    for i in range(len(ax.lines)-1, n-1, -1):
                        ax.lines[i].remove()

                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                
            elif len(func) % 2 == 0:
                start_pt = func[-1]
                if len(ax.lines) > 3:
                    n = len(func) * 5 // 2 - 1


                    for i in range(len(ax.lines) - 1, n - 1, -1):
                        ax.lines[i].remove()

                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')

            self.imageCanvas.draw_idle()

            """ ax = self.fittingAxes
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            for line in ax.lines:
                if line.get_marker() == 'x':
                    line.remove()
            ax.plot(event.xdata, event.ydata, 'rx')
            ax.set_xlim(xlim)
            ax.set_ylim(ylim)
            self.fittingCanvas.draw_idle()"""

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
        self.imgCoordOnStatusBar.setText("")

    def plotScrolled(self, event):
        """
        This function is called when a mouse scrolled on the graph in fitting tab. This will affect zoom-in and zoom-out
        """
        xrayViewer = self.xrayViewer
        if xrayViewer is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        max_size = (max(self.xrayViewer.hist) * 1.1, len(self.xrayViewer.hist))
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
        

    def zoomInGraphClicked(self):
        if self.zoomInGraphButton.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a rectangle on the image to zoom in (ESC to cancel)")
            self.function = ['g_zoomin']
        else:
            self.function = None
            self.resetStatusbar()


    def resetZoomClicked(self):
        self.graph_zoom = None
        self.updateFittingTab(self.xrayViewer.hist)

    def imageZoomIn(self):
        """
        Trigger when set zoom in button is pressed (image tab)
        """
        if self.imgZoomInB.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a rectangle on the image to zoom in (ESC to cancel)")
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
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
        self.default_img_zoom = None
        self.img_zoom = None
        self.refreshImageTab()
        
    def doubleZoomChecked(self):
        """
        Triggered when double zoom is checked
        """

        self.doubleZoomGUI.doubleZoomChecked(img=self.xrayViewer.orig_img,
                                             canv=self.imageCanvas,
                                             is_checked=self.doubleZoom.isChecked())
        

    def measureDistChecked(self):
        """
        Prepare for measure distance
        :return:
        """
        if self.xrayViewer is None:
            return
        QApplication.restoreOverrideCursor()
        ax = self.imageAxes
        for i in range(len(ax.lines)-1,-1,-1):
            ax.lines[i].remove()
        for i in range(len(ax.patches)-1,-1,-1):
            ax.patches[i].remove()
            self.imageCanvas.draw_idle()
        if self.measureDist.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a line on the image to measure a distance (ESC to cancel)")
            self.function = ["dist"]  # set current active function
        else:
            self.refreshAllTabs()

    def updateMeasureDistBoxOkClicked(self):
        self.measureDist.setChecked(False)
        self.measureDist2.setChecked(False)
        self.refreshAllTabs()

    def updateMeasureDistBox(self):
        func = self.function
        if func[0] == "dist" and len(func) > 2:
            if len(func) == 3:
                dist = math.dist(func[1], func[2])
                self.inf_text = "Distance 1: " + str(round(dist,2)) + " pixels"

                self.measureDistanceBox = QMessageBox()
                self.measureDistanceBox.setWindowModality(Qt.NonModal)  # Ensure it's non-modal
                self.measureDistanceBox.setInformativeText(self.inf_text)
                self.measureDistanceBox.setStandardButtons(QMessageBox.Ok)
                self.measureDistanceBox.button(QMessageBox.Ok).clicked.connect(self.updateMeasureDistBoxOkClicked)
                self.measureDistanceBox.setWindowTitle("Measure a Distance")
                self.measureDistanceBox.setStyleSheet("QLabel{min-width: 500px;}")
                self.measureDistanceBox.show()

            elif len(func) % 2 != 0:
                dist = math.dist
                dist = math.dist(func[-1], func[-2])
                self.inf_text += "\nDistance " + str(int((len(func) - 1) / 2)) + ": " + str(round(dist,2)) + " pixels"
                self.measureDistanceBox.setInformativeText(self.inf_text)

    
    def measureDistChecked2(self):
        """
        Prepare for measure distance
        :return:
        """
        if self.xrayViewer is None:
            return
        QApplication.restoreOverrideCursor()
        if self.measureDist2.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a line on the image to measure a distance (ESC to cancel)")
            self.function = ["dist"]  # set current active function
        else:
            self.updateMeasureDistBoxOkClicked()

    def setSliceChecked(self):
        """
        Prepare for slice selection
        :return:
        """
        if self.xrayViewer is None:
            return
        QApplication.restoreOverrideCursor()
        ax = self.imageAxes
        for i in range(len(ax.lines)-1,-1,-1):
            ax.lines[i].remove()
        for i in range(len(ax.patches)-1,-1,-1):
            ax.patches[i].remove()
            self.imageCanvas.draw_idle()
        if self.setSlice.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a line on the image to choose a slice (ESC to cancel)")
            self.function = ["slice"]  # set current active function
            self.first_slice = True
        else:
            if self.function is not None and self.function[0] == "slice":
                func = self.function
                self.saved_slice = func
                test_first_slice = True
            else:
                func = self.saved_slice
                test_first_slice = False
            cx, cy, angle = 0, 0, 0
            for i in range(1, len(func) - 1, 2):
                cx = (func[i+1][0] + func[i][0])//2
                cy = (func[i+1][1] + func[i][1])//2
                angle_rad = math.atan2(func[i+1][1] - func[i][1], func[i+1][0] - func[i][0])
                angle = math.degrees(angle_rad)
                if test_first_slice:
                    print("Center of the line: ", (cx, cy))
            if len(func) > 1:
                rotImg, newCenter, _ = rotateImage(self.xrayViewer.orig_img, [cx, cy], angle)
                self.xrayViewer.hist = rotImg[newCenter[1], :]
                self.updateFittingTab(self.xrayViewer.hist)
                self.saveGraphSlice.setEnabled(True)
            self.refreshAllTabs()

    def setSliceBoxChecked(self):
        """
        Prepare for slice selection
        :return:
        """
        if self.xrayViewer is None:
            return
        QApplication.restoreOverrideCursor()
        ax = self.imageAxes
        for i in range(len(ax.lines)-1,-1,-1):
            ax.lines[i].remove()
        for i in range(len(ax.patches)-1,-1,-1):
            ax.patches[i].remove()
            self.imageCanvas.draw_idle()
        if self.setSliceBox.isChecked():
            self.imgPathOnStatusBar.setText(
                "Draw a line on the image then select width (ESC to cancel)")
            self.function = ["slice_box"]  # set current active function
            self.first_box = True
        else:
            if self.function is not None and self.function[0] == "slice_box":
                func = self.function
                self.saved_slice = func
                test_first_box = True
            else:
                func = self.saved_slice
                test_first_box = False
            if len(func) > 1:
                cx, cy, angle = 0, 0, 0
                cx = (func[2][0] + func[1][0])//2
                cy = (func[2][2] + func[1][1])//2
                angle = func[2][4]
                if test_first_box:
                    print("Center of the line: ", (cx, cy))

                rotImg, newCenter, _ = rotateImage(self.xrayViewer.orig_img, [cx, cy], angle)
                l = func[3][3]//2
                self.xrayViewer.hist = rotImg[newCenter[1]-func[3][2], newCenter[0]-l:newCenter[0]+l]
                for i in range(newCenter[1]-func[3][2], newCenter[1]+func[3][2]):
                    self.xrayViewer.hist += rotImg[i, newCenter[0]-l:newCenter[0]+l]
                self.xrayViewer.hist[self.xrayViewer.hist <= -1] = -1
                self.updateFittingTab(self.xrayViewer.hist)
                self.saveGraphSlice.setEnabled(True)
            self.refreshAllTabs()
            
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
        elif self.doubleZoomGUI.doubleZoomMode:
            self.doubleZoomGUI.mouseClickBehavior(x, y)
            return
            
        if self.doubleZoom.isChecked() and not self.doubleZoomGUI.doubleZoomMode:
            x, y = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
            self.doubleZoomGUI.doubleZoomMode = True

        if self.function is not None and self.function[0] == 'ignorefold':
            self.function = None

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
                    self.refreshAllTabs()
            elif func[0] in ["slice"]:
                ax = self.imageAxes
                axis_size = 5
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                self.imageCanvas.draw_idle()
                func.append((x, y))
            elif func[0] in ["dist"]:
                ax = self.imageAxes
                axis_size = 5
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                self.imageCanvas.draw_idle()       
                func.append((x, y))
                self.updateMeasureDistBox()
                if len(func) > 2 and len(func) % 2 != 0:
                    ax.add_artist(self.makeText(func[-1], func[-2], str(int((len(func) - 1) / 2))))
            elif func[0] == "slice_box":
                ax = self.imageAxes
                axis_size = 5
                if len(func) == 2:
                    pivot = ((x + func[1][0])//2, (y + func[1][1])//2)
                    deltax = x - pivot[0]
                    deltay = y - pivot[1]
                    x2 = pivot[0] - deltax
                    y2 = pivot[1] - deltay
                    ax.plot([x, x2], [y, y2], color="r")

                    if abs(x - pivot[0]) == 0:
                        new_angle = -90
                    else:
                        new_angle = -180. * np.arctan((pivot[1] - y) / (pivot[0] - x)) / np.pi

                    func.append((x, x2, y, y2, new_angle))
                    #self.doubleZoom.setChecked(False)
                elif len(func) == 3:
                    p1 = np.asarray(func[1])
                    p2 = np.asarray((func[2][0],func[2][2]))
                    p3 = np.asarray((x, y))
                    width = np.linalg.norm(np.cross(p2 - p1, p1 - p3))/np.linalg.norm(p2 - p1)
                    length = math.dist(p2, p1)
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    func.append((x, y, int(width), int(length)))
                else:
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    func.append((x, y))
                self.imageCanvas.draw_idle()


    def makeText(self, p1, p2, txt, dist=100):
        middle = [(p1[0] + p2[0]) / 2, (p1[1] + p2[1]) / 2]
        if (p1[0] > p2[0] and p1[1] > p2[1]) or (p1[0] < p2[0] and p1[1] < p2[1]):
            slope = 'pos'
        else:
            slope = 'neg'

        diff_x = abs(p1[0] - p2[0])
        diff_y = abs(p2[0] - p2[1])
        total = diff_x + diff_y

        diff_x_share = (diff_x / total) * dist
        diff_y_share = (diff_y / total) * dist

        if slope == 'neg':
            x = middle[0] + diff_y_share
            y = middle[1] + diff_x_share
        else:
            x = middle[0] - diff_y_share
            y = middle[1] + diff_x_share

        return Text(x=x, y=y, text=txt, color='red', ha='center', va='center')

    def imageOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """
        if not self.ableToProcess():
            return
        x = event.xdata
        y = event.ydata
        img = self.xrayViewer.orig_img

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:

            if self.doubleZoomGUI.doubleZoomMode:
                self.doubleZoomGUI.beginImgMotion(x, y, len(img[0]), len(img), (0,0), self.imageAxes)


            x = int(round(x))
            y = int(round(y))
            
            if x < img.shape[1] and y < img.shape[0]:
                
                if 'center' not in self.xrayViewer.info:
                    q, units = [-1, ""]
                elif self.calSettings is not None and self.calSettings and 'scale' in self.calSettings:
                    q, units = inverseNmFromCenter([x, y], self.xrayViewer.info['center'], self.calSettings['scale'])
                else:
                    q, units = [-1,""]
                
                self.imgCoordOnStatusBar.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]) + ", distance=" + str(q) + units)
                if self.doubleZoom.isChecked() and self.doubleZoomGUI.doubleZoomMode and x>10 and x<img.shape[1]-10 and y>10 and y<img.shape[0]-10:
                    ax1 = self.doubleZoomGUI.axes
                    imgCropped = img[int(y - 10):int(y + 10), int(x - 10):int(x + 10)]
                    if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                        imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                        self.doubleZoomGUI.doubleZoomPoint = (x,y)
                        ax1.imshow(imgScaled)
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        for i in range(len(ax1.patches)-1,-1,-1):
                            ax1.patches[i].remove()
                        self.imageCanvas.draw_idle()

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
        if func[0] == "im_zoomin" and len(self.function) == 1 and self.doubleZoom.isChecked():
            if not self.doubleZoomGUI.doubleZoomMode:
                self.doubleZoomGUI.updateAxes(x, y)
                self.imageCanvas.draw_idle()
        if func[0] == "im_zoomin" and len(self.function) == 2:
            # draw rectangle
            if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                if len(ax.patches) > 0:
                    ax.patches[0].remove()
                start_pt = func[1]
                w = abs(start_pt[0] - x)
                h = abs(start_pt[1] - y)
                x = min(start_pt[0], x)
                y = min(start_pt[1], y)
                ax.add_patch(patches.Rectangle((x, y), w, h,
                                            linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            else:
                self.doubleZoomGUI.updateAxes(x, y)
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
        elif func[0] in ["slice"]:
            # draw X on points and a line between points
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5
            if len(func) == 1:

                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                    if len(ax.lines) > 0:
                        for i in range(len(ax.lines)-1,-1,-1):
                            if ax.lines[i].get_label() != "Blue Dot":
                                ax.lines[i].remove()
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomGUI.axes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            elif len(func) == 2:
                start_pt = func[1]
                    # ax.lines = first_cross
                # ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                # ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                # ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:

                    if len(ax.lines) > 2:
                        # first_cross = ax.lines[:2]
                        for i in range(len(ax.lines)-1,1,-1):
                            if ax.lines[i].get_label() != "Blue Dot":
                                ax.lines[i].remove()
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomGUI.axes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                        newX, newY = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
                        ax.plot((start_pt[0], newX), (start_pt[1], newY), color='r')
            elif len(func) >= 2:
                ax = self.imageAxes
                for i in range(len(ax.lines)-1,-1,-1):
                    ax.lines[i].remove()
                for i in range(len(ax.patches)-1,-1,-1):
                    ax.patches[i].remove()
                self.imageCanvas.draw_idle()
                if func[0] == "slice":
                    self.setSlice.setChecked(False)
                    self.setSliceChecked()

            self.imageCanvas.draw_idle()
        elif func[0] in ["dist"]:
            # draw X on points and a line between points
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomGUI.axes
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
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                    # ax.lines = first_cross
                # ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                # ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                # ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomGUI.axes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                        newX, newY = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
                        ax.plot((start_pt[0], newX), (start_pt[1], newY), color='r')
            elif len(func) % 2 != 0:
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                    if len(ax.lines) > 0:
                        n = (len(func)-1)*5//2 + 2
                    
                        for i in range(len(ax.lines)-1, n-3, -1):
                            if ax.lines[i].get_label() != "Blue Dot":
                                ax.lines[i].remove()
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomGUI.axes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            elif len(func) % 2 == 0:
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                    start_pt = func[-1]
                    if len(ax.lines) > 3:
                        n = len(func) * 5 // 2 - 1

                        for i in range(len(ax.lines) - 1, n-3, -1):
                            if ax.lines[i].get_label() != "Blue Dot":
                                ax.lines[i].remove()

                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomGUI.axes
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                        ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                        #ax1.plot((start_pt[0], x), (start_pt[1], y), color='r')
            self.imageCanvas.draw_idle()
        elif func[0] == "slice_box":
            # draw X on points and a line between points
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                if not self.doubleZoom.isChecked():
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                else:
                    if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                        axis_size = 1
                        ax1 = self.doubleZoomGUI.axes
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
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                    # ax.lines = first_cross
                # ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                # ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                # ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                    ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                    ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                    ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)
                    newX, newY = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
                    ax.plot((start_pt[0], newX), (start_pt[1], newY), color='r')
            elif len(func) == 3:
                if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                    for i in range(len(ax.lines)-1,1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                    angle = np.radians(90 - func[2][4])
                    p_mouse = np.array([x,y])
                    if func[2][0] < func[2][1]: # p1 is to the left
                        p_left = np.array((func[2][0], func[2][2]))
                        p_right = np.array((func[2][1], func[2][3]))
                    else:
                        p_left = np.array((func[2][1], func[2][3]))
                        p_right = np.array((func[2][0], func[2][2]))

                    v1 = p_left - p_right
                    v2 = p_mouse - p_right
                    u = v1/np.linalg.norm(v1)
                    n = (-u[1], u[0])

                    height = np.abs(np.dot(v2, n))
                    width = np.linalg.norm(p_left - p_right)

                    # if height*2 < width:

                    x1_left = p_left[0] - height*np.cos(angle)
                    y1_left = p_left[1] - height*np.sin(angle)
                    x2_left = p_left[0] + height*np.cos(angle)
                    y2_left = p_left[1] + height*np.sin(angle)
                    x1_right = p_right[0] - height*np.cos(angle)
                    y1_right = p_right[1] - height*np.sin(angle)
                    x2_right= p_right[0] + height*np.cos(angle)
                    y2_right= p_right[1] + height*np.sin(angle)

                    ax.plot([func[2][0], func[2][1]], [func[2][2], func[2][3]], color="r")

                    ax.plot([p_left[0], x2_left], [p_left[1], y2_left], color="r", linestyle='dotted')
                    ax.plot([x1_left, p_left[0]], [y1_left, p_left[1]], color="r", linestyle='dotted')

                    ax.plot([p_right[0], x2_right], [p_right[1], y2_right], color="r", linestyle='dotted')
                    ax.plot([x1_right, p_right[0]], [y1_right, p_right[1]], color="r", linestyle='dotted')

                    ax.plot([x1_left, x1_right], [y1_left, y1_right], color="r", linestyle='dotted')
                    ax.plot([x2_left, x2_right], [y2_left, y2_right], color="r", linestyle='dotted')
                else:
                    self.doubleZoomGUI.updateAxes(x, y)
            elif len(func) >= 4:
                ax = self.imageAxes
                for i in range(len(ax.lines)-1,-1,-1):
                    ax.lines[i].remove()
                for i in range(len(ax.patches)-1,-1,-1):
                    ax.patches[i].remove()
                self.imageCanvas.draw_idle()
                self.setSliceBox.setChecked(False)
                self.setSliceBoxChecked()
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
        if self.xrayViewer is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img_size = self.xrayViewer.orig_img.shape

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

    def ableToProcess(self):
        """
        Check if image can be processed
        """
        return not self.uiUpdating

    def initialWidgets(self, img):
        """
        Initial some widgets values which depends on current image
        :param img: selected image
        :param previnfo: info of the last image
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

        if 'float' in str(img.dtype):
            self.spmaxInt.setDecimals(2)
            self.spminInt.setDecimals(2)
        else:
            self.spmaxInt.setDecimals(2)
            self.spminInt.setDecimals(2)

        self.uiUpdating = False

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new viewer object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        fileName = self.fileManager.get_display_name()
        self.filenameLineEdit.setText(fileName)
        self.filenameLineEdit2.setText(fileName)

        try:
            img = self.fileManager.load_current()
            self.xrayViewer = XRayViewer(img)
        except Exception as e:
            infMsg = QMessageBox()
            infMsg.setText("Error Opening File: " + str(fileName))
            infMsg.setInformativeText("The file is likely corrupted or missing.\nError: " + str(e))
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()

        if self.inpaintChkBx.isChecked():
            self.statusPrint("Inpainting...")
            self.xrayViewer.orig_img = inpaint_img(self.xrayViewer.orig_img)
            self.statusPrint("")

        original_image = self.xrayViewer.orig_img
        self.imgDetailOnStatusBar.setText(
            str(original_image.shape[0]) + 'x' + str(original_image.shape[1]) + ' : ' + str(original_image.dtype))
        self.initialWidgets(original_image)
        if self.tabWidget.currentIndex() == 1 or self.saveGraphSlice.isChecked():
            if self.first_slice:
                self.setSliceChecked()
            elif self.first_box:
                self.setSliceBoxChecked()
            if self.saveGraphSlice.isChecked():
                self.csv_manager.writeNewData(self.xrayViewer)
        else:
            self.refreshAllTabs()
        self.send_signal()

    def closeEvent(self, ev):
        """
        Close the event
        """
        self.close()

    def refreshAllTabs(self):
        """
        Set all tab update status to be not update, and Refresh (Redraw) all tab
        """
        self.updated['img'] = False
        self.function = None
        self.updateUI()
        self.resetStatusbar()

    def refreshImageTab(self):
        """
        Refresh (Redraw) image tab
        """
        self.updated['img'] = False
        self.updateUI()

    def updateUI(self):
        """
        Update current all widget in current tab , spinboxes, and refresh status bar
        """
        if self.ableToProcess():
            if self.tabWidget.currentIndex() == 0:
                self.scrollWheelStatusBar.setVisible(False)
                self.updateImageTab()
            elif self.tabWidget.currentIndex() == 1 and self.xrayViewer.hist is not None:
                self.scrollWheelStatusBar.setVisible(True)
                self.updateFittingTab(self.xrayViewer.hist)

    def updateImageTab(self):
        """
        Display image in image tab, and draw lines
        """
        if not self.updated['img']:
            self.uiUpdating = True

            ax = self.imageAxes
            ax.cla()
            img = self.xrayViewer.orig_img
            if self.logScaleIntChkBx.isChecked():
                ax.imshow(img, cmap=self.colorMapChoice.currentText(), norm=LogNorm(vmin=max(1, self.spminInt.value()), vmax=self.spmaxInt.value()))
            else:
                ax.imshow(img, cmap=self.colorMapChoice.currentText(), norm=Normalize(vmin=self.spminInt.value(), vmax=self.spmaxInt.value()))
            ax.set_facecolor('black')

            # Set Zoom in location
            if self.img_zoom is not None and len(self.img_zoom) == 2:
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
            elif self.default_img_zoom is not None and len(self.default_img_zoom) == 2:
                ax.set_xlim(self.default_img_zoom[0])
                ax.set_ylim(self.default_img_zoom[1])
            else:
                ax.set_xlim((0, img.shape[1]))
                ax.set_ylim((0, img.shape[0]))

            self.img_zoom = [ax.get_xlim(), ax.get_ylim()]
            ax.invert_yaxis()
            self.imageFigure.tight_layout()
            self.imageCanvas.draw()
            self.updated['img'] = True
            self.uiUpdating = False

    def updateFittingTab(self, hist):
        """
        Draw all UI in fitting tab
        """
        ax = self.fittingAxes
        ax.cla()
        ax.plot(hist, color = 'k')

        # Zoom
        self.plot_min = -50
        func = self.function
        if func is None:
            func = self.saved_slice
        if self.graph_zoom is not None and len(self.graph_zoom) == 2:
            ax.set_xlim(self.graph_zoom[0])
            ax.set_ylim(self.graph_zoom[1])
        elif self.default_img_zoom is not None and len(self.default_img_zoom) == 2:
            ax.set_xlim(self.default_img_zoom[0])
            ax.set_ylim(self.default_img_zoom[1])
        else:
            self.plot_min = ax.get_ylim()[0]
            # ax.set_xlim(0, len(self.xrayViewer.hist))
            if func[0] == "slice":
                ax.set_xlim(func[1][0], func[2][0])
            elif func[0] == "slice_box":
                ax.set_xlim(0, len(self.xrayViewer.hist))

        self.fittingFigure.tight_layout()
        self.fittingCanvas.draw()

    def resetStatusbar(self):
        """
        Reset the status bar
        """
        dir_path = getattr(self.fileManager, 'dir_path', '')
        fileFullPath = fullPath(dir_path, self.fileManager.get_display_name())
        # Use names (final) if available, else fallback to file_list length
        total_count = len(self.fileManager.names) if getattr(self.fileManager, 'names', None) else len(self.fileManager.file_list)
        total = str(total_count) + ('*' if self._provisionalCount else '')
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(self.fileManager.current + 1) + '/' + total + ') : ' + fileFullPath)

    def onNewFileSelected(self, newFile):
        """
        Load selected file and setup for navigation
        Two-phase loading:
        1. Sync: FileManager scans directory and displays selected file
        2. Async: background scan to expand all HDF5 frames for accurate count
        :param newFile: full name of selected file
        """
        QApplication.setOverrideCursor(Qt.WaitCursor)
        
        # Create or get FileManager
        fm = getattr(self, 'fileManager', None)
        if fm is None:
            self.fileManager = FileManager()
        
        # Phase 1: Initialize FileManager with selected file (includes fast directory scan)
        self.fileManager.set_from_file(str(newFile))
        
        # Mark counts provisional until async scan completes
        self._provisionalCount = True

        if self.fileManager.dir_path and self.fileManager.file_list:
            fileName = self.fileManager.get_display_name()
            self.setH5Mode(str(newFile))
            self.csv_manager = XV_CSVManager(self.fileManager.dir_path)
            self.selectImageButton.setHidden(True)
            self.imageCanvas.setHidden(False)
            self.updateLeftWidgetWidth()
            self.tabWidget.setTabEnabled(1, True)
            self.onImageChanged()
            
            # Phase 2: Background async scan to expand all HDF5 frames
            self._scan_timer.start()
            # Let FileManager scan and update its own listing; GUI just polls and updates counts
            self.fileManager.start_async_scan(self.fileManager.dir_path)
        
        QApplication.restoreOverrideCursor()
            


    def setH5Mode(self, file_name):
        """
        Sets the H5 list of file and displays the right set of buttons depending on the file selected
        """
        ext = ''
        try:
            _, ext = os.path.splitext(str(file_name))
            ext = ext.lower()
        except Exception:
            ext = ''
        if ext in ['.h5', '.hdf5']:
            # Show navigation buttons for H5; we no longer maintain explicit H5 lists here
            self.nextFileButton.show()
            self.prevFileButton.show()
            self.nextFileButton2.show()
            self.prevFileButton2.show()
        else:
            self.nextFileButton.hide()
            self.prevFileButton.hide()
            self.nextFileButton2.hide()
            self.prevFileButton2.hide()


    def _checkScanDone(self):
        """
        Check if background directory scan is complete.
        Updates the image layer with full HDF5 frame expansion for accurate count.
        """
        if not self.fileManager:
            return
        # When FileManager finishes, it has already updated names/specs
        if not self.fileManager.is_scan_done():
            return
        self._provisionalCount = False
        self._scan_timer.stop()
        self.resetStatusbar()

    def batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if (self.processFolderButton.isChecked() or self.processFolderButton2.isChecked()) and self.processFolderButton.text() == "Play":
            if not self.progressBar.isVisible():
                self.processFolderButton.setText("Pause")
                self.processFolderButton.setChecked(True)
                self.processFolderButton2.setText("Pause")
                self.processFolderButton2.setChecked(True)
                self.processFolder()
        else:
            self.stop_process = True

    def processFolder(self):
        """
        Triggered when a folder has been selected to process it
        """
        self.stop_process = False
        self.progressBar.setVisible(True)
        total_count = len(self.fileManager.names) if getattr(self.fileManager, 'names', None) else len(self.fileManager.file_list)
        self.progressBar.setRange(0, total_count)
        for i in range(self.fileManager.current, total_count):
            if self.stop_process:
                break
            self.progressBar.setValue(i)
            QApplication.processEvents()
            self.nextClicked()
        self.progressBar.setVisible(False)

        self.processFolderButton.setChecked(False)
        self.processFolderButton.setText("Play")
        self.processFolderButton2.setChecked(False)
        self.processFolderButton2.setText("Play")

    def browseFile(self):
        """
        Popup input dialog and set file selection
        """
        file_name = getAFile()
        if file_name != "":
            try:
                self.onNewFileSelected(str(file_name))
                self.centralWidget.setMinimumSize(700, 500)
            except:
                infMsg = QMessageBox()
                infMsg.setText("Error Opening File: " + str(file_name))
                infMsg.setInformativeText("The file is likely corrupted or missing.")
                infMsg.setStandardButtons(QMessageBox.Ok)
                infMsg.setIcon(QMessageBox.Information)
                infMsg.exec_()

    def prevClicked(self):
        """
        Going to the previous image
        """
        total_count = len(self.fileManager.names) if getattr(self.fileManager, 'names', None) else len(self.fileManager.file_list)
        if total_count > 0:
            self.fileManager.prev_frame()
            self.onImageChanged()

    def nextClicked(self):
        """
        Going to the next image
        """
        total_count = len(self.fileManager.names) if getattr(self.fileManager, 'names', None) else len(self.fileManager.file_list)
        if total_count > 0:
            self.fileManager.next_frame()
            self.onImageChanged()
    
    def prevFileClicked(self):
        """
        Jump to first frame of previous file (typically for HDF5 navigation)
        """
        if len(self.fileManager.file_list) > 1:
            self.fileManager.prev_file()
            self.onImageChanged()

    def nextFileClicked(self):
        """
        Jump to first frame of next file (typically for HDF5 navigation)
        """
        if len(self.fileManager.file_list) > 1:
            self.fileManager.next_file()
            self.onImageChanged()

    def fileNameChanged(self):
        """
        Triggered when the name of the current file is changed
        """
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            fileName = self.filenameLineEdit.text().strip()
        elif selected_tab == 1:
            fileName = self.filenameLineEdit2.text().strip()
        if fileName not in self.fileManager.names:
            return
        self.fileManager.switch_to_image_by_name(fileName)
        self.onImageChanged()

    def showAbout(self):
        """
        Display About Dialog
        """
        msgBox = QMessageBox()
        msgBox.setWindowTitle("About")
        msgBox.setTextFormat(Qt.RichText)
        msgBox.setText("<br><br><br>" +
                       "X-Ray Viewer is running under" +
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

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        self.statusReport.setText(text)
        QApplication.processEvents()
