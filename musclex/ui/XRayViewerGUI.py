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
from musclex import __version__
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.XRayViewer import XRayViewer
from ..csv_manager.XV_CSVManager import XV_CSVManager
from .pyqt_utils import *
from .LogTraceViewer import LogTraceViewer

class XRayViewerGUI(QMainWindow):
    """
    A class for window displaying all information of a selected image.
    This window contains 2 tabs : image, and graph
    """
    currSaved = pyqtSignal(int)
    def __init__(self):
        """
        Initial window
        """
        QWidget.__init__(self)
        self.imgList = [] # all images name in current directory
        self.h5List = [] # if the file selected is an H5 file, regroups all the other h5 files names
        self.h5index = 0
        self.windowList = []
        self.counter = 0
        self.filePath = "" # current directory
        self.numberOfFiles = 0
        self.currentFileNumber = 0
        self.img_zoom = None # zoom location of original image (x,y range)
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

        self.initUI() # initial all GUI
        self.setConnections() # set triggered function for widgets
        self.setMinimumHeight(800)
        self.setMinimumWidth(1000)

    def initUI(self):
        """
        Open a file finder and return the name of the file selected
        """
        self.setWindowTitle("X-Ray Viewer v." + __version__)

        self.centralWidget = QWidget(self)
        self.setCentralWidget(self.centralWidget)
        self.mainHLayout = QHBoxLayout(self.centralWidget)

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
        self.tabWidget.addTab(self.imageTab, "Image")

        self.verImgLayout = QVBoxLayout()
        self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)
        self.selectImageButton = QPushButton('Click Here to Select an Image...')
        self.selectImageButton.setFixedHeight(100)
        self.selectImageButton.setFixedWidth(300)

        self.verImgLayout.addWidget(self.selectImageButton)
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
        self.displayOptGrpBx.setLayout(self.dispOptLayout)

        self.optionsLayout = QVBoxLayout()
        self.optionsLayout.setAlignment(Qt.AlignCenter)
        self.settingsGroup = QGroupBox("Image Processing")
        self.settingsLayout = QGridLayout()
        self.settingsGroup.setLayout(self.settingsLayout)

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

        self.settingsLayout.addWidget(self.openTrace, 0, 0, 1, 2)
        self.settingsLayout.addWidget(self.measureDist, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.setSlice, 2, 0, 1, 2)
        self.settingsLayout.addWidget(self.setSliceBox, 3, 0, 1, 2)
        self.settingsLayout.addWidget(self.saveGraphSlice, 4, 0, 1, 2)
        self.settingsLayout.addWidget(self.inpaintChkBx, 5, 0, 1, 2)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Play")
        self.processFolderButton.setStyleSheet(pfss)
        self.processFolderButton.setCheckable(True)

        self.nextButton = QPushButton(">")
        self.prevButton = QPushButton("<")
        self.nextFileButton = QPushButton(">>>")
        self.prevFileButton = QPushButton("<<<")
        self.nextButton.setToolTip('Next Frame')
        self.prevButton.setToolTip('Previous Frame')
        self.nextFileButton.setToolTip('Next H5 File in this Folder')
        self.prevFileButton.setToolTip('Previous H5 File in this Folder')
        self.filenameLineEdit = QLineEdit()
        self.buttonsLayout = QGridLayout()
        self.buttonsLayout.addWidget(self.processFolderButton,0,0,1,2)
        self.buttonsLayout.addWidget(self.prevButton,1,0,1,1)
        self.buttonsLayout.addWidget(self.nextButton,1,1,1,1)
        self.buttonsLayout.addWidget(self.prevFileButton,2,0,1,1)
        self.buttonsLayout.addWidget(self.nextFileButton,2,1,1,1)
        self.buttonsLayout.addWidget(self.filenameLineEdit,3,0,1,2)

        self.displayOptGrpBx.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingsGroup)
        self.optionsLayout.addStretch()
        self.optionsLayout.addLayout(self.buttonsLayout)
        self.frameOfKeys = QFrame()
        self.frameOfKeys.setFixedWidth(350)
        self.frameOfKeys.setLayout(self.optionsLayout)
        self.imageTabLayout.addWidget(self.frameOfKeys)

        self.processFolderButton2 = QPushButton("Play")
        self.processFolderButton2.setStyleSheet(pfss)
        self.processFolderButton2.setCheckable(True)
        self.bottomLayout2 = QGridLayout()
        self.nextButton2 = QPushButton(">")
        self.prevButton2 = QPushButton("<")
        self.nextFileButton2 = QPushButton(">>>")
        self.prevFileButton2 = QPushButton("<<<")
        self.nextButton2.setToolTip('Next Frame')
        self.prevButton2.setToolTip('Previous Frame')
        self.nextFileButton2.setToolTip('Next H5 File in this Folder')
        self.prevFileButton2.setToolTip('Previous H5 File in this Folder')
        self.filenameLineEdit2 = QLineEdit()
        self.bottomLayout2.addWidget(self.processFolderButton2, 0, 0, 1, 2)
        self.bottomLayout2.addWidget(self.prevButton2, 1, 0, 1, 1)
        self.bottomLayout2.addWidget(self.nextButton2, 1, 1, 1, 1)
        self.bottomLayout2.addWidget(self.prevFileButton2, 2, 0, 1, 1)
        self.bottomLayout2.addWidget(self.nextFileButton2, 2, 1, 1, 1)
        self.bottomLayout2.addWidget(self.filenameLineEdit2, 3, 0, 1, 2)

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
        self.setStatusBar(self.statusBar)

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
        self.openTrace.clicked.connect(self.openTraceClicked)
        self.measureDist.clicked.connect(self.measureDistChecked)
        self.setSlice.clicked.connect(self.setSliceChecked)
        self.setSliceBox.clicked.connect(self.setSliceBoxChecked)
        self.saveGraphSlice.stateChanged.connect(self.saveGraphSliceChecked)
        self.inpaintChkBx.stateChanged.connect(self.onImageChanged)

        self.selectImageButton.clicked.connect(self.browseFile)
        self.imgZoomInB.clicked.connect(self.imageZoomIn)
        self.imgZoomOutB.clicked.connect(self.imageZoomOut)
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

    def openTraceClicked(self):
        """
        Triggered when the Open Trace button is clicked
        """
        newWindows = LogTraceViewer(self, self.currentFileNumber)
        if len(self.windowList) >= 1:
            self.windowList = []
            self.counter = 0
        self.windowList.append(newWindows)

    def send_signal(self):
        if self.windowList != []:
            signal=self.currentFileNumber
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
                    self.imgZoomInB.setChecked(False)
                    self.refreshAllTabs()

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
            self.imgCoordOnStatusBar.setText("x=" + str(round(x,2)) + ', y=' + str(round(y,2)))

        if self.function is None or len(self.function) < 2:
            return

        func = self.function
        if func[0] == "g_zoomin":
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
        elif func[0] == "g_move":
            # Change zoom location (x, y ranges) in order to go round plot by dragging
            if self.graph_zoom is not None:
                hist = self.xrayViewer.hist
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
            if self.function is not None and self.function[0] == "dist":
                func = self.function
                for i in range(1, len(func) - 1, 2):
                    dist = math.dist(func[i], func[i+1])
                print("Distance:", str(round(dist,2)), "pixels")
                msg = QMessageBox()
                msg.setInformativeText("Distance: " + str(round(dist,2)) + " pixels")
                msg.setStandardButtons(QMessageBox.Ok)
                msg.setWindowTitle("Measure a Distance")
                msg.setStyleSheet("QLabel{min-width: 500px;}")
                msg.exec_()
            self.refreshAllTabs()

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
            elif func[0] in ["slice", "dist"]:
                ax = self.imageAxes
                axis_size = 5
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                self.imageCanvas.draw_idle()
                func.append((x, y))
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

    def imageOnMotion(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if not self.ableToProcess():
            return
        x = event.xdata
        y = event.ydata
        img = self.xrayViewer.orig_img

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
                ax.patches[0].remove()
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
        elif func[0] in ["slice", "dist"]:
            # draw X on points and a line between points
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    # first_cross = ax.lines[:2]
                    for i in range(len(ax.lines)-1,1,-1):
                        ax.lines[i].remove()
                    # ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
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
                elif func[0] == "dist":
                    self.measureDist.setChecked(False)
                    self.measureDistChecked()
            self.imageCanvas.draw_idle()
        elif func[0] == "slice_box":
            # draw X on points and a line between points
            ax = self.imageAxes
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5
            if len(func) == 1:
                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            elif len(func) == 2:
                start_pt = func[-1]
                if len(ax.lines) > 3:
                    n = len(func) * 5 // 2 - 1
                    # first_cross = ax.lines[:n]
                    for i in range(len(ax.lines)-1,n-1,-1):
                        ax.lines[i].remove()
                    # ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            elif len(func) == 3:
                for i in range(len(ax.lines)-1,1,-1):
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

                if height*2 < width:

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
        fileName = self.imgList[self.currentFileNumber]
        self.filenameLineEdit.setText(fileName)
        self.filenameLineEdit2.setText(fileName)

        self.xrayViewer = XRayViewer(self.filePath, fileName, self.fileList, self.ext)

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
                self.updateImageTab()

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
        if self.graph_zoom is not None and len(self.graph_zoom) == 2:
            ax.set_xlim(self.graph_zoom[0])
            ax.set_ylim(self.graph_zoom[1])
        elif self.default_img_zoom is not None and len(self.default_img_zoom) == 2:
            ax.set_xlim(self.default_img_zoom[0])
            ax.set_ylim(self.default_img_zoom[1])
        else:
            self.plot_min = ax.get_ylim()[0]
            ax.set_xlim(0, len(self.xrayViewer.hist))

        self.fittingFigure.tight_layout()
        self.fittingCanvas.draw()

    def resetStatusbar(self):
        """
        Reset the status bar
        """
        fileFullPath = fullPath(self.filePath, self.imgList[self.currentFileNumber])
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(self.currentFileNumber + 1) + '/' + str(self.numberOfFiles) + ') : ' + fileFullPath)

    def onNewFileSelected(self, newFile):
        """
        Preprocess folder of the file and process current image
        :param newFile: full name of selected file
        """
        QApplication.setOverrideCursor(Qt.WaitCursor)
        self.filePath, self.imgList, self.currentFileNumber, self.fileList, self.ext = getImgFiles(str(newFile))
        self.numberOfFiles = len(self.imgList)
        fileName = self.imgList[self.currentFileNumber]
        self.h5List = []
        self.setH5Mode(str(newFile))
        self.xrayViewer = XRayViewer(self.filePath, fileName, self.fileList, self.ext)
        self.csv_manager = XV_CSVManager(self.filePath)
        self.selectImageButton.setHidden(True)
        self.imageCanvas.setHidden(False)
        self.tabWidget.setTabEnabled(1, True)
        self.onImageChanged()
        QApplication.restoreOverrideCursor()

    def setH5Mode(self, file_name):
        """
        Sets the H5 list of file and displays the right set of buttons depending on the file selected
        """
        if self.ext in ['.h5', '.hdf5']:
            for file in os.listdir(self.filePath):
                if file.endswith(".h5") or file.endswith(".hdf5"):
                    self.h5List.append(file)
            self.h5index = self.h5List.index(os.path.split(file_name)[1])
            self.nextFileButton.show()
            self.prevFileButton.show()
            self.nextFileButton2.show()
            self.prevFileButton2.show()
        else:
            self.nextFileButton.hide()
            self.prevFileButton.hide()
            self.nextFileButton2.hide()
            self.prevFileButton2.hide()

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
        self.progressBar.setRange(0, self.numberOfFiles)
        for i in range(self.currentFileNumber, self.numberOfFiles):
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
    
    def prevFileClicked(self):
        """
        Going to the previous h5 file
        """
        if len(self.h5List) > 1:
            self.h5index = (self.h5index - 1) % len(self.h5List)
            self.onNewFileSelected(os.path.join(self.filePath, self.h5List[self.h5index]))

    def nextFileClicked(self):
        """
        Going to the next h5 file
        """
        if len(self.h5List) > 1:
            self.h5index = (self.h5index + 1) % len(self.h5List)
            self.onNewFileSelected(os.path.join(self.filePath, self.h5List[self.h5index]))

    def fileNameChanged(self):
        """
        Triggered when the name of the current file is changed
        """
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            fileName = self.filenameLineEdit.text().strip()
        elif selected_tab == 1:
            fileName = self.filenameLineEdit2.text().strip()
        if fileName not in self.imgList:
            return
        self.currentFileNumber = self.imgList.index(fileName)
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