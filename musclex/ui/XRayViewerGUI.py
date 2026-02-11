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
from .widgets.image_navigator_widget import ImageNavigatorWidget


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
        # State variables
        self.windowList = []
        self.counter = 0
        self.calSettings = None
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
        
        # Create ImageNavigatorWidget (handles file management, display, navigation)
        self.navigator = ImageNavigatorWidget(
            parent=self,
            show_display_panel=True,
            show_double_zoom=True,
            auto_display=True,  # Navigator displays images automatically
            navigation_process_folder_text="Play Current Folder",
            navigation_process_h5_text="Play Current H5 File"
        )
        
        # Expose components for compatibility
        self.file_manager = self.navigator.file_manager
        self.imageAxes = self.navigator.image_viewer.axes
        self.imageCanvas = self.navigator.image_viewer.canvas
        self.imageFigure = self.navigator.image_viewer.figure
        self.navControls = self.navigator.nav_controls
        
        # Connect navigator's signals
        self.navigator.imageChanged.connect(self._on_image_changed)
        
        # Connect ImageViewer's interaction signals (high-level, cleaner than matplotlib events)
        self.navigator.image_viewer.coordinatesChanged.connect(self._on_coordinates_changed)
        self.navigator.image_viewer.canvasClicked.connect(self._on_canvas_clicked)

        self.initUI() # initial all GUI
        self.setConnections() # set triggered function for widgets
        #self.setMinimumHeight(800)
        #self.setMinimumWidth(1000)


    def initUI(self):
        """
        Open a file finder and return the name of the file selected
        """
        self.setWindowTitle("X-Ray Viewer v." + __version__)

        # Create main container that holds scroll area AND status bars
        self.mainContainer = QWidget(self)
        self.mainContainerLayout = QVBoxLayout(self.mainContainer)
        self.mainContainerLayout.setContentsMargins(0, 0, 0, 0)
        self.mainContainerLayout.setSpacing(0)
        self.setCentralWidget(self.mainContainer)
        
        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.mainContainerLayout.addWidget(self.scrollArea, 1)  # Scroll area takes all space

        self.centralWidget = QWidget()
        self.scrollArea.setWidget(self.centralWidget)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 200px; }")
        self.mainLayout.addWidget(self.tabWidget)

        ##### Image Tab - Use Navigator #####
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.tabWidget.addTab(self.imageTab, "Image")
        
        # Add navigator (includes image viewer + right panel with display options + nav controls)
        self.imageTabLayout.addWidget(self.navigator, 1)
        
        # Create custom settings group for XRayViewer features
        self.settingsGroup = QGroupBox("Image Processing")
        self.settingsGroup.setStyleSheet("QGroupBox { font-weight: bold; }")
        self.settingsLayout = QGridLayout()
        self.settingsGroup.setLayout(self.settingsLayout)

        self.calibrationButton = QPushButton("Calibration Settings")
        self.calibrationButton.setToolTip("Open calibration settings to set detector parameters\nand calculate q-values from pixel distances.")
        self.openTrace = QPushButton("Open Trace Window")
        self.openTrace.setToolTip("Open a separate window to view intensity traces\nacross multiple images in the folder.")
        self.measureDist = QPushButton("Measure a Distance")
        self.measureDist.setCheckable(True)
        self.measureDist.setToolTip("Click two points on the image to measure the distance between them.\nYou can measure multiple distances. Press ESC or uncheck to finish.")
        self.setSlice = QPushButton("Set Graph Slice")
        self.setSlice.setCheckable(True)
        self.setSlice.setToolTip("Click two points to define a slice line.\nThe intensity profile along this line will be shown in the Graph tab.")
        self.setSliceBox = QPushButton("Set Graph Box")
        self.setSliceBox.setCheckable(True)
        self.setSliceBox.setToolTip("Click three points to define a box region:\n1. First click: start of center line\n2. Second click: end of center line\n3. Third click: set the box width\nThe summed intensity within the box will be shown in the Graph tab.")
        self.saveGraphSlice = QCheckBox("Save Graph Profile")
        self.saveGraphSlice.setEnabled(False)
        self.saveGraphSlice.setToolTip("When checked, the graph profile data will be saved to a CSV file.")
        self.inpaintChkBx = QCheckBox("Inpainting")
        self.inpaintChkBx.setToolTip("Fill in masked/dead pixels using surrounding pixel values.\nUseful for removing detector artifacts.")
        
        self.checkableButtons.extend([self.measureDist, self.setSlice, self.setSliceBox])
        
        self.settingsLayout.addWidget(self.calibrationButton, 0, 0, 1, 2)
        self.settingsLayout.addWidget(self.openTrace, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.measureDist, 2, 0, 1, 2)
        self.settingsLayout.addWidget(self.setSlice, 3, 0, 1, 2)
        self.settingsLayout.addWidget(self.setSliceBox, 4, 0, 1, 2)
        self.settingsLayout.addWidget(self.saveGraphSlice, 5, 0, 1, 2)
        self.settingsLayout.addWidget(self.inpaintChkBx, 6, 0, 1, 2)
        
        # Add settings to navigator's right panel
        self.navigator.right_panel.add_widget(self.settingsGroup)
        
        self.measureDist2 = QPushButton("Measure a Distance")
        self.measureDist2.setCheckable(True)
        self.measureDist2.setToolTip("Click two points on the graph to measure the distance between them.")

        self.zoomInGraphButton = QPushButton("Zoom In")
        self.zoomInGraphButton.setCheckable(True)
        self.zoomInGraphButton.setToolTip("Draw a rectangle on the graph to zoom into that region.")

        self.resetZoomButton = QPushButton("Reset Zoom")
        self.resetZoomButton.setToolTip("Reset the graph view to show the full data range.")
        
        # Navigation widget container for Graph tab (widget moved here on tab switch)
        self.bottomLayout2 = QGridLayout()

        self.bottomLayout2.addWidget(self.zoomInGraphButton, 0, 0, 1, 2)
        self.bottomLayout2.addWidget(self.resetZoomButton, 2, 0, 1, 2)
        self.bottomLayout2.addWidget(self.measureDist2, 3, 0, 1, 2)
        # navControls will be added here dynamically when switching to Graph tab

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
        self.statusBar.setSizeGripEnabled(False)  # Disable size grip to prevent layout changes
        self.progressBar = QProgressBar()
        self.progressBar.setMaximum(100)
        self.progressBar.setMinimum(0)
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusReport = QLabel()
        self.imgDetailOnStatusBar = QLabel()
        self.imgCoordOnStatusBar = QLabel()
        self.imgCoordOnStatusBar.setMinimumWidth(450)  # Fixed width to prevent resize
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

        #Add both status bars OUTSIDE scroll area to prevent jitter
        self.mainContainerLayout.addWidget(self.scrollWheelStatusBar)
        self.mainContainerLayout.addWidget(self.statusBar)

        #### Menu Bar #####
        selectImageAction = QAction('Select an Image...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.triggered.connect(self.navigator.browse_file)

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
        self.tabWidget.currentChanged.connect(self.onTabChanged)

        ##### Navigation & Display - Handled by Navigator #####
        # Image interaction signals already connected in __init__:
        # - navigator.imageChanged -> _on_image_changed
        # - navigator.image_viewer.coordinatesChanged -> _on_coordinates_changed  
        # - navigator.image_viewer.canvasClicked -> _on_canvas_clicked
        # Display options (zoom, intensity, etc.) handled internally by ImageViewer
        # Navigation buttons handled internally by Navigator
        
        ##### Batch processing #####
        self.navControls.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.navControls.processH5Button.toggled.connect(self.h5batchProcBtnToggled)
        
        ##### Custom XRayViewer features #####
        self.calibrationButton.clicked.connect(self.launchCalibrationSettings)
        self.openTrace.clicked.connect(self.openTraceClicked)
        self.measureDist.clicked.connect(self.measureDistChecked)
        self.setSlice.clicked.connect(self.setSliceChecked)
        self.setSliceBox.clicked.connect(self.setSliceBoxChecked)
        self.saveGraphSlice.stateChanged.connect(self.saveGraphSliceChecked)
        self.inpaintChkBx.stateChanged.connect(self._reprocess_with_inpaint)

        ##### Graph tab - Keep matplotlib events (different canvas) #####
        self.fittingFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.fittingFigure.canvas.mpl_connect('motion_notify_event', self.plotOnMotion)
        self.fittingFigure.canvas.mpl_connect('button_release_event', self.plotReleased)
        self.fittingFigure.canvas.mpl_connect('figure_leave_event', self.leavePlot)
        self.fittingFigure.canvas.mpl_connect('scroll_event', self.plotScrolled)
        self.measureDist2.clicked.connect(self.measureDistChecked2)
        self.resetZoomButton.clicked.connect(self.resetZoomClicked)
        self.zoomInGraphButton.clicked.connect(self.zoomInGraphClicked)
        self.checkableButtons.append(self.measureDist2)

    # ========== Navigator Signal Handlers ==========
    
    def _on_image_changed(self, img, filename, dir_path):
        """
        Called when navigator loads a new image.
        This is the ONLY entry point for image changes.
        """
        # Initialize CSV manager if needed
        if not hasattr(self, 'csv_manager'):
            self.csv_manager = XV_CSVManager(dir_path)
        
        # Create XRayViewer
        try:
            self.xrayViewer = XRayViewer(img)
        except Exception as e:
            infMsg = QMessageBox()
            infMsg.setText("Error Opening File: " + str(filename))
            infMsg.setInformativeText(f"The file is likely corrupted or missing.\nError: {str(e)}")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            return
        
        # Apply inpainting if enabled
        if self.inpaintChkBx.isChecked():
            self.statusPrint("Inpainting...")
            self.xrayViewer.orig_img = inpaint_img(self.xrayViewer.orig_img)
            self.statusPrint("")
            # Redisplay the inpainted image
            self.navigator.image_viewer.display_image(self.xrayViewer.orig_img)
        
        # Update status bar with image info
        original_image = self.xrayViewer.orig_img
        self.imgDetailOnStatusBar.setText(
            f"{original_image.shape[0]}x{original_image.shape[1]} : {original_image.dtype}")
        
        # Handle tab-specific updates
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
        self.resetStatusbar()
    
    def _on_coordinates_changed(self, x, y, value):
        """
        Handle coordinate changes (mouse motion over image).
        Replaces imageOnMotion for basic coordinate display.
        Only called when no ImageViewer tool is active.
        """
        if not self.ableToProcess() or not self.xrayViewer:
            return
        
        # Display coordinates with calibrated q-value
        if 'center' not in self.xrayViewer.info:
            q, units = [-1, ""]
        elif self.calSettings and 'scale' in self.calSettings:
            q, units = inverseNmFromCenter([x, y], 
                                          self.xrayViewer.info['center'], 
                                          self.calSettings['scale'])
        else:
            q, units = [-1, ""]
        
        self.imgCoordOnStatusBar.setText(
            f"x={x:.2f}, y={y:.2f}, value={value:.2f}, distance={q:.2f}{units}")
        
        # Custom tool drawing (only if function is active)
        if self.function is None:
            return
        
        func = self.function
        ax = self.imageAxes
        axis_size = 5
        
        if func[0] == "slice":
            # Draw slice markers as mouse moves
            if len(func) == 1:
                # Clear old markers
                for i in range(len(ax.lines)-1,-1,-1):
                    if ax.lines[i].get_label() != "Blue Dot":
                        ax.lines[i].remove()
                # Draw crosshair
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            elif len(func) == 2:
                # Draw line from first point to cursor
                start_pt = func[1]
                if len(ax.lines) > 2:
                    for i in range(len(ax.lines)-1,1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            elif len(func) >= 3:
                # Auto-complete slice after second point is clicked
                for i in range(len(ax.lines)-1,-1,-1):
                    ax.lines[i].remove()
                for i in range(len(ax.patches)-1,-1,-1):
                    ax.patches[i].remove()
                self.imageCanvas.draw_idle()
                self.setSlice.setChecked(False)
                self.setSliceChecked()
                return  # Exit early since setSliceChecked will redraw
            self.imageCanvas.draw_idle()
        
        elif func[0] == "dist":
            # Draw distance measurement markers
            if len(func) == 1:
                for i in range(len(ax.lines)-1,-1,-1):
                    if ax.lines[i].get_label() != "Blue Dot":
                        ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    for i in range(len(ax.lines)-1,1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            self.imageCanvas.draw_idle()
        
        elif func[0] == "slice_box":
            # Draw slice box markers as mouse moves
            if len(func) == 1:
                # First point, draw crosshair
                for i in range(len(ax.lines)-1,-1,-1):
                    if ax.lines[i].get_label() != "Blue Dot":
                        ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            elif len(func) == 2:
                # Second point, draw line
                start_pt = func[1]
                if len(ax.lines) > 2:
                    for i in range(len(ax.lines)-1,1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            elif len(func) == 3:
                # Third point for width, draw box preview
                for i in range(len(ax.lines)-1,1,-1):
                    if ax.lines[i].get_label() != "Blue Dot":
                        ax.lines[i].remove()
                angle = np.radians(90 - func[2][4])
                p_mouse = np.array([x, y])
                if func[2][0] < func[2][1]:  # p1 is to the left
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
                
                x1_left = p_left[0] - height*np.cos(angle)
                y1_left = p_left[1] - height*np.sin(angle)
                x2_left = p_left[0] + height*np.cos(angle)
                y2_left = p_left[1] + height*np.sin(angle)
                x1_right = p_right[0] - height*np.cos(angle)
                y1_right = p_right[1] - height*np.sin(angle)
                x2_right = p_right[0] + height*np.cos(angle)
                y2_right = p_right[1] + height*np.sin(angle)
                
                ax.plot([func[2][0], func[2][1]], [func[2][2], func[2][3]], color="r")
                ax.plot([p_left[0], x2_left], [p_left[1], y2_left], color="r", linestyle='dotted')
                ax.plot([x1_left, p_left[0]], [y1_left, p_left[1]], color="r", linestyle='dotted')
                ax.plot([p_right[0], x2_right], [p_right[1], y2_right], color="r", linestyle='dotted')
                ax.plot([x1_right, p_right[0]], [y1_right, p_right[1]], color="r", linestyle='dotted')
                ax.plot([x1_left, x1_right], [y1_left, y1_right], color="r", linestyle='dotted')
                ax.plot([x2_left, x2_right], [y2_left, y2_right], color="r", linestyle='dotted')
            elif len(func) >= 4:
                # Auto-complete slice box after third point is clicked
                for i in range(len(ax.lines)-1,-1,-1):
                    ax.lines[i].remove()
                for i in range(len(ax.patches)-1,-1,-1):
                    ax.patches[i].remove()
                self.imageCanvas.draw_idle()
                self.setSliceBox.setChecked(False)
                self.setSliceBoxChecked()
                return  # Exit early since setSliceBoxChecked will redraw
            self.imageCanvas.draw_idle()
    
    def _on_canvas_clicked(self, event):
        """
        Handle canvas clicks for custom tools.
        Replaces imageClicked for custom tool handling.
        Only called when no ImageViewer tool is active.
        """
        if not self.ableToProcess():
            return
        
        x = event.xdata
        y = event.ydata
        
        if x is None or y is None:
            return
        
        # Handle custom tools
        if self.function is None:
            return
        
        func = self.function
        ax = self.imageAxes
        axis_size = 5
        
        if func[0] == "slice":
            # Record click point for slice
            ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.imageCanvas.draw_idle()
            func.append((x, y))
        
        elif func[0] == "dist":
            # Record click point for distance measurement
            ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.imageCanvas.draw_idle()
            func.append((x, y))
            self.updateMeasureDistBox()
            if len(func) > 2 and len(func) % 2 != 0:
                ax.add_artist(self.makeText(func[-1], func[-2], str(int((len(func) - 1) / 2))))
        
        elif func[0] == "slice_box":
            # Handle slice box clicks (3-click process: point 1, point 2 with angle, point 3 for width)
            if len(func) == 1:
                # First click: just record the point
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                func.append((x, y))
            elif len(func) == 2:
                # Second click: calculate line and angle
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
                # Third click: calculate width and length
                p1 = np.asarray(func[1])
                p2 = np.asarray((func[2][0], func[2][2]))
                p3 = np.asarray((x, y))
                width = np.linalg.norm(np.cross(p2 - p1, p1 - p3))/np.linalg.norm(p2 - p1)
                length = math.dist(p2, p1)
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                func.append((x, y, int(width), int(length)))
            self.imageCanvas.draw_idle()
    
    def _reprocess_with_inpaint(self):
        """Reprocess current image with inpainting toggled."""
        if self.file_manager.current_image is not None:
            self._on_image_changed(
                self.file_manager.current_image,
                self.file_manager.current_image_name,
                self.file_manager.dir_path
            )

    # ========== End Navigator Signal Handlers ==========

    def launchCalibrationSettings(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or calibration.tif in the folder
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """

        if self.calSettingsDialog is None:
            dir_path = getattr(self.file_manager, 'dir_path', None)
            self.calSettingsDialog = CalibrationSettings(dir_path) if dir_path is None else \
                CalibrationSettings(dir_path, center=self.xrayViewer.orig_image_center)
        self.calSettings = None
        cal_setting = self.calSettingsDialog.calSettings
        if cal_setting is not None or force:
            result = self.calSettingsDialog.exec_()
            if result == 1:
                self.calSettings = self.calSettingsDialog.getValues()
                # Always save calibrated center if available
                # Whether to USE it is controlled by Persistent Center in main GUI
                if 'center' in self.calSettings:
                    self.xrayViewer.info['center'] = self.calSettings['center']
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
        newWindows = LogTraceViewer(self, self.file_manager.current)
        if len(self.windowList) >= 1:
            self.windowList = []
            self.counter = 0
        self.windowList.append(newWindows)

    def send_signal(self):
        if self.windowList != []:
            signal=self.file_manager.current
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
                self.xrayViewer.hist = rotImg[int(newCenter[1]), :]
                self.updateFittingTab(self.xrayViewer.hist)
                self.saveGraphSlice.setEnabled(True)
                self.tabWidget.setTabEnabled(1, True)
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
                l = int(func[3][3]//2)
                nc_x, nc_y = int(newCenter[0]), int(newCenter[1])
                self.xrayViewer.hist = rotImg[nc_y-func[3][2], nc_x-l:nc_x+l]
                for i in range(nc_y-func[3][2], nc_y+func[3][2]):
                    self.xrayViewer.hist += rotImg[i, nc_x-l:nc_x+l]
                self.xrayViewer.hist[self.xrayViewer.hist <= -1] = -1
                self.updateFittingTab(self.xrayViewer.hist)
                self.saveGraphSlice.setEnabled(True)
                self.tabWidget.setTabEnabled(1, True)
            self.refreshAllTabs()
            
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


    def ableToProcess(self):
        """
        Check if image can be processed
        """
        return not self.uiUpdating

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

    def onTabChanged(self, index):
        """
        Handle tab switching (simplified - navControls now stay in navigator)
        """
        # Trigger UI update for the new tab
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
        Update image tab (image display is handled by ImageNavigatorWidget)
        """
        if not self.updated['img'] and self.xrayViewer:
            self.uiUpdating = True
            
            # ImageNavigatorWidget handles display, we just track the zoom state
            ax = self.imageAxes
            self.img_zoom = [ax.get_xlim(), ax.get_ylim()]
            
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
        if not self.file_manager or not self.file_manager.current_image_name:
            return
        
        dir_path = self.file_manager.dir_path or ''
        fileFullPath = fullPath(dir_path, self.file_manager.current_image_name)
        total_count = len(self.file_manager.names) if self.file_manager.names else len(self.file_manager.file_list)
        
        # Build status message
        status_msg = f'Current Image ({self.file_manager.current + 1}/{total_count}) : {fileFullPath}'
        if self.file_manager.current_h5_nframes:
            status_msg += f' ({self.file_manager.current_frame_idx + 1}/{self.file_manager.current_h5_nframes})'
        
        self.imgPathOnStatusBar.setText(status_msg)


    def batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.navControls.processFolderButton.isChecked():
            if not self.progressBar.isVisible():
                self.navControls.processFolderButton.setText("Pause")
                self.navControls.processFolderButton.setChecked(True)
                self.processFolder()
        else:
            self.stop_process = True

    def h5batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.navControls.processH5Button.isChecked():
            if not self.progressBar.isVisible():
                self.navControls.processH5Button.setText("Pause")
                self.navControls.processH5Button.setChecked(True)
                self.processH5File()
        else:
            self.stop_process = True


    def processFolder(self):
        """
        Triggered when a folder has been selected to process it
        """
        idx = self.file_manager.current
        img_ids = list(range(idx, len(self.file_manager.names))) + list(range(0, idx))
        self._process_image_list(img_ids)
        self.navControls.processFolderButton.setChecked(False)
        self.navControls.processFolderButton.setText("Play Current Folder")

    def processH5File(self):
        """
        Triggered when a folder has been selected to process it
        """
        
        start_idx, end_idx = self.file_manager.get_current_h5_range()
        idx = self.file_manager.current
        img_ids = list(range(idx + 1, end_idx + 1)) + list(range(start_idx, idx + 1))
        self._process_image_list(img_ids)
        self.navControls.processH5Button.setChecked(False)
        self.navControls.processH5Button.setText("Play Current H5 File")


    def _process_image_list(self, img_ids):
        self.stop_process = False
        self.progressBar.setVisible(True)
        self.progressBar.setRange(0, len(img_ids))
        
        for progress, img_idx in enumerate(img_ids):
            if self.stop_process:
                break
            self.progressBar.setValue(progress)
            QApplication.processEvents()
            
            # Switch to image by index and load it
            self.file_manager.switch_image_by_index(img_idx)
            img = self.file_manager.current_image
            if img is not None:
                # Manually trigger image processing for batch mode
                self._on_image_changed(img, self.file_manager.current_image_name, self.file_manager.dir_path)
        
        self.progressBar.setVisible(False)



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
