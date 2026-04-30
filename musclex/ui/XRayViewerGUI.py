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
from ..utils.misc_utils import qFromCenter
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.XRayViewer import XRayViewer
from ..csv_manager.XV_CSVManager import XV_CSVManager
from ..CalibrationSettings import CalibrationSettings
from .pyqt_utils import *
from .LogTraceViewer import LogTraceViewer
from .widgets.image_navigator_widget import ImageNavigatorWidget
from .widgets import output_dir_dialog


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
        # Box Intensity Stats popup state (multi-shot ROI tool with per-row
        # Edit/Done buttons). At most one entry is the "editable" one — its
        # button shows "Done"; everyone else shows "Edit".
        self.boxStatsBox = None              # the QDialog shown to the user
        self._boxRowsContainer = None        # QWidget hosting the rows layout
        self._boxRowsLayout = None           # QVBoxLayout of per-box rows (+ tail stretch)
        self._okBoxBtn = None                # OK button (closes popup + tool)
        # idx -> dict(idx, bounds, stats, frozen, row_widget, label, edit_btn)
        self.boxStatsEntries = {}
        # Re-entry guards for tool ↔ GUI sync (see _on_*_deactivated handlers)
        self._box_stats_closing = False
        self._xv_legacy_closing = False

        # Current path-label message override. None means "show current file
        # path from file_manager"; a string overrides it (used to display
        # tool hints like "Draw a rectangle..."). Routed through
        # _setStatusPath() / resetStatusbar() so the elision width is
        # always recomputed against the right-side widgets.
        self._statusPathMessage = None
        
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
        
        self.dir_context = None
        self.csv_manager = None

        # Connect navigator's signals
        self.navigator.fileLoaded.connect(self._on_file_loaded)
        self.navigator.imageChanged.connect(self._on_image_changed)
        
        # Connect ImageViewer's interaction signals (high-level, cleaner than matplotlib events)
        # Display chain: coordinatesChanged (x/y/value) -> status bar update only
        self.navigator.image_viewer.coordinatesChanged.connect(self._on_coordinates_changed)
        # Interaction chain: mouseMoved (raw event) -> tool preview drawing (slice/dist/slice_box)
        self.navigator.image_viewer.mouseMoved.connect(self._on_mouse_moved_for_preview)
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

        # Center checkbox - shown alongside the other display toggles in the
        # display options panel (mirrors the Projection Traces UI).
        self.centerChkBx = QCheckBox("Center")
        self.centerChkBx.setChecked(False)
        self.centerChkBx.setToolTip(
            "Show a marker at the detected (or calibrated) beam center."
        )
        self.navigator.image_viewer.display_panel.add_to_top_slot(self.centerChkBx)

        # Quadrant Folded checkbox - placed directly under the display options panel.
        # Auto-detected from filename/TIFF metadata; user can override.
        self.qfChkBx = QCheckBox("Quadrant Folded?")
        self.qfChkBx.setToolTip(
            "Check if this image is a quadrant-folded image.\n"
            "Folded images are symmetric around the geometric center, so the\n"
            "geometric center is used directly instead of auto-detecting it."
        )
        self.navigator.right_panel.add_widget(self.qfChkBx)

        # Create custom settings group for XRayViewer features
        self.settingsGroup = QGroupBox("Image Processing")
        self.settingsGroup.setStyleSheet("QGroupBox { font-weight: bold; }")
        self.settingsLayout = QGridLayout()
        self.settingsGroup.setLayout(self.settingsLayout)

        self.calibrationButton = QPushButton("Calibration Settings")
        self.calibrationButton.setToolTip("Open calibration settings to set detector parameters\nand calculate q-values from pixel distances.")

        # Unit radio buttons — only visible when calibration is active
        unit_tooltip = (
            "Choose how calibrated distances are displayed in the status bar:\n"
            "  d (nm)  — real-space d-spacing\n"
            "  q (nm\u207b\u00b9)  — reciprocal-space scattering vector"
        )
        self.unitRadioNm = QRadioButton("d (nm)")
        self.unitRadioNm.setChecked(True)
        self.unitRadioNm.setToolTip(unit_tooltip)
        self.unitRadioQ = QRadioButton("q (nm\u207b\u00b9)")
        self.unitRadioQ.setToolTip(unit_tooltip)

        self.unitRadioWidget = QWidget()
        self.unitRadioWidget.setVisible(False)
        unit_radio_layout = QHBoxLayout(self.unitRadioWidget)
        unit_radio_layout.setContentsMargins(0, 0, 0, 0)
        unit_radio_layout.setSpacing(4)
        unit_radio_label = QLabel("Cursor Readout:")
        unit_radio_label.setStyleSheet("font-weight: normal;")
        unit_radio_layout.addWidget(unit_radio_label)
        unit_radio_layout.addWidget(self.unitRadioNm)
        unit_radio_layout.addWidget(self.unitRadioQ)
        unit_radio_layout.addStretch()

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
        self.boxStats = QPushButton("Box Intensity Stats")
        self.boxStats.setCheckable(True)
        self.boxStats.setToolTip(
            "Drag a rectangle on the image to compute pixel intensity\n"
            "statistics (sum, mean, std, min, max, median) inside the box.\n"
            "Drag the corner/edge handles to fine-tune the current box; the\n"
            "popup updates live. Click 'Add Another Box' on the popup to\n"
            "freeze the current box and start a new one. Press ESC or click\n"
            "OK to finish."
        )
        self.saveGraphSlice = QCheckBox("Save Graph Profile")
        self.saveGraphSlice.setEnabled(False)
        self.saveGraphSlice.setToolTip("When checked, the graph profile data will be saved to a CSV file.")
        self.inpaintChkBx = QCheckBox("Inpainting")
        self.inpaintChkBx.setToolTip("Fill in masked/dead pixels using surrounding pixel values.\nUseful for removing detector artifacts.")
        
        self.checkableButtons.extend([self.measureDist, self.setSlice, self.setSliceBox, self.boxStats])
        
        self.settingsLayout.addWidget(self.calibrationButton, 0, 0, 1, 2)
        self.settingsLayout.addWidget(self.unitRadioWidget, 1, 0, 1, 2)
        self.settingsLayout.addWidget(self.openTrace, 2, 0, 1, 2)
        self.settingsLayout.addWidget(self.measureDist, 3, 0, 1, 2)
        self.settingsLayout.addWidget(self.setSlice, 4, 0, 1, 2)
        self.settingsLayout.addWidget(self.setSliceBox, 5, 0, 1, 2)
        self.settingsLayout.addWidget(self.boxStats, 6, 0, 1, 2)
        self.settingsLayout.addWidget(self.saveGraphSlice, 7, 0, 1, 2)
        self.settingsLayout.addWidget(self.inpaintChkBx, 8, 0, 1, 2)
        
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
        # navControls will be added at row 4 dynamically when switching to Graph tab
        self._nav_graph_row = 4

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
        self.imgCoordOnStatusBar.setMinimumWidth(700)  # Fixed width to prevent resize
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
        
        exportViewAction = QAction('Export Current View to PNG...', self)
        exportViewAction.setShortcut('Ctrl+E')
        exportViewAction.triggered.connect(self.exportCurrentViewToPNG)

        exportGraphDataAction = QAction('Export Graph Data to Text File...', self)
        exportGraphDataAction.setShortcut('Ctrl+Shift+E')
        exportGraphDataAction.triggered.connect(self.exportGraphDataToTextFile)

        changeOutputDirAction = QAction('Change Output Directory...', self)
        changeOutputDirAction.triggered.connect(self._change_output_directory)

        menubar = self.menuBar()
        # menubar.setNativeMenuBar(False)
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addAction(exportViewAction)
        fileMenu.addAction(exportGraphDataAction)
        fileMenu.addSeparator()
        fileMenu.addAction(changeOutputDirAction)
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
        # - navigator.image_viewer.coordinatesChanged -> _on_coordinates_changed (status bar)
        # - navigator.image_viewer.mouseMoved -> _on_mouse_moved_for_preview (tool drawing)
        # - navigator.image_viewer.canvasClicked -> _on_canvas_clicked
        # Display options (zoom, intensity, etc.) handled internally by ImageViewer
        # Navigation buttons handled internally by Navigator
        
        ##### Batch processing #####
        self.navControls.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.navControls.processH5Button.toggled.connect(self.h5batchProcBtnToggled)
        
        ##### Interactive tools (registered with the image viewer's ToolManager) #####
        # Two tools live in the ToolManager:
        #   - 'box_stats'   : real BoxStatsTool used by the Box Intensity Stats button
        #   - 'xv_legacy'   : PlaceholderTool occupied while the legacy self.function
        #                     state machine is active (measure dist / set slice / set
        #                     graph box). Mutual exclusion is automatic — activating
        #                     any tool deactivates the previous one, which fires our
        #                     on_deactivated callbacks for clean state sync.
        self._register_tools()

        ##### Custom XRayViewer features #####
        self.calibrationButton.clicked.connect(self.launchCalibrationSettings)
        self.openTrace.clicked.connect(self.openTraceClicked)
        self.measureDist.clicked.connect(self.measureDistChecked)
        self.setSlice.clicked.connect(self.setSliceChecked)
        self.setSliceBox.clicked.connect(self.setSliceBoxChecked)
        self.boxStats.clicked.connect(self._on_box_stats_clicked)
        self.saveGraphSlice.stateChanged.connect(self.saveGraphSliceChecked)
        self.inpaintChkBx.stateChanged.connect(self._reprocess_with_inpaint)
        self.qfChkBx.stateChanged.connect(self._on_qf_toggled)
        self.unitRadioNm.toggled.connect(self._on_unit_changed)
        self.unitRadioQ.toggled.connect(self._on_unit_changed)
        self.centerChkBx.stateChanged.connect(self._draw_center_overlay)

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

    def _register_tools(self):
        """Register XRayViewer-specific interactive tools to the ToolManager.

        See ``setConnections`` docstring for the rationale behind the two
        registrations and how mutual exclusion is achieved automatically.
        """
        from .tools.box_stats_tool import BoxStatsTool
        from .tools.placeholder_tool import PlaceholderTool

        tool_mgr = self.navigator.image_viewer.tool_manager
        tool_mgr.register_tool(
            'box_stats', BoxStatsTool,
            on_box_drawn=self._on_box_drawn,
            on_committed=self._on_box_committed,
            on_deactivated=self._on_box_stats_deactivated,
        )
        tool_mgr.register_tool(
            'xv_legacy', PlaceholderTool,
            on_deactivated=self._on_xv_legacy_deactivated,
        )

    # ========== Navigator Signal Handlers ==========

    def _on_file_loaded(self, dir_path):
        """
        Called when a new folder is loaded. Resolves the output directory.
        """
        ctx = output_dir_dialog.resolve_output_directory(dir_path, parent=self)
        if ctx is None:
            return
        self.dir_context = ctx
        self.csv_manager = None  # reset so it's recreated with the new output dir

    def _change_output_directory(self):
        """Let the user pick a new output directory."""
        from PySide6.QtWidgets import QDialog, QMessageBox
        from .widgets.output_dir_dialog import OutputDirDialog, _store
        from ..utils.directory_context import DirectoryContext

        if not self.dir_context:
            QMessageBox.information(
                self, "No folder loaded",
                "Please load a folder before changing the output directory.")
            return

        input_dir = self.dir_context.input_dir
        dlg = OutputDirDialog(input_dir, self.dir_context.output_dir, parent=self)
        if dlg.exec() != QDialog.Accepted or dlg.chosen_output is None:
            return

        new_output = dlg.chosen_output
        _store.save(input_dir, new_output)
        self.dir_context = DirectoryContext(input_dir=input_dir, output_dir=new_output)
        self.csv_manager = None  # recreated on next image

    def _on_image_changed(self, img, filename, dir_path):
        """
        Called when navigator loads a new image.
        This is the ONLY entry point for image changes.
        """
        # Drop any in-flight interactive tool (Box Intensity Stats, legacy
        # measure/slice operations) before swapping the image. Each tool's
        # _on_deactivate callback handles its own cleanup so the new image
        # starts from a known-clean state.
        if hasattr(self, 'navigator'):
            tm = self.navigator.image_viewer.tool_manager
            if tm.has_active_tool():
                tm.deactivate_current_tool()

        # Initialize CSV manager if needed (write to output dir)
        if not hasattr(self, 'csv_manager') or self.csv_manager is None:
            csv_dir = self.dir_context.output_dir if self.dir_context else dir_path
            self.csv_manager = XV_CSVManager(csv_dir)
        
        # Create XRayViewer (pass img name so it can auto-detect quadrant-folded)
        img_name = os.path.basename(filename) if filename else None
        try:
            self.xrayViewer = XRayViewer(img, img_path=dir_path, img_name=img_name)
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

        # Sync Quadrant Folded checkbox to auto-detected state, then compute center
        self._sync_qf_checkbox()
        self.xrayViewer.findCenter()

        # Always display the (possibly inpainted) image
        self.navigator.image_viewer.display_image(self.xrayViewer.orig_img)
        # Re-add the center marker after display_image's cla() wipes the axes
        self._draw_center_overlay()
        
        # Update status bar with image info
        original_image = self.xrayViewer.orig_img
        self._setStatusDetail(
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
        Pure display handler: updates the status bar with position, value, and
        calibrated q-distance. This is decoupled from tool preview drawing so
        it always fires regardless of DoubleZoom or tool state.
        """
        if not self.ableToProcess() or not self.xrayViewer:
            return
        
        # Display coordinates with calibrated values (unit depends on unitSelector)
        if self.xrayViewer.orig_image_center is not None and self.calSettings and 'scale' in self.calSettings:
            center = self.xrayViewer.orig_image_center
            q_x, q_y, q_R, q_unit = qFromCenter([x, y], center, self.calSettings['scale'])
            r_px = np.sqrt((x - center[0]) ** 2 + (y - center[1]) ** 2)

            show_nm = self.unitRadioNm.isChecked()

            if show_nm:
                def _fmt(q):
                    return f"{1.0 / q:.4f}" if abs(q) > 1e-6 else "\u221e"
                unit_label = "nm"
                self._setStatusCoord(
                    f"x={x:.2f}, y={y:.2f}, r={r_px:.1f} px, value={value:.2f}, "
                    f"dX={_fmt(q_x)} {unit_label}, dY={_fmt(q_y)} {unit_label}, dR={_fmt(q_R)} {unit_label}"
                )
            else:
                self._setStatusCoord(
                    f"x={x:.2f}, y={y:.2f}, r={r_px:.1f} px, value={value:.2f}, "
                    f"qX={q_x:.4f} {q_unit}, qY={q_y:.4f} {q_unit}, qR={q_R:.4f} {q_unit}"
                )
        else:
            self._setStatusCoord(
                f"x={x:.2f}, y={y:.2f}, value={value:.2f}"
            )

    def _on_mouse_moved_for_preview(self, event):
        """
        Handle raw mouse motion for tool preview drawing (slice/dist/slice_box).
        Driven by the mouseMoved signal (raw matplotlib event) so preview lines
        use float coordinates for smooth rendering.
        """
        if not self.ableToProcess() or not self.xrayViewer:
            return
        
        # Validate event is within the image axes
        if event.inaxes != self.imageAxes or event.xdata is None or event.ydata is None:
            return
        
        # Only draw when a custom tool function is active
        if self.function is None:
            return
        
        x = event.xdata
        y = event.ydata
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
                    if ax.patches[i].get_label() != self._CENTER_MARKER_LABEL:
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
                    if ax.patches[i].get_label() != self._CENTER_MARKER_LABEL:
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
            # Remove temporary preview lines drawn by mouse move handler
            if len(func) == 1:
                # No permanent lines yet, remove all preview lines
                for i in range(len(ax.lines)-1,-1,-1):
                    if ax.lines[i].get_label() != "Blue Dot":
                        ax.lines[i].remove()
            elif len(func) == 2:
                # Keep first 2 permanent lines (first X mark), remove preview lines
                if len(ax.lines) > 2:
                    for i in range(len(ax.lines)-1,1,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
            # Draw permanent X mark at click position
            ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            func.append((x, y))
            # Draw the fixed connecting line when a distance segment is completed
            if len(func) >= 3 and len(func) % 2 != 0:
                start_pt = func[-2]
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')
            self.imageCanvas.draw_idle()
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

    def _on_unit_changed(self, _checked):
        """The unit radio changed — the status bar will update on the next mouse move."""
        pass  # _on_coordinates_changed reads unitRadioNm.isChecked() live

    def _on_qf_toggled(self, _state):
        """Handle Quadrant Folded checkbox toggle.

        Updates the XRayViewer's folded state and recomputes the center so the
        d-spacing display in the status bar reflects the new center immediately.
        """
        if self.xrayViewer is None:
            return
        is_checked = self.qfChkBx.isChecked()
        self.xrayViewer.set_folded(is_checked)
        # Recompute center now (uses geometric center if folded, else getCenter)
        self.xrayViewer.findCenter()
        # If calibration is active, also override the calibration-supplied center
        # so the status bar d-spacing uses the same point.
        if self.calSettings is not None and is_checked:
            # Folded image: prefer geometric center over any stale calibration center
            self.calSettings.pop('center', None)
        # Refresh the on-image center marker (if shown) so it reflects the new center
        self._draw_center_overlay()

    def _sync_qf_checkbox(self):
        """Sync the QF checkbox to xrayViewer.is_folded without triggering the toggle handler."""
        if self.xrayViewer is None:
            return
        self.qfChkBx.blockSignals(True)
        self.qfChkBx.setChecked(self.xrayViewer.is_folded)
        self.qfChkBx.blockSignals(False)

    # Label used to identify our center-marker patch so other tool drawings
    # (slice/dist preview) can leave it untouched when they clear their own
    # patches off the image axes.
    _CENTER_MARKER_LABEL = "XV Center"

    def _draw_center_overlay(self):
        """Show or hide a marker at the beam center on the image axes.

        Mirrors the behavior of ProjectionTraces' Center checkbox: a small
        green disk drawn at ``xrayViewer.orig_image_center``. Idempotent and
        safe to call any time the image is redrawn.
        """
        if self.xrayViewer is None or self.imageAxes is None:
            return
        ax = self.imageAxes
        for patch in list(ax.patches):
            if patch.get_label() == self._CENTER_MARKER_LABEL:
                patch.remove()
        if self.centerChkBx.isChecked() and self.xrayViewer.orig_image_center is not None:
            circle = plt.Circle(
                self.xrayViewer.orig_image_center, 10, color='g',
                label=self._CENTER_MARKER_LABEL,
            )
            ax.add_patch(circle)
        self.imageCanvas.draw_idle()

    # ========== End Navigator Signal Handlers ==========

    def launchCalibrationSettings(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or calibration.tif in the folder
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """

        if self.calSettingsDialog is None:
            from ..utils.settings_manager import SettingsManager
            dir_path = getattr(self.file_manager, 'dir_path', None)
            sm = SettingsManager(dir_path) if dir_path else SettingsManager()
            self.calSettingsDialog = CalibrationSettings(
                dir_path,
                center=self.xrayViewer.orig_image_center,
                settings_manager=sm,
            )
        self.calSettings = None
        cal_setting = self.calSettingsDialog.calSettings
        if cal_setting is not None or force:
            result = self.calSettingsDialog.exec_()
            if result == 1:
                self.calSettings = self.calSettingsDialog.getValues()
                # 'scale' is only present after a real calibration fit (image mode)
                # or when param mode fills it in.  'silverB' alone (without 'scale')
                # means the image-mode checkbox is on but the image was unset.
                cal_active = bool(self.calSettings and 'scale' in self.calSettings)
                self.unitRadioWidget.setVisible(cal_active)
                if not cal_active:
                    return False
                # Always save calibrated center if available
                # Whether to USE it is controlled by Persistent Center in main GUI
                if 'center' in self.calSettings:
                    self.xrayViewer.orig_image_center = self.calSettings['center']
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
            self._setStatusCoord("")
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
            self._setStatusCoord("")
            return

        x = event.xdata
        y = event.ydata
        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self._setStatusCoord("")
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
            self._setStatusCoord("x=" + str(round(x,2)) + ', y=' + str(round(y,2)))

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
        self._setStatusCoord("")

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
            self._setStatusPath(
                "Draw a rectangle on the image to zoom in (ESC to cancel)")
            self.function = ['g_zoomin']
        else:
            self.function = None
            self._setStatusPath(None)


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
            if ax.patches[i].get_label() != self._CENTER_MARKER_LABEL:
                ax.patches[i].remove()
            self.imageCanvas.draw_idle()
        tool_mgr = self.navigator.image_viewer.tool_manager
        if self.measureDist.isChecked():
            self._setStatusPath(
                "Draw a line on the image to measure a distance (ESC to cancel)")
            self.function = ["dist"]  # set current active function
            # Occupy the ToolManager via the placeholder so any other tool
            # (Box Intensity Stats, zoom, etc.) auto-cancels this operation
            # via _on_xv_legacy_deactivated.
            tool_mgr.activate_tool('xv_legacy')
        else:
            tool_mgr.deactivate_tool('xv_legacy')
            self._setStatusPath(None)
            self.refreshAllTabs()

    def updateMeasureDistBoxOkClicked(self):
        self.measureDist.setChecked(False)
        self.measureDist2.setChecked(False)
        # Drop the placeholder so the ToolManager slot frees up.
        self.navigator.image_viewer.tool_manager.deactivate_tool('xv_legacy')
        ax = self.imageAxes
        for i in range(len(ax.lines)-1, -1, -1):
            ax.lines[i].remove()
        for i in range(len(ax.patches)-1, -1, -1):
            if ax.patches[i].get_label() != self._CENTER_MARKER_LABEL:
                ax.patches[i].remove()
        for i in range(len(ax.artists)-1, -1, -1):
            ax.artists[i].remove()
        for i in range(len(ax.texts)-1, -1, -1):
            ax.texts[i].remove()
        self.imageCanvas.draw_idle()
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
        tool_mgr = self.navigator.image_viewer.tool_manager
        if self.measureDist2.isChecked():
            self._setStatusPath(
                "Draw a line on the image to measure a distance (ESC to cancel)")
            self.function = ["dist"]  # set current active function
            tool_mgr.activate_tool('xv_legacy')
        else:
            self._setStatusPath(None)
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
            if ax.patches[i].get_label() != self._CENTER_MARKER_LABEL:
                ax.patches[i].remove()
            self.imageCanvas.draw_idle()
        tool_mgr = self.navigator.image_viewer.tool_manager
        if self.setSlice.isChecked():
            self._setStatusPath(
                "Draw a line on the image to choose a slice (ESC to cancel)")
            self.function = ["slice"]  # set current active function
            self.first_slice = True
            tool_mgr.activate_tool('xv_legacy')
        else:
            tool_mgr.deactivate_tool('xv_legacy')
            self._setStatusPath(None)
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
            if ax.patches[i].get_label() != self._CENTER_MARKER_LABEL:
                ax.patches[i].remove()
            self.imageCanvas.draw_idle()
        tool_mgr = self.navigator.image_viewer.tool_manager
        if self.setSliceBox.isChecked():
            self._setStatusPath(
                "Draw a line on the image then select width (ESC to cancel)")
            self.function = ["slice_box"]  # set current active function
            self.first_box = True
            tool_mgr.activate_tool('xv_legacy')
        else:
            tool_mgr.deactivate_tool('xv_legacy')
            self._setStatusPath(None)
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

    # ============== Box Intensity Stats (multi-shot ROI tool) ==============
    #
    # Architecture (mirrors ProcessingWorkspace's tool registration pattern):
    #
    #   button toggle ──► tool_manager.activate_tool('box_stats')
    #          │
    #          ▼
    #   user drags / resizes the rectangle on the image
    #          │
    #          ▼
    #   BoxStatsTool fires on_box_drawn(bounds, idx)
    #          │
    #          ▼
    #   _on_box_drawn shows / refreshes the popup. The drawn entry's row
    #   is the only one with its button reading "Done"; everyone else is
    #   "Edit".
    #          │
    #          ▼
    #   user clicks "Done" on the editable row    →   commit_current() freezes
    #   user clicks "Edit" on a frozen row        →   start_editing() (auto-
    #                                                 commits any current
    #                                                 first, then re-opens
    #                                                 that idx for editing)
    #          │
    #          ▼
    #   user clicks OK / presses ESC / closes the dialog / activates another tool
    #          │
    #          ▼
    #   tool deactivates → on_deactivated → _on_box_stats_deactivated cleans
    #   up the popup, button state, and any leftover artists. OK is a pure
    #   "abandon and exit" — it never implicitly commits anything.

    def _on_box_stats_clicked(self, checked):
        """Toggle handler for the Box Intensity Stats button."""
        if self.xrayViewer is None or self.xrayViewer.orig_img is None:
            self.boxStats.setChecked(False)
            return
        tool_mgr = self.navigator.image_viewer.tool_manager
        if checked:
            self._setStatusPath(
                "Drag a rectangle on the image to compute box statistics. "
                "Drag handles to fine-tune. Click 'Done' on a row to freeze "
                "it; click 'Edit' on a frozen row to bring it back. ESC or "
                "OK to finish.")
            self.boxStatsEntries = {}
            tool_mgr.activate_tool('box_stats')
        else:
            tool_mgr.deactivate_tool('box_stats')
            self._setStatusPath(None)

    # ----- Tool callbacks ---------------------------------------------------

    def _on_box_drawn(self, bounds, idx):
        """
        BoxStatsTool callback: the editable rect was drawn or resized.

        ``idx`` is the index this box should appear under in the popup. For
        a brand new draw it's ``frozen_count + 1`` and a row is created; for
        a re-edit (after start_editing) it's the original idx and we update
        the existing row in-place. In either case, this row becomes "the
        editable one" — its button reads "Done" — and any other row that was
        showing "Done" is forced back to "Edit"/frozen.
        """
        stats = self._computeBoxStats(bounds)
        if stats is None:
            return
        self._ensure_box_stats_popup()

        # Invariant: at most one entry is editable. Force any stragglers to
        # frozen. This is mostly defensive: re-edits route through the tool's
        # start_editing → commit_current → on_committed first, so that path
        # has already flipped any previous editable.
        for other_idx, other in self.boxStatsEntries.items():
            if other_idx != idx and not other['frozen']:
                other['frozen'] = True
                if other['edit_btn'] is not None:
                    other['edit_btn'].setText("Edit")

        if idx in self.boxStatsEntries:
            entry = self.boxStatsEntries[idx]
            entry['bounds'] = dict(bounds)
            entry['stats'] = stats
            entry['frozen'] = False
            if entry['edit_btn'] is not None:
                entry['edit_btn'].setText("Done")
            if entry['label'] is not None:
                entry['label'].setText(self._format_stats_text(idx, stats))
        else:
            entry = {
                'idx': idx,
                'bounds': dict(bounds),
                'stats': stats,
                'frozen': False,
                'row_widget': None,
                'label': None,
                'edit_btn': None,
            }
            self.boxStatsEntries[idx] = entry
            self._add_box_row(entry)

        self._setStatusReport(
            f"Box {idx} (editing) — sum={stats['sum']:.4g}, "
            f"mean={stats['mean']:.4g}, std={stats['std']:.4g}, n={stats['n']}")

    def _on_box_committed(self, idx, bounds):
        """
        BoxStatsTool callback: a box was frozen via commit_current().

        Flip the row's button back to "Edit" and refresh its stats with the
        final committed bounds. The visual rectangle on the image was already
        drawn by the tool itself.
        """
        stats = self._computeBoxStats(bounds)
        if idx not in self.boxStatsEntries:
            # Shouldn't happen — _on_box_drawn always fires before commit —
            # but if it does, create the row so we don't lose the stats.
            if stats is None:
                return
            self.boxStatsEntries[idx] = {
                'idx': idx, 'bounds': dict(bounds), 'stats': stats,
                'frozen': True,
                'row_widget': None, 'label': None, 'edit_btn': None,
            }
            self._ensure_box_stats_popup()
            self._add_box_row(self.boxStatsEntries[idx])
            return

        entry = self.boxStatsEntries[idx]
        entry['bounds'] = dict(bounds)
        entry['frozen'] = True
        if entry['edit_btn'] is not None:
            entry['edit_btn'].setText("Edit")
        if stats is not None:
            entry['stats'] = stats
            if entry['label'] is not None:
                entry['label'].setText(self._format_stats_text(idx, stats))

    def _on_edit_clicked(self, idx):
        """
        Per-row Edit/Done button handler.

        - If the row is currently frozen (button = "Edit"): tell the tool to
          start editing this idx. The tool auto-commits any other in-progress
          box first; the resulting on_committed + on_box_drawn callbacks
          then update the row buttons.
        - If the row is currently editable (button = "Done"): commit it.
        """
        entry = self.boxStatsEntries.get(idx)
        if entry is None:
            return
        tool = self.navigator.image_viewer.tool_manager.tools.get('box_stats')
        if tool is None:
            return
        if entry['frozen']:
            tool.start_editing(entry['bounds'], idx)
        else:
            tool.commit_current()

    def _on_box_stats_deactivated(self):
        """
        Called whenever 'box_stats' is deactivated, regardless of who
        triggered it (user uncheck, ESC, OK button, ToolManager auto-pull
        because another tool was activated). Synchronises GUI state.
        """
        if self._box_stats_closing:
            return
        self._box_stats_closing = True
        try:
            self._closeBoxStatsPopup()
            if self.boxStats.isChecked():
                # Block the clicked signal so we don't re-enter deactivate_tool().
                self.boxStats.blockSignals(True)
                self.boxStats.setChecked(False)
                self.boxStats.blockSignals(False)
            self.resetStatusbar()
        finally:
            self._box_stats_closing = False

    # ----- Popup helpers ----------------------------------------------------

    def _ensure_box_stats_popup(self):
        """Lazily create the non-modal popup on first stats result.

        We use a plain QDialog (instead of QMessageBox) because QMessageBox's
        internal button slot calls ``done()`` for *every* added button
        regardless of role. With a raw QDialog we have full control: the body
        is a QScrollArea of per-box rows (each row is QLabel + Edit/Done
        QPushButton), so users can edit any box at any time without losing
        the others.
        """
        if self.boxStatsBox is not None:
            return

        self.boxStatsBox = QDialog(self)
        self.boxStatsBox.setWindowTitle("Box Intensity Statistics")
        self.boxStatsBox.setModal(False)
        # Don't let the dialog auto-delete on close — we manage its lifetime
        # explicitly so we can reuse it within a single tool session.
        self.boxStatsBox.setAttribute(Qt.WA_DeleteOnClose, False)

        layout = QVBoxLayout(self.boxStatsBox)
        layout.setContentsMargins(12, 12, 12, 12)
        layout.setSpacing(8)

        header = QLabel(
            "Drag handles on the image to adjust the editable box (live "
            "update).\nClick 'Done' to freeze a box, 'Edit' to bring a "
            "frozen one back. OK exits without saving.")
        layout.addWidget(header)

        # Scrollable container for per-box rows. We use QScrollArea so the
        # dialog stays a sensible size when the user accumulates many boxes.
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setMinimumWidth(760)
        scroll.setMinimumHeight(240)

        self._boxRowsContainer = QWidget()
        self._boxRowsLayout = QVBoxLayout(self._boxRowsContainer)
        self._boxRowsLayout.setContentsMargins(4, 4, 4, 4)
        self._boxRowsLayout.setSpacing(6)
        # Tail stretch keeps rows packed at the top.
        self._boxRowsLayout.addStretch(1)
        scroll.setWidget(self._boxRowsContainer)
        layout.addWidget(scroll, 1)

        btn_row = QHBoxLayout()
        btn_row.addStretch(1)
        self._okBoxBtn = QPushButton("OK")
        self._okBoxBtn.setDefault(True)
        self._okBoxBtn.setToolTip(
            "Close the popup and exit the Box Intensity Stats tool. Boxes "
            "are removed from the image. Use 'Done' on each row to freeze "
            "a box before clicking OK if you want to copy its stats.")
        btn_row.addWidget(self._okBoxBtn)
        layout.addLayout(btn_row)

        self._okBoxBtn.clicked.connect(self._onBoxStatsOkClicked)
        # X-button / Escape inside the dialog → same as OK.
        self.boxStatsBox.finished.connect(self._on_box_stats_dialog_finished)

        self.boxStatsBox.show()

    def _add_box_row(self, entry):
        """Append a row widget for ``entry`` to the popup."""
        if self._boxRowsLayout is None:
            return
        row_widget = QWidget()
        row = QHBoxLayout(row_widget)
        row.setContentsMargins(0, 0, 0, 0)
        row.setSpacing(8)

        label = QLabel(self._format_stats_text(entry['idx'], entry['stats']))
        label.setStyleSheet("QLabel { font-family: monospace; }")
        label.setTextInteractionFlags(Qt.TextSelectableByMouse)
        label.setWordWrap(False)
        label.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)
        row.addWidget(label, 1)

        btn = QPushButton("Done" if not entry['frozen'] else "Edit")
        btn.setFixedWidth(80)
        btn.setToolTip(
            "Click 'Done' to freeze this box, or 'Edit' to bring a frozen "
            "box back to live editing (any in-progress box is committed "
            "first).")
        idx = entry['idx']
        btn.clicked.connect(lambda _checked=False, i=idx: self._on_edit_clicked(i))
        row.addWidget(btn, 0, Qt.AlignVCenter)

        # Insert just before the trailing stretch so rows stay packed top.
        insert_idx = max(0, self._boxRowsLayout.count() - 1)
        self._boxRowsLayout.insertWidget(insert_idx, row_widget)

        entry['row_widget'] = row_widget
        entry['label'] = label
        entry['edit_btn'] = btn

    def _onBoxStatsOkClicked(self):
        """
        User pressed OK. Pure exit — abandon any editable + frozen boxes,
        clean up artists, and shut the popup down.

        We deactivate the tool directly via ToolManager rather than going
        through ``self.boxStats.setChecked(False)``: ``setChecked`` only
        emits ``toggled``, not ``clicked``, so it would not trigger
        ``_on_box_stats_clicked`` (which is the slot connected to
        ``clicked``). Going through the manager fires the tool's own
        ``_on_deactivate`` (removes axes artists + selector), which then
        invokes our ``_on_box_stats_deactivated`` callback to close the
        popup and sync the button.
        """
        self.navigator.image_viewer.tool_manager.deactivate_tool('box_stats')

    def _on_box_stats_dialog_finished(self, _result):
        """Dialog was closed by the user (X button / Esc inside the dialog).

        Treated identically to OK: full tool teardown.
        """
        if self._box_stats_closing:
            return
        self.navigator.image_viewer.tool_manager.deactivate_tool('box_stats')

    def _closeBoxStatsPopup(self):
        """Tear down the popup + accumulated state."""
        if self.boxStatsBox is not None:
            try:
                if self._okBoxBtn is not None:
                    self._okBoxBtn.clicked.disconnect(self._onBoxStatsOkClicked)
            except (TypeError, RuntimeError):
                pass
            try:
                self.boxStatsBox.finished.disconnect(self._on_box_stats_dialog_finished)
            except (TypeError, RuntimeError):
                pass
            self.boxStatsBox.close()
            self.boxStatsBox.deleteLater()
            self.boxStatsBox = None
            self._boxRowsContainer = None
            self._boxRowsLayout = None
            self._okBoxBtn = None
        # Drop entry refs (Qt parent ownership cleans up the widgets).
        self.boxStatsEntries = {}

    # ----- Stats computation -----------------------------------------------

    def _computeBoxStats(self, bounds):
        """
        Compute pixel intensity statistics inside an axis-aligned ROI.

        Args:
            bounds: dict with 'x0', 'y0', 'x1', 'y1' in image coordinates.

        Returns:
            dict with x_min/x_max/y_min/y_max (clamped, integer pixel
            indices), n (valid pixel count), sum, mean, std, min, max,
            median — or None if the ROI is empty / fully masked.
        """
        if (self.xrayViewer is None
                or self.xrayViewer.orig_img is None
                or not bounds):
            return None
        img = self.xrayViewer.orig_img
        H, W = img.shape[:2]
        x_min = int(max(0, math.floor(bounds['x0'])))
        x_max = int(min(W, math.ceil(bounds['x1'])))
        y_min = int(max(0, math.floor(bounds['y0'])))
        y_max = int(min(H, math.ceil(bounds['y1'])))
        if x_max <= x_min or y_max <= y_min:
            return None

        roi = img[y_min:y_max, x_min:x_max].astype(np.float64)
        # MuscleX uses -1 as a mask sentinel for dead/blank pixels (see e.g.
        # setSliceBoxChecked clamping `hist[hist <= -1] = -1`). Exclude them.
        valid = roi > -1
        if not valid.any():
            return None
        vals = roi[valid]
        return dict(
            x_min=x_min, x_max=x_max, y_min=y_min, y_max=y_max,
            n=int(valid.sum()),
            sum=float(vals.sum()),
            mean=float(vals.mean()),
            std=float(vals.std(ddof=0)),
            min=float(vals.min()),
            max=float(vals.max()),
            median=float(np.median(vals)),
        )

    def _format_stats_text(self, idx, s):
        """Format one entry's label text. Two lines so the row stays compact
        but still shows all the stats. The Edit/Done button on the row tells
        the user which box is currently editable."""
        return (
            f"Box {idx}: x=[{s['x_min']}, {s['x_max']}), "
            f"y=[{s['y_min']}, {s['y_max']})  "
            f"({s['x_max'] - s['x_min']}\u00d7{s['y_max'] - s['y_min']} px, "
            f"n={s['n']})\n"
            f"       sum={s['sum']:.6g}, mean={s['mean']:.6g}, std={s['std']:.6g},\n"
            f"       min={s['min']:.6g}, max={s['max']:.6g}, median={s['median']:.6g}"
        )

    # ============== Legacy 'xv_legacy' placeholder cleanup =================

    def _on_xv_legacy_deactivated(self):
        """
        Triggered whenever the 'xv_legacy' placeholder is deactivated. This
        fires for two reasons:

        1. We deactivated it ourselves at the end of a legacy function
           (Done / cancel / ESC). In that case the buttons + ``self.function``
           have already been cleared, so this becomes a no-op.
        2. ToolManager auto-deactivated it because another tool was just
           activated (e.g. Box Intensity Stats). In that case we need to
           clear ``self.function``, uncheck whichever legacy button is
           currently checked, and wipe any in-progress preview overlays so
           the new tool starts from a clean slate.

        The ``_xv_legacy_closing`` guard prevents recursion through the
        button-toggle → ``*Checked(False)`` → ``deactivate_tool`` chain.
        """
        if self._xv_legacy_closing:
            return
        self._xv_legacy_closing = True
        try:
            self.function = None
            for b in (self.measureDist, self.setSlice, self.setSliceBox):
                if b.isChecked():
                    b.blockSignals(True)
                    b.setChecked(False)
                    b.blockSignals(False)
            ax = self.imageAxes
            for i in range(len(ax.lines) - 1, -1, -1):
                ax.lines[i].remove()
            for i in range(len(ax.patches) - 1, -1, -1):
                if ax.patches[i].get_label() != self._CENTER_MARKER_LABEL:
                    ax.patches[i].remove()
            self.imageCanvas.draw_idle()
            self.resetStatusbar()
        finally:
            self._xv_legacy_closing = False

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
        Handle tab switching.

        Moves the shared navigation widget (navControls) between the navigator's
        right panel (Image tab) and the bottom of the Graph tab options frame
        (Graph tab) so the user can keep navigating frames while inspecting the
        graph profile.
        """
        if index == 1:
            self._move_nav_to_graph_tab()
        else:
            self._move_nav_to_image_tab()

        # Trigger UI update for the new tab
        self.updateUI()

    def _move_nav_to_graph_tab(self):
        """Reparent the navigation widget into the Graph tab's bottom area."""
        if self.navControls.parentWidget() is self.fittingOptionsFrame2:
            return  # already there
        # Detach from the navigator's right panel
        self.navigator.right_panel.bottom_layout.removeWidget(self.navControls)
        # addWidget reparents to the layout's owner widget (fittingOptionsFrame2)
        self.bottomLayout2.addWidget(self.navControls, self._nav_graph_row, 0, 1, 2)
        self.navControls.show()

    def _move_nav_to_image_tab(self):
        """Reparent the navigation widget back to the navigator's right panel."""
        if self.navControls.parentWidget() is self.navigator.right_panel.bottom_widget:
            return  # already there
        # Detach from the Graph tab bottom grid
        self.bottomLayout2.removeWidget(self.navControls)
        # add_bottom_widget appends to the right_panel's bottom QVBoxLayout
        self.navigator.right_panel.add_bottom_widget(self.navControls)
        self.navControls.show()

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

    def _setStatusPath(self, msg=None):
        """
        Set the path-label content on the status bar.

        Pass a string to display a temporary message (e.g. tool hint
        like "Draw a rectangle..."). Pass None to restore the
        current-file-path display built from file_manager state.

        Always re-runs the elision so the message fits in the space not
        occupied by the right-side widgets.
        """
        self._statusPathMessage = msg
        self.resetStatusbar()

    def _setStatusCoord(self, text):
        """Update the coordinate label and re-elide the path."""
        self.imgCoordOnStatusBar.setText(text)
        self.resetStatusbar()

    def _setStatusReport(self, text):
        """Update the status-report label and re-elide the path."""
        self.statusReport.setText(text)
        self.resetStatusbar()

    def _setStatusDetail(self, text):
        """Update the image-detail label and re-elide the path."""
        self.imgDetailOnStatusBar.setText(text)
        self.resetStatusbar()

    def resetStatusbar(self):
        """
        Re-render the status bar's path label.

        The path label on the left is elided so it always fits in the
        space NOT occupied by the right-side widgets (statusReport,
        imgCoordOnStatusBar, imgDetailOnStatusBar, progressBar).
        The available width is computed dynamically from the actual
        size hints of the visible right-side widgets, so the path
        adapts as the coordinate text grows / the progress bar shows.

        If `_statusPathMessage` is set (via `_setStatusPath`) it is
        shown verbatim (still elided to fit); otherwise the current
        file path from `file_manager` is rendered.
        """
        # Determine the full message to render.
        if self._statusPathMessage is not None:
            status_msg = self._statusPathMessage
        elif self.file_manager and getattr(self.file_manager, 'current_image_name', None):
            dir_path = self.file_manager.dir_path or ''
            fileFullPath = fullPath(dir_path, self.file_manager.current_image_name)
            total_count = (len(self.file_manager.names)
                           if self.file_manager.names
                           else len(self.file_manager.file_list))

            status_msg = f'Current Image ({self.file_manager.current + 1}/{total_count}) : {fileFullPath}'
            if self.file_manager.current_h5_nframes:
                status_msg += (f' ({self.file_manager.current_frame_idx + 1}/'
                               f'{self.file_manager.current_h5_nframes})')
        else:
            return

        self.imgPathOnStatusBar.setToolTip(status_msg)

        # Compute the actual width occupied by the right-side widgets.
        right_widgets = (
            self.statusReport,
            self.imgCoordOnStatusBar,
            self.imgDetailOnStatusBar,
            self.progressBar,
        )
        # sizeHint() reflects the current text content; isVisible() handles
        # the progress bar which is hidden outside batch mode.
        right_w = sum(
            max(w.sizeHint().width(), w.minimumWidth())
            for w in right_widgets if w.isVisible()
        )
        # Account for inter-widget spacing + margins of the status bar.
        spacing = self.statusBar.layout().spacing() if self.statusBar.layout() else 6
        margins = self.statusBar.contentsMargins()
        padding = spacing * (len(right_widgets) + 1) + margins.left() + margins.right()

        max_width = max(200, self.statusBar.width() - right_w - padding)

        metrics = self.imgPathOnStatusBar.fontMetrics()
        elided = metrics.elidedText(status_msg, Qt.ElideMiddle, max_width)
        self.imgPathOnStatusBar.setText(elided)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        self.resetStatusbar()

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
        # Return to the starting image after playback
        self._navigate_to(idx)
        self.navControls.processFolderButton.setChecked(False)
        self.navControls.processFolderButton.setText("Play Current Folder")

    def processH5File(self):
        """
        Triggered when a folder has been selected to process it
        """
        start_idx, end_idx = self.file_manager.get_current_h5_range()
        if start_idx is None or end_idx is None:
            return
        idx = self.file_manager.current
        img_ids = list(range(idx + 1, end_idx + 1)) + list(range(start_idx, idx + 1))
        self._process_image_list(img_ids)
        # Return to the starting image after playback
        self._navigate_to(idx)
        self.navControls.processH5Button.setChecked(False)
        self.navControls.processH5Button.setText("Play Current H5 File")


    def _navigate_to(self, idx):
        """Switch to image at idx and display it."""
        self.file_manager.switch_image_by_index(idx)
        img = self.file_manager.current_image
        if img is not None:
            self._on_image_changed(img, self.file_manager.current_image_name, self.file_manager.dir_path)

    def _process_image_list(self, img_ids):
        self.stop_process = False
        self.progressBar.setVisible(True)
        self.progressBar.setRange(0, len(img_ids))
        self.resetStatusbar()

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
        self.resetStatusbar()



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

    def exportCurrentViewToPNG(self):
        """
        Export the current view to a PNG file (image only, without overlays).
        Opens a file dialog for the user to choose the save location.
        """
        if self.xrayViewer is None or self.xrayViewer.orig_img is None:
            infMsg = QMessageBox()
            infMsg.setText("No Image Loaded")
            infMsg.setInformativeText("Please load an image before exporting.")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            return
        
        # Open file dialog to get save path
        from PySide6.QtWidgets import QFileDialog
        import os
        
        # Get default filename without extension
        default_name = ""
        if self.file_manager.current_image_name:
            # Remove extension from current image name
            name_without_ext = os.path.splitext(self.file_manager.current_image_name)[0]
            default_name = name_without_ext + ".png"
        
        save_path, _ = QFileDialog.getSaveFileName(
            self,
            "Export Current View to PNG",
            default_name,
            "PNG Files (*.png)"
        )
        
        if not save_path:
            # User cancelled
            return
        
        # Ensure the file has .png extension
        if not save_path.lower().endswith('.png'):
            save_path += '.png'
        
        try:
            # Get the current image and zoom bounds
            img = self.xrayViewer.orig_img
            xlim = self.imageAxes.get_xlim()
            ylim = self.imageAxes.get_ylim()
            
            # Convert zoom bounds to integer pixel coordinates
            x_min = max(0, int(round(xlim[0])))
            x_max = min(img.shape[1], int(round(xlim[1])))
            y_min = max(0, int(round(min(ylim))))
            y_max = min(img.shape[0], int(round(max(ylim))))
            
            # Crop the image to the current view
            cropped_img = img[y_min:y_max, x_min:x_max]
            
            # Get current display settings from the navigator's image viewer
            display_options = self.navigator.image_viewer.get_display_options()
            vmin = display_options['vmin']
            vmax = display_options['vmax']
            log_scale = display_options['log_scale']
            colormap = display_options['colormap']
            
            # Create an off-screen figure (bypass pyplot/Qt to avoid window resize side effects)
            from matplotlib.figure import Figure
            from matplotlib.backends.backend_agg import FigureCanvasAgg
            temp_fig = Figure(figsize=(10, 10))
            FigureCanvasAgg(temp_fig)
            temp_ax = temp_fig.add_subplot(111)
            
            # Display the cropped image with current settings
            if log_scale:
                temp_ax.imshow(cropped_img, cmap=colormap, 
                             norm=LogNorm(vmin=max(1, vmin), vmax=vmax))
            else:
                temp_ax.imshow(cropped_img, cmap=colormap,
                             norm=Normalize(vmin=vmin, vmax=vmax))
            
            # Remove axes for clean export
            temp_ax.axis('off')
            
            # Save with tight layout
            temp_fig.savefig(save_path, dpi=150, bbox_inches='tight', 
                           pad_inches=0, facecolor='black')
            
            # Show success dialog
            infMsg = QMessageBox()
            infMsg.setText("Export Successful")
            infMsg.setInformativeText(f"Current view has been saved to:\n{save_path}")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            
        except Exception as e:
            # Show error message
            errMsg = QMessageBox()
            errMsg.setText("Export Failed")
            errMsg.setInformativeText(f"Failed to export image:\n{str(e)}")
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Critical)
            errMsg.exec_()

    def exportGraphDataToTextFile(self):
        """
        Export the current graph profile to a plain ASCII text file with
        two columns: pixel index (x) and intensity (y). The x values match
        the indices used to plot the curve in the Graph tab.
        """
        hist = getattr(self.xrayViewer, 'hist', None) if self.xrayViewer is not None else None
        if hist is None or len(hist) == 0:
            infMsg = QMessageBox()
            infMsg.setText("No Graph Data")
            infMsg.setInformativeText(
                "There is no graph profile to export.\n"
                "Use 'Set Graph Slice' or 'Set Graph Box' on the image first."
            )
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()
            return

        import os

        default_name = ""
        if self.file_manager and self.file_manager.current_image_name:
            name_without_ext = os.path.splitext(self.file_manager.current_image_name)[0]
            default_name = name_without_ext + "_profile.txt"

        save_path, _ = QFileDialog.getSaveFileName(
            self,
            "Export Graph Data to Text File",
            default_name,
            "Text Files (*.txt);;Data Files (*.dat);;All Files (*)"
        )

        if not save_path:
            return

        if not any(save_path.lower().endswith(ext) for ext in ('.txt', '.dat', '.csv')):
            save_path += '.txt'

        try:
            hist_arr = np.asarray(hist)
            x = np.arange(len(hist_arr))

            mode = ""
            if self.saved_slice and len(self.saved_slice) > 0:
                if self.saved_slice[0] == "slice":
                    mode = "Set Graph Slice"
                elif self.saved_slice[0] == "slice_box":
                    mode = "Set Graph Box"

            header_lines = [
                "X-Ray Viewer graph profile export",
                f"MuscleX version: {__version__}",
            ]
            if self.file_manager and self.file_manager.current_image_name:
                header_lines.append(f"Source image: {self.file_manager.current_image_name}")
            if mode:
                header_lines.append(f"Mode: {mode}")
            header_lines.append(f"Number of points: {len(hist_arr)}")
            header_lines.append("Columns: pixel_index intensity")
            header = "\n".join(header_lines)

            np.savetxt(
                save_path,
                np.column_stack([x, hist_arr]),
                fmt=("%d", "%.6g"),
                header=header
            )

            infMsg = QMessageBox()
            infMsg.setText("Export Successful")
            infMsg.setInformativeText(f"Graph data has been saved to:\n{save_path}")
            infMsg.setStandardButtons(QMessageBox.Ok)
            infMsg.setIcon(QMessageBox.Information)
            infMsg.exec_()

        except Exception as e:
            errMsg = QMessageBox()
            errMsg.setText("Export Failed")
            errMsg.setInformativeText(f"Failed to export graph data:\n{str(e)}")
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Critical)
            errMsg.exec_()

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        self._setStatusReport(text)
        QApplication.processEvents()
