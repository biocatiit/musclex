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
import time
import sys
import copy
import os
from os.path import split
import shutil
import json
import math
import traceback
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
from skimage.feature import peak_local_max
from musclex import __version__
from .pyqt_utils import *
from ..CalibrationSettings import CalibrationSettings
from ..utils.file_manager import FileManager, fullPath, getImgFiles
from ..utils import logger
from ..utils.image_processor import *
from ..modules.EquatorImage import EquatorImage, getCardiacGraph
# from ..modules.QuadrantFolder import QuadrantFolder
from ..csv_manager import EQ_CSVManager
from ..ui.EQ_FittingTab import EQ_FittingTab
from .BlankImageSettings import BlankImageSettings
from .ImageMaskTool import ImageMaskerWindow
from .DoubleZoomGUI import DoubleZoom
from .widgets.navigation_controls import NavigationControls
from .widgets import ProcessingWorkspace
from ..utils.image_data import ImageData
from skimage.morphology import binary_dilation
from PySide6.QtCore import QRunnable, QThreadPool, QEventLoop, Signal, QTimer
from queue import Queue


class WorkerSignals(QObject):
    
    finished = Signal(object)
    error = Signal(tuple)
    result = Signal(object)


class Worker(QRunnable):

    def __init__(self, bioImg, settings, paramInfo):
        """
        Worker thread for EquatorImage processing.
        
        :param bioImg: EquatorImage instance (already created with ImageData)
        :param settings: Processing settings dict
        :param paramInfo: Parameter info dict
        """
        super().__init__()
        self.bioImg = bioImg
        self.settings = settings
        self.paramInfo = paramInfo
        self.signals = WorkerSignals()
        
    @Slot()
    def run(self):
        try:
            self.bioImg.process(self.settings, self.paramInfo)
        except:
            traceback.print_exc()
            self.signals.error.emit((traceback.format_exc()))
        else:
            self.signals.result.emit(self.bioImg)
        finally:
            self.signals.finished.emit(self.bioImg)

class EquatorWindow(QMainWindow):
    """
    Window displaying all information of a selected image.
    This window contains 3 tabs : image, fitting, results
    """
    def __init__(self, mainWin):
        """
        Init window with main window object and selected file name
        :param mainWin: main window object
        :param filename: selected file name
        """
        super().__init__()
        self.mainWindow = mainWin
        self.logger = None
        self.editableVars = {}
        self.bioImg = None  # Current EquatorImage object
        self.default_img_zoom = None  # default zoom calculated after processing image
        self.zoomOutClicked = False  # to check whether zoom out is clicked for using default zoom value
        self.img_zoom = None  # Params for x and y ranges of displayed image in image tab
        self.graph_zoom = None # Params for x and y ranges of displayed graph in fitting tab
        self.function = None  # Current active function
        self.syncUI = False  # boolean status for UI sync. Prevent recursive infinite processing
        self.update_plot = {'img': True, 'graph' :True, 'results': True}  # update status of each tab
        self.in_batch_process = False
        self.fixedIntArea = None
        self.first = True
        self.orientationModel = None
        self.modeOrientation = None
        self.newImgDimension = None
        self.plot_min = None
        self.stop_process = False
        self.chordpoints = []
        self.chordLines = []
        
        self.del_hist = False
        self.threadPool = QThreadPool()
        self.tasksQueue = Queue()
        self.currentTask = None
        self.worker = None
        self.tasksDone = 0
        self.totalFiles = 1
        self.imageMaskingTool = None
        
        self.gap_lines = []
        self.gaps = []
        
        # Multiprocessing task management
        from ..utils.task_manager import ProcessingTaskManager
        self.taskManager = ProcessingTaskManager()
        self.processExecutor = None
        self.currentDisplayIndex = 0
        self.pendingUIUpdates = {}  # {job_index: task}
        
        # UI update timer for sequential display
        self.uiUpdateTimer = QTimer(self)
        self.uiUpdateTimer.timeout.connect(self.processUIUpdateQueue)
        self.uiUpdateTimer.setInterval(100)  # Check every 100ms

        self._provisionalCount = False
        self._scan_timer = QTimer(self)
        self._scan_timer.setInterval(200)
        self._scan_timer.timeout.connect(self._checkScanDone)
        
        self.current_image_data = None  # Current ImageData object
        self.filePath = ""
        self.dir_path = ""
        
        self.initUI()  # Initial all UI

        self.doubleZoomGUI = DoubleZoom(self.displayImgFigure)

        self.setAllToolTips()  # Set tooltips for widgets
        self.setConnections()  # Set interaction for widgets
        self.show()

        self.browseFile()
        
        if self.file_manager.names is None or len(self.file_manager.names) == 0:
            self.inputerror()
            return
        self.csvManager = EQ_CSVManager(self.dir_path)  # Create a CSV Manager object
        self.setWindowTitle("Muscle X Equator v." + __version__)
        self.onImageChanged(first_run=True)
        
        # Connect workspace signals AFTER initial load to prevent double-processing
        self._connectWorkspaceSignals()

    def _checkScanDone(self):
        """
        Check if the scan is done
        """
        if not self.file_manager:
            return
        
        # Show HDF5 processing progress
        h5_done, h5_total = self.file_manager.get_h5_progress()
        if h5_total > 0:
            if not self.progressBar.isVisible():
                self.progressBar.setVisible(True)
                self.progressBar.setRange(0, h5_total)
            self.progressBar.setValue(h5_done)
            self.progressBar.setFormat(f"Processing HDF5 files: {h5_done}/{h5_total}")
        
        if not self.file_manager.is_scan_done():
            return
        
        # Hide progress bar when done
        self.progressBar.setVisible(False)
        self.progressBar.setFormat("%p%")  # Reset format to default
        
        self._provisionalCount = False
        self._scan_timer.stop()
        self.refreshStatusbar()

    def initProcessExecutor(self):
        """Initialize persistent process pool for parallel processing"""
        from concurrent.futures import ProcessPoolExecutor
        from ..headless.mp_executor import init_worker
        import os
        
        worker_count = int(os.environ.get('MUSCLEX_WORKERS', max(1, os.cpu_count() - 2)))
        
        try:
            self.processExecutor = ProcessPoolExecutor(
                max_workers=worker_count,
                initializer=init_worker
            )
            print(f"✓ Initialized process pool with {worker_count} workers")
        except Exception as e:
            print(f"⚠ Failed to create process pool: {e}")
            print("  Falling back to single-process mode")
            self.processExecutor = None
    
    def processUIUpdateQueue(self):
        """
        Process queued UI updates in submission order.
        Called by timer every 100ms. Only used during batch processing.
        """
        next_index = self.currentDisplayIndex
        
        if next_index in self.pendingUIUpdates:
            task = self.pendingUIUpdates.pop(next_index)
            self._updateBatchImagePreview(task)
            
            self.currentDisplayIndex += 1
            # Check if more are ready
            QTimer.singleShot(0, self.processUIUpdateQueue)
    
    def _updateBatchImagePreview(self, task):
        """
        Lightweight UI update during batch processing.
        Only updates Image tab if it's currently visible.
        """
        # Always update progress and status
        stats = self.taskManager.get_statistics()
        self.progressBar.setValue(stats['completed'] + stats['failed'])
        self.statusReport.setText(
            f"Processing: {task.filename} ({stats['completed']}/{stats['total']})"
        )
        
        # Update Image tab preview if visible
        if self.tabWidget.currentIndex() == 0:  # Image tab
            if task.result and not task.error:
                # Update bioImg with processed image data from worker
                self.bioImg.info = task.result['info']
                self.bioImg.image = task.result['image']
                # Reconstruct rotated_img cache
                self.bioImg.rotated_img = [
                    task.result['info']['center'],
                    task.result['info']['rotationAngle'],
                    task.result['image'],
                    task.result['rotated_img']
                ]
                self.updateImageTab()
        
        # Immediately release large image data after UI update to prevent memory accumulation
        if task.result:
            task.result['image'] = None
            task.result['rotated_img'] = None

        # Check if batch is complete - check after each UI update
        # Must ensure: all tasks done + all accounted for + no pending UI updates
        if (stats['pending'] == 0 and 
            stats['completed'] + stats['failed'] == stats['total'] and 
            not self.pendingUIUpdates):
            self.onBatchComplete()
        
        QApplication.processEvents()

    def inputerror(self):
        """
        Display input error to screen
        """
        errMsg = QMessageBox()
        errMsg.setText('Invalid Input')
        errMsg.setInformativeText("Please select non empty failedcases.txt or an image\n\n")
        errMsg.setStandardButtons(QMessageBox.Ok)
        errMsg.setIcon(QMessageBox.Warning)
        errMsg.exec_()
        # self.close()
        self.browseFile()

    def mousePressEvent(self, event):
        """
        Clear focus when mouse pressed
        """
        focused_widget = QApplication.focusWidget()
        if focused_widget is not None:
            focused_widget.clearFocus()

    def initUI(self):
        """
        Initial all GUIs including : image tab, fitting tab, results tab, menu bar, and status bar
        """
        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.centralWidget = QWidget(self)

        self.scrollArea.setWidget(self.centralWidget)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.scrollArea)
        

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 20px; width: 200px; }")
        self.checkableButtons = []

        ### Image Tab ###
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.imageTabLayout.setContentsMargins(0, 0, 0, 0)
        
        # Create ProcessingWorkspace (handles file navigation, center/rotation/blank/mask settings)
        # show_display_panel=False and show_double_zoom=False since EQ has its own display options
        self.workspace = ProcessingWorkspace(
            settings_dir=self.filePath,
            coord_transform_func=self._coord_transform_func,
            get_display_center_func=self._get_display_center,
        )
        
        # Expose components for backward compatibility
        self.file_manager = self.workspace.file_manager
        self.navigator = self.workspace.navigator
        self.image_viewer = self.workspace.image_viewer
        self.navControls = self.workspace.navigator.nav_controls
        self.right_panel = self.workspace.right_panel
        
        # Backward compatibility for axes/canvas/figure (used by custom drawing)
        self.displayImgFigure = self.image_viewer.figure
        self.displayImgAxes = self.image_viewer.axes
        self.displayImgCanvas = self.image_viewer.canvas
        
        # Expose display_panel controls for backward compatibility
        self.minIntSpnBx = self.image_viewer.display_panel.minIntSpnBx
        self.maxIntSpnBx = self.image_viewer.display_panel.maxIntSpnBx
        self.minIntLabel = self.image_viewer.display_panel.minIntLabel
        self.maxIntLabel = self.image_viewer.display_panel.maxIntLabel
        self.logScaleIntChkBx = self.image_viewer.display_panel.logScaleChkBx
        self.persistIntensity = self.image_viewer.display_panel.persistChkBx
        self.imgZoomInB = self.image_viewer.display_panel.zoomInBtn
        self.imgZoomOutB = self.image_viewer.display_panel.zoomOutBtn
        
        # Add to checkableButtons list
        self.checkableButtons.append(self.imgZoomInB)
        self.checkableButtons.append(self.imgZoomOutB)
        


        # Create display option checkboxes
        self.centerChkBx = QCheckBox('Center')
        self.centerChkBx.setChecked(True)
        self.rminChkBx = QCheckBox('R-min')
        self.rminChkBx.setChecked(True)
        self.rmaxChkBx = QCheckBox('R-max')
        self.histChkBx = QCheckBox('Histogram')
        self.histChkBx.setChecked(True)
        self.intChkBx = QCheckBox('Integrated Area')
        self.intChkBx.setChecked(True)
        self.imgPeakChkBx = QCheckBox('Peaks')
        self.imgPeakChkBx.setChecked(True)
        
        # Fitting error threshold
        self.fittingErrorText = QLabel("Fitting Error Threshold:")
        self.fittingErrorThreshold = QDoubleSpinBox()
        self.fittingErrorThreshold.setValue(0.2)
        self.fittingErrorThreshold.setKeyboardTracking(False)
        
        # Create a container widget with grid layout for checkboxes (2 columns x 3 rows)
        checkboxContainer = QWidget()
        checkboxGridLayout = QGridLayout()
        checkboxGridLayout.setContentsMargins(0, 0, 0, 0)  # Remove extra margins
        checkboxGridLayout.setSpacing(5)
        
        # Add checkboxes in 2x3 grid: Row 0-2, Col 0-1
        checkboxGridLayout.addWidget(self.centerChkBx, 0, 0)
        checkboxGridLayout.addWidget(self.intChkBx, 0, 1)
        checkboxGridLayout.addWidget(self.rminChkBx, 1, 0)
        checkboxGridLayout.addWidget(self.rmaxChkBx, 1, 1)
        checkboxGridLayout.addWidget(self.histChkBx, 2, 0)
        checkboxGridLayout.addWidget(self.imgPeakChkBx, 2, 1)
        
        checkboxContainer.setLayout(checkboxGridLayout)
        self.image_viewer.display_panel.add_to_top_slot(checkboxContainer)
        fittingErrorContainer = QWidget()
        fittingErrorGridLayout = QGridLayout()
        fittingErrorGridLayout.setContentsMargins(0, 0, 0, 0)
        fittingErrorGridLayout.setSpacing(5)
        fittingErrorGridLayout.addWidget(self.fittingErrorText, 0, 0)
        fittingErrorGridLayout.addWidget(self.fittingErrorThreshold, 0, 1)
        fittingErrorContainer.setLayout(fittingErrorGridLayout)
        self.image_viewer.display_panel.add_to_bottom_slot(fittingErrorContainer)


        qf_checkbox = self.workspace.create_qf_checkbox()
        self.workspace.right_panel.add_widget(qf_checkbox)
        self.workspace.right_panel.add_widget(self.workspace._center_widget)
        self.workspace.right_panel.add_widget(self.workspace._rotation_widget)
        self.workspace.right_panel.add_widget(self.workspace._blank_mask_widget)



        self.imgProcGrp = QGroupBox("Image Processing")
        self.imgProcGrp.setStyleSheet("QGroupBox { font-weight: bold; }")
        self.imgProcLayout = QGridLayout()
        self.imgProcGrp.setLayout(self.imgProcLayout)
        # NOTE: Center/rotation buttons are now provided by ProcessingWorkspace
        # Expose workspace's buttons for backward compatibility
        self.calibrationB = self.workspace._center_widget.calibrationButton
        self.setRotAndCentB = self.workspace._center_widget.setCenterRotationButton
        self.setCentByChords = self.workspace._center_widget.setCentByChords
        self.setCentByPerp = self.workspace._center_widget.setCentByPerp
        self.setAngleB = self.workspace._rotation_widget.setRotationButton
        self.setRminB = QPushButton("Set R-min")
        self.setRminB.setCheckable(True)
        self.setRmaxB = QPushButton("Set R-max")
        self.setRmaxB.setCheckable(True)
        self.setIntAreaB = QPushButton("Set Box Width")
        self.setIntAreaB.setCheckable(True)
        
        self.brightSpot = QCheckBox("Find Orientation with Brightest Spots")
        self.brightSpot.setChecked(False)
        self.checkableButtons.extend([self.setIntAreaB, self.setRminB, self.setRmaxB])
        self.fixedAngleChkBx = QCheckBox("Fixed Angle:")
        self.fixedAngleChkBx.setChecked(False)
        self.fixedRminChkBx = QCheckBox("Fixed R-min:")
        self.fixedRminChkBx.setChecked(False)
        self.fixedRmaxChkBx = QCheckBox("Fixed R-max:")
        self.fixedRmaxChkBx.setChecked(False)
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
        self.fixedRmax = QSpinBox()
        self.fixedRmax.setObjectName('fixedRmax')
        self.editableVars[self.fixedRmax.objectName()] = None
        self.fixedRmax.setKeyboardTracking(False)
        self.fixedRmax.setRange(1, 10000)
        self.fixedRmax.setEnabled(False)
        # NOTE: Blank/mask settings now handled by ProcessingWorkspace
        self.doubleZoom = QCheckBox("Double Zoom")
        self.doubleZoom.setChecked(False)
        self.maskThresSpnBx = QDoubleSpinBox()
        self.maskThresSpnBx.setObjectName('maskThresSpnBx')
        self.editableVars[self.maskThresSpnBx.objectName()] = None
        self.maskThresSpnBx.setRange(-10000,100000)
        self.maskThresSpnBx.setKeyboardTracking(False)
        self.orientationCmbBx = QComboBox()
        self.orientationCmbBx.addItem("Max Intensity")
        self.orientationCmbBx.addItem("GMM")
        self.orientationCmbBx.addItem("Herman Factor (Half Pi)")
        self.orientationCmbBx.addItem("Herman Factor (Pi)")
        self.rotation90ChkBx = QCheckBox("Rotate 90")
        self.forceRot90ChkBx = QCheckBox("Persist Rotation")

        self.resetAllB = QPushButton("Reset All")
        # NOTE: Center/rotation/blank/mask buttons removed from this layout
        # They are now part of ProcessingWorkspace's right panel
        self.imgProcLayout.addWidget(self.setRminB, 0, 0, 1, 2)
        self.imgProcLayout.addWidget(self.setRmaxB, 0, 2, 1, 2)
        self.imgProcLayout.addWidget(self.setIntAreaB, 1, 0, 1, 4)
        self.imgProcLayout.addWidget(self.brightSpot, 2, 0, 1, 2)
        self.imgProcLayout.addWidget(self.doubleZoom, 3, 0, 1, 2)
        self.imgProcLayout.addWidget(QLabel("Mask Threshold:"), 4, 0, 1, 2)
        self.imgProcLayout.addWidget(self.maskThresSpnBx, 4, 2, 1, 2)
        self.imgProcLayout.addWidget(self.fixedAngleChkBx, 5, 0, 1, 2)
        self.imgProcLayout.addWidget(self.fixedAngle, 5, 2, 1, 2)
        self.imgProcLayout.addWidget(self.fixedRminChkBx, 6, 0, 1, 2)
        self.imgProcLayout.addWidget(self.fixedRmin, 6, 2, 1, 2)
        self.imgProcLayout.addWidget(self.fixedRmaxChkBx, 7, 0, 1, 2)
        self.imgProcLayout.addWidget(self.fixedRmax, 7, 2, 1, 2)
        self.imgProcLayout.addWidget(self.fixedIntAreaChkBx, 8, 0, 1, 4)
        self.imgProcLayout.addWidget(self.modeAngleChkBx, 8, 2, 1, 2)
        self.imgProcLayout.addWidget(QLabel("Orientation Finding:"), 9, 0, 1, 2)
        self.imgProcLayout.addWidget(self.orientationCmbBx, 9, 2, 1, 2)
        self.imgProcLayout.addWidget(self.rotation90ChkBx, 10, 0, 1, 2)
        self.imgProcLayout.addWidget(self.forceRot90ChkBx, 10, 2, 1, 2)
        self.imgProcLayout.addWidget(self.resetAllB, 11, 0, 1, 4)

        self.rejectChkBx = QCheckBox("Reject")
        self.rejectChkBx.setFixedWidth(100)

        # Use workspace's navigation controls (exposed for backward compatibility)
        self.navImg = self.workspace.navigator.nav_controls

        # Add EQ-specific settings to workspace's right panel
        self.workspace.right_panel.add_widget(self.imgProcGrp)
        self.workspace.right_panel.add_widget(self.rejectChkBx)

        # Add workspace to image tab (replaces old manual layout)
        self.imageTabLayout.addWidget(self.workspace, 1)
        self.tabWidget.addTab(self.imageTab, "Image")

        ### Fitting Tab ###
        self.fittingTab = QWidget()
        self.fittingTabLayout = QGridLayout(self.fittingTab)

        # Plot
        self.fittingFigure = plt.figure()
        self.fittingAxes = self.fittingFigure.add_subplot(111)
        self.fittingVLayout = QVBoxLayout()
        self.fittingCanvas = FigureCanvas(self.fittingFigure)

        self.generalGrp = QGroupBox("General Settings")
        self.generalGrp.setStyleSheet("QGroupBox { font-weight: bold; }")
        self.genLayout = QGridLayout(self.generalGrp)
        self.skeletalChkBx = QCheckBox("Skeletal Muscle (Z line)")
        self.skeletalChkBx.setFixedWidth(200)
        self.extraPeakChkBx = QCheckBox("Extra Peak")
        self.extraPeakChkBx.setFixedWidth(200)
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
        self.genLayout.addWidget(self.extraPeakChkBx, 1, 0, 1, 2)
        self.genLayout.addWidget(QLabel("Number of peaks: <br/>on each side"), 2, 0, 1, 1)
        self.genLayout.addWidget(self.nPeakSpnBx, 2, 1, 1, 1)
        self.genLayout.addWidget(QLabel("Model : "), 3, 0, 1, 1)
        self.genLayout.addWidget(self.modelSelect, 3, 1, 1, 1)
        self.genLayout.addWidget(self.setPeaksB, 4, 0, 1, 2)

        self.fitDispOptionGrp = QGroupBox('Display Options')
        self.fitDispOptionGrp.setStyleSheet("QGroupBox { font-weight: bold; }")
        self.fitDispOptLayout = QGridLayout()
        self.origHistChkBx = QCheckBox('Original\nHistogram')
        self.hullChkBx = QCheckBox('After\nConvexhull')
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

        self.k_chkbx = QCheckBox("Fixed Background K : ")
        self.k_chkbx.setChecked(True)
        self.k_spnbx = QDoubleSpinBox()
        self.k_spnbx.setObjectName('k_spnbx')
        self.editableVars[self.k_spnbx.objectName()] = None
        self.k_spnbx.setDecimals(4)
        self.k_spnbx.setRange(0, 99999999)
        # self.k_spnbx.setEnabled(False)
        self.k_spnbx.setValue(0)
        self.k_layout = QHBoxLayout()
        self.use_previous_fit_chkbx = QCheckBox("Use Previous Fit")
        self.k_layout.addWidget(self.k_chkbx)
        self.k_layout.addWidget(self.k_spnbx)
        
        
        self.use_smooth_alg = QCheckBox("Interpolate Gaps")
        self.use_smooth_spnbx = QSpinBox()
        self.use_smooth_spnbx.setValue(3)

        self.gaps_grp_bx = QGroupBox()
        self.gaps_grp_bx.setStyleSheet("QGroupBox { font-weight: bold; }")
        self.gaps_grp_bx_layout = QVBoxLayout()
        self.gaps_grp_bx.setLayout(self.gaps_grp_bx_layout)
        self.marginLayout = QHBoxLayout()
        
        self.marginLabel = QLabel("Margin: ")
        self.marginLayout.addWidget(self.marginLabel)
        self.marginLayout.addWidget(self.use_smooth_spnbx)

        self.smoothingWindowLayout = QHBoxLayout()
        self.smoothing_window = QSpinBox()
        self.smoothing_window.setValue(11)
        self.smoothing_window.setRange(3, 101)
        self.smoothing_label = QLabel("Smoothing Window: ")
        self.smoothingWindowLayout.addWidget(self.smoothing_label)
        self.smoothingWindowLayout.addWidget(self.smoothing_window)

        # self.marginLabel.setVisible(False)
        # self.use_smooth_spnbx.setVisible(False)
        # self.smoothing_label.setVisible(False)
        # self.smoothing_window.setVisible(False)
        
        self.addGapsButton = QPushButton("Add Gaps")
        self.addGapsButton.setCheckable(True)
        # self.addGapsButton.setVisible(False)
        
        self.clearGapsButton = QPushButton("Clear Gaps")
        # self.clearGapsButton.setVisible(False)
        
        self.gaps_grp_bx_layout.addLayout(self.marginLayout)
        self.gaps_grp_bx_layout.addLayout(self.smoothingWindowLayout)
        self.gaps_grp_bx_layout.addWidget(self.addGapsButton)
        self.gaps_grp_bx_layout.addWidget(self.clearGapsButton)
        
        self.gaps_grp_bx.setVisible(False)

        self.refittingB = QPushButton("Refit current image")
        self.refitAllButton = QPushButton("Refit current folder")
        self.refitAllButton.setCheckable(True)

        # Reusable navigation controls for Fitting tab
        self.navFit = NavigationControls(process_folder_text="Process Current Folder", process_h5_text="Process Current H5 File")

        self.bottomLayout2 = QGridLayout()
        self.bottomLayout2.addWidget(self.navFit, 0, 0, 1, 2)

        self.fittingOptionsFrame1 = QFrame()
        self.fittingOptionsFrame1.setFixedWidth(505)
        self.fittingOptionsLayout = QHBoxLayout(self.fittingOptionsFrame1)
        self.fittingOptionsLayout.setAlignment(Qt.AlignLeft)
        self.fittingOptionsLayout.addWidget(self.generalGrp)
        self.fittingOptionsLayout.addSpacing(10)
        self.fittingOptionsLayout.addWidget(self.fitDispOptionGrp)
        self.fittingOptionsLayout.addStretch()

        self.fittingOptionsFrame2 = QFrame()
        self.fittingOptionsFrame2.setFixedWidth(505)
        self.fittingOptionsLayout2 = QVBoxLayout(self.fittingOptionsFrame2)
        self.fittingOptionsLayout2.addWidget(self.fittingTabWidget)
        self.fittingOptionsLayout2.addLayout(self.k_layout)
        self.fittingOptionsLayout2.addWidget(self.use_previous_fit_chkbx)
        self.fittingOptionsLayout2.addWidget(self.use_smooth_alg)
        # self.fittingOptionsLayout2.addLayout(self.marginLayout)
        # self.fittingOptionsLayout2.addLayout(self.smoothingWindowLayout)
        # self.fittingOptionsLayout2.addWidget(self.addGapsButton)
        # self.fittingOptionsLayout2.addWidget(self.clearGapsButton)
        self.fittingOptionsLayout2.addWidget(self.gaps_grp_bx)
        self.fittingOptionsLayout2.addWidget(self.refittingB)
        self.fittingOptionsLayout2.addWidget(self.refitAllButton)
        self.fittingOptionsLayout2.addStretch()
        self.fittingOptionsLayout2.addLayout(self.bottomLayout2)

        self.fittingTabLayout.addWidget(self.fittingOptionsFrame1, 1, 0, 1, 1)
        self.fittingTabLayout.addWidget(self.fittingCanvas, 0, 1, 2, 2)
        self.fittingTabLayout.addWidget(self.fittingOptionsFrame2, 0, 0, 1, 1)
        self.tabWidget.addTab(self.fittingTab, "Fitting")

        ### Results Tab ###
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

        ### Parameter Editor ###
        self.parameterEditorTab = QWidget()
        self.parameterEditorTab.setContentsMargins(0, 0, 0, 0)
        self.parameterEditorLayout = QGridLayout(self.parameterEditorTab)
        self.paramEditorTitlebox = QGroupBox()
        self.paramEditorTitlebox.setStyleSheet("QGroupBox { font-weight: bold; }")
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

        ### Menu Bar ###
        selectImageAction = QAction('Select a File (or Failed Cases)...', self)
        selectImageAction.setShortcut('Ctrl+O')
        selectImageAction.triggered.connect(self.browseFile)

        saveSettingsAction = QAction('Save Current Settings', self)
        saveSettingsAction.setShortcut('Ctrl+S')
        saveSettingsAction.triggered.connect(self.saveSettings)

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
        fileMenu.addAction(saveSettingsAction)

        shortcutKeysAct = QAction('Shortcut keys', self)
        # shortcutKeysAct.setShortcut('Ctrl+K')
        shortcutKeysAct.triggered.connect(self.showKeysHelpDialog)
        aboutAct = QAction('About', self)
        # aboutAct.setShortcut('Ctrl+A')
        aboutAct.triggered.connect(self.showAbout)
        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)
        helpMenu.addAction(shortcutKeysAct)

        ### Status Bar ###
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
        self.setStatusBar(self.statusBar)

        self.resize(1400, 1000)
        # self.setMinimumHeight(1000)
        # self.setMinimumWidth(1400)

    def setAllToolTips(self):
        """
        Set Tooltips for widgets
        """
        ### image tab ###
        self.centerChkBx.setToolTip("Show the detected projection center")
        self.intChkBx.setToolTip("Show the detected Integrated area ")
        self.rminChkBx.setToolTip("Show the detected R-min")
        self.rmaxChkBx.setToolTip("Show the selected R-max")
        self.histChkBx.setToolTip("Show the background subtracted histogram obtained using convex hull operators")
        self.imgPeakChkBx.setToolTip("Show the detected peaks")

        # NOTE: Center/rotation/blank/mask tooltips now managed by ProcessingWorkspace
        self.setRminB.setToolTip("Activate R-min adjustment.\n To adjust, please click location of R-min on the image")
        self.setRmaxB.setToolTip("Activate R-max adjustment.\n To adjust, please click location of R-max on the image")
        self.setIntAreaB.setToolTip(
            "Activate Integrated Area adjustment.\n To adjust, please click start and end position of the Integrated area on the image")
        self.brightSpot.setToolTip("Use The Brightest Spots to Find The Orientation\n")
        self.maskThresSpnBx.setToolTip("Pixel values to discard")
        self.resetAllB.setToolTip("Reset all manual settings, and process image again with default detection")
        self.rejectChkBx.setToolTip(
            "Reject the case when model cannot be fitted. The word \"REJECTED\" will appear in summary.csv")
        self.navImg.processFolderButton.setToolTip("Process all images in the same directory as the current file with current fitting parameters and image settings")

        ### Fitting tab ###
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
        self.navFit.processFolderButton.setToolTip("Process all images in the same directory as the current file with current fitting parameters and image settings")
        self.refittingB.setToolTip("If you change parameters relating to image processing (e.g. center finding) they will not be used when you refit. Also, image processing parameters (e.g. center) will not change when you refit.")
        self.refitAllButton.setToolTip("Refit all images in the directory again with current fitting parameters")

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        ### Tab Widget ###
        self.tabWidget.currentChanged.connect(self.updateUI)
        # self.fittingTabWidget.currentChanged.connect(self.updateUI)

        ### Image Tab ###
        self.centerChkBx.stateChanged.connect(self.updateImage)
        self.histChkBx.stateChanged.connect(self.updateImage)
        self.intChkBx.stateChanged.connect(self.updateImage)
        self.rminChkBx.stateChanged.connect(self.updateImage)
        self.rmaxChkBx.stateChanged.connect(self.updateImage)
        self.imgPeakChkBx.stateChanged.connect(self.updateImage)

        self.logScaleIntChkBx.stateChanged.connect(self.updateImage)
        self.imgZoomInB.clicked.connect(self.imgZoomIn)
        self.imgZoomOutB.clicked.connect(self.imgZoomOut)
        self.fittingErrorThreshold.valueChanged.connect(self.fittingErrorChanged)

        # NOTE: Center/rotation/blank/mask button connections now handled by ProcessingWorkspace
        self.setRminB.clicked.connect(self.setRminClicked)
        self.setRmaxB.clicked.connect(self.setRmaxClicked)
        self.setIntAreaB.clicked.connect(self.setIntAreaClicked)
        self.brightSpot.clicked.connect(self.brightSpotClicked)
        self.fixedAngleChkBx.stateChanged.connect(self.fixedAngleChecked)
        self.fixedRminChkBx.stateChanged.connect(self.fixedRminChecked)
        self.fixedRmaxChkBx.stateChanged.connect(self.fixedRmaxChecked)
        self.fixedIntAreaChkBx.stateChanged.connect(self.fixedIntAreaChecked)
        self.modeAngleChkBx.clicked.connect(self.modeAngleChecked)
        self.fixedAngle.editingFinished.connect(self.fixedAngleChanged)
        self.fixedRmin.editingFinished.connect(self.fixedRminChanged)
        self.fixedRmax.editingFinished.connect(self.fixedRmaxChanged)
        self.maskThresSpnBx.editingFinished.connect(self.maskThresChanged)
        self.doubleZoom.stateChanged.connect(self.doubleZoomChecked)
        self.orientationCmbBx.currentIndexChanged.connect(self.orientationModelChanged)
        self.rotation90ChkBx.stateChanged.connect(self.rotation90Checked)
        self.forceRot90ChkBx.stateChanged.connect(self.forceRot90Checked)
        self.resetAllB.clicked.connect(self.resetAll)

        # NOTE: prev/next/filename navigation now handled by ProcessingWorkspace's navigator
        # We only connect batch processing buttons
        self.navImg.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.navImg.processH5Button.toggled.connect(self.h5batchProcBtnToggled)
        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)
        self.displayImgFigure.canvas.mpl_connect('button_release_event', self.imgReleased)
        self.displayImgFigure.canvas.mpl_connect('figure_leave_event', self.leaveImage)
        self.displayImgFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)
        self.rejectChkBx.stateChanged.connect(self.rejectClicked)

        #### Fitting Tab
        self.skeletalChkBx.stateChanged.connect(self.skeletalChecked)
        self.nPeakSpnBx.editingFinished.connect(self.nPeakChanged)
        self.modelSelect.currentIndexChanged.connect(self.modelChanged)
        self.setPeaksB.clicked.connect(self.setManualPeaks)
        self.origHistChkBx.stateChanged.connect(self.refreshGraph)
        self.hullChkBx.stateChanged.connect(self.refreshGraph)
        self.peakChkBx.stateChanged.connect(self.refreshGraph)
        self.dispZlineChkBx.stateChanged.connect(self.refreshGraph)
        self.centerXChkBx.stateChanged.connect(self.refreshGraph)
        self.fitChkBx.stateChanged.connect(self.refreshGraph)
        self.graphZoomInB.clicked.connect(self.graphZoomIn)
        self.graphZoomOutB.clicked.connect(self.graphZoomOut)

        #self.navFit.processFolderButton.clicked.connect(self.processFolder)
        self.navFit.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.navFit.processH5Button.toggled.connect(self.h5batchProcBtnToggled)
        self.navFit.prevButton.clicked.connect(self.prevClicked)
        self.navFit.nextButton.clicked.connect(self.nextClicked)
        self.navFit.nextFileButton.clicked.connect(self.nextFileClicked)
        self.navFit.prevFileButton.clicked.connect(self.prevFileClicked)
        self.navFit.filenameLineEdit.editingFinished.connect(self.fileNameChanged)
        self.fittingFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.fittingFigure.canvas.mpl_connect('motion_notify_event', self.plotOnMotion)
        self.fittingFigure.canvas.mpl_connect('button_release_event', self.plotReleased)
        self.fittingFigure.canvas.mpl_connect('figure_leave_event', self.leavePlot)
        self.fittingFigure.canvas.mpl_connect('scroll_event', self.plotScrolled)

        self.k_chkbx.stateChanged.connect(self.k_checked)
        self.k_spnbx.editingFinished.connect(self.kChanged)
        self.refittingB.clicked.connect(self.refitting)
        self.refitAllButton.toggled.connect(self.refitAllBtnToggled)
        self.use_smooth_alg.stateChanged.connect(self.useSmoothClicked)
        self.use_smooth_spnbx.editingFinished.connect(self.useSmoothSpnboxChanged)
        self.smoothing_window.editingFinished.connect(self.smoothingWindowChanged)
        self.addGapsButton.clicked.connect(self.addGaps)
        self.clearGapsButton.clicked.connect(self.clearGaps)

        #### Parameter Editor Tab
        self.parameterEditorTable.itemClicked.connect(self.onRowFixed)
        self.refitParamsBtn.clicked.connect(self.refitParamEditor)
        self.addSPeakBtn.clicked.connect(self.addSPeak)
        self.enableExtraGaussBtn.clicked.connect(self.enableExtraGauss)
    
    # ==================== ProcessingWorkspace Integration ====================
    
    def _connectWorkspaceSignals(self):
        """Connect ProcessingWorkspace signals to EquatorWindow handlers."""
        # Main processing pipeline: ImageData ready -> create EquatorImage -> process
        self.workspace.imageDataReady.connect(self._on_image_data_ready)
        
        # Settings changed -> reprocess current image
        self.workspace.needsReprocess.connect(self.processImage)
        
        # Status bar updates
        self.workspace.statusTextRequested.connect(self._on_status_text_requested)
        
        # Scan progress (for HDF5 files)
        self.workspace.scanComplete.connect(self._on_scan_complete)
        self.workspace.scanProgressChanged.connect(self._on_scan_progress)
    
    def _coord_transform_func(self, x, y):
        """
        Transform coordinates from displayed (rotated) image to original image.
        Used by ProcessingWorkspace for center/rotation tools.
        
        Args:
            x, y: Coordinates in rotated/displayed image
            
        Returns:
            (orig_x, orig_y): Coordinates in original image
        """
        if self.bioImg and self.bioImg.rotMat is not None:
            inv_mat = cv2.invertAffineTransform(self.bioImg.rotMat)
            point = np.dot(inv_mat, [x, y, 1])
            return point[0], point[1]
        return x, y
    
    def _get_display_center(self):
        """
        Get center in display (rotated) coordinates.
        Used by ProcessingWorkspace for rotation tool.
        
        Returns:
            (x, y) center in display coordinates, or None
        """
        if self.bioImg:
            return self.bioImg.center
        return None
    
    def _on_image_data_ready(self, image_data):
        """
        Handle new image from workspace (called when navigation changes image).
        
        Args:
            image_data: ImageData instance from workspace
        """
        self.current_image_data = image_data
        # Trigger the existing processing pipeline
        self.onImageChanged()

    
    def _on_status_text_requested(self, text):
        """Update status bar from workspace requests."""
        if text:
            self.left_status.setText(text)
        else:
            self.refreshStatusbar()
    
    def _on_scan_complete(self):
        """Handle scan completion from workspace."""
        self._provisionalCount = False
        self._scan_timer.stop()
        self.refreshStatusbar()
    
    def _on_scan_progress(self, done, total):
        """Handle scan progress update from workspace."""
        if total > 0:
            if not self.progressBar.isVisible():
                self.progressBar.setVisible(True)
                self.progressBar.setRange(0, total)
            self.progressBar.setValue(done)
            self.progressBar.setFormat(f"Processing HDF5 files: {done}/{total}")
    
    # ==================== End ProcessingWorkspace Integration ====================
    
    def clearGaps(self):
        self.bioImg.info['gaps'] = []
    
    def addGaps(self):
        if self.addGapsButton.isChecked():
            self.function = ['addGaps']
            if 'gaps' not in self.bioImg.info:
                self.bioImg.info['gaps'] = []
        else:
            self.function = None

    
    def smoothingWindowChanged(self):
        self.bioImg.info['smoothing_window'] = self.smoothing_window.value()
    
    def useSmoothSpnboxChanged(self):
        self.bioImg.info['smooth_margin'] = self.use_smooth_spnbx.value()
        # del self.bioImg.info['hulls']
        # del self.bioImg.info['hist']
        # if (self.use_smooth_alg.isChecked()):
        #     self.refitting()
        
    def useSmoothClicked(self):
        # self.addGapsButton.setVisible(self.use_smooth_alg.isChecked())
        # self.clearGapsButton.setVisible(self.use_smooth_alg.isChecked())
        # self.marginLabel.setVisible(self.use_smooth_alg.isChecked())
        # self.use_smooth_spnbx.setVisible(self.use_smooth_alg.isChecked())
        # self.smoothing_label.setVisible(self.use_smooth_alg.isChecked())
        # self.smoothing_window.setVisible(self.use_smooth_alg.isChecked())
        self.gaps_grp_bx.setVisible(self.use_smooth_alg.isChecked())
        self.bioImg.info['use_smooth_alg'] = self.use_smooth_alg.isChecked()
        self.bioImg.info['smooth_margin'] = self.use_smooth_spnbx.value()
        self.bioImg.info['smoothing_window'] = self.smoothing_window.value()
        # if 'hulls' in self.bioImg.info:
        #     del self.bioImg.info['hulls']
        # if 'hist' in self.bioImg.info:
        #     del self.bioImg.info['hist']
        # self.refitting()
        
        
    def fittingErrorChanged(self):
        self.bioImg.fitting_error = self.fittingErrorThreshold.value()
        self.refitting()

    def skeletalChecked(self):
        """
        Handle when skeletal z line is checked or unchecked
        """
        self.extraPeakChkBx.setEnabled(self.skeletalChkBx.isChecked())
        self.bioImg.info["isSkeletal"] = self.skeletalChkBx.isChecked()
        self.bioImg.saveCache()
        if not self.skeletalChkBx.isChecked():
            self.extraPeakChkBx.setChecked(False)

    def modelChanged(self):
        """
        Handle when model is changed
        """
        self.bioImg.info["model"] = str(self.modelSelect.currentText())
        self.bioImg.saveCache()

    def k_checked(self):
        """
        Handle when bias k is checked or unchecked
        """
        self.k_spnbx.setEnabled(self.k_chkbx.isChecked())

    def kChanged(self):
        """
        Handle when bias k is changed
        """
        self.log_changes('backgroudK', obj=self.k_spnbx)

    def refitting(self):
        """
        Fixed Value Changed. Remove fit_results from info dict to make it be re-calculated and Recalculate
        :return:
        """
        if 'use_smooth_alg' in self.bioImg.info:
            if 'hist' in self.bioImg.info:
                del self.bioImg.info['hist']
            if 'hulls' in self.bioImg.info:
                del self.bioImg.info['hulls']

        self.refreshAllFittingParams()
        if self.use_previous_fit_chkbx.isChecked() and self.bioImg is not None:
            print("Using previous fit")
            ret = self.updateFittingParamsInParamInfo()
            if ret == -1:
                return
            self.processImage(self.bioImg.info['paramInfo'])
            return
        else:
            self.processImage()

    def updateFittingParamsInParamInfo(self):
        """
        Update the fitting params in param info
        """
        if 'paramInfo' not in self.bioImg.info:
            errMsg = QMessageBox()
            errMsg.setText('Cache file not found')
            errMsg.setInformativeText("Please process the image first before using previous fit")
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
            return -1
        paramInfo = self.bioImg.info['paramInfo']
        settings = self.getSettings()
        paramInfo['isSkeletal']['val'] = settings['isSkeletal']
        paramInfo['isExtraPeak']['val'] = settings['isExtraPeak']

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
            self.updateParamInfo('intz_EP', side, fitparams)
            self.updateParamInfo('sigz_EP', side, fitparams)
            self.updateParamInfo('zline_EP', side, fitparams)
            self.updateParamInfo('gammaz_EP', side, fitparams)
        return 0

    def updateParamInfo(self, param, side, fitparams):
        """
        Update param info with the parameters given to the function
        """
        paramInfo = self.bioImg.info['paramInfo']
        #Handling sigz and sigmaz discrepancy, side_fix_sigz vs side_sigmaz
        p = param
        if param == 'sigz':
            p = 'sigmaz'
        if param == 'sigz_EP':
            p = 'sigmaz_EP'
        pInfo = paramInfo[side + '_' + p]
        if side+'_fix_' + param in fitparams:
            pInfo['fixed'] = True
            pInfo['val'] = fitparams[side+'_fix_'+param]
        else:
            pInfo['fixed'] = False
            if side + '_' + param in fitparams:
                pInfo['val'] = fitparams[side + '_' + param]

    def refitAllBtnToggled(self):
        """
        Triggered when refit all button is clicked
        """
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
        nImg = len(self.file_manager.names)
        errMsg = QMessageBox()
        errMsg.setText('Refitting All')
        text = 'The current folder will be refitted using current settings. Make sure to adjust them before refitting the folder. \n\n'
        settings = self.getSettings()
        text += "\nCurrent Settings"

        if 'fixed_angle' in settings:
            text += "\n  - Fixed Angle : " + str(settings["fixed_angle"])
        if 'fixed_rmin' in settings:
            text += "\n  - Fixed R-min : " + str(settings["fixed_rmin"])
        if 'fixed_rmax' in settings:
            text += "\n  - Fixed R-max : " + str(settings["fixed_rmax"])
        if 'fixed_int_area' in settings:
            text += "\n  - Fixed Box Width : " + str(settings["fixed_int_area"])

        text += "\n  - Orientation Finding : " + str(self.orientationCmbBx.currentText())
        text += "\n  - Skeletal Muscle : " + str(settings["isSkeletal"])
        text += "\n  - Extra Peak : " + str(settings["isExtraPeak"])
        text += "\n  - Number of Peaks on each side : " + str(settings["nPeaks"])
        text += "\n  - Model : " + str(settings["model"])

        for side in ['left', 'right']:
            if side+'_fix_sigmac' in settings:
                text += "\n  - "+side+" Fixed Sigma C : " + str(settings[side+'_fix_sigmac'])
            if side+'_fix_sigmad' in settings:
                text += "\n  - "+side+" Fixed Sigma D : " + str(settings[side+'_fix_sigmad'])
            if side+'_fix_sigmas' in settings:
                text += "\n  - "+side+" Fixed Sigma S : " + str(settings[side+'_fix_sigmas'])
            if side+'_fix_gamma' in settings:
                text += "\n  - "+side+" Fixed Gamma : " + str(settings[side+'_fix_gamma'])
            if side+'_fix_zline' in settings:
                text += "\n  - "+side+" Fixed Z line Center: " + str(settings[side+'_fix_zline'])
            if side+'_fix_intz' in settings:
                text += "\n  - "+side+" Fixed Z line Intensity : " + str(settings[side+'_fix_intz'])
            if side+'_fix_sigz' in settings:
                text += "\n  - "+side+" Fixed Z line Sigma : " + str(settings[side+'_fix_sigz'])
            if side+'_fix_gammaz' in settings:
                text += "\n  - "+side+" Fixed Z line Gamma : " + str(settings[side+'_fix_gammaz'])
            if side+'_fix_zline_EP' in settings:
                text += "\n  - "+side+" Fixed Extra Peak Center: " + str(settings[side+'_fix_zline_EP'])
            if side+'_fix_intz_EP' in settings:
                text += "\n  - "+side+" Fixed Extra Peak Intensity : " + str(settings[side+'_fix_intz_EP'])
            if side+'_fix_sigz_EP' in settings:
                text += "\n  - "+side+" Fixed Extra Peak Sigma : " + str(settings[side+'_fix_sigz_EP'])
            if side+'_fix_gammaz_EP' in settings:
                text += "\n  - "+side+" Fixed Extra Peak Gamma : " + str(settings[side+'_fix_gammaz_EP'])

        calSettings = self.workspace.calibration_settings
        if calSettings is not None:
            if "center" in calSettings:
                text += "\n  - Calibration Center : " + str(calSettings["center"])
            if 'type' in calSettings:
                if calSettings["type"] == "img":
                    text += "\n  - Silver Behenate : " + str(calSettings["silverB"]) + " nm"
                    text += "\n  - Sdd : " + str(calSettings["radius"]) + " pixels"
                else:
                    text += "\n  - Lambda : " + str(calSettings["lambda"]) + " nm"
                    text += "\n  - Sdd : " + str(calSettings["sdd"]) + " mm"
                    text += "\n  - Pixel Size : " + str(calSettings["pixel_size"]) + " nm"

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
                self.nextImageFitting(False)
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

    def doubleZoomChecked(self):
        """
        Triggered when double zoom toggle is checked
        """
        self.doubleZoomGUI.doubleZoomChecked(img=self.bioImg.getRotatedImage(),
                                             canv=self.displayImgCanvas,
                                             center=self.bioImg.center,
                                             is_checked=self.doubleZoom.isChecked())


    def blankSettingClicked(self):
        """
        Trigger when Set Blank Image and Mask clicked
        """
        # dlg = BlankImageSettings(self.dir_path)
        # result = dlg.exec_()
        # if result == 1 and self.bioImg is not None:
        #     self.resetAll()
        
        isH5 = False
        if self.file_manager.current_file_type == 'h5':
            isH5 = True

        img = self.bioImg.getRotatedImage()

        try:
            fabio.tifimage.tifimage(data=img).write(os.path.join(self.dir_path,'settings/tempMaskFile_eq.tif'))
        except:
            print("ERROR WITH SAVING THE IMAGE") 

        max_val =  np.max(np.ravel(img))

        orig_size = self.bioImg.orig_img.shape 

        rotationAngle = self.bioImg.rotation

        trans_x = (img.shape[0] - self.bioImg.orig_img.shape[0]) / 2
        trans_y = (img.shape[1] - self.bioImg.orig_img.shape[1]) / 2

        trans_mat = np.float32([[1,0,trans_x],[0,1,trans_y]])

        self.imageMaskingTool = ImageMaskerWindow(self.dir_path , 
                                                  os.path.join(self.dir_path, "settings/tempMaskFile_eq.tif"), 
                                                  self.minIntSpnBx.value(), 
                                                  self.maxIntSpnBx.value(), 
                                                  max_val=max_val, 
                                                  orig_size=orig_size, 
                                                  trans_mat=trans_mat, 
                                                  rot_angle=rotationAngle, 
                                                  isHDF5=isH5)
            
        if self.imageMaskingTool is not None and self.imageMaskingTool.exec_():
            if os.path.exists(os.path.join(os.path.join(self.dir_path, 'settings'), 'blank_image_settings.json')):
                with open(os.path.join(os.path.join(self.dir_path, 'settings'), 'blank_image_settings.json'), 'r') as f:
                    info = json.load(f)
                    if 'path' in info:
                        img = fabio.open(info['path']).data
                        fabio.tifimage.tifimage(data=img).write(os.path.join(os.path.join(self.dir_path, 'settings'),'blank.tif'))    
            else:
                if os.path.exists(os.path.join(os.path.join(self.dir_path, 'settings'), 'mask.tif')):
                    os.rename(os.path.join(os.path.join(self.dir_path, 'settings'), 'mask.tif'), os.path.join(os.path.join(self.dir_path, 'settings'), 'maskonly.tif'))
                    
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
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            ax.plot(bioImg.info['hulls']['all'], color = 'b')
            self.fittingCanvas.draw_idle()
        else:
            # Finish peak selections
            self.setPeaksB.setText("Start Manual Peak Selection")
            centerX = bioImg.center[0]
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
                centerX = bioImg.center[0]
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
            elif func[0] == 'addGaps':
                x = int(round(x))
                func.append(x)
                ax = self.fittingAxes
                line = ax.axvline(x, linewidth=2, color='r')
                self.gap_lines.append(line)
                self.fittingCanvas.draw_idle()
                if len(func) == 3:
                    gap_start = func[1]
                    gap_end = func[2]
                    self.bioImg.info['gaps'].append((gap_start, gap_end))
                    self.addGapsButton.setChecked(False)
                    print(self.bioImg.info['gaps'])
                    self.function = None
                    

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
                       "Equator is running under" +
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

    def imgScrolled(self, event):
        """
        This function is called when a mouse scrolled on the image in image tab. This will affect zoom-in and zoom-out
        """
        
        if self.bioImg is None or event.xdata is None or event.ydata is None:
            return
        
        self.scrollArea.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.scrollArea.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)

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
        
        self.scrollArea.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.scrollArea.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)

    def browseFile(self):
        """
        Popup an input file dialog. Users can select an image or .txt for failed cases list.
        Now delegates to ProcessingWorkspace for file management.
        """
        file_name = getAFile(add_txt=True)
        print("FILE: ", file_name)
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

        self.fileName = file_name
        # Load file through workspace (handles FileManager, settings, navigation)
        self.workspace.load_from_file(str(file_name))
        self.dir_path = self.file_manager.dir_path
        self.filePath = self.dir_path
        self._provisionalCount = True
        self._scan_timer.start()


    def saveSettings(self):
        """
        save settings to json
        """
        settings = self.getSettings()
        # paramInfo = self.getInfoFromParameterEditor()
        # settings['paramInfo'] = paramInfo

        filename = getSaveFile(os.path.join("musclex", "settings", "eqsettings.json"), None)
        if filename!="":
            with open(filename,'w') as f:
                json.dump(settings,f)

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
            self.bioImg.info["nPeaks"] = self.skeletalChkBx.isChecked()
            self.bioImg.saveCache()
            self.log_changes('nPeaks', self.nPeakSpnBx)

    def batchProcBtnToggled(self):
        """
        Triggered when the process batch button is toggled.
        """
        if self.navImg.processFolderButton.isChecked():
            if not self.in_batch_process:
                self.navImg.processFolderButton.setText("Stop")
                self.processFolder()
        elif self.navFit.processFolderButton.isChecked():
            if not self.in_batch_process:
                self.navFit.processFolderButton.setText("Stop")
                self.processFolder()
        else:
            self.stopProcess()

    def h5batchProcBtnToggled(self):
        """
        Triggered when the batch process button is toggled
        """
        if self.navImg.processH5Button.isChecked():
            if not self.progressBar.isVisible():
                self.navImg.processH5Button.setText("Stop")
                self.processH5Folder()
        elif self.navFit.processH5Button.isChecked():
            if not self.progressBar.isVisible():
                self.navFit.processH5Button.setText("Stop")
                self.processH5Folder()
        else:
            self.stopProcess()

    def onBatchComplete(self):
        """Called when all batch tasks complete"""
        stats = self.taskManager.get_statistics()
        
        # Re-enable navigation
        self.navImg.prevButton.setEnabled(True)
        self.navImg.nextButton.setEnabled(True)
        self.navFit.prevButton.setEnabled(True)
        self.navFit.nextButton.setEnabled(True)
        
        # Print summary
        print("\n" + "="*60)
        print("BATCH PROCESSING COMPLETE")
        print("="*60)
        print(f"Total: {stats['total']}, Success: {stats['completed']}, Failed: {stats['failed']}")
        print(f"Average time: {stats['avg_time']:.2f}s per image")
        print("="*60)
        
        # Cleanup
        self._cleanupAfterBatch()
        
        # Show completion dialog
        QMessageBox.information(self, "Batch Complete",
            f"Processed {stats['completed']}/{stats['total']} images\n"
            f"Failed: {stats['failed']}\n"
            f"Avg time: {stats['avg_time']:.2f}s per image")
    

    def _cleanupAfterBatch(self):
        """
        Cleanup after batch processing completes.
        Properly shuts down process pool and releases all resources.
        """
        # Stop UI update timer
        if self.uiUpdateTimer.isActive():
            self.uiUpdateTimer.stop()
        
        # Shutdown process pool properly to release child processes
        if self.processExecutor:
            try:
                print("Shutting down process pool...")
                self.processExecutor.shutdown(wait=True, cancel_futures=False)
                print("✓ Process pool shutdown complete")
            except Exception as e:
                print(f"⚠ Error shutting down process pool: {e}")
        
        # Cleanup UI state
        self.in_batch_process = False
        self.progressBar.setVisible(False)
        self.navImg.processFolderButton.setChecked(False)
        self.navFit.processFolderButton.setChecked(False)
        self.navImg.processFolderButton.setText("Process current folder")
        self.navFit.processFolderButton.setText("Process current folder")
        
        # Clear reference after shutdown
        self.processExecutor = None
    
    def _buildProcessSettingsText(self, settings, nImg, description):
        """
        Build settings information text for process confirmation dialog
        
        Args:
            settings: Current settings dictionary
            nImg: Number of images to process
            description: Description of what will be processed
            
        Returns:
            Formatted text string with all settings
        """
        text = f'The {description} will be processed using current settings. Make sure to adjust them before processing. \n\n'
        text += "\nCurrent Settings"

        if 'fixed_angle' in settings:
            text += "\n  - Fixed Angle : " + str(settings["fixed_angle"])
        if 'fixed_rmin' in settings:
            text += "\n  - Fixed R-min : " + str(settings["fixed_rmin"])
        if 'fixed_rmax' in settings:
            text += "\n  - Fixed R-max : " + str(settings["fixed_rmax"])
        if 'fixed_int_area' in settings:
            text += "\n  - Fixed Box Width : " + str(settings["fixed_int_area"])

        text += "\n  - Orientation Finding : " + str(self.orientationCmbBx.currentText())
        text += "\n  - Skeletal Muscle : " + str(settings["isSkeletal"])
        text += "\n  - Extra Peak : " + str(settings["isExtraPeak"])
        text += "\n  - Number of Peaks on each side : " + str(settings["nPeaks"])
        text += "\n  - Model : " + str(settings["model"])

        for side in ['left', 'right']:
            if side+'_fix_sigmac' in settings:
                text += "\n  - "+side+" Fixed Sigma C : " + str(settings[side+'_fix_sigmac'])
            if side+'_fix_sigmad' in settings:
                text += "\n  - "+side+" Fixed Sigma D : " + str(settings[side+'_fix_sigmad'])
            if side+'_fix_sigmas' in settings:
                text += "\n  - "+side+" Fixed Sigma S : " + str(settings[side+'_fix_sigmas'])
            if side+'_fix_gamma' in settings:
                text += "\n  - "+side+" Fixed Gamma : " + str(settings[side+'_fix_gamma'])
            if side+'_fix_zline' in settings:
                text += "\n  - "+side+" Fixed Z line Center: " + str(settings[side+'_fix_zline'])
            if side+'_fix_intz' in settings:
                text += "\n  - "+side+" Fixed Z line Intensity : " + str(settings[side+'_fix_intz'])
            if side+'_fix_sigz' in settings:
                text += "\n  - "+side+" Fixed Z line Sigma : " + str(settings[side+'_fix_sigz'])
            if side+'_fix_gammaz' in settings:
                text += "\n  - "+side+" Fixed Z line Gamma : " + str(settings[side+'_fix_gammaz'])
            if side+'_fix_zline_EP' in settings:
                text += "\n  - "+side+" Fixed Extra Peak Center: " + str(settings[side+'_fix_zline_EP'])
            if side+'_fix_intz_EP' in settings:
                text += "\n  - "+side+" Fixed Extra Peak Intensity : " + str(settings[side+'_fix_intz_EP'])
            if side+'_fix_sigz_EP' in settings:
                text += "\n  - "+side+" Fixed Extra Peak Sigma : " + str(settings[side+'_fix_sigz_EP'])
            if side+'_fix_gammaz_EP' in settings:
                text += "\n  - "+side+" Fixed Extra Peak Gamma : " + str(settings[side+'_fix_gammaz_EP'])

        calSettings = self.workspace.calibration_settings
        if calSettings is not None and len(calSettings) > 0:
            # For quadrant folded images, always show that geometric center is used
            if self.bioImg and self.bioImg.quadrant_folded:
                geo_center = (self.bioImg.orig_img.shape[1] / 2, self.bioImg.orig_img.shape[0] / 2)
                text += "\n  - Calibration Center : " + str(geo_center) + " (geometric center for quadrant folded)"
            elif "center" in calSettings:
                text += "\n  - Calibration Center : " + str(calSettings["center"])
            
            if 'type' in calSettings:
                if calSettings["type"] == "img":
                    text += "\n  - Silver Behenate : " + str(calSettings["silverB"]) + " nm"
                    text += "\n  - Sdd : " + str(calSettings["radius"]) + " pixels"
                else:
                    text += "\n  - Lambda : " + str(calSettings["lambda"]) + " nm"
                    text += "\n  - Sdd : " + str(calSettings["sdd"]) + " mm"
                    text += "\n  - Pixel Size : " + str(calSettings["pixel_size"]) + " nm"

        text += f'\n\nAre you sure you want to process {nImg} image(s)? \nThis might take a long time.'
        return text

    def _batchProcessImages(self, job_indices, process_type="folder"):
        """
        Common batch processing logic using multiprocessing
        
        Args:
            job_indices: List or range of indices in self.file_manager.names to process
            process_type: Type of processing ("folder" or "h5") for logging
        """
        # Fallback to old method if multiprocessing failed
        if self.processExecutor is None:
            self.initProcessExecutor()

        nImg = len(job_indices)
        
        # Setup for batch processing
        self.in_batch_process = True
        self.stop_process = False
        
        # Reset task management
        self.taskManager.clear()
        self.currentDisplayIndex = 0
        self.pendingUIUpdates = {}
        
        # Display progress bar
        self.progressBar.setMaximum(nImg)
        self.progressBar.setMinimum(0)
        self.progressBar.setValue(0)
        self.progressBar.setVisible(True)
        
        
        # Prepare settings
        settings = self.getSettings()
        settings['no_cache'] = True 
        
        # Submit all jobs to process pool
        from ..headless.mp_executor import process_one_image
        
        for job_index in job_indices:
            if self.stop_process:
                break
            
            filename = self.file_manager.names[job_index]
            spec = self.file_manager.specs[job_index]
            job_args = (settings, None, self.file_manager.dir_path, filename, spec)
            
            future = self.processExecutor.submit(process_one_image, job_args)
            task = self.taskManager.submit_task(filename, job_index, future)
            
            # Attach callback
            future.add_done_callback(self._onFutureDone)
        
        print(f"Batch {process_type} started: {nImg} images submitted to process pool")

    def processFolder(self):
        """
        Process current folder
        """
        ## Popup confirm dialog with settings
        nImg = len(self.file_manager.names)
        settings = self.getSettings()
        
        errMsg = QMessageBox()
        errMsg.setText('Process Current Folder')
        text = self._buildProcessSettingsText(settings, nImg, "current folder")
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            # Process all images in folder
            self._batchProcessImages(range(len(self.file_manager.names)), process_type="folder")
        else:
            # User cancelled
            self.navImg.processFolderButton.setChecked(False)
            self.navFit.processFolderButton.setChecked(False)
            

    def processH5Folder(self):
        """
        Process current H5 file (all frames)
        """
        ## Popup confirm dialog with settings
        nImg = self.file_manager.current_h5_nframes
        settings = self.getSettings()
        
        errMsg = QMessageBox()
        errMsg.setText('Process Current H5 File')
        text = self._buildProcessSettingsText(settings, nImg, "current H5 file")
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        errMsg.setIcon(QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QMessageBox.Yes:
            # Get the range of indices corresponding to current H5 file
            current_h5_path = self.file_manager._get_current_file_info()[2]  # Get file path
            if current_h5_path in self.file_manager.h5_index_map:
                start_idx, end_idx = self.file_manager.h5_index_map[current_h5_path]
                # Process all frames in this H5 file
                self._batchProcessImages(range(start_idx, end_idx + 1), process_type="h5")
            else:
                print("Error: Could not find H5 file in index map")
        else:
            # User cancelled
            self.navImg.processH5Button.setChecked(False)
            self.navFit.processH5Button.setChecked(False)
        

    def stopProcess(self):
        """
        Stop the process
        """
        self.stop_process = True
        if self.processExecutor:
            self.processExecutor.shutdown(wait=False, cancel_futures=True)
        running_count = self.taskManager.get_running_count()

        # Use QProgressDialog with indeterminate progress (no progress bar)
        msg = f"Stopping Batch Processing\n\nWaiting for {running_count} tasks to complete..."
        self._stopProgress = QProgressDialog(msg, None, 0, 0, self)
        self._stopProgress.setWindowFlags(Qt.Window | Qt.FramelessWindowHint | Qt.WindowStaysOnTopHint)
        self._stopProgress.setModal(False)
        self._stopProgress.show()

        self._stopMsgTimer = QTimer(self)
        self._stopMsgTimer.setInterval(300)
        self._stopMsgTimer.timeout.connect(self._updateStopProgress)
        self._stopMsgTimer.start()
        
        
        
    def _updateStopProgress(self):
        if not hasattr(self, '_stopProgress') or self._stopProgress is None:
            return
        running_count = self.taskManager.get_running_count()
        
        # Update the message text only
        msg = f"Stopping Batch Processing\n\nWaiting for {running_count} tasks to complete..."
        self._stopProgress.setLabelText(msg)
        
        if running_count == 0:
            self._stopMsgTimer.stop()
            self._stopProgress.close()
            self._cleanupAfterBatch()
        
            if getattr(self, '_closingAfterStop', False):
                self._closingAfterStop = False
                QTimer.singleShot(0, self.close)



    def rejectClicked(self):
        """
        Mark EquatorImage object as rejected. Save to cache and write data to summary file
        """
        if self.bioImg is None or self.syncUI:
            return
        self.bioImg.info["reject"] = self.rejectChkBx.isChecked()
        self.bioImg.saveCache()
        self.csvManager.writeNewData(self.bioImg)
        self.csvManager.writeNewData2(self.bioImg)

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
            self.fixedRmaxChkBx.setChecked(False)
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
        """
        Refresh the fitting parameters by cleaning the old information
        """
        if self.bioImg is not None:
            self.bioImg.removeInfo(side + '_fix_sigmac')
            self.bioImg.removeInfo(side + '_fix_sigmad')
            self.bioImg.removeInfo(side + '_fix_sigmas')
            self.bioImg.removeInfo(side + '_fix_gamma')
            self.bioImg.removeInfo(side + '_fix_sigz')
            self.bioImg.removeInfo(side + '_fix_intz')
            self.bioImg.removeInfo(side + '_fix_zline')
            self.bioImg.removeInfo(side + '_fix_gammaz')
            self.bioImg.removeInfo(side + '_fix_sigz_EP')
            self.bioImg.removeInfo(side + '_fix_intz_EP')
            self.bioImg.removeInfo(side + '_fix_zline_EP')
            self.bioImg.removeInfo(side + '_fix_gammaz_EP')
            self.bioImg.removeInfo('fix_k')

    def refreshProcessingParams(self):
        """
        Refresh the processing parameters by cleaning the old information
        """
        if self.bioImg is not None:
            self.bioImg.removeInfo('center')
            self.bioImg.removeInfo('rmin')
            self.bioImg.removeInfo('rmax')
            self.bioImg.removeInfo('int_area')
            self.bioImg.removeInfo('hist')
            self.bioImg.removeInfo('hulls')
            self.bioImg.removeInfo('fit_results')

    def prevClicked(self):
        """
        Going to the previous image
        """
        self.file_manager.prev_frame()
        self.onImageChanged()

    def nextImageFitting(self, reprocess):
        """
        Used for processing of a folder to process the next image
        :param reprocess (bool): boolean telling if we need to reprocess the image or not
        """
        self.file_manager.next_frame()
        fileName = self.file_manager.current_image_name
        self.navFit.filenameLineEdit.setText(fileName)
        self.current_image_data = self.workspace.create_image_data(
            self.file_manager.current_image, fileName
        )
        self.bioImg = EquatorImage(self.current_image_data, self)
        if reprocess:
            self.refreshProcessingParams()
        self.bioImg.skeletalVarsNotSet = not ('isSkeletal' in self.bioImg.info and self.bioImg.info['isSkeletal'])
        self.bioImg.extraPeakVarsNotSet = not ('isExtraPeak' in self.bioImg.info and self.bioImg.info['isExtraPeak'])
        settings = None
        # if len(self.bioImg.info) < 2: # use settings of the previous image
        settings = self.getSettings()
        # nPeaks = settings['nPeaks'] if 'nPeaks' in settings else None
        # isSkeletal = settings['isSkeletal'] if 'isSkeletal' in settings else None
        # isExtraPeak = settings['isExtraPeak'] if 'isExtraPeak' in settings else None
        # settings.update(self.bioImg.info)

        # if nPeaks is not None:
        #     settings['nPeaks'] = nPeaks
        # if isSkeletal is not None:
        #     settings['isSkeletal'] = isSkeletal
        # if isExtraPeak is not None:
        #     settings['isExtraPeak'] = isExtraPeak
        self.initWidgets(settings)
        self.img_zoom = None
        self.refreshStatusbar()
        self.refitting()

    def nextClicked(self):
        """
        Going to the next image
        """
        self.file_manager.next_frame()
        self.onImageChanged()

    def prevFileClicked(self):
        """
        Going to the previous h5 file
        """
        self.file_manager.prev_file
        self.onImageChanged()

    def nextFileClicked(self):
        """
        Going to the next h5 file
        """
        self.file_manager.next_file
        self.onImageChanged()

    def setH5Mode(self):
        """
        Sets the H5 list of file and displays the right set of buttons depending on the file selected
        """
        if self.file_manager.current_file_type == 'h5':
            self.navImg.nextFileButton.show()
            self.navImg.prevFileButton.show()
            self.navFit.nextFileButton.show()
            self.navFit.prevFileButton.show()
            self.navImg.processH5Button.show()
            self.navFit.processH5Button.show()

        else:
            self.navImg.nextFileButton.hide()
            self.navImg.prevFileButton.hide()
            self.navFit.nextFileButton.hide()
            self.navFit.prevFileButton.hide()
            self.navImg.processH5Button.hide()
            self.navFit.processH5Button.hide()


    def fileNameChanged(self):
        """
        Triggered when the name of the current file is changed
        """
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            fileName = str(self.navImg.filenameLineEdit.text()).strip()
        elif selected_tab == 1:
            fileName = str(self.navFit.filenameLineEdit.text()).strip()
        if fileName not in self.file_manager.names:
            return
        self.file_manager.switch_image_by_name(fileName)
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
        elif key == Qt.Key_Q:
            self.close()
        elif key == Qt.Key_N:
            self.browseFile()
        elif key == Qt.Key_F:
            self.processFolder()
        elif key == Qt.Key_O:
            self.browseFile()

    def setRminClicked(self):
        """
        Prepare for manual R-min setting
        """
        if self.setRminB.isChecked():
            self.setLeftStatus("Please select R-min size (ESC to cancel)")
            ax = self.displayImgAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.displayImgCanvas.draw_idle()
            self.function = ["rmin"]  # set current active function
        else:
            self.resetUI()
    
    def setRmaxClicked(self):
        """
        Prepare for manual R-max setting
        """
        if self.setRmaxB.isChecked():
            self.setLeftStatus("Please select R-max size (ESC to cancel)")
            ax = self.displayImgAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.displayImgCanvas.draw_idle()
            self.function = ["rmax"]  # set current active function
        else:
            self.resetUI()

    def brightSpotClicked(self):
        """
        Find orientation along the brightest spots and route results through workspace.
        """
        if self.brightSpot.isChecked():
            self.setLeftStatus("Finding orientation from brightest spots...")
            ax = self.displayImgAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.displayImgCanvas.draw_idle()

            xc, yc = self.bioImg.center
            im = copy.copy(self.bioImg.getRotatedImage())

            coordinates, m, b = self.fitb(im, xc, yc)

            ax.scatter(coordinates[:,1], coordinates[:,0], marker='o')
            fit_eq = m * coordinates[:,1] + b
            ax.plot(coordinates[:,1], fit_eq, color='r')
            fit_eq1 = m * coordinates[:,1] + b + 23 * math.sqrt(1 + m**2)
            fit_eq2 = m * coordinates[:,1] + b - 23 * math.sqrt(1 + m**2)
            ax.plot(coordinates[:,1], fit_eq1, color='r')
            ax.plot(coordinates[:,1], fit_eq2, color='r')

            self.displayImgCanvas.draw_idle()
            for i in range(0, im.shape[0]):
                for j in range(0, im.shape[1]):
                    if m * j - i + b + 23 * math.sqrt(1 + m**2) <= 0:
                        im[i][j] = 0
                    elif m * j - i + b + 50 * math.sqrt(1 + m**2) > 0 and m * j - i + b - 50 * math.sqrt(1 + m**2) < 0:
                        pass
                    else:
                        im[i][j] = 0

            _, new_center = processImageForIntCenter(im, getCenter(im))
            new_angle = math.degrees(math.atan(m))

            filename = self.file_manager.current_image_name
            self.workspace.set_center_from_source(filename, new_center, "bright_spot")
            self.workspace.set_rotation_from_source(filename, new_angle, "bright_spot")

            self.brightSpot.setChecked(False)
            self.processImage()

        else:
            self.resetUI()

    def setIntAreaClicked(self):
        """
        Prepare for manual integrated area (Box width) setting
        """
        if self.setIntAreaB.isChecked():
            self.setLeftStatus("Please select Integrated area by select start line and end line (ESC to cancel)")
            ax = self.displayImgAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
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
    
    def fixedRmaxChecked(self):
        """
        Triggered when fixed R-max is checked or unchecked
        """
        self.fixedRmax.setEnabled(self.fixedRmaxChkBx.isChecked())
        if not self.fixedRmaxChkBx.isChecked() and self.bioImg is not None:
            self.bioImg.removeInfo("fixed_rmax")
            self.bioImg.removeInfo("rmax")
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
        print("Function executed", flush=True)

        if self.bioImg is not None:

            modeOrientation = self.getModeRotation()
            if modeOrientation is not None:
                if not self.modeAngleChkBx.isChecked():
                    self.bioImg.removeInfo('rmin')  # Remove R-min from info dict to make it be re-calculated
                    self.bioImg.removeInfo("mode_angle")
                    self.processImage()
                else:
                    self.bioImg.removeInfo('rmin')  # Remove R-min from info dict to make it be re-calculated
                    self.bioImg.info["mode_angle"] = modeOrientation
                    self.processImage()
            else:
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
        if self.modeOrientation is not None:
            return self.modeOrientation
        print("Calculating mode of angles of images in directory")
        angles = []
        for idx, filename in enumerate(self.file_manager.names):
            img = self.file_manager.get_image_by_index(idx)
            image_data = self.workspace.create_image_data(img, filename)
            bioImg = EquatorImage(image_data, self)
            print(f'Getting angle {filename}')

            angle = bioImg.rotation
            if angle == 0.0 and not bioImg._image_data.has_manual_rotation and 'rotationAngle' not in bioImg.info:
                return None
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

    def fixedRmaxChanged(self):
        """
        Triggered when fixed R-max spinbox value is changed
        """
        if self.bioImg is not None and not self.syncUI:
            self.log_changes('fixedRmax', obj=self.fixedRmax)
            self.bioImg.info['fixed_rmax'] = self.fixedRmax.value()
            self.processImage()

    def orientationModelChanged(self):
        """
        Use when the orientation model is changed in the GUI
        """
        self.orientationModel = self.orientationCmbBx.currentIndex()
        self.bioImg.removeInfo('rotationAngle')
        self.processImage()

    def rotation90Checked(self):
        """
        Triggered when the rotation 90 degrees is checked
        """
        self.bioImg.removeInfo('rmin')
        self.processImage()

    def forceRot90Checked(self):
        """
        Force the rotation of 90 degrees
        """
        if self.forceRot90ChkBx.isChecked():
            self.rotation90ChkBx.setChecked(True)
            self.rotation90ChkBx.setEnabled(False)
        else:
            self.rotation90ChkBx.setEnabled(True)

    def inrec(self, x, y, xmin, ymin, xmax, ymax):
        """
        inrec
        """
        if x < xmin:
            return False
        elif x > xmax:
            return False
        elif y < ymin:
            return False
        elif y > ymax:
            return False
        else:
            return True

    def rec(self, cx, cy, coordinates):
        """
        rec
        """
        xmin = cx - 300
        xmax = cx + 300
        ymin = cy - 300
        ymax = cy + 300
        deletep = []
        for (i, coord) in enumerate(coordinates):
            x = coord[0]
            y = coord[1]
            if not self.inrec(x, y, xmin, ymin, xmax, ymax):
                deletep.append(i)
        return np.delete(coordinates, deletep, 0)

    def deleteOutlier(self, coordinates):
        """
        Delete the outliers for the coordinates given
        :param coordinates
        """
        lold = coordinates.shape[0]
        lnew = 0
        y = coordinates[:,0]
        x = coordinates[:,1]

        while lnew != lold:
            lold = lnew
            m, b = np.polyfit(x, y, 1)
            dist = []
            for (i, _) in enumerate(x):
                dist.append(abs(m*x[i] - y[i] + b) / math.sqrt(m**2 + 1))
            dist = np.array(dist)
            std = np.std(dist)
            avg = np.mean(dist)
            lower = avg - 2*std
            upper = avg + 2*std

            indx = []
            for (i, d) in enumerate(dist):
                if d > upper or d < lower or d > 2*avg:
                    indx.append(i)
            x = np.delete(x, indx)
            y = np.delete(y, indx)
            lnew = len(x)
        return x, y, m, b

    def fitb(self, im, xc, yc):
        """
        fitb
        """
        coordinates = peak_local_max(im, min_distance=10, threshold_rel=0.5)
        coordinates = self.rec(xc, yc, coordinates)
        l = 20
        if coordinates.shape[0] < 20:
            l = coordinates.shape[0]
        coordinates = coordinates[0:l, :]
        x, y, m, b = self.deleteOutlier(coordinates)
        coordinates = np.stack((y, x), axis=1)
        return coordinates, m, b

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

        elif self.doubleZoomGUI.doubleZoomMode:
            self.doubleZoomGUI.mouseClickBehavior(x, y)
            return

        if self.doubleZoom.isChecked() and not self.doubleZoomGUI.doubleZoomMode:
            x, y = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
            self.doubleZoomGUI.doubleZoomMode = True

        func = self.function
        # Provide different behavior depending on current active function
        if func is None:
            self.function = ["im_move", (x, y)]
        elif func[0] == "int_area":
            # draw 2 horizontal lines
            func.append(y)
            ax = self.displayImgAxes
            ax.axhline(y=y, color='y')
            self.displayImgCanvas.draw_idle()
            if len(func) == 3:
                int_area = [int(round(func[1])), int(round(func[2]))]
                min_t = min(int_area)
                b = max(int_area)
                # Set new integrated area, re-calculate from getting histogram process
                self.bioImg.info['int_area'] = (min_t, b)
                self.log_changes('intArea', varName='int_area', newValue=(min_t, b))
                self.bioImg.removeInfo('hist')
                if self.fixedIntAreaChkBx.isChecked():
                    self.fixedIntArea = (min_t, b)
                self.function = None
                self.setIntAreaB.setChecked(False)
                self.processImage()
        elif func[0] == "rmin":
            # Set new R-min, re-calculate from getting integrated area process
            self.bioImg.info['rmin'] = int(np.round(distance(self.bioImg.center, (x, y))))
            self.fixedRmin.setValue(self.bioImg.info['rmin'])
            self.log_changes('Rmin', obj=self.fixedRmin)
            self.bioImg.removeInfo('int_area')
            self.function = None
            self.setRminB.setChecked(False)
            self.processImage()
        elif func[0] == "rmax":
            # Set new R-min, re-calculate from getting integrated area process
            self.bioImg.info['rmax'] = int(np.round(distance(self.bioImg.center, (x, y))))
            self.fixedRmax.setValue(self.bioImg.info['rmax'])
            self.log_changes('Rmax', obj=self.fixedRmax)
            self.bioImg.removeInfo('hulls')
            self.function = None
            self.setRmaxB.setChecked(False)
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

        # img = self.bioImg.getRotatedImage()
        img = self.bioImg.image

        ax = self.displayImgAxes

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:

            if self.doubleZoomGUI.doubleZoomMode:
                self.doubleZoomGUI.beginImgMotion(x, y, len(img[0]), len(img), (0,0), self.displayImgAxes)

            x = int(round(x))
            y = int(round(y))
            unit = "px"
            calSettings = self.workspace.calibration_settings
            if calSettings is not None and 'scale' in calSettings:
                if 'center' in calSettings and calSettings['center'] is not None:
                    center = calSettings['center']
                else:
                    center = (self.bioImg.info['centerx'], self.bioImg.info['centery'])
                mouse_distance = np.sqrt((center[0] - x) ** 2 + (center[1] - y) ** 2)
                scale = calSettings['scale']
                d = mouse_distance / scale
                if (d > 0.01):
                    q = 1.0/d
                    unit = "nm^-1"
                else:
                    q = mouse_distance
        
                q = f"{q:.4f}"
            if img is not None and img.shape is not None and x < img.shape[1] and y < img.shape[0]:
                if calSettings is not None and 'scale' in calSettings:
                    self.pixel_detail.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x])+ ", distance=" + str(q) + unit)
                else:
                    mouse_distance = np.sqrt((self.bioImg.center[0] - x) ** 2 + (self.bioImg.center[1] - y) ** 2)
                    mouse_distance = f"{mouse_distance:.4f}"
                    self.pixel_detail.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]) + ", distance=" + str(mouse_distance) + unit)
                if self.doubleZoom.isChecked() and self.doubleZoomGUI.doubleZoomMode and x > 10 and x < img.shape[1]-10 and y > 10 and y < img.shape[0]-10:
                    ax1 = self.doubleZoomGUI.axes
                    imgCropped = img[y - 10:y + 10, x - 10:x + 10]
                    if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                        imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                        self.doubleZoomGUI.doubleZoomPoint = (x,y)
                        ax1.imshow(imgScaled)
                        if len(ax1.lines) > 0:
                            for i in range(len(ax1.lines)-1,-1,-1):
                                ax1.lines[i].remove()
                        for i in range(len(ax1.patches)-1,-1,-1):
                            ax1.patches[i].remove()
                        self.displayImgCanvas.draw_idle()

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

            if len(func) == 1 and self.doubleZoom.isChecked():
                if not self.doubleZoomGUI.doubleZoomMode:
                    self.doubleZoomGUI.updateAxes(x, y)
            elif len(func) == 2:
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
            self.displayImgCanvas.draw_idle()

        elif func[0] == "int_area":
            # draw horizontal lines
            ax = self.displayImgAxes
            if not self.doubleZoom.isChecked() or self.doubleZoomGUI.doubleZoomMode:
                if len(ax.lines) > len(func) - 1:
                    # line = ax.lines[:len(func) - 1]
                    for i in range(len(ax.lines)-1, len(func) - 2,-1):
                        if ax.lines[i].get_label() != "Blue Dot":
                            ax.lines[i].remove()
                    # ax.lines = line
                ax.axhline(y, color='g')
            else: 
                if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                    self.doubleZoomGUI.updateAxesInner(x, y)
                elif self.doubleZoomGUI.doubleZoomMode:
                    if len(ax.lines) > len(func) - 1:
                        # line = ax.lines[:len(func) - 1]
                        for i in range(len(ax.lines)-1, len(func) - 2,-1):
                            ax.lines[i].remove()
                        # ax.lines = line
                    ax.axhline(y, color='g')
            self.displayImgCanvas.draw_idle()
        elif func[0] in ("rmin", "rmax"):
            # draw R-min circle
            center = self.bioImg.center
            dis = int(np.round(distance(center, (x, y))))
            ax = self.displayImgAxes
            if not self.doubleZoom.isChecked():
                for i in range(len(ax.patches)-1,-1,-1):
                    ax.patches[i].remove()
                ax.add_patch(
                    patches.Circle(tuple(center), dis, linewidth=2, edgecolor='r', facecolor='none', linestyle='dotted'))
            else: 
                if (not self.doubleZoomGUI.doubleZoomMode) and x < 200 and y < 200:
                    self.doubleZoomGUI.updateAxesInner(x, y)
                elif self.doubleZoomGUI.doubleZoomMode:
                    for i in range(len(ax.patches)-1,-1,-1):
                        ax.patches[i].remove()
                    ax.add_patch(
                        patches.Circle(tuple(center), dis, linewidth=2, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.displayImgCanvas.draw_idle()
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

        # self.doubleZoomCanvas.draw_idle()

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
        self.zoomOutClicked = True
        self.default_img_zoom = None
        self.img_zoom = None
        self.updateImage()

    def closeEvent(self, ev):
        """
        Trigger when window is closed
        """
        if hasattr(self, 'taskManager') and self.taskManager.get_running_count() > 0:
            # Show confirmation dialog
            reply = QMessageBox.question(
                self,
                'Confirm Close',
                'Tasks are currently running. Are you sure you want to close and stop all tasks?',
                QMessageBox.Yes | QMessageBox.No,
                QMessageBox.No
            )
            
            if reply == QMessageBox.Yes:
                ev.ignore()
                if not getattr(self, '_closingAfterStop', False):
                    self._closingAfterStop = True
                    self.stopProcess()
            else:
                ev.ignore()
            return

        
        
        # delete window object from main window
        if self.logger is not None:
            self.logger.popup()
            self.logger.close()

        if not getattr(self, "_notifiedParentClosed", False):
            self._notifiedParentClosed = True
            self.mainWindow.childWindowClosed(self)
        
        super().closeEvent(ev)

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
            self.extraPeakChkBx.setEnabled(info['isSkeletal'])

        if 'isExtraPeak' in info:
            self.extraPeakChkBx.setChecked(info['isExtraPeak'])

        self.maskThresSpnBx.setValue(info['mask_thres'])

        if 'fit_results' in info:
            fit_results = info['fit_results']
            self.modelSelect.setCurrentIndex(self.modelSelect.findText(fit_results["model"]))
            self.k_spnbx.setValue(fit_results['k'])

        # init bias k
        if 'fix_k' in info:
            self.k_chkbx.setChecked(True)
            self.k_spnbx.setValue(info['fix_k'])
        else:
            self.k_chkbx.setChecked(False)
            if 'paramInfo' in info:
                self.k_spnbx.setValue(info['paramInfo']['k']['val'])

        if 'nPeaks' in info:
            self.nPeakSpnBx.setValue(info['nPeaks'])

        # Initital reject check box
        if "reject" in info.keys():
            self.rejectChkBx.setChecked(info["reject"])
        else:
            self.rejectChkBx.setChecked(False)

        # NOTE: blank_mask checkbox now handled by ProcessingWorkspace

        if 'fixed_angle' in info:
            self.fixedAngle.setValue(info['fixed_angle'])
        self.fixedAngleChkBx.setChecked('fixed_angle' in info)
        self.fixedAngle.setEnabled('fixed_angle' in info)

        self.fixedRminChkBx.setChecked('fixed_rmin' in info)
        self.fixedRmin.setEnabled('fixed_rmin' in info)
        if 'fixed_rmin' in info:
            self.fixedRmin.setValue(info['fixed_rmin'])

        self.fixedRmaxChkBx.setChecked('fixed_rmax' in info)
        self.fixedRmax.setEnabled('fixed_rmax' in info)
        if 'fixed_rmax' in info:
            self.fixedRmax.setValue(info['fixed_rmax'])


        if self.rotation90ChkBx.isEnabled():
            self.rotation90ChkBx.setChecked('90rotation' in info and info['90rotation'])

        self.syncUI = False

    def onImageChanged(self, first_run=False):
        """
        Called when image changes (including initial load).
        Creates a new EquatorImage object and processes it.
        
        :param first_run: True if this is the initial image load in __init__
        """
        # Skip refitting check on first run
        if not first_run and self.fixedFittingParamChanged(self.getSettings()):
            print("Refitting current image first")
            self.refitting()

        # Update UI with current filename
        fileName = self.file_manager.current_image_name
        self.navFit.filenameLineEdit.setText(fileName)

        # Create ImageData if not already provided (e.g., from _on_image_data_ready)
        if self.current_image_data is None or self.current_image_data.img_name != fileName:
            self.current_image_data = self.workspace.create_image_data(
                self.file_manager.current_image, fileName
            )
        
        # Create EquatorImage with ImageData
        self.bioImg = EquatorImage(self.current_image_data, self)
        self.bioImg.skeletalVarsNotSet = not ('isSkeletal' in self.bioImg.info and self.bioImg.info['isSkeletal'])
        self.bioImg.extraPeakVarsNotSet = not ('isExtraPeak' in self.bioImg.info and self.bioImg.info['isExtraPeak'])
        
        self.workspace.update_display(self.current_image_data)
        # First-run specific setup
        if first_run:
            if 'paramInfo' in self.bioImg.info:
                self.k_chkbx.setChecked(self.bioImg.info['paramInfo']['k']['fixed'])
        # Prepare settings
        if first_run:
            settings = self.getSettings(first_run=(True if 'model' not in self.bioImg.info else False))
            settings.update(self.bioImg.info)
        elif len(self.bioImg.info) < 2:
            settings = self.getSettings()
            settings.update(self.bioImg.info)
        else:
            settings = self.bioImg.info
        
        # Initialize UI widgets
        self.initWidgets(settings)
        self.img_zoom = None
        self.refreshStatusbar()
        
        # First-run specific setup
        if first_run:
            self.workspace.show_calibration_dialog(self.workspace._current_image_data, force=False)
            self.setH5Mode()
            self.initProcessExecutor()
            self.uiUpdateTimer.start()
        
        # Process image
        if not first_run and self.use_previous_fit_chkbx.isChecked():
            print("Using previous fit")
            ret = self.updateFittingParamsInParamInfo()
            if ret == -1:
                return
            self.processImage(self.bioImg.info['paramInfo'])
        else:
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

        # Check fixed rmax
        if self.fixedRmaxChkBx.isChecked() and self.paramChanged(prevInfo, currentInfo, 'rmax'):
            self.bioImg.removeInfo('hulls')  # Remove hulls from info dict to make it be re-calculated
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

        if self.brightSpot.isChecked() and self.paramChanged(prevInfo,currentInfo,'rotationAngle'):
            self.bioImg.removeInfo('rotationAngle')
            self.bioImg.removeInfo('rmin')
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

        if self.paramChanged(prevInfo, currentInfo, 'isExtraPeak'):
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
                if fitting_tab.fixedGammaZ.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_gammaz'):
                    return True
            
            if fitting_tab.parent.extraPeakChkBx.isChecked():
                if fitting_tab.fixedIntZEP.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_intz_EP'):
                    return True
                if fitting_tab.fixedSigZEP.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_sigz_EP'):
                    return True
                if fitting_tab.fixedZlineEP.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_zline_EP'):
                    return True
                if fitting_tab.fixedGammaZEP.isChecked() and self.paramChanged(prevInfo, currentInfo, side + '_fix_gammaz_EP'):
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
        self.tabWidget.tabBar().setEnabled(False)
        self.tabWidget.tabBar().setToolTip("Tab switching is disabled while processing")
        QApplication.setOverrideCursor(Qt.WaitCursor)
        QApplication.processEvents()
        settings = self.getSettings()
        print("Settings in processImage:")
        print(settings)
        try:
            
            self.addTask(paramInfo)

        except Exception:
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

        
    def addTask(self, paramInfo=None):
        self.tasksQueue.put((self.bioImg, self.getSettings(), paramInfo))
        
        # If there's no task currently running, start the next task
        self.startNextTask()
            
    def thread_done(self, bioImg):
        self.tasksDone += 1
        self.progressBar.setValue(100. / len(self.file_manager.names) * self.tasksDone)
        # Store finished image for onProcessingFinished; do not switch context here
        self._finishedBioImg = bioImg
        print("thread done")
                    
    def startNextTask(self):
        # Launch up to a safe concurrency limit to keep UI responsive
        limit = max(1, self.threadPool.maxThreadCount() // 2)
        started_any = False
        while not self.tasksQueue.empty() and self.threadPool.activeThreadCount() < limit:
            print("starting new task")
            bioImg, settings, paramInfo = self.tasksQueue.get()

            if settings.get('find_oritation'):
                self.brightSpotClicked()

            worker = Worker(bioImg, settings, paramInfo)
            worker.signals.result.connect(self.thread_done)
            worker.signals.finished.connect(self.onProcessingFinished)
            self.threadPool.start(worker)
            self.currentTask = worker
            started_any = True

        if not started_any and self.tasksQueue.empty() and self.threadPool.activeThreadCount() == 0:
            self.progressBar.setVisible(False)
        
    def onProcessingFinished(self, finishedImg):
        # Temporarily switch context to the finished image to update outputs
        prevBio = self.bioImg
        self.bioImg = finishedImg
        self.updateParams()
        self.csvManager.writeNewData(finishedImg)
        self.csvManager.writeNewData2(finishedImg)
        self.resetUI()
        self.refreshStatusbar()
        QApplication.restoreOverrideCursor()
        self.tabWidget.tabBar().setEnabled(True)
        self.tabWidget.tabBar().setToolTip("")
        self.currentTask = None
        # Restore previous reference so batch refitting continues to target the UI's current file
        self.bioImg = prevBio
        if self.first:
            self.init_logging()
            self.first = False
        else:
            self.startNextTask()

    def _onFutureDone(self, future):
        """Dispatch onImageProcessed to main thread safely."""
        QTimer.singleShot(0, self, lambda: self.onImageProcessed(future))

    def onImageProcessed(self, future):
        """
        Callback when image processing completes.
        Runs in main thread via Qt's callback mechanism.
        """
        try:
            # Retrieve result from future
            result = future.result()
            error = result.get('error')
            
            # Organize result via task manager
            task = self.taskManager.complete_task(future, result, error)
            
            if not task:
                return
            
            if error:
                print(f"Error processing {task.filename}: {error}")
            else:
                # Save results to disk (main thread only)
                self._organizeAndSaveResult(task)
            
            # Queue UI update (will be processed in order)
            self.pendingUIUpdates[task.job_index] = task
            
        except Exception as e:
            print(f"Callback error: {e}")
            traceback.print_exc()
    
    def _organizeAndSaveResult(self, task):
        """
        Organize and persist processing results.
        ALWAYS runs in main thread - safe for file I/O.
        """
        result = task.result
        filename = task.filename
        
        # Get the correct image for this task
        img = self.file_manager.get_image_by_index(task.job_index)
        
        # Create ImageData and EquatorImage with correct filename from task
        image_data = self.workspace.create_image_data(img, filename)
        bioImg = EquatorImage(image_data, self)
        bioImg.info = result['info']
        
        # Write cache (main thread only)
        try:
            bioImg.saveCache()
        except Exception as e:
            print(f"Failed to save cache for {filename}: {e}")
        
        # Write CSV (main thread only)
        try:
            self.csvManager.writeNewData(bioImg)
            self.csvManager.writeNewData2(bioImg)
        except Exception as e:
            print(f"Failed to write CSV for {filename}: {e}")
        
        # Explicitly delete large objects to ensure immediate memory release
        del img
        del bioImg
        
        # Force garbage collection every 10 images to prevent memory accumulation
        stats = self.taskManager.get_statistics()
        if stats['completed'] % 10 == 0:
            import gc
            collected = gc.collect()
            print(f"[GC] Collected {collected} objects after {stats['completed']} images")
        
        print(f"✓ Completed {filename} in {task.processing_time:.2f}s")
    
    def _processImageFallback(self, paramInfo=None):
        """Fallback to thread-based processing if multiprocessing fails"""
        QApplication.setOverrideCursor(Qt.WaitCursor)
        QApplication.processEvents()
        settings = self.getSettings()
        
        try:
            self.bioImg.process(settings, paramInfo)
            self.updateParams()
            self.csvManager.writeNewData(self.bioImg)
            self.csvManager.writeNewData2(self.bioImg)
            self.resetUI()
            self.refreshStatusbar()
        except Exception as e:
            QApplication.restoreOverrideCursor()
            errMsg = QMessageBox()
            errMsg.setText('Unexpected error')
            errMsg.setInformativeText(str(e))
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
            raise
        finally:
            QApplication.restoreOverrideCursor()
            self.tabWidget.tabBar().setEnabled(True)

    def setLeftStatus(self, s):
        """
        Set text on status bar on the left
        :param s: input text (str)
        """
        self.left_status.setText(s)
        QApplication.processEvents()

    def updateParams(self):
        """
        Update the parameters
        """
        info = self.bioImg.info
        if 'orientation_model' in info:
            self.orientationModel = info['orientation_model']
        if not self.zoomOutClicked and self.bioImg.quadrant_folded:
            cx, cy = self.bioImg.center
            xlim, ylim = self.bioImg.initialImgDim
            xlim, ylim = int(xlim/2), int(ylim/2)
            self.default_img_zoom = [(cx-xlim, cx+xlim), (cy-ylim, cy+ylim)]

    def refreshStatusbar(self):
        """
        Set Left status bar to be image detail
        Set Right status bar to by image shape and type
        Clear pixel detail
        """
        if self.bioImg is None:
            return
        total = str(len(self.file_manager.names)) + ('*' if self._provisionalCount else '')
        self.setLeftStatus(
            "(" + str(self.file_manager.current + 1) + "/" + total + ") " + fullPath(self.dir_path,
                                                                                            self.bioImg.filename))
        img = self.bioImg.orig_img
        self.right_status.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype))
        self.pixel_detail.setText("")
        QApplication.processEvents()

    def getSettings(self, first_run=False):
        """
        Get all settings for EquatorImage process() from widgets
        :return: settings (dict)
        """
        settings = {}
        settings.update(self.left_fitting_tab.getFittingSettings(first_run))
        settings.update(self.right_fitting_tab.getFittingSettings(first_run))

        settings['orientation_model'] = self.orientationModel
        settings['nPeaks'] = self.nPeakSpnBx.value()
        settings['model'] = str(self.modelSelect.currentText())
        settings['isSkeletal'] = self.skeletalChkBx.isChecked()
        settings['isExtraPeak'] = self.extraPeakChkBx.isChecked()
        settings['mask_thres'] = self.maskThresSpnBx.value()
        settings['90rotation'] = self.rotation90ChkBx.isChecked()

        calSettings = self.workspace.calibration_settings
        if calSettings is not None:
            if 'type' in calSettings:
                if calSettings["type"] == "img":
                    settings["lambda_sdd"] = calSettings["silverB"] * calSettings["radius"]
                else:
                    settings["lambda_sdd"] = 1. * calSettings["lambda"] * calSettings["sdd"] / calSettings["pixel_size"]
            if "center" in calSettings:
                settings["calib_center"] = calSettings["center"]
            if "detector" in calSettings:
                settings["detector"] = calSettings["detector"]

        if self.fixedAngleChkBx.isChecked():
            settings['fixed_angle'] = self.fixedAngle.value()

        if self.brightSpot.isChecked():
            settings['find_oritation']=True
        else:
            settings['find_oritation']=False

        if self.fixedRminChkBx.isChecked():
            settings['fixed_rmin'] = self.fixedRmin.value()

        if self.fixedRmaxChkBx.isChecked():
            settings['fixed_rmax'] = self.fixedRmax.value()


        if self.fixedIntAreaChkBx.isChecked() and self.fixedIntArea is not None:
            settings["fixed_int_area"] = self.fixedIntArea

        if self.modeAngleChkBx.isChecked():
            modeOrientation = self.getModeRotation()
            if modeOrientation is not None:
                settings["mode_angle"] = modeOrientation

        # Get blank/mask config from workspace
        blank_mask_config = self.workspace.get_blank_mask_config()
        settings['blank_mask'] = blank_mask_config['apply_blank'] or blank_mask_config['apply_mask']

        if self.k_chkbx.isChecked():
            settings['fix_k'] = self.k_spnbx.value()

        return settings

    def refreshGraph(self):
        """
        Refresht the graph displayed by updating the UI
        """
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
            # print(b.text())
            b.setChecked(False)  
        
        for k in self.update_plot:
            self.update_plot[k] = True

        # change seleck peak button's text
        self.setPeaksB.setText("Start Manual Peak Selection")

        self.updateUI()


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
        self.fixedAngle.setValue(round(info["rotationAngle"]))
        self.fixedRmin.setValue(info['rmin'])
        if self.fixedRmaxChkBx.isChecked():
            self.fixedRmax.setValue(info['rmax'])

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

        QApplication.processEvents()

    def updateImageTab(self):
        """
        Draw all UI in image tab
        """
        info = copy.copy(self.bioImg.info)

        img = self.bioImg.getRotatedImage()
        hulls = info['hulls']['all']
        center = self.bioImg.center
        rmin = info['rmin']
        int_area = info['int_area']

        ax = self.displayImgAxes
        ax.cla()
        self.image_viewer.display_image(img)


        self.orientationCmbBx.setCurrentIndex(0 if self.orientationModel is None else self.orientationModel)
        if self.rotation90ChkBx.isEnabled():
            self.rotation90ChkBx.setChecked('90rotation' in info and info['90rotation'])

        if self.centerChkBx.isChecked():
            # Draw center
            ax.plot([center[0]], [center[1]], 'bo')

        if self.rminChkBx.isChecked():
            # Draw R-min
            ax.add_patch(
                patches.Circle(tuple(center), rmin, linewidth=2, edgecolor='r', facecolor='none', linestyle='dotted'))
        
        if self.rmaxChkBx.isChecked() and 'rmax' in info:
            # Draw R-max
            ax.add_patch(
                patches.Circle(tuple(center), info['rmax'], linewidth=2, edgecolor='orange', facecolor='none', linestyle='dotted'))

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
        elif self.default_img_zoom is not None and len(self.default_img_zoom) == 2:
            ax.set_xlim(self.default_img_zoom[0])
            ax.set_ylim(self.default_img_zoom[1])
        else:
            ax.set_xlim((0, img.shape[1]))
            ax.set_ylim((0, img.shape[0]))

        self.img_zoom = [ax.get_xlim(), ax.get_ylim()]
        ax.invert_yaxis()
        self.displayImgFigure.tight_layout()
        # ax.set_position([0.06, 0.06, 0.96, 0.96])  # left,bottom,width,height
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
        if self.gap_lines is not None:
            for line in self.gap_lines:
                line.remove()
            self.gap_lines.clear()
        if 'gaps' in self.bioImg.info and self.bioImg.info['gaps']:
            if self.gaps is not None and self.gaps:
                for rect in self.gaps:
                    rect.remove()
                self.gaps.clear()
            for gap in self.bioImg.info['gaps']:
                rect = patches.Rectangle((gap[0], 0), gap[1] - gap[0], 1, linewidth=1, edgecolor='r', facecolor='r', alpha=0.2, transform=ax.get_xaxis_transform())
                ax.add_patch(rect)
                self.gaps.append(rect)
        else:
            for rect in self.gaps:
                rect.remove()
            self.gaps.clear()
                    

        if 'fit_results' in info:
            fit_result = info['fit_results']

            if fit_result['isSkeletal'] and self.dispZlineChkBx.isChecked():
                # Draw z line
                ax.axvline(fit_result['centerX'] + fit_result['right_zline'], color='y', alpha=0.5)
                ax.axvline(fit_result['centerX'] - fit_result['left_zline'], color='y', alpha=0.5)
                if fit_result['isExtraPeak']:
                    ax.axvline(fit_result['centerX'] + fit_result['right_zline_EP'], color='y', alpha=0.5)
                    ax.axvline(fit_result['centerX'] - fit_result['left_zline_EP'], color='y', alpha=0.5)

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
        self.plot_min = -50
        if self.graph_zoom is not None and len(self.graph_zoom) == 2:
            ax.set_xlim(self.graph_zoom[0])
            ax.set_ylim(self.graph_zoom[1])
        # elif self.default_img_zoom is not None and len(self.default_img_zoom) == 2:
        #    ax.set_xlim(self.default_img_zoom[0])
        #    ax.set_ylim(self.default_img_zoom[1])
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
            if fit_results['fiterror'] > self.fittingErrorThreshold.value():
                genResults += " <b>(High Error)</b>"
        else:
            genResults +=  "<b>Model cannot be fit</b>"

        calSettings = self.workspace.calibration_settings
        if calSettings is not None:
            genResults += "<h2>Calibration Settings</h2>"
            if "center" in calSettings:
                genResults += "<b>Calibration Center : </b>" + str(calSettings["center"]) + '<br/><br/>'
            if 'type' in calSettings:
                if calSettings["type"] == "img":
                    genResults += "<b>S<sub>dd</sub> (in pixel) : </b>" + str(calSettings["radius"]) + '<br/><br/>'
                    genResults += "<b>Calibrant ring d-spacing : </b>" + str(calSettings["silverB"]) + '<br/><br/>'
                else:
                    genResults += "<b>Lambda : </b>" + str(calSettings["lambda"]) + '<br/><br/>'
                    genResults += "<b>S<sub>dd</sub> : </b>" + str(calSettings["sdd"]) + '<br/><br/>'
                    genResults += "<b>Pixel Size : </b>" + str(calSettings["pixel_size"]) + '<br/><br/>'

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

                if fit_results['isExtraPeak']:
                    # Display Z line center in table
                    self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Z line Extra Peak"))
                    self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_zline_EP'])))
                    self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_zline_EP'])))
                    ind += 1

                    # Display Z line sigma in table
                    self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Sigma Z Extra Peak"))
                    self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_sigmaz_EP'])))
                    self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_sigmaz_EP'])))
                    ind += 1

                    # Display Z line intensity in table
                    self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Iz Extra Peak"))
                    self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_intz_EP'])))
                    self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_intz_EP'])))
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

                    if fit_results['isExtraPeak']:
                        # Display gamma in table if model is Voigt
                        self.fiberResultTable.setItem(ind, 0, QTableWidgetItem("Gamma Z Extra Peak"))
                        self.fiberResultTable.setItem(ind, 1, QTableWidgetItem(str(fit_results['left_gammaz_EP'])))
                        self.fiberResultTable.setItem(ind, 2, QTableWidgetItem(str(fit_results['right_gammaz_EP'])))
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
                if not isinstance(v, bool) and isinstance(v, (float, int)):

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
        if item is None:
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
        """
        on row fixed
        """
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
            if table.cellWidget(row, 2) is not None:
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
                if c2 in ('True', 'False'):
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
        except Exception:
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
        self.csvManager.writeNewData2(self.bioImg)
        self.resetUI()
        self.refreshStatusbar()
        QApplication.restoreOverrideCursor()
        

    def init_logging(self):
        """
        Initialize the logging
        """
        for objName in self.editableVars:
            self.editableVars[objName] = self.findChild(QAbstractSpinBox, objName).value()
        self.editableVars['int_area'] = self.bioImg.info['int_area']
        self.editableVars['center'] = self.bioImg.center
        self.left_fitting_tab.init_logging()
        self.right_fitting_tab.init_logging()

    def write_log(self, msg):
        """
        Write the log
        """
        if self.logger is None:
            self.logger = logger.Logger('equator', self.bioImg.dir_path)
        img_name = os.path.join(os.path.split(self.bioImg.dir_path)[-1], self.bioImg.filename)
        self.logger.write(f'[{img_name}] {msg}')

    def log_changes(self, name, obj=None, varName='', newValue=None):
        """
        Change the log file and rewrite it
        """
        if obj is not None:
            varName = obj.objectName()
            newValue = obj.value()
        if self.editableVars[varName] == newValue:
            return
        self.write_log(f'{name}Changed: {self.editableVars[varName]} -> {newValue}')
        self.editableVars[varName] = newValue

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        self.statusReport.setText(text)
        QApplication.processEvents()
