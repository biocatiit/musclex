from .pyqt_utils import *
import matplotlib.patches as patches
from numpy import ma
from matplotlib import scale as mscale
from matplotlib import transforms as mtransforms
from matplotlib.colors import LogNorm, Normalize
import logging
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
from ..CalibrationSettings import CalibrationSettings
from ..csv_manager import CP_CSVManager
import musclex
from .BlankImageSettings import BlankImageSettings

class DSpacingScale(mscale.ScaleBase):
    name = 'dspacing'

    def __init__(self, axis, **kwargs):
        mscale.ScaleBase.__init__(self)
        self.lambda_sdd = kwargs.pop('lambda_sdd', 1501.45)

    def get_transform(self):
        return self.DSpacingTransform(self.lambda_sdd)

    def set_default_locators_and_formatters(self, axis):
        """
        Override to set up the locators and formatters to use with the
        scale.  This is only required if the scale requires custom
        locators and formatters.  Writing custom locators and
        formatters is rather outside the scope of this example, but
        there are many helpful examples in ``ticker.py``.

        In our case, the Mercator example uses a fixed locator from
        -90 to 90 degrees and a custom formatter class to put convert
        the radians to degrees and put a degree symbol after the
        value::
        """
        from matplotlib.ticker import Formatter, AutoLocator
        class DSpacingFormatter(Formatter):
            def __init__(self, lambda_sdd):
                Formatter.__init__(self)
                self.lambda_sdd = lambda_sdd
            def __call__(self, x, pos=None):
                if x == 0:
                    return u"\u221E"
                else:
                    return "%.2f" % (self.lambda_sdd / x)

        axis.set_major_locator(AutoLocator())
        axis.set_major_formatter(DSpacingFormatter(self.lambda_sdd))
        axis.set_minor_formatter(DSpacingFormatter(self.lambda_sdd))

    def limit_range_for_scale(self, vmin, vmax, minpos):
        """
        Override to limit the bounds of the axis to the domain of the
        transform.  In the case of Mercator, the bounds should be
        limited to the threshold that was passed in.  Unlike the
        autoscaling provided by the tick locators, this range limiting
        will always be adhered to, whether the axis range is set
        manually, determined automatically or changed through panning
        and zooming.
        """
        return max(vmin, 1), vmax

    class DSpacingTransform(mtransforms.Transform):
        # There are two value members that must be defined.
        # ``input_dims`` and ``output_dims`` specify number of input
        # dimensions and output dimensions to the transformation.
        # These are used by the transformation framework to do some
        # error checking and prevent incompatible transformations from
        # being connected together.  When defining transforms for a
        # scale, which are, by definition, separable and have only one
        # dimension, these members should always be set to 1.
        input_dims = 1
        output_dims = 1
        is_separable = True
        has_inverse = True

        def __init__(self, lambda_sdd):
            mtransforms.Transform.__init__(self)
            self.lambda_sdd = lambda_sdd

        def transform_non_affine(self, a):
            """
            This transform takes an Nx1 ``numpy`` array and returns a
            transformed copy.  Since the range of the Mercator scale
            is limited by the user-specified threshold, the input
            array must be masked to contain only valid values.
            ``matplotlib`` will handle masked arrays and remove the
            out-of-range data from the plot.  Importantly, the
            ``transform`` method *must* return an array that is the
            same shape as the input array, since these values need to
            remain synchronized with values in the other dimension.
            """
            masked = ma.masked_where(a <= 0, a)
            if masked.mask.any():
                return self.lambda_sdd / masked
            else:
                return self.lambda_sdd / a

        def inverted(self):
            """
            Override this method so matplotlib knows how to get the
            inverse transform for this transform.
            """
            return DSpacingScale.InvertedDSpacingTransform(
                self.lambda_sdd)

    class InvertedDSpacingTransform(mtransforms.Transform):
        input_dims = 1
        output_dims = 1
        is_separable = True
        has_inverse = True

        def __init__(self, lambda_sdd):
            mtransforms.Transform.__init__(self)
            self.lambda_sdd = lambda_sdd

        def transform_non_affine(self, a):
            masked = ma.masked_where(a <= 0, a)
            if masked.mask.any():
                return np.flipud(self.lambda_sdd / masked)
            else:
                return np.flipud(self.lambda_sdd / a)

        def inverted(self):
            return DSpacingScale.DSpacingTransform(self.lambda_sdd)

mscale.register_scale(DSpacingScale)

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
        self.ROI = None
        self.merged_peaks = None
        self.orientationModel = None
        self.in_batch_process = False

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
        self.setWindowTitle("Muscle X Scanning Diffraction v." + musclex.__version__)
        # self.setStyleSheet(getStyleSheet())
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
        self.displayImgFigure = plt.figure()
        self.displayImgAxes = self.displayImgFigure.add_subplot(111)
        self.displayImgCanvas = FigureCanvas(self.displayImgFigure)
        self.imageOptionsFrame = QFrame()
        self.imageOptionsFrame.setFixedWidth(400)
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
        self.logScaleIntChkBx = QCheckBox("Log scale intensity")
        self.intensityLayout.addWidget(self.minIntLabel, 0, 0)
        self.intensityLayout.addWidget(self.minInt, 0, 1)
        self.intensityLayout.addWidget(self.maxIntLabel, 1, 0)
        self.intensityLayout.addWidget(self.maxInt, 1, 1)
        self.intensityLayout.addWidget(self.logScaleIntChkBx, 2, 0, 1, 2)
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
        self.roiChkBx = QCheckBox("Display ROI")
        self.roiChkBx.setChecked(False)

        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet(pfss)
        self.processFolderButton.setCheckable(True)
        self.pnButtons = QGridLayout()
        self.prevButton = QPushButton('<')
        self.prevButton.clearFocus()
        self.nextButton = QPushButton('>')
        self.filenameLineEdit = QLineEdit()
        self.pnButtons.addWidget(self.processFolderButton, 0, 0, 1, 2)
        self.pnButtons.addWidget(self.prevButton, 1, 0, 1, 1)
        self.pnButtons.addWidget(self.nextButton, 1, 1, 1, 1)
        self.pnButtons.addWidget(self.filenameLineEdit, 2, 0, 1, 2)

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
        self.displayOptionsLayout.addSpacing(10)
        self.displayOptionsLayout.addWidget(self.roiChkBx)
        self.displayOptionGrp.setLayout(self.displayOptionsLayout)

        self.setCaliButton = QPushButton("Calibration Settings")
        self.setBlankImageButton = QPushButton("Blank Image and Mask")
        self.setHullRange = QPushButton("Set Fixed R-min and R-max")
        self.setHullRange.setCheckable(True)
        self.checkable_buttons.append(self.setHullRange)
        self.setRoiBtn = QPushButton("Set ROI")
        self.setRoiBtn.setCheckable(True)
        self.checkable_buttons.append(self.setRoiBtn)
        self.selectRings = QPushButton("Select Rings Manually")
        self.selectRings.setCheckable(True)
        self.persistRingsChkBx = QCheckBox("Persist Rings")
        self.checkable_buttons.append(self.selectRings)
        self.orientationCmbBx = QComboBox()
        self.orientationCmbBx.addItem("Max Intensity")
        self.orientationCmbBx.addItem("GMM2")
        self.orientationCmbBx.addItem("GMM3")
        self.orientationCmbBx.addItem("Herman Factor (Half Pi)")
        self.orientationCmbBx.addItem("Herman Factor (Pi)")
        self.orientationCmbBx.setCurrentIndex(2)
        self.persistROIChkBx = QCheckBox("Persist R-min, R-max & ROI")
        self.rotation90ChkBx = QCheckBox("Rotate 90")
        self.forceRot90ChkBx = QCheckBox("Persist Rotation")

        self.settingGrp = QGroupBox("Settings")
        self.settingLayout = QVBoxLayout(self.settingGrp)
        self.settingLayout.addWidget(self.setCaliButton)
        self.settingLayout.addWidget(self.setBlankImageButton)
        self.settingLayout.addWidget(self.setHullRange)
        self.settingLayout.addWidget(self.setRoiBtn)
        self.settingLayout.addWidget(self.selectRings)
        self.settingLayout.addWidget(self.persistRingsChkBx)
        self.settingLayout.addWidget(QLabel("Finding orientation:"))
        self.settingLayout.addWidget(self.orientationCmbBx)
        self.settingLayout.addWidget(self.persistROIChkBx)
        self.settingLayout.addWidget(self.rotation90ChkBx)
        self.settingLayout.addWidget(self.forceRot90ChkBx)

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

        self.m1_partial_hist_figure = plt.figure()
        self.m1_partial_hist_figure.subplots_adjust(top=0.90, bottom=0.20)
        self.m1_partial_hist_axes = self.m1_partial_hist_figure.add_subplot(111)
        self.m1_partial_hist_canvas = FigureCanvas(self.m1_partial_hist_figure)
        # self.toolbar = NavigationToolbar(self.m1_partial_hist_canvas, self)
        # self.method1TabLayout.addWidget(self.toolbar)
        self.m1_hist_figure = plt.figure()
        self.m1_hist_axes = self.m1_hist_figure.add_subplot(111)
        self.m1_hist_canvas = FigureCanvas(self.m1_hist_figure)
        self.m1_img_fig = plt.figure()
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

        self.m2_cent_diff_fig = plt.figure()
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

        self.dspacing_chkbx = QCheckBox("D-spacing")
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
        self.graph_options_layout.addWidget(self.dspacing_chkbx)
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
        self.result_graph_figure = plt.figure()
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
        self.logScaleIntChkBx.stateChanged.connect(self.updateUI)
        self.noBGImgChkBx.stateChanged.connect(self.updateUI)
        self.blankChkBx.stateChanged.connect(self.updateUI)
        self.angleChkBx.stateChanged.connect(self.updateUI)
        self.rminmaxChkBx.stateChanged.connect(self.updateUI)
        self.roiChkBx.stateChanged.connect(self.updateUI)
        self.setCaliButton.clicked.connect(self.calibrationClicked)
        self.setBlankImageButton.clicked.connect(self.setBlankAndMask)
        self.setHullRange.clicked.connect(self.setHullRangeClicked)
        self.setRoiBtn.clicked.connect(self.setRoiBtnClicked)
        self.selectRings.clicked.connect(self.selectRingsClicked)
        self.orientationCmbBx.currentIndexChanged.connect(self.orientationModelChanged)
        self.rotation90ChkBx.stateChanged.connect(self.rotation90Checked)
        self.forceRot90ChkBx.stateChanged.connect(self.forceRot90Checked)
        #self.processFolderButton.clicked.connect(self.processFolder)
        self.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.prevButton.clicked.connect(self.prevImage)
        self.nextButton.clicked.connect(self.nextImage)
        self.filenameLineEdit.editingFinished.connect(self.fileNameChanged)

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
        self.dspacing_chkbx.stateChanged.connect(self.updateUI)
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
        dialog = BlankImageSettings(self.filePath)
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
            self.processImage()
            self.updateImageTab()


    def setRoiBtnClicked(self):
        """
        Triggered when select ROI button pressed (Image tab)
        """
        if self.cirProj is None:
            return

        if self.setRoiBtn.isChecked():
            self.function = ['ROI']
            ax = self.displayImgAxes
            ax.lines = []
            ax.patches = []
            self.displayImgCanvas.draw_idle()
        else:
            self.function = None
            self.processImage()
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
            self.merged_peaks = self.cirProj.info['merged_peaks']
            self.selectRings.setText("Select Rings Manually")
            self.cirProj.removeInfo('fitResult')
            self.function = None
            self.processImage()

    def orientationModelChanged(self):
        self.orientationModel = str(self.orientationCmbBx.currentText())
        self.processImage()

    def rotation90Checked(self):
        self.cirProj.removeInfo('ring_hists')
        self.processImage()

    def forceRot90Checked(self):
        if self.forceRot90ChkBx.isChecked():
            self.rotation90ChkBx.setChecked(True)
            self.rotation90ChkBx.setEnabled(False)
        else:
            self.rotation90ChkBx.setEnabled(True)

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
        center = self.cirProj.info['center']
        dis = distance(center, (x, y))
        if func[0] == 'rings':
            # Add rings to list and draw circles
            ax = self.displayImgAxes
            ax.add_patch(
                patches.Circle(tuple(center), dis, linewidth=2, edgecolor='y', facecolor='none'))
            self.function.append(int(round(dis)))
            self.displayImgCanvas.draw_idle()
        elif func[0] == 'hull':
            self.function.append(int(round(dis)))
            if len(self.function) == 3:
                rs = self.function[1:]
                rmin = int(min(rs))
                rmax = int(max(rs))
                self.fixed_hull_range = (rmin, rmax)
                if self.ROI is None:
                    self.ROI = [rmin, rmax]
                else:
                    self.ROI[0] = max(rmin, self.ROI[0])
                    self.ROI[1] = min(rmax, self.ROI[1])
                self.cirProj.removeInfo('2dintegration')
                self.processImage()
            else:
                ax = self.displayImgAxes
                ax.add_patch(
                    patches.Circle(tuple(center), dis, linewidth=2, edgecolor='y', facecolor='none'))
                self.displayImgCanvas.draw_idle()
        elif func[0] == 'ROI':
            self.function.append(int(round(dis)))
            if len(self.function) == 3:
                rs = self.function[1:]
                innerR, outerR = min(rs), max(rs)
                innerR = max(innerR, self.cirProj.info['start_point'])
                outerR = min(outerR, self.cirProj.info['rmax'])
                self.cirProj.info['ROI'] = [innerR, outerR]
                self.ROI = [innerR, outerR]
                self.cirProj.removeInfo('ring_hists')
                self.cirProj.removeInfo('m1_rings')
                self.cirProj.removeInfo('m2_rings')
                self.cirProj.removeInfo('merged_peaks')
                self.processImage()
            else:
                ax = self.displayImgAxes
                ax.add_patch(
                    patches.Circle(tuple(center), dis, linewidth=2, edgecolor='r', facecolor='none'))
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
            text = "x={0}, y={1}, val={2}".format(round(x,2), round(y,2), round(orig_img[iy,ix],2))
            if self.selectRings.isChecked() or self.setHullRange.isChecked() or \
               self.setRoiBtn.isChecked():
                dist = distance(self.cirProj.info['center'], (x, y))
                text = "radius={0}, {1}".format(round(dist, 2), text)
            self.statusLabel.setText(text)

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

        if self.function is None or len(self.function) == 0:
            return

        func = self.function

        center = self.cirProj.info['center']
        dis = distance(center, (x, y))
        if func[0] == 'rings':
            # draw circle
            ax = self.displayImgAxes
            del ax.patches[(len(self.function) - 1):]
            ax.add_patch(
                patches.Circle(tuple(center), dis, linewidth=2, edgecolor='y', facecolor='none'))
            self.displayImgCanvas.draw_idle()
        elif func[0] == 'hull':
            # draw circle
            ax = self.displayImgAxes
            del ax.patches[(len(self.function) - 1):]
            ax.add_patch(
                patches.Circle(tuple(center), dis, linewidth=2, edgecolor='y', facecolor='none'))
            self.displayImgCanvas.draw_idle()
        elif func[0] == 'ROI':
            # draw circle
            ax = self.displayImgAxes
            del ax.patches[(len(self.function) - 1):]
            dis = min(max(dis, self.cirProj.info['start_point']), self.cirProj.info['rmax'])
            ax.add_patch(
                patches.Circle(tuple(center), dis, linewidth=2, edgecolor='r', facecolor='none'))
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

    def batchProcBtnToggled(self):
        if self.processFolderButton.isChecked():
            if not self.in_batch_process:
                self.processFolderButton.setText("Stop")
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
        flags = self.getFlags()
        text += "\nCurrent Settings"
        text += "\n - Partial integration angle range : "+ str(flags['partial_angle'])
        if 'orientation_model' in flags:
            text += "\n - Orientation Model : "+ flags['orientation_model']
        if 'ROI' in flags:
            text += "\n - ROI : "+ str(flags['ROI'])
        if 'fixed_hull' in flags:
            text += "\n - R-min & R-max : "+ str(flags['fixed_hull'])
        if self.calSettings is not None:
            if "center" in self.calSettings:
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
            self.in_batch_process = True
            self.stop_process = False
            for i in range(nImg):
                if self.stop_process:
                    break
                self.nextImage()
                self.progressBar.setValue(i)
                QApplication.processEvents()
            self.in_batch_process = False
            self.folder_processed = True
        else:
            self.folder_processed = False

        self.progressBar.setVisible(False)
        self.processFolderButton.setChecked(False)
        self.processFolderButton.setText("Process Current Folder")

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

    def getFlags(self, imgChanged=True):
        flags = {}
        flags['partial_angle'] = self.partialRange.value()
        if self.merged_peaks is not None and self.persistRingsChkBx.isChecked():
            print("Persisting rings at {}..".format(self.merged_peaks))
            flags['merged_peaks'] = self.merged_peaks
            flags['m1_rings'] = self.merged_peaks
            flags['m2_rings'] = self.merged_peaks
            flags['model_peaks'] = self.merged_peaks
            flags['persist_rings'] = True
        if self.ROI is not None and (self.persistROIChkBx.isChecked() or not imgChanged):
            flags['ROI'] = self.ROI
        if self.orientationModel is not None:
            flags['orientation_model'] = self.orientationModel
        flags['90rotation'] = self.rotation90ChkBx.isChecked()
        if self.calSettings is not None:
            if self.calSettings["type"] == "img":
                flags["center"] = self.calSettings["center"]
                flags["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
            else:
                flags["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings["pixel_size"]
                if "center" in self.calSettings:
                    flags["center"] = self.calSettings["center"]

        if self.fixed_hull_range is not None and (self.persistROIChkBx.isChecked() or not imgChanged):
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
        self.filenameLineEdit.setText(fileName)
        fileFullPath = fullPath(self.filePath, fileName)
        self.updateStatusBar(fileFullPath+' ('+str(self.currentFileNumber+1)+'/'+str(self.numberOfFiles)+') is processing ...')
        self.cirProj = ScanningDiffraction(self.filePath, fileName, logger=self.logger)
        self.setMinMaxIntensity(self.cirProj.original_image, self.minInt, self.maxInt, self.minIntLabel, self.maxIntLabel)
        if self.rotation90ChkBx.isEnabled():
            self.rotation90ChkBx.setChecked('90rotation' in self.cirProj.info and self.cirProj.info['90rotation'])
        self.processImage(True)
        self.updateStatusBar(fileFullPath + ' (' + str(self.currentFileNumber + 1) + '/' + str(
            self.numberOfFiles) + ') is processed.')

    def processImage(self, imgChanged=False):
        if self.cirProj is not None:
            QApplication.setOverrideCursor(Qt.WaitCursor)
            flags = self.getFlags(imgChanged)
            self.cirProj.process(flags)
            QApplication.restoreOverrideCursor()
            self.updateParams()
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

    def updateParams(self):
        info = self.cirProj.info
        if 'fixed_hull' in info:
            self.fixed_hull_range = info['fixed_hull']
        if 'merged_peaks' in info:
            self.merged_peaks = info['merged_peaks']
        if self.ROI is None and info['ROI'] != [info['start_point'], info['rmax']]:
            self.ROI = info['ROI']
        if self.orientationModel is None:
            if 'orientation_model' in info:
                self.orientationCmbBx.setCurrentIndex(
                    self.orientationCmbBx.findText(info['orientation_model']))
            self.orientationModel = str(self.orientationCmbBx.currentText())

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

    def fileNameChanged(self):
        fileName = str(self.filenameLineEdit.text()).strip()
        if fileName not in self.imgList:
            return
        self.currentFileNumber = self.imgList.index(fileName)
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

        #img = getBGR(get8bitImage(img, min=self.minInt.value(), max=self.maxInt.value()))

        ax = self.displayImgAxes
        ax.cla()
        if self.logScaleIntChkBx.isChecked():
            ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.minInt.value()), vmax=self.maxInt.value()))
        else:
            ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.minInt.value(), vmax=self.maxInt.value()))
        ax.set_facecolor('black')

        if self.rotation90ChkBx.isEnabled():
            self.rotation90ChkBx.setChecked('90rotation' in self.cirProj.info and self.cirProj.info['90rotation'])

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


        if 'ring_models' in self.cirProj.info and 'ring_errors' in self.cirProj.info and len(self.cirProj.info['ring_errors']) > 0:
            models = self.cirProj.info['ring_models']
            errors = self.cirProj.info['ring_errors']
            best_ind = min(errors.items(), key=lambda err:err[1])[0]
            #model = models[best_ind]
            model = self.cirProj.info.get('average_ring_model', models[best_ind])
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

        if self.roiChkBx.isChecked():
            roi = self.cirProj.info['ROI']
            ax.add_patch(patches.Wedge(tuple(center), roi[1], 0, 360, width=roi[1]-roi[0], fc='r', alpha=0.25))

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
        self.dspacing_chkbx.setHidden(hide)
        self.skip_first_peak_chkbx.setHidden(hide)
        self.original_hist_chkbx.setHidden(hide)
        self.rings_chkbx.setHidden(hide)
        self.hull_hist_chkbx.setHidden(hide)
        self.fit_hist_chkbx.setHidden(hide)
        self.selectPeaks.setHidden(hide)
        self.average_ring_chkbx.setHidden(not hide)
        self.ring_hists_chkbx.setHidden(not hide)
        self.g_model_chkbx.setHidden(not hide or not self.orientationModel.startswith('GMM'))

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

            self.dspacing_chkbx.setEnabled('lambda_sdd' in self.cirProj.info)
            if 'lambda_sdd' in self.cirProj.info and self.dspacing_chkbx.isChecked():
                ax.set_xlim(model_peaks[0] / 2, end_plot)
                ax.set_xscale('dspacing', lambda_sdd=self.cirProj.info['lambda_sdd'])
                ax.set_xlabel('d-spacing (nm)')
            else:
                ax.set_xlim(start_plot, end_plot)
                ax.set_xlabel('Radial distance')

            ax.set_ylabel('Intensity')
            # ax.legend(lines, labels)
            # self.update_plot['image_result'] = False

        elif self.orientationModel.startswith('GMM'):
            model = self.orientationModel
            self.g_model_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())
            self.ring_hists_chkbx.setEnabled('ring_hists' in self.cirProj.info.keys())
            self.average_ring_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())

            ax = self.result_graph_axes
            ax.cla()
            ax.set_xlabel("Radian")
            ax.set_ylabel("Intensity")

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
                            gauss = (orientation_GMM2 if model == "GMM2" else orientation_GMM3)(x=x, **ring_models[i])
                            ax.plot(x, gauss, color='g')
                            u1 = ring_models[i]['u']
                            u2 = u1 - np.pi if u1 >= np.pi else u1 + np.pi
                            ax.plot((u1, u1), (0, max(gauss)), color='y')
                            ax.plot((u2, u2), (0, max(gauss)), color='y')

                if 'average_ring_model' in self.cirProj.info.keys() and self.average_ring_chkbx.isChecked():
                    mod = self.cirProj.info['average_ring_model']
                    gauss = (orientation_GMM2 if model == "GMM2" else orientation_GMM3)(x=x, **mod)
                    u1 = mod['u']
                    u2 = u1 - np.pi if u1 >= np.pi else u1 + np.pi
                    ax.plot(x, gauss, color='k')
                    ax.plot((u1, u1), (0, max(gauss)), color='r')
                    ax.plot((u2, u2), (0, max(gauss)), color='r')

        elif self.orientationModel.startswith('Herman'):
            self.ring_hists_chkbx.setEnabled('ring_hists' in self.cirProj.info.keys())
            self.average_ring_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())

            ax = self.result_graph_axes
            ax.cla()
            ax.set_xlabel("Radian")
            ax.set_ylabel("Herman Orientation Factor")

            if 'ring_hists' in self.cirProj.info.keys():
                x = np.arange(0, 2 * np.pi, np.pi / 180)
                if 'ring_models' in self.cirProj.info.keys() and self.ring_hists_chkbx.isChecked():
                    ring_models = self.cirProj.info['ring_models']
                    for i in ring_models:
                        ax.plot(x, ring_models[i]['HoFs'], color='g')
                        u1 = ring_models[i]['u']
                        ax.plot((u1, u1), (-0.5, 1), color='y')

                if 'average_ring_model' in self.cirProj.info.keys() and self.average_ring_chkbx.isChecked():
                    mod = self.cirProj.info['average_ring_model']
                    ax.plot(x, mod['HoFs'], color='k')
                    u1 = mod['u']
                    ax.plot((u1, u1), (-0.5, 1), color='r')

        else: # Max Intensity
            self.ring_hists_chkbx.setEnabled('ring_hists' in self.cirProj.info.keys())
            self.average_ring_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())

            ax = self.result_graph_axes
            ax.cla()
            ax.set_xlabel("Radian")
            ax.set_ylabel("Intensity")

            if 'ring_hists' in self.cirProj.info.keys():
                ring_hists = self.cirProj.info['ring_hists']
                x = np.arange(0, 2 * np.pi, np.pi / 180)
                if 'ring_models' in self.cirProj.info.keys() and self.ring_hists_chkbx.isChecked():
                    ring_models = self.cirProj.info['ring_models']
                    print(ring_models, len(ring_hists))
                    for i, idx in enumerate(ring_models):
                        ax.plot(x, ring_hists[i], color = tuple(np.array(self.ring_colors[i%len(self.ring_colors)])/255.))
                        u1 = ring_models[idx]['u']
                        ax.plot((u1, u1), (0, max(ring_hists[i])), color='y')
                        ax.plot((u1 + np.pi, u1 + np.pi), (0, max(ring_hists[i])), color='y')

                if 'average_ring_model' in self.cirProj.info.keys() and self.average_ring_chkbx.isChecked():
                    mod = self.cirProj.info['average_ring_model']
                    ax.plot(x, mod['hist'], color='k')
                    u1 = mod['u']
                    ax.plot((u1, u1), (0, max(mod['hist'])), color='r')
                    ax.plot((u1 + np.pi, u1 + np.pi), (0, max(mod['hist'])), color='r')

        self.result_graph_figure.tight_layout()
        self.result_graph_canvas.draw()

        processing_results_text = "Total Intensity : "+ str(self.cirProj.info['area'])
        processing_results_text += "\n\nFitting Results :"
        if 'fitResult' in self.cirProj.info.keys():
                fit_result = self.cirProj.info['fitResult']
                n = int(len(fit_result.keys())/3)
                for i in range(1, n+1):
                    processing_results_text += "\nPeak "+str(i)+': '
                    processing_results_text += "\tcenter(pixel) : "+str(fit_result['u'+str(i)])+'\n'
                    if 'peak_ds' in self.cirProj.info:
                        processing_results_text += "\tcenter(nm) : " + str(self.cirProj.info['peak_ds'][i-1]) + '\n'
                    processing_results_text += "\tarea  : " + str(fit_result['alpha' + str(i)]) + '\n'
                    processing_results_text += "\tsigmad : " + str(fit_result['sigmad' + str(i)]) + '\n'

        self.processing_results.setText(processing_results_text)

        if 'ring_models' in self.cirProj.info.keys() and len(self.cirProj.info['ring_models']) > 0:
            models = self.cirProj.info['ring_models']
            errors = self.cirProj.info['ring_errors']

            rings_info = "Rings Information : \n"

            for i in models.keys():
                m = models[i]
                rings_info += "Ring " + str(i + 1) + " : \n"
                rings_info += "\tAngle : " + str(m['u']) + " rads. " + str(convertRadtoDegrees(m['u'])) + "degrees\n"
                if self.orientationModel.startswith('GMM'):
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
                if self.orientationModel.startswith('GMM'):
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
