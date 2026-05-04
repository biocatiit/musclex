"""
Background subtraction settings popup for Quadrant Folding.
"""

import copy

from .pyqt_utils import *
from .widgets.collapsible_groupbox import CollapsibleGroupBox
from .ManualBackgroundAssignmentDialog import ManualBackgroundAssignmentDialog
from ..utils.optimization_cache import get_user_background_configurations, set_user_background_configurations
from ..utils.file_manager import *
from pathlib import Path


class BackgroundSubtractionDialog(QDialog):
    """Popup window that contains all background subtraction controls."""

    ### ===== Class Constants =====
    # Spinbox Ranges
    RMIN_RMAX_RANGE = (-1, 5000)
    EQUATOR_HEIGHT_RANGE = (1, 1000)
    EQUATOR_CENTER_RANGE = (1, 1000)
    LAYER_LINE_RANGE = (1, 1000)
    BG_PARAM_RANGE = (1, 1000)
    TOPHAT_RANGE = (1, 200)
    RADIAL_BIN_RANGE = (1, 200)
    ITERATIONS_RANGE = (1, 1000)
    PIXEL_RANGE_LIMIT = (0, 100) # percent
    
    # Default Values
    DEFAULT_RMIN_RMAX = -1
    DEFAULT_EQUATOR_HEIGHT = 70
    DEFAULT_EQUATOR_CENTER = 70
    DEFAULT_LAYER_SPACING = 100
    DEFAULT_LAYER_WIDTH = 5
    DEFAULT_GAUSSIAN_FWHM = 15
    DEFAULT_BOXCAR_SIZE = 15
    DEFAULT_CYCLES = 250
    DEFAULT_WINDOW_SIZE = 15
    DEFAULT_WINDOW_SEP = 10
    DEFAULT_PIXEL_MIN = 0
    DEFAULT_PIXEL_MAX = 25
    DEFAULT_THETA_BIN_INDEX = 4
    DEFAULT_DEGREE_INDEX = 1
    DEFAULT_RADIAL_BIN = 10
    DEFAULT_SMOOTHING = 0.1
    DEFAULT_TENSION = 1.0
    DEFAULT_TOPHAT_SIZE = 50
    DEFAULT_MAX_ITERATIONS = 10
    DEFAULT_EARLY_STOP = 0.01
    DEFAULT_MEAN_MSE = 100.0
    DEFAULT_MEAN_NEG_SYN = 20.0
    DEFAULT_MEAN_BASELINE = 35.0
    DEFAULT_MEAN_NEG_CON = 7.0
    DEFAULT_MEAN_SMOOTH = 30.0
    DEFAULT_EVAL_BASELINE = 0.0
    DEFAULT_AMP = 0.01
    DEFAULT_SIGMA_X = 5.0
    DEFAULT_SIGMA_Y = 10.0
    DEFAULT_WEIGHT_MSE = 0.1
    DEFAULT_WEIGHT_NEG_SYN = 0.3
    DEFAULT_WEIGHT_BASELINE = 0.1
    DEFAULT_WEIGHT_NEG_CON = 0.1
    DEFAULT_WEIGHT_SMOOTH = 0.4
    
    # Downsample and Frequency Options
    DOWNSAMPLE_OPTIONS = ["1", "2", "4"]
    DEFAULT_DOWNSAMPLE_INDEX = 1
    THETA_BIN_OPTIONS = ["3", "5", "10", "15", "30", "45", "90"]
    DEGREE_OPTIONS = ["0.5", "1", "2", "3", "5", "9", "10", "15"]
    FREQ_OPTIONS = ["sparse", "medium", "dense"]
    DEFAULT_FREQ = "medium"
    
    # BG Subtraction Methods
    BG_METHODS = [
        'None',
        '2D Convexhull',
        'Circularly-symmetric',
        'White-top-hats',
        'Smoothed-Gaussian',
        'Smoothed-BoxCar',
        'Roving Window'
    ]
    OPTIMIZATION_METHODS = BG_METHODS[1:]  # Exclude 'None' from optimization options
    DEFAULT_OPTIMIZATION_METHODS = ['Circularly-symmetric', 'White-top-hats', 'Smoothed-Gaussian']
    DEFAULT_OPTIMIZATION_STEPS = "500, 250, 100, 50, 25, 10, 5, 3, 1"
    
    # Styles
    STYLES = {
        "doc_panel": "#documentationPanel { background: #F1F8E9; border: 1px solid #C8E6C9; border-radius: 4px; }",
        "label_small": "QLabel { font-size: 12px; }",
        "label_bold_small": "font-size: 11px; font-weight: 700; color: #444;",
        "step_label": "font-weight: 700; font-size: 15px; color: #990000;",
        "apply_button": "QPushButton { color: #ededed; background-color: #999999; }",
        "process_button": "QPushButton { color: #ededed; background-color: #af6207; }",
        "metrics_title": "font-weight: bold; font-size: 14px; color: #d35400;",
        "table_widget": "QTableWidget { font-size: 13px; }",
        "table_header": "QHeaderView::section { font-size: 11px; padding: 2px 4px; }",
        "groupbox_border": "QGroupBox { border: 1; margin-top: 0px; }",
    }

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Background Subtraction Settings")
        self.resize(620, 760)

        self._parent_gui = parent
        self.backgroundConfigurations = []
        self.manualBackgroundAssignments = {}

        self._create_widgets()
        self._create_layout()

    def _get_parent_gui(self):
        parent = self.parent()
        return parent if parent is not None else self._parent_gui

    def _get_parent_attr(self, name, default=None):
        parent = self._get_parent_gui()
        return getattr(parent, name, default) if parent is not None else default

    def _able_to_process(self):
        parent = self._get_parent_gui()
        if parent is not None and hasattr(parent, "ableToProcess"):
            return parent.ableToProcess()
        return False

    def _format_bg_params_text(self, params):
        parent = self._get_parent_gui()
        if parent is not None and hasattr(parent, "_format_bg_params_text"):
            return parent._format_bg_params_text(params)
        if not isinstance(params, dict) or len(params) == 0:
            return "None"
        parts = []
        for key in sorted(params.keys()):
            value = params[key]
            try:
                value_text = f"{float(value):.6g}"
            except Exception:
                value_text = str(value)
            parts.append(f"{key}={value_text}")
        return ", ".join(parts)

    # ===== Factory Methods =====
    def _create_spinbox(self, min_val=0, max_val=100, value=0, step=1, suffix="", prefix="", tooltip=""):
        """Factory method for consistent QSpinBox creation."""
        spnbx = QSpinBox()
        spnbx.setRange(min_val, max_val)
        spnbx.setValue(value)
        spnbx.setSingleStep(step)
        spnbx.setKeyboardTracking(False)
        if suffix:
            spnbx.setSuffix(suffix)
        if prefix:
            spnbx.setPrefix(prefix)
        if tooltip:
            spnbx.setToolTip(tooltip)
        return spnbx

    def _create_double_spinbox(self, min_val=0.0, max_val=100.0, value=0.0, decimals=2, 
                              step=0.1, suffix="", prefix="", tooltip=""):
        """Factory method for consistent QDoubleSpinBox creation."""
        spnbx = QDoubleSpinBox()
        spnbx.setRange(min_val, max_val)
        spnbx.setValue(value)
        spnbx.setDecimals(decimals)
        spnbx.setSingleStep(step)
        spnbx.setKeyboardTracking(False)
        if suffix:
            spnbx.setSuffix(suffix)
        if prefix:
            spnbx.setPrefix(prefix)
        if tooltip:
            spnbx.setToolTip(tooltip)
        return spnbx

    def _create_label(self, text, style_key=None):
        """Create a QLabel with optional preset styling."""
        label = QLabel(text)
        if style_key == "small":
            label.setStyleSheet("QLabel { font-size: 12px; }")
        elif style_key == "bold_small":
            label.setStyleSheet("font-size: 11px; font-weight: 700; color: #444;")
        return label

    @staticmethod
    def build_current_config_summary_widget(
        method_label,
        params_label,
        loss_label,
        title=None,
        min_params_width=400,
        value_style="font-size: 13px; color: #222;",
        field_label_style="font-size: 11px; font-weight: 700; color: #444;",
    ):
        """Create a reusable "Current Configuration" summary widget."""
        params_label.setWordWrap(True)
        params_label.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Preferred)
        if min_params_width:
            params_label.setMinimumWidth(min_params_width)

        for label in [method_label, params_label, loss_label]:
            label.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)
            label.setStyleSheet(value_style)

        current_summary_widget = QWidget()
        current_summary_widget.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Fixed)
        form = QFormLayout(current_summary_widget)
        form.setContentsMargins(6, 4, 6, 4)
        form.setHorizontalSpacing(8)
        form.setVerticalSpacing(4)
        form.setFieldGrowthPolicy(QFormLayout.ExpandingFieldsGrow)
        form.setRowWrapPolicy(QFormLayout.WrapLongRows)

        if title:
            current_config_title = QLabel(title)
            current_config_title.setStyleSheet(field_label_style)
            form.addRow(current_config_title)

        method_label_header = QLabel("Method:")
        method_label_header.setStyleSheet(field_label_style)
        params_label_header = QLabel("Parameters:")
        params_label_header.setStyleSheet(field_label_style)
        loss_label_header = QLabel("Loss:")
        loss_label_header.setStyleSheet(field_label_style)

        form.addRow(method_label_header, method_label)
        form.addRow(params_label_header, params_label)
        form.addRow(loss_label_header, loss_label)

        return current_summary_widget

    # ===== Widget Creation Methods =====
    def _create_widgets(self):
        """Create all UI widgets by delegating to specialized helper methods."""
        self._create_documentation_panel()
        self._create_processing_mode_widgets()
        self._create_rmin_rmax_widgets()
        self._create_image_processing_widgets()
        self._create_evaluation_mask_widgets()
        self._create_background_parameter_widgets()
        self._create_optimization_widgets()
        self._create_evaluation_metric_widgets()
        self._create_table_and_button_widgets()
        self._create_group_boxes()
        self._update_processing_mode_visibility()

    def _create_documentation_panel(self):
        """Create documentation panel at the top of the dialog."""
        self.documentationPanel = QFrame()
        self.documentationPanel.setObjectName("documentationPanel")
        self.documentationPanel.setStyleSheet(self.STYLES["doc_panel"])
        self.documentationLabel = QLabel(
            "<span style='color:#2e7d32;'>"
            "For information on how to use these settings and interpret the metrics, "
            "please review: "
            "<a href='https://musclex.readthedocs.io/en/latest/AppSuite/QuadrantFolding/index.html#id1'>"
            "Quadrant Folding Documentation"
            "</a>."
            "</span>"
        )
        self.documentationLabel.setWordWrap(True)
        self.documentationLabel.setTextFormat(Qt.RichText)
        self.documentationLabel.setTextInteractionFlags(Qt.TextBrowserInteraction)
        self.documentationLabel.setOpenExternalLinks(True)

    def _create_processing_mode_widgets(self):
        """Create processing mode selection and background choice widgets."""
        # ===== Background Removal Mode Selection =====
        self.processingModeLabel = QLabel("Processing Mode:")
        self.processingModeCB = QComboBox()
        self.processingModeCB.addItems(["Manual", "Automated"])
        self.processingModeCB.setCurrentIndex(0)
        self.processingModeCB.currentIndexChanged.connect(self._update_processing_mode_visibility)

        self.allBGChoices = self.BG_METHODS
        self.bgChoiceIn = QComboBox()
        self.bgChoiceIn.setCurrentIndex(0)
        for c in self.allBGChoices:
            self.bgChoiceIn.addItem(c)

    def _create_rmin_rmax_widgets(self):
        """Create R-min/R-max settings widgets."""
        self.setRminRmaxButton = QPushButton("Manual R-min/max")
        self.setRminRmaxButton.setCheckable(True)

        self.rminSpnBx = self._create_spinbox(min_val=self.RMIN_RMAX_RANGE[0], max_val=self.RMIN_RMAX_RANGE[1], 
                                              value=self.DEFAULT_RMIN_RMAX, step=2)
        self.rminLabel = QLabel("R-min")

        self.rmaxSpnBx = self._create_spinbox(min_val=self.RMIN_RMAX_RANGE[0], max_val=self.RMIN_RMAX_RANGE[1], 
                                              value=self.DEFAULT_RMIN_RMAX, step=2)
        self.rmaxLabel = QLabel("R-max")

        self.showRminRmaxChkBx = QCheckBox("Show R-min/max")
        self.fixedRadiusRangeChkBx = QCheckBox("Persist R-min/max")

    def _create_image_processing_widgets(self):
        """Create image processing settings widgets."""
        self.downsampleLabel = QLabel("Downsample")
        self.downsampleCB = QComboBox()
        self.downsampleCB.addItems(self.DOWNSAMPLE_OPTIONS)
        self.downsampleCB.setCurrentIndex(self.DEFAULT_DOWNSAMPLE_INDEX)

        self.smoothImageChkbx = QCheckBox("Smooth Image")
        self.smoothImageChkbx.setChecked(False)

    def _create_evaluation_mask_widgets(self):
        """Create evaluation mask settings widgets."""

        self.equatorMaskHeightLabel = QLabel("Equator Height : ")
        self.equatorMaskHeightSpnBx = self._create_spinbox(min_val=self.EQUATOR_HEIGHT_RANGE[0], 
                                                        max_val=self.EQUATOR_HEIGHT_RANGE[1], 
                                                        value=self.DEFAULT_EQUATOR_HEIGHT)

        self.equatorCenterBeamLabel = QLabel("Equator Center Radius : ")
        self.equatorCenterBeamSpnBx = self._create_spinbox(min_val=self.EQUATOR_CENTER_RANGE[0], 
                                                           max_val=self.EQUATOR_CENTER_RANGE[1], 
                                                           value=self.DEFAULT_EQUATOR_CENTER)

        self.m1Label = QLabel("Layer line spacing : ")
        self.m1SpnBx = self._create_spinbox(min_val=self.LAYER_LINE_RANGE[0], 
                                            max_val=self.LAYER_LINE_RANGE[1], 
                                            value=self.DEFAULT_LAYER_SPACING)

        self.layerLineWidthLabel = QLabel("Layer line width : ")
        self.layerLineWidthSpnBx = self._create_spinbox(min_val=self.LAYER_LINE_RANGE[0], 
                                                        max_val=self.LAYER_LINE_RANGE[1], 
                                                        value=self.DEFAULT_LAYER_WIDTH)

    def _create_background_parameter_widgets(self):
        """Create background subtraction parameter widgets."""
        # Gaussian parameters
        self.gaussFWHMLabel = QLabel("Gaussian FWHM : ")
        self.gaussFWHM = self._create_spinbox(min_val=self.BG_PARAM_RANGE[0], max_val=self.BG_PARAM_RANGE[1], 
                                              value=self.DEFAULT_GAUSSIAN_FWHM)

        # Boxcar parameters
        self.boxcarLabel = QLabel("Box Car Size : ")
        self.boxcarX = self._create_spinbox(min_val=self.BG_PARAM_RANGE[0], max_val=self.BG_PARAM_RANGE[1], 
                                            value=self.DEFAULT_BOXCAR_SIZE, prefix='X:')
        self.boxcarY = self._create_spinbox(min_val=self.BG_PARAM_RANGE[0], max_val=self.BG_PARAM_RANGE[1], 
                                            value=self.DEFAULT_BOXCAR_SIZE, prefix='Y:')

        # Cycle parameters
        self.cycleLabel = QLabel("Number of Cycles : ")
        self.cycle = self._create_spinbox(min_val=self.BG_PARAM_RANGE[0], max_val=self.BG_PARAM_RANGE[1], 
                                          value=self.DEFAULT_CYCLES)

        # Window size parameters
        self.windowSizeLabel = QLabel("Window Size : ")
        self.winSizeX = self._create_spinbox(min_val=self.BG_PARAM_RANGE[0], max_val=self.BG_PARAM_RANGE[1], 
                                             value=self.DEFAULT_WINDOW_SIZE, prefix='X:')
        self.winSizeY = self._create_spinbox(min_val=self.BG_PARAM_RANGE[0], max_val=self.BG_PARAM_RANGE[1], 
                                             value=self.DEFAULT_WINDOW_SIZE, prefix='Y:')

        # Window separation parameters
        self.windowSepLabel = QLabel("Window Separation : ")
        self.winSepX = self._create_spinbox(min_val=self.BG_PARAM_RANGE[0], max_val=self.BG_PARAM_RANGE[1], 
                                            value=self.DEFAULT_WINDOW_SEP, prefix='X:')
        self.winSepY = self._create_spinbox(min_val=self.BG_PARAM_RANGE[0], max_val=self.BG_PARAM_RANGE[1], 
                                            value=self.DEFAULT_WINDOW_SEP, prefix='Y:')

        # Pixel range parameters
        self.minPixRange = self._create_double_spinbox(min_val=self.PIXEL_RANGE_LIMIT[0], 
                                                       max_val=self.PIXEL_RANGE_LIMIT[1], 
                                                       value=self.DEFAULT_PIXEL_MIN, 
                                                       decimals=2, step=2, suffix="%")
        self.maxPixRange = self._create_double_spinbox(min_val=self.PIXEL_RANGE_LIMIT[0], 
                                                       max_val=self.PIXEL_RANGE_LIMIT[1], 
                                                       value=self.DEFAULT_PIXEL_MAX, 
                                                       decimals=2, step=2, suffix="%")
        self.pixRangeLabel = QLabel("Pixel Range : ")

        # Theta bin
        self.thetaBinLabel = QLabel("Bin Theta (deg) : ")
        self.thetabinCB = QComboBox()
        self.thetabinCB.addItems(self.THETA_BIN_OPTIONS)
        self.thetabinCB.setCurrentIndex(self.DEFAULT_THETA_BIN_INDEX)

        # Radial bin
        self.radialBinLabel = QLabel("Radial Bin : ")
        self.radialBinSpnBx = self._create_spinbox(min_val=self.RADIAL_BIN_RANGE[0], 
                                                   max_val=self.RADIAL_BIN_RANGE[1], 
                                                   value=self.DEFAULT_RADIAL_BIN, suffix=" Pixel(s)")

        # Smoothing and tension
        self.smoothLabel = QLabel("Smoothing factor : ")
        self.smoothSpnBx = self._create_double_spinbox(min_val=0, max_val=10000, 
                                                       value=self.DEFAULT_SMOOTHING, 
                                                       decimals=2, step=0.1)

        self.tensionLabel = QLabel("Tension factor : ")
        self.tensionSpnBx = self._create_double_spinbox(min_val=0, max_val=100, 
                                                        value=self.DEFAULT_TENSION, 
                                                        decimals=2, step=0.1)

        # Top-hat
        self.tophat1Label = QLabel("Top-hat Disk Size: ")
        self.tophat1SpnBx = self._create_spinbox(min_val=self.TOPHAT_RANGE[0], 
                                                 max_val=self.TOPHAT_RANGE[1], 
                                                 value=self.DEFAULT_TOPHAT_SIZE)

        # Degree
        self.deg1Label = QLabel("Step Degree : ")
        self.deg1CB = QComboBox()
        self.deg1CB.addItems(self.DEGREE_OPTIONS)
        self.deg1CB.setCurrentIndex(self.DEFAULT_DEGREE_INDEX)

    def _create_optimization_widgets(self):
        """Create optimization/automated processing widgets."""
        self.optimizeFlag = False

        self.optimizationMethodsLabel = QLabel("BG Subtraction Methods (Multi-select):")
        self.optimizationMethodsList = QListWidget()
        self.optimizationMethodsList.setSelectionMode(QAbstractItemView.MultiSelection)
        self.optimizationMethodsList.setMaximumHeight(150)
        self.optimizationMethods = self.OPTIMIZATION_METHODS
        for method in self.optimizationMethods:
            self.optimizationMethodsList.addItem(method)

        # Default methods aligned with current optimization workflow
        self._set_selected_methods(self.DEFAULT_OPTIMIZATION_METHODS)

        self.stepsLabel = QLabel("Step Sizes:")
        self.stepsLineEdit = QLineEdit(self.DEFAULT_OPTIMIZATION_STEPS)
        self.stepsLineEdit.setToolTip("Comma-separated values used for optimization step schedule.")

        self.maxIterationsLabel = QLabel("Max Iterations:")
        self.maxIterationsSpnBx = self._create_spinbox(min_val=self.ITERATIONS_RANGE[0], 
                                                       max_val=self.ITERATIONS_RANGE[1], 
                                                       value=self.DEFAULT_MAX_ITERATIONS,
            tooltip="Maximum number of candidate (+/- step) evaluations per background subtraction parameter.")

        self.earlyStopLabel = QLabel("Early Stop Loss Threshold:")
        self.earlyStopSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1.0, 
                                                          value=self.DEFAULT_EARLY_STOP,
            decimals=4, step=0.01,
            tooltip="Threshold for early stopping during optimization per background subtraction parameter.")
        
        

    def _create_evaluation_metric_widgets(self):
        """Create normalization means, evaluation metrics, and metric weight widgets."""
        # ===== Normalization Means (Used in table) =====
        self.meanMSELabel = self._create_label("Mean Squared Error of Synthetic Signal, intst. cnts.", "small")
        self.meanMSESpnBx = self._create_double_spinbox(min_val=1e-6, max_val=1e9, 
                                                        value=self.DEFAULT_MEAN_MSE, decimals=2)

        self.meanNegSynLabel = self._create_label("Fraction of Synthetic Oversubtraction, %", "small")
        self.meanNegSynSpnBx = self._create_double_spinbox(min_val=1e-6, max_val=100.0, 
                                                           value=self.DEFAULT_MEAN_NEG_SYN, 
                                                           decimals=2, suffix=" %")

        self.meanNonBaselineLabel = self._create_label("Fraction of Non Near-Zero Baseline Pixels, %", "small")
        self.meanNonBaselineSpnBx = self._create_double_spinbox(min_val=1e-6, max_val=100.0, 
                                                                value=self.DEFAULT_MEAN_BASELINE, 
                                                                decimals=2, suffix=" %")

        self.meanNegConLabel = self._create_label("Fraction of Negative Connected Pixels, %", "small")
        self.meanNegConSpnBx = self._create_double_spinbox(min_val=1e-6, max_val=100.0, 
                                                           value=self.DEFAULT_MEAN_NEG_CON, 
                                                           decimals=2, suffix=" %")

        self.meanSmoothLabel = self._create_label("Smoothness Metric", "small")
        self.meanSmoothSpnBx = self._create_double_spinbox(min_val=1e-6, max_val=1e6, 
                                                           value=self.DEFAULT_MEAN_SMOOTH, decimals=2)

        # ===== Evaluation Metrics Settings =====
        self.evaluationBaselineLabel = self._create_label("Evaluation Baseline:", "small")
        self.evaluationBaselineSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e9, 
                                                                   value=self.DEFAULT_EVAL_BASELINE,
            decimals=4, step=0.01,
            tooltip="Baseline value for near-zero pixel evaluation.")

        self.ampLabel = self._create_label("Amplitude multiplier (I10):", "small")
        self.ampSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                    value=self.DEFAULT_AMP, 
                                                    decimals=2, step=0.001)

        self.sigmaXDivLabel = self._create_label("Sigma X divisor (I01):", "small")
        self.sigmaXDivSpnBx = self._create_double_spinbox(min_val=0.0001, max_val=1e6, 
                                                          value=self.DEFAULT_SIGMA_X, 
                                                          decimals=2, step=0.5)

        self.sigmaYDivLabel = self._create_label("Sigma Y divisor (M1):", "small")
        self.sigmaYDivSpnBx = self._create_double_spinbox(min_val=0.0001, max_val=1e6, 
                                                          value=self.DEFAULT_SIGMA_Y, 
                                                          decimals=2, step=0.5)

        self.freqLabel = self._create_label("Sampling Frequency:", "small")
        self.freqCB = QComboBox()
        self.freqCB.addItems(self.FREQ_OPTIONS)
        self.freqCB.setCurrentText(self.DEFAULT_FREQ)

        # ===== Metric Weights (Used in table) =====
        self.metricWeightsLabel = QLabel("Metric Weights:")
        self.weightMSELabel = self._create_label("Mean Squared Error of Synthetic Signal, intst. cnts.", "small")
        self.weightMSESpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                          value=self.DEFAULT_WEIGHT_MSE, decimals=2)

        self.weightNegSynLabel = self._create_label("Fraction of Synthetic Oversubtraction, %", "small")
        self.weightNegSynSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                             value=self.DEFAULT_WEIGHT_NEG_SYN, decimals=2)

        self.weightNonBaselineLabel = self._create_label("Fraction of Non Near-Zero Baseline Pixels, %", "small")
        self.weightNonBaselineSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                                  value=self.DEFAULT_WEIGHT_BASELINE, decimals=2)

        self.weightNegConLabel = self._create_label("Fraction of Negative Connected Pixels, %", "small")
        self.weightNegConSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                             value=self.DEFAULT_WEIGHT_NEG_CON, decimals=2)

        self.weightSmoothLabel = self._create_label("Smoothness Metric, intst. cnts.", "small")
        self.weightSmoothSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                             value=self.DEFAULT_WEIGHT_SMOOTH, decimals=2)

        self.metricWeightsHintLabel = QLabel(
            "<i><span style='color:#2e7d32;'>"
            "Compound Loss weights may be adjusted as needed and should roughly add up to 1."
            "</span></i>"
        )
        self.metricWeightsHintLabel.setTextFormat(Qt.RichText)
        self.metricWeightsHintLabel.setWordWrap(True)

        # ===== Loss Parameter Table (Weights and Means) =====
        self.lossParamsTable = QTableWidget(5, 2)
        self.lossParamsTable.setHorizontalHeaderLabels(["Mean", "Weight"])
        self.lossParamsTable.setVerticalHeaderLabels([
            "Mean Squared Error of Synthetic Signal, intst. cnts.",
            "Fraction of Synthetic Oversubtraction, %",
            "Fraction of Non Near-Zero Baseline Pixels, %",
            "Fraction of Negative Connected Pixels, %",
            "Smoothness Metric, intst. cnts."
        ])
        
        self.lossParamsTable.setAlternatingRowColors(True)
        self.lossParamsTable.setMinimumHeight(180)
        self.lossParamsTable.setMaximumHeight(200)
        self.lossParamsTable.setEditTriggers(
            QAbstractItemView.DoubleClicked
            | QAbstractItemView.EditKeyPressed
            | QAbstractItemView.AnyKeyPressed
        )
        self.lossParamsTable.setSelectionBehavior(QAbstractItemView.SelectItems)
        self.lossParamsTable.setStyleSheet(self.STYLES["table_widget"])
        self.lossParamsTable.horizontalHeader().setStyleSheet(self.STYLES["table_header"])
        self.lossParamsTable.verticalHeader().setStyleSheet(
            "QHeaderView::section { font-size: 13px; padding: 2px 4px; }"
        )
        self.lossParamsTable.horizontalHeader().setStretchLastSection(False)
        self.lossParamsTable.horizontalHeader().setSectionResizeMode(0, QHeaderView.Stretch)
        self.lossParamsTable.horizontalHeader().setSectionResizeMode(1, QHeaderView.Fixed)
        self.lossParamsTable.horizontalHeader().setSectionResizeMode(2, QHeaderView.Fixed)

        self._populate_loss_params_table()
        self.lossParamsTable.itemChanged.connect(self._on_loss_params_table_changed)

        self.lossParamsTable.setColumnHidden(0, True)

        header = self.lossParamsTable.horizontalHeader()
        header.setSectionsClickable(True)
        header.setContextMenuPolicy(Qt.CustomContextMenu)
        header.sectionDoubleClicked.connect(self._hide_unhide_means_column)
        header.setToolTip("Double-click to show normalization means.")

        # ===== Subtraction Evaluation Metrics Table =====
        self.bgMetricsTableTitle = QLabel("Loss = ")
        self.bgMetricsTableTitle.setStyleSheet(self.STYLES["metrics_title"])
        self.bgMetricsTable = QTableWidget(0, 4)
        self.bgMetricsTable.setHorizontalHeaderLabels(["Metric", "Raw", "Normalized", "Loss Contrib."])
        self.bgMetricsTable.verticalHeader().setVisible(False)
        self.bgMetricsTable.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self.bgMetricsTable.setSelectionMode(QAbstractItemView.NoSelection)
        self.bgMetricsTable.setFocusPolicy(Qt.NoFocus)
        self.bgMetricsTable.setAlternatingRowColors(True)
        self.bgMetricsTable.setMinimumHeight(200)
        self.bgMetricsTable.setMaximumHeight(320)
        self.bgMetricsTable.horizontalHeader().setStretchLastSection(False)
        self.bgMetricsTable.horizontalHeader().setSectionResizeMode(0, QHeaderView.Stretch)
        self.bgMetricsTable.horizontalHeader().setSectionResizeMode(1, QHeaderView.Fixed)
        self.bgMetricsTable.horizontalHeader().setSectionResizeMode(2, QHeaderView.Fixed)
        self.bgMetricsTable.horizontalHeader().setSectionResizeMode(3, QHeaderView.Fixed)
        self.bgMetricsTable.setColumnWidth(1, 80)
        self.bgMetricsTable.setColumnWidth(2, 80)
        self.bgMetricsTable.setColumnWidth(3, 80)
        self.bgMetricsTable.setStyleSheet(
            "QTableWidget { font-size: 13px; }"
            "QHeaderView::section { font-size: 11px; padding: 2px 4px; }"
        )


        # ===== Configuration Selection Widgets =====

    def _create_table_and_button_widgets(self):
        """Create tables, buttons, and other control widgets."""
        # ===== Apply Button =====
        self.applyBGButton = QPushButton("Apply Selected Subtraction Settings")
        self.applyBGButton.setStyleSheet(self.STYLES["apply_button"])

        # ===== Background Configurations Table =====
        self.addBackgroundConfigButton = QPushButton("Add Background Configuration")

        self.backgroundConfigsTable = QTableWidget(0, 4)
        self.backgroundConfigsTable.setHorizontalHeaderLabels(["Name", "Method", "Parameters", "Loss"])
        self.backgroundConfigsTable.verticalHeader().setVisible(False)
        self.backgroundConfigsTable.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self.backgroundConfigsTable.setSelectionBehavior(QAbstractItemView.SelectRows)
        self.backgroundConfigsTable.setSelectionMode(QAbstractItemView.SingleSelection)
        self.backgroundConfigsTable.setAlternatingRowColors(True)
        self.backgroundConfigsTable.setMinimumHeight(50)
        self.backgroundConfigsTable.setMaximumHeight(150)
        self.backgroundConfigsTable.setStyleSheet(
            "QTableWidget { font-size: 13px; }"
            "QHeaderView::section { font-size: 13px; }"
        )
        self.backgroundConfigsTable.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeToContents)
        self.backgroundConfigsTable.horizontalHeader().setSectionResizeMode(2, QHeaderView.ResizeToContents)
        self.backgroundConfigsTable.setContextMenuPolicy(Qt.CustomContextMenu)

        self.deleteBackgroundConfigButton = QPushButton("Delete Selected Configuration")
        self.deleteBackgroundConfigButton.setEnabled(False)

        # ===== Batch Processing Controls =====
        self.chooseConfigurationsAutoChkBx = QCheckBox("Choose best configuration for images automatically")
        self.chooseConfigurationsAutoChkBx.setToolTip(
            "Automatically choose the best background configuration for each image based on background metrics"
        )
        self.chooseConfigurationsAutoChkBx.setChecked(True)

        self.createNewConfigurationsChkBx = QCheckBox("Automatically create new configurations for outlier images")
        self.createNewConfigurationsChkBx.setToolTip(
            "Automatically create new background configurations for images that are considered outliers "
            "(based on background metrics) and add them to the Background Configurations table"
        )

        self.assignConfgurationsManually = QPushButton("Manually assign configurations to images")

        self.processFolderWithSelections = QPushButton("Process Current Folder")
        self.processFolderWithSelections.setStyleSheet(self.STYLES["process_button"])

    def _create_group_boxes(self):
        """Create group boxes and collapsible sections."""
        # ===== Section Groups =====
        self.settingsGroup = QGroupBox("Subtraction Settings")
        self.evaluationGroup = QGroupBox("Optimization Target and Evaluation Settings")
        self.evaluationMetricsGroup = QGroupBox("Results")
        self.batchProcessingGroup = QGroupBox("Batch Processing Settings")

        # ===== Manual/Automated Groups =====
        self.manualGroup = QGroupBox("Manual Processing")
        self.autoGroup = QGroupBox("Automated Processing")
        self.manualGroup.setTitle("")
        self.autoGroup.setTitle("")
        self.manualGroup.setStyleSheet(self.STYLES["groupbox_border"])
        self.autoGroup.setStyleSheet(self.STYLES["groupbox_border"])

        # ===== Collapsible Groups =====
        self.rminGroup = CollapsibleGroupBox("R-min/R-max", start_expanded=False)
        self.imageProcGroup = CollapsibleGroupBox("Image Processing", start_expanded=False)
        self.subtractionGroup = CollapsibleGroupBox("Subtraction", start_expanded=False)
        self.configurationGroup = CollapsibleGroupBox("Configurations", start_expanded=False)
        self.folderGroup = CollapsibleGroupBox("Folder Processing", start_expanded=False)
        self.evalGroup = CollapsibleGroupBox("Results", start_expanded=False)
        self.evalMaskGroup = CollapsibleGroupBox("Evaluation Masks", start_expanded=False)
        self.metricsGroup = CollapsibleGroupBox("Metric Settings", start_expanded=False)

    def _create_layout(self):
        """Create the main dialog layout by delegating to helper methods."""
        main_layout = QVBoxLayout(self)
        
        # Add documentation panel at top
        self._setup_documentation_layout(main_layout)
        
        # Create scrollable container with all sections
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        container_layout = self._create_main_container_layout()
        main_layout.addWidget(scroll_area)
        
        # Set scroll area widget
        container = QWidget()
        container.setLayout(container_layout)
        scroll_area.setWidget(container)

    def _setup_documentation_layout(self, parent_layout):
        """Setup the documentation panel layout."""
        doc_layout = QVBoxLayout(self.documentationPanel)
        doc_layout.setContentsMargins(10, 8, 10, 8)
        doc_layout.addWidget(self.documentationLabel)
        parent_layout.addWidget(self.documentationPanel)

    def _create_main_container_layout(self):
        """Create the main container layout with all sections."""
        container_layout = QVBoxLayout()
        
        # Setup all group layouts
        common_layout, evaluation_layout = self._setup_common_and_evaluation_layouts()
        self._setup_subtraction_layout()
        self._setup_metrics_layout()
        self._setup_rmin_rmax_layout()
        self._setup_image_processing_layout()
        self._setup_evaluation_mask_layout()
        self._setup_evaluation_results_layout()
        
        # Build the container with sections
        container_layout.addWidget(self._create_step_label("Step 1: Adjust Image Setting and Process"))
        container_layout.addWidget(self.settingsGroup)
        container_layout.addWidget(self.evaluationGroup)
        container_layout.addWidget(self.applyBGButton)
        
        container_layout.addWidget(self._create_step_label("Step 2: Review Results"))
        container_layout.addWidget(self.evalGroup)
        
        container_layout.addWidget(self._create_step_label("Step 3: Adjust Batch Processing Settings and Launch"))
        self._create_current_configs()
        container_layout.addWidget(self.configurationGroup)
        container_layout.addWidget(self.folderGroup)
        self._setup_folder_layout()
        container_layout.addWidget(self.processFolderWithSelections)
        container_layout.addStretch(1)
        
        return container_layout

    def _create_step_label(self, text):
        """Create a formatted step label."""
        label = QLabel(text)
        label.setStyleSheet(self.STYLES["step_label"])
        return label

    def _setup_common_and_evaluation_layouts(self):
        """Setup common (settings) and evaluation layouts."""
        common_layout = QVBoxLayout(self.settingsGroup)
        evaluation_layout = QVBoxLayout(self.evaluationGroup)
        
        common_layout.addWidget(self.rminGroup)
        common_layout.addWidget(self.imageProcGroup)
        common_layout.addWidget(self.subtractionGroup)
        
        evaluation_layout.addWidget(self.evalMaskGroup)
        evaluation_layout.addWidget(self.metricsGroup)
        
        return common_layout, evaluation_layout

    def _setup_subtraction_layout(self):
        """Setup background subtraction settings layout (manual and automated)."""
        modeWidget = QWidget()
        modeLayout = QGridLayout(modeWidget)
        modeLayout.setContentsMargins(0, 0, 0, 0)
        modeLayout.addWidget(self.processingModeLabel, 0, 0, 1, 2)
        modeLayout.addWidget(self.processingModeCB, 0, 2, 1, 2)

        # Manual processing layout
        manual_layout = QGridLayout(self.manualGroup)
        self._populate_manual_processing_layout(manual_layout)

        # Automated processing layout
        auto_layout = QGridLayout(self.autoGroup)
        self._populate_automated_processing_layout(auto_layout)

        # Combine into subtraction group
        subtraction_layout = QGridLayout()
        subtraction_layout.setContentsMargins(8, 8, 8, 8)
        subtraction_layout.addWidget(modeWidget, 0, 0, 1, 4)
        subtraction_layout.addWidget(self.manualGroup, 1, 0, 1, 4)
        subtraction_layout.addWidget(self.autoGroup, 2, 0, 1, 4)
        self.subtractionGroup.setLayout(subtraction_layout)

    def _populate_manual_processing_layout(self, layout):
        """Populate manual processing controls."""
        layout.addWidget(QLabel("Subtraction Method:"), 2, 0, 1, 2)
        layout.addWidget(self.bgChoiceIn, 2, 2, 1, 2)
        layout.addWidget(self.gaussFWHMLabel, 4, 2, 1, 1)
        layout.addWidget(self.gaussFWHM, 4, 3, 1, 1)
        layout.addWidget(self.boxcarLabel, 4, 2, 1, 1)
        layout.addWidget(self.boxcarX, 4, 3, 1, 1)
        layout.addWidget(self.boxcarY, 5, 3, 1, 1)
        layout.addWidget(self.cycleLabel, 3, 2, 1, 1)
        layout.addWidget(self.cycle, 3, 3, 1, 1)
        layout.addWidget(self.thetaBinLabel, 6, 2, 1, 1)
        layout.addWidget(self.thetabinCB, 6, 3, 1, 1)
        layout.addWidget(self.radialBinLabel, 7, 2, 1, 1)
        layout.addWidget(self.radialBinSpnBx, 7, 3, 1, 1)
        layout.addWidget(self.windowSizeLabel, 6, 2, 1, 1)
        layout.addWidget(self.winSizeX, 6, 3, 1, 1)
        layout.addWidget(self.winSizeY, 7, 3, 1, 1)
        layout.addWidget(self.windowSepLabel, 8, 2, 1, 1)
        layout.addWidget(self.winSepX, 8, 3, 1, 1)
        layout.addWidget(self.winSepY, 9, 3, 1, 1)
        layout.addWidget(self.pixRangeLabel, 10, 2, 1, 1)
        layout.addWidget(self.minPixRange, 10, 3, 1, 1)
        layout.addWidget(self.maxPixRange, 11, 3, 1, 1)
        layout.addWidget(self.smoothLabel, 14, 2, 1, 1)
        layout.addWidget(self.smoothSpnBx, 14, 3, 1, 1)
        layout.addWidget(self.tensionLabel, 11, 2, 1, 1)
        layout.addWidget(self.tensionSpnBx, 11, 3, 1, 1)
        layout.addWidget(self.deg1Label, 12, 2, 1, 1)
        layout.addWidget(self.deg1CB, 12, 3, 1, 1)
        layout.addWidget(self.tophat1Label, 13, 2, 1, 1)
        layout.addWidget(self.tophat1SpnBx, 13, 3, 1, 1)

    def _populate_automated_processing_layout(self, layout):
        """Populate automated processing controls."""
        layout.addWidget(self.optimizationMethodsLabel, 1, 0, 1, 2)
        layout.addWidget(self.optimizationMethodsList, 2, 0, 1, 2)
        layout.addWidget(self.stepsLabel, 3, 0, 1, 1)
        layout.addWidget(self.stepsLineEdit, 3, 1, 1, 3)
        layout.addWidget(self.maxIterationsLabel, 4, 0, 1, 1)
        layout.addWidget(self.maxIterationsSpnBx, 4, 1, 1, 1)
        layout.addWidget(self.earlyStopLabel, 4, 2, 1, 1)
        layout.addWidget(self.earlyStopSpnBx, 4, 3, 1, 1)

    def _setup_metrics_layout(self):
        """Setup evaluation metrics settings layout."""
        metric_layout = QGridLayout()
        metric_layout.addWidget(self.evaluationBaselineLabel, 0, 0, 1, 1)
        metric_layout.addWidget(self.evaluationBaselineSpnBx, 0, 1, 1, 1)
        metric_layout.addWidget(self.ampLabel, 1, 0, 1, 1)
        metric_layout.addWidget(self.ampSpnBx, 1, 1, 1, 1)
        metric_layout.addWidget(self.sigmaXDivLabel, 1, 2, 1, 1)
        metric_layout.addWidget(self.sigmaXDivSpnBx, 1, 3, 1, 1)
        metric_layout.addWidget(self.sigmaYDivLabel, 2, 0, 1, 1)
        metric_layout.addWidget(self.sigmaYDivSpnBx, 2, 1, 1, 1)
        metric_layout.addWidget(self.freqLabel, 2, 2, 1, 1)
        metric_layout.addWidget(self.freqCB, 2, 3, 1, 1)
        metric_layout.addWidget(self.lossParamsTable, 3, 0, 1, 4)
        metric_layout.addWidget(self.metricWeightsHintLabel, 4, 0, 1, 4)
        self.metricsGroup.setLayout(metric_layout)

    def _setup_rmin_rmax_layout(self):
        """Setup R-min/R-max settings layout."""
        rmin_layout = QGridLayout()
        rmin_layout.addWidget(self.showRminRmaxChkBx, 0, 0, 1, 2)
        rmin_layout.addWidget(self.rminLabel, 2, 0, 1, 1)
        rmin_layout.addWidget(self.rminSpnBx, 2, 1, 1, 1)
        rmin_layout.addWidget(self.rmaxLabel, 3, 0, 1, 1)
        rmin_layout.addWidget(self.rmaxSpnBx, 3, 1, 1, 1)
        rmin_layout.addWidget(self.setRminRmaxButton, 3, 2, 1, 2)
        rmin_layout.addWidget(self.fixedRadiusRangeChkBx, 5, 0, 1, 2)
        self.rminGroup.setLayout(rmin_layout)

    def _setup_image_processing_layout(self):
        """Setup image processing settings layout."""
        image_proc_layout = QGridLayout()
        image_proc_layout.addWidget(self.smoothImageChkbx, 4, 3, 1, 2)
        image_proc_layout.addWidget(self.downsampleLabel, 4, 0, 1, 1)
        image_proc_layout.addWidget(self.downsampleCB, 4, 1, 1, 1)
        self.imageProcGroup.setLayout(image_proc_layout)

    def _setup_evaluation_mask_layout(self):
        """Setup evaluation mask settings layout."""
        eval_mask_layout = QGridLayout()
        eval_mask_layout.addWidget(self.equatorMaskHeightLabel, 1, 0, 1, 1)
        eval_mask_layout.addWidget(self.equatorMaskHeightSpnBx, 1, 1, 1, 1)
        eval_mask_layout.addWidget(self.equatorCenterBeamLabel, 2, 0, 1, 1)
        eval_mask_layout.addWidget(self.equatorCenterBeamSpnBx, 2, 1, 1, 1)
        eval_mask_layout.addWidget(self.m1Label, 1, 2, 1, 1)
        eval_mask_layout.addWidget(self.m1SpnBx, 1, 3, 1, 1)
        eval_mask_layout.addWidget(self.layerLineWidthLabel, 2, 2, 1, 1)
        eval_mask_layout.addWidget(self.layerLineWidthSpnBx, 2, 3, 1, 1)
        self.evalMaskGroup.setLayout(eval_mask_layout)

    def _setup_evaluation_results_layout(self):
        """Setup evaluation results display layout."""
        eval_group_layout = QGridLayout()
        eval_group_layout.addWidget(self.bgMetricsTableTitle, 0, 0)
        eval_group_layout.addWidget(self.bgMetricsTable, 1, 0)
        self.evalGroup.setLayout(eval_group_layout)

    def _setup_folder_layout(self):
        """Setup folder processing settings layout."""
        folder_layout = QGridLayout()
        folder_layout.addWidget(self.chooseConfigurationsAutoChkBx)
        folder_layout.addWidget(self.createNewConfigurationsChkBx)
        folder_layout.addWidget(self.assignConfgurationsManually)
        self.folderGroup.setLayout(folder_layout)

    


    # ===== Current Background Configuration Display =====

    def _create_current_configs(self):
        self.currentBGMethodLabel = QLabel("None")
        self.currentBGModeLabel = QLabel("None")
        self.currentBGParamsLabel = QLabel("None")
        self.currentBGLossLabel = QLabel("None")
        current_summary_widget = self.build_current_config_summary_widget(
            method_label=self.currentBGMethodLabel,
            params_label=self.currentBGParamsLabel,
            loss_label=self.currentBGLossLabel,
            title="Current Configuration",
            min_params_width=520,
        )

        current_config_section = QWidget()
        current_config_layout = QGridLayout(current_config_section)
        current_config_layout.setContentsMargins(8, 8, 8, 8)
        current_config_layout.setSpacing(6)

        saved_configs_section = QWidget()
        saved_configs_layout = QGridLayout(saved_configs_section)
        saved_configs_layout.setContentsMargins(8, 8, 8, 8)
        saved_configs_layout.setSpacing(6)
        saved_configs_layout.addWidget(self.backgroundConfigsTable, 0, 0, 1, 4)

        configurationLayout = QGridLayout()
        configurationLayout.addWidget(current_config_section)
        configurationLayout.addWidget(saved_configs_section)
        self.configurationGroup.setLayout(configurationLayout)

        current_config_layout.addWidget(current_summary_widget, 0, 0, 1, 2, Qt.AlignLeft | Qt.AlignTop)
        current_config_layout.addWidget(self.addBackgroundConfigButton, 1, 0, 1, 1)

    def _hide_unhide_means_column(self):
        if self.lossParamsTable.isColumnHidden(0):
            self.lossParamsTable.setColumnHidden(0, False)
        else:
            self.lossParamsTable.setColumnHidden(0, True)

    def _set_selected_methods(self, methods):
        method_set = set(methods)
        for i in range(self.optimizationMethodsList.count()):
            item = self.optimizationMethodsList.item(i)
            item.setSelected(item.text() in method_set)

    def _update_processing_mode_visibility(self):
        automated = self.processingModeCB.currentText() == "Automated"
        self.manualGroup.setVisible(not automated)
        self.autoGroup.setVisible(automated)
        self.optimizeFlag = automated

    def _populate_loss_params_table(self):
        """Sync internal SpinBox values to the editable table."""
        data = [
            (self.meanMSESpnBx, self.weightMSESpnBx),
            (self.meanNegSynSpnBx, self.weightNegSynSpnBx),
            (self.meanNonBaselineSpnBx, self.weightNonBaselineSpnBx),
            (self.meanNegConSpnBx, self.weightNegConSpnBx),
            (self.meanSmoothSpnBx, self.weightSmoothSpnBx),
        ]
        self.lossParamsTable.blockSignals(True)
        for row, (mean_sb, weight_sb) in enumerate(data):
            self.lossParamsTable.setItem(row, 0, QTableWidgetItem(f"{mean_sb.value():.4g}"))
            self.lossParamsTable.setItem(row, 1, QTableWidgetItem(f"{weight_sb.value():.4g}"))
        self.lossParamsTable.blockSignals(False)

    def _on_loss_params_table_changed(self, item):
        """Sync table edits back to the internal SpinBoxes."""
        row, col = item.row(), item.column()
        try:
            val = float(item.text())
            data = [
                (self.meanMSESpnBx, self.weightMSESpnBx),
                (self.meanNegSynSpnBx, self.weightNegSynSpnBx),
                (self.meanNonBaselineSpnBx, self.weightNonBaselineSpnBx),
                (self.meanNegConSpnBx, self.weightNegConSpnBx),
                (self.meanSmoothSpnBx, self.weightSmoothSpnBx),
            ]
            spinbox = data[row][col]
            spinbox.setValue(val)
        except ValueError:
            # Revert on invalid input
            self._populate_loss_params_table()



    # ==== Evaluation Table ======

    def _set_table_item(self, row, col, text):
        item = QTableWidgetItem(text)
        item.setFlags(item.flags() & ~Qt.ItemIsEditable)
        self.bgMetricsTable.setItem(row, col, item)
        
    def _clear_bg_metrics_table(self):
        if not hasattr(self, 'bgMetricsTable'):
            return
        if hasattr(self, 'bgMetricsTableTitle'):
            self.bgMetricsTableTitle.setText("Loss = —")
        self.bgMetricsTable.setRowCount(0)
    
    def _update_bg_metrics_table(self):
        if not hasattr(self, 'bgMetricsTable'):
            return

        parent = self._get_parent_gui()
        if parent is None or parent.quadFold is None:
            self._clear_bg_metrics_table()
            return

        result_bg = parent.quadFold.info.get('result_bg', {})
        raw_metrics = result_bg.get('metrics_raw', {}) or {}
        norm_metrics = result_bg.get('metrics_normalized', {}) or {}
        loss = result_bg.get('loss', None)

        if hasattr(self, 'bgMetricsTableTitle'):
            loss_text = _to_metric_text(loss)
            self.bgMetricsTableTitle.setText(f"Loss = {loss_text}")

        metric_rows = [
            ("MSE of Synthetic Signal, intst. cnt.", "MSE"),
            ("Fraction of Synthetic Oversubtraction, %", "Share_Neg_Synthetic"),
            ("Fraction of Non Near Baseline Pixels, %", "Share_Non_Baseline"),
            ("Fraction of Negative Connected Pixels, %", "Share_Neg_Connected"),
            ("Smoothness Metric, intst. cnt.", "Smoothness"),
        ]
        fraction_metric_keys = {
            "Share_Neg_Synthetic",
            "Share_Non_Baseline",
            "Share_Neg_Connected",
        }

        # Loss metrics mapping for weights
        weight_keys = {
            "MSE": "MSE",
            "Share_Neg_Synthetic": "Neg_Synthetic",
            "Share_Non_Baseline": "Non_Baseline",
            "Share_Neg_Connected": "Neg_Connected",
            "Smoothness": "Smoothness"
        }

        weights = {}
        if hasattr(self, 'weightMSESpnBx'):
            weights = {
                "MSE": self.weightMSESpnBx.value(),
                "Neg_Synthetic": self.weightNegSynSpnBx.value(),
                "Non_Baseline": self.weightNonBaselineSpnBx.value(),
                "Neg_Connected": self.weightNegConSpnBx.value(),
                "Smoothness": self.weightSmoothSpnBx.value()
            }

        mapped_keys = {raw_key for _, raw_key in metric_rows}
        available_keys = set(raw_metrics.keys()) | set(norm_metrics.keys())
        for key in sorted(available_keys):
            if key not in mapped_keys and key != "Loss":
                metric_rows.append((key, key))

        if len(metric_rows) == 0:
            self._clear_bg_metrics_table()
            return

        self.bgMetricsTable.setRowCount(len(metric_rows))
        for row, (label, raw_key) in enumerate(metric_rows):
            self._set_table_item(row, 0, label)
            raw_value = raw_metrics.get(raw_key, None)
            norm_value = norm_metrics.get(raw_key, None)
            
            self._set_table_item(row, 1, _to_metric_text(raw_value, as_percent=raw_key in fraction_metric_keys))
            self._set_table_item(row, 2, _to_metric_text(norm_value))

            # Calculate and set contribution as percentage of total loss
            contribution_text = "—"
            if norm_value is not None and loss and float(loss) > 0:
                w_key = weight_keys.get(raw_key)
                if w_key and w_key in weights:
                    contribution_val = (float(norm_value) * float(weights[w_key])) / float(loss)
                    contribution_text = _to_metric_text(contribution_val, as_contribution=True)
            
            self._set_table_item(row, 3, contribution_text)
   

    # ==== Background Config Table & Configuration Cache ======

    def _set_config_table_item(self, row, col, text):
        item = QTableWidgetItem(text)
        item.setFlags(item.flags() & ~Qt.ItemIsEditable)
        self.backgroundConfigsTable.setItem(row, col, item)

    def _find_config_row_by_name(self, name):
        if not hasattr(self, 'backgroundConfigsTable'):
            return -1
        for row in range(self.backgroundConfigsTable.rowCount()):
            item = self.backgroundConfigsTable.item(row, 0)
            if item is not None and item.text() == name:
                return row
        return -1

    def _upsert_background_configuration(self, name, method, params_text, loss_text):
        row = self._find_config_row_by_name(name)
        if row < 0:
            row = self.backgroundConfigsTable.rowCount()
            self.backgroundConfigsTable.insertRow(row)
        self._set_config_table_item(row, 0, name)
        self._set_config_table_item(row, 1, method)
        self._set_config_table_item(row, 2, params_text)
        self._set_config_table_item(row, 3, loss_text)

    def _clear_background_configurations_table(self):
        if hasattr(self, 'backgroundConfigsTable'):
            self.backgroundConfigsTable.setRowCount(0)
        self.backgroundConfigurations = []
        self._on_background_config_selection_changed()

    def _on_background_config_selection_changed(self):
        if not hasattr(self, 'deleteBackgroundConfigButton'):
            return
        has_selection = len(self.backgroundConfigsTable.selectionModel().selectedRows()) > 0
        self.deleteBackgroundConfigButton.setEnabled(has_selection)

    def _show_background_config_context_menu(self, pos):
        if not hasattr(self, 'backgroundConfigsTable'):
            return
        row = self.backgroundConfigsTable.rowAt(pos.y())
        if row < 0:
            return
        self.backgroundConfigsTable.selectRow(row)
        menu = QMenu(self)
        delete_action = menu.addAction("Delete Configuration")
        chosen = menu.exec_(self.backgroundConfigsTable.viewport().mapToGlobal(pos))
        if chosen == delete_action:
            self.deleteSelectedBackgroundConfiguration()

    def deleteSelectedBackgroundConfiguration(self):
        """Delete selected background configuration row and its in-memory entry."""
        if not hasattr(self, 'backgroundConfigsTable'):
            return

        selected_rows = self.backgroundConfigsTable.selectionModel().selectedRows()
        if len(selected_rows) == 0:
            QMessageBox.information(self, "Delete Configuration", "Please select a configuration row to delete.")
            return

        row = selected_rows[0].row()
        name_item = self.backgroundConfigsTable.item(row, 0)
        config_name = name_item.text() if name_item is not None else "this configuration"

        reply = QMessageBox.question(
            self,
            "Delete Configuration",
            f"Delete '{config_name}'?",
            QMessageBox.Yes | QMessageBox.No,
            QMessageBox.No,
        )
        if reply != QMessageBox.Yes:
            return

        self.backgroundConfigsTable.removeRow(row)
        self.backgroundConfigurations = [
            c for c in self.backgroundConfigurations if c.get('name') != config_name
        ]
        self._on_background_config_selection_changed()
        self._save_background_configurations_to_cache()

    def addBackgroundConfiguration(self, name=None):
        """Add current background method/result into Background Configurations table with a user-defined name."""
        if not self._able_to_process():
            return
        parent = self._get_parent_gui()
        if not name:
            name, ok = QInputDialog.getText(
                self,
                "Save Configuration",
                "Enter configuration name:"
            )
            name = (name or "").strip()
            if not ok or not name:
                return

        if parent is None or parent.quadFold is None:
            return

        result_bg = parent.quadFold.info.get('result_bg', {}) or {}
        method = result_bg.get('method', None)
        if method in (None, ""):
            method = parent.quadFold.info.get('bgsub', 'None')

        params = result_bg.get('final_params', None)
        config_context = self._get_background_configuration_context()
        params_text = self._format_bg_params_text(params)
        loss_value = result_bg.get('loss', None)
        loss_text = "—" if loss_value is None else _to_metric_text(loss_value)

        self._upsert_background_configuration(
            name=name,
            method=str(method),
            params_text=params_text,
            loss_text=loss_text,
        )

        _, _, additional_info = self._get_optimization_cache_file_and_key()

        config_entry = {
            'name': name,
            'method': str(method),
            'params': params if isinstance(params, dict) else None,
            'loss': loss_value,
            'downsample': config_context.get('downsample', 1),
            'smooth_image': config_context.get('smooth_image', False),
            'additional_info': additional_info or {},
        }
        existing_idx = next((i for i, c in enumerate(self.backgroundConfigurations) if c.get('name') == name), -1)
        if existing_idx >= 0:
            self.backgroundConfigurations[existing_idx] = config_entry
        else:
            self.backgroundConfigurations.append(config_entry)
        self._save_background_configurations_to_cache()

    ### Configration cache management
    def _get_optimization_cache_file_and_key(self):
        """Build shared optimization cache path/key matching QuadrantFolder."""
        parent = self._get_parent_gui()
        if parent is None or not getattr(parent, "filePath", None):
            return None, None, None

        cache_dir = fullPath(parent.filePath, "qf_cache")
        createFolder(cache_dir)
        cache_file = fullPath(cache_dir, "background_cache.json")

        flags = parent.getFlags() if parent is not None else {}
        dataset_key = Path(parent.filePath).resolve().name
        additional_info = {
            'methods': flags.get('methods', []),
            'steps': flags.get('steps', []),
            'early_stop': flags.get('early_stop', 0.01),
            'max_iterations': flags.get('max_iterations', 15),
            'mean_metric_values': flags.get('mean_metric_values', None),
            'metric_weights': flags.get('metric_weights', None),
            'detector': flags.get('detector', None),
            'orientation_model': flags.get('orientation_model', None),
        }
        return cache_file, dataset_key, additional_info

    def _get_background_configuration_context(self):
        parent = self._get_parent_gui()
        flags = parent.getFlags() if parent is not None else {}
        return {
            'downsample': int(flags.get('downsample', 1) or 1),
            'smooth_image': bool(flags.get('smooth_image', False)),
        }
    
    def _save_background_configurations_to_cache(self):
        cache_file, cache_key, additional_info = self._get_optimization_cache_file_and_key()
        if not cache_file or cache_key is None:
            return

        try:
            set_user_background_configurations(
                cache_file,
                cache_key,
                self.backgroundConfigurations,
                additional_info=additional_info,
            )
        except Exception as e:
            print(f"Failed to save background configurations to cache: {e}")

    def _load_background_configurations_from_cache(self):
        cache_file, cache_key, additional_info = self._get_optimization_cache_file_and_key()
        self._clear_background_configurations_table()
        if not cache_file or cache_key is None:
            return

        try:
            configs = get_user_background_configurations(cache_file, cache_key)
        except Exception as e:
            print(f"Failed to load background configurations from cache: {e}")
            return

        for config in configs:
            name = str(config.get('name', '') or '').strip()
            method = str(config.get('method', 'None') or 'None')
            params = config.get('params', {}) if isinstance(config.get('params', {}), dict) else {}
            loss_value = config.get('loss', None)
            downsample = int(config.get('downsample', 1) or 1)
            smooth_image = bool(config.get('smooth_image', False))
            row_additional_info = config.get('additional_info', additional_info)
            params_text = self._format_bg_params_text(params)
            loss_text = "—" if loss_value is None else _to_metric_text(loss_value)

            if not name:
                name = f"Config {self.backgroundConfigsTable.rowCount() + 1}"

            self._upsert_background_configuration(
                name=name,
                method=method,
                params_text=params_text,
                loss_text=loss_text,
            )

            self.backgroundConfigurations.append({
                'name': name,
                'method': method,
                'params': params,
                'loss': loss_value,
                'downsample': downsample,
                'smooth_image': smooth_image,
                'additional_info': row_additional_info,
            })

        self._on_background_config_selection_changed()
    
    def _read_background_configurations(self):
        """
        Read saved user background configurations from cache for the current folder context.
        Returns only minimally valid rows (method + params dict).
        """
        cache_file, cache_key, additional_info = self._get_optimization_cache_file_and_key()
        if not cache_file or cache_key is None:
            return []

        try:
            configs = get_user_background_configurations(cache_file, cache_key)
        except Exception as e:
            print(f"Failed to read background configurations for batch processing: {e}")
            return []

        cleaned = []
        for config in configs:
            if not isinstance(config, dict):
                continue
            method = str(config.get('method', '') or '').strip()
            params = config.get('params', {})
            if not method or not isinstance(params, dict):
                continue
            cleaned.append({
                'name': str(config.get('name', '') or '').strip(),
                'method': method,
                'params': dict(params),
            })
        return cleaned


    # ===== Manual Assignment for Batch Processing ======
    def _on_manual_assignment_accepted(self, dialog):
        self.manualBackgroundAssignments = dialog.get_assignments()
        QMessageBox.information(
            self,
            "Manual Assignment",
            f"Saved manual assignments for {len(self.manualBackgroundAssignments)} image(s)."
        )

    def openManualBackgroundAssignmentsDialog(self):
        """Open dialog to manually assign saved background configurations to images."""
        parent = self._get_parent_gui()
        if parent is None or parent.file_manager is None or not parent.file_manager.names:
            QMessageBox.information(self, "Manual Assignment", "Please load a folder with images first.")
            return

        config_names = [str(c.get('name', '') or '').strip() for c in self.backgroundConfigurations]
        config_names = [n for n in config_names if n]
        if len(config_names) == 0:
            QMessageBox.information(
                self,
                "Manual Assignment",
                "No saved background configurations are available. Add configurations first."
            )
            return

        dialog = ManualBackgroundAssignmentDialog(
            image_names=parent.file_manager.names,
            configuration_names=config_names,
            current_assignments=self.manualBackgroundAssignments,
            parent=self,
        )
        dialog.accepted.connect(lambda: self._on_manual_assignment_accepted(dialog))
        dialog.show()

    def _resolve_manual_background_assignments_for_batch(self, configurations):
        """
        Resolve manual image->configuration-name selections into image->configuration records.
        """
        if not isinstance(configurations, list) or len(configurations) == 0:
            return {}

        config_by_name = {}
        for config in configurations:
            if not isinstance(config, dict):
                continue
            name = str(config.get('name', '') or '').strip()
            method = str(config.get('method', '') or '').strip()
            params = config.get('params', {})
            if not name or not method or not isinstance(params, dict):
                continue
            config_by_name[name] = {
                'name': name,
                'method': method,
                'params': dict(params),
            }

        resolved = {}
        for img_name, cfg_name in (self.manualBackgroundAssignments or {}).items():
            cfg_key = str(cfg_name or '').strip()
            if cfg_key in config_by_name:
                resolved[str(img_name)] = copy.deepcopy(config_by_name[cfg_key])
        return resolved
    









# === Utility Functions for Metrics Formatting ===
def _to_metric_text(value, as_percent=False, as_contribution=False):
    if value is None:
        return "—"
    try:
        numeric = float(value)
        if as_contribution:
            # Format as "+ X.X%" or "0.0%"
            prefix = "+" if numeric > 0.004 else ""
            return f"{prefix}{numeric * 100:.0f} %"
        if as_percent:
            return f"{numeric * 100:.0f} %"
        return f"{numeric:.2f}"
    except Exception:
        return str(value)

def _fraction_to_percent_for_ui(value):
    """Map stored fraction values (0..1) to percentage UI values (0..100)."""
    try:
        numeric = float(value)
    except Exception:
        return value
    if 0.0 <= numeric <= 1.0:
        return numeric * 100.0
    return numeric

def _percent_to_fraction_for_flags(value):
    """Map percentage UI values (0..100) to stored fraction values (0..1)."""
    try:
        return float(value) / 100.0
    except Exception:
        return value
