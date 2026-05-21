"""
Background subtraction settings popup for Quadrant Folding.
"""

import copy

from .pyqt_utils import *
from .widgets.collapsible_groupbox import CollapsibleGroupBox
from .ManualBackgroundAssignmentDialog import ManualBackgroundAssignmentDialog
from ..utils.optimization_cache import get_user_background_configurations, set_user_background_configurations
from ..utils.file_manager import *
from ..utils import qf_defaults
from pathlib import Path


class BackgroundSubtractionDialog(QDialog):
    """Popup window that contains all background subtraction controls."""
    
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
        spnbx.setDecimals(decimals)
        spnbx.setSingleStep(step)
        spnbx.setValue(value)
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
        self._create_background_out_parameter_widgets()
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
            "<a href='https://musclex.readthedocs.io/en/latest/AppSuite/QuadrantFolding/Quadrant-Folding--Background-Subtraction.html'>"
            "Background Subtraction documentation"
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

        self.allBGChoices = qf_defaults.BG_METHODS
        self.bgChoiceIn = QComboBox()
        self.bgChoiceIn.setCurrentIndex(0)
        for c in self.allBGChoices:
            self.bgChoiceIn.addItem(c)
        self.bgChoiceOut = QComboBox()
        self.bgChoiceOut.setCurrentIndex(0)
        for c in self.allBGChoices:
            self.bgChoiceOut.addItem(c)

    def _create_rmin_rmax_widgets(self):
        """Create R-min/R-max settings widgets."""
        self.setRminRmaxButton = QPushButton("Manual R-min/max")
        self.setRminRmaxButton.setCheckable(True)

        self.rminSpnBx = self._create_spinbox(min_val=qf_defaults.RMIN_RMAX_RANGE[0], max_val=qf_defaults.RMIN_RMAX_RANGE[1], 
                                              value=qf_defaults.DEFAULT_RMIN_RMAX, step=2)
        self.rminLabel = QLabel("R-min")

        self.rmaxSpnBx = self._create_spinbox(min_val=qf_defaults.RMIN_RMAX_RANGE[0], max_val=qf_defaults.RMIN_RMAX_RANGE[1], 
                                              value=qf_defaults.DEFAULT_RMIN_RMAX, step=2)
        self.rmaxLabel = QLabel("R-max")

        self.showRminRmaxChkBx = QCheckBox("Show R-min/max")
        self.fixedRadiusRangeChkBx = QCheckBox("Persist R-min/max")

    def _create_image_processing_widgets(self):
        """Create image processing settings widgets."""
        self.downsampleLabel = QLabel("Downsample")
        self.downsampleCB = QComboBox()
        self.downsampleCB.addItems(qf_defaults.DOWNSAMPLE_OPTIONS)
        self.downsampleCB.setCurrentIndex(qf_defaults.DEFAULT_DOWNSAMPLE_INDEX)
        self.downsampleCB.currentIndexChanged.connect(self._persist_image_processing_settings_to_image_cache)

        self.smoothImageChkbx = QCheckBox("Smooth Image")
        self.smoothImageChkbx.setChecked(True)
        self.smoothImageChkbx.toggled.connect(self._persist_image_processing_settings_to_image_cache)

    def _create_evaluation_mask_widgets(self):
        """Create evaluation mask settings widgets."""

        self.equatorMaskHeightLabel = QLabel("Equator Height : ")
        self.equatorMaskHeightSpnBx = self._create_spinbox(min_val=qf_defaults.EQUATOR_HEIGHT_RANGE[0], 
                                                        max_val=qf_defaults.EQUATOR_HEIGHT_RANGE[1], 
                                                        value=qf_defaults.DEFAULT_EQUATOR_HEIGHT)

        self.equatorCenterBeamLabel = QLabel("Equator Center Radius : ")
        self.equatorCenterBeamSpnBx = self._create_spinbox(min_val=qf_defaults.EQUATOR_CENTER_RANGE[0], 
                                                           max_val=qf_defaults.EQUATOR_CENTER_RANGE[1], 
                                                           value=qf_defaults.DEFAULT_EQUATOR_CENTER)

        self.m1Label = QLabel("Layer line spacing : ")
        self.m1SpnBx = self._create_spinbox(min_val=qf_defaults.LAYER_LINE_RANGE[0], 
                                            max_val=qf_defaults.LAYER_LINE_RANGE[1], 
                                            value=qf_defaults.DEFAULT_LAYER_SPACING)

        self.layerLineWidthLabel = QLabel("Layer line width : ")
        self.layerLineWidthSpnBx = self._create_spinbox(min_val=qf_defaults.LAYER_LINE_RANGE[0], 
                                                        max_val=qf_defaults.LAYER_LINE_RANGE[1], 
                                                        value=qf_defaults.DEFAULT_LAYER_WIDTH)

    def _create_background_parameter_widgets(self):
        """Create background subtraction parameter widgets."""
        # Gaussian parameters
        self.gaussFWHMLabel = QLabel("Gaussian FWHM : ")
        self.gaussFWHM = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                              value=qf_defaults.DEFAULT_GAUSSIAN_FWHM)

        # Boxcar parameters
        self.boxcarLabel = QLabel("Box Car Size : ")
        self.boxcarX = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                            value=qf_defaults.DEFAULT_BOXCAR_SIZE, prefix='X:')
        self.boxcarY = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                            value=qf_defaults.DEFAULT_BOXCAR_SIZE, prefix='Y:')

        # Cycle parameters
        self.cycleLabel = QLabel("Number of Cycles : ")
        self.cycle = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                          value=qf_defaults.DEFAULT_CYCLES)

        # Window size parameters
        self.windowSizeLabel = QLabel("Window Size : ")
        self.winSizeX = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                             value=qf_defaults.DEFAULT_WINDOW_SIZE, prefix='X:')
        self.winSizeY = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                             value=qf_defaults.DEFAULT_WINDOW_SIZE, prefix='Y:')

        # Window separation parameters
        self.windowSepLabel = QLabel("Window Separation : ")
        self.winSepX = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                            value=qf_defaults.DEFAULT_WINDOW_SEP, prefix='X:')
        self.winSepY = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                            value=qf_defaults.DEFAULT_WINDOW_SEP, prefix='Y:')

        # Pixel range parameters
        self.minPixRange = self._create_double_spinbox(min_val=qf_defaults.PIXEL_RANGE_LIMIT[0], 
                                                       max_val=qf_defaults.PIXEL_RANGE_LIMIT[1], 
                                                       value=qf_defaults.DEFAULT_PIXEL_MIN, 
                                                       decimals=2, step=2, suffix="%")
        self.maxPixRange = self._create_double_spinbox(min_val=qf_defaults.PIXEL_RANGE_LIMIT[0], 
                                                       max_val=qf_defaults.PIXEL_RANGE_LIMIT[1], 
                                                       value=qf_defaults.DEFAULT_PIXEL_MAX, 
                                                       decimals=2, step=2, suffix="%")
        self.pixRangeLabel = QLabel("Pixel Range : ")

        # Theta bin
        self.thetaBinLabel = QLabel("Bin Theta (deg) : ")
        self.thetabinCB = QComboBox()
        self.thetabinCB.addItems(qf_defaults.THETA_BIN_OPTIONS)
        self.thetabinCB.setCurrentIndex(qf_defaults.DEFAULT_THETA_BIN_INDEX)

        # Radial bin
        self.radialBinLabel = QLabel("Radial Bin : ")
        self.radialBinSpnBx = self._create_spinbox(min_val=qf_defaults.RADIAL_BIN_RANGE[0], 
                                                   max_val=qf_defaults.RADIAL_BIN_RANGE[1], 
                                                   value=qf_defaults.DEFAULT_RADIAL_BIN, suffix=" Pixel(s)")

        # Smoothing and tension
        self.smoothLabel = QLabel("Smoothing factor : ")
        self.smoothSpnBx = self._create_double_spinbox(min_val=0, max_val=10000, 
                                                       value=qf_defaults.DEFAULT_SMOOTHING, 
                                                       decimals=2, step=0.1)

        self.tensionLabel = QLabel("Tension factor : ")
        self.tensionSpnBx = self._create_double_spinbox(min_val=0, max_val=100, 
                                                        value=qf_defaults.DEFAULT_TENSION, 
                                                        decimals=2, step=0.1)

        # Top-hat
        self.tophatLabel = QLabel("Top-hat Disk Size: ")
        self.tophatSpnBx = self._create_spinbox(min_val=qf_defaults.TOPHAT_RANGE[0], 
                                                 max_val=qf_defaults.TOPHAT_RANGE[1], 
                                                 value=qf_defaults.DEFAULT_TOPHAT_SIZE)

        # Degree
        self.degreeLabel = QLabel("Step Degree : ")
        self.degreeCB = QComboBox()
        self.degreeCB.addItems(qf_defaults.DEGREE_OPTIONS)
        self.degreeCB.setCurrentIndex(qf_defaults.DEFAULT_DEGREE_INDEX)

    def _create_background_out_parameter_widgets(self):
        """Create background subtraction parameter widgets for outer background."""
        # Gaussian parameters
        self.gaussFWHMOutLabel = QLabel("Gaussian FWHM : ")
        self.gaussFWHMOut = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                              value=qf_defaults.DEFAULT_GAUSSIAN_FWHM)

        # Boxcar parameters
        self.boxcarOutLabel = QLabel("Box Car Size : ")
        self.boxcarOutX = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                            value=qf_defaults.DEFAULT_BOXCAR_SIZE, prefix='X:')
        self.boxcarOutY = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                            value=qf_defaults.DEFAULT_BOXCAR_SIZE, prefix='Y:')

        # Cycle parameters
        self.cycleOutLabel = QLabel("Number of Cycles : ")
        self.cycleOut = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                          value=qf_defaults.DEFAULT_CYCLES)

        # Window size parameters
        self.windowSizeOutLabel = QLabel("Window Size : ")
        self.winSizeOutX = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                             value=qf_defaults.DEFAULT_WINDOW_SIZE, prefix='X:')
        self.winSizeOutY = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                             value=qf_defaults.DEFAULT_WINDOW_SIZE, prefix='Y:')

        # Window separation parameters
        self.windowSepOutLabel = QLabel("Window Separation : ")
        self.winSepOutX = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                            value=qf_defaults.DEFAULT_WINDOW_SEP, prefix='X:')
        self.winSepOutY = self._create_spinbox(min_val=qf_defaults.BG_PARAM_RANGE[0], max_val=qf_defaults.BG_PARAM_RANGE[1], 
                                            value=qf_defaults.DEFAULT_WINDOW_SEP, prefix='Y:')

        # Pixel range parameters
        self.minPixRangeOut = self._create_double_spinbox(min_val=qf_defaults.PIXEL_RANGE_LIMIT[0], 
                                                       max_val=qf_defaults.PIXEL_RANGE_LIMIT[1], 
                                                       value=qf_defaults.DEFAULT_PIXEL_MIN, 
                                                       decimals=2, step=2, suffix="%")
        self.maxPixRangeOut = self._create_double_spinbox(min_val=qf_defaults.PIXEL_RANGE_LIMIT[0], 
                                                       max_val=qf_defaults.PIXEL_RANGE_LIMIT[1], 
                                                       value=qf_defaults.DEFAULT_PIXEL_MAX, 
                                                       decimals=2, step=2, suffix="%")
        self.pixRangeLabelOut = QLabel("Pixel Range : ")

        # Theta bin
        self.thetaBinOutLabel = QLabel("Bin Theta (deg) : ")
        self.thetaBinOutCB = QComboBox()
        self.thetaBinOutCB.addItems(qf_defaults.THETA_BIN_OPTIONS)
        self.thetaBinOutCB.setCurrentIndex(qf_defaults.DEFAULT_THETA_BIN_INDEX)

        # Radial bin
        self.radialBinOutLabel = QLabel("Radial Bin : ")
        self.radialBinOutSpnBx = self._create_spinbox(min_val=qf_defaults.RADIAL_BIN_RANGE[0], 
                                                   max_val=qf_defaults.RADIAL_BIN_RANGE[1], 
                                                   value=qf_defaults.DEFAULT_RADIAL_BIN, suffix=" Pixel(s)")

        # Smoothing and tension
        self.smoothOutLabel = QLabel("Smoothing factor : ")
        self.smoothOutSpnBx = self._create_double_spinbox(min_val=0, max_val=10000, 
                                                       value=qf_defaults.DEFAULT_SMOOTHING, 
                                                       decimals=2, step=0.1)

        self.tensionOutLabel = QLabel("Tension factor : ")
        self.tensionOutSpnBx = self._create_double_spinbox(min_val=0, max_val=100, 
                                                        value=qf_defaults.DEFAULT_TENSION, 
                                                        decimals=2, step=0.1)

        # Top-hat
        self.tophatOutLabel = QLabel("Top-hat Disk Size: ")
        self.tophatOutSpnBx = self._create_spinbox(min_val=qf_defaults.TOPHAT_RANGE[0], 
                                                   max_val=qf_defaults.TOPHAT_RANGE[1], 
                                                   value=qf_defaults.DEFAULT_TOPHAT_SIZE)

        self.tranRLabel = QLabel("Transition Radius : ")
        self.tranRSpnBx = self._create_spinbox(min_val=-1, 
                                                   max_val=4000, 
                                                   value=400)
        
        self.tranDeltaLabel = QLabel("Transition Delta : ")
        self.tranDeltaSpnBx = self._create_spinbox(min_val=-1, 
                                                   max_val=2000, 
                                                   value=60)
        
        self.showTranRadDeltaChkBx = QCheckBox("Show Transition Radius and Delta")
        self.showTranRadDeltaChkBx.setToolTip("Draw the transition radius and delta circles on the folded image")


    def _create_optimization_widgets(self):
        """Create optimization/automated processing widgets."""
        self.optimizeFlag = False

        self.optimizationMethodsLabel = QLabel("BG Subtraction Methods (Multi-select):")
        self.optimizationMethodsList = QListWidget()
        self.optimizationMethodsList.setSelectionMode(QAbstractItemView.MultiSelection)
        self.optimizationMethodsList.setMaximumHeight(150)
        self.optimizationMethods = qf_defaults.OPTIMIZATION_METHODS
        for method in self.optimizationMethods:
            self.optimizationMethodsList.addItem(method)

        # Default methods aligned with current optimization workflow
        self._set_selected_methods(qf_defaults.DEFAULT_OPTIMIZATION_METHODS)

        self.stepsLabel = QLabel("Step Sizes:")
        self.stepsLineEdit = QLineEdit(qf_defaults.DEFAULT_OPTIMIZATION_STEPS)
        self.stepsLineEdit.setToolTip("Comma-separated values used for optimization step schedule.")

        self.maxIterationsLabel = QLabel("Max Iterations:")
        self.maxIterationsSpnBx = self._create_spinbox(min_val=qf_defaults.ITERATIONS_RANGE[0], 
                                                       max_val=qf_defaults.ITERATIONS_RANGE[1], 
                                                       value=qf_defaults.DEFAULT_MAX_ITERATIONS,
            tooltip="Maximum number of candidate (+/- step) evaluations per background subtraction parameter.")

        self.earlyStopLabel = QLabel("Early Stop Loss Threshold:")
        self.earlyStopSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1.0, 
                                                          value=qf_defaults.DEFAULT_EARLY_STOP,
            decimals=4, step=0.005,
            tooltip="Threshold for early stopping during optimization per background subtraction parameter.")
        
        

    def _create_evaluation_metric_widgets(self):
        """Create normalization means, evaluation metrics, and metric weight widgets."""
        # ===== Normalization Means (Used in table) =====
        self.meanMSELabel = self._create_label("Normalized MSE of Synthetic Signal, intst. cnts.", "small")
        self.meanMSESpnBx = self._create_double_spinbox(min_val=1e-6, max_val=1e9, 
                                                        value=qf_defaults.DEFAULT_MEAN_MSE, decimals=3)

        self.meanNegSynLabel = self._create_label("Fraction of Synthetic Oversubtraction, %", "small")
        self.meanNegSynSpnBx = self._create_double_spinbox(min_val=1e-6, max_val=100.0, 
                                                           value=_fraction_to_percent_for_ui(qf_defaults.DEFAULT_MEAN_NEG_SYN), 
                                                           decimals=3, suffix=" %")

        self.meanNonBaselineLabel = self._create_label("Fraction of Non Near-Zero Baseline Pixels, %", "small")
        self.meanNonBaselineSpnBx = self._create_double_spinbox(min_val=1e-6, max_val=100.0, 
                                                                value=_fraction_to_percent_for_ui(qf_defaults.DEFAULT_MEAN_BASELINE), 
                                                                decimals=3, suffix=" %")

        self.meanNegConLabel = self._create_label("Fraction of Negative Connected Pixels, %", "small")
        self.meanNegConSpnBx = self._create_double_spinbox(min_val=1e-6, max_val=100.0, 
                                                           value=_fraction_to_percent_for_ui(qf_defaults.DEFAULT_MEAN_NEG_CON), 
                                                           decimals=3, suffix=" %")

        self.meanSmoothLabel = self._create_label("Smoothness Metric", "small")
        self.meanSmoothSpnBx = self._create_double_spinbox(min_val=1e-6, max_val=1e6, 
                                                           value=qf_defaults.DEFAULT_MEAN_SMOOTH, decimals=3)

        # ===== Evaluation Metrics Settings =====
        self.evaluationBaselineLabel = self._create_label("Evaluation Baseline:", "small")
        self.evaluationBaselineSpnBx = self._create_double_spinbox(min_val=qf_defaults.MIN_EVAL_BASELINE, max_val=1e9, 
                                                                   value=qf_defaults.DEFAULT_EVAL_BASELINE,
            decimals=3, step=0.01,
            tooltip="Baseline value for near-zero pixel evaluation.")
        self.persistEvaluationBaselineChkBx = QCheckBox("Persist evaluation baseline")
        self.persistEvaluationBaselineChkBx.setToolTip(
            "When checked, the evaluation baseline value is carried over when you switch images "
            "(same as the spinbox). When unchecked, the baseline is recomputed for each image."
        )

        self.persistSyntheticDataChkBx = QCheckBox("Persist synthetic data")
        self.persistSyntheticDataChkBx.setToolTip(
            "When checked, synthetic Gaussian parameters (amplitude, sigma X, sigma Y) "
            "are carried over when you switch images. "
            "When unchecked, they are recomputed per image from the equator / meridian "
            "profile and shown on the UI."
        )

        self.amplitudeLabel = self._create_label("Synthetic Amplitude:", "small")
        self.amplitudeSpnBx = self._create_double_spinbox(min_val=qf_defaults.MIN_SYNTHETIC_AMPLITUDE, max_val=1e6, 
                                value=qf_defaults.DEFAULT_SYNTHETIC_AMPLITUDE, 
                                decimals=0, step=0.001)

        self.sigmaXLabel = self._create_label("Sigma X:", "small")
        self.sigmaXSpnBx = self._create_double_spinbox(min_val=qf_defaults.MIN_SYNTHETIC_SIGMA_X, max_val=1e6, 
                                  value=qf_defaults.DEFAULT_SYNTHETIC_SIGMA_X, 
                                  decimals=2, step=0.5)

        self.sigmaYLabel = self._create_label("Sigma Y:", "small")
        self.sigmaYSpnBx = self._create_double_spinbox(min_val=qf_defaults.MIN_SYNTHETIC_SIGMA_Y, max_val=1e6, 
                                  value=qf_defaults.DEFAULT_SYNTHETIC_SIGMA_Y, 
                                  decimals=2, step=0.5)

        self.freqLabel = self._create_label("Sampling Frequency:", "small")
        self.freqCB = QComboBox()
        self.freqCB.addItems(qf_defaults.FREQ_OPTIONS)
        self.freqCB.setCurrentText(qf_defaults.DEFAULT_FREQ)

        # ===== Metric Weights (Used in table) =====
        self.metricWeightsLabel = QLabel("Metric Weights:")
        self.weightMSELabel = self._create_label("Normalized MSE of Synthetic Signal, intst. cnts.", "small")
        self.weightMSESpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                          value=qf_defaults.DEFAULT_WEIGHT_MSE, decimals=2)

        self.weightNegSynLabel = self._create_label("Fraction of Synthetic Oversubtraction, %", "small")
        self.weightNegSynSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                             value=qf_defaults.DEFAULT_WEIGHT_NEG_SYN, decimals=2)

        self.weightNonBaselineLabel = self._create_label("Fraction of Non Near-Zero Baseline Pixels, %", "small")
        self.weightNonBaselineSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                                  value=qf_defaults.DEFAULT_WEIGHT_BASELINE, decimals=2)

        self.weightNegConLabel = self._create_label("Fraction of Negative Connected Pixels, %", "small")
        self.weightNegConSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                             value=qf_defaults.DEFAULT_WEIGHT_NEG_CON, decimals=2)

        self.weightSmoothLabel = self._create_label("Smoothness Metric, intst. cnts.", "small")
        self.weightSmoothSpnBx = self._create_double_spinbox(min_val=0.0, max_val=1e6, 
                                                             value=qf_defaults.DEFAULT_WEIGHT_SMOOTH, decimals=2)

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
            "Normalized MSE of Synthetic Signal, intst. cnts.",
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
        self._connect_metric_persistence_signals()

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
        self.saveMetricsToCsvChkBx = QCheckBox("Save result metrics to csv")


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

        self.optimizeEachImageChkBx = QCheckBox("Optimize each image")
        self.optimizeEachImageChkBx.setToolTip(
            "Run full background optimization for every image in folder processing using current optimization settings"
        )

        self.createNewConfigurationsChkBx = QCheckBox("Automatically create new configurations for outlier images")
        self.createNewConfigurationsChkBx.setToolTip(
            "Automatically create new background configurations for images that are considered outliers "
            "(based on background metrics) and add them to the Background Configurations table"
        )

        self.assignConfgurationsManually = QPushButton("Manually assign configurations to images")

        self.chooseConfigurationsAutoChkBx.toggled.connect(self._update_folder_processing_controls)
        self.optimizeEachImageChkBx.toggled.connect(self._update_folder_processing_controls)
        self._update_folder_processing_controls()

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
        self.additionalSettingsGroup = CollapsibleGroupBox("Additional Settings", start_expanded=False)

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
        # NOTE: Avoid reusing the same (row, col) positions; doing so causes earlier
        # labels/widgets to be overwritten and "disappear" in the grid.
        row = 0

        layout.addWidget(QLabel("Subtraction Method:"), row, 0, 1, 1)
        layout.addWidget(self.bgChoiceIn, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.degreeLabel, row, 0, 1, 1)
        layout.addWidget(self.degreeCB, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.tophatLabel, row, 0, 1, 1)
        layout.addWidget(self.tophatSpnBx, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.cycleLabel, row, 0, 1, 1)
        layout.addWidget(self.cycle, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.gaussFWHMLabel, row, 0, 1, 1)
        layout.addWidget(self.gaussFWHM, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.boxcarLabel, row, 0, 1, 1)
        layout.addWidget(self.boxcarX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.boxcarY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.windowSizeLabel, row, 0, 1, 1)
        layout.addWidget(self.winSizeX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.winSizeY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.windowSepLabel, row, 0, 1, 1)
        layout.addWidget(self.winSepX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.winSepY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.thetaBinLabel, row, 0, 1, 1)
        layout.addWidget(self.thetabinCB, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.radialBinLabel, row, 0, 1, 1)
        layout.addWidget(self.radialBinSpnBx, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.pixRangeLabel, row, 0, 1, 1)
        layout.addWidget(self.minPixRange, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.maxPixRange, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.tensionLabel, row, 0, 1, 1)
        layout.addWidget(self.tensionSpnBx, row, 1, 1, 1)
        row += 1


        layout.addWidget(self.smoothLabel, row, 0, 1, 1)
        layout.addWidget(self.smoothSpnBx, row, 1, 1, 1)

    def _populate_manual_processing_layout_out(self, layout):
        """Populate manual processing controls for outside image."""
        row = 0

        layout.addWidget(QLabel("Subtraction Method:"), row, 0, 1, 1)
        layout.addWidget(self.bgChoiceOut, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.cycleOutLabel, row, 0, 1, 1)
        layout.addWidget(self.cycleOut, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.gaussFWHMOutLabel, row, 0, 1, 1)
        layout.addWidget(self.gaussFWHMOut, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.boxcarOutLabel, row, 0, 1, 1)
        layout.addWidget(self.boxcarOutX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.boxcarOutY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.windowSizeOutLabel, row, 0, 1, 1)
        layout.addWidget(self.winSizeOutX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.winSizeOutY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.windowSepOutLabel, row, 0, 1, 1)
        layout.addWidget(self.winSepOutX, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.winSepOutY, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.thetaBinOutLabel, row, 0, 1, 1)
        layout.addWidget(self.thetaBinOutCB, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.radialBinOutLabel, row, 0, 1, 1)
        layout.addWidget(self.radialBinOutSpnBx, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.pixRangeOutLabel, row, 0, 1, 1)
        layout.addWidget(self.minPixRangeOut, row, 1, 1, 1)
        row += 1
        layout.addWidget(QWidget(), row, 0, 1, 1)
        layout.addWidget(self.maxPixRangeOut, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.tensionOutLabel, row, 0, 1, 1)
        layout.addWidget(self.tensionOutSpnBx, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.tophatOutLabel, row, 0, 1, 1)
        layout.addWidget(self.tophatOutSpnBx, row, 1, 1, 1)
        row += 1

        layout.addWidget(self.smoothOutLabel, row, 0, 1, 1)
        layout.addWidget(self.smoothOutSpnBx, row, 1, 1, 1)


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
        metric_layout.addWidget(self.lossParamsTable, 0, 0, 1, 4)
        metric_layout.addWidget(self.metricWeightsHintLabel, 1, 0, 1, 4)

        additional_layout = QGridLayout()
        additional_layout.addWidget(self.evaluationBaselineLabel, 0, 0, 1, 1)
        additional_layout.addWidget(self.evaluationBaselineSpnBx, 0, 1, 1, 1)
        additional_layout.addWidget(self.persistEvaluationBaselineChkBx, 0, 2, 1, 2)
        additional_layout.addWidget(self.amplitudeLabel, 1, 0, 1, 1)
        additional_layout.addWidget(self.amplitudeSpnBx, 1, 1, 1, 1)
        additional_layout.addWidget(self.freqLabel, 1, 2, 1, 1)
        additional_layout.addWidget(self.freqCB, 1, 3, 1, 1)
        additional_layout.addWidget(self.sigmaXLabel, 2, 0, 1, 1)
        additional_layout.addWidget(self.sigmaXSpnBx, 2, 1, 1, 1)
        additional_layout.addWidget(self.sigmaYLabel, 2, 2, 1, 1)
        additional_layout.addWidget(self.sigmaYSpnBx, 2, 3, 1, 1)
        additional_layout.addWidget(self.persistSyntheticDataChkBx, 3, 0, 1, 4)

        self.additionalSettingsGroup.setLayout(additional_layout)
        metric_layout.addWidget(self.additionalSettingsGroup, 2, 0, 1, 4)
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
        eval_group_layout.addWidget(self.saveMetricsToCsvChkBx, 2, 0, Qt.AlignLeft)
        self.evalGroup.setLayout(eval_group_layout)

    def _setup_folder_layout(self):
        """Setup folder processing settings layout."""
        folder_layout = QGridLayout()
        folder_layout.addWidget(self.optimizeEachImageChkBx)
        folder_layout.addWidget(self.chooseConfigurationsAutoChkBx)
        # folder_layout.addWidget(self.createNewConfigurationsChkBx)
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

    def _update_folder_processing_controls(self, checked=None):
        if not hasattr(self, "chooseConfigurationsAutoChkBx") or not hasattr(self, "assignConfgurationsManually"):
            return

        optimize_each = bool(getattr(self, "optimizeEachImageChkBx", None) and self.optimizeEachImageChkBx.isChecked())

        self.chooseConfigurationsAutoChkBx.blockSignals(True)
        self.chooseConfigurationsAutoChkBx.setEnabled(not optimize_each)
        if optimize_each:
            self.chooseConfigurationsAutoChkBx.setChecked(False)
            self.assignConfgurationsManually.setEnabled(False)
        else:
            auto_checked = self.chooseConfigurationsAutoChkBx.isChecked() if checked is None else bool(checked)
            self.assignConfgurationsManually.setEnabled(not auto_checked)
        self.chooseConfigurationsAutoChkBx.blockSignals(False)

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

    def _connect_metric_persistence_signals(self):
        """Persist evaluation/metric settings whenever related values change."""
        metric_spinboxes = [
            self.meanMSESpnBx,
            self.meanNegSynSpnBx,
            self.meanNonBaselineSpnBx,
            self.meanNegConSpnBx,
            self.meanSmoothSpnBx,
            self.weightMSESpnBx,
            self.weightNegSynSpnBx,
            self.weightNonBaselineSpnBx,
            self.weightNegConSpnBx,
            self.weightSmoothSpnBx,
            self.evaluationBaselineSpnBx,
        ]
        for spinbox in metric_spinboxes:
            spinbox.valueChanged.connect(self._persist_metric_settings_to_image_cache)

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
            loss_text = _to_metric_text(loss, decimal_places=4) if loss is not None else "—"
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
            self._set_table_item(row, 2, _to_metric_text(norm_value, decimal_places=4))

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

        if parent is not None and parent.quadFold is not None:
            parent.quadFold.info.setdefault('result_bg', {})
            parent.quadFold.info['result_bg']['selected_configuration_name'] = name

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
            'early_stop': flags.get('early_stop', 0.005),
            'max_iterations': flags.get('max_iterations', 20),
            'mean_metric_values': flags.get('mean_metric_values', None),
            'evaluation_baseline': flags.get('evaluation_baseline', None),
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

    def _persist_metric_settings_to_image_cache(self):
        """
        Persist edited metric means/weights/baseline into current image info + cache.
        """
        parent = self._get_parent_gui()
        if parent is None or not hasattr(parent, "getFlags"):
            return
        if not hasattr(parent, "quadFold") or parent.quadFold is None:
            return

        try:
            if parent.uiUpdating:
                return

            metric_weights = {
                'MSE': float(self.weightMSESpnBx.value()),
                'Share_Neg_Synthetic': float(self.weightNegSynSpnBx.value()),
                'Share_Non_Baseline': float(self.weightNonBaselineSpnBx.value()),
                'Share_Neg_Connected': float(self.weightNegConSpnBx.value()),
                'Smoothness': float(self.weightSmoothSpnBx.value()),
            }
            mean_metric_values = {
                'MSE_SYN_MEAN': float(self.meanMSESpnBx.value()),
                'SHARE_NEG_SYN_MEAN': float(self.meanNegSynSpnBx.value()),
                'SHARE_NON_BASELINE_MEAN': float(self.meanNonBaselineSpnBx.value()),
                'SHARE_NEG_CON_MEAN': float(self.meanNegConSpnBx.value()),
                'SMOOTH_MEAN': float(self.meanSmoothSpnBx.value()),
            }
            evaluation_baseline = max(
                qf_defaults.MIN_EVAL_BASELINE,
                float(self.evaluationBaselineSpnBx.value()),
            )
            synthetic_amplitude = float(self.amplitudeSpnBx.value())
            synthetic_sigma_x = float(self.sigmaXSpnBx.value())
            synthetic_sigma_y = float(self.sigmaYSpnBx.value())
            freq = str(self.freqCB.currentText())

            info = parent.quadFold.info
            info['metric_weights'] = metric_weights
            info['mean_metric_values'] = mean_metric_values
            info['evaluation_baseline'] = evaluation_baseline
            info['synthetic_amplitude'] = synthetic_amplitude
            info['synthetic_sigma_x'] = synthetic_sigma_x
            info['synthetic_sigma_y'] = synthetic_sigma_y
            info['freq'] = freq

            # TODO: decide when to save to cache
            # if hasattr(parent.quadFold, "cacheInfo"):
            #     parent.quadFold.cacheInfo()
        except Exception as e:
            print(f"Failed to persist metric settings to image cache: {e}")

    def _persist_image_processing_settings_to_image_cache(self):
        """
        Persist downsample/smooth-image settings into current image info + cache.
        """
        parent = self._get_parent_gui()
        if parent is None or not hasattr(parent, "quadFold") or parent.quadFold is None:
            return
        try:
            if parent.uiUpdating:
                return
            info = parent.quadFold.info
            info['downsample'] = int(self.downsampleCB.currentText())
            info['smooth_image'] = bool(self.smoothImageChkbx.isChecked())

            # TODO: decide when to save to cache
            # if hasattr(parent.quadFold, "cacheInfo"):
            #     parent.quadFold.cacheInfo()
        except Exception as e:
            print(f"Failed to persist image processing settings to image cache: {e}")
    
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
        if not cache_file or cache_key is None:
            return

        try:
            configs = get_user_background_configurations(cache_file, cache_key)
        except Exception as e:
            print(f"Failed to load background configurations from cache: {e}")
            return
        
        # Only clear the table if we successfully retrieved configurations
        if not configs:
            print(f"No background configurations found in cache for key: {cache_key}")
            return
        
        self._clear_background_configurations_table()

        for config in configs:
            name = str(config.get('name', '') or '').strip()
            method = str(config.get('method', 'None') or 'None')
            params = config.get('params', {}) if isinstance(config.get('params', {}), dict) else {}
            loss_value = config.get('loss', None)
            downsample = int(config.get('downsample', 1) or 1)
            smooth_image = bool(config.get('smooth_image', False))
            row_additional_info = config.get('additional_info', additional_info)
            params_text = self._format_bg_params_text(params)
            loss_text = "—" if loss_value is None else _to_metric_text(loss_value, decimal_places=4)

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
def _to_metric_text(value, as_percent=False, as_contribution=False, decimal_places=2):
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
        return f"{numeric:.{decimal_places}f}"
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
