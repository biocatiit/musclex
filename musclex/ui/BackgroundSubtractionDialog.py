"""
Background subtraction settings popup for Quadrant Folding.
"""

from .pyqt_utils import *
from .widgets.collapsible_groupbox import CollapsibleGroupBox


class BackgroundSubtractionDialog(QDialog):
    """Popup window that contains all background subtraction controls."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Background Subtraction Settings")
        self.resize(620, 760)

        self._create_widgets()
        self._create_layout()

    def _create_widgets(self):
        # ===== Section Groups =====
        self.currentGroup = QGroupBox("Current Background Subtraction")
        self.commonGroup = QGroupBox("Common Settings")
        self.manualGroup = QGroupBox("Manual Processing")
        self.autoGroup = QGroupBox("Automated Processing")

        self.currentMethodLabel = QLabel("Method: None")
        self.currentSourceLabel = QLabel("Source: None")
        self.currentParamsLabel = QLabel("Parameters: None")
        self.currentParamsLabel.setWordWrap(True)

        self.processingModeLabel = QLabel("Processing Mode:")
        self.processingModeCB = QComboBox()
        self.processingModeCB.addItems(["Manual Processing", "Automated Processing"])
        self.processingModeCB.setCurrentIndex(0)

        # ===== ROI Settings =====
        self.setFitRoi = QPushButton("Set Crop ROI")
        self.setFitRoi.setCheckable(True)
        self.unsetRoi = QPushButton("Unset ROI")
        self.fixedRoiChkBx = QCheckBox("Fixed ROI Radius:")
        self.fixedRoiChkBx.setChecked(False)
        self.fixedRoi = QSpinBox()
        self.fixedRoi.setObjectName('fixedRoi')
        self.fixedRoi.setKeyboardTracking(False)
        self.fixedRoi.setRange(1, 10000)
        self.fixedRoi.setEnabled(False)

        # ===== Background Choice Dropdown =====
        self.allBGChoices = [
            'None',
            '2D Convexhull',
            'Circularly-symmetric',
            'White-top-hats',
            'Smoothed-Gaussian',
            'Smoothed-BoxCar',
            'Roving Window'
        ]
        self.bgChoiceIn = QComboBox()
        self.bgChoiceIn.setCurrentIndex(0)
        for c in self.allBGChoices:
            self.bgChoiceIn.addItem(c)

        # ===== R-min / R-max Settings =====
        self.setRminRmaxButton = QPushButton("Manual R-min/max")
        self.setRminRmaxButton.setCheckable(True)

        self.rminSpnBx = QSpinBox()
        self.rminSpnBx.setSingleStep(2)
        self.rminSpnBx.setValue(-1)
        self.rminSpnBx.setRange(-1, 3000)
        self.rminSpnBx.setKeyboardTracking(False)
        self.rminLabel = QLabel("R-min")

        self.rmaxSpnBx = QSpinBox()
        self.rmaxSpnBx.setSingleStep(2)
        self.rmaxSpnBx.setValue(-1)
        self.rmaxSpnBx.setRange(-1, 3000)
        self.rmaxSpnBx.setKeyboardTracking(False)
        self.rmaxLabel = QLabel("R-max")

        self.downsampleLabel = QLabel("Downsample")
        self.downsampleCB = QComboBox()
        self.downsampleCB.addItems(["1", "2", "4"])
        self.downsampleCB.setCurrentIndex(1)

        self.smoothImageChkbx = QCheckBox("Smooth Image")
        self.smoothImageChkbx.setChecked(False)

        self.showResultMaskChkBx = QCheckBox("Show Eval Mask")
        self.showRminRmaxChkBx = QCheckBox("Show R-min/max")
        self.fixedRadiusRangeChkBx = QCheckBox("Persist R-min/max")
        self.equatorYLengthLabel = QLabel("Equator Y Length : ")
        self.equatorYLengthSpnBx = QSpinBox()
        self.equatorYLengthSpnBx.setRange(1, 10000)
        self.equatorYLengthSpnBx.setValue(30)
        self.equatorYLengthSpnBx.setKeyboardTracking(False)

        self.equatorCenterBeamLabel = QLabel("Equator Center Beam : ")
        self.equatorCenterBeamSpnBx = QSpinBox()
        self.equatorCenterBeamSpnBx.setRange(1, 10000)
        self.equatorCenterBeamSpnBx.setValue(20)
        self.equatorCenterBeamSpnBx.setKeyboardTracking(False)

        self.m1Label = QLabel("M1 (Layer line spacing) : ")
        self.m1SpnBx = QSpinBox()
        self.m1SpnBx.setRange(1, 10000)
        self.m1SpnBx.setValue(50)
        self.m1SpnBx.setKeyboardTracking(False)

        self.layerLineWidthLabel = QLabel("Layer line width : ")
        self.layerLineWidthSpnBx = QSpinBox()
        self.layerLineWidthSpnBx.setRange(1, 10000)
        self.layerLineWidthSpnBx.setValue(5)
        self.layerLineWidthSpnBx.setKeyboardTracking(False)

        # ===== Background Parameters =====
        self.gaussFWHMLabel = QLabel("Gaussian FWHM : ")
        self.gaussFWHM = QSpinBox()
        self.gaussFWHM.setRange(1, 3000)
        self.gaussFWHM.setValue(15)
        self.gaussFWHM.setKeyboardTracking(False)

        self.boxcarLabel = QLabel("Box Car Size : ")
        self.boxcarX = QSpinBox()
        self.boxcarX.setRange(1, 3000)
        self.boxcarX.setValue(15)
        self.boxcarX.setPrefix('X:')
        self.boxcarX.setKeyboardTracking(False)
        self.boxcarY = QSpinBox()
        self.boxcarY.setRange(1, 3000)
        self.boxcarY.setValue(15)
        self.boxcarY.setPrefix('Y:')
        self.boxcarY.setKeyboardTracking(False)

        self.cycleLabel = QLabel("Number of Cycles : ")
        self.cycle = QSpinBox()
        self.cycle.setValue(250)
        self.cycle.setKeyboardTracking(False)
        self.cycle.setRange(1, 3000)

        self.windowSizeLabel = QLabel("Window Size : ")
        self.winSizeX = QSpinBox()
        self.winSizeX.setPrefix('X:')
        self.winSizeX.setKeyboardTracking(False)
        self.winSizeX.setRange(1, 3000)
        self.winSizeX.setValue(15)
        self.winSizeY = QSpinBox()
        self.winSizeY.setPrefix('Y:')
        self.winSizeY.setKeyboardTracking(False)
        self.winSizeY.setRange(1, 3000)
        self.winSizeY.setValue(15)

        self.windowSepLabel = QLabel("Window Separation : ")
        self.winSepX = QSpinBox()
        self.winSepX.setPrefix('X:')
        self.winSepX.setKeyboardTracking(False)
        self.winSepX.setRange(1, 3000)
        self.winSepX.setValue(10)
        self.winSepY = QSpinBox()
        self.winSepY.setPrefix('Y:')
        self.winSepY.setKeyboardTracking(False)
        self.winSepY.setRange(1, 3000)
        self.winSepY.setValue(10)

        self.minPixRange = QDoubleSpinBox()
        self.minPixRange.setSuffix("%")
        self.minPixRange.setDecimals(2)
        self.minPixRange.setSingleStep(2)
        self.minPixRange.setValue(0)
        self.minPixRange.setRange(0, 100)
        self.minPixRange.setKeyboardTracking(False)

        self.maxPixRange = QDoubleSpinBox()
        self.maxPixRange.setSuffix("%")
        self.maxPixRange.setDecimals(2)
        self.maxPixRange.setSingleStep(2)
        self.maxPixRange.setValue(25)
        self.maxPixRange.setRange(0, 100)
        self.maxPixRange.setKeyboardTracking(False)
        self.pixRangeLabel = QLabel("Pixel Range : ")

        self.thetaBinLabel = QLabel("Bin Theta (deg) : ")
        self.thetabinCB = QComboBox()
        self.thetabinCB.addItems(["3", "5", "10", "15", "30", "45", "90"])
        self.thetabinCB.setCurrentIndex(4)

        self.radialBinSpnBx = QSpinBox()
        self.radialBinSpnBx.setRange(1, 100)
        self.radialBinSpnBx.setValue(10)
        self.radialBinSpnBx.setKeyboardTracking(False)
        self.radialBinSpnBx.setSuffix(" Pixel(s)")
        self.radialBinLabel = QLabel("Radial Bin : ")

        self.smoothSpnBx = QDoubleSpinBox()
        self.smoothSpnBx.setRange(0, 10000)
        self.smoothSpnBx.setValue(0.1)
        self.smoothSpnBx.setKeyboardTracking(False)
        self.smoothLabel = QLabel("Smoothing factor : ")

        self.tensionSpnBx = QDoubleSpinBox()
        self.tensionSpnBx.setRange(0, 100)
        self.tensionSpnBx.setValue(1)
        self.tensionSpnBx.setKeyboardTracking(False)
        self.tensionLabel = QLabel("Tension factor : ")

        self.tophat1SpnBx = QSpinBox()
        self.tophat1SpnBx.setRange(1, 100)
        self.tophat1SpnBx.setValue(50)
        self.tophat1SpnBx.setKeyboardTracking(False)
        self.tophat1Label = QLabel("Top-hat Disk Size: ")

        self.deg1Label = QLabel("Step Degree : ")
        self.deg1CB = QComboBox()
        self.deg1CB.addItems(["0.5", "1", "2", "3", "5", "9", "10", "15"])
        self.deg1CB.setCurrentIndex(1)

        self.applyBGButton = QPushButton("Apply")

        # ===== Automated Processing / Optimization =====
        self.optimizeChkBx = QCheckBox("Enable optimization")
        self.optimizeChkBx.setChecked(False)
        self.optimizeChkBx.setVisible(False)

        self.optimizationMethodsLabel = QLabel("BG Subtraction Methods:")
        self.optimizationMethodsList = QListWidget()
        self.optimizationMethodsList.setSelectionMode(QAbstractItemView.MultiSelection)
        self.optimizationMethods = [
            '2D Convexhull',
            'Circularly-symmetric',
            'White-top-hats',
            'Smoothed-Gaussian',
            'Smoothed-BoxCar',
            'Roving Window'
        ]
        for method in self.optimizationMethods:
            self.optimizationMethodsList.addItem(method)

        # Default methods aligned with current optimization workflow
        self._set_selected_methods(['Circularly-symmetric', 'White-top-hats', 'Smoothed-Gaussian'])

        self.stepsLabel = QLabel("Step Sizes:")
        self.stepsLineEdit = QLineEdit("50, 30, 10, 7, 5, 3, 1")
        self.stepsLineEdit.setToolTip("Comma-separated values used for optimization step schedule.")

        self.maxIterationsLabel = QLabel("Max Iterations:")
        self.maxIterationsSpnBx = QSpinBox()
        self.maxIterationsSpnBx.setRange(1, 1000)
        self.maxIterationsSpnBx.setValue(10)
        self.maxIterationsSpnBx.setKeyboardTracking(False)

        self.earlyStopLabel = QLabel("Early Stop Threshold:")
        self.earlyStopSpnBx = QDoubleSpinBox()
        self.earlyStopSpnBx.setDecimals(5)
        self.earlyStopSpnBx.setRange(0.0, 1.0)
        self.earlyStopSpnBx.setSingleStep(0.0001)
        self.earlyStopSpnBx.setValue(0.001)
        self.earlyStopSpnBx.setKeyboardTracking(False)

        self.normalizationMeansLabel = QLabel("Normalization Means:")
        self.meanMSELabel = QLabel("mean mseSyn")
        self.meanMSESpnBx = QDoubleSpinBox()
        self.meanMSESpnBx.setDecimals(5)
        self.meanMSESpnBx.setRange(1e-6, 1e9)
        self.meanMSESpnBx.setValue(100.0)
        self.meanMSESpnBx.setKeyboardTracking(False)

        self.meanNegSynLabel = QLabel("mean fNegSyn")
        self.meanNegSynSpnBx = QDoubleSpinBox()
        self.meanNegSynSpnBx.setDecimals(5)
        self.meanNegSynSpnBx.setRange(1e-6, 1e6)
        self.meanNegSynSpnBx.setValue(0.20)
        self.meanNegSynSpnBx.setKeyboardTracking(False)

        self.meanNegGenLabel = QLabel("mean fNegGen")
        self.meanNegGenSpnBx = QDoubleSpinBox()
        self.meanNegGenSpnBx.setDecimals(5)
        self.meanNegGenSpnBx.setRange(1e-6, 1e6)
        self.meanNegGenSpnBx.setValue(0.08)
        self.meanNegGenSpnBx.setKeyboardTracking(False)

        self.meanNonBaselineLabel = QLabel("mean fNonBaseline")
        self.meanNonBaselineSpnBx = QDoubleSpinBox()
        self.meanNonBaselineSpnBx.setDecimals(5)
        self.meanNonBaselineSpnBx.setRange(1e-6, 1e6)
        self.meanNonBaselineSpnBx.setValue(0.35)
        self.meanNonBaselineSpnBx.setKeyboardTracking(False)

        self.meanNegConLabel = QLabel("mean fNegCon")
        self.meanNegConSpnBx = QDoubleSpinBox()
        self.meanNegConSpnBx.setDecimals(5)
        self.meanNegConSpnBx.setRange(1e-6, 1e6)
        self.meanNegConSpnBx.setValue(0.07)
        self.meanNegConSpnBx.setKeyboardTracking(False)

        self.meanSmoothLabel = QLabel("mean Smoothness")
        self.meanSmoothSpnBx = QDoubleSpinBox()
        self.meanSmoothSpnBx.setDecimals(5)
        self.meanSmoothSpnBx.setRange(1e-6, 1e6)
        self.meanSmoothSpnBx.setValue(0.03)
        self.meanSmoothSpnBx.setKeyboardTracking(False)

        self.metricWeightsLabel = QLabel("Metric Weights:")
        self.weightMSELabel = QLabel("mseSyn")
        self.weightMSESpnBx = QDoubleSpinBox()
        self.weightMSESpnBx.setDecimals(5)
        self.weightMSESpnBx.setRange(0.0, 1e6)
        self.weightMSESpnBx.setValue(0.1)
        self.weightMSESpnBx.setKeyboardTracking(False)

        self.weightNegSynLabel = QLabel("fNegSyn")
        self.weightNegSynSpnBx = QDoubleSpinBox()
        self.weightNegSynSpnBx.setDecimals(5)
        self.weightNegSynSpnBx.setRange(0.0, 1e6)
        self.weightNegSynSpnBx.setValue(0.1)
        self.weightNegSynSpnBx.setKeyboardTracking(False)

        self.weightNegGenLabel = QLabel("fNegGen")
        self.weightNegGenSpnBx = QDoubleSpinBox()
        self.weightNegGenSpnBx.setDecimals(5)
        self.weightNegGenSpnBx.setRange(0.0, 1e6)
        self.weightNegGenSpnBx.setValue(0.0)
        self.weightNegGenSpnBx.setKeyboardTracking(False)

        self.weightNonBaselineLabel = QLabel("fNonBaseline")
        self.weightNonBaselineSpnBx = QDoubleSpinBox()
        self.weightNonBaselineSpnBx.setDecimals(5)
        self.weightNonBaselineSpnBx.setRange(0.0, 1e6)
        self.weightNonBaselineSpnBx.setValue(0.1)
        self.weightNonBaselineSpnBx.setKeyboardTracking(False)

        self.weightNegConLabel = QLabel("fNegCon")
        self.weightNegConSpnBx = QDoubleSpinBox()
        self.weightNegConSpnBx.setDecimals(5)
        self.weightNegConSpnBx.setRange(0.0, 1e6)
        self.weightNegConSpnBx.setValue(0.3)
        self.weightNegConSpnBx.setKeyboardTracking(False)

        self.weightSmoothLabel = QLabel("Smoothness")
        self.weightSmoothSpnBx = QDoubleSpinBox()
        self.weightSmoothSpnBx.setDecimals(5)
        self.weightSmoothSpnBx.setRange(0.0, 1e6)
        self.weightSmoothSpnBx.setValue(0.4)
        self.weightSmoothSpnBx.setKeyboardTracking(False)

        self.metricWeightsHintLabel = QLabel(
            "<i><span style='color:#2e7d32;'>"
            "Weights should roughly add up to 1.0 and may be adjusted as needed."
            "</span></i>"
        )
        self.metricWeightsHintLabel.setTextFormat(Qt.RichText)
        self.metricWeightsHintLabel.setWordWrap(True)

        self.normalizationMeansHintLabel = QLabel(
            "<i><span style='color:#2e7d32;'>"
            "Normalization means are tunable reference values and may be adjusted for your data."
            "</span></i>"
        )
        self.normalizationMeansHintLabel.setTextFormat(Qt.RichText)
        self.normalizationMeansHintLabel.setWordWrap(True)

        self.rrangeSettingFrame = QFrame()
        self.rrangeSettingLayout = QGridLayout(self.rrangeSettingFrame)
        self.rrangeSettingLayout.setContentsMargins(0, 0, 0, 0)
        self.rrangeSettingLayout.addWidget(self.rminLabel, 2, 0, 1, 1)
        self.rrangeSettingLayout.addWidget(self.rminSpnBx, 2, 1, 1, 1)
        self.rrangeSettingLayout.addWidget(self.rmaxLabel, 2, 2, 1, 1)
        self.rrangeSettingLayout.addWidget(self.rmaxSpnBx, 2, 3, 1, 1)
        self.rrangeSettingLayout.addWidget(self.setRminRmaxButton, 3, 0, 1, 1)
        self.rrangeSettingLayout.addWidget(self.showRminRmaxChkBx, 3, 1, 1, 1)
        self.rrangeSettingLayout.addWidget(self.fixedRadiusRangeChkBx, 3, 2, 1, 2)
        self.rrangeSettingLayout.addWidget(self.smoothImageChkbx, 4, 0, 1, 2)
        self.rrangeSettingLayout.addWidget(self.downsampleLabel, 4, 2, 1, 1)
        self.rrangeSettingLayout.addWidget(self.downsampleCB, 4, 3, 1, 1)

        self.maskSeparator = QFrame()
        self.maskSeparator.setFrameShape(QFrame.HLine)
        self.maskSeparator.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        self.maskSeparator.setLineWidth(1)

        self.maskSettingsFrame = QFrame()
        self.maskSettingsLayout = QGridLayout(self.maskSettingsFrame)
        self.maskSettingsLayout.setContentsMargins(0, 0, 0, 0)
        self.maskSettingsLayout.addWidget(self.showResultMaskChkBx, 0, 0, 1, 2)
        self.maskSettingsLayout.addWidget(self.equatorYLengthLabel, 1, 0, 1, 1)
        self.maskSettingsLayout.addWidget(self.equatorYLengthSpnBx, 1, 1, 1, 1)
        self.maskSettingsLayout.addWidget(self.equatorCenterBeamLabel, 1, 2, 1, 1)
        self.maskSettingsLayout.addWidget(self.equatorCenterBeamSpnBx, 1, 3, 1, 1)
        self.maskSettingsLayout.addWidget(self.m1Label, 2, 0, 1, 1)
        self.maskSettingsLayout.addWidget(self.m1SpnBx, 2, 1, 1, 1)
        self.maskSettingsLayout.addWidget(self.layerLineWidthLabel, 2, 2, 1, 1)
        self.maskSettingsLayout.addWidget(self.layerLineWidthSpnBx, 2, 3, 1, 1)

        self.optimizeChkBx.stateChanged.connect(self._update_optimization_widgets_state)
        self.processingModeCB.currentIndexChanged.connect(self._update_processing_mode_visibility)
        self.processingModeCB.currentIndexChanged.connect(self._sync_mode_to_optimization_flag)
        self._update_optimization_widgets_state()
        self._sync_mode_to_optimization_flag()
        self._update_processing_mode_visibility()

    def _set_selected_methods(self, methods):
        method_set = set(methods)
        for i in range(self.optimizationMethodsList.count()):
            item = self.optimizationMethodsList.item(i)
            item.setSelected(item.text() in method_set)

    def _update_optimization_widgets_state(self):
        enabled = self.optimizeChkBx.isChecked()
        self.optimizationMethodsList.setEnabled(enabled)
        self.stepsLineEdit.setEnabled(enabled)
        self.maxIterationsSpnBx.setEnabled(enabled)
        self.earlyStopSpnBx.setEnabled(enabled)

    def _sync_mode_to_optimization_flag(self):
        automated = self.processingModeCB.currentText() == "Automated Processing"
        self.optimizeChkBx.setChecked(automated)

    def _update_processing_mode_visibility(self):
        automated = self.processingModeCB.currentText() == "Automated Processing"
        self.manualGroup.setVisible(not automated)
        self.autoGroup.setVisible(automated)

    def _create_layout(self):
        main_layout = QVBoxLayout(self)

        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        container = QWidget()
        container_layout = QVBoxLayout(container)

        modeWidget = QWidget()
        modeLayout = QGridLayout(modeWidget)
        modeLayout.setContentsMargins(0, 0, 0, 0)
        modeLayout.addWidget(self.processingModeLabel, 0, 0, 1, 1)
        modeLayout.addWidget(self.processingModeCB, 0, 1, 1, 1)

        currentLayout = QVBoxLayout(self.currentGroup)
        currentLayout.setContentsMargins(8, 8, 8, 8)
        currentLayout.addWidget(self.currentMethodLabel)
        currentLayout.addWidget(self.currentSourceLabel)
        currentLayout.addWidget(self.currentParamsLabel)

        commonLayout = QGridLayout(self.commonGroup)
        commonLayout.setContentsMargins(8, 8, 8, 8)

        manualLayout = QGridLayout(self.manualGroup)

        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        separator.setLineWidth(1)

        separator_2 = QFrame()
        separator_2.setFrameShape(QFrame.HLine)
        separator_2.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Minimum)
        separator_2.setLineWidth(1)

        manualLayout.addWidget(self.setFitRoi, 0, 0, 1, 1)
        manualLayout.addWidget(self.unsetRoi, 0, 1, 1, 1)
        manualLayout.addWidget(self.fixedRoiChkBx, 0, 2, 1, 1)
        manualLayout.addWidget(self.fixedRoi, 0, 3, 1, 1)
        manualLayout.addWidget(separator_2, 1, 0, 1, 4)
        manualLayout.addWidget(QLabel("Background Subtraction Method:"), 2, 0, 1, 2)
        manualLayout.addWidget(self.bgChoiceIn, 2, 2, 1, 2)

        manualLayout.addWidget(self.gaussFWHMLabel, 3, 0, 1, 1)
        manualLayout.addWidget(self.gaussFWHM, 3, 1, 1, 1)

        manualLayout.addWidget(self.boxcarLabel, 4, 0, 1, 1)
        manualLayout.addWidget(self.boxcarX, 4, 1, 1, 1)
        manualLayout.addWidget(self.boxcarY, 5, 1, 1, 1)

        manualLayout.addWidget(self.cycleLabel, 3, 2, 1, 1)
        manualLayout.addWidget(self.cycle, 3, 3, 1, 1)

        manualLayout.addWidget(self.thetaBinLabel, 6, 0, 1, 1)
        manualLayout.addWidget(self.thetabinCB, 6, 1, 1, 1)

        manualLayout.addWidget(self.radialBinLabel, 7, 2, 1, 1)
        manualLayout.addWidget(self.radialBinSpnBx, 7, 3, 1, 1)

        manualLayout.addWidget(self.windowSizeLabel, 8, 0, 1, 1)
        manualLayout.addWidget(self.winSizeX, 8, 1, 1, 1)
        manualLayout.addWidget(self.winSizeY, 9, 1, 1, 1)

        manualLayout.addWidget(self.windowSepLabel, 8, 2, 1, 1)
        manualLayout.addWidget(self.winSepX, 8, 3, 1, 1)
        manualLayout.addWidget(self.winSepY, 9, 3, 1, 1)

        manualLayout.addWidget(self.pixRangeLabel, 10, 0, 1, 1)
        manualLayout.addWidget(self.minPixRange, 10, 1, 1, 1)
        manualLayout.addWidget(self.maxPixRange, 11, 1, 1, 1)

        manualLayout.addWidget(self.smoothLabel, 10, 2, 1, 1)
        manualLayout.addWidget(self.smoothSpnBx, 10, 3, 1, 1)

        manualLayout.addWidget(self.tensionLabel, 11, 2, 1, 1)
        manualLayout.addWidget(self.tensionSpnBx, 11, 3, 1, 1)

        manualLayout.addWidget(self.deg1Label, 12, 2, 1, 1)
        manualLayout.addWidget(self.deg1CB, 12, 3, 1, 1)

        manualLayout.addWidget(self.tophat1Label, 13, 2, 1, 1)
        manualLayout.addWidget(self.tophat1SpnBx, 13, 3, 1, 1)

        manualLayout.addWidget(separator, 14, 0, 1, 4)

        autoLayout = QGridLayout(self.autoGroup)
        autoLayout.addWidget(self.optimizeChkBx, 0, 0, 1, 2)
        autoLayout.addWidget(self.optimizationMethodsLabel, 1, 0, 1, 2)
        autoLayout.addWidget(self.optimizationMethodsList, 2, 0, 1, 2)
        autoLayout.addWidget(self.stepsLabel, 3, 0, 1, 1)
        autoLayout.addWidget(self.stepsLineEdit, 3, 1, 1, 1)
        autoLayout.addWidget(self.maxIterationsLabel, 4, 0, 1, 1)
        autoLayout.addWidget(self.maxIterationsSpnBx, 4, 1, 1, 1)
        autoLayout.addWidget(self.earlyStopLabel, 4, 2, 1, 1)
        autoLayout.addWidget(self.earlyStopSpnBx, 4, 3, 1, 1)
        self.metricWeightsGroup = CollapsibleGroupBox("Metric Weights", start_expanded=False)
        metricWeightsLayout = QGridLayout()
        metricWeightsLayout.addWidget(self.weightMSELabel, 0, 0, 1, 1)
        metricWeightsLayout.addWidget(self.weightMSESpnBx, 0, 1, 1, 1)
        metricWeightsLayout.addWidget(self.weightNegSynLabel, 0, 2, 1, 1)
        metricWeightsLayout.addWidget(self.weightNegSynSpnBx, 0, 3, 1, 1)
        metricWeightsLayout.addWidget(self.weightNegGenLabel, 1, 0, 1, 1)
        metricWeightsLayout.addWidget(self.weightNegGenSpnBx, 1, 1, 1, 1)
        metricWeightsLayout.addWidget(self.weightNonBaselineLabel, 1, 2, 1, 1)
        metricWeightsLayout.addWidget(self.weightNonBaselineSpnBx, 1, 3, 1, 1)
        metricWeightsLayout.addWidget(self.weightNegConLabel, 2, 0, 1, 1)
        metricWeightsLayout.addWidget(self.weightNegConSpnBx, 2, 1, 1, 1)
        metricWeightsLayout.addWidget(self.weightSmoothLabel, 2, 2, 1, 1)
        metricWeightsLayout.addWidget(self.weightSmoothSpnBx, 2, 3, 1, 1)
        metricWeightsLayout.addWidget(self.metricWeightsHintLabel, 3, 0, 1, 4)
        self.metricWeightsGroup.setLayout(metricWeightsLayout)

        self.normalizationMeansGroup = CollapsibleGroupBox("Normalization Means", start_expanded=False)
        normalizationMeansLayout = QGridLayout()
        normalizationMeansLayout.addWidget(self.meanMSELabel, 0, 0, 1, 1)
        normalizationMeansLayout.addWidget(self.meanMSESpnBx, 0, 1, 1, 1)
        normalizationMeansLayout.addWidget(self.meanNegSynLabel, 0, 2, 1, 1)
        normalizationMeansLayout.addWidget(self.meanNegSynSpnBx, 0, 3, 1, 1)
        normalizationMeansLayout.addWidget(self.meanNegGenLabel, 1, 0, 1, 1)
        normalizationMeansLayout.addWidget(self.meanNegGenSpnBx, 1, 1, 1, 1)
        normalizationMeansLayout.addWidget(self.meanNonBaselineLabel, 1, 2, 1, 1)
        normalizationMeansLayout.addWidget(self.meanNonBaselineSpnBx, 1, 3, 1, 1)
        normalizationMeansLayout.addWidget(self.meanNegConLabel, 2, 0, 1, 1)
        normalizationMeansLayout.addWidget(self.meanNegConSpnBx, 2, 1, 1, 1)
        normalizationMeansLayout.addWidget(self.meanSmoothLabel, 2, 2, 1, 1)
        normalizationMeansLayout.addWidget(self.meanSmoothSpnBx, 2, 3, 1, 1)
        normalizationMeansLayout.addWidget(self.normalizationMeansHintLabel, 3, 0, 1, 4)
        self.normalizationMeansGroup.setLayout(normalizationMeansLayout)

        autoLayout.addWidget(self.metricWeightsGroup, 6, 0, 1, 4)
        autoLayout.addWidget(self.normalizationMeansGroup, 7, 0, 1, 4)

        commonLayout.addWidget(self.rrangeSettingFrame, 0, 0, 1, 4)
        commonLayout.addWidget(self.maskSeparator, 1, 0, 1, 4)
        commonLayout.addWidget(self.maskSettingsFrame, 2, 0, 1, 4)

        container_layout.addWidget(modeWidget)
        container_layout.addWidget(self.currentGroup)
        container_layout.addWidget(self.commonGroup)
        container_layout.addWidget(self.manualGroup)
        container_layout.addWidget(self.autoGroup)
        container_layout.addStretch(1)

        scroll_area.setWidget(container)
        main_layout.addWidget(scroll_area)
        main_layout.addWidget(self.applyBGButton)

    def update_current_bg_summary(self, method=None, params=None, source=None):
        method_text = "None" if method in (None, "") else str(method)
        source_text = "None" if source in (None, "") else str(source)

        if isinstance(params, dict) and len(params) > 0:
            parts = []
            for key in sorted(params.keys()):
                value = params[key]
                try:
                    value_text = f"{float(value):.6g}"
                except Exception:
                    value_text = str(value)
                parts.append(f"{key}={value_text}")
            params_text = ", ".join(parts)
        else:
            params_text = "None"

        self.currentMethodLabel.setText(f"Method: {method_text}")
        self.currentSourceLabel.setText(f"Source: {source_text}")
        self.currentParamsLabel.setText(f"Parameters: {params_text}")
