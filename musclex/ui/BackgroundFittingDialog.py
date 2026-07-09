"""
Iterative 2D background fitting window for Quadrant Folding.

A standalone dialog (launched from the Background Subtraction settings) that
runs the two-stage iterative background fit
(:mod:`musclex.utils.bg_fitting.background_fitting`) on the current folded image and lets
the user inspect the fitted background, its equator / general components, or the
residual. On exit the residual (background-subtracted image) replaces the
current result.
"""

import os
import traceback

import numpy as np

from .pyqt_utils import *
from matplotlib.figure import Figure

from ..utils.bg_fitting import background_fitting as bf
from ..utils.bg_search.background_search import get_projection, makeFullImage
from ..utils import qf_defaults
from .widgets.collapsible_groupbox import CollapsibleGroupBox


# View modes for the visualization dropdown.
VIEW_MODES = [
    "Original",
    "Fitted background (equator+general)",
    "Equator component",
    "General component",
    "Residual (background removed)",
    "Equator / Meridian profiles",
    "General mask",
    "Equator mask",
]

# View modes available before a fit: only the mask overlays, so the user can
# inspect and adjust the masking before running the fit.
MASK_VIEW_MODES = ["General mask", "Equator mask"]

# Colormap options for the image views: display name -> matplotlib cmap.
COLORMAPS = [
    ("Viridis", "viridis"),
    ("Grey", "gray"),
    ("Red-Blue", "RdBu_r"),
]


class FitWorker(QObject):
    """Runs the (slow) fit off the UI thread."""

    progress = Signal(str, float)  # (stage, frac)
    finished = Signal(object)
    failed = Signal(str)

    def __init__(self, img, general_mask, equator_mask, rmin, rmax, cfg,
                 rminrmax_mask=None):
        super().__init__()
        self._args = (img, general_mask, equator_mask, rmin, rmax, cfg,
                      rminrmax_mask)

    def run(self):
        img, gmask, emask, rmin, rmax, cfg, rrmask = self._args
        try:
            result = bf.two_stage_iterative_fit(
                img, gmask, equator_mask=emask, rmin=rmin, rmax=rmax, cfg=cfg,
                rminrmax_mask=rrmask,
                progress_cb=lambda stage, frac: self.progress.emit(stage, frac))
            self.finished.emit(result)
        except Exception:  # noqa: BLE001
            self.failed.emit(traceback.format_exc())


class BackgroundFittingDialog(QDialog):
    """Popup that fits and visualizes the iterative 2D background."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Iterative 2D Background Fitting")
        self.resize(1000, 720)

        self._parent_gui = parent
        self.result = None            # last fit result dict
        self._inputs = None           # (img, gmask, emask, rmin, rmax, rrmask)
        self._thread = None
        self._worker = None

        self._create_widgets()
        self._create_layout()
        self._init_mask_param_sync()
        self._init_mask_preview()

    # ------------------------------------------------------------------ #
    # widgets / layout
    # ------------------------------------------------------------------ #
    @staticmethod
    def _make_mask_spinbox(rng, value, tooltip):
        """Factory for a mirrored evaluation-mask QSpinBox."""
        spn = QSpinBox()
        spn.setRange(rng[0], rng[1])
        spn.setValue(value)
        spn.setKeyboardTracking(False)
        spn.setToolTip(tooltip)
        return spn

    def _create_widgets(self):
        self.comp2CB = QComboBox()
        self.comp2CB.addItems(qf_defaults.COMP2_OPTIONS)
        self.comp2CB.setToolTip(
            "Second general-background component (exp + comp2 + baseline). "
            "'auto' tries lorentzian/powerlaw/stretched each iteration and keeps "
            "the best by anti-oversubtraction score.")
        self.comp2CB.setCurrentIndex(qf_defaults.DEFAULT_COMP2_INDEX)

        self.itersSpnBx = QSpinBox()
        self.itersSpnBx.setRange(*qf_defaults.FIT_MAX_ITERATIONS_RANGE)
        self.itersSpnBx.setValue(qf_defaults.DEFAULT_FIT_MAX_ITERATIONS)
        self.itersSpnBx.setToolTip("Number of equator<->general alternating rounds.")

        self.eqMaxNfevSpnBx = QSpinBox()
        self.eqMaxNfevSpnBx.setRange(*qf_defaults.FIT_MAX_NFEV_RANGE)
        self.eqMaxNfevSpnBx.setSingleStep(qf_defaults.FIT_MAX_NFEV_STEP)
        self.eqMaxNfevSpnBx.setValue(qf_defaults.DEFAULT_EQUATOR_MAX_NFEV)
        self.eqMaxNfevSpnBx.setToolTip(
            "Maximum least-squares iterations (function evaluations) for the "
            "equator fit.")

        self.genMaxNfevSpnBx = QSpinBox()
        self.genMaxNfevSpnBx.setRange(*qf_defaults.FIT_MAX_NFEV_RANGE)
        self.genMaxNfevSpnBx.setSingleStep(qf_defaults.FIT_MAX_NFEV_STEP)
        self.genMaxNfevSpnBx.setValue(qf_defaults.DEFAULT_GENERAL_MAX_NFEV)
        self.genMaxNfevSpnBx.setToolTip(
            "Maximum least-squares iterations (function evaluations) for the "
            "general-background fit.")

        self.fitSizeSpnBx = QSpinBox()
        self.fitSizeSpnBx.setRange(128, 4096)
        self.fitSizeSpnBx.setSingleStep(50)
        # This is the fit radius (rmax); the image is center-cropped to twice
        # this value. Default to 0.8*rmax so the crop tightly covers the data;
        # fall back to 1000 when the parent QF GUI has no rmax yet.
        qf = self._get_quadfold()
        rmax = qf.info.get("rmax") if qf is not None else None
        self.fitSizeSpnBx.setValue(int(rmax * 0.8) if rmax else 1000)
        self.fitSizeSpnBx.setToolTip(
            "Fit radius (rmax) used only for the fitting step, to speed up "
            "processing. The image is center-cropped to twice this value. "
            "Lower values are preferred for faster fitting, as long as they "
            "still cover the data.")

        self.downsampleSpnBx = QSpinBox()
        self.downsampleSpnBx.setRange(*qf_defaults.FIT_DOWNSAMPLE_RANGE)
        self.downsampleSpnBx.setValue(qf_defaults.DEFAULT_FIT_DOWNSAMPLE)
        self.downsampleSpnBx.setToolTip("Downsample factor used during fitting (speed).")

        self.useStep0ChkBx = QCheckBox("Use step-0 projection background")
        self.useStep0ChkBx.setChecked(True)
        self.useStep0ChkBx.setToolTip(
            "Seed the first equator fit with a rough projection-based general "
            "background (per-sector cone fit).")

        self.baselineReductionSpnBx = QDoubleSpinBox()
        self.baselineReductionSpnBx.setRange(*qf_defaults.FIT_REDUCTION_RANGE)
        self.baselineReductionSpnBx.setDecimals(1)
        self.baselineReductionSpnBx.setSingleStep(1.0)
        self.baselineReductionSpnBx.setSuffix(" %")
        self.baselineReductionSpnBx.setValue(qf_defaults.DEFAULT_BASELINE_REDUCTION)
        self.baselineReductionSpnBx.setKeyboardTracking(False)
        self.baselineReductionSpnBx.setToolTip(
            "Cut the general-background baseline by this fraction before "
            "subtracting (always applied) to guard against oversubtraction.\n"
            "After a fit this shows the reduction actually used (including any "
            "auto-reduce increase); editing it rebuilds the background and "
            "residual with the new value.")
        self.baselineReductionSpnBx.valueChanged.connect(self._on_reduction_changed)

        self.equatorReductionSpnBx = QDoubleSpinBox()
        self.equatorReductionSpnBx.setRange(*qf_defaults.FIT_REDUCTION_RANGE)
        self.equatorReductionSpnBx.setDecimals(1)
        self.equatorReductionSpnBx.setSingleStep(1.0)
        self.equatorReductionSpnBx.setSuffix(" %")
        self.equatorReductionSpnBx.setValue(qf_defaults.DEFAULT_EQUATOR_REDUCTION)
        self.equatorReductionSpnBx.setKeyboardTracking(False)
        self.equatorReductionSpnBx.setToolTip(
            "Scale the fitted equator streak down by this fraction before "
            "subtracting (always applied).\n"
            "After a fit this shows the reduction actually used (including any "
            "auto-reduce increase); editing it rebuilds the background and "
            "residual with the new value.")
        self.equatorReductionSpnBx.valueChanged.connect(self._on_reduction_changed)

        self.autoReduceChkBx = QCheckBox("Auto-reduce (equator && baseline)")
        self.autoReduceChkBx.setChecked(qf_defaults.DEFAULT_AUTO_REDUCE)
        self.autoReduceChkBx.setToolTip(
            "Automatically increase both reductions on top of the fixed values "
            "above until the oversubtracted-pixel fraction stops improving.")

        # Evaluation-mask parameters mirrored from the Background Subtraction
        # settings. They drive the general/equator masks used by the fit and are
        # kept two-way synced with the originals (see _init_mask_param_sync).
        self._mask_sync_guard = False
        self.maskEquatorHeightSpnBx = self._make_mask_spinbox(
            qf_defaults.EQUATOR_HEIGHT_RANGE, qf_defaults.DEFAULT_EQUATOR_HEIGHT,
            "Half-height (px) of the equatorial band kept for the equator fit.")
        self.maskEquatorCenterSpnBx = self._make_mask_spinbox(
            qf_defaults.EQUATOR_CENTER_RANGE, qf_defaults.DEFAULT_EQUATOR_CENTER,
            "Radius (px) of the central beam removed from the equator mask.")
        self.maskM1SpnBx = self._make_mask_spinbox(
            qf_defaults.LAYER_LINE_RANGE, qf_defaults.DEFAULT_LAYER_SPACING,
            "Layer-line spacing M1 (px) used to mask the layer lines.")
        self.maskLayerWidthSpnBx = self._make_mask_spinbox(
            qf_defaults.LAYER_LINE_RANGE, qf_defaults.DEFAULT_LAYER_WIDTH,
            "Width (px) of each masked layer line.")
        self.maskRmaxSpnBx = self._make_mask_spinbox(
            qf_defaults.RMIN_RMAX_RANGE, qf_defaults.DEFAULT_RMIN_RMAX,
            "Outer radius (px) of the rmin..rmax annulus used for the fit "
            "masks. Kept in sync with R-max in the Background Subtraction "
            "settings (-1 uses the automatic value).")
        # Equatorial Bragg-peak mask (local to this dialog; drives
        # QuadrantFolder._create_equator_peaks_mask via qf.info).
        self.maskNPeaksSpnBx = self._make_mask_spinbox(
            qf_defaults.N_PEAKS_RANGE, qf_defaults.DEFAULT_N_PEAKS,
            "Number of equatorial Bragg peaks to detect and mask out of the "
            "equator fit (0 masks no peaks).")
        self.maskPeakWidthSpnBx = self._make_mask_spinbox(
            qf_defaults.PEAK_WIDTH_RANGE, qf_defaults.DEFAULT_PEAK_WIDTH,
            "Width (px) of the mask placed over each detected equatorial "
            "Bragg peak.")

        self.saveBgChkBx = QCheckBox("Save fitted backgrounds")
        self.saveBgChkBx.setChecked(qf_defaults.DEFAULT_SAVE_FITTED_BACKGROUNDS)
        self.saveParamsChkBx = QCheckBox("Save fit parameters")
        self.saveParamsChkBx.setChecked(qf_defaults.DEFAULT_SAVE_FIT_PARAMS)
        self.saveParamsChkBx.setToolTip(
            "Saved automatically to <output>/qf_results/bg_fit_params/.")

        self.runButton = QPushButton("Run Fit")
        self.runButton.setStyleSheet(
            "QPushButton { color: #ededed; background-color: #af6207; }")
        self.runButton.clicked.connect(self.runFit)

        self.progressBar = QProgressBar()
        self.progressBar.setRange(0, 100)
        self.progressBar.setValue(0)

        self.statusLabel = QLabel("Ready.")
        self.statusLabel.setWordWrap(True)
        self.statusLabel.setAlignment(Qt.AlignCenter)
        self.statusLabel.setStyleSheet("font-size: 10px;")

        # Before a fit only the mask overlays are available; the full set of
        # views is populated once a fit completes (see _on_finished).
        self.viewModeCB = QComboBox()
        self.viewModeCB.addItems(MASK_VIEW_MODES)
        self.viewModeCB.currentIndexChanged.connect(self._on_view_mode_changed)
        self.viewModeCB.setEnabled(False)

        self.cmapCB = QComboBox()
        self.cmapCB.addItems([name for name, _ in COLORMAPS])
        self.cmapCB.setToolTip("Colormap for the image views.")
        self.cmapCB.currentIndexChanged.connect(self._on_view_mode_changed)

        # clipping / ylim range for the visualization
        self.autoClipChkBx = QCheckBox("Auto range")
        self.autoClipChkBx.setChecked(True)
        self.autoClipChkBx.setToolTip(
            "Auto: 2nd-98th percentile for images, data range for profiles. "
            "Uncheck to set the color-clip (images) or y-limits (profiles) below.")
        self.autoClipChkBx.toggled.connect(self._on_auto_clip_toggled)

        # Toggles whether the "Equator / Meridian profiles" view draws the data
        # projection from the raw image or from the image with the general
        # evaluation mask applied (unused pixels zeroed).
        self.maskedProfileChkBx = QCheckBox("Masked")
        self.maskedProfileChkBx.setChecked(False)
        self.maskedProfileChkBx.setToolTip(
            "Profiles view: show the data projection with the general "
            "evaluation mask applied (masked-out pixels set to 0).")
        self.maskedProfileChkBx.toggled.connect(self._on_view_mode_changed)

        self.clipMinSpnBx = QDoubleSpinBox()
        self.clipMinSpnBx.setRange(-1e9, 1e9)
        self.clipMinSpnBx.setDecimals(2)
        self.clipMinSpnBx.setValue(0.0)
        self.clipMinSpnBx.setKeyboardTracking(True)
        self.clipMinSpnBx.setEnabled(False)
        self.clipMinSpnBx.setToolTip("Minimum (color-clip / y-limit).")
        self.clipMinSpnBx.valueChanged.connect(self._on_clip_value_changed)

        self.clipMaxSpnBx = QDoubleSpinBox()
        self.clipMaxSpnBx.setRange(-1e9, 1e9)
        self.clipMaxSpnBx.setDecimals(2)
        self.clipMaxSpnBx.setValue(1.0)
        self.clipMaxSpnBx.setKeyboardTracking(True)
        self.clipMaxSpnBx.setEnabled(False)
        self.clipMaxSpnBx.setToolTip("Maximum (color-clip / y-limit).")
        self.clipMaxSpnBx.valueChanged.connect(self._on_clip_value_changed)

        # bottom-left panel showing the fitted parameters
        self.paramsText = QTextEdit()
        self.paramsText.setReadOnly(True)
        self.paramsText.setLineWrapMode(QTextEdit.NoWrap)
        self.paramsText.setFont(QFont("Monospace", 9))
        self.paramsText.setPlainText("No fit yet.")

        self.figure = Figure(figsize=(6, 6), tight_layout=True)
        self.canvas = FigureCanvas(self.figure)

        self.applyButton = QPushButton("Apply (use residual) && Close")
        self.applyButton.clicked.connect(self.applyAndClose)
        self.applyButton.setEnabled(False)
        self.closeButton = QPushButton("Close")
        self.closeButton.clicked.connect(self.reject)

    def _create_layout(self):
        main = QHBoxLayout(self)

        # --- left control column ---
        controls = QGroupBox("Fitting")
        form = QFormLayout(controls)
        form.addRow("Component 1:", QLabel("Exponential"))
        form.addRow("Component 2:", self.comp2CB)
        form.addRow("Component 3:", QLabel("Constant Baseline"))

        # Evaluation-mask parameters (mirrored from the Background Subtraction
        # settings) in their own collapsible box, placed above the advanced
        # settings so the masking can be inspected/adjusted (via the mask views)
        # before running a fit.
        self.maskParamsBox = CollapsibleGroupBox(
            "Mask Parameters", start_expanded=True)
        mask_form = QFormLayout()
        mask_form.addRow("Equator Height:", self.maskEquatorHeightSpnBx)
        mask_form.addRow("Equator Center Radius:", self.maskEquatorCenterSpnBx)
        mask_form.addRow("Layer line spacing (M1):", self.maskM1SpnBx)
        mask_form.addRow("Layer line width:", self.maskLayerWidthSpnBx)
        mask_form.addRow("Rmax:", self.maskRmaxSpnBx)
        mask_form.addRow("Equator peaks:", self.maskNPeaksSpnBx)
        mask_form.addRow("Equator peak width:", self.maskPeakWidthSpnBx)
        self.maskParamsBox.set_content_layout(mask_form)
        form.addRow(self.maskParamsBox)

        # Advanced knobs tucked into a collapsible section (collapsed by default).
        self.additionalSettingsBox = CollapsibleGroupBox(
            "Additional Settings", start_expanded=False)
        additional_form = QFormLayout()
        additional_form.addRow("Max iterations:", self.itersSpnBx)
        additional_form.addRow("Equator max fit iters:", self.eqMaxNfevSpnBx)
        additional_form.addRow("General max fit iters:", self.genMaxNfevSpnBx)
        additional_form.addRow("Fit size (rmax*):", self.fitSizeSpnBx)
        additional_form.addRow("Downsample:", self.downsampleSpnBx)
        additional_form.addRow(self.useStep0ChkBx)
        additional_form.addRow("Baseline reduction:", self.baselineReductionSpnBx)
        additional_form.addRow("Equator reduction:", self.equatorReductionSpnBx)
        additional_form.addRow(self.autoReduceChkBx)
        self.additionalSettingsBox.set_content_layout(additional_form)
        form.addRow(self.additionalSettingsBox)

        form.addRow(self.saveBgChkBx)
        form.addRow(self.saveParamsChkBx)
        form.addRow(self.runButton)
        form.addRow(self.progressBar)
        form.addRow(self.statusLabel)

        # bottom-left: fitted-parameters panel
        paramsBox = QGroupBox("Fitted parameters")
        paramsLayout = QVBoxLayout(paramsBox)
        paramsLayout.addWidget(self.paramsText)

        left = QVBoxLayout()
        left.addWidget(controls)
        left.addWidget(paramsBox, 1)
        left.addWidget(self.applyButton)
        left.addWidget(self.closeButton)

        left_widget = QWidget()
        left_widget.setLayout(left)
        left_widget.setMaximumWidth(380)

        # --- right visualization column ---
        right = QVBoxLayout()
        view_row = QHBoxLayout()
        view_row.addWidget(QLabel("View:"))
        view_row.addWidget(self.viewModeCB, 1)
        view_row.addWidget(QLabel("Colormap:"))
        view_row.addWidget(self.cmapCB)
        view_row.addWidget(self.maskedProfileChkBx)
        view_row.addWidget(self.autoClipChkBx)
        view_row.addWidget(QLabel("Min:"))
        view_row.addWidget(self.clipMinSpnBx)
        view_row.addWidget(QLabel("Max:"))
        view_row.addWidget(self.clipMaxSpnBx)
        right.addLayout(view_row)
        right.addWidget(self.canvas, 1)

        right_widget = QWidget()
        right_widget.setLayout(right)

        main.addWidget(left_widget)
        main.addWidget(right_widget, 1)

    # ------------------------------------------------------------------ #
    # inputs from the parent QF GUI
    # ------------------------------------------------------------------ #
    def _get_quadfold(self):
        parent = self._parent_gui if self._parent_gui is not None else self.parent()
        return getattr(parent, "quadFold", None) if parent is not None else None

    def _current_image_path(self):
        """Full path of the image being processed, for the status label."""
        qf = self._get_quadfold()
        if qf is None:
            return ""
        d = getattr(qf, "img_path", "") or ""
        n = getattr(qf, "img_name", "") or ""
        return os.path.join(d, n) if d else n

    def _grab_inputs(self, warn=True):
        """Collect the folded image, masks and rmin/rmax from the parent QF GUI.

        Returns (img, general_mask, equator_mask, rmin, rmax, rminrmax_mask)
        or None. Pass ``warn=False`` to suppress the warning popups (used for the
        silent mask preview built when the dialog opens).
        """
        qf = self._get_quadfold()
        if qf is None or not getattr(qf, "imgCache", None):
            if warn:
                QMessageBox.warning(self, "No image",
                                    "No processed folded image available. Process "
                                    "an image in Quadrant Folding first.")
            return None

        # Recompute the average fold from the original image; calculateAvgFold
        # rebuilds it fresh (unaffected by any residual applied to resultImg),
        # so every run starts from the unmodified fold.
        try:
            qf.calculateAvgFold()
        except Exception as e:  # noqa: BLE001
            if warn:
                QMessageBox.warning(self, "No image",
                                    f"Could not compute the average fold:\n{e}")
            return None
        avg_fold = qf.imgCache.get("avg_fold")
        if avg_fold is None:
            if warn:
                QMessageBox.warning(self, "No image",
                                    "No folded image (avg_fold) found.")
            return None

        img = makeFullImage(avg_fold).astype(float)
        h, w = img.shape

        # Masks come from QuadrantFolder.createMask (background masks: 0 = excluded).
        rmin = int(qf.info.get("rmin", 30))
        rmax = int(qf.info.get("rmax", int(0.9 * min(h, w) / 2)))
        print(f"Background fitting: rmin={rmin}, rmax={rmax}, img shape={img.shape}")
        try:
            qf.createMask()
            general_mask = np.asarray(qf.imgCache.get("mask")).astype(bool)
        except Exception as e:  # noqa: BLE001
            if warn:
                QMessageBox.warning(self, "Mask error",
                                    f"Could not build mask from Quadrant Folding:\n{e}")
            return None

        # rmin..rmax annulus: the region shared by both fit masks, and the one
        # used for every oversubtraction metric (fit selection, reduction and
        # the reported number) so optimization and reporting agree.
        try:
            rminrmax_mask = np.asarray(qf._create_rminrmax_mask(h, w)).astype(bool)
        except Exception:  # noqa: BLE001
            rminrmax_mask = None   # fall back to synthetic ring inside the fitter

        # Equator-fit mask: keep the equatorial streak but drop the beam and the
        # equatorial Bragg peaks (rebuilt from QF's own mask pieces).
        equator_mask = self._build_equator_mask(qf, avg_fold, h, w, rminrmax_mask)

        return img, general_mask, equator_mask, rmin, rmax, rminrmax_mask

    def _build_equator_mask(self, qf, avg_fold, h, w, rminrmax_mask):
        """Equator-fit mask from QF's own mask pieces (rmin..rmax ring minus the
        beam and equatorial Bragg peaks). Returns None to fall back to the
        general mask inside the fitter."""
        try:
            full = makeFullImage(avg_fold)
            eq_ring = (rminrmax_mask if rminrmax_mask is not None
                       else np.asarray(qf._create_rminrmax_mask(h, w)).astype(bool))
            return (
                eq_ring
                & np.asarray(qf._create_equator_peaks_mask(h, w, full)).astype(bool)
                & np.asarray(qf._create_non_equator_mask(h, w, full)).astype(bool)
                & np.asarray(qf._create_equator_center_beam_mask(h, w, full)).astype(bool))
        except Exception:  # noqa: BLE001
            return None

    # ------------------------------------------------------------------ #
    # evaluation-mask parameter sync
    # ------------------------------------------------------------------ #
    def _init_mask_param_sync(self):
        """Seed the mirrored mask spinboxes from the Background Subtraction
        controls and keep the two sets two-way synced."""
        self._mask_param_pairs = [
            (self.maskEquatorHeightSpnBx, "equatorMaskHeightSpnBx"),
            (self.maskEquatorCenterSpnBx, "equatorCenterBeamSpnBx"),
            (self.maskM1SpnBx, "m1SpnBx"),
            (self.maskLayerWidthSpnBx, "layerLineWidthSpnBx"),
            (self.maskRmaxSpnBx, "rmaxSpnBx"),
        ]
        for local, src_name in self._mask_param_pairs:
            src = self._source_mask_spinbox(src_name)
            if src is not None:
                local.blockSignals(True)
                local.setValue(src.value())
                local.blockSignals(False)
                src.valueChanged.connect(
                    lambda _v, l=local, s=src: self._on_source_mask_changed(l, s))
            local.valueChanged.connect(
                lambda _v, l=local, n=src_name: self._on_local_mask_changed(l, n))

        # Equatorial Bragg-peak controls have no Background Subtraction
        # counterpart: they live only on qf.info and drive
        # QuadrantFolder._create_equator_peaks_mask directly.
        self._peak_mask_pairs = [
            (self.maskNPeaksSpnBx, "n_peaks", qf_defaults.DEFAULT_N_PEAKS),
            (self.maskPeakWidthSpnBx, "peak_width", qf_defaults.DEFAULT_PEAK_WIDTH),
        ]
        qf = self._get_quadfold()
        info = getattr(qf, "info", None) if qf is not None else None
        for spn, key, default in self._peak_mask_pairs:
            if info is not None:
                spn.blockSignals(True)
                spn.setValue(int(info.get(key, default)))
                spn.blockSignals(False)
                info[key] = spn.value()
            spn.valueChanged.connect(
                lambda _v, s=spn, k=key: self._on_peak_mask_changed(s, k))

    def _on_peak_mask_changed(self, spn, key):
        """User edited an equatorial Bragg-peak control: store the value on
        qf.info and rebuild the masks so the views/next fit reflect it."""
        qf = self._get_quadfold()
        if qf is not None and getattr(qf, "info", None) is not None:
            qf.info[key] = spn.value()
        self._recompute_masks()

    def _init_mask_preview(self):
        """Build the fit masks when the dialog opens so the user can inspect and
        adjust the masking (via the Mask Parameters) before running a fit. On
        success the view dropdown is enabled with the mask overlays only."""
        inputs = self._grab_inputs(warn=False)
        if inputs is None:
            return
        self._inputs = inputs
        self.viewModeCB.setEnabled(True)
        self.updateView()

    def _set_view_modes(self, modes, default_text):
        """Repopulate the view dropdown with ``modes`` and select ``default_text``
        without emitting change signals."""
        self.viewModeCB.blockSignals(True)
        self.viewModeCB.clear()
        self.viewModeCB.addItems(modes)
        idx = modes.index(default_text) if default_text in modes else 0
        self.viewModeCB.setCurrentIndex(idx)
        self.viewModeCB.blockSignals(False)

    def _source_mask_spinbox(self, name):
        """The matching mask spinbox owned by the parent (QF GUI / Background
        Subtraction settings), or None."""
        parent = self._parent_gui if self._parent_gui is not None else self.parent()
        return getattr(parent, name, None) if parent is not None else None

    def _on_local_mask_changed(self, local, src_name):
        """User edited a mirrored mask spinbox: push the value into the source
        control (which refreshes info + the QF mask cache), then rebuild our
        general/equator masks."""
        if self._mask_sync_guard:
            return
        self._mask_sync_guard = True
        try:
            src = self._source_mask_spinbox(src_name)
            if src is not None and src.value() != local.value():
                src.setValue(local.value())
        finally:
            self._mask_sync_guard = False
        self._recompute_masks()

    def _on_source_mask_changed(self, local, src):
        """A Background Subtraction mask control changed elsewhere: mirror it
        here and rebuild our masks."""
        if self._mask_sync_guard:
            return
        self._mask_sync_guard = True
        try:
            if local.value() != src.value():
                local.setValue(src.value())
        finally:
            self._mask_sync_guard = False
        self._recompute_masks()

    def _recompute_masks(self):
        """Rebuild the general/equator masks from the current mask settings so
        the mask views and the next fit reflect the new parameters."""
        if self._inputs is None:
            return
        qf = self._get_quadfold()
        if qf is None or not getattr(qf, "imgCache", None):
            return
        avg_fold = qf.imgCache.get("avg_fold")
        if avg_fold is None:
            return
        img = self._inputs[0]
        h, w = img.shape
        try:
            qf.createMask()
            general_mask = np.asarray(qf.imgCache.get("mask")).astype(bool)
        except Exception:  # noqa: BLE001
            return
        try:
            rminrmax_mask = np.asarray(qf._create_rminrmax_mask(h, w)).astype(bool)
        except Exception:  # noqa: BLE001
            rminrmax_mask = self._inputs[5] if len(self._inputs) > 5 else None
        equator_mask = self._build_equator_mask(qf, avg_fold, h, w, rminrmax_mask)
        self._inputs = (img, general_mask, equator_mask,
                        self._inputs[3], self._inputs[4], rminrmax_mask)
        if self.viewModeCB.currentText() in ("General mask", "Equator mask"):
            self.updateView()

    def _build_cfg(self):
        return bf.FitConfig(
            comp2=self.comp2CB.currentText(),
            iters=self.itersSpnBx.value(),
            eq_max_nfev=self.eqMaxNfevSpnBx.value(),
            gen_max_nfev=self.genMaxNfevSpnBx.value(),
            fit_size=2 * self.fitSizeSpnBx.value(),
            downsample_factor=self.downsampleSpnBx.value(),
            use_step0=self.useStep0ChkBx.isChecked(),
            baseline_reduction=self.baselineReductionSpnBx.value() / 100.0,
            equator_reduction=self.equatorReductionSpnBx.value() / 100.0,
            auto_reduce=self.autoReduceChkBx.isChecked(),
        )

    # ------------------------------------------------------------------ #
    # run
    # ------------------------------------------------------------------ #
    def runFit(self):
        inputs = self._grab_inputs()
        if inputs is None:
            return

        box = QMessageBox(self)
        box.setIcon(QMessageBox.Warning)
        box.setWindowTitle("Run background fit")
        box.setText("The fitting may take a few minutes. Continue or exit?")
        continueBtn = box.addButton("Continue", QMessageBox.AcceptRole)
        box.addButton("Exit", QMessageBox.RejectRole)
        box.setDefaultButton(continueBtn)
        box.exec_()
        if box.clickedButton() is not continueBtn:
            return

        self._inputs = inputs
        img, gmask, emask, rmin, rmax, rrmask = inputs
        cfg = self._build_cfg()

        self.runButton.setEnabled(False)
        self.applyButton.setEnabled(False)
        self.viewModeCB.setEnabled(False)
        self.progressBar.setValue(0)
        self.statusLabel.setText("Fitting…")

        self._thread = QThread(self)
        self._worker = FitWorker(img, gmask, emask, rmin, rmax, cfg, rrmask)
        self._worker.moveToThread(self._thread)
        self._thread.started.connect(self._worker.run)
        self._worker.progress.connect(self._on_progress)
        self._worker.finished.connect(self._on_finished)
        self._worker.failed.connect(self._on_failed)
        self._worker.finished.connect(self._thread.quit)
        self._worker.failed.connect(self._thread.quit)
        self._thread.start()

    def _on_progress(self, stage, frac):
        self.progressBar.setValue(int(max(0.0, min(1.0, frac)) * 100))
        self.statusLabel.setText(f"Fitting… ({stage})")

    def _on_finished(self, result):
        self.result = result
        self.progressBar.setValue(100)
        self.statusLabel.setText("Fit complete.")
        self.runButton.setEnabled(True)
        self.applyButton.setEnabled(True)
        self.viewModeCB.setEnabled(True)
        # A fit is available now: offer the full set of views, defaulting to the
        # fitted background.
        self._set_view_modes(VIEW_MODES, "Residual (background removed)")
        self._reflect_reductions_from_result()
        self._update_params_panel()
        self._maybe_save_outputs()
        self.updateView()
        self._warn_if_no_lobes(result)

    def _reflect_reductions_from_result(self):
        """Show the reductions actually used by the fit (post auto-reduce) in the
        spinboxes, without triggering a recompute."""
        if self.result is None:
            return
        eq_red = self.result.get("equator_reduction")
        bl_red = self.result.get("baseline_reduction")
        for box, frac in ((self.equatorReductionSpnBx, eq_red),
                          (self.baselineReductionSpnBx, bl_red)):
            if frac is None:
                continue
            box.blockSignals(True)
            box.setValue(float(frac) * 100.0)
            box.blockSignals(False)

    def _on_reduction_changed(self, _value=0.0):
        """Rebuild the reduced backgrounds/residual from the fitted parameters
        when the user edits a reduction after a fit (no re-fit)."""
        if self.result is None:
            return
        try:
            eq_red = self.equatorReductionSpnBx.value() / 100.0
            bl_red = self.baselineReductionSpnBx.value() / 100.0
            equator, general, residual = bf.reduce_backgrounds(
                self._inputs[0],
                self.result["equator_params"], self.result["general_params"],
                self.result["eq_norm"], self.result["gen_norm"],
                self.result["comp2"], self.result["downsample_factor"],
                eq_red, bl_red,
                self.result.get("equator_keep_baseline", False))
        except Exception as e:  # noqa: BLE001
            self.statusLabel.setText(f"Could not update reductions: {e}")
            return
        self.result["equator"] = equator
        self.result["general"] = general
        self.result["residual"] = residual
        self.result["equator_reduction"] = eq_red
        self.result["baseline_reduction"] = bl_red
        # Refresh the reported oversubtraction over the rmin..rmax annulus so the
        # params panel matches the new residual.
        rrmask = self._inputs[5] if len(self._inputs) > 5 else None
        if rrmask is not None:
            try:
                valid = np.isfinite(residual) & (self._inputs[0] > 0) & np.asarray(rrmask)
                neg = bf.bfu.negative_stats(residual, valid)
                self.result["oversub_frac"] = neg["frac_negative"]
                self.result["oversub_flux_frac"] = neg["oversub_flux_frac"]
                self.result["n_negative"] = neg["n_negative"]
                self.result["n_valid"] = neg["n_valid"]
            except Exception:  # noqa: BLE001
                pass
        self._update_params_panel()
        self.updateView()

    def _warn_if_no_lobes(self, result):
        """Warn when the equator never formed two lobes (lobe_ok False for every
        iteration), i.e. the fit fell back to the least-bad round."""
        rounds = result.get("rounds") or []
        if rounds and all(not r.get("lobe_ok") for r in rounds):
            QMessageBox.warning(
                self, "Equator lobes not detected",
                "The equator fit did not form two lobes in any iteration; the "
                "result fell back to the least-oversubtracted round. The fitted "
                "equator background may be unreliable -- inspect it before "
                "applying.")

    def _on_failed(self, tb):
        self.runButton.setEnabled(True)
        self.statusLabel.setText("Fit failed (see console).")
        print(tb)
        QMessageBox.critical(self, "Fit failed", tb.splitlines()[-1] if tb else "error")

    # ------------------------------------------------------------------ #
    # saving
    # ------------------------------------------------------------------ #
    def _bgfit_save_dir(self):
        """The automatic output folder ``<output>/qf_results/bg_fit_params``.

        Uses the parent QuadrantFolder's output directory so the parameters land
        next to the other qf_results, creating the folder if needed. Returns the
        path, or None if no output directory is known.
        """
        qf = self._get_quadfold()
        if qf is None:
            return None
        out = getattr(qf, "output_dir", None) or getattr(qf, "img_path", None)
        if not out:
            return None
        try:
            from ..utils.file_manager import fullPath
            save_dir = fullPath(fullPath(out, "qf_results"), "bg_fit_params")
            os.makedirs(save_dir, exist_ok=True)
            return save_dir
        except Exception:  # noqa: BLE001
            return None

    def _maybe_save_outputs(self):
        if not (self.saveBgChkBx.isChecked() or self.saveParamsChkBx.isChecked()):
            return
        if self.result is None:
            return
        save_dir = self._bgfit_save_dir()
        if not save_dir:
            QMessageBox.warning(self, "Save error",
                                "No output directory available to save fit outputs.")
            return
        qf = self._get_quadfold()
        name = os.path.splitext(os.path.basename(
            getattr(qf, "img_name", "") or "image"))[0] or "image"
        try:
            if self.saveBgChkBx.isChecked():
                from PIL import Image
                for key in ("equator", "general", "residual"):
                    Image.fromarray(self.result[key].astype(np.float32)).save(
                        os.path.join(save_dir, f"{name}_{key}.tif"))
            if self.saveParamsChkBx.isChecked():
                np.savez(
                    os.path.join(save_dir, f"{name}_bgfit_params.npz"),
                    equator_params=self.result["equator_params"],
                    general_params=self.result["general_params"],
                    comp2=self.result["comp2"],
                    best_iter=self.result["best_iter"],
                    fallback=self.result["fallback"],
                    equator_reduction=self.result.get("equator_reduction", 0.0),
                    baseline_reduction=self.result.get("baseline_reduction", 0.0),
                    oversub_frac=self.result.get("oversub_frac", float("nan")))
            self.statusLabel.setText(f"{self.statusLabel.text()}  Saved to {save_dir}")
        except Exception as e:  # noqa: BLE001
            QMessageBox.warning(self, "Save error", str(e))

    # ------------------------------------------------------------------ #
    # fitted-parameters panel
    # ------------------------------------------------------------------ #
    @staticmethod
    def _softplus(x):
        return float(np.log1p(np.exp(-abs(x))) + max(x, 0.0)) + 1e-6

    def _format_equator_params(self, params):
        """Elliptical equator model: [A, baseline, (amp, u0, u_w, v0, v_w, m)]."""
        p = np.asarray(params, dtype=float).ravel()
        lines = ["EQUATOR (elliptical streak)"]
        if p.size < 2:
            return lines + ["  <no params>"]
        lines.append(f"  Ellipticity A : {p[0]:>10.3f} px")
        lines.append(f"  Baseline      : {p[1]:>10.3f} counts")
        peak_names = ["Equatorial streak", "Lamellar", "Fibrillar"]
        n_peaks = (p.size - 2) // 6
        for i in range(n_peaks):
            idx = 2 + i * 6
            name = peak_names[i] if i < len(peak_names) else f"Peak {i + 1}"
            lines.append(f"  {name}:")
            lines.append(f"    Amplitude   : {p[idx]:>10.3f} counts")
            lines.append(f"    u-position  : {p[idx + 1]:>10.3f} px")
            lines.append(f"    u-width     : {p[idx + 2]:>10.3f} px (FWHM)")
            lines.append(f"    v-position  : {p[idx + 3]:>10.3f} deg")
            lines.append(f"    v-width     : {p[idx + 4]:>10.3f} deg (FWHM)")
            lines.append(f"    m           : {p[idx + 5]:>10.3f}")
        return lines

    def _format_general_params(self, params, comp2):
        """Circular-exponential general bg (K=0, comp1=exponential):
        [baseline, rmin, amp1, scale1_raw, | amp2, scale2_raw, (shape2)]."""
        p = np.asarray(params, dtype=float).ravel()
        lines = [f"GENERAL (exp + {comp2} + baseline)"]
        if p.size < 4:
            return lines + ["  <no params>"]
        lines.append(f"  Baseline      : {p[0]:>10.3f} counts")
        lines.append(f"  Radial rmin   : {p[1]:>10.3f} px")
        lines.append("  Comp1 (exponential):")
        lines.append(f"    amp         : {p[2]:>10.3f} counts")
        lines.append(f"    scale (eff) : {self._softplus(p[3]):>10.3f} px")
        rest = p[4:]
        if rest.size >= 2:
            lines.append(f"  Comp2 ({comp2}):")
            lines.append(f"    amp         : {rest[0]:>10.3f} counts")
            lines.append(f"    scale (eff) : {self._softplus(rest[1]):>10.3f} px")
            if comp2 == "stretched" and rest.size >= 3:
                lines.append(f"    beta        : {rest[2]:>10.3f}")
            elif comp2 == "powerlaw" and rest.size >= 3:
                lines.append(f"    n           : {rest[2]:>10.3f}")
        return lines

    def _update_params_panel(self):
        if self.result is None:
            self.paramsText.setPlainText("No fit yet.")
            return
        r = self.result
        head = [
            f"Best iter      : {r.get('best_iter')}",
            f"comp2 kernel   : {r.get('comp2')}"
            + ("  (fallback)" if r.get("fallback") else ""),
        ]
        over = r.get("oversub_frac")
        if over is not None:
            head.append(
                f"Oversubtraction: {over * 100:.2f}%  "
                f"({r.get('n_negative')}/{r.get('n_valid')} px)")
        eq_red = r.get("equator_reduction")
        bl_red = r.get("baseline_reduction")
        if eq_red is not None and bl_red is not None:
            head.append(
                f"Reductions     : equator {eq_red * 100:.1f}%, "
                f"baseline {bl_red * 100:.1f}%")
        head.append("")
        try:
            eq = self._format_equator_params(r.get("equator_params"))
        except Exception:  # noqa: BLE001
            eq = ["EQUATOR: <unparseable>", str(np.asarray(r.get("equator_params")))]
        try:
            gen = self._format_general_params(r.get("general_params"), r.get("comp2"))
        except Exception:  # noqa: BLE001
            gen = ["GENERAL: <unparseable>", str(np.asarray(r.get("general_params")))]
        self.paramsText.setPlainText("\n".join(head + eq + [""] + gen))

    # ------------------------------------------------------------------ #
    # visualization
    # ------------------------------------------------------------------ #
    # -- clip range handlers --------------------------------------------- #
    def _on_view_mode_changed(self, _index=0):
        self.updateView()

    def _selected_cmap(self):
        """Matplotlib colormap name chosen in the Colormap dropdown."""
        return COLORMAPS[self.cmapCB.currentIndex()][1]

    def _on_auto_clip_toggled(self, checked):
        self.clipMinSpnBx.setEnabled(not checked)
        self.clipMaxSpnBx.setEnabled(not checked)
        self.updateView()

    def _on_clip_value_changed(self, _value=0.0):
        if not self.autoClipChkBx.isChecked():
            self.updateView()

    def _set_clip_boxes(self, lo, hi):
        """Reflect auto-computed limits into the (disabled) boxes without
        re-triggering an update."""
        for box, val in ((self.clipMinSpnBx, lo), (self.clipMaxSpnBx, hi)):
            box.blockSignals(True)
            box.setValue(float(val))
            box.blockSignals(False)

    def _resolve_range(self, data, sym):
        """(lo, hi) for the current view: auto (percentile / data range) when the
        Auto box is checked, otherwise the manual Min/Max box values."""
        if not self.autoClipChkBx.isChecked():
            return self.clipMinSpnBx.value(), self.clipMaxSpnBx.value()
        lo, hi = self._auto_range(data, sym)
        self._set_clip_boxes(lo, hi)
        return lo, hi

    def _auto_range(self, data, sym):
        finite = data[np.isfinite(data)]
        if finite.size == 0:
            return 0.0, 1.0
        if sym:
            hi = float(np.nanpercentile(np.abs(finite), 98)) or 1.0
            return -hi, hi
        lo = float(np.nanpercentile(finite, 2))
        hi = float(np.nanpercentile(finite, 98))
        if hi <= lo:
            hi = lo + 1.0
        return lo, hi

    # -- drawing --------------------------------------------------------- #
    def updateView(self):
        if self._inputs is None:
            return
        img, general_mask, equator_mask = self._inputs[0], self._inputs[1], self._inputs[2]
        mode = self.viewModeCB.currentText()

        # The mask overlays use a fixed grayscale base + green overlay, so the
        # colormap picker is irrelevant there.
        self.cmapCB.setEnabled(mode not in MASK_VIEW_MODES)
        # The "Masked" toggle only affects the profiles view.
        self.maskedProfileChkBx.setEnabled(mode == "Equator / Meridian profiles")

        self.figure.clear()

        # Mask overlays only need the (pre-fit) masks, so they work before a fit.
        if mode == "General mask":
            self._draw_mask(general_mask, img, "General mask (green = used)")
            self.canvas.draw_idle()
            return
        if mode == "Equator mask":
            self._draw_mask(
                equator_mask, img,
                "Equator-fit mask (green = used)",
                none_text="Equator mask fell back to the general mask.")
            self.canvas.draw_idle()
            return

        # Every other view needs a completed fit.
        if self.result is None:
            return
        equator = self.result["equator"]
        general = self.result["general"]
        residual = self.result["residual"]

        if mode == "Equator / Meridian profiles":
            self._draw_profiles(img, equator, general, residual,
                                general_mask, equator_mask)
        else:
            cmap = self._selected_cmap()
            if mode == "Original":
                data, sym = img, False
            elif mode.startswith("Fitted background"):
                data, sym = equator + general, False
            elif mode == "Equator component":
                data, sym = equator, False
            elif mode == "General component":
                data, sym = general, False
            else:  # residual
                data, sym = residual, True
            self._draw_image(data, cmap, sym, mode)

        self.canvas.draw_idle()

    def _draw_image(self, data, cmap, sym, title):
        ax = self.figure.add_subplot(111)
        lo, hi = self._resolve_range(data, sym)
        im = ax.imshow(data, origin="upper", cmap=cmap, vmin=lo, vmax=hi)
        ax.set_title(title)
        self.figure.colorbar(im, ax=ax, fraction=0.046, pad=0.04)

    def _draw_mask(self, mask, img, title, none_text=None):
        ax = self.figure.add_subplot(111)
        if mask is None:
            ax.axis("off")
            ax.text(0.5, 0.5, none_text or "No mask available.",
                    ha="center", va="center", transform=ax.transAxes)
            return
        # Base image (grayscale) with the mask overlaid semi-transparently so the
        # masked regions can be read against the data being fitted.
        lo, hi = self._resolve_range(img, sym=False)
        ax.imshow(img, origin="upper", cmap="gray", vmin=lo, vmax=hi)
        mask = np.asarray(mask).astype(bool)
        overlay = np.zeros(mask.shape + (4,), dtype=float)
        overlay[..., 0] = 0.6            # light green marks the used pixels
        overlay[..., 1] = 1.0
        overlay[..., 2] = 0.6
        overlay[..., 3] = np.where(mask, 0.5, 0.0)
        ax.imshow(overlay, origin="upper")
        ax.set_title(title)

    @staticmethod
    def _masked_projection(img, mask, gap, orientation):
        """Projection over the same central strip as ``get_projection`` but
        summing only the masked-in (used) pixels. Columns of the strip that are
        fully masked out are returned as NaN so the plotted line shows a gap
        there instead of collapsing to zero."""
        img = np.asarray(img, dtype=float)
        mask = np.asarray(mask).astype(bool)
        center = img.shape[0] // 2, img.shape[1] // 2
        if orientation == 0:
            sl = slice(center[0] - gap // 2, center[0] + gap // 2)
            data_strip, mask_strip = img[sl, :], mask[sl, :]
        else:
            sl = slice(center[1] - gap // 2, center[1] + gap // 2)
            data_strip, mask_strip = img[:, sl], mask[:, sl]
        used = mask_strip.sum(axis=orientation)
        proj = np.where(mask_strip, data_strip, 0.0).sum(axis=orientation)
        return np.where(used > 0, proj, np.nan)

    def _draw_profiles(self, img, equator, general, residual,
                       general_mask=None, equator_mask=None):
        bg = equator + general
        # Optionally show the masked data projection: sum only the pixels used
        # by the evaluation mask (mask True = used) and leave gaps where they
        # are fully masked out. The equatorial profile uses the equator-fit mask
        # (falling back to the general mask when unavailable); the meridional
        # profile uses the general mask.
        show_masked = self.maskedProfileChkBx.isChecked()
        masks = {
            0: equator_mask if equator_mask is not None else general_mask,
            1: general_mask,
        }
        ax1 = self.figure.add_subplot(211)
        ax2 = self.figure.add_subplot(212)
        manual = not self.autoClipChkBx.isChecked()
        for ax, orient, name in ((ax1, 0, "Equatorial"), (ax2, 1, "Meridional")):
            mask = masks[orient]
            masked = show_masked and mask is not None
            if masked:
                # Sum over used pixels only and break the line (NaN) where a
                # strip column is fully masked, so the trace shows gaps instead
                # of dropping to zero across the masked-out regions.
                d = self._masked_projection(img, mask, gap=4, orientation=orient)
            else:
                d = get_projection(img, gap=4, orientation=orient)
            b = get_projection(bg, gap=4, orientation=orient)
            r = get_projection(residual, gap=4, orientation=orient)
            ax.plot(d, label="masked data" if masked else "data", alpha=0.7)
            ax.plot(b, label="background", alpha=0.7)
            ax.plot(r, label="residual", ls="--", alpha=0.7)
            ax.axhline(0, color="k", lw=0.6)
            ax.set_title(f"{name} profile")
            if manual:
                ax.set_ylim(self.clipMinSpnBx.value(), self.clipMaxSpnBx.value())
            else:
                dmax = float(np.nanmax(d)) if np.isfinite(d).any() else 1.0
                ax.set_ylim(-abs(dmax) * 0.1 - 1, dmax * 1.1 + 1)
            ax.legend(fontsize=8)
            ax.grid(True, alpha=0.3)
        # reflect the equatorial auto y-limits into the boxes for reference
        if not manual:
            self._set_clip_boxes(*ax1.get_ylim())

    # ------------------------------------------------------------------ #
    # apply / close
    # ------------------------------------------------------------------ #
    def applyAndClose(self):
        self._apply_residual_to_parent()
        self.accept()

    def _apply_residual_to_parent(self):
        """Replace the parent's displayed result with the fitted residual."""
        if self.result is None:
            return
        parent = self._parent_gui if self._parent_gui is not None else self.parent()
        qf = self._get_quadfold()
        if qf is None or not getattr(qf, "imgCache", None):
            return
        residual = self.result["residual"]
        bg_full = self.result["equator"] + self.result["general"]
        h, w = bg_full.shape
        # Quadrant-shaped background (top-left), matching avg_fold so QF can
        # subtract it before its own background removal (imgCache["BgFoldFit"]).
        bg_fold = bg_full[: h // 2, : w // 2].astype(np.float32)
        try:
            qf.imgCache["resultImg"] = residual.astype(np.float32)
            qf.imgCache["BgFold"] = bg_fold
            qf.imgCache["BgFoldFit"] = bg_fold
            qf.info["bgfit_applied"] = True
        except Exception:  # noqa: BLE001
            return
        # A fit has now been applied and a matching background is in the cache,
        # so turn on subtraction of the fitted background automatically. The
        # toggled signal keeps the main-panel proxy and summary in sync.
        if parent is not None:
            chk = getattr(parent, "subtractBgFitChkBx", None)
            if chk is not None and not chk.isChecked():
                chk.setChecked(True)
        # Redraw the parent's result tab from the updated cache.
        if parent is not None:
            for meth in ("refreshResultTab", "refreshAllTabs"):
                fn = getattr(parent, meth, None)
                if callable(fn):
                    try:
                        fn()
                        break
                    except Exception:  # noqa: BLE001
                        continue

    def closeEvent(self, event):
        # ensure the worker thread is stopped
        if self._thread is not None and self._thread.isRunning():
            self._thread.quit()
            self._thread.wait(2000)
        super().closeEvent(event)
