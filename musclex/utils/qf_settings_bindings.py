"""
qfsettings.json <-> QuadrantFoldingGUI widget bindings.

These declarative tables drive QuadrantFoldingGUI.loadSettings(). They
are the inverse of QuadrantFoldingGUI.getFlags() -- if you add a key
in one, add it in the other.

The whole module is Qt-free on purpose so unit tests (which assert
the bindings stay in sync with the persisted JSON schema) can import
it without dragging in PySide6 / a display.

Schema invariant enforced by tests:
    every key persisted to qfsettings.json must be classified by
    classify_qf_setting_key() as one of {spinbox, combo, checkbox,
    special, skip} -- never 'unknown'.
"""

# (json_key, widget_attr) -- widget value goes through .value()/.setValue()
QF_SPINBOX_BINDINGS = (
    ('cirmin',                    'minPixRange'),
    ('cirmax',                    'maxPixRange'),
    ('win_size_x',                'winSizeX'),
    ('win_size_y',                'winSizeY'),
    ('win_sep_x',                 'winSepX'),
    ('win_sep_y',                 'winSepY'),
    ('radial_bin',                'radialBinSpnBx'),
    ('smooth',                    'smoothSpnBx'),
    ('tension',                   'tensionSpnBx'),
    ('tophat',                    'tophatSpnBx'),
    ('fwhm',                      'gaussFWHM'),
    ('boxcar_x',                  'boxcarX'),
    ('boxcar_y',                  'boxcarY'),
    ('cycles',                    'cycle'),
    ('cirmin_out',                'minPixRangeOut'),
    ('cirmax_out',                'maxPixRangeOut'),
    ('win_size_x_out',            'winSizeOutX'),
    ('win_size_y_out',            'winSizeOutY'),
    ('win_sep_x_out',             'winSepOutX'),
    ('win_sep_y_out',             'winSepOutY'),
    ('radial_bin_out',            'radialBinOutSpnBx'),
    ('smooth_out',                'smoothOutSpnBx'),
    ('tension_out',               'tensionOutSpnBx'),
    ('tophat_out',                'tophatOutSpnBx'),
    ('fwhm_out',                  'gaussFWHMOut'),
    ('boxcar_x_out',              'boxcarOutX'),
    ('boxcar_y_out',              'boxcarOutY'),
    ('cycles_out',                'cycleOut'),
    ('transition_radius',         'tranRSpnBx'),
    ('transition_delta',          'tranDeltaSpnBx'),
    ('max_iterations',            'maxIterationsSpnBx'),
    ('early_stop',                'earlyStopSpnBx'),
    ('equator_mask_height',       'equatorMaskHeightSpnBx'),
    ('equator_center_beam_width', 'equatorCenterBeamSpnBx'),
    ('m1',                        'm1SpnBx'),
    ('layer_line_width',          'layerLineWidthSpnBx'),
    ('fixed_rmin',                'rminSpnBx'),
    ('fixed_rmax',                'rmaxSpnBx'),
)


def _degree_to_combo(value):
    """JSON stores degree as float (e.g. 1.0). degreeCB items are strings
    like '0.5', '1', '2'. Render whole-number floats without a decimal
    so setCurrentText / findText finds the item exactly."""
    try:
        f = float(value)
    except (TypeError, ValueError):
        return str(value)
    if f.is_integer():
        return str(int(f))
    return str(f)


# (json_key, widget_attr, to_combo_text)
QF_COMBO_TEXT_BINDINGS = (
    ('bgsub',      'bgChoiceIn',   str),
    ('bgsub_out',  'bgChoiceOut',  str),
    ('degree',     'degreeCB',     _degree_to_combo),
    ('downsample', 'downsampleCB', lambda v: str(int(v))),
    ('freq',       'freqCB',       str),
)


# (json_key, widget_attr) -- via .isChecked()/.setChecked()
QF_CHECKBOX_BINDINGS = (
    ('smooth_image',                 'smoothImageChkbx'),
    ('optimize_each_image',          'optimizeEachImageChkBx'),
    ('choose_configurations_auto',   'chooseConfigurationsAutoChkBx'),
    ('persist_evaluation_baseline',  'persistEvaluationBaselineChkBx'),
    ('compressed',                   'compressFoldedImageChkBx'),
)


# Keys handled by custom logic in loadSettings() (not in the simple
# binding tables): nested dicts, lists, mode switches, paired widgets.
QF_SPECIAL_KEYS = frozenset({
    'mean_metric_values',         # nested dict, see _sync_metric_and_synthetic_widgets_from_info
    'metric_weights',             # nested dict, ditto
    'methods',                    # list -> _set_selected_optimization_methods
    'steps',                      # list -> stepsLineEdit.setText
    'bg_options',                 # combo .setCurrentIndex; triggers _on_bg_options_changed
    'optimize',                   # not a widget: self.optimizeFlag
    'background_configurations',  # nested list of configs -> bgSubDialog
    'fixed_roi_w',                # paired with fixedRoiChkBx + fixedRoiW spinbox
    'fixed_roi_h',
    'synthetic_amplitude',        # see _sync_metric_and_synthetic_widgets_from_info
    'synthetic_sigma_x',
    'synthetic_sigma_y',
    'evaluation_baseline',
})


# Per-image / runtime state that may appear in older or hand-edited
# qfsettings.json files. We accept them in the JSON for backward
# compatibility but never apply them -- they are reconstructed from
# the current image / current batch driver on every process() call.
#
# The 'silverB', 'radius' and 'type' keys are calibration metadata
# (silver behenate d-spacing, calibration ring radius, calibration
# input type). They are not QF processing parameters and the GUI's
# saveSettings() does not write them; they only appear in legacy
# calibration files (e.g. MAR/PILATUS test data) that have been
# repurposed as qfsettings.json inputs for headless calibration.
# Skipping them on load means the QF GUI ignores those fields
# without complaining, while calibration consumers (Calibration
# Settings dialog, EquatorWindow, etc.) still find them via their
# own code paths.
QF_SKIP_KEYS = frozenset({
    'orientation_model', 'ignore_folds', 'blank_mask', 'apply_mask',
    'mode_angle', 'roi_w', 'roi_h', 'detector', 'center',
    'fold_image', 'rotate',
    'batch_processing', 'force_recalc_bg', 'manual_background_assignments',
    'silverB', 'radius', 'type',
})


def classify_qf_setting_key(key):
    """Return one of 'spinbox', 'combo', 'checkbox', 'special', 'skip',
    'unknown' describing how loadSettings() will treat ``key``.

    'unknown' means the test suite should fail -- it indicates either
    a new key in the JSON without a corresponding load path, or an
    obsolete key that should be removed at the source
    (getFlags / saveSettings).
    """
    for k, _ in QF_SPINBOX_BINDINGS:
        if k == key:
            return 'spinbox'
    for k, *_ in QF_COMBO_TEXT_BINDINGS:
        if k == key:
            return 'combo'
    for k, _ in QF_CHECKBOX_BINDINGS:
        if k == key:
            return 'checkbox'
    if key in QF_SPECIAL_KEYS:
        return 'special'
    if key in QF_SKIP_KEYS:
        return 'skip'
    return 'unknown'
