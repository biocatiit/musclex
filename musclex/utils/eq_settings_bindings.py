"""
eqsettings.json <-> EquatorWindow widget bindings.

These declarative tables drive EquatorWindow.loadSettings(). They are
the inverse of EquatorWindow.getSettings() / EQ_FittingTab.getFittingSettings()
-- if you add a key in either, add it in the right table here.

The whole module is Qt-free on purpose so unit tests (which assert the
bindings stay in sync with the persisted JSON schema) can import it
without dragging in PySide6 / a display.

Sparse-fix schema reminder
--------------------------
EQ persists "fix this parameter" choices as *presence of the key*.
That is, ``"left_fix_sigmac": 1.0`` in the JSON means "tick fixSigmaC
on the left tab and set sigmaCSpinBx = 1.0"; the key being absent
means "leave fixSigmaC unchecked". This is different from QF, where
every flag has a dense slot.

Schema invariant enforced by tests:
    every key persisted to eqsettings.json must be classified by
    classify_eq_setting_key() as one of {spinbox, combo_text,
    combo_index, checkbox, sparse_fix, fitting_fix, special, skip} --
    never 'unknown'.
"""

# (json_key, widget_attr) on EquatorWindow -- value via .value()/.setValue()
EQ_SPINBOX_BINDINGS = (
    ("nPeaks", "nPeakSpnBx"),
    ("mask_thres", "maskThresSpnBx"),
)


# (json_key, widget_attr, to_combo_text)
EQ_COMBO_TEXT_BINDINGS = (("model", "modelSelect", str),)


# (json_key, widget_attr) where the JSON stores an integer index instead
# of the visible text. orientation_model is the index of orientationCmbBx
# (0=Max Intensity, 1=GMM, 2=Herman Half Pi, 3=Herman Pi).
EQ_COMBO_INDEX_BINDINGS = (("orientation_model", "orientationCmbBx"),)


# (json_key, widget_attr) on EquatorWindow -- value via .isChecked()/.setChecked()
EQ_CHECKBOX_BINDINGS = (
    ("isSkeletal", "skeletalChkBx"),
    ("isExtraPeak", "extraPeakChkBx"),
    ("90rotation", "rotation90ChkBx"),
    ("find_oritation", "brightSpot"),
    ("inpaint", "inpaintChkBx"),
)


# Sparse "presence-of-key = enable + setValue" pairs on the EquatorWindow.
# (json_key, checkbox_attr, spinbox_attr)
EQ_SPARSE_FIX_BINDINGS = (
    ("fix_k", "k_chkbx", "k_spnbx"),
    ("fixed_rmin", "fixedRminChkBx", "fixedRmin"),
    ("fixed_rmax", "fixedRmaxChkBx", "fixedRmax"),
)


# Suffixes that, prefixed by 'left' or 'right', identify keys handled by
# EQ_FittingTab.applyLoadedSettings(). Two variants exist:
#
# _fix_* (sparse-fix): key present → check the checkbox + set spinbox value.
#                       key absent → uncheck the checkbox.
# _*    (value hint):  key present → set the spinbox value without checking
#                       the checkbox (parameter is free but has an initial
#                       guess from the previous GUI run).
#
# getFittingSettings() writes exactly one of the two variants per parameter:
# the _fix_ key when the checkbox is checked, the plain key otherwise.
EQ_FITTING_FIX_SUFFIXES = (
    "_fix_sigmac",
    "_fix_sigmas",
    "_fix_sigmad",
    "_fix_gamma",
    "_fix_intz",
    "_fix_sigz",
    "_fix_zline",
    "_fix_gammaz",
    "_fix_intz_EP",
    "_fix_sigz_EP",
    "_fix_zline_EP",
    "_fix_gammaz_EP",
)

# Unfixed value-hint suffixes: written by getFittingSettings() when the
# parameter's fix-checkbox is NOT ticked. applyLoadedSettings() sets the
# spinbox but leaves the checkbox unchecked.
EQ_FITTING_VAL_SUFFIXES = (
    "_sigmac",
    "_sigmas",
    "_sigmad",
    "_gamma",
)


# (json_key, fitting_tab_attr, spinbox_attr, checkbox_attr,
#  requires_model, requires_skeletal, requires_extra_peak)
# Used by EQ_FittingTab.applyLoadedSettings(); kept here so the schema
# test sees the full per-side enumeration.
def _build_fitting_fix_bindings():
    rows = []
    for side in ("left", "right"):
        for suffix, cb, sp, model, skeletal, extra in (
            ("_fix_sigmac", "fixSigmaC", "sigmaCSpinBx", None, False, False),
            ("_fix_sigmas", "fixSigmaS", "sigmaSSpinBx", None, False, False),
            ("_fix_sigmad", "fixSigmaD", "sigmaDSpinBx", None, False, False),
            ("_fix_gamma", "fixGamma", "gammaSpinBx", "Voigt", False, False),
            ("_fix_intz", "fixedIntZ", "intZSpnBx", None, True, False),
            ("_fix_sigz", "fixedSigZ", "sigZSpnBx", None, True, False),
            ("_fix_zline", "fixedZline", "zlineSpnBx", None, True, False),
            ("_fix_gammaz", "fixedGammaZ", "gammaZSpnBx", "Voigt", True, False),
            ("_fix_intz_EP", "fixedIntZEP", "intZSpnBxEP", None, True, True),
            ("_fix_sigz_EP", "fixedSigZEP", "sigZSpnBxEP", None, True, True),
            ("_fix_zline_EP", "fixedZlineEP", "zlineSpnBxEP", None, True, True),
            ("_fix_gammaz_EP", "fixedGammaZEP", "gammaZSpnBxEP", "Voigt", True, True),
        ):
            rows.append((side + suffix, side, cb, sp, model, skeletal, extra))
    return tuple(rows)


EQ_FITTING_FIX_BINDINGS = _build_fitting_fix_bindings()


# Keys handled by custom logic in loadSettings() (not in the simple
# binding tables): nested structures, paired widgets, side effects.
EQ_SPECIAL_KEYS = frozenset(
    {
        # tuple (min_t, b) -- stored on self.fixedIntArea + fixedIntAreaChkBx
        "fixed_int_area",
    }
)


# Keys that may appear in eqsettings.json but are owned by another
# subsystem and the EQ load path intentionally ignores them.
#
#   - Calibration-derived fields (lambda_sdd, detector, calib_center,
#     silverB, radius, type, sdd, pixel_size, lambda, beam_energy):
#     single-sourced from <dataset>/settings/calibration.info via
#     SettingsManager. EquatorWindow.saveSettings() strips these on
#     export and EquatorWindowh.getSettings() strips them on import,
#     so a well-behaved JSON should not contain them. They are listed
#     here only so legacy / hand-edited files do not trip the schema
#     test (they get classified as 'skip' instead of 'unknown').
#   - blank_mask: owned by workspace.get_blank_mask_config().
EQ_SKIP_KEYS = frozenset(
    {
        "lambda_sdd",
        "calib_center",
        "detector",
        "silverB",
        "radius",
        "type",
        "sdd",
        "pixel_size",
        "lambda",
        "beam_energy",
        "blank_mask",
    }
)


def _is_fitting_fix_key(key):
    """True if ``key`` is one of the per-side fitting-tab fix keys."""
    if key.startswith("left"):
        suffix = key[len("left") :]
    elif key.startswith("right"):
        suffix = key[len("right") :]
    else:
        return False
    return suffix in EQ_FITTING_FIX_SUFFIXES


def _is_fitting_val_key(key):
    """True if ``key`` is the unfixed value-hint variant of a fitting param."""
    if key.startswith("left"):
        suffix = key[len("left") :]
    elif key.startswith("right"):
        suffix = key[len("right") :]
    else:
        return False
    return suffix in EQ_FITTING_VAL_SUFFIXES


def classify_eq_setting_key(key):
    """Return one of 'spinbox', 'combo_text', 'combo_index', 'checkbox',
    'sparse_fix', 'fitting_fix', 'fitting_val', 'special', 'skip', 'unknown'
    describing how EquatorWindow.loadSettings() will treat ``key``.

    'unknown' means the test suite should fail -- it indicates either a
    new key in the JSON without a corresponding load path, or an obsolete
    key that should be removed at the source (getSettings /
    getFittingSettings).
    """
    for k, _ in EQ_SPINBOX_BINDINGS:
        if k == key:
            return "spinbox"
    for k, *_ in EQ_COMBO_TEXT_BINDINGS:
        if k == key:
            return "combo_text"
    for k, _ in EQ_COMBO_INDEX_BINDINGS:
        if k == key:
            return "combo_index"
    for k, _ in EQ_CHECKBOX_BINDINGS:
        if k == key:
            return "checkbox"
    for k, _, _ in EQ_SPARSE_FIX_BINDINGS:
        if k == key:
            return "sparse_fix"
    if _is_fitting_fix_key(key):
        return "fitting_fix"
    if _is_fitting_val_key(key):
        return "fitting_val"
    if key in EQ_SPECIAL_KEYS:
        return "special"
    if key in EQ_SKIP_KEYS:
        return "skip"
    return "unknown"
