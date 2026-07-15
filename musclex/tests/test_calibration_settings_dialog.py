import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pyFAI")

from PySide6.QtWidgets import QApplication

from musclex.CalibrationSettings.CalibrationSettings import (
    CUSTOM_MAR_165_2X2,
    CalibrationSettings,
    _find_mar_165_base_detector_name,
    _halved_shape,
)


class _FakeSettingsManager:
    def __init__(self):
        self.saved_cache = None

    def save_calibration_cache(self, cache):
        self.saved_cache = cache

    def load_calibration_cache_file(self, *args, **kwargs):
        return None


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def _dialog(tmp_path, settings):
    return CalibrationSettings(
        str(tmp_path),
        initial_settings={
            "path": str(tmp_path / "calibration.tif"),
            "settings": settings,
        },
        settings_manager=_FakeSettingsManager(),
    )


def test_detector_choices_are_sorted_and_include_mar_165_2x2():
    choices = CalibrationSettings._detector_choices()

    assert choices == sorted(choices, key=str.casefold)
    assert CUSTOM_MAR_165_2X2 in choices


def test_custom_mar_165_2x2_metadata_doubles_base_pixel_size_and_halves_shape():
    base_name, pixel_size, shape = CalibrationSettings._custom_mar_165_2x2_metadata()

    assert pixel_size > 0
    assert base_name == _find_mar_165_base_detector_name()

    base_pixel_size = None
    if base_name:
        base_pixel_size = CalibrationSettings._detector_pixel_size_mm(None, base_name)
    if base_pixel_size:
        assert pixel_size == pytest.approx(base_pixel_size * 2.0)

    if shape:
        from pyFAI.detectors import Detector

        base_shape = None
        if base_name:
            base_shape = getattr(Detector.registry.get(base_name), "MAX_SHAPE", None)
        if base_shape:
            assert shape == _halved_shape(base_shape)


def test_manual_pixel_size_clears_manual_detector_selection(qapp, tmp_path):
    dialog = _dialog(
        tmp_path,
        {
            "type": "cont",
            "lambda": 0.1,
            "beam_energy": 12.39841984,
            "sdd": 2000.0,
            "pixel_size": 0.079,
            "detector": CUSTOM_MAR_165_2X2,
        },
    )

    dialog.manDetector.setChecked(True)
    dialog.detectorChoice.setCurrentText(CUSTOM_MAR_165_2X2)
    dialog._apply_manual_detector_metadata()
    assert dialog.detector_metadata

    dialog.pixsSpnBx.setValue(0.2)
    dialog.settingChanged("pixS", dialog.pixsSpnBx)

    assert not dialog.manDetector.isChecked()
    assert not dialog.detectorChoice.isEnabled()
    assert dialog.detector_metadata == {}

    assert dialog.saveSettings()
    saved = dialog._settings_manager.saved_cache["settings"]
    assert saved["pixel_size"] == pytest.approx(0.2)
    assert "detector" not in saved
    assert "detector_metadata" not in saved


def test_calibration_image_syncs_scale_and_inferred_sdd(qapp, tmp_path):
    dialog = _dialog(
        tmp_path,
        {
            "type": "img",
            "silverB": 5.0,
            "radius": 100.0,
            "scale": 500.0,
            "center": [10.0, 20.0],
        },
    )
    dialog.lambdaSpnBx.setValue(0.1)
    dialog.pixsSpnBx.setValue(0.2)
    dialog.silverBehenate.setValue(5.0)

    dialog._sync_parameters_from_calibration_image()

    assert dialog.calSettings["scale"] == pytest.approx(500.0)
    assert dialog.sddSpnBx.value() == pytest.approx(1000.0)
    assert "500" in dialog.scaleComputedLabel.text()
    assert "calibrant ring d-spacing" in dialog.scaleComputedLabel.text()
    assert "1000" in dialog.sddComputedLabel.text()


def test_invalid_pixel_size_leaves_sdd_and_computed_sdd_unavailable(qapp, tmp_path):
    dialog = _dialog(
        tmp_path,
        {
            "type": "img",
            "silverB": 5.0,
            "radius": 100.0,
            "scale": 500.0,
            "center": [10.0, 20.0],
        },
    )
    dialog.lambdaSpnBx.setValue(0.1)
    dialog.pixsSpnBx.setValue(0.0)
    dialog.sddSpnBx.setValue(1234.0)

    dialog._sync_parameters_from_calibration_image()

    assert dialog.sddSpnBx.value() == pytest.approx(1234.0)
    assert "unavailable" in dialog.sddComputedLabel.text()
