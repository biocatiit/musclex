import sys
import types

from musclex.headless.mp_executor import process_one_image


def test_process_one_image_uses_per_image_geometry(monkeypatch):
    captured = {}

    class FakeImageData:
        def __init__(self, **kwargs):
            captured.update(kwargs)

    class FakeEquatorImage:
        def __init__(self, image_data, parent, output_dir=None):
            self.info = {}
            self.image = "processed"

        def process(self, settings, param_info):
            pass

    class FakeSettingsManager:
        def __init__(self, output_dir):
            self.output_dir = output_dir

    file_manager = types.ModuleType("musclex.utils.file_manager")
    file_manager.load_image_via_spec = lambda *args: "image"
    image_data_module = types.ModuleType("musclex.utils.image_data")
    image_data_module.ImageData = FakeImageData
    equator_image_module = types.ModuleType("musclex.modules.EquatorImage")
    equator_image_module.EquatorImage = FakeEquatorImage
    settings_manager_module = types.ModuleType("musclex.utils.settings_manager")
    settings_manager_module.SettingsManager = FakeSettingsManager

    for name, module in (
        ("musclex.utils.file_manager", file_manager),
        ("musclex.utils.image_data", image_data_module),
        ("musclex.modules.EquatorImage", equator_image_module),
        ("musclex.utils.settings_manager", settings_manager_module),
    ):
        monkeypatch.setitem(sys.modules, name, module)

    manual_result = process_one_image(
        (
            {"orientation_model": 3},
            None,
            "/input",
            "manual.tif",
            ("tiff", "/input/manual.tif"),
            "/output",
            ((12.5, 18.25), 0.0, 1),
        )
    )
    assert manual_result["error"] is None
    assert captured["center"] == (12.5, 18.25)
    assert captured["rotation"] == 0.0
    assert captured["orientation_model"] == 1

    captured.clear()
    auto_result = process_one_image(
        (
            {"orientation_model": 2},
            None,
            "/input",
            "auto.tif",
            ("tiff", "/input/auto.tif"),
            "/output",
        )
    )
    assert auto_result["error"] is None
    assert captured["center"] is None
    assert captured["rotation"] is None
    assert captured["orientation_model"] == 2
