import numpy as np

from musclex.utils.image_data import ImageData
from musclex.utils.settings_manager import SettingsManager


def test_fingerprint_uses_settings_manager_directory(tmp_path):
    input_dir = tmp_path / "input"
    output_dir = tmp_path / "output"
    input_dir.mkdir()
    output_settings = output_dir / "settings"
    output_settings.mkdir(parents=True)

    settings_manager = SettingsManager(str(output_dir))
    image_data = ImageData(
        img=np.zeros((4, 4), dtype=np.float32),
        img_path=str(input_dir),
        img_name="image.tif",
        apply_mask=True,
        settings_manager=settings_manager,
    )

    before = image_data.get_fingerprint()
    assert before["config:mask.tif"] is None

    mask_path = output_settings / "mask.tif"
    mask_path.write_bytes(b"mask")

    after = image_data.get_fingerprint()
    assert after["config:mask.tif"] is not None


def test_folded_image_uses_geometric_center_over_cached_auto_center(tmp_path):
    settings_manager = SettingsManager(str(tmp_path))
    settings_manager.set_auto_cache("sample_folded.tif", (8.5, 9.5), 1.0)
    settings_manager.save_auto_cache()

    image_data = ImageData(
        img=np.zeros((10, 12), dtype=np.float32),
        img_path=str(tmp_path),
        img_name="sample_folded.tif",
        quadrant_folded=ImageData.detect_folded(str(tmp_path), "sample_folded.tif"),
        settings_manager=settings_manager,
    )

    assert image_data.center == (5.5, 4.5)
