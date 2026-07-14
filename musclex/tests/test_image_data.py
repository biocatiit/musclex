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
