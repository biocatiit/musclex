import os
from types import SimpleNamespace

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import matplotlib

# QuadrantFoldingGUI selects QtAgg at import time, while these tests do not
# render figures and may run without a display server.
_matplotlib_use = matplotlib.use
matplotlib.use = lambda *_args, **_kwargs: None
try:
    from musclex.ui.QuadrantFoldingGUI import QuadrantFoldingGUI
finally:
    matplotlib.use = _matplotlib_use


class _BatchFileManager:
    def __init__(self, initial_specs, batch_specs, current=0):
        self.specs = initial_specs
        self._batch_specs = batch_specs
        self.current = current
        self.switched_to = None

    def load_from_sources(self, _folders):
        self.specs = self._batch_specs
        self.current = 0

    def switch_image_by_index(self, index):
        self.current = index
        self.switched_to = index


def _gui_with_file_manager(file_manager):
    navigator = SimpleNamespace(_load_current_image=lambda: None)
    return SimpleNamespace(
        selected_batch_folders=["first", "second"],
        file_manager=file_manager,
        workspace=SimpleNamespace(navigator=navigator),
    )


def test_batch_selection_keeps_current_image_when_present(tmp_path):
    current_path = str(tmp_path / "second" / "current.tif")
    file_manager = _BatchFileManager(
        initial_specs=[("tiff", current_path)],
        batch_specs=[
            ("tiff", str(tmp_path / "first" / "other.tif")),
            ("tiff", current_path),
        ],
    )

    QuadrantFoldingGUI._load_selected_batch_folders_into_file_manager(
        _gui_with_file_manager(file_manager)
    )

    assert file_manager.current == 1
    assert file_manager.switched_to == 1


def test_batch_selection_uses_first_image_when_current_is_absent(tmp_path):
    file_manager = _BatchFileManager(
        initial_specs=[("tiff", str(tmp_path / "old" / "current.tif"))],
        batch_specs=[("tiff", str(tmp_path / "new" / "first.tif"))],
    )

    QuadrantFoldingGUI._load_selected_batch_folders_into_file_manager(
        _gui_with_file_manager(file_manager)
    )

    assert file_manager.current == 0
    assert file_manager.switched_to is None


def test_batch_selection_distinguishes_h5_frames(tmp_path):
    h5_path = str(tmp_path / "images.h5")
    file_manager = _BatchFileManager(
        initial_specs=[("h5", h5_path, 3)],
        batch_specs=[("h5", h5_path, 0), ("h5", h5_path, 3)],
    )

    QuadrantFoldingGUI._load_selected_batch_folders_into_file_manager(
        _gui_with_file_manager(file_manager)
    )

    assert file_manager.current == 1
    assert file_manager.switched_to == 1
