from types import SimpleNamespace

from musclex.ui.QFAlignmentDialog import _persist_pending_batch_geometry
from musclex.utils.settings_manager import SettingsManager


class _RowMapper:
    def __init__(self, names):
        self._names = names

    def row_count(self):
        return len(self._names)

    def name_for_row(self, row):
        return self._names[row]


def test_pending_batch_geometry_is_saved_to_each_source_folder(tmp_path):
    first_dir = tmp_path / "first"
    second_dir = tmp_path / "second"
    first_dir.mkdir()
    second_dir.mkdir()

    first = SettingsManager(str(first_dir))
    second = SettingsManager(str(second_dir))
    first.set_center("unrelated.tif", (1.0, 2.0), "manual")
    first.save_center()

    rows = _RowMapper(["first/a.tif", "first/b.tif", "second/c.tif"])
    row_settings = {
        0: (first, "a.tif"),
        1: (first, "b.tif"),
        2: (second, "c.tif"),
    }
    workspace = SimpleNamespace(get_batch_all_geometry=lambda: ((12.5, 18.25), 3.5))

    _persist_pending_batch_geometry(
        workspace,
        rows,
        lambda row, _name: row_settings[row],
    )

    reloaded_first = SettingsManager(str(first_dir))
    reloaded_second = SettingsManager(str(second_dir))
    assert reloaded_first.get_center("a.tif") == (12.5, 18.25)
    assert reloaded_first.get_center("b.tif") == (12.5, 18.25)
    assert reloaded_second.get_center("c.tif") == (12.5, 18.25)
    assert reloaded_first.get_rotation("a.tif") == 3.5
    assert reloaded_first.get_rotation("b.tif") == 3.5
    assert reloaded_second.get_rotation("c.tif") == 3.5
    assert reloaded_first.get_center("unrelated.tif") == (1.0, 2.0)


def test_no_pending_geometry_leaves_source_settings_unchanged(tmp_path):
    source_dir = tmp_path / "source"
    source_dir.mkdir()
    manager = SettingsManager(str(source_dir))
    manager.set_center("image.tif", (4.0, 6.0), "manual")
    manager.save_center()

    workspace = SimpleNamespace(get_batch_all_geometry=lambda: (None, None))
    rows = _RowMapper(["source/image.tif"])

    _persist_pending_batch_geometry(
        workspace,
        rows,
        lambda _row, _name: (manager, "image.tif"),
    )

    reloaded = SettingsManager(str(source_dir))
    assert reloaded.get_center("image.tif") == (4.0, 6.0)
    assert reloaded.get_rotation("image.tif") is None
