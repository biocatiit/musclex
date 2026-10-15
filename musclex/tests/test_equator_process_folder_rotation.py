from types import SimpleNamespace
import importlib


equator_window_module = importlib.import_module("musclex.ui.EquatorWindow")


class _ConfirmDialog:
    Yes = 1
    Cancel = 2
    Warning = 3

    def __init__(self):
        pass

    def setText(self, _text):
        pass

    def setInformativeText(self, _text):
        pass

    def setStandardButtons(self, _buttons):
        pass

    def setIcon(self, _icon):
        pass

    def exec_(self):
        return self.Yes


def test_process_folder_applies_current_rotation_to_all_before_batch(monkeypatch):
    operations = []
    window = SimpleNamespace(
        selected_batch_folders=[],
        file_manager=SimpleNamespace(names=["first.tif", "second.tif"]),
        getSettings=lambda: {"rotationAngle": 17.5},
        _restore_current_folder_after_batch_selection_clear=lambda **_kwargs: None,
        _buildProcessSettingsText=lambda settings, nImg, desc: "Mock settings text",  # <-- Add this line
        workspace=SimpleNamespace(
            apply_rotation_to_batch=lambda angle, scope, reprocess_current: operations.append(
                ("apply", angle, scope, reprocess_current)
            )
        ),
        _batchProcessImages=lambda indices, process_type: operations.append(
            ("process", list(indices), process_type)
        ),
    )
    monkeypatch.setattr(equator_window_module, "QMessageBox", _ConfirmDialog)

    equator_window_module.EquatorWindow.processFolder(window)

    assert operations == [
        ("apply", 17.5, "all", False),
        ("process", [0, 1], "folder"),
    ]
