import builtins
import importlib
import sys
import types
import unittest
from unittest.mock import patch


def _import_ddf_processor_with_stubbed_pyqt():
    stub = types.ModuleType("musclex.ui.pyqt_utils")
    stub.QMainWindow = object
    stub.QApplication = types.SimpleNamespace(
        setOverrideCursor=lambda *_args, **_kwargs: None,
        processEvents=lambda *_args, **_kwargs: None,
        restoreOverrideCursor=lambda *_args, **_kwargs: None,
    )
    stub.Qt = types.SimpleNamespace(WaitCursor="wait", AlignCenter=0)
    stub.QCheckBox = lambda *_args, **_kwargs: None
    sys.modules["musclex.ui.pyqt_utils"] = stub
    sys.modules.pop("musclex.ui.ddf_processor", None)
    return importlib.import_module("musclex.ui.ddf_processor")


class _TrackingFile:
    def __init__(self, lines):
        self._lines = lines
        self.closed = False

    def __iter__(self):
        return iter(self._lines)

    def __enter__(self):
        return self

    def __exit__(self, *_args):
        self.closed = True


class _FakeSpinBox:
    def __init__(self):
        self.range = None

    def setRange(self, minimum, maximum):
        self.range = (minimum, maximum)


class _FakeWindow:
    def __init__(self):
        self.colChkBxs = []
        self.freqSpnBx = _FakeSpinBox()
        self.data = None

    def resize(self, *_args, **_kwargs):
        pass


class _FakeStatus:
    def setText(self, *_args, **_kwargs):
        pass


class _FakeButton:
    def setEnabled(self, *_args, **_kwargs):
        pass


class DDFProcessorTests(unittest.TestCase):
    def setUp(self):
        self.ddf_processor = _import_ddf_processor_with_stubbed_pyqt()

    def test_process_file_closes_file_when_parsing_raises(self):
        tracking_file = _TrackingFile(["Sample\tStim\n", "bad\tline\n"])
        fake_window = types.SimpleNamespace(
            data=None,
            current_file="dummy.ddf",
            statusText=_FakeStatus(),
            generateButton=_FakeButton(),
        )
        with patch.object(builtins, "open", return_value=tracking_file):
            with self.assertRaises(ValueError):
                self.ddf_processor.DDFWindow.processFile(fake_window)
        self.assertTrue(tracking_file.closed)

    def test_update_ui_sets_safe_default_frequency_range_without_data(self):
        fake_window = _FakeWindow()
        self.ddf_processor.DDFWindow.updateUI(fake_window)
        self.assertEqual(fake_window.freqSpnBx.range, (1, 1))


if __name__ == "__main__":
    unittest.main()
