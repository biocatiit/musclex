from types import SimpleNamespace
from unittest.mock import Mock

import matplotlib
import pytest

# QuadrantFoldingGUI selects the interactive QtAgg backend at import time.
# Rendering is irrelevant here, and CI may not provide a display server.
_matplotlib_use = matplotlib.use
matplotlib.use = lambda *_args, **_kwargs: None
try:
    from musclex.ui import QuadrantFoldingGUI as qf_gui
finally:
    matplotlib.use = _matplotlib_use


class FakeMessageBox:
    ActionRole = object()
    Warning = object()
    Information = object()
    clicked_label = None

    def __init__(self, parent=None):
        self.parent = parent
        self.buttons = {}

    def setWindowTitle(self, _title):
        pass

    def setIcon(self, _icon):
        pass

    def setText(self, _text):
        pass

    def setDetailedText(self, _text):
        pass

    def setInformativeText(self, _text):
        pass

    def addButton(self, label, _role):
        button = object()
        self.buttons[label] = button
        return button

    def exec_(self):
        pass

    def clickedButton(self):
        return self.buttons[self.clicked_label]


@pytest.mark.parametrize(
    ("clicked_label", "expected_close_calls"),
    [("Exit", 1), ("Close", 0)],
)
def test_processing_complete_dialog_actions(
    monkeypatch, clicked_label, expected_close_calls
):
    FakeMessageBox.clicked_label = clicked_label
    monkeypatch.setattr(qf_gui, "QMessageBox", FakeMessageBox)
    app = Mock()
    application = Mock()
    application.instance.return_value = app
    monkeypatch.setattr(qf_gui, "QApplication", application)

    window = SimpleNamespace(
        successCount=1,
        retrySuccessCount=0,
        retryFailCount=0,
        saveErrors={},
        failedTaskErrors={},
        close=Mock(),
    )

    qf_gui.QuadrantFoldingGUI.showProcessingFinishedMessage(window)

    assert window.close.call_count == expected_close_calls
    assert app.quit.call_count == expected_close_calls
