import os
from types import SimpleNamespace
from unittest.mock import Mock

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import matplotlib
import numpy as np
import pytest
from PySide6.QtWidgets import QApplication

from musclex.ui.widgets.image_viewer_widget import ImageViewerWidget

# QuadrantFoldingGUI selects QtAgg at import time; these tests run headlessly.
_matplotlib_use = matplotlib.use
matplotlib.use = lambda *_args, **_kwargs: None
try:
    from musclex.ui import QuadrantFoldingGUI as qf_gui
    from musclex.ui.QuadrantFoldingGUI import QuadrantFoldingGUI
finally:
    matplotlib.use = _matplotlib_use


@pytest.fixture(scope="module")
def qapplication():
    return QApplication.instance() or QApplication([])


def test_rectangle_zoom_preserves_inverted_image_y_axis(qapplication):
    viewer = ImageViewerWidget(show_display_panel=True)
    viewer.display_image(np.arange(10_000).reshape(100, 100))

    assert viewer.axes.yaxis_inverted()

    viewer._on_zoom_applied([(20, 80), (30, 70)])

    assert viewer.axes.get_xlim() == pytest.approx((20, 80))
    assert viewer.axes.get_ylim() == pytest.approx((70, 30))
    assert viewer.axes.yaxis_inverted()


def test_result_scroll_zoom_preserves_inverted_image_y_axis():
    axes = Mock()
    window = SimpleNamespace(
        quadFold=SimpleNamespace(imgCache={"resultImg": np.zeros((100, 100))}),
        result_zoom=[(0, 100), (0, 100)],
        resultAxes=axes,
        resultCanvas=Mock(),
    )
    event = SimpleNamespace(xdata=50, ydata=50, button="up")

    QuadrantFoldingGUI.resultScrolled(window, event)

    assert window.result_zoom == [(5, 95), (5, 95)]
    axes.set_ylim.assert_called_once_with(95, 5)


def test_ignore_menu_action_uses_captured_fold_number(monkeypatch):
    class FakeSignal:
        def connect(self, callback):
            self.callback = callback

        def emit(self):
            self.callback(False)

    class FakeAction:
        def __init__(self, text, _parent):
            self.text = text
            self.triggered = FakeSignal()

        def setToolTip(self, _text):
            pass

    class FakeMenu:
        latest = None

        def __init__(self, _parent):
            self.actions = []
            FakeMenu.latest = self

        def addAction(self, action):
            self.actions.append(action)

        def popup(self, _position):
            pass

    monkeypatch.setattr(qf_gui, "QAction", FakeAction)
    monkeypatch.setattr(qf_gui, "QMenu", FakeMenu)
    monkeypatch.setattr(qf_gui.QCursor, "pos", lambda: None)

    window = SimpleNamespace(
        quadFold=SimpleNamespace(
            info={"ignore_folds": set()}, getFoldNumber=Mock(return_value=2)
        ),
        addIgnoreQuadrant=Mock(),
        removeIgnoreQuadrant=Mock(),
    )

    QuadrantFoldingGUI._on_image_right_click(window, 10, 20)
    FakeMenu.latest.actions[0].triggered.emit()

    assert FakeMenu.latest.actions[0].text == "Ignore This Quadrant"
    window.addIgnoreQuadrant.assert_called_once_with(2)


def test_unignore_quadrant_is_safe_and_reprocesses():
    window = SimpleNamespace(
        ignoreFolds={2},
        deleteImgCache=Mock(),
        processImage=Mock(),
    )

    QuadrantFoldingGUI.removeIgnoreQuadrant(window, 2)

    assert window.ignoreFolds == set()
    window.deleteImgCache.assert_called_once_with(["avg_fold"])
    window.processImage.assert_called_once_with()
