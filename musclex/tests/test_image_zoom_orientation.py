import os
from types import SimpleNamespace
from unittest.mock import Mock

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import matplotlib
import numpy as np
import pytest
from PySide6.QtWidgets import QApplication

from musclex.ui.DoubleZoomGUI import DoubleZoom as LegacyDoubleZoom
from musclex.ui.DoubleZoomViewer import DoubleZoom as ViewerDoubleZoom
from musclex.ui.widgets.image_viewer_widget import ImageViewerWidget

# QuadrantFoldingGUI selects QtAgg at import time; these tests run headlessly.
_matplotlib_use = matplotlib.use
matplotlib.use = lambda *_args, **_kwargs: None
try:
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


def test_shared_wheel_zoom_preserves_inverted_image_y_axis(qapplication):
    viewer = ImageViewerWidget()
    viewer.display_image(np.arange(10_000).reshape(100, 100))
    event = SimpleNamespace(xdata=50, ydata=50, button="up")

    viewer._handle_wheel_zoom(event)

    assert abs(np.diff(viewer.axes.get_xlim())[0]) == pytest.approx(90)
    assert abs(np.diff(viewer.axes.get_ylim())[0]) == pytest.approx(90)
    assert viewer.axes.yaxis_inverted()


def test_shared_drag_pan_preserves_inverted_image_y_axis(qapplication):
    viewer = ImageViewerWidget()
    viewer.display_image(np.arange(10_000).reshape(100, 100))
    viewer.set_zoom_bounds((10, 90), (90, 10))
    viewer._pan_start = (50, 50)

    viewer._handle_pan_drag(SimpleNamespace(xdata=55, ydata=55))

    assert viewer.axes.get_xlim() == pytest.approx((5, 85))
    assert viewer.axes.get_ylim() == pytest.approx((85, 5))
    assert viewer.axes.yaxis_inverted()


def test_qf_result_scroll_zoom_preserves_inverted_y_axis():
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


def test_qf_result_drag_preserves_inverted_y_axis():
    axes = Mock()
    window = SimpleNamespace(
        ableToProcess=lambda: True,
        quadFold=SimpleNamespace(imgCache={"resultImg": np.zeros((100, 100))}),
        result_zoom=[(10, 90), (10, 90)],
        resultAxes=axes,
        resultCanvas=Mock(),
        imgCoordOnStatusBar=Mock(),
        function=["r_move", (50, 50)],
    )
    event = SimpleNamespace(xdata=55, ydata=55)

    QuadrantFoldingGUI.resultOnMotion(window, event)

    axes.set_ylim.assert_called_once_with(
        window.result_zoom[1][1], window.result_zoom[1][0]
    )


@pytest.mark.parametrize("double_zoom_class", [LegacyDoubleZoom, ViewerDoubleZoom])
def test_double_zoom_updates_do_not_toggle_orientation(double_zoom_class):
    figure = matplotlib.figure.Figure()
    double_zoom = double_zoom_class(figure)
    image = np.arange(10_000).reshape(100, 100)
    canvas = Mock()

    double_zoom.doubleZoomChecked(image, canvas, center=(50, 50), is_checked=True)
    assert double_zoom.axes.yaxis_inverted()

    double_zoom.mouseHoverBehavior(60, 60, image, canvas, is_checked=True)
    double_zoom.mouseHoverBehavior(65, 65, image, canvas, is_checked=True)

    assert double_zoom.axes.yaxis_inverted()
