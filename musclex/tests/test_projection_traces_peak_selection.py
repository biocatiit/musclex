import os
from types import SimpleNamespace

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest

pytest.importorskip("PySide6")

from musclex.modules.ProjectionProcessor import ProcessingBox
from musclex.ui.ProjectionTracesGUI import ProjectionTracesGUI


class _UncheckedButton:
    def isChecked(self):
        return False


class _PeakSelectionHarness:
    _mirror_selected_peaks = staticmethod(ProjectionTracesGUI._mirror_selected_peaks)
    updatePeaks = ProjectionTracesGUI.updatePeaks
    addPeaks = ProjectionTracesGUI.addPeaks

    def __init__(self, selected_peaks):
        template_box = ProcessingBox(
            name="axis",
            coordinates=((0, 100), (0, 20)),
            type="h",
            bgsub=2,
        )
        processor_box = ProcessingBox(
            name="axis",
            coordinates=template_box.coordinates,
            type=template_box.type,
            bgsub=template_box.bgsub,
        )
        self.boxes = {"axis": template_box}
        self.projProc = SimpleNamespace(boxes={"axis": processor_box})
        self.function = ["peaks", {"axis": selected_peaks}]
        self.selectPeaksButton = _UncheckedButton()
        self.process_count = 0

    def processImage(self):
        self.process_count += 1


def test_image_peak_selection_mirrors_single_peak_before_processing():
    gui = _PeakSelectionHarness([25])

    gui.addPeaks()

    assert gui.projProc.boxes["axis"].peaks == [25, -25]
    assert gui.boxes["axis"].peaks == [25]
    assert gui.process_count == 1


def test_image_peak_selection_keeps_all_selected_peaks_before_mirrors():
    gui = _PeakSelectionHarness([12, 30])

    gui.addPeaks()

    assert gui.projProc.boxes["axis"].peaks == [12, 30, -12, -30]
    assert gui.boxes["axis"].peaks == [12, 30]
