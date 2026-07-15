import os
from types import SimpleNamespace

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest

pytest.importorskip("PySide6")

from PySide6.QtWidgets import QApplication, QCheckBox, QTabWidget, QWidget

from musclex.modules.ProjectionProcessor import ProcessingBox
from musclex.ui.ProjectionBoxTab import ProjectionBoxTab
from musclex.ui.ProjectionTracesGUI import ProjectionTracesGUI


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


class _FakeProcessor:
    def __init__(self, boxes, rejected=False):
        self.boxes = boxes
        self.state = SimpleNamespace(rejected=rejected)
        self.cache_writes = 0

    def cacheInfo(self):
        self.cache_writes += 1


class _FakeCsvManager:
    def __init__(self):
        self.writes = []

    def writeNewData(self, proj_proc):
        self.writes.append(proj_proc.state.rejected)


class _RejectHarness:
    _syncRejectCheckboxes = ProjectionTracesGUI._syncRejectCheckboxes
    setImageRejected = ProjectionTracesGUI.setImageRejected
    onRejectChanged = ProjectionTracesGUI.onRejectChanged

    def __init__(self, rejected=False):
        self.boxes = {
            name: ProcessingBox(
                name=name,
                coordinates=((0, 20), (0, 10)),
                type="h",
                bgsub=2,
            )
            for name in ("first", "second")
        }
        self.projProc = _FakeProcessor(self.boxes, rejected=rejected)
        self.csv_manager = _FakeCsvManager()
        self.rejectChkBx = QCheckBox("Reject this image")
        self.rejectChkBx.setChecked(rejected)
        self.rejectChkBx.stateChanged.connect(self.onRejectChanged)

        self.tabWidget = QTabWidget()
        self.tabWidget.addTab(QWidget(), "Image")
        self.fit_tabs = []
        for name in self.boxes:
            tab = ProjectionBoxTab(self, name)
            self.fit_tabs.append(tab)
            self.tabWidget.addTab(tab, f"Box {name}")

    def _get_batch_csv_manager_for_proc(self, proj_proc):
        assert proj_proc is self.projProc
        return self.csv_manager

    def keyPressEvent(self, event):
        pass


def test_fit_tab_rejection_updates_state_cache_csv_and_all_tabs(qapp):
    gui = _RejectHarness()

    gui.fit_tabs[0].rejectChkBx.setChecked(True)

    assert gui.projProc.state.rejected is True
    assert gui.rejectChkBx.isChecked()
    assert all(tab.rejectChkBx.isChecked() for tab in gui.fit_tabs)
    assert gui.projProc.cache_writes == 1
    assert gui.csv_manager.writes == [True]


def test_image_tab_can_clear_rejection_without_duplicate_writes(qapp):
    gui = _RejectHarness(rejected=True)

    gui.rejectChkBx.setChecked(False)

    assert gui.projProc.state.rejected is False
    assert not any(tab.rejectChkBx.isChecked() for tab in gui.fit_tabs)
    assert gui.projProc.cache_writes == 1
    assert gui.csv_manager.writes == [False]


def test_fit_tabs_restore_current_image_rejection_state(qapp):
    gui = _RejectHarness(rejected=True)
    assert all(tab.rejectChkBx.isChecked() for tab in gui.fit_tabs)

    gui.projProc.state.rejected = False
    gui._syncRejectCheckboxes(gui.projProc.state.rejected)

    rebuilt_tab = ProjectionBoxTab(gui, "first")
    assert not gui.rejectChkBx.isChecked()
    assert not any(tab.rejectChkBx.isChecked() for tab in gui.fit_tabs)
    assert not rebuilt_tab.rejectChkBx.isChecked()
    assert gui.projProc.cache_writes == 0
    assert gui.csv_manager.writes == []
