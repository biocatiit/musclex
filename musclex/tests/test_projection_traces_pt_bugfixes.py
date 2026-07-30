import os
from types import SimpleNamespace

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import numpy as np
import pytest

pytest.importorskip("PySide6")

from PySide6.QtWidgets import QApplication, QWidget

_APP = QApplication.instance() or QApplication([])

from musclex.modules.ProjectionProcessor import (
    ProcessingBox,
    ProcessingState,
    ProjectionProcessor,
)
from musclex.ui.GMMParameterEditorDialog import GMMParameterEditorDialog
from musclex.ui.ProjectionTracesGUI import ProjectionTracesGUI, Worker


def _processor_with_box(box):
    processor = ProjectionProcessor.__new__(ProjectionProcessor)
    processor.state = ProcessingState(version="test", filename="test.tif", dir_path=".")
    processor.state.boxes[box.name] = box
    processor._image_data = SimpleNamespace(center=(100.0, 5.0))
    return processor


def _symmetric_histogram(sigma):
    x = np.arange(201, dtype=float)

    def gaussian(center):
        return (
            1000.0
            / (sigma * np.sqrt(2.0 * np.pi))
            * np.exp(-0.5 * ((x - center) / sigma) ** 2)
        )

    return gaussian(80.0) + gaussian(120.0)


@pytest.mark.parametrize("use_common_sigma", [False, True])
def test_fit_keeps_mirrored_peak_positions_exactly_symmetric(use_common_sigma):
    box = ProcessingBox(
        name="M3",
        coordinates=((0, 200), (0, 10)),
        type="h",
        bgsub=1,
        peaks=[20.0, -20.0],
        use_common_sigma=use_common_sigma,
        hist2=_symmetric_histogram(4.0),
    )
    processor = _processor_with_box(box)

    processor.fitModel()

    assert box.fit_results is not None
    assert box.fit_results["p_1"] == pytest.approx(-box.fit_results["p_0"], abs=1e-12)
    if use_common_sigma:
        assert box.fit_results["common_sigma"] == pytest.approx(4.0, abs=0.25)
    else:
        assert box.fit_results["sigma0"] == pytest.approx(4.0, abs=0.25)
        assert box.fit_results["sigma1"] == pytest.approx(4.0, abs=0.25)


def test_broad_common_sigma_is_not_capped_at_legacy_bound():
    box = ProcessingBox(
        name="MLL1",
        coordinates=((0, 200), (0, 10)),
        type="h",
        bgsub=1,
        peaks=[20.0, -20.0],
        use_common_sigma=True,
        hist2=_symmetric_histogram(16.0),
    )
    processor = _processor_with_box(box)

    processor.fitModel()

    assert box.fit_results["common_sigma"] == pytest.approx(16.0, abs=0.5)
    assert box.param_bounds["common_sigma"] == {
        "min": 0.5,
        "max": 50.0,
        "source": "auto",
    }


def test_sigma_bounds_migrate_legacy_auto_and_preserve_explicit_user_bounds():
    legacy = ProcessingBox(
        name="legacy",
        coordinates=((0, 10), (0, 10)),
        type="h",
        bgsub=1,
        param_bounds={"common_sigma": {"min": 0.0, "max": 10.0}},
    )
    processor = _processor_with_box(legacy)

    assert processor._get_sigma_bounds("legacy", "common_sigma") == (0.5, 50.0)
    assert legacy.param_bounds["common_sigma"]["source"] == "auto"

    legacy.param_bounds["common_sigma"] = {
        "min": 2.0,
        "max": 18.0,
        "source": "user",
    }
    assert processor._get_sigma_bounds("legacy", "common_sigma") == (2.0, 18.0)


def test_folder_template_keeps_only_user_authored_bounds():
    box = ProcessingBox(
        name="trace",
        coordinates=((0, 10), (0, 10)),
        type="h",
        bgsub=1,
        param_bounds={
            "common_sigma": {"min": 2.0, "max": 18.0, "source": "user"},
            "p_0": {"min": 19.0, "max": 21.0, "source": "auto"},
        },
    )

    copied = ProjectionTracesGUI._user_param_bounds(box)

    assert copied == {"common_sigma": {"min": 2.0, "max": 18.0, "source": "user"}}
    assert copied["common_sigma"] is not box.param_bounds["common_sigma"]


class _CachedProcessor:
    def __init__(self):
        old_box = ProcessingBox(
            name="trace",
            coordinates=((0, 100), (0, 10)),
            type="h",
            bgsub=1,
            peaks=[30.0, -30.0],
            fit_results={"p_0": 30.0, "p_1": -30.0},
        )
        self.state = SimpleNamespace(
            boxes={"trace": old_box},
            original_boxes={"trace": {"boxes": old_box}},
            main_peak_info={"trace": {"bg_sigma": 9.0}},
            rejected=True,
            comments="keep this review note",
        )

    def store_original_boxes(self, name, box):
        self.state.original_boxes[name] = {"boxes": box}


def test_batch_configuration_replaces_cached_peaks_but_preserves_review_metadata():
    processor = _CachedProcessor()
    current_box = ProcessingBox(
        name="trace",
        coordinates=((0, 100), (0, 10)),
        type="h",
        bgsub=1,
        peaks=[60.0],
    )

    Worker._install_current_box_configuration(processor, {"trace": current_box})

    assert processor.state.boxes["trace"].peaks == [60.0, -60.0]
    assert processor.state.boxes["trace"].fit_results is None
    assert processor.state.main_peak_info == {}
    assert processor.state.rejected is True
    assert processor.state.comments == "keep this review note"


class _EditorParent(QWidget):
    def __init__(self, processor):
        super().__init__()
        self.parent = SimpleNamespace(projProc=processor)
        self.preview_params = None
        self.preview_hull_range = None
        self.need_update = False

    def updateUI(self):
        pass


def test_parameter_editor_makes_mirrored_position_read_only():
    box = ProcessingBox(
        name="M3",
        coordinates=((0, 200), (0, 10)),
        type="h",
        bgsub=1,
        peaks=[20.0, -20.0],
        use_common_sigma=True,
        fit_results={
            "p_0": 20.0,
            "p_1": -20.0,
            "amplitude0": 100.0,
            "amplitude1": 100.0,
            "sigma0": 4.0,
            "sigma1": 4.0,
            "common_sigma": 4.0,
            "use_common_sigma": True,
        },
    )
    processor = _processor_with_box(box)
    parent = _EditorParent(processor)
    dialog = GMMParameterEditorDialog(parent, "M3")

    rows = {
        dialog.paramTable.item(row, 1).text(): row
        for row in range(dialog.paramTable.rowCount())
    }
    assert dialog.paramTable.cellWidget(rows["p_0"], 2).isEnabled()
    assert not dialog.paramTable.cellWidget(rows["p_1"], 2).isEnabled()
    assert not dialog.paramTable.cellWidget(rows["p_1"], 3).isEnabled()
    assert not dialog.paramTable.cellWidget(rows["p_1"], 4).isEnabled()

    dialog.close()
    parent.close()
