import os
from types import SimpleNamespace

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor
from PySide6.QtWidgets import QApplication

from musclex.ui.widgets.image_alignment_table import (
    ColKey,
    ImageAlignmentTable,
    axial_angle_difference,
)
from musclex.ui.widgets.image_alignment_widget import (
    ImageAlignmentWidget,
    manual_or_auto,
)
from musclex.ui.widgets.processing_workspace import ProcessingWorkspace
from musclex.utils.settings_manager import SettingsManager


class _RowMapper:
    def __init__(self, names):
        self._names = names

    def row_count(self):
        return len(self._names)

    def name_for_row(self, row):
        return self._names[row]

    def fm_index_for_row(self, row):
        return row

    def populate_table(self, table):
        table.setRowCount(len(self._names))


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def test_detection_snapshot_defers_manual_geometry_until_recaptured(qapp, tmp_path):
    manager = SettingsManager(str(tmp_path))
    manager.set_center("image.tif", (1.0, 2.0), "initial")
    manager.set_rotation("image.tif", 3.0, "initial")
    rows = _RowMapper(["image.tif"])
    workspace = SimpleNamespace(settings_manager=manager)
    panel = ImageAlignmentWidget(
        workspace=workspace,
        row_mapper=rows,
        col_map={ColKey.FRAME: 0},
        headers=["Frame"],
        enable_symmetry_test=False,
        settings_resolver=lambda _row, name: (manager, name),
        detection_snapshot_mode=True,
    )

    panel.capture_geometry_snapshot()
    manager.set_center("image.tif", (10.0, 20.0), "calibration")
    manager.set_rotation("image.tif", 0.0, "manual")

    assert panel._manual_geometry_for_row(0, manager, "image.tif") == (
        (1.0, 2.0),
        3.0,
    )

    panel.capture_geometry_snapshot()
    assert panel._manual_geometry_for_row(0, manager, "image.tif") == (
        (10.0, 20.0),
        0.0,
    )


def test_subsequent_batch_center_is_eagerly_saved_per_manager(tmp_path):
    first_dir = tmp_path / "first"
    second_dir = tmp_path / "second"
    first_dir.mkdir()
    second_dir.mkdir()
    first = SettingsManager(str(first_dir))
    second = SettingsManager(str(second_dir))
    targets = {
        0: (first, "a.tif"),
        1: (first, "b.tif"),
        2: (second, "c.tif"),
    }
    names = ["first/a.tif", "first/b.tif", "second/c.tif"]

    def resolve(name, index=None):
        return targets[names.index(name) if index is None else index]

    workspace = SimpleNamespace(
        _file_manager=SimpleNamespace(names=names, current=1),
        resolve_geometry_settings=resolve,
        _current_filename="first/b.tif",
        _current_image_data=None,
        _center_widget=SimpleNamespace(update_mode_indicator=lambda **_kwargs: None),
        needsReprocess=SimpleNamespace(emit=lambda: None),
        batchSettingsChanged=SimpleNamespace(emit=lambda: None),
        _after_center_save=lambda: None,
        update_mode_statistics=lambda _count: None,
    )

    ProcessingWorkspace.apply_center_to_batch(workspace, (12.5, 18.25), "subsequent")

    assert SettingsManager(str(first_dir)).get_center("a.tif") is None
    assert SettingsManager(str(first_dir)).get_center("b.tif") == (12.5, 18.25)
    assert SettingsManager(str(second_dir)).get_center("c.tif") == (12.5, 18.25)

    ProcessingWorkspace.set_center_from_source(
        workspace, "first/b.tif", (30.0, 40.0), "calibration"
    )

    assert SettingsManager(str(first_dir)).get_center("b.tif") == (30.0, 40.0)
    assert SettingsManager(str(second_dir)).get_center("c.tif") == (12.5, 18.25)


def test_zero_degree_manual_rotation_is_not_replaced_by_auto_rotation():
    assert manual_or_auto(0.0, 17.5) == 0.0
    assert manual_or_auto(None, 17.5) == 17.5


def test_rotation_difference_uses_axial_wraparound():
    assert axial_angle_difference(179.0, -1.0) == 0.0
    assert axial_angle_difference(1.0, 179.0) == 2.0
    assert axial_angle_difference(179.0, 1.0) == -2.0


def test_center_threshold_uses_applied_center_distance_from_applied_base(qapp):
    table = ImageAlignmentTable(
        {
            ColKey.CENTER_DIST: 0,
            ColKey.AUTO_MANUAL_DIST: 1,
            ColKey.ROTATION_DIFF: 2,
            ColKey.AUTO_ROT_DIFF: 3,
            ColKey.IMAGE_DIFF: 4,
        },
        [
            "Dist from Base",
            "Auto-to-Applied",
            "Rotation from Base",
            "Auto Rotation",
            "Image Diff",
        ],
    )
    table.setRowCount(1)

    # A large correction remains visible for diagnosis, but is not itself a
    # reason to mark the corrected image as misaligned.
    table.fill_auto_manual_dist(0, (10.0, 10.0), (20.0, 20.0))
    correction_item = table.item(0, table.col(ColKey.AUTO_MANUAL_DIST))
    assert correction_item.text() == "14.14"
    assert correction_item.data(Qt.BackgroundRole) is None

    # The applied image and applied base centers agree, so residual center
    # misalignment is zero even though the raw auto center was far away.
    table.fill_distance_deviation(
        0,
        effective_center=(20.0, 20.0),
        effective_rotation=None,
        base_center=(20.0, 20.0),
        base_rotation=None,
        dist_thresh_enabled=True,
        dist_thresh=5.0,
    )
    center_dist_item = table.item(0, table.col(ColKey.CENTER_DIST))
    assert center_dist_item.text() == "0.00"
    assert center_dist_item.data(Qt.BackgroundRole) is None
    table.apply_threshold_highlighting(
        dist_enabled=True,
        dist_thresh=5.0,
        rot_enabled=False,
        rot_thresh=0.0,
        diff_enabled=False,
        diff_thresh=0.0,
    )
    assert center_dist_item.data(Qt.BackgroundRole) is None
    assert correction_item.data(Qt.BackgroundRole) is None

    # Residual distance between the applied centers still triggers the limit,
    # including when highlighting is reapplied after a threshold UI change.
    table.fill_distance_deviation(
        0,
        effective_center=(30.0, 20.0),
        effective_rotation=None,
        base_center=(20.0, 20.0),
        base_rotation=None,
        dist_thresh_enabled=False,
        dist_thresh=5.0,
    )
    center_dist_item = table.item(0, table.col(ColKey.CENTER_DIST))
    assert center_dist_item.text() == "10.00"
    assert center_dist_item.data(Qt.BackgroundRole) is None
    table.apply_threshold_highlighting(
        dist_enabled=True,
        dist_thresh=5.0,
        rot_enabled=False,
        rot_thresh=0.0,
        diff_enabled=False,
        diff_thresh=0.0,
    )
    assert center_dist_item.background().color() == QColor(255, 100, 100)
