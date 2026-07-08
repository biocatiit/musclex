import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest

pytest.importorskip("PySide6")

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QApplication

from musclex.ui.widgets.batch_folder_selection_dialog import BatchFolderSelectionDialog


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


def _top_level_item(dialog):
    return dialog.tree.invisibleRootItem().child(0)


def _child_named(parent, name):
    for row in range(parent.childCount()):
        child = parent.child(row)
        if child.text(0) == name:
            return child
    raise AssertionError(f"Could not find child folder {name!r}")


def test_selected_folders_follow_checked_order_and_move_buttons(qapp, tmp_path):
    alpha = tmp_path / "alpha"
    beta = tmp_path / "beta"
    alpha.mkdir()
    beta.mkdir()

    dialog = BatchFolderSelectionDialog(start_dir=str(tmp_path))
    root = _top_level_item(dialog)

    beta_item = _child_named(root, "beta")
    alpha_item = _child_named(root, "alpha")

    beta_item.setCheckState(0, Qt.Checked)
    alpha_item.setCheckState(0, Qt.Checked)

    assert dialog.selected_folders() == [beta, alpha]

    dialog.selectedList.setCurrentRow(1)
    dialog.move_selected_folder_up()

    assert dialog.selected_folders() == [alpha, beta]

    dialog.move_selected_folder_down()

    assert dialog.selected_folders() == [beta, alpha]


def test_unchecking_folder_removes_it_from_selected_order(qapp, tmp_path):
    first = tmp_path / "first"
    second = tmp_path / "second"
    first.mkdir()
    second.mkdir()

    dialog = BatchFolderSelectionDialog(start_dir=str(tmp_path))
    root = _top_level_item(dialog)

    first_item = _child_named(root, "first")
    second_item = _child_named(root, "second")

    first_item.setCheckState(0, Qt.Checked)
    second_item.setCheckState(0, Qt.Checked)
    first_item.setCheckState(0, Qt.Unchecked)

    assert dialog.selected_folders() == [second]
