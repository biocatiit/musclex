import os

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

import pytest

pytest.importorskip("PySide6")

from PySide6.QtWidgets import QApplication

output_dir_dialog = pytest.importorskip("musclex.ui.widgets.output_dir_dialog")
from musclex.utils.association_store import AssociationStore


@pytest.fixture(scope="module")
def qapp():
    app = QApplication.instance()
    if app is None:
        app = QApplication([])
    return app


@pytest.fixture
def isolated_associations(monkeypatch, tmp_path):
    store_path = tmp_path / "directory_associations.json"
    store = AssociationStore(store_path)
    monkeypatch.setattr(output_dir_dialog, "_store", store)
    monkeypatch.setattr(output_dir_dialog, "_session_associations", {})
    return store_path


def test_persist_checkbox_is_unchecked_by_default(qapp, tmp_path):
    dialog = output_dir_dialog.OutputDirDialog(
        str(tmp_path / "input"), str(tmp_path / "output")
    )

    assert dialog.persist_checkbox.text() == (
        "Persist this output directory for future sessions"
    )
    assert not dialog.persist_choice

    dialog.persist_checkbox.setChecked(True)
    assert dialog.persist_choice


def test_temporary_association_is_kept_only_in_memory(isolated_associations, tmp_path):
    input_dir = tmp_path / "input"
    output_dir = tmp_path / "temporary-output"
    input_dir.mkdir()
    output_dir.mkdir()
    previous_output = tmp_path / "previous-persistent-output"
    previous_output.mkdir()
    output_dir_dialog._store.save(str(input_dir), str(previous_output))

    output_dir_dialog._set_association(str(input_dir), str(output_dir), persist=False)

    assert output_dir_dialog._lookup_association(str(input_dir)) == str(output_dir)
    assert AssociationStore(isolated_associations).lookup(str(input_dir)) is None

    # A new process starts with an empty in-memory association map.
    output_dir_dialog._session_associations.clear()
    assert output_dir_dialog._lookup_association(str(input_dir)) is None


def test_persistent_association_survives_an_empty_session(
    isolated_associations, tmp_path, monkeypatch
):
    input_dir = tmp_path / "input"
    output_dir = tmp_path / "persistent-output"
    input_dir.mkdir()
    output_dir.mkdir()

    output_dir_dialog._set_association(str(input_dir), str(output_dir), persist=True)
    output_dir_dialog._session_associations.clear()
    monkeypatch.setattr(
        output_dir_dialog, "_store", AssociationStore(isolated_associations)
    )

    assert output_dir_dialog._lookup_association(str(input_dir)) == str(output_dir)
