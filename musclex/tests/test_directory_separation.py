"""
Tests for the Read/Write Directory Separation architecture.

Covers:
  - DirectoryContext  (musclex/utils/directory_context.py)
  - AssociationStore  (musclex/utils/association_store.py)
  - _is_writable helper and resolve_output_directory_headless
    (musclex/ui/widgets/output_dir_dialog.py)

All tests are pure-Python (no Qt) and use tmp_path so they never touch
~/.musclex or the project source tree.

Copyright 1999 Illinois Institute of Technology
(same licence as the rest of MuscleX – see any source file for full text)
"""

import json
import os
import stat
import sys
import unittest
from pathlib import Path
from unittest.mock import patch

import pytest

try:
    from ..utils.directory_context import DirectoryContext
    from ..utils.association_store import AssociationStore
    from ..ui.widgets.output_dir_dialog import (
        _is_writable,
        resolve_output_directory_headless,
    )
except ImportError:
    from musclex.utils.directory_context import DirectoryContext
    from musclex.utils.association_store import AssociationStore
    from musclex.ui.widgets.output_dir_dialog import (
        _is_writable,
        resolve_output_directory_headless,
    )


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _make_readonly_dir(path: Path) -> Path:
    """Create a directory and strip write permission from it."""
    path.mkdir(parents=True, exist_ok=True)
    path.chmod(stat.S_IRUSR | stat.S_IXUSR)  # r-x for owner only
    return path


def _restore_writable(path: Path) -> None:
    """Restore write permission so pytest can clean up tmp_path."""
    path.chmod(stat.S_IRWXU)


# ---------------------------------------------------------------------------
# DirectoryContext
# ---------------------------------------------------------------------------

class TestDirectoryContext:

    def test_colocated_factory_sets_both_dirs(self, tmp_path):
        ctx = DirectoryContext.colocated(str(tmp_path))
        assert ctx.input_dir == str(tmp_path)
        assert ctx.output_dir == str(tmp_path)

    def test_is_colocated_true_when_same_path(self, tmp_path):
        ctx = DirectoryContext.colocated(str(tmp_path))
        assert ctx.is_colocated is True

    def test_is_colocated_false_when_different_paths(self, tmp_path):
        out = tmp_path / "output"
        out.mkdir()
        ctx = DirectoryContext(input_dir=str(tmp_path), output_dir=str(out))
        assert ctx.is_colocated is False

    def test_is_colocated_uses_realpath_for_symlinks(self, tmp_path):
        real_dir = tmp_path / "real"
        real_dir.mkdir()
        link_dir = tmp_path / "link"
        link_dir.symlink_to(real_dir)

        ctx = DirectoryContext(
            input_dir=str(real_dir),
            output_dir=str(link_dir),
        )
        assert ctx.is_colocated is True

    def test_dataclass_fields_accessible(self, tmp_path):
        ctx = DirectoryContext(input_dir="/in", output_dir="/out")
        assert ctx.input_dir == "/in"
        assert ctx.output_dir == "/out"


# ---------------------------------------------------------------------------
# AssociationStore
# ---------------------------------------------------------------------------

class TestAssociationStore:

    @pytest.fixture
    def store_path(self, tmp_path) -> Path:
        return tmp_path / "associations.json"

    @pytest.fixture
    def store(self, store_path) -> AssociationStore:
        return AssociationStore(path=store_path)

    # lookup ----------------------------------------------------------------

    def test_lookup_returns_none_when_empty(self, store, tmp_path):
        assert store.lookup(str(tmp_path)) is None

    def test_lookup_returns_output_dir_after_save(self, store, tmp_path):
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        output_dir.mkdir()

        store.save(str(input_dir), str(output_dir))
        result = store.lookup(str(input_dir))

        assert result == os.path.realpath(str(output_dir))

    def test_lookup_returns_none_when_output_dir_missing(self, store, tmp_path):
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        output_dir.mkdir()

        store.save(str(input_dir), str(output_dir))

        # Delete the output directory so it no longer exists
        output_dir.rmdir()

        assert store.lookup(str(input_dir)) is None

    def test_lookup_uses_realpath_key(self, store, tmp_path):
        real_input = tmp_path / "real_input"
        real_input.mkdir()
        link_input = tmp_path / "link_input"
        link_input.symlink_to(real_input)

        output_dir = tmp_path / "output"
        output_dir.mkdir()

        store.save(str(real_input), str(output_dir))

        # Look up via symlink — should still resolve to same entry
        result = store.lookup(str(link_input))
        assert result == os.path.realpath(str(output_dir))

    # save ------------------------------------------------------------------

    def test_save_persists_to_disk(self, store_path, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store = AssociationStore(path=store_path)
        store.save(str(input_dir), str(output_dir))

        # Load a fresh instance from the same file
        store2 = AssociationStore(path=store_path)
        result = store2.lookup(str(input_dir))
        assert result == os.path.realpath(str(output_dir))

    def test_save_overwrites_previous_association(self, store, tmp_path):
        input_dir = tmp_path / "in"
        first_out = tmp_path / "out1"
        second_out = tmp_path / "out2"
        input_dir.mkdir()
        first_out.mkdir()
        second_out.mkdir()

        store.save(str(input_dir), str(first_out))
        store.save(str(input_dir), str(second_out))

        assert store.lookup(str(input_dir)) == os.path.realpath(str(second_out))

    def test_save_stores_realpath(self, store, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()
        link_out = tmp_path / "link_out"
        link_out.symlink_to(output_dir)

        store.save(str(input_dir), str(link_out))

        # The stored value must be the real path, not the symlink
        result = store.lookup(str(input_dir))
        assert result == os.path.realpath(str(output_dir))

    # remove ----------------------------------------------------------------

    def test_remove_drops_entry(self, store, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store.save(str(input_dir), str(output_dir))
        store.remove(str(input_dir))

        assert store.lookup(str(input_dir)) is None

    def test_remove_is_noop_when_absent(self, store, tmp_path):
        """Removing a non-existent key must not raise."""
        input_dir = tmp_path / "in"
        input_dir.mkdir()
        store.remove(str(input_dir))  # no exception expected

    def test_remove_persists_deletion(self, store_path, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store = AssociationStore(path=store_path)
        store.save(str(input_dir), str(output_dir))
        store.remove(str(input_dir))

        store2 = AssociationStore(path=store_path)
        assert store2.lookup(str(input_dir)) is None

    # robustness ------------------------------------------------------------

    def test_corrupt_json_yields_empty_store(self, store_path):
        store_path.parent.mkdir(parents=True, exist_ok=True)
        store_path.write_text("NOT VALID JSON", encoding="utf-8")

        store = AssociationStore(path=store_path)
        # Should not raise; data should be empty
        assert store._data == {}

    def test_missing_file_yields_empty_store(self, tmp_path):
        nonexistent = tmp_path / "no_dir" / "associations.json"
        store = AssociationStore(path=nonexistent)
        assert store._data == {}

    def test_json_file_format(self, store_path, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store = AssociationStore(path=store_path)
        store.save(str(input_dir), str(output_dir))

        with open(store_path, "r", encoding="utf-8") as f:
            data = json.load(f)

        assert isinstance(data, dict)
        assert os.path.realpath(str(input_dir)) in data


# ---------------------------------------------------------------------------
# _is_writable
# ---------------------------------------------------------------------------

class TestIsWritable:

    def test_existing_writable_dir(self, tmp_path):
        assert _is_writable(str(tmp_path)) is True

    @pytest.mark.skipif(os.getuid() == 0, reason="root ignores chmod")
    def test_existing_readonly_dir(self, tmp_path):
        ro_dir = tmp_path / "readonly"
        _make_readonly_dir(ro_dir)
        try:
            assert _is_writable(str(ro_dir)) is False
        finally:
            _restore_writable(ro_dir)

    def test_nonexistent_path_with_writable_parent(self, tmp_path):
        child = tmp_path / "new_subdir"
        # child doesn't exist yet; parent (tmp_path) is writable
        assert _is_writable(str(child)) is True

    def test_nonexistent_path_with_nonexistent_parent(self, tmp_path):
        deep = tmp_path / "a" / "b" / "c"
        # parent "a/b" doesn't exist
        assert _is_writable(str(deep)) is False


# ---------------------------------------------------------------------------
# resolve_output_directory_headless
# ---------------------------------------------------------------------------

class TestResolveOutputDirectoryHeadless:
    """
    Each test patches the module-level _store used by
    resolve_output_directory_headless so tests are isolated.
    """

    _MODULE = "musclex.ui.widgets.output_dir_dialog"

    def _patch_store(self, store):
        return patch(f"{self._MODULE}._store", store)

    # explicit output_dir given ---------------------------------------------

    def test_explicit_existing_output_dir(self, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        with self._patch_store(store):
            ctx = resolve_output_directory_headless(
                str(input_dir), output_dir=str(output_dir)
            )

        assert ctx is not None
        assert ctx.input_dir == str(input_dir)
        assert ctx.output_dir == os.path.realpath(str(output_dir))
        assert not ctx.is_colocated

    def test_explicit_output_dir_created_when_missing(self, tmp_path):
        input_dir = tmp_path / "in"
        input_dir.mkdir()
        output_dir = tmp_path / "out" / "nested"  # does not exist yet

        store = AssociationStore(path=tmp_path / "assoc.json")
        with self._patch_store(store):
            ctx = resolve_output_directory_headless(
                str(input_dir), output_dir=str(output_dir)
            )

        assert ctx is not None
        assert os.path.isdir(str(output_dir))

    def test_explicit_output_dir_saves_association(self, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        with self._patch_store(store):
            resolve_output_directory_headless(
                str(input_dir), output_dir=str(output_dir)
            )

        assert store.lookup(str(input_dir)) == os.path.realpath(str(output_dir))

    @pytest.mark.skipif(os.getuid() == 0, reason="root ignores chmod")
    def test_explicit_output_dir_unwritable_returns_none(self, tmp_path, capsys):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        _make_readonly_dir(output_dir)

        store = AssociationStore(path=tmp_path / "assoc.json")
        try:
            with self._patch_store(store):
                ctx = resolve_output_directory_headless(
                    str(input_dir), output_dir=str(output_dir)
                )
        finally:
            _restore_writable(output_dir)

        assert ctx is None
        captured = capsys.readouterr()
        assert "not writable" in captured.out.lower() or "error" in captured.out.lower()

    # no explicit output_dir — stored association ---------------------------

    def test_stored_association_used_when_writable(self, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        store.save(str(input_dir), str(output_dir))

        with self._patch_store(store):
            ctx = resolve_output_directory_headless(str(input_dir))

        assert ctx is not None
        assert ctx.output_dir == os.path.realpath(str(output_dir))

    def test_stored_association_ignored_when_dir_gone(self, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        store.save(str(input_dir), str(output_dir))

        # Remove output so lookup returns None
        output_dir.rmdir()

        # input_dir is writable, so should fall back to co-located
        with self._patch_store(store):
            ctx = resolve_output_directory_headless(str(input_dir))

        assert ctx is not None
        assert ctx.is_colocated

    # fallback to co-located ------------------------------------------------

    def test_fallback_to_colocated_when_input_writable(self, tmp_path):
        input_dir = tmp_path / "in"
        input_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        with self._patch_store(store):
            ctx = resolve_output_directory_headless(str(input_dir))

        assert ctx is not None
        assert ctx.is_colocated
        assert ctx.input_dir == str(input_dir)

    @pytest.mark.skipif(os.getuid() == 0, reason="root ignores chmod")
    def test_returns_none_when_input_readonly_and_no_association(
        self, tmp_path, capsys
    ):
        input_dir = tmp_path / "in"
        _make_readonly_dir(input_dir)

        store = AssociationStore(path=tmp_path / "assoc.json")
        try:
            with self._patch_store(store):
                ctx = resolve_output_directory_headless(str(input_dir))
        finally:
            _restore_writable(input_dir)

        assert ctx is None
        captured = capsys.readouterr()
        assert "error" in captured.out.lower()

    # returned context shape ------------------------------------------------

    def test_returned_context_is_directory_context_instance(self, tmp_path):
        input_dir = tmp_path / "in"
        input_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        with self._patch_store(store):
            ctx = resolve_output_directory_headless(str(input_dir))

        assert isinstance(ctx, DirectoryContext)

    def test_colocated_context_input_equals_output(self, tmp_path):
        input_dir = tmp_path / "in"
        input_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        with self._patch_store(store):
            ctx = resolve_output_directory_headless(str(input_dir))

        assert ctx.input_dir == ctx.output_dir


# ---------------------------------------------------------------------------
# Backward-compatibility invariants (table from the architecture doc)
# ---------------------------------------------------------------------------

class TestBackwardCompatibility:

    def test_colocated_context_is_degenerate_legacy_case(self, tmp_path):
        ctx = DirectoryContext.colocated(str(tmp_path))
        assert ctx.is_colocated is True

    def test_explicit_output_dir_produces_non_colocated_context(self, tmp_path):
        input_dir = tmp_path / "in"
        output_dir = tmp_path / "out"
        input_dir.mkdir()
        output_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        with patch("musclex.ui.widgets.output_dir_dialog._store", store):
            ctx = resolve_output_directory_headless(
                str(input_dir), output_dir=str(output_dir)
            )

        assert ctx is not None
        assert not ctx.is_colocated

    def test_module_without_output_dir_arg_defaults_to_colocated(self, tmp_path):
        """
        resolve_output_directory_headless(input_dir) with a writable
        input_dir and no stored association must return a co-located context,
        mirroring the legacy single-directory behaviour.
        """
        input_dir = tmp_path / "images"
        input_dir.mkdir()

        store = AssociationStore(path=tmp_path / "assoc.json")
        with patch("musclex.ui.widgets.output_dir_dialog._store", store):
            ctx = resolve_output_directory_headless(str(input_dir))

        assert ctx is not None
        assert ctx.is_colocated
