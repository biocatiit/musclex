import os

import numpy as np
import pytest

pytest.importorskip("fabio")
pytest.importorskip("PySide6")

from musclex.utils import file_manager


class _ImmediateFuture:
    def __init__(self, value):
        self._value = value

    def result(self):
        return self._value


class _ImmediatePool:
    def __init__(self, max_workers=None):
        pass

    def __enter__(self):
        return self

    def __exit__(self, *args):
        pass

    def submit(self, function, path):
        return _ImmediateFuture(function(path))


def test_nexus_is_treated_as_hdf5():
    assert file_manager.isHdf5("scan.nxs")
    assert file_manager.isHdf5("SCAN.NXS")

    directory, names, current, specs = file_manager.build_provisional_selection(
        os.path.join("data", "scan.nxs")
    )

    assert directory == "data"
    assert names == ["scan_00001.nxs"]
    assert current == 0
    assert specs == [("h5", os.path.join("data", "scan.nxs"), 0)]


def test_nexus_references_are_resolved_and_deduplicated(tmp_path):
    image_dir = tmp_path / "images"
    image_dir.mkdir()
    master = image_dir / "scan_master.h5"
    data = image_dir / "scan_data_000001.h5"
    tiff = image_dir / "frame.tif"
    for path in (master, data, tiff):
        path.touch()

    nexus_path = tmp_path / "scan.nxs"
    with file_manager.h5py.File(nexus_path, "w") as nexus:
        entry = nexus.create_group("entry")
        entry["detector"] = file_manager.h5py.ExternalLink(
            "images/scan_master.h5", "/entry/data"
        )
        entry.create_dataset(
            "image_paths",
            data=[
                b"images/frame.tif",
                b"images/frame.tif",
                b"images/scan_data_000001.h5:/entry/data/data",
                b"images/missing.tif",
            ],
        )

    references = file_manager.resolve_nexus_image_references(str(nexus_path))

    assert references == [str(master), str(tiff)]


def test_file_manager_loads_all_frames_from_referenced_hdf5(tmp_path):
    data_path = tmp_path / "detector.h5"
    with file_manager.h5py.File(data_path, "w") as data_file:
        data_file.attrs["default"] = "entry"
        entry = data_file.create_group("entry")
        entry.attrs["NX_class"] = "NXentry"
        entry.attrs["default"] = "data"
        nxdata = entry.create_group("data")
        nxdata.attrs["NX_class"] = "NXdata"
        nxdata.attrs["signal"] = "data"
        nxdata.create_dataset(
            "data", data=np.arange(1 * 2 * 64 * 80).reshape(1, 2, 64, 80)
        )

    monitor_path = tmp_path / "monitor.h5"
    with file_manager.h5py.File(monitor_path, "w") as monitor_file:
        monitor_file.create_dataset("entry/data/data", shape=(1, 2, 80, 11))

    nexus_path = tmp_path / "scan.nxs"
    with file_manager.h5py.File(nexus_path, "w") as nexus:
        nexus["detector_data"] = file_manager.h5py.ExternalLink(
            "detector.h5", "/entry/data/data"
        )
        nexus["monitor_data"] = file_manager.h5py.ExternalLink(
            "monitor.h5", "/entry/data/data"
        )

    manager = file_manager.FileManager()
    manager.set_from_file(str(nexus_path))
    scan_thread = manager.start_async_scan()
    scan_thread.join(timeout=10)

    assert manager.reference_sources == [str(data_path)]
    assert manager.current_file_type == "h5"
    assert manager.current_image.shape == (64, 80)
    assert manager.names == ["detector_00001.h5", "detector_00002.h5"]


def test_container_only_hdf5_loads_every_frame_without_parent_directory(tmp_path):
    selected_path = tmp_path / "selected.h5"
    with file_manager.h5py.File(selected_path, "w") as selected_file:
        selected_file.create_dataset(
            "entry/data/data", data=np.arange(3 * 64 * 80).reshape(3, 64, 80)
        )
    with file_manager.h5py.File(tmp_path / "unrelated.h5", "w") as unrelated:
        unrelated.create_dataset("data", shape=(2, 64, 80))

    manager = file_manager.FileManager()
    manager.set_from_file(str(selected_path), container_only=True)

    assert manager.container_listing_complete is True
    assert manager.file_list == [("selected.h5", "h5", str(selected_path))]
    assert manager.names == [
        "selected_00001.h5",
        "selected_00002.h5",
        "selected_00003.h5",
    ]
    assert manager.current_image.shape == (64, 80)
    manager.next_frame()
    assert manager.current_frame_idx == 1


def test_hdf5_auto_selection_ignores_detector_calibration_images(tmp_path):
    master_path = tmp_path / "detector_master.h5"
    with file_manager.h5py.File(master_path, "w") as master:
        detector_specific = master.create_group(
            "entry/instrument/detector/detectorSpecific"
        )
        detector_specific.create_dataset(
            "flatfield", data=np.ones((64, 80), dtype=np.float32)
        )
        detector_specific.create_dataset(
            "pixel_mask", data=np.zeros((64, 80), dtype=np.uint32)
        )

    specs, shape = file_manager._hdf5_dataset_frames(str(master_path))

    assert specs == []
    assert shape is None


def test_hdf5_auto_selection_still_finds_detector_data(tmp_path):
    data_path = tmp_path / "detector_data.h5"
    expected = np.arange(2 * 64 * 80, dtype=np.uint32).reshape(2, 64, 80)
    with file_manager.h5py.File(data_path, "w") as data_file:
        data_file.create_dataset("entry/data/data", data=expected)
        data_file.create_dataset(
            "entry/instrument/detector/detectorSpecific/flatfield",
            data=np.ones((64, 80), dtype=np.float32),
        )

    specs, shape = file_manager._hdf5_dataset_frames(str(data_path))

    assert len(specs) == 2
    assert specs[0][2] == "entry/data/data"
    assert shape == (64, 80)


def test_explicit_nexus_dataset_becomes_one_navigable_stack(tmp_path):
    nexus_path = tmp_path / "scan.nxs"
    expected = np.arange(4 * 8 * 9).reshape(4, 8, 9)
    with file_manager.h5py.File(nexus_path, "w") as nexus:
        nexus.create_dataset("entry/images", data=expected)
        # This larger dataset would win the automatic heuristic.
        nexus.create_dataset("entry/unrelated", shape=(10, 128, 128))

    manager = file_manager.FileManager()
    manager.set_from_file(
        str(nexus_path),
        dataset_path="/entry/images",
        container_only=True,
    )

    assert manager.names == [
        "scan_00001.nxs",
        "scan_00002.nxs",
        "scan_00003.nxs",
        "scan_00004.nxs",
    ]
    assert np.array_equal(manager.current_image, expected[0])
    manager.next_frame()
    assert np.array_equal(manager.current_image, expected[1])


def test_directory_scan_expands_nexus_and_filters_redundant_files(
    tmp_path, monkeypatch
):
    for name in (
        "scan_master.nxs",
        "scan_data_000001.h5",
        "other.h5",
        "image.tif",
        "notes.txt",
    ):
        (tmp_path / name).touch()

    frame_counts = {
        "scan_master.nxs": (3, (10, 20)),
        "scan_data_000001.h5": (3, (10, 20)),
        "other.h5": (2, (5, 6)),
    }
    monkeypatch.setattr(file_manager, "ProcessPoolExecutor", _ImmediatePool)
    monkeypatch.setattr(
        file_manager,
        "_h5_nframes",
        lambda path: frame_counts[os.path.basename(path)],
    )

    names, specs, source_map, _sizes = file_manager.scan_directory_images_cached(
        str(tmp_path)
    )
    files = file_manager.scan_directory_files_sync(str(tmp_path))

    assert names == [
        "image.tif",
        "other_00001.h5",
        "other_00002.h5",
        "scan_master_00001.nxs",
        "scan_master_00002.nxs",
        "scan_master_00003.nxs",
    ]
    assert len(specs) == len(names)
    assert str(tmp_path / "scan_master.nxs") in source_map
    assert all("scan_data_000001.h5" not in spec for spec in specs)
    assert [name for name, _kind, _path in files] == [
        "image.tif",
        "other.h5",
        "scan_master.nxs",
    ]
