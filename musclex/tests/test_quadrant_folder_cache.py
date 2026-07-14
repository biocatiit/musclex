import pickle
import os

from musclex.modules.QuadrantFolder import CACHE_FORMAT_VERSION, QuadrantFolder


def test_nested_result_parent_can_be_created(tmp_path):
    result_file = (
        tmp_path
        / "qf_results"
        / "F10_cre-flnc-224"
        / "F10_cre-flnc-224_21_00001_folded_compressed.tif"
    )
    os.makedirs(os.path.dirname(result_file), exist_ok=True)
    result_file.write_bytes(b"result")
    assert result_file.is_file()


def test_cache_info_creates_parent_for_relative_image_name(tmp_path):
    qf = QuadrantFolder.__new__(QuadrantFolder)
    qf.output_dir = str(tmp_path)
    qf.img_name = "F12_cre-flnc-224/F12_cre-flnc-224_23_00001.tif"
    qf.info = {"value": 42}
    qf.version = "test-version"

    qf.cacheInfo()

    cache_file = (
        tmp_path
        / "qf_cache"
        / "F12_cre-flnc-224"
        / "F12_cre-flnc-224_23_00001.tif.info"
    )
    assert cache_file.is_file()
    with cache_file.open("rb") as stream:
        cached = pickle.load(stream)
    assert cached["value"] == 42
    assert cached["program_version"] == "test-version"
    assert cached["cache_format_version"] == CACHE_FORMAT_VERSION
