from musclex.modules.QuadrantFolder import QuadrantFolder


def test_slow_path_invalidates_all_fold_dependent_images():
    qf = QuadrantFolder.__new__(QuadrantFolder)
    stale_keys = (
        "avg_fold",
        "BgSubFold",
        "BgFold",
        "mask",
        "resultImg",
        "resultBg",
        "resultFolded",
    )
    qf.imgCache = {key: object() for key in stale_keys}
    qf.imgCache["unrelated"] = object()
    qf.info = {"rmin": 30, "rmax": 1003, "fixed_rmin": 25}

    qf._invalidate_slow_path_image_caches()

    assert all(key not in qf.imgCache for key in stale_keys)
    assert "unrelated" in qf.imgCache
    assert "rmin" not in qf.info
    assert "rmax" not in qf.info
    assert qf.info["fixed_rmin"] == 25
