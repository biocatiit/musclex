import sys
import importlib
from types import SimpleNamespace

import numpy as np

from musclex.algorithms.calibration_refinement import adapter


def test_center_method_selection_cascades_to_required_previous_stages():
    assert adapter._ordered_center_methods(["gradient"]) == [
        "registration",
        "gradient",
    ]
    assert adapter._ordered_center_methods(["search"]) == [
        "registration",
        "gradient",
        "search",
    ]


def test_package_refine_center_name_is_not_callable_api():
    import musclex.algorithms.calibration_refinement as package

    if hasattr(package, "refine_center"):
        delattr(package, "refine_center")
    assert not hasattr(package, "refine_center")
    imported = importlib.import_module(
        "musclex.algorithms.calibration_refinement.refine_center"
    )
    assert package.refine_center is imported
    assert not callable(package.refine_center)
    assert callable(adapter.refine_center)


def test_center_refinement_runs_methods_in_pipeline_order(monkeypatch):
    calls = []

    def make_grid(width, height, radius_x=None, radius_y=None):
        return np.zeros((1, 1)), np.zeros((1, 1))

    reg = SimpleNamespace(
        make_canonical_grid=make_grid,
        run_ecc_refinement=lambda **kwargs: calls.append("registration")
        or {
            "best_center": (11.0, 12.0),
            "best_angle_deg": 3.0,
            "best_loss": 9.0,
        },
    )
    grad = SimpleNamespace(
        make_canonical_grid=make_grid,
        optimize_symmetry_gradient=lambda **kwargs: calls.append("gradient")
        or {
            "center_x": 12.0,
            "center_y": 13.0,
            "angle_deg": 4.0,
            "loss": 8.0,
        },
    )
    search = SimpleNamespace(
        make_canonical_grid=make_grid,
        parse_levels=lambda levels: [{"level": levels}],
        coarse_to_fine_search=lambda **kwargs: calls.append("search")
        or {
            "center_x": 13.0,
            "center_y": 14.0,
            "angle_deg": 5.0,
            "loss": 7.0,
        },
    )

    monkeypatch.setitem(
        sys.modules,
        "musclex.algorithms.calibration_refinement.refine_center.registration_refinement",
        reg,
    )
    monkeypatch.setitem(
        sys.modules,
        "musclex.algorithms.calibration_refinement.refine_center.gradient_refinement",
        grad,
    )
    monkeypatch.setitem(
        sys.modules,
        "musclex.algorithms.calibration_refinement.refine_center.search_refinement",
        search,
    )

    result = adapter.refine_center(
        np.ones((8, 8)),
        None,
        (10.0, 10.0),
        2.0,
        ["search", "registration", "gradient"],
    )

    assert calls == ["registration", "gradient", "search"]
    assert result["center"] == (13.0, 14.0)
    assert result["rotation"] == 5.0


def test_rotation_refinement_keeps_center_fixed(monkeypatch):
    captured = {}

    def coarse_to_fine_search(**kwargs):
        captured.update(kwargs)
        return {"center_x": 99.0, "center_y": 99.0, "angle_deg": 4.5, "loss": 1.0}

    rotation_search = SimpleNamespace(
        make_canonical_grid=lambda *args, **kwargs: (
            np.zeros((1, 1)),
            np.zeros((1, 1)),
        ),
        parse_angle_only_levels=lambda levels: [{"level": levels}],
        coarse_to_fine_search=coarse_to_fine_search,
    )
    monkeypatch.setitem(
        sys.modules,
        "musclex.algorithms.calibration_refinement.refine_rotation",
        rotation_search,
    )

    result = adapter.refine_rotation(np.ones((8, 8)), None, (3.0, 4.0), 2.0)

    assert captured["init_center"] == (3.0, 4.0)
    assert result["center"] == (3.0, 4.0)
    assert result["rotation"] == 4.5


def test_mask_is_converted_to_excluded_pixel_mask(monkeypatch):
    captured = {}

    def coarse_to_fine_search(**kwargs):
        captured.update(kwargs)
        return {"center_x": 3.0, "center_y": 4.0, "angle_deg": 2.0, "loss": 1.0}

    search = SimpleNamespace(
        make_canonical_grid=lambda *args, **kwargs: (
            np.zeros((1, 1)),
            np.zeros((1, 1)),
        ),
        parse_levels=lambda levels: [{"level": levels}],
        coarse_to_fine_search=coarse_to_fine_search,
    )
    monkeypatch.setitem(
        sys.modules,
        "musclex.algorithms.calibration_refinement.refine_center.search_refinement",
        search,
    )

    adapter.refine_center(
        np.ones((4, 4)),
        np.array([[0, 1, 0, 0]] * 4),
        (2.0, 2.0),
        0.0,
        ["search"],
    )

    assert captured["mask"].dtype == np.uint8
    assert captured["mask"][0, 0] == 0
    assert captured["mask"][0, 1] == 1
