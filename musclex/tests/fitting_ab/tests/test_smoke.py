"""
Smoke tests for the fitting A/B framework.

These tests:

1. Build a synthetic ``FitCase`` (no real image needed).
2. Run it through every registered adapter.
3. Save & load the case via pickle (round-trip schema check).
4. Verify ``LmfitBaselineAdapter`` reproduces the same ``result.values`` as
   the original ``model.fit(...)`` call shape used by ProjectionProcessor.

Run with::

    python -m unittest musclex.tests.fitting_ab.tests.test_smoke
"""

from __future__ import annotations

import os
import tempfile
import unittest
from pathlib import Path

import numpy as np

from musclex.modules.ProjectionProcessor import layerlineModel
from musclex.tests.fitting_ab.adapters import (
    ADAPTER_REGISTRY,
    CaseMeta,
    FitCase,
    FitInputs,
    LmfitBaselineAdapter,
    LmfitPoissonWeightedAdapter,
    LmfitTRFAdapter,
    ParamSpec,
    ReferenceResult,
    SCHEMA_VERSION,
    load_case,
    save_case,
)
from musclex.tests.fitting_ab.adapters.lmfit_adapters import (
    _is_aborted,
    _manual_chi2,
    _maybe_perturb_init,
)
from musclex.tests.fitting_ab.metrics import summarize


def _make_synthetic_case(seed: int = 0) -> FitCase:
    """Construct a tiny but realistic FitCase: 2 Gaussian peaks + Gaussian bg."""
    rng = np.random.default_rng(seed)

    n = 256
    x = np.arange(n, dtype=np.float64)
    centerX = 128.0
    bg_line = 0.0

    # Truth values used to synthesize y; the fit will recover these.
    truth = dict(
        bg_sigma=60.0, bg_amplitude=2000.0,
        center_sigma1=15.0, center_amplitude1=300.0,
        center_sigma2=5.0, center_amplitude2=400.0,
        p_0=-30.0, sigma0=4.0, amplitude0=900.0,
        p_1=+40.0, sigma1=5.0, amplitude1=700.0,
    )
    y_clean = layerlineModel(
        x=x, centerX=centerX, bg_line=bg_line, **truth,
    )
    y = y_clean + rng.normal(0.0, np.sqrt(np.maximum(y_clean, 1.0)) * 0.5)

    # Initial values are intentionally a bit off from truth to exercise
    # the optimizer (but inside the bounds).
    free = {
        "bg_sigma":         ParamSpec(init=80.0, min=1.0, max=2 * n + 1.0),
        "bg_amplitude":     ParamSpec(init=0.0, min=-1.0, max=float(y.sum() + 1)),
        "center_sigma1":    ParamSpec(init=15.0, min=1.0, max=n + 1.0),
        "center_amplitude1":ParamSpec(init=200.0, min=-1.0, max=float(y.sum() + 1)),
        "center_sigma2":    ParamSpec(init=5.0, min=1.0, max=n + 1.0),
        "center_amplitude2":ParamSpec(init=200.0, min=-1.0, max=float(y.sum() + 1)),
        "p_0":              ParamSpec(init=-32.0, min=-35.0, max=-28.0),
        "sigma0":           ParamSpec(init=5.0, min=1.0, max=10.0),
        "amplitude0":       ParamSpec(init=float(y.sum() / 10.0), min=-1.0, max=None),
        "p_1":              ParamSpec(init=42.0, min=37.0, max=45.0),
        "sigma1":           ParamSpec(init=5.0, min=1.0, max=10.0),
        "amplitude1":       ParamSpec(init=float(y.sum() / 10.0), min=-1.0, max=None),
    }

    inputs = FitInputs(
        x=x,
        y=y,
        weights=None,
        model_kind="standard",
        has_voigt=False,
        free_params=free,
        independent_vars={"centerX": centerX, "bg_line": bg_line},
        fixed_params={},
    )
    meta = CaseMeta(
        case_id="synthetic_smoke_001",
        box_name="synthetic",
        box_type="h",
        bgsub=0,
        use_common_sigma=False,
        merid_bg=True,
        peaks_seed=[-30.0, 40.0],
        hull_range=None,
        peak_tolerance=2.0,
        sigma_tolerance=100.0,
        difficulty_tags=["n_peaks=2", "gmm=False", "synthetic"],
    )
    return FitCase(
        schema_version=SCHEMA_VERSION,
        meta=meta,
        inputs=inputs,
        reference=None,  # set below by running baseline once
    )


class SmokeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.case = _make_synthetic_case()

    def test_pickle_roundtrip(self):
        with tempfile.TemporaryDirectory() as d:
            path = Path(d) / "case.pkl"
            save_case(self.case, path)
            loaded = load_case(path)
        self.assertEqual(loaded.schema_version, SCHEMA_VERSION)
        self.assertEqual(loaded.meta.case_id, self.case.meta.case_id)
        self.assertEqual(
            list(loaded.inputs.free_params.keys()),
            list(self.case.inputs.free_params.keys()),
        )
        np.testing.assert_array_equal(loaded.inputs.x, self.case.inputs.x)

    def test_baseline_adapter_runs(self):
        adapter = LmfitBaselineAdapter()
        res = adapter.fit(self.case)
        self.assertTrue(res.success, msg=res.message)
        self.assertTrue(res.converged)
        # Recovered peak positions should be close to truth (-30, +40)
        self.assertAlmostEqual(res.values["p_0"], -30.0, delta=1.0)
        self.assertAlmostEqual(res.values["p_1"], +40.0, delta=1.0)
        self.assertGreater(res.r2, 0.95)

    def test_trf_adapter_runs(self):
        adapter = LmfitTRFAdapter()
        res = adapter.fit(self.case)
        self.assertTrue(res.success, msg=res.message)
        self.assertAlmostEqual(res.values["p_0"], -30.0, delta=1.0)
        self.assertAlmostEqual(res.values["p_1"], +40.0, delta=1.0)

    def test_poisson_adapter_runs(self):
        adapter = LmfitPoissonWeightedAdapter()
        res = adapter.fit(self.case)
        self.assertTrue(res.success, msg=res.message)
        self.assertAlmostEqual(res.values["p_0"], -30.0, delta=1.0)
        self.assertAlmostEqual(res.values["p_1"], +40.0, delta=1.0)

    def test_all_registered_adapters_run(self):
        for name, cls in ADAPTER_REGISTRY.items():
            with self.subTest(adapter=name):
                res = cls().fit(self.case)
                self.assertTrue(res.success, msg=f"{name}: {res.message}")
                self.assertGreater(res.r2, 0.9, msg=name)

    def test_smart_perturb_bounded_param_stays_in_range(self):
        # A position-style param: tight absolute bounds [543, 547], init 546.
        spec = ParamSpec(init=546.0, min=543.0, max=547.0)
        rng = np.random.default_rng(0)

        # Smart perturbation should produce values inside the bound width,
        # i.e. delta within ±perturb * (max-min)/2 = ±0.15*2 = ±0.3.
        perturbed = [_maybe_perturb_init(spec, 0.15, rng) for _ in range(500)]
        self.assertTrue(
            all(543.0 <= v <= 547.0 + 1e-9 for v in perturbed),
            msg=f"some perturbed values escaped bounds: max={max(perturbed)}, min={min(perturbed)}",
        )
        # And the perturbations must actually move things (not a no-op).
        self.assertGreater(np.std(perturbed), 0.05)

    def test_smart_perturb_unbounded_param_uses_multiplicative(self):
        # An amplitude-style param: lower bound only, init >> 0.
        spec = ParamSpec(init=5803.1, min=-1.0, max=None)
        rng = np.random.default_rng(0)
        perturbed = [_maybe_perturb_init(spec, 0.15, rng) for _ in range(500)]
        # Multiplicative ±15% means deviations ~ 0.15 * 5803 ~ 870.
        deviations = [v - 5803.1 for v in perturbed]
        self.assertGreater(np.std(deviations), 100.0)
        self.assertLess(np.max(np.abs(deviations)), 0.18 * 5803.1)

    def test_aborted_detection_helper(self):
        class _R:
            def __init__(self, success, message):
                self.success = success
                self.message = message

        self.assertTrue(_is_aborted(_R(False, "Fit aborted: number of function evaluations > 28000")))
        self.assertTrue(_is_aborted(_R(False, "Maximum number of iterations exceeded")))
        self.assertFalse(_is_aborted(_R(True, "`ftol` termination condition is satisfied.")))

    def test_manual_chi2_matches_unweighted_sum(self):
        rng = np.random.default_rng(7)
        y = rng.normal(100.0, 10.0, size=64)
        predicted = y + rng.normal(0.0, 1.0, size=64)
        residuals = y - predicted
        expected = float(np.sum(residuals ** 2))
        self.assertAlmostEqual(_manual_chi2(y, predicted, weights=None), expected, places=8)

    def test_manual_chi2_handles_non_finite(self):
        y = np.array([1.0, 2.0, 3.0])
        predicted = np.array([1.0, np.nan, 3.0])
        self.assertIsNone(_manual_chi2(y, predicted, weights=None))

    def test_summarize_against_reference(self):
        # First produce a reference via baseline, then re-run baseline and
        # check that the diff metrics are essentially zero.
        case = _make_synthetic_case(seed=42)
        baseline = LmfitBaselineAdapter()
        ref_run = baseline.fit(case)
        case.reference = ReferenceResult(
            adapter=baseline.name,
            success=ref_run.success,
            values=ref_run.values,
            stderr=ref_run.stderr,
            chi2=ref_run.chi2,
            redchi=ref_run.redchi,
            r2=ref_run.r2,
            n_eval=ref_run.n_eval,
            elapsed_s=ref_run.elapsed_s,
            message=ref_run.message,
        )
        replay = baseline.fit(case)
        summary = summarize(replay, case)
        self.assertLess(summary["p_max_abs_diff"], 1e-6)
        # chi2 ratio should be very close to 1.0 (deterministic LM)
        self.assertAlmostEqual(summary["chi2_ratio"], 1.0, places=4)


if __name__ == "__main__":
    unittest.main()
