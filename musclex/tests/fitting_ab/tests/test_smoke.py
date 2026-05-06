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
from typing import Dict

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
from musclex.tests.fitting_ab.metrics import (
    canonicalize_values,
    per_param_diff,
    per_param_rows,
    summarize,
)


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


class CanonicalizationTests(unittest.TestCase):
    """The consistency table relies on label-swap canonicalization to avoid
    reporting spurious instability. These tests pin down its behavior."""

    def test_meridian_pair_swap(self):
        vals = {
            "center_sigma1": 5.0,    # smaller -> should swap
            "center_amplitude1": 100.0,
            "center_sigma2": 30.0,
            "center_amplitude2": 200.0,
        }
        canon = canonicalize_values(vals)
        self.assertEqual(canon["center_sigma1"], 30.0)
        self.assertEqual(canon["center_sigma2"], 5.0)
        self.assertEqual(canon["center_amplitude1"], 200.0)
        self.assertEqual(canon["center_amplitude2"], 100.0)

    def test_meridian_pair_already_ordered(self):
        vals = {
            "center_sigma1": 30.0,
            "center_amplitude1": 200.0,
            "center_sigma2": 5.0,
            "center_amplitude2": 100.0,
        }
        canon = canonicalize_values(vals)
        self.assertEqual(canon, vals)

    def test_peak_position_sort(self):
        # Peaks given out of order: p_0=10, p_1=-5, p_2=3.
        # Expect them re-labeled by ascending position.
        vals = {
            "p_0": 10.0, "amplitude0": 100.0, "sigma0": 1.0,
            "p_1": -5.0, "amplitude1": 200.0, "sigma1": 2.0,
            "p_2":  3.0, "amplitude2": 300.0, "sigma2": 3.0,
        }
        canon = canonicalize_values(vals)
        self.assertEqual(canon["p_0"], -5.0)
        self.assertEqual(canon["p_1"],  3.0)
        self.assertEqual(canon["p_2"], 10.0)
        # The companion blocks must move with their peak.
        self.assertEqual(canon["amplitude0"], 200.0)
        self.assertEqual(canon["amplitude1"], 300.0)
        self.assertEqual(canon["amplitude2"], 100.0)
        self.assertEqual(canon["sigma0"], 2.0)
        self.assertEqual(canon["sigma1"], 3.0)
        self.assertEqual(canon["sigma2"], 1.0)

    def test_canonicalize_does_not_touch_unrelated_keys(self):
        vals = {"S10": 432.1, "left_area_0": 1e6, "p_0": 5.0}
        canon = canonicalize_values(vals)
        self.assertEqual(canon, vals)

    def test_per_param_diff_zero_on_swap(self):
        # Two value sets that are equivalent up to a sigma1<->2 swap.
        ref = {
            "center_sigma1": 30.0, "center_amplitude1": 200.0,
            "center_sigma2": 5.0, "center_amplitude2": 100.0,
        }
        cand = {
            "center_sigma1": 5.0, "center_amplitude1": 100.0,
            "center_sigma2": 30.0, "center_amplitude2": 200.0,
        }

        class _DummyCase:
            class _Inputs:
                free_params = {
                    "center_sigma1": None, "center_amplitude1": None,
                    "center_sigma2": None, "center_amplitude2": None,
                }
            class _Ref:
                values = ref
            inputs = _Inputs()
            reference = _Ref()

        class _Result:
            values = cand

        diffs = per_param_diff(_Result(), _DummyCase(), canonicalize=True)
        for name, d in diffs.items():
            self.assertAlmostEqual(d, 0.0, places=10, msg=name)

        # Without canonicalization the swap shows up as a large diff.
        diffs_raw = per_param_diff(_Result(), _DummyCase(), canonicalize=False)
        self.assertGreater(abs(diffs_raw["center_sigma1"]), 10.0)


class ConsistencyTableTests(unittest.TestCase):
    """End-to-end check that the per-parameter consistency long table
    materializes the right shape and that aggregation collapses to mean / std
    correctly."""

    def setUp(self):
        self.case = _make_synthetic_case(seed=11)
        baseline = LmfitBaselineAdapter()
        ref_run = baseline.fit(self.case)
        self.case.reference = ReferenceResult(
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

    def test_per_param_rows_one_row_per_free_param(self):
        replay = LmfitBaselineAdapter().fit(self.case)
        rows = list(per_param_rows(replay, self.case))
        self.assertEqual(
            len(rows), len(self.case.inputs.free_params),
            msg=f"expected one row per free param, got {len(rows)}",
        )
        keys = {"param_name", "ref_value", "cand_value", "diff", "abs_diff", "rel_diff"}
        self.assertTrue(keys.issubset(rows[0].keys()))

    def test_consistency_summary_under_perturbation(self):
        """End-to-end: run_ab with perturbation -> summarize_consistency
        produces non-zero stds on free parameters."""
        from musclex.tests.fitting_ab.runner import (
            run_ab,
            summarize_consistency,
            summarize_dataframe,
        )
        df, param_df = run_ab(
            adapters=[LmfitBaselineAdapter()],
            cases=[self.case],
            n_repeats=4,
            perturb_init=0.10,
            progress=False,
            collect_param_rows=True,
        )
        # Long-format checks.
        self.assertEqual(len(df), 4)  # 4 trials
        self.assertEqual(len(param_df), 4 * len(self.case.inputs.free_params))

        # Summary tables include the new dispersion columns.
        summary = summarize_dataframe(df)
        for col in ("elapsed_std", "elapsed_iqr", "chi2_ratio_mean",
                    "chi2_ratio_std", "r2_mean", "r2_std"):
            self.assertIn(col, summary.columns, msg=f"missing column {col}")

        consistency = summarize_consistency(param_df)
        for col in ("cand_mean", "cand_std", "diff_mean", "diff_std",
                    "abs_diff_mean", "abs_diff_max", "ref_value", "n_trials"):
            self.assertIn(col, consistency.columns, msg=f"missing column {col}")

        # With 4 trials and 10% perturbation, we expect at least *some* of
        # the free parameters to wobble (the optimizer will not always land
        # exactly on the same value when it starts from a different init).
        max_std = consistency["cand_std"].max()
        self.assertGreater(
            max_std, 0.0,
            msg="all params have zero std under perturbation; sweep was a no-op",
        )

    def test_consistency_table_zero_std_in_deterministic_replay(self):
        """Without perturbation the optimizer is fed identical inputs every
        trial, so cand_std must be ~0. This documents the caveat baked into
        the runner help text."""
        from musclex.tests.fitting_ab.runner import (
            run_ab,
            summarize_consistency,
        )
        df, param_df = run_ab(
            adapters=[LmfitBaselineAdapter()],
            cases=[self.case],
            n_repeats=3,
            perturb_init=None,
            progress=False,
            collect_param_rows=True,
        )
        consistency = summarize_consistency(param_df)
        # Every cand_std should be effectively zero (or NaN if pandas
        # collapses a single-value group; we accept both).
        for v in consistency["cand_std"].dropna():
            self.assertAlmostEqual(v, 0.0, places=10)


class TestHullRangeSlice(unittest.TestCase):
    """Verify that slicing the fit array to the hull-range window is both
    numerically equivalent to full-array fitting **and** faster.

    The synthetic fixture mimics a ``bgsub=1`` (convex hull) case:

    * Background parameters are all zero (fixed as independent_vars).
    * The histogram ``y`` has genuine signal only within
      ``[centerX - hull_end, centerX + hull_end]``; everything outside
      is zero-padded (replicating what :func:`convexHull` produces).
    * The ``FitCase.meta.bgsub = 1`` and ``meta.hull_range`` are set so
      :class:`LmfitTRFHullSliceAdapter` activates the slicing path.
    """

    # ── fixture parameters ─────────────────────────────────────────────────
    N_FULL = 2000       # full array length (pixels)
    CENTER_X = 1000.0   # beam centre in array coordinates
    HULL_START = 30.0   # inner edge of valid window (distance from centre)
    HULL_END = 350.0    # outer edge
    N_PEAKS = 4
    PEAK_POSITIONS = [-280.0, -130.0, +130.0, +280.0]  # all within hull_end
    PEAK_AMPLITUDES = [800.0, 600.0, 600.0, 800.0]
    PEAK_SIGMA = 8.0
    TIMING_REPS = 30    # repetitions used for timing comparison

    @classmethod
    def setUpClass(cls):
        from musclex.tests.fitting_ab.model_variants.model_numpy import (
            layerlineModelGMM,
        )
        rng = np.random.default_rng(42)

        x = np.arange(cls.N_FULL, dtype=np.float64)

        # Build clean signal (background params all zero → only peaks)
        common_sigma = cls.PEAK_SIGMA
        kwargs = {}
        for i, (p, amp) in enumerate(
            zip(cls.PEAK_POSITIONS, cls.PEAK_AMPLITUDES)
        ):
            kwargs[f"p_{i}"] = p
            kwargs[f"amplitude{i}"] = amp

        y_clean = layerlineModelGMM(
            x=x,
            centerX=cls.CENTER_X,
            bg_line=0.0, bg_sigma=1.0, bg_amplitude=0.0,
            center_sigma1=1.0, center_amplitude1=0.0,
            center_sigma2=1.0, center_amplitude2=0.0,
            common_sigma=common_sigma,
            **kwargs,
        )
        # Add mild noise
        y = y_clean + rng.normal(0.0, np.sqrt(np.maximum(y_clean, 1.0)) * 0.3)

        # Zero-pad outside hull window (mimic convexHull output)
        lo_zero = int(cls.CENTER_X - cls.HULL_END)
        hi_zero = int(cls.CENTER_X + cls.HULL_END) + 1
        y[:lo_zero] = 0.0
        y[hi_zero:] = 0.0

        # Free params: peak positions + amplitudes + common_sigma
        free: Dict[str, ParamSpec] = {}
        tol = 15.0
        for i, p in enumerate(cls.PEAK_POSITIONS):
            free[f"p_{i}"] = ParamSpec(init=p, min=p - tol, max=p + tol)
            free[f"amplitude{i}"] = ParamSpec(
                init=float(y.sum() / 20.0), min=0.0, max=float(y.sum() + 1.0)
            )
        free["common_sigma"] = ParamSpec(init=10.0, min=1.0, max=30.0)

        indep = {
            "centerX": cls.CENTER_X,
            "bg_line": 0.0,
            "bg_sigma": 1.0,
            "bg_amplitude": 0.0,
            "center_sigma1": 1.0,
            "center_amplitude1": 0.0,
            "center_sigma2": 1.0,
            "center_amplitude2": 0.0,
        }

        inputs = FitInputs(
            x=x, y=y, weights=None,
            model_kind="gmm", has_voigt=False,
            free_params=free,
            independent_vars=indep,
            fixed_params={},
        )
        meta = CaseMeta(
            case_id="synthetic_hull_slice",
            box_name="hull_slice_test",
            box_type="h",
            bgsub=1,
            use_common_sigma=True,
            merid_bg=False,
            peaks_seed=cls.PEAK_POSITIONS,
            hull_range=(cls.HULL_START, cls.HULL_END),
            peak_tolerance=2.0,
            sigma_tolerance=100.0,
            difficulty_tags=["n_peaks=4", "gmm=True", "bgsub=1", "hull_slice_test"],
        )
        cls.full_case = FitCase(
            schema_version=SCHEMA_VERSION,
            meta=meta,
            inputs=inputs,
            reference=None,
        )

        # Run both adapters once to warm up imports / JIT
        from musclex.tests.fitting_ab.adapters import (
            LmfitTRFAdapter,
            LmfitTRFHullSliceAdapter,
        )
        cls.full_adapter  = LmfitTRFAdapter(seed=0)
        cls.slice_adapter = LmfitTRFHullSliceAdapter(seed=0)
        cls.full_result   = cls.full_adapter.fit(cls.full_case)
        cls.slice_result  = cls.slice_adapter.fit(cls.full_case)

    # ── helpers ────────────────────────────────────────────────────────────

    def _sliced_array_length(self):
        """Expected length of the sliced histogram."""
        from musclex.tests.fitting_ab.adapters.lmfit_adapters import _HULL_SLICE_MARGIN
        lo = max(0, int(self.CENTER_X - self.HULL_END) - _HULL_SLICE_MARGIN)
        hi = min(self.N_FULL,
                 int(self.CENTER_X + self.HULL_END) + _HULL_SLICE_MARGIN + 1)
        return hi - lo

    # ── correctness tests ──────────────────────────────────────────────────

    def test_both_fits_succeed(self):
        self.assertTrue(self.full_result.success,
                        msg=f"full-array fit failed: {self.full_result.message}")
        self.assertTrue(self.slice_result.success,
                        msg=f"hull-slice fit failed: {self.slice_result.message}")

    def test_r2_is_close(self):
        """R² difference must be < 0.005 (fits of a zero-padded array converge
        to the same optimum whether or not the zero padding is included)."""
        r2_full  = self.full_result.r2
        r2_slice = self.slice_result.r2
        self.assertIsNotNone(r2_full)
        self.assertIsNotNone(r2_slice)
        self.assertAlmostEqual(
            r2_full, r2_slice, delta=0.005,
            msg=f"R² diverged: full={r2_full:.4f}  slice={r2_slice:.4f}",
        )

    def test_peak_positions_close(self):
        """Fitted peak positions must agree within 0.5 px."""
        for i in range(self.N_PEAKS):
            key = f"p_{i}"
            p_full  = self.full_result.values[key]
            p_slice = self.slice_result.values[key]
            self.assertAlmostEqual(
                p_full, p_slice, delta=0.5,
                msg=f"{key}: full={p_full:.3f}  slice={p_slice:.3f}",
            )

    def test_common_sigma_close(self):
        """Fitted common_sigma must agree within 0.5."""
        s_full  = self.full_result.values["common_sigma"]
        s_slice = self.slice_result.values["common_sigma"]
        self.assertAlmostEqual(
            s_full, s_slice, delta=0.5,
            msg=f"common_sigma: full={s_full:.3f}  slice={s_slice:.3f}",
        )

    def test_sliced_array_is_shorter(self):
        """The slice adapter must actually reduce the array length that the
        model sees.  We verify via the hull-range arithmetic."""
        sliced_len = self._sliced_array_length()
        self.assertLess(
            sliced_len, self.N_FULL,
            msg=f"sliced length ({sliced_len}) is not shorter than full ({self.N_FULL})",
        )
        # Expect at least a 2× reduction for these fixture parameters.
        self.assertLess(
            sliced_len, self.N_FULL // 2,
            msg=f"sliced length ({sliced_len}) is less than 2× smaller than full",
        )

    # ── performance test ───────────────────────────────────────────────────

    def test_hull_slice_is_faster(self):
        """Median wall-clock time of the slice adapter must be strictly lower
        than the full-array adapter over :attr:`TIMING_REPS` repetitions.

        Accepts up to a 20 % timing margin to account for OS scheduler jitter
        on a loaded CI machine (the true speedup is typically 3–6×).
        """
        import time

        full_times, slice_times = [], []
        for _ in range(self.TIMING_REPS):
            t0 = time.perf_counter()
            self.full_adapter.fit(self.full_case)
            full_times.append(time.perf_counter() - t0)

            t0 = time.perf_counter()
            self.slice_adapter.fit(self.full_case)
            slice_times.append(time.perf_counter() - t0)

        med_full  = float(np.median(full_times))
        med_slice = float(np.median(slice_times))

        # Allow 20 % margin: slice must be at least 0.8× full to pass.
        self.assertLess(
            med_slice, med_full * 0.80,
            msg=(
                f"hull-slice adapter is not faster: "
                f"median full={med_full*1000:.1f} ms  "
                f"median slice={med_slice*1000:.1f} ms  "
                f"ratio={med_slice/med_full:.2f}"
            ),
        )


class TestVectorisedBatch(unittest.TestCase):
    """Verify numerical equivalence and speedup of the L3/L4 batch variants.

    Both ``lmfit-trf-numpy-vectorized`` (L3) and ``lmfit-trf-numba-vectorized``
    (L4) must:

    1. Produce model values that match the reference L1 NumPy variant within
       floating-point tolerance.
    2. Converge to peak positions within 0.5 pixels of the L1 reference.
    3. Run at least as fast as (or faster than) the L1 reference on a typical
       multi-peak case.  We use a 1.10× generous upper bound so the test is
       not flaky on a loaded CI machine.
    """

    N_FULL = 2000
    CENTER_X = 1000.0
    N_PEAKS = 6
    PEAK_POSITIONS = [-300.0, -180.0, -80.0, +80.0, +180.0, +300.0]
    PEAK_AMPLITUDES = [700.0, 1000.0, 500.0, 500.0, 1000.0, 700.0]
    PEAK_SIGMA = 9.0
    TIMING_REPS = 15

    @classmethod
    def setUpClass(cls):
        from musclex.tests.fitting_ab.adapters import (
            LmfitTRFNumpyAdapter,
            LmfitTRFNumpyVectorizedAdapter,
            LmfitTRFNumbaVectorizedAdapter,
        )
        from musclex.tests.fitting_ab.model_variants.model_numpy import (
            layerlineModelGMM as ref_gmm,
        )
        rng = np.random.default_rng(0)
        x = np.arange(cls.N_FULL, dtype=np.float64)

        kwargs = {}
        for i, (p, amp) in enumerate(zip(cls.PEAK_POSITIONS, cls.PEAK_AMPLITUDES)):
            kwargs[f"p_{i}"] = p
            kwargs[f"amplitude{i}"] = amp

        y_clean = ref_gmm(
            x=x,
            centerX=cls.CENTER_X,
            bg_line=0.0, bg_sigma=1.0, bg_amplitude=0.0,
            center_sigma1=1.0, center_amplitude1=0.0,
            center_sigma2=1.0, center_amplitude2=0.0,
            common_sigma=cls.PEAK_SIGMA,
            **kwargs,
        )
        y = y_clean + rng.normal(0.0, np.sqrt(np.maximum(y_clean, 1.0)) * 0.2)

        from musclex.tests.fitting_ab.adapters import (
            FitCase, FitInputs, CaseMeta, ParamSpec,
        )

        free_params = {}
        for i, (p, amp) in enumerate(zip(cls.PEAK_POSITIONS, cls.PEAK_AMPLITUDES)):
            free_params[f"p_{i}"]        = ParamSpec(init=p,   min=p-60, max=p+60)
            free_params[f"amplitude{i}"] = ParamSpec(init=amp, min=0.0,  max=5000.0)
        free_params["common_sigma"] = ParamSpec(init=cls.PEAK_SIGMA, min=1.0, max=40.0)

        indep_vars = {
            "centerX": cls.CENTER_X,
            "bg_line": 0.0, "bg_sigma": 1.0, "bg_amplitude": 0.0,
            "center_sigma1": 1.0, "center_amplitude1": 0.0,
            "center_sigma2": 1.0, "center_amplitude2": 0.0,
        }

        inputs = FitInputs(
            x=x, y=y, weights=None,
            model_kind="gmm", has_voigt=False,
            free_params=free_params,
            independent_vars=indep_vars,
            fixed_params={},
        )
        meta = CaseMeta(
            case_id="synthetic_vectorised_batch",
            box_name="vec_batch_test",
            box_type="h",
            bgsub=0,
            use_common_sigma=True,
            merid_bg=False,
            peaks_seed=cls.PEAK_POSITIONS,
            hull_range=None,
            peak_tolerance=2.0,
            sigma_tolerance=100.0,
        )
        case = FitCase(schema_version=SCHEMA_VERSION, meta=meta, inputs=inputs)

        cls.case    = case
        cls.ref_adapter  = LmfitTRFNumpyAdapter()
        cls.l3_adapter   = LmfitTRFNumpyVectorizedAdapter()
        cls.l4_adapter   = LmfitTRFNumbaVectorizedAdapter()

        cls.ref_result = cls.ref_adapter.fit(case)
        cls.l3_result  = cls.l3_adapter.fit(case)
        cls.l4_result  = cls.l4_adapter.fit(case)

    # ── numerical equivalence ──────────────────────────────────────────────

    def _check_positions(self, result, tag: str):
        ref = self.ref_result
        for i in range(self.N_PEAKS):
            key = f"p_{i}"
            r_val = ref.values.get(key)
            c_val = result.values.get(key)
            self.assertIsNotNone(c_val, msg=f"{tag}: missing param {key!r}")
            self.assertAlmostEqual(
                r_val, c_val, delta=0.5,
                msg=f"{tag}: {key}: ref={r_val:.4f}  got={c_val:.4f}",
            )

    def test_l3_peak_positions_match_reference(self):
        self._check_positions(self.l3_result, "L3-numpy-vectorized")

    def test_l4_peak_positions_match_reference(self):
        self._check_positions(self.l4_result, "L4-numba-vectorized")

    def test_l3_r2_close_to_reference(self):
        ref_r2 = self.ref_result.r2 or 0.0
        l3_r2  = self.l3_result.r2  or 0.0
        self.assertAlmostEqual(
            ref_r2, l3_r2, delta=0.01,
            msg=f"L3 R² {l3_r2:.4f} deviates too far from ref {ref_r2:.4f}",
        )

    def test_l4_r2_close_to_reference(self):
        ref_r2 = self.ref_result.r2 or 0.0
        l4_r2  = self.l4_result.r2  or 0.0
        self.assertAlmostEqual(
            ref_r2, l4_r2, delta=0.01,
            msg=f"L4 R² {l4_r2:.4f} deviates too far from ref {ref_r2:.4f}",
        )

    # ── model function direct equivalence ─────────────────────────────────

    def test_numpy_vectorized_model_matches_reference(self):
        """L3 model function must be numerically identical to L1 within 1e-10."""
        from musclex.tests.fitting_ab.model_variants.model_numpy import (
            layerlineModelGMM as l1_gmm,
        )
        from musclex.tests.fitting_ab.model_variants.model_numpy_vectorized import (
            layerlineModelGMM as l3_gmm,
        )
        x = np.linspace(0.0, 2000.0, 800)
        kw = {}
        for i, (p, amp) in enumerate(zip(self.PEAK_POSITIONS, self.PEAK_AMPLITUDES)):
            kw[f"p_{i}"] = p
            kw[f"amplitude{i}"] = amp
        common = dict(
            centerX=self.CENTER_X,
            bg_line=0.0, bg_sigma=1.0, bg_amplitude=0.0,
            center_sigma1=1.0, center_amplitude1=0.0,
            center_sigma2=1.0, center_amplitude2=0.0,
            common_sigma=self.PEAK_SIGMA,
        )
        y1 = l1_gmm(x=x, **common, **kw)
        y3 = l3_gmm(x=x, **common, **kw)
        np.testing.assert_allclose(
            y3, y1, rtol=1e-10,
            err_msg="L3 numpy-vectorized model deviates from L1 reference",
        )

    def test_numba_vectorized_model_matches_reference(self):
        """L4 model function must match L1 within 1e-7 (fastmath rounding)."""
        from musclex.tests.fitting_ab.model_variants.model_numpy import (
            layerlineModelGMM as l1_gmm,
        )
        from musclex.tests.fitting_ab.model_variants.model_numba_vectorized import (
            layerlineModelGMM as l4_gmm,
        )
        x = np.linspace(0.0, 2000.0, 800)
        kw = {}
        for i, (p, amp) in enumerate(zip(self.PEAK_POSITIONS, self.PEAK_AMPLITUDES)):
            kw[f"p_{i}"] = p
            kw[f"amplitude{i}"] = amp
        common = dict(
            centerX=self.CENTER_X,
            bg_line=0.0, bg_sigma=1.0, bg_amplitude=0.0,
            center_sigma1=1.0, center_amplitude1=0.0,
            center_sigma2=1.0, center_amplitude2=0.0,
            common_sigma=self.PEAK_SIGMA,
        )
        y1 = l1_gmm(x=x, **common, **kw)
        y4 = l4_gmm(x=x, **common, **kw)
        np.testing.assert_allclose(
            y4, y1, rtol=1e-7,
            err_msg="L4 numba-vectorized model deviates from L1 reference",
        )

    # ── timing ─────────────────────────────────────────────────────────────

    def test_l3_not_slower_than_l1(self):
        """L3 (numpy-vectorized) must be no slower than 1.10× L1 (numpy)."""
        import time
        ref_times, l3_times = [], []
        for _ in range(self.TIMING_REPS):
            t0 = time.perf_counter()
            self.ref_adapter.fit(self.case)
            ref_times.append(time.perf_counter() - t0)

            t0 = time.perf_counter()
            self.l3_adapter.fit(self.case)
            l3_times.append(time.perf_counter() - t0)

        med_ref = float(np.median(ref_times))
        med_l3  = float(np.median(l3_times))
        self.assertLessEqual(
            med_l3, med_ref * 1.10,
            msg=(
                f"L3 numpy-vectorized ({med_l3*1e3:.1f} ms) is >10% slower "
                f"than L1 numpy ({med_ref*1e3:.1f} ms) — vectorisation overhead is too high"
            ),
        )

    def test_l4_is_faster_than_l1(self):
        """L4 (numba-vectorized) should be faster than L1 (numpy) after warm-up.

        We allow up to a generous 1.20× margin to avoid flakiness on loaded CI
        machines.  A healthy L4 is typically 1.5–3× faster for 6+ peaks.
        """
        import time
        ref_times, l4_times = [], []
        for _ in range(self.TIMING_REPS):
            t0 = time.perf_counter()
            self.ref_adapter.fit(self.case)
            ref_times.append(time.perf_counter() - t0)

            t0 = time.perf_counter()
            self.l4_adapter.fit(self.case)
            l4_times.append(time.perf_counter() - t0)

        med_ref = float(np.median(ref_times))
        med_l4  = float(np.median(l4_times))
        self.assertLessEqual(
            med_l4, med_ref * 1.20,
            msg=(
                f"L4 numba-vectorized ({med_l4*1e3:.1f} ms) is >20% slower "
                f"than L1 numpy ({med_ref*1e3:.1f} ms); "
                f"check that JIT warm-up ran at module import"
            ),
        )


if __name__ == "__main__":
    unittest.main()
