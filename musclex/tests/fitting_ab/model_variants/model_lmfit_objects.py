"""
Variant: lmfit-objects  (baseline / "before" state)

Creates a fresh ``lmfit.models.GaussianModel`` or ``VoigtModel`` Python
object on **every single call** to ``_gaussian_eval`` / ``_voigt_eval``.
This matches the original production code before the direct-NumPy rewrite
and provides the slowest reference point in the benchmark.
"""
from __future__ import annotations

from ._backbone import build_model_functions


def _gaussian_eval(x, amplitude, center, sigma):
    from lmfit.models import GaussianModel  # noqa: PLC0415
    return GaussianModel().eval(x=x, amplitude=amplitude, center=center, sigma=sigma)


def _voigt_eval(x, amplitude, center, sigma, gamma):
    from lmfit.models import VoigtModel  # noqa: PLC0415
    return VoigtModel().eval(x=x, amplitude=amplitude, center=center, sigma=sigma, gamma=gamma)


layerlineModel, layerlineModelGMM = build_model_functions(_gaussian_eval, _voigt_eval)
