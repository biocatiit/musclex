"""
Shared model-function backbone used by all variants.

Each variant module calls ``build_model_functions(gaussian_eval, voigt_eval)``
which returns a ``(layerlineModel, layerlineModelGMM)`` pair with the
provided leaf implementations closed over.  This avoids duplicating the
loop / dispatch logic while keeping each variant fully self-contained.
"""
from __future__ import annotations

from typing import Callable


def build_model_functions(
    gaussian_eval: Callable,
    voigt_eval: Callable,
):
    """Return ``(layerlineModel, layerlineModelGMM)`` using the supplied leaf impls."""

    def _layerlineBackground(x, centerX, bg_line, bg_sigma, bg_amplitude, **kwargs):
        return gaussian_eval(x=x, amplitude=bg_amplitude, center=centerX, sigma=bg_sigma) + bg_line

    def _meridianBackground(x, centerX, center_sigma1, center_amplitude1, **kwargs):
        return gaussian_eval(x=x, amplitude=center_amplitude1, center=centerX, sigma=center_sigma1)

    def _meridianGauss(x, centerX, center_sigma2, center_amplitude2, **kwargs):
        return gaussian_eval(x=x, amplitude=center_amplitude2, center=centerX, sigma=center_sigma2)

    def _layerlineModelBackground(
        x, centerX, bg_line, bg_sigma, bg_amplitude,
        center_sigma1, center_amplitude1,
        center_sigma2, center_amplitude2, **kwargs,
    ):
        return (
            _layerlineBackground(x, centerX, bg_line, bg_sigma, bg_amplitude)
            + _meridianBackground(x, centerX, center_sigma1, center_amplitude1)
            + _meridianGauss(x, centerX, center_sigma2, center_amplitude2)
        )

    def layerlineModel(
        x, centerX, bg_line, bg_sigma, bg_amplitude,
        center_sigma1, center_amplitude1,
        center_sigma2, center_amplitude2, **kwargs,
    ):
        result = _layerlineModelBackground(
            x, centerX, bg_line, bg_sigma, bg_amplitude,
            center_sigma1, center_amplitude1,
            center_sigma2, center_amplitude2,
        )
        i = 0
        while "p_" + str(i) in kwargs:
            p = kwargs["p_" + str(i)]
            sigma = kwargs["sigma" + str(i)]
            amplitude = kwargs["amplitude" + str(i)]
            if "gamma" + str(i) in kwargs:
                gamma = kwargs["gamma" + str(i)]
                result += voigt_eval(
                    x=x, amplitude=amplitude, center=centerX + p,
                    sigma=sigma, gamma=gamma,
                )
            else:
                result += gaussian_eval(
                    x=x, amplitude=amplitude, center=centerX + p, sigma=sigma,
                )
            i += 1
        return result

    def layerlineModelGMM(
        x, centerX, bg_line, bg_sigma, bg_amplitude,
        center_sigma1, center_amplitude1,
        center_sigma2, center_amplitude2,
        common_sigma, **kwargs,
    ):
        result = _layerlineModelBackground(
            x, centerX, bg_line, bg_sigma, bg_amplitude,
            center_sigma1, center_amplitude1,
            center_sigma2, center_amplitude2,
        )
        i = 0
        while "p_" + str(i) in kwargs:
            p = kwargs["p_" + str(i)]
            amplitude = kwargs["amplitude" + str(i)]
            if "gamma" + str(i) in kwargs:
                gamma = kwargs["gamma" + str(i)]
                result += voigt_eval(
                    x=x, amplitude=amplitude, center=centerX + p,
                    sigma=common_sigma, gamma=gamma,
                )
            else:
                result += gaussian_eval(
                    x=x, amplitude=amplitude, center=centerX + p, sigma=common_sigma,
                )
            i += 1
        return result

    return layerlineModel, layerlineModelGMM
