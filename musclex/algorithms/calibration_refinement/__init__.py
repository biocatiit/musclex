"""Calibration refinement helpers for MuscleX GUI integration.

Use :mod:`musclex.algorithms.calibration_refinement.adapter` for the callable
GUI-facing API. The ``refine_center`` name is reserved for the center
refinement package directory.
"""

from . import adapter

__all__ = ["adapter"]
