"""
Core data classes and ``FitterAdapter`` protocol for the A/B framework.

The schema is intentionally **library-agnostic**: a ``FitCase`` is a pure
Python + NumPy snapshot of one fit invocation, plus a frozen reference
result. Any adapter (lmfit/scipy/VarPro/iminuit/...) can replay it without
touching the original ``ProjectionProcessor`` code.
"""

from __future__ import annotations

import pickle
from abc import ABC, abstractmethod
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple, Union

import numpy as np


SCHEMA_VERSION = 1


# --------------------------------------------------------------------------- #
# Parameter spec
# --------------------------------------------------------------------------- #


@dataclass
class ParamSpec:
    """Initial value + (optional) bounds for one free parameter.

    ``min``/``max`` use ``None`` to mean "unbounded" so we never persist
    ``-inf``/``+inf`` floats that pickle inconsistently across NumPy versions.
    """

    init: float
    min: Optional[float] = None
    max: Optional[float] = None

    def to_lmfit_min(self) -> float:
        return -np.inf if self.min is None else float(self.min)

    def to_lmfit_max(self) -> float:
        return np.inf if self.max is None else float(self.max)


# --------------------------------------------------------------------------- #
# Inputs
# --------------------------------------------------------------------------- #


@dataclass
class FitInputs:
    """Everything an adapter needs to (re)build the model and the fit.

    Notes on splitting fixed values into two buckets:

    * ``independent_vars`` mirrors lmfit's ``independent_vars`` argument:
      entries are passed straight to the model function as kwargs.
      In MuscleX these are ``x``, ``centerX``, ``bg_line``, plus the
      background block when ``bgsub == 1`` (Convex hull mode).
    * ``fixed_params`` are user-locked or feature-disabled parameters
      that lmfit treats as ``vary=False`` Parameters.

    For non-lmfit adapters the distinction is moot, both are constants.
    """

    x: np.ndarray
    y: np.ndarray
    weights: Optional[np.ndarray]

    # 'standard' -> layerlineModel ; 'gmm' -> layerlineModelGMM
    model_kind: str
    has_voigt: bool

    free_params: Dict[str, ParamSpec]
    independent_vars: Dict[str, Any]  # kwargs passed straight to model func
    fixed_params: Dict[str, float]    # vary=False Parameters in lmfit


# --------------------------------------------------------------------------- #
# Reference output (frozen baseline)
# --------------------------------------------------------------------------- #


@dataclass
class ReferenceResult:
    """Snapshot of the baseline lmfit run, used as the diff target."""

    adapter: str
    success: bool
    values: Dict[str, float]                    # full result.values
    stderr: Dict[str, Optional[float]]          # per-free-param stderr (may be None)
    chi2: Optional[float]
    redchi: Optional[float]
    r2: Optional[float]
    n_eval: Optional[int]
    elapsed_s: Optional[float]
    message: str = ""


# --------------------------------------------------------------------------- #
# Per-case metadata (traceability)
# --------------------------------------------------------------------------- #


@dataclass
class CaseMeta:
    case_id: str
    box_name: str
    box_type: str            # 'h' | 'v' | 'oriented'
    bgsub: int               # 0 | 1 | 2
    use_common_sigma: bool
    merid_bg: bool
    peaks_seed: List[float]
    hull_range: Optional[Tuple[float, float]]
    peak_tolerance: float
    sigma_tolerance: float

    source_image: Optional[str] = None
    frame_index: Optional[int] = None
    invocation: Dict[str, str] = field(default_factory=dict)
    difficulty_tags: List[str] = field(default_factory=list)


# --------------------------------------------------------------------------- #
# The complete case
# --------------------------------------------------------------------------- #


@dataclass
class FitCase:
    """A self-contained, replayable fit fixture."""

    schema_version: int
    meta: CaseMeta
    inputs: FitInputs
    reference: Optional[ReferenceResult] = None
    optional: Dict[str, Any] = field(default_factory=dict)

    # ------------------------------------------------------------------ #
    # Convenience
    # ------------------------------------------------------------------ #
    def free_param_names(self) -> List[str]:
        return list(self.inputs.free_params.keys())

    def n_free(self) -> int:
        return len(self.inputs.free_params)


# --------------------------------------------------------------------------- #
# Adapter result
# --------------------------------------------------------------------------- #


@dataclass
class FitResult:
    """Uniform container for what every adapter returns.

    Notes
    -----
    * ``chi2`` is the **unweighted** sum of squared residuals
      ``sum((y - predicted) ** 2)``, recomputed from the final parameter
      values, so it is comparable across adapters even when one of them
      uses internal weights (e.g. Poisson). Backend-reported chi-square
      may diverge from this if the fit was aborted; we always trust the
      manual recompute.
    * ``aborted`` is ``True`` when the optimizer hit its function-evaluation
      or iteration cap rather than reaching a convergence criterion. Useful
      to distinguish "failed to converge" from "converged to a worse
      optimum" in reports.
    """

    success: bool
    values: Dict[str, float]            # all known params (free + fixed)
    stderr: Dict[str, Optional[float]]  # may be empty if backend doesn't provide
    n_eval: Optional[int]
    n_iter: Optional[int]
    elapsed_s: float
    converged: bool
    chi2: Optional[float]
    redchi: Optional[float]
    r2: Optional[float]
    message: str = ""
    aborted: bool = False
    raw: Any = None  # backend-specific result, kept for debugging only


# --------------------------------------------------------------------------- #
# Adapter interface
# --------------------------------------------------------------------------- #


class FitterAdapter(ABC):
    """Strategy interface every fitting backend must implement."""

    #: Short human-readable label, used in reports.
    name: str = "abstract"

    @abstractmethod
    def fit(self, case: FitCase, *, perturb_init: Optional[float] = None) -> FitResult:
        """Run a single fit on ``case`` and return a uniform :class:`FitResult`.

        Parameters
        ----------
        case
            The replayable fixture.
        perturb_init
            If not ``None``, multiply each free param's initial value by
            a per-call random factor in ``[1 - perturb_init, 1 + perturb_init]``.
            Used for robustness experiments.
        """


# --------------------------------------------------------------------------- #
# Pickle I/O
# --------------------------------------------------------------------------- #


def save_case(case: FitCase, path: Union[str, Path]) -> None:
    """Persist a :class:`FitCase` to disk as a pickle file."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "wb") as f:
        pickle.dump(case, f, protocol=pickle.HIGHEST_PROTOCOL)


def load_case(path: Union[str, Path]) -> FitCase:
    """Load a previously saved :class:`FitCase`."""
    with open(path, "rb") as f:
        case = pickle.load(f)
    if not isinstance(case, FitCase):
        raise TypeError(f"Pickled object at {path!s} is not a FitCase")
    if case.schema_version != SCHEMA_VERSION:
        # Forward compat: warn but accept; downstream code should branch.
        import warnings

        warnings.warn(
            f"FitCase {path!s} has schema_version={case.schema_version}, "
            f"current SCHEMA_VERSION={SCHEMA_VERSION}",
            stacklevel=2,
        )
    return case
