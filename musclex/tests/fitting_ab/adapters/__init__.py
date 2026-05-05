"""Fitter adapters for the A/B test framework."""

from .base import (
    SCHEMA_VERSION,
    CaseMeta,
    FitCase,
    FitInputs,
    FitResult,
    FitterAdapter,
    ParamSpec,
    ReferenceResult,
    load_case,
    save_case,
)
from .lmfit_adapters import (
    LmfitBaselineAdapter,
    LmfitPoissonWeightedAdapter,
    LmfitTRFAdapter,
    LmfitTRFCythonAdapter,
    LmfitTRFJacAdapter,
    LmfitTRFLmfitObjectsAdapter,
    LmfitTRFNumbaAdapter,
    LmfitTRFNumpyAdapter,
    LmfitTRFPoissonAdapter,
)

#: Registry of built-in adapters keyed by ``adapter.name``.
ADAPTER_REGISTRY = {
    a.name: a
    for a in (
        LmfitBaselineAdapter,
        LmfitTRFAdapter,
        LmfitTRFJacAdapter,
        LmfitPoissonWeightedAdapter,
        LmfitTRFPoissonAdapter,
        # Model-variant speedup comparison adapters (all use TRF optimizer)
        LmfitTRFLmfitObjectsAdapter,
        LmfitTRFNumpyAdapter,
        LmfitTRFNumbaAdapter,
        LmfitTRFCythonAdapter,
    )
}


def get_adapter(name: str, **kwargs) -> FitterAdapter:
    """Instantiate a registered adapter by ``name`` (e.g. ``'lmfit-baseline-leastsq'``)."""
    if name not in ADAPTER_REGISTRY:
        raise KeyError(
            f"Unknown adapter {name!r}. Available: {sorted(ADAPTER_REGISTRY)}"
        )
    return ADAPTER_REGISTRY[name](**kwargs)


__all__ = [
    "SCHEMA_VERSION",
    "ADAPTER_REGISTRY",
    "CaseMeta",
    "FitCase",
    "FitInputs",
    "FitResult",
    "FitterAdapter",
    "ParamSpec",
    "ReferenceResult",
    "LmfitBaselineAdapter",
    "LmfitTRFAdapter",
    "LmfitTRFJacAdapter",
    "LmfitPoissonWeightedAdapter",
    "LmfitTRFPoissonAdapter",
    "LmfitTRFLmfitObjectsAdapter",
    "LmfitTRFNumpyAdapter",
    "LmfitTRFNumbaAdapter",
    "LmfitTRFCythonAdapter",
    "get_adapter",
    "load_case",
    "save_case",
]
