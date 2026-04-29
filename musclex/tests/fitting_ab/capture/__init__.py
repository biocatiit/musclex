"""Capture utilities: dump in-flight ``model.fit`` invocations to disk."""

from .recorder import (
    DEFAULT_CAPTURE_DIR,
    is_capture_enabled,
    maybe_record_fit,
)

__all__ = [
    "DEFAULT_CAPTURE_DIR",
    "is_capture_enabled",
    "maybe_record_fit",
]
