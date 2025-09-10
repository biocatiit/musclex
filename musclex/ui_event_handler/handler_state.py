from enum import Flag, auto


class UIEventHanlerState(Flag):
    INIT = auto()
    # Action is disabled
    DISABLED = auto()
    # Action can be executed
    READY = auto()
    # Action is currently being executed
    RUNNING = auto()
    # Action finished successfully
    COMPLETED = auto()
    # Action is paused
    PAUSED = auto()
    # Action failed
    FAILED = auto()
    # Action is cancelled
    CANCELLED = auto()
