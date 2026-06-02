"""
Cross-environment launcher for the pyFAI-drawmask GUI.

The drawmask tool can be reached via three different mechanisms depending
on how MuscleX is installed, and this helper picks whichever works:

1. ``pyFAI-drawmask`` console script on ``$PATH`` (typical venv / pip install).
2. ``python -m pyFAI.app.drawmask`` using the running interpreter
   (works for any non-frozen Python where pyFAI is importable but the
   console script isn't on ``$PATH``).
3. A ``multiprocessing`` child process that imports and runs
   ``pyFAI.app.drawmask.main`` directly. This is the only viable path
   inside the PyInstaller-frozen ``.deb`` build, where ``sys.executable``
   points at the frozen MuscleX binary itself and there is no
   standalone Python interpreter to invoke.

The function blocks until the drawmask window is closed and returns the
child process's exit code (0 on success).
"""

from __future__ import annotations

import multiprocessing
import os
import shutil
import subprocess
import sys
from typing import Union

PathLike = Union[str, "os.PathLike[str]"]


def _drawmask_child_entrypoint(input_path: str) -> None:
    """Run pyFAI's drawmask GUI inside a freshly-spawned child process.

    Importing pyFAI here (not at module top) keeps the parent process
    light and avoids creating a second QApplication in the same process
    as the host MuscleX UI, which would race with Qt's main thread.
    """
    sys.argv = ["pyFAI-drawmask", str(input_path)]
    from pyFAI.app.drawmask import main as drawmask_main

    drawmask_main()


def run_pyfai_drawmask(input_path: PathLike) -> int:
    """Launch the pyFAI-drawmask GUI on ``input_path`` and wait for it.

    Returns the exit code of the underlying invocation (0 on success).
    """
    input_path = str(input_path)

    # 1. Prefer the console script when it is on PATH (venv/pip installs).
    drawmask_exe = shutil.which("pyFAI-drawmask")
    if drawmask_exe:
        return subprocess.call([drawmask_exe, input_path])

    # 2. Non-frozen interpreters can run the module directly even without
    #    the console script on PATH.
    if not getattr(sys, "frozen", False):
        return subprocess.call([sys.executable, "-m", "pyFAI.app.drawmask", input_path])

    # 3. PyInstaller-frozen build (e.g. the .deb): there is no separate
    #    Python interpreter and no pyFAI-drawmask script on PATH. Run the
    #    drawmask main in a multiprocessing child so its eventual
    #    sys.exit() / QApplication only affects the child, not MuscleX.
    ctx = multiprocessing.get_context("spawn")
    proc = ctx.Process(target=_drawmask_child_entrypoint, args=(input_path,))
    proc.start()
    proc.join()
    return proc.exitcode if proc.exitcode is not None else 1
