"""
PyInstaller hook for silx — full collect.

Why "collect everything":

    silx is a large GUI/scientific library that loads a wide variety of
    resources at runtime (icons in .png/.svg/.mng, OpenCL kernels in
    .cl, precomputed lookup tables in .npy, etc.) through indirection
    layers like ``importlib.resources`` and ``pkg_resources``. The
    PyInstaller static analyzer cannot see those resource references,
    so writing a hand-curated whitelist of extensions becomes a game of
    whack-a-mole: each unused feature breaks the .deb the first time
    someone exercises it (the ``zoom-original`` icon failure that
    prompted this rewrite is a textbook example).

    ``collect_all('silx')`` instructs PyInstaller to pull in:
        * every submodule of ``silx``
        * every non-Python data file ``silx`` ships
        * every compiled extension (.so/.pyd) ``silx`` ships
        * the package metadata (so ``importlib.metadata`` works)

    The frozen bundle gets a bit larger, but in exchange we are immune
    to "missing resource" regressions whenever silx adds a new icon,
    data table, or submodule in a future release. For MuscleX this
    trade-off is clearly correct: silx is reached transitively through
    pyFAI-drawmask (a child process spawned from
    ``musclex/utils/drawmask_launcher.py``) and the static analyzer has
    no chance of tracing the full surface area used by that GUI.

The earlier version of this hook collected only ``*.cl`` files, which
is why drawmask worked in venv installs but crashed in the .deb with
``ValueError: Not an icon name: zoom-original``.
"""

from PyInstaller.utils.hooks import collect_all

datas, binaries, hiddenimports = collect_all("silx")
