"""
PyInstaller hook for pyFAI — full collect.

Why "collect everything":

    pyFAI ships:
        * Qt .ui descriptors loaded at runtime via pkg_resources
          (``pyFAI/resources/gui/*.ui``)
        * Calibrant data tables (``.D``, ``.json``)
        * OpenCL kernels (``.cl``)
        * Icons (``.png``, ``.svg``)
        * Many compiled Cython extensions in ``pyFAI.ext``
        * Several GUI subpackages that pull in silx widgets

    These are reached indirectly — particularly through
    ``pyFAI.app.drawmask``, which MuscleX launches as a child process
    from ``musclex/utils/drawmask_launcher.py``. PyInstaller's static
    analyzer starts from ``musclex/main.py`` and cannot see anything
    behind that ``multiprocessing.spawn`` boundary, so without help it
    aggressively prunes pyFAI down to what ``main.py`` directly imports
    (which is essentially just the integrator). The drawmask GUI then
    fails at runtime with import errors and missing-resource errors.

    ``collect_all('pyFAI')`` instructs PyInstaller to pull in every
    submodule, every data file, every compiled extension, and the
    package metadata. The frozen bundle grows a little, but in return
    we stop playing whack-a-mole every time someone exercises a pyFAI
    code path that wasn't reachable from a top-level import.
"""

from PyInstaller.utils.hooks import collect_all

datas, binaries, hiddenimports = collect_all("pyFAI")

# pyFAI.ext.splitBBox_common / splitpixel_common are compiled extensions
# that some pyFAI versions load lazily via getattr/importlib. They are
# usually picked up by collect_all, but keep them explicit so a future
# version that hides them behind even more indirection still loads.
hiddenimports += [
    "pyFAI.ext.splitBBox_common",
    "pyFAI.ext.splitpixel_common",
]
