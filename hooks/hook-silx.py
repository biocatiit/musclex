"""
PyInstaller hook for silx.

silx ships a lot of non-code resources that are loaded at runtime by
``importlib.resources`` / ``pkg_resources``:

* ``*.cl``   - OpenCL kernels (used for GPU compute paths).
* ``*.png`` / ``*.svg`` - GUI icons (toolbar buttons like ``zoom-original``,
  ``shape-circle``, etc.). pyFAI-drawmask's GUI is built on top of
  ``silx.gui.plot.PlotWindow``, which fails on startup with
  ``ValueError: Not an icon name: zoom-original`` if these are missing.
* ``*.ui``   - Qt Designer descriptors loaded dynamically.
* ``*.qss``  - Qt style sheets.
* ``*.json`` / ``*.npz`` - calibration / lookup tables used by several
  silx submodules.

Without this hook, MuscleX's PyInstaller-frozen builds (e.g. the .deb)
cannot launch the drawmask GUI. The original hook only collected ``*.cl``
which is why drawmask worked in venv installs but crashed in the .deb.

We also force a couple of silx subpackages to be included because
``pyFAI.app.drawmask`` reaches them indirectly and PyInstaller's static
analyzer would otherwise prune them.
"""

from PyInstaller.utils.hooks import collect_data_files, collect_submodules

datas = collect_data_files(
    'silx',
    includes=[
        '**/*.cl',
        '**/*.png',
        '**/*.svg',
        '**/*.ui',
        '**/*.qss',
        '**/*.json',
        '**/*.npz',
        '**/*.h5',
    ],
)

hiddenimports = collect_submodules('silx.gui')
hiddenimports += collect_submodules('silx.resources')
