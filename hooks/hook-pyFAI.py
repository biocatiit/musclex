from PyInstaller.utils.hooks import collect_data_files, collect_submodules, logger

datas = collect_data_files('pyFAI.resources')

# Also bundle the pyFAI.resources subpackage code itself: drawmask.py loads its
# Qt .ui descriptors via pkg_resources/importlib at runtime, and a few of its
# helper modules (e.g. pyFAI.gui.utils, pyFAI.gui.matplotlib) are pulled in
# transitively. The ``app.drawmask`` entry point is invoked indirectly via a
# multiprocessing child in musclex/utils/drawmask_launcher.py, so PyInstaller's
# static analyzer cannot see it from musclex/main.py and we must declare it
# explicitly here.
hiddenimports = [
    'pyFAI.ext.splitBBox_common',
    'pyFAI.ext.splitpixel_common',
    'pyFAI.app',
    'pyFAI.app.drawmask',
]
hiddenimports += collect_submodules('pyFAI.gui')

