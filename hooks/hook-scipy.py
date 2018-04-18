from PyInstaller.utils.hooks import collect_dynamic_libs

# The hidden import is necessary for SciPy 1.0.1+.
hiddenimports = ['scipy._lib.messagestream']
binaries = [(src, '') for src, des in collect_dynamic_libs('scipy')]