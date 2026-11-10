# -*- mode: python -*-
from PyInstaller.utils.hooks import collect_data_files

block_cipher = None

# Collect sklearn data files (CSS, HTML templates, etc.)
sklearn_datas = collect_data_files('sklearn')

# pyinstaller --clean -y musclex_mac.spec
# The spec builds both the PyInstaller onedir output and a native macOS app bundle.

a = Analysis(['musclex/main.py'],
             pathex=['.'],
             binaries=[],
             datas=[
                 ('musclex/tests/testImages', 'testImages'),
                 ('musclex/tests/testResults', 'testResults'),
                 ('musclex/tests/test_images', 'test_images'),
                 ('musclex/tests/test_logs', 'test_logs')
             ] + sklearn_datas,  # Add sklearn data files
             hiddenimports=[
                 'PyMca5',
                 'cmath',
                 'PySide6',
                 'PySide6.QtCore',
                 'PySide6.QtGui',
                 'PySide6.QtWidgets'
             ],
             hookspath=['hooks'],
             runtime_hooks=[],
             excludes=['tcl', 'zmq', 'IPython'],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher)

pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)
exe = EXE(pyz,
          a.scripts,
          exclude_binaries=True,
          name='musclex-main',
          debug=False,
          strip=False,
          upx=True,
          console=False)

coll = COLLECT(exe,
               a.binaries,
               a.zipfiles,
               a.datas,
               strip=False,
               upx=True,
               name='musclex')

app = BUNDLE(coll,
             name='musclex.app',
             icon='dev_docs/linux/AppIcon.icns',
             bundle_identifier='org.biocatiit.musclex',
             version='0.0.0')
