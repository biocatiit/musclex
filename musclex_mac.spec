# -*- mode: python -*-
from PyInstaller.utils.hooks import collect_data_files

block_cipher = None

# Collect sklearn data files (CSS, HTML templates, etc.)
sklearn_datas = collect_data_files('sklearn')

# pyinstaller --clean -y musclex_mac.spec
# Post-build steps may be needed:
# - Fix OpenCV library: cp /usr/local/Cellar/libpng/1.6.34/lib/libpng16.16.dylib dist/musclex/libpng16.16.dylib
# - Create pyFAI resources: mkdir dist/musclex/pyFAI/utils
# - Copy to app bundle: cp -r dist/musclex/ dist/musclex.app/Contents/MacOS

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

# analysis for launcher
la = Analysis(['musclex/launcher.py'],
               pathex=['.'],
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

MERGE((a, 'main', 'musclex'),
      (la, 'launcher', 'musclex-launcher'))

pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)
exe = EXE(pyz,
          a.scripts,
          exclude_binaries=True,
          name='musclex-main',
          debug=False,
          strip=False,
          upx=True,
          console=True )

lpyz = PYZ(la.pure, la.zipped_data,
             cipher=block_cipher)
lexe = EXE(lpyz,
           la.scripts,
           exclude_binaries=True,
           name='musclex-launcher',
           debug=False,
           strip=False,
           upx=True,
           console=False )

coll = COLLECT(exe, lexe,
               a.binaries, la.binaries,
               a.zipfiles, la.zipfiles,
               a.datas, la.datas,
               strip=False,
               upx=True,
               name='musclex')
