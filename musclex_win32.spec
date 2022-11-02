# -*- mode: python -*-

block_cipher = None

# windows cmd:
# pyinstaller --clean -y musclex_win32.spec 2>&1 | findstr "..*" | findstr /v "api-ms-win"

a = Analysis(['musclex\\main.py'],
             pathex=['.'],
             binaries=[],
             datas=[('musclex\\tests\\testImages', 'testImages'),('musclex\\tests\\testResults', 'testResults'),
             ('musclex\\tests\\test_images', 'test_images'),('musclex\\tests\\test_logs', 'test_logs')],
             hiddenimports=['PyMca5'],
             hookspath=['hooks'],
             runtime_hooks=[],
             excludes=['tcl', 'zmq', 'IPython'],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher)

# analysis for launcher
la = Analysis(['musclex\\launcher.py'],
               pathex=['.'],
               binaries=[],
               datas=[],
               hiddenimports=['PyMca5'],
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
