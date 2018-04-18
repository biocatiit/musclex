# -*- mode: python -*-

block_cipher = None

# windows cmd:
# pyinstaller --clean -y musclex_win32.spec 2>&1 | findstr "..*" | findstr /v "api-ms-win"

a = Analysis(['musclex\\main.py'],
             pathex=['.'],
             binaries=[],
             datas=[],
             hiddenimports=['PyMca5'],
             hookspath=['hooks'],
             runtime_hooks=[],
             excludes=['tcl', 'zmq', 'IPython', 'PIL'],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher)
pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)
exe = EXE(pyz,
          a.scripts,
          exclude_binaries=True,
          name='musclex',
          debug=False,
          strip=False,
          upx=True,
          console=True )
coll = COLLECT(exe,
               a.binaries,
               a.zipfiles,
               a.datas,
               strip=False,
               upx=True,
               name='musclex')
