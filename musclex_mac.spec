# -*- mode: python -*-

block_cipher = None

# pyinstaller --clean -y musclex_mac.spec
# cp /usr/local/Cellar/opencv/3.4.1_4/lib/python3.6/site-packages/cv2.cpython-36m-darwin.so dist/musclex/cv2.so
# mkdir dist/musclex/pyFAI/utils


a = Analysis(['musclex/main.py'],
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

# analysis for launcher
la = Analysis(['musclex/launcher.py'],
               pathex=['.'],
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
          name='musclex',
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