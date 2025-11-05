# -*- mode: python -*-
from PyInstaller.utils.hooks import collect_data_files

block_cipher = None

# Collect sklearn data files (CSS, etc.)
sklearn_datas = collect_data_files('sklearn')

a = Analysis(['musclex/main.py'],
             pathex=['.'],
             binaries=[],
             datas=[('musclex/tests/testImages', 'testImages'),('musclex/tests/testResults', 'testResults'),
             ('musclex/tests/test_images', 'test_images'),('musclex/tests/test_logs', 'test_logs')] + sklearn_datas,
             hiddenimports=['PyMca5', 'PySide6', 'PySide6.QtCore', 'PySide6.QtGui', 'PySide6.QtWidgets'],
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
          a.binaries,
          a.zipfiles,
          a.datas,
          name='musclex',
          debug=False,
          strip=False,
          upx=True,
          console=True )

# if you want the deb package to be able to launch from the Application finder (on click), you need to add in main.py, in the "if not run" after all the prints (line 400):
# from musclex.launcher import LauncherForm
# app = QApplication(sys.argv)
# myapp = LauncherForm.main()
# sys.exit(app.exec_())
