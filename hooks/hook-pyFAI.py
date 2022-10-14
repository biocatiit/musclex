from PyInstaller.utils.hooks import collect_data_files, logger

datas = collect_data_files('pyFAI.resources')

hiddenimports = ['pyFAI.ext.splitBBox_common','pyFAI.ext.splitpixel_common']

