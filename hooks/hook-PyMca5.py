from PyInstaller.utils.hooks import collect_data_files, logger

datas = collect_data_files('PyMca5.PyMcaData')
datas += [(src, datas[0][1]) for src, des in collect_data_files('fisx') if src.endswith('.dat')]