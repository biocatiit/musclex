from PyInstaller.utils.hooks import logger, get_module_file_attribute
import os

def pre_safe_import_module(psim_api):
    import PyMca5.PyMca as PyMca
    for p in PyMca.__path__:
        psim_api.append_package_path(p)
