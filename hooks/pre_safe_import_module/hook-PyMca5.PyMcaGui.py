from PyInstaller.utils.hooks import logger

def pre_safe_import_module(psim_api):
    import PyMca5.PyMcaGui as PyMcaGui
    for p in PyMcaGui.__path__:
        psim_api.append_package_path(p)
