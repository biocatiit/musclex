# Create Stand-alone Windows Program for MuscleX with PyInstaller
Environment: Python 3.6.4 [MSC v.1900 64 bit (AMD64)] on win32
## Contents
- [Basic steps](#basic-steps)
- [Debugging](#debugging)

## Basic steps
### Build a spec file
Build a [spec (specification) file][1]. (Work in the root directory
of musclex project.)
```
pyi-makespec -n musclex musclex\main.py
```
### Edit the spec file
```python
    ...
    pathex=['.'],
    hiddenimports=['PyMca5'],
    hookspath=['hooks'],
    excludes=['tcl', 'zmq', 'IPython', 'PIL'],
    ...
```
`hiddenimports` and **hooks** will be expained in later parts. 
`excludes` list lib packages not necessarily needed here.
### Build the application and run
Assume your spec file is named "musclex_win32.spec".
```
pyinstaller --clean -y musclex_win32.spec
```
Hopefully, this will not be interrupted by errors. But there must be
warnings, some of which really matter at runtime.  
Two folders are created: `build\musclex_win32` and `dist\musclex`.
`warnmusclex_win32.txt` in the former one shows some noticeable
warnings. And the latter folder stores the whole application.
```
.\dist\musclex\musclex
```

## Debugging
If musclex fails to run, this part might be helpful.  
PyInstaller provides a [useful doccument][2] for solving unpredictable
problems. This part only shows some examples involved in building
MuscleX.
### Hook
This is the most common method dealing with problems about third-party
libraries. (See [Understanding PyInstaller Hooks][3].) It helps
PyInstaller's analyzer find or import modules and files needed.
#### Common hooks
1. ModuleNotFoundError: No module named ...  
  This is the simplest case, probably involving some .pyd file. Just
  write a hook file `hook-full.import.name.py` and add `hiddenimports`.
  For example, `hook-scipy.py`:
```python
hiddenimports = ['scipy._lib.messagestream']
```

2. The error itself may not indicate that some module is missing, but
  it is caused by some missing module. For example, `hook-pandas.py`:
```python
hiddenimports = ['pandas._libs.tslibs.timedeltas']
```

3. ImportError: DLL load failed  
  Some .dll files are difficult to find by analyzer, for example:
  `scipy\extra-dll`. We can write a hook to import those files,
  `hook-scipy.py`:
```python
from PyInstaller.utils.hooks import collect_dynamic_libs

binaries = collect_dynamic_libs('scipy')
```

4. Additional data files are missing. For example, `hook-PyMca5.py`:
```python
from PyInstaller.utils.hooks import collect_data_files, logger

datas = collect_data_files('PyMca5.PyMcaData')
```

#### Special hooks
`__init__.py` of a module is free to extend its `__path__` to 
include other directories. (See [Extending a Package’s __path__][4].)
This will also cause **ModuleNotFoundError** if we do not inform the
analyzer.

The [*pre_safe_import_module( psim_api )*][5] method is used to fix
such problems. For example, some submodule of module `PyMca5.PyMcaGui`
can not be found, we should write `hook-PyMca5.PyMcaGui.py`:
```python
def pre_safe_import_module(psim_api):
    import PyMca5.PyMcaGui as PyMcaGui
    for p in PyMcaGui.__path__:
        psim_api.append_package_path(p)
```
Note that the code should be written exactly in `hook-PyMca5.PyMcaGui.py`,
modification in `hook-PyMca5.py` will not help.

### Other tips
1. [Dependency Walker][6]  
  This tool can help you find the exact DLLs depended by a module (.pyd
  file).
2. **api-ms-win-\*.dll**
  This kind of DLLs will probably occur in warnings of both Dependency
  Walker and PyInstaller, but really does not matter. You might prefer
  not to see those warnings when running PyInstaller, try
```
pyinstaller --clean -y musclex_win32.spec 2>&1 | findstr "..*" | findstr /v "api-ms-win"
```

[1]:https://pyinstaller.readthedocs.io/en/v3.3.1/spec-files.html
[2]:https://pyinstaller.readthedocs.io/en/v3.3.1/when-things-go-wrong.html
[3]:https://pyinstaller.readthedocs.io/en/v3.3.1/hooks.html
[4]:https://pyinstaller.readthedocs.io/en/v3.3.1/when-things-go-wrong.html#extending-a-package-s-path
[5]:https://pyinstaller.readthedocs.io/en/v3.3.1/hooks.html#the-pre-safe-import-module-psim-api-method
[6]:http://www.dependencywalker.com/
