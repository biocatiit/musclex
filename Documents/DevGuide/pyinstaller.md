# Create Stand-alone Program for MuscleX with PyInstaller
## Contents
- [Basic steps](#basic-steps)
- [Debugging](#debugging)
- [Building Mac OS X App Bundle](#building-mac-os-x-app-bundle)

## Basic steps
Environment: Python 3.6.4 [MSC v.1900 64 bit (AMD64)] on win32
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
include other directories. (See [Extending a Package’s \_\_path__][4].)
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

## Building Mac OS X App Bundle
Above parts describe the process in Windows. For building Mac App, baisc
steps and settings are almost the same as those for Windows,  but there
are a few more stuff needed to be done.  
Environment: Python 3.6.5 [GCC 4.2.1 Compatible Apple LLVM 9.0.0 (clang-900.0.39.2)] on darwin
### Additional Issues
1. OpenCV library  
  **Description**: Error occurs when importing *cv2* module. (Opencv is
  installed by "brew install".)  
  **Solution**: Recopy `cv2.so` to the generated folder.
```
cp /usr/local/Cellar/opencv/3.4.1_4/lib/python3.6/site-packages/cv2.*so dist/musclex/cv2.so
```

2. File not found at `pyFAI/utils/../resources`  
  **Description**: The directory `pyFAI/resources` is referred to as
  `pyFAI/utils/../resources`, but `pyFAI/utils` is not generated.  
  **Solution**: Make the directory `pyFAI/utils`.
```
mkdir dist/musclex/pyFAI/utils
```

### Building App Bundle
PyInstaller will do this only when both flag `-w` and `-F` are set. (See
[Building Mac OS X App Bundles][7].) However, we can build it mannually.
[musclex.app](../../dist/) is a template. (See [Anatomy of a macOS
Application Bundle][8].)
1. Move the executables to `Contents/MacOS`  
  Assume that after the previous process the stand-alone program is
  generated in `dist/musclex` which contains all executables, libraries
  and resouses needed at runtime, and `dist/musclex.app` is the target
  App Bundle.
```
cp -r dist/musclex/ dist/musclex.app/Contents/MacOS
```
2. Edit the file `Info.plist`  
  Specify the values of *CFBundleExecutable* and other attributes. (See
  [the example](../../dist/musclex.app/Contents/Info.plist).)


[1]:https://pyinstaller.readthedocs.io/en/v3.3.1/spec-files.html
[2]:https://pyinstaller.readthedocs.io/en/v3.3.1/when-things-go-wrong.html
[3]:https://pyinstaller.readthedocs.io/en/v3.3.1/hooks.html
[4]:https://pyinstaller.readthedocs.io/en/v3.3.1/when-things-go-wrong.html#extending-a-package-s-path
[5]:https://pyinstaller.readthedocs.io/en/v3.3.1/hooks.html#the-pre-safe-import-module-psim-api-method
[6]:http://www.dependencywalker.com/
[7]:https://pyinstaller.readthedocs.io/en/v3.3.1/usage.html#building-mac-os-x-app-bundles
[8]:https://developer.apple.com/library/content/documentation/CoreFoundation/Conceptual/CFBundles/BundleTypes/BundleTypes.html#//apple_ref/doc/uid/10000123i-CH101-SW19
