# Install ccp13 on Windows

- [Requirements](#requirements)
- [Preparing built packages](#preparing-built-packages)
  - [Extra requirements](#extra-requirements)
  - [Build distribution of ccp13](#build-distribution-of-ccp13)
- [Installing musclex_ccp13](#installing-musclex_ccp13)

## Requirements
Required by both the package administrator and users.
- Python and pip
- [Microsoft Build Tools 2015 Update 3][1]

Basic functions from [Microsoft Build Tools 2015 Update 3][1] is enough for our
purpose. We do not need to install the whole Visual Studio.

## Preparing built packages
This part is only for package administrator.

### Extra requirements
Besides basic requirements, package administrator needs additional tools.
- [mingw-w64][2]
- [twine][3]
- wheel

1. [MinGW-W64][2] or any other available package containing Fortran compiler is
  required to build musclex_ccp13.
2. [twine][3] is used to upload distributions to PyPI repository.
3. *wheel* is used to build wheel distribution.

#### Install MinGW-W64
When installing [MinGW-W64][2], the installer will ask you to specify some setup
settings. You should set them according to your system environment, for example:
- Version: 7.2.0
- Architecture: x86_64
- Threads: win32
- Exception: seh
- Build revision: 1

According to above settings, the default `<mingw-w64_installation_directory>` would
probably be `C:\Program Files\mingw-w64\x86_64-7.2.0-win32-seh-rt_v5-rev1`. Also,
you can specify it yourself.  
After installation, we need to add its excutable directory to the environment variable
`%PATH%`. There are [several ways][4] to do so:
1. Setting "Properties" of "My Computer" (Recommended)

  Trace through `"My Computer" > "Properties" > "Advanced" > "Environment Variables"`.
  Edit variable "PATH" either in user variable table or in system variable table. Add
  `<mingw-w64_installation_directory>\mingw64\bin` and move it to the top.

2. Using "setx" command
```
setx PATH "<mingw-w64_installation_directory>\mingw64\bin;%PATH%"
```
  Note that viariable `%PATH%` contains paths in both current user environment and
  system environment. So, this is actually **not** a good way.

3. Using "set" or "path" command
```
set PATH=<mingw-w64_installation_directory>\mingw64\bin;%PATH%
```
  or
```
path <mingw-w64_installation_directory>\mingw64\bin;%PATH%
```
  This only works in current command line session, which means after closing current
  command line window this setting will not exist.

> **Note**: For the first two methods, we need to reopen the command line window to
  get the changes applied.

After adding it, we can run `echo %PATH%` or `path` to check.

#### Install twine
```
pip install --upgrade twine
```

#### Install wheel
```
pip install --upgrade wheel
```

### Build distribution of ccp13
1. Build ccp13  
  Work in the root directory of musclex_ccp13 project (where setup.py is located).
```
python setup.py build --no-user-cfg
```
  We use default configuration. Option `--no-user-cfg` is added here in case there is
  user configuration. (Check that by running `more %USERPROFILE%\pydistutils.cfg`.)

  This step create a folder `build`. Under `build`, there are three folders
  `lib.<suffix>`, `src.<suffix>` and `temp.<suffix>`. (`<suffix>` depends on the
  system type and python version, for example, it might be "win-amd64-3.6".)
  All files under `lib.<suffix>` will be put into a built distribution and can be
  installed to user's computer.

2. Prepare the package  
  Under folder `build\lib.<suffix>`, there is a file `ccp13.<suffix1>.pyd` which
  can be *import*ed in python. It relies on a DLL file located at 
  `build\lib.<suffix>\musclex_ccp13\.libs` to run. So we need to move this file to
  `build\lib.<suffix>`.
```
move build\temp.<suffix>\Release\extra-dll\*.dll build\lib.<suffix>
rd /s /q build\lib.<suffix>\musclex_ccp13
```
  We remove the folder `build\lib.<suffix>\musclex_ccp13` since it is actually not
  necessary in the built distribution. And we move the DLL file from folder `temp`
  because if not, when we create a built distribution it will recreate the folder
  `build\lib.<suffix>\musclex_ccp13`. Assume the system is win-amd64 and python is
  version 3.6, then the actual commands would be:
```
move build\temp.win-amd64-3.6\Release\extra-dll\*.dll build\lib.win-amd64-3.6
rd /s /q build\lib.win-amd64-3.6\musclex_ccp13
```

3. Build distribution  
  Build a windows distribution (an executable installer for windows).
```
python setup.py bdist_wininst
```
  Since *numpy.distutils* does not support building a wheel distribution, we have
  to convert the windows distribution to a wheel one.
```
wheel convert -d dist dist\musclex_ccp13*.exe
```

4. Upload project to PyPI  
  See [Uploading your Project to PyPI][5] for details.
```
twine upload --skip-existing dist\*.whl
```

## Installing musclex_ccp13
```
pip install musclex-ccp13
```

[1]: https://www.visualstudio.com/vs/older-downloads/
[2]: http://mingw-w64.org/doku.php/download/mingw-builds
[3]: https://pypi.python.org/pypi/twine
[4]: https://www.computerhope.com/issues/ch000549.htm
[5]: https://packaging.python.org/tutorials/distributing-packages/#uploading-your-project-to-pypi
