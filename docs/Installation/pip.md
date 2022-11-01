![python logo](https://www.python.org/static/community_logos/python-logo-master-v3-TM.png)

# Installation by Pip

In order to install the program, you need to install [python 2.7 or 3.6](https://www.python.org/).

## Preparing
There are some modules need to be installed before installing Muscle X. They are Pip, OpenCV, PyQT5 (PyQT4 is not Supported), gfortran, PyFAI and Cython.

### Linux
```
sudo apt-get update
sudo apt-get install python3 python3-pip python3-dev gfortran
sudo apt-get install '^libxcb.*-dev' libx11-xcb-dev libglu1-mesa-dev libxrender-dev libxi-dev libxkbcommon-dev libxkbcommon-x11-dev

pip3 install --upgrade pip
pip3 install --upgrade distro
pip3 install --upgrade pyopencl
pip3 install --upgrade cython
pip3 install --upgrade numpy
pip3 install --upgrade opencv-python-headless
pip3 install --upgrade pyfai
pip3 install --upgrade PyQt5
pip3 install --upgrade musclexflibs
pip3 install --upgrade hdf5plugin
pip3 install --upgrade fisx
pip3 install --upgrade future
```

### Mac OS
```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install opencv
brew install pyqt5
brew install gcc
sudo easy_install pip
sudo pip install pyfai
sudo pip install cython
```


### Windows
For windows, we have built the Fortran source for you, so Fortran compiler is not required. But C compiler is still required to build C extensions during the installation process, and **Microsoft Visual C++** (MSVC) is recommended.

#### Python 2.7 on Windows
The default MSVC chosen by Python 2.7 is MSVC 9.0. We recommend forcing the use of MSVC 14.0 instead. Follow the steps below.

1. Uninstall **Microsoft Visual C++ Compiler for Python 2.7** (MSVC 9.0)
   If [Microsoft Visual C++ Compiler for Python 2.7][1] is installed, uninstall it (See [Repair or remove programs in Windows 10][3]).

2. Install **Microsoft Build Tools 2015 Update 3** (MSVC 14.0)
   The minimal customized installation of [Microsoft Build Tools 2015 Update 3][2] is not enough. Make sure Windows SDK (8.1 or 10) is included.

3. Temporarily set environment variable `VS90COMNTOOLS`
   Type the following command in your current cmd prompt:
   `set VS90COMNTOOLS=%VS140COMNTOOLS%`

Then install dependencies:
```
pip install numpy cython python-qt5 opencv-python-headless pyfai
```

#### Python 3.6 on Windows
The default MSVC chosen by Python 3.6 is MSVC 14.0. The minimal customized installation of [Microsoft Build Tools 2015 Update 3][2] is enough. Then install dependencies: 
```
pip install cython numpy pyqt5 opencv-python-headless pyfai
```

## Installing

### System install

Run this command to install Muscle X programs
```
pip install musclex
```
Note: If you are upgrading, add --upgrade at the end.  

### Virtual environment install

Instead of a system install you may install inside a virtual environment. A virtual environment allows you to have a clean environment that is independent of othe software installations. Use the following to create a virtual environment and install musclex inside it:

```
python3 -m pip install --user virtualenv
python3 -m venv musclex
source musclex/bin/activate
which python3
pip install --upgrade pip
pip3 install --upgrade distro
pip3 install --upgrade pyopencl
pip3 install --upgrade cython
pip3 install --upgrade numpy
pip3 install --upgrade opencv-python-headless
pip3 install --upgrade pyfai
pip3 install --upgrade hdf5plugin
pip3 install --upgrade PyQt5
pip3 install --upgrade musclex
#pip3 install git+https://github.com/biocatiit/musclex.git # use this for the most up-to-date version
musclex eq
```
To exit the virtual environment use:
```
deactivate
```
To re-enter the virtual environment use:
```
source musclex/bin/activate
musclex eq
```

## Running a program
Simply run
```
musclex [program shortcut]
```
For example, run this command to run Diffraction-Centroids
```
musclex dc
```
If you get _ImportError: C extension: iNaT not built. If you want to import pandas from the source directory, you may need to run 'python setup.py build_ext --inplace --force' to build the C extensions first while you're running the program_, please re-install pandas by
```
pip install -U pandas
```
If you get "Could not load the Qt platform plugin" install an older version of opencv by
```
pip3 install opencv-python==4.2.0.32
```

## Updating
```
pip install -U musclex
```
if you do not need to update other dependencies, you can run
```
pip install -U --no-deps musclex
```
if you need to install the latest version directly from github, you can run
```
pip install git+https://github.com/biocatiit/musclex.git
```
if you need to install a previous version (e.g. v1.15.2) directly from github, you can run
```
pip install git+https://github.com/biocatiit/musclex.git@v1.15.2
```

[1]: http://aka.ms/vcpython27
[2]: https://www.visualstudio.com/vs/older-downloads/
[3]: https://support.microsoft.com/en-us/help/4028054/windows-repair-or-remove-programs-in-windows-10
