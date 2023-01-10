![python logo](https://www.python.org/static/community_logos/python-logo-master-v3-TM.png)

# Installation by Pip

In order to install the program, you need to install [python 3.8](https://www.python.org/).

## Preparing
There are some modules need to be installed before installing Muscle X. They are Pip, OpenCV, PyQT5 (PyQT4 is not Supported), gfortran, PyFAI and Cython.

### Linux

It is recommended to install Python3.8 on Linux to use MuscleX as it is the Python version we use and test. You can still use another one, newer or older, but MuscleX has not been tested on other Python versions. Whichever version you plan on using, you will need the associated pip, dev, and distutils packages. 

Here is the recommended setup to run:
```
sudo apt-get update
sudo apt-get install python3.8 python3-pip python3.8-dev python3.8-distutils 
sudo apt-get install gfortran
#sudo apt-get install git # run this if you are going to use the most up-to-date version
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

```eval_rst
.. note:: If the "Run tests" functionalities are not working, try to go in `Applications > Python 3.8 folder` and double click on `Install Certificates.command`.
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

### Virtual environment install (Recommended)

A virtual environment allows you to have a clean environment that is independent of other software installations. Use the following to create a virtual environment and install musclex inside of it:
```
python3.8 -m pip install --user virtualenv
python3.8 -m venv musclex
source musclex/bin/activate
which python

pip install --upgrade pip
pip install --upgrade distro
pip install --upgrade pyopencl
pip install --upgrade cython
pip install --upgrade numpy
pip install --upgrade opencv-python-headless
pip install --upgrade pyfai
pip install --upgrade PyQt5
pip install --upgrade musclexflibs
pip install --upgrade hdf5plugin
pip install --upgrade numba
pip install --upgrade fisx
pip install --upgrade future

pip install --upgrade musclex
#pip install --upgrade git+https://github.com/biocatiit/musclex.git # use this for the most up-to-date version
```
Note: if you use a different Python version, don't forget to replace `python3.8` by your version.

To exit the virtual environment use:
```
deactivate
```
To re-enter the virtual environment use:
```
source musclex/bin/activate
musclex eq
```

### System install

Instead of a virtual environment install you may install the program directly on the system. It is not recommended as it may create conflicts with other libraries on your computer, or other versions of MuscleX already installed.
First install the libraries needed for MuscleX. 
```
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
pip3 install --upgrade numba
pip3 install --upgrade fisx
pip3 install --upgrade future
```

Run this command to install Muscle X programs
```
pip3 install musclex
```
Note: If you are upgrading, add --upgrade at the end.  

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
