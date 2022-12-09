![GitHub logo](https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png)

# Directly working with downloaded GitHub code

You can access the GitHub repository [here](https://github.com/biocatiit/musclex).

In order to run the code, you need to install [python](https://www.python.org/).

## Preparing

Some modules need to be installed before installing Muscle X. They are Pip, OpenCV, PyQT5 (PyQT4 is not Supported), gfortran, PyFAI and Cython.

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
pip3 install --upgrade numba
pip3 install --upgrade fisx
pip3 install --upgrade future

```
### Virtual environment on Linux

Instead of a system install you may install inside a virtual environment. A virtual environment allows you to have a clean environment that is independent of other software installations. Use the following to create a virtual environment (you will run MuscleX in this environment):

```
sudo apt-get update
sudo apt-get install python3 python3-pip python3-dev gfortran
sudo apt-get install '^libxcb.*-dev' libx11-xcb-dev libglu1-mesa-dev libxrender-dev libxi-dev libxkbcommon-dev libxkbcommon-x11-dev

python3 -m pip install --user virtualenv
python3 -m venv musclex
source musclex/bin/activate
which python3
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
```eval_rst
.. note:: The `apt-get` commands are needed and they are not restricted to the virtual environment even in the case.
```

To exit the virtual environment use:
```
deactivate
```
To re-enter the virtual environment use:
```
source musclex/bin/activate
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

## Installing

Simply [download the zip file from GitHub](https://github.com/biocatiit/musclex) and extract it or clone the repository on your computer.
```
git clone https://github.com/biocatiit/musclex.git
```

## Running the program

First, you will need to build the program from the code. Open a terminal window at the root of the musclex code folder.

Build the program:
```
python3 setup.py clean --all
python3 setup.py build
python3 setup.py install
```
Run the program:
```
musclex [program shortcut]
```
For example, run this command to run Diffraction-Centroids:
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

