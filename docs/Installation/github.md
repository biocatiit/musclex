![GitHub logo](https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png)

# Directly working with downloaded GitHub code

You can access the GitHub repository [here](https://github.com/biocatiit/musclex).

In order to run the code, you need to install [python](https://www.python.org/).

## Preparing

Some modules need to be installed before installing Muscle X. They are Pip, OpenCV, PyQT5 (PyQT4 is not Supported), gfortran, PyFAI and Cython.

### Linux

It is recommended to install Python 3.10 on Linux to use MuscleX as it is the Python version we use and test. You can still use another one, newer or older, but MuscleX has not been tested on other Python versions. Whichever version you plan on using, you will need the associated pip, dev, and distutils packages. 

Here is the recommended setup to run:
```
sudo apt-get update
sudo apt-get install python3 python3-pip python3-dev python3-distutils 
sudo apt-get install gfortran
#sudo apt-get install git # run this if you are going to use the most up-to-date version
```
```eval_rst
.. note:: These `apt-get` commands are needed and not contained no matter the environment you use (virtual environment or system).
```

#### Virtual environment on Linux (Recommended)

A virtual environment allows you to have a clean environment that is independent of other software installations. Use the following to create a virtual environment (you will run MuscleX in this environment):

```
python3 -m pip install --user virtualenv
python3 -m venv musclex
source musclex/bin/activate
which python3
pip install --upgrade pip
pip install --upgrade distro
pip install --upgrade pyopencl
pip install --upgrade cython
pip install --upgrade numpy
pip install --upgrade opencv-python-headless
pip install --upgrade pyfai
pip install --upgrade PyQt5
pip install --upgrade musclex-ccp13
pip install --upgrade hdf5plugin
pip install --upgrade numba
pip install --upgrade fisx
pip install --upgrade future
```

To exit the virtual environment use:
```
deactivate
```
To re-enter the virtual environment use:
```
source musclex/bin/activate
```

#### System environment on Linux

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
pip3 install --upgrade musclex-ccp13
pip3 install --upgrade hdf5plugin
pip3 install --upgrade numba
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
If you get "Could not load the Qt platform plugin", it might be because you have the normal version of OpenCV already installed. Try to remove it with:
```
pip uninstall opencv-python
pip install --upgrade opencv-python-headless
```
If this does not work, install an older version of OpenCV by
```
pip install opencv-python==4.2.0.32
```

