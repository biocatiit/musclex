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
wget https://raw.githubusercontent.com/biocatiit/musclex/master/requirements
pip install -r requirements
rm requirements
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
pip install wheel numpy
wget https://raw.githubusercontent.com/biocatiit/musclex/master/requirements
pip install -r requirements
rm requirements
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

### Installing with pip from GitHub

The simplest way to install the latest version of musclex available on GitHub is to use pip.
```
pip install git+https://github.com/biocatiit/musclex
```

### Alternative: Install from the downloaded code and build it locally

Simply [download the zip file from GitHub](https://github.com/biocatiit/musclex) and extract it or clone the repository on your computer. 
 Replace '<tag>' below with the name of the tag for the version you would like to use.
```
git clone https://github.com/biocatiit/musclex.git
git checkout tags/<tag>
git pull
```

The **development** version of the code is represented by the master branch on the GitHub repo, so once the repo is cloned, the development version of the code is used by default.  If you have checked out a tag (built the release version from source with the commands above) would like to switch back to the **development** version of the program, cd into the root of the musclex code folder then run the following command

```
git checkout master
git pull
```

The commands will switch from the main (release) branch to a development branch.

**Note that the development branch contains features that are untested and will most likely contain more bugs than the release version. Please report all bugs you find so we can fix them**


To run the program, you will need to build the program from the code. Open a terminal window at the root of the musclex code folder.

Build the program:
```
python3 setup.py clean --all
python3 setup.py build
python3 setup.py install
```

## Running the program

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

