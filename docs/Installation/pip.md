![python logo](https://www.python.org/static/community_logos/python-logo-master-v3-TM.png)

# Installation by Pip

In order to install the program, you need to install [Python 3](https://www.python.org/).

## Preparing
There are some modules need to be installed before installing Muscle X. They are Pip, OpenCV, PyQT5 (PyQT4 is not Supported), gfortran, PyFAI and Cython.

### Linux

It is recommended to install Python 3.10 on Linux to use MuscleX as it is the Python version we use and test. You can still use another one, newer or older, but MuscleX has not been tested on other Python versions. Whichever version you plan on using, you will need the associated pip, dev, and distutils packages. 

Then here is the recommended setup to run:
```
sudo apt-get update
sudo apt-get install python3 python3-pip python3-dev python3-distutils 
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
.. note:: If the "Run tests" functionalities are not working, try to go in `Applications > Python 3.x folder` and double click on `Install Certificates.command`.
```

### Windows
For windows, we have built the Fortran source for you, so Fortran compiler is not required. But C compiler is still required to build C extensions during the installation process, and **Microsoft Visual C++** (MSVC) is recommended, refer to [this section](#download-of-the-microsoft-studio-build-tools). If you do need to build the Fortran source, refer to [Installation on Windows from source](#installation-of-musclex-from-source-on-windows).

#### Installing Python
We recommend installing [Anaconda](https://www.anaconda.com/products/individual) to install Python and the required dependencies. Anaconda is a free and open-source distribution of the Python and R programming languages for scientific computing, that aims to simplify package management and deployment. 

##### Download of the Microsoft studio build tools
Some of the libraries used by MuscleX require a compilation of the source code to properly work on windows. To do so, you need to install the build tools from [Microsoft Build Tools 2022](https://visualstudio.microsoft.com/visual-cpp-build-tools/).

##### Installation of the Microsoft studio build tools
To install the build tools, follow these steps:
- start to the visual studio installer app
- once the visual studio installer app is opened, click on "modify" in the visual studio build tools window. You should then be in the "workloads" tab, check the "C++ build tools" box
- in the right panel, only check MSVC … C++ x64/x86 build tools and windows <your version number> SDK and uncheck the rest of options 
- install the tools by hitting the "Modify" button



## Installing

### Virtual environment install (Recommended)
A virtual environment allows you to have a clean environment that is independent of other software installations. We will create a virtual environment and install musclex inside of it.

#### Installation of the virtual environment on linux and Mac OS

```
python3 -m pip install --user virtualenv
python3 -m venv musclex
source musclex/bin/activate
which python

pip install --upgrade pip
pip install wheel numpy
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
musclex eq
```

#### Installation of the virtual environment on Windows
To install the virtual environment on windows, you need to install the anaconda distribution of python. We assume that you did it in the section [Installing Python](#installing-python).

##### Creation of the virtual environment
Open the "anaconda prompt" app. This app is installed with anaconda and can be found in the start menu. Once the app is opened, type the following commands in the terminal:
```
conda create -n musclex python=3.10
conda activate musclex
```
This will create a virtual environment called musclex and activate it. 
Once the virtual environment is activated, you should see (musclex) at the beginning of the line in the terminal.

##### Installation of the dependencies
We assume that you activated the virtual environment in the previous section. If not, please do it before continuing. 
Then, type the following commands in the anaconda prompt terminal:
```
pip install cython numpy pyqt5 opencv-python-headless pyfai
pip install --upgrade pip
pip install wheel numpy
curl -O https://raw.githubusercontent.com/biocatiit/musclex/master/requirements
pip install -r requirements
del requirements
```
Note: If the command "pip install -r requirements" does not work, it is likely that the package manager didn't manage to build the Fortran source files contained in the musclex_cpp13 package. In this case, Follow the instructions in the section [Installation of musclex from source on windows](#installation-of-musclex-from-source-on-windows) to install the package from source.

### Installation of MuscleX

#### Normal install
Once the virtual environment is activated and the dependencies are installed, you can install MuscleX by typing the following command in the anaconda prompt terminal:
```
pip install --upgrade musclex
#pip install --upgrade git+https://github.com/biocatiit/musclex.git # use this for the most up-to-date version
```

#### Installation of MuscleX from source on windows

If the normal install fails on windows, you can install MuscleX from source.
We assume that you already installed the microsoft studio build tools in the section [Download of the Microsoft studio build tools](#download-of-the-microsoft-studio-build-tools).

##### Installation of git
Git is a version control system that allows you to download the source code of MuscleX. Download the latest version of git from https://git-scm.com/download/win and install it.
Open the "command prompt" app from the start menu. Once the app is opened, type the following commands in the terminal:
```
set PATH=%PATH%;C:\Users\<Your_username>\AppData\Local\Programs\Git\bin
```
where <Your_username> is your windows username. Hit enter to validate the command.
Type the command "git" in the terminal. If git is installed correctly, you should see a list of commands. If not, please check that you followed the instructions correctly.

##### Installation of the fortran compiler
The fortran compiler is required to compile the Fortran source files contained in the musclex_cpp13 package. To install the fortran compiler, follow these steps:
- Visit https://winlibs.com
- Download the latest version of the fortran compiler in the "MSVCRT runtime" category in 64 bits
- Unzip the downloaded file
- Copy the path to the "bin" folder of the unzipped file, by right clicking on the address bar of the file explorer and clicking on "copy path"
- Open the "anaconda prompt" app from the start menu
- Set the path to the fortran compiler executable by typing the following command in the anaconda prompt terminal:
```
set PATH=%PATH%;<path_to_the_bin_folder_of_installed_file>
```
where <path_to_the_bin_folder_of_installed_file> is the path to the bin folder of the unzipped file that you copied earlier. Hit enter to validate the command.
- Restart your computer to apply the changes

##### Compilation of the musclex_cpp13 package
One of the dependencies of MuscleX is the musclex_cpp13 package. This package contains some Fortran code that needs to be compiled. To do so, you need to type the following commands in the anaconda prompt terminal:
```
conda activate musclex # if you are not already in the virtual environment
pip install numpy
git clone https://github.com/biocatiit/musclex_ccp13
cd musclex_ccp13
```
Hit enter to validate each command.

- Compile the Fortran code by typing the following command in the anaconda prompt terminal:
```
python setup.py build
```
Don't leave the anaconda prompt terminal until the compilation is finished. Leave it open for the next step.

##### Creation of the musclex_ccp13 package
Once the Fortran code is compiled, we can create the precompiled musclex_cpp13 package. To do so, type the following command in the anaconda prompt terminal you opened in the previous section:
```
start .
```
This will open a file explorer in the current directory. In this file explorer: 
- navigate to the “build” folder, lib.<suffix> where the suffix depends on your windows version
- enter this folder. There you’ll find a folder named “ccp13”
- open this folder, where you’ll find a “.libs” folder. Open this folder and copy the .dll file inside it. go back to the lib.<suffix> file, and paste the dll file you copied. delete the “ccp13” folder.
- go back to the terminal and type: 
```
python setup.py bdist_wheel 
```
This will create the precompiled musclex_cpp13 package, also called a wheel.

##### Installation of the musclex_cpp13 package
In the anacoda prompt terminal, type the following command:
```
pip install dist/musclex_ccp13-<version>.whl
```
where <version> is the version of the package you just created. Hit enter to validate the command.

##### Installation of MuscleX using the precompiled musclex_cpp13 package
Once the musclex_cpp13 package is installed, you can install MuscleX by typing the following command in the anaconda prompt terminal:
```
pip install --upgrade musclex
```


### System install (Only recommended for Linux and Mac OS users)

Instead of a virtual environment install you may install the program directly on the system. It is not recommended as it may create conflicts with other libraries on your computer, or other versions of MuscleX already installed.
First install the libraries needed for MuscleX. 
```
pip3 install --upgrade pip
pip install wheel numpy
wget https://raw.githubusercontent.com/biocatiit/musclex/master/requirements
pip install -r requirements
rm requirements
```

Run this command to install Muscle X programs
```
pip3 install musclex
```
Note: If you are upgrading, add --upgrade at the end.  

## Running a program

### If you installed MuscleX in a virtual environment on Linux or Mac OS
To run a program, you need to activate the virtual environment first. Assuming you created a virtual environment called musclex, you can activate it by typing the following command in the terminal:
```
source musclex/bin/activate
```
Then you can run the program by typing the program shortcut in the terminal. 

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
If you get "Could not load the Qt platform plugin", it might be because you have the normal version of OpenCV already installed. Try to remove it with:
```
pip uninstall opencv-python
pip install --upgrade opencv-python-headless
```
If this change does not work, install an older version of OpenCV by
```
pip install opencv-python==4.2.0.32
```
Once you are done using MuscleX, you can deactivate the virtual environment by typing the following command in the terminal:
```
deactivate
```

### If you installed MuscleX in a virtual environment on Windows
To run a program, you need to activate the virtual environment first. Assuming you created a virtual environment called musclex, you can activate it by typing the following command in the anaconda prompt terminal:
```
conda activate musclex
```
Then you can run the program by typing the program shortcut in the anaconda prompt terminal. 

Simply run
```
musclex [program shortcut]
```
For example, run this command to run Diffraction-Centroids
```
musclex dc
```
Once you are done using MuscleX, you can deactivate the virtual environment by typing the following command in the anaconda prompt terminal:
```
conda deactivate
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

