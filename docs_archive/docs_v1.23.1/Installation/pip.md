![python logo](https://www.python.org/static/community_logos/python-logo-master-v3-TM.png)

# Installation by Pip

This guide will walk you through the steps for installing MuscleX using pip, Python's package manager.

## Before You Begin

This installation guide assumes that you have already set up a virtual environment according to our [Prerequisite Guide: Setting Up Virtual Environments](Prerequisites.md). It is essential to have your environment ready to ensure MuscleX is installed in an isolated space, preventing any potential conflicts with other packages on your system.

If you have not yet prepared a virtual environment, please refer to the prerequisite guide for detailed instructions.

## Installing MuscleX

With your virtual environment set up and activated, you are now ready to install MuscleX. Execute the following command within your virtual environment:

```bash
pip install --upgrade musclex
```

Or, to install the most current development version directly from GitHub:

```bash
pip install --upgrade git+https://github.com/biocatiit/musclex.git
```

Ensure that these commands are run within the virtual environment you have prepared for MuscleX to maintain the isolation from other Python packages.

## Post-Installation

After successfully installing MuscleX, you can verify the installation by checking the installed packages in your environment:

```bash
pip list | grep musclex
```

This command should list MuscleX and its version, indicating that the installation was successful.

## Running MuscleX

With MuscleX installed, you can now run the application directly from the command line, within your virtual environment. Make sure your virtual environment is activated whenever you wish to use MuscleX.

To deactivate your virtual environment when you're finished working with MuscleX, simply run:

```bash
deactivate
```

if you are using a venv environment, or:

```bash
conda deactivate
```
if you are using a Conda virtual environment.

## Updating MuscleX

To update MuscleX to the latest version, use the following command within your virtual environment:

```bash
pip install --upgrade musclex
```

### System install (Only recommended for Linux and Mac OS users)

Instead of a virtual environment install you may install the program directly on the system. It is not recommended as it may create conflicts with other libraries on your computer, or other versions of MuscleX already installed.

Run this command to install Muscle X programs
```
pip install musclex
```
Note: If you are upgrading, add --upgrade at the end.  

## Running a program

### If you installed MuscleX in a virtual environment on Linux or Mac OS
To run a program, you need to activate the virtual environment first. Assuming you created a virtual environment called musclex, you can activate it by typing the following command in the terminal:
```
conda activate musclex # or use 'source activate musclex' if you are using a venv environment
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
conda deactivate # or use 'deactivate' if you are using a venv environment
```

### If you installed MuscleX in a virtual environment on Windows
To run a program, you need to activate the virtual environment first. Assuming you created a virtual environment called musclex, you can activate it by typing the following command in the anaconda prompt terminal:
```
conda activate musclex # or use 'activate musclex' if you are using a venv environment
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
conda deactivate # or use 'deactivate' if you are using a venv environment
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



## Troubleshooting

If you encounter any issues during the installation process, ensure that your virtual environment is correctly set up and activated. For further assistance, refer back to the [Prerequisite Guide: Setting Up Virtual Environments](Prerequisites.md) or consult the troubleshooting section of the MuscleX documentation.