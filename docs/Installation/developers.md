# Developer Installation Guide

This guide is for developers who want to **clone, modify, test, and optionally contribute** to the MuscleX source code.

> **Python Version Compatibility:**  
> MuscleX is tested with **Python 3.10** and should work with **Python 3.8 through 3.12**.

Repository: [https://github.com/biocatiit/musclex](https://github.com/biocatiit/musclex)



## Overview

A typical development workflow includes:

1. Installing system and Python prerequisites  
2. Creating and activating a virtual environment (recommended)
3. Cloning the MuscleX GitHub repository  
4. Installing Python package requirements  
5. (Optional) Installing the Software
6. Running and modifying the code  
5. (Optional) Committing and pushing changes back to GitHub



## 1. Install System and Python Prerequisites  

### Linux (Ubuntu/Debian)

```bash
sudo apt update
sudo apt install python3 python3-pip python3-dev build-essential gfortran git
```

### macOS

1. Install [Homebrew](https://brew.sh/)
2. Then run:

```bash
brew install python gcc opencv pyqt5 git
sudo pip3 install cython pyfai
```

### Windows

1. Install Python (3.10 recommended): https://www.python.org/downloads/
2. Install Git for Windows: https://git-scm.com/download/win
3. Install [Visual C++ Build Tools](https://visualstudio.microsoft.com/visual-cpp-build-tools/)
4. (Optional) Install Miniconda or Anaconda



## 2. Create and Activate a Virtual Environment (Recommended)

### Using `venv` on Linux/macOS

```bash
python3 -m venv musclex_env
source musclex_env/bin/activate
```

### Using `venv` on Windows

```cmd
python -m venv musclex_env
musclex_env\Scripts\activate
```

### Using `conda` (any platform)

```bash
conda create -n musclex python=3.10
conda activate musclex
```



## 3. Clone the MuscleX GitHub Repository 

```bash
git clone https://github.com/biocatiit/musclex.git
cd musclex
```

To use a specific release:

```bash
git checkout tags/v1.25.0
```

To use the latest development version:

```bash
git checkout master
```



## 4. Install Python Package Requirements  

We assume you are now inside the cloned `musclex` directory (`cd musclex`) and that `requirements` is located in the same folder.

```bash
pip install --upgrade pip
pip install -r requirements
```

To make the `musclex` command available globally on your system (via a registered entry point), you can install in **editable mode**:

```bash
pip install -e .
```

> This step is optional. If you do **not** run `pip install -e .`, you can still use and modify the code—but you will need to run it explicitly using `python`.



## 5. (Optional) Install the Software

Installing the software is **not strictly required** for development, but it provides a convenient command-line entry point (`musclex`) to run the software from anywhere in your terminal. This is useful for testing modules or using the tool like a regular installed application.

There are two ways to install:

### Option 1: Install in Editable Mode (Recommended)

This allows you to modify the code and immediately see changes without reinstalling.

```bash
pip install -e .
```

- Registers the `musclex` CLI entry point
- No code is copied—your edits take effect right away
- Best choice for active development and testing

### Option 2: Build and Install via `setup.py` (Less Preferred)

This builds and installs a copy of the code into your Python environment.

```bash
python3 setup.py clean --all
python3 setup.py build
python3 setup.py install
```

- Also enables the `musclex` CLI
- **Copies** the code into `site-packages` — edits to the local folder will **not** affect the installed version
- Offers more manual control over the build/install process
- Typically used in specialized environments or offline setups

> Note: This method is considered legacy and is generally discouraged in favor of using `pip`.




## 6. Run and Modify the Code

You can now modify the code directly in the local repository.

### If you installed the code (with `pip install -e .`):

You can run the command-line tool with:

```bash
musclex xv
```

### If you did **not** install the code :

Run the main entry point directly:

- **Linux/macOS**:

  ```bash
  python3 musclex
  ```

- **Windows**:

  ```cmd
  python musclex
  ```

> You can also run individual modules/scripts from the `musclex` directory by calling them with `python3` (or `python` on Windows).



## 7. Commit and Push Changes (Optional)

If you have write access:

```bash
git add .
git commit -m "Describe your changes"
git push
```

If not, fork the repository, push your changes to your fork, and submit a pull request on GitHub.



## Troubleshooting

- **Missing Python.h**
   Install development headers:

  ```bash
  sudo apt install python3-dev        # Debian/Ubuntu
  sudo dnf install python3-devel      # Fedora
  ```

- **Qt Plugin Error**
   Resolve OpenCV conflicts:

  ```bash
  pip uninstall opencv-python
  pip install opencv-python-headless
  ```

   If needed:
  
   ```bash
   pip install opencv-python==4.2.0.32
   ```
  
- **Build Errors**
   Ensure you have GCC, GFortran, and Python development headers installed.
   
- **Missing GUI**:
   Ensure `PyQt5` or `PySide2` is installed:

  ```bash
  pip install PyQt5
  ```

- **Missing C extension or iNaT error**:

  ```bash
  pip install -U pandas

- For detailed testing instructions, see the Advanced Testing Guide.
