# Install with pip

This guide covers installation of MuscleX using Python's `pip` package manager.



## Prerequisites

MuscleX is **tested with Python 3.10**, but should work with **Python 3.8 through 3.12**.



### Check Your Python Version

Run the appropriate command for your platform:

- **Linux/macOS**:

  ```bash
  python3 --version
  ```

- **Windows (with Python Launcher)**:

  ```cmd
  py --version
  ```

If your version falls within the supported range, there's no need to install Python 3.10.



### Install prerequisites

#### **Linux (Ubuntu/Debian)**

```bash
sudo apt update
sudo apt install python3-pip python3-dev build-essential gfortran
```

#### **macOS**

```bash
brew install python gcc
```

#### **Windows**

- Install Python from https://www.python.org/
- Install [Build Tools for Visual Studio](https://visualstudio.microsoft.com/visual-cpp-build-tools/)



## Create and Activate a Virtual Environment (Recommended)

It is strongly recommended to install MuscleX inside a virtual environment to avoid conflicts with other Python packages.

> **Note:** `venv` is recommended for pip installations. If you are using Conda, refer to the Conda installation instructions instead.

### **venv (Linux/macOS)**

```bash
python3 -m venv musclex_env
source musclex_env/bin/activate
```

### **venv (Windows)**

```bash
python -m venv musclex_env
musclex_env\Scripts\activate
```

### **conda (all platforms)**

```bash
conda create -n musclex python=3.10
conda activate musclex
```



## Install MuscleX

### Install Latest Stable Release

```bash
pip install --upgrade musclex
```

### Install a Specific Version (e.g., v1.24.0)

```bash
pip install musclex==1.24.0
```

### Install Latest Beta/Pre-release from GitHub

```bash
pip install --upgrade git+https://github.com/biocatiit/musclex.git
```

### To install a previous release directly from GitHub:

```bash
pip install git+https://github.com/biocatiit/musclex.git@v1.15.2
```



## Verifying Installation

To confirm that MuscleX was installed correctly:

```bash
pip list | grep musclex
```



## Running MuscleX

After activation, run any program like so:

```bash
musclex xv
```

To deactivate:

```bash
deactivate              # venv
conda deactivate        # conda
```



## Troubleshooting

- **Missing `Python.h`**:
   Install Python development headers:

  ```bash
  sudo apt install python3-dev        # Debian/Ubuntu
  sudo dnf install python3-devel      # Fedora
  ```

- **Build errors**:
   Try installing using `conda` instead, or ensure all required build tools are available.

- **Missing GUI**:
   If you get GUI-related import errors, ensure `PyQt5` or `PySide2` is installed:

  ```bash
  pip install PyQt5
  ```

- **Missing C extension or iNaT error**:

  ```bash
  pip install -U pandas
  ```

- **Qt platform plugin error** (e.g. `qt.qpa.plugin: Could not load the Qt platform plugin "xcb"`):

  First, swap `opencv-python` (which bundles its own Qt and frequently clashes with the system Qt) for the headless build:

  ```bash
  pip uninstall opencv-python
  pip install opencv-python-headless
  ```

  If needed, pin to an older OpenCV release:

  ```bash
  pip install opencv-python==4.2.0.32
  ```

  On Linux, the `xcb` plugin also requires several native XCB / X11 libraries. On Debian/Ubuntu install:

  ```bash
  sudo apt update
  sudo apt install libxcb-cursor0 libxkbcommon-x11-0 libxcb-xinerama0
  sudo apt install libxcb-render-util0 libxcb-image0 libxcb-keysyms1 libxcb-icccm4
  sudo apt install x11-apps qt6-base-plugins qt6-base-dev
  sudo apt-get install python3-opencv
  ```

  To diagnose which specific library is missing, rerun with Qt plugin debug output:

  ```bash
  QT_DEBUG_PLUGINS=1 musclex xv
  ```

  See also the cross-platform [troubleshooting guide](troubleshooting.md#qt-platform-plugin-xcb-error-linux).

- **Pip dependency issues**:
   Ensure wheel and numpy are up to date:

  ```bash
  pip install --upgrade wheel
  pip install --upgrade numpy
  ```



## Optional: Installing Python 3.10

If you need to match the tested version or experience compatibility issues, follow these steps to install Python 3.10.

### **Linux (Ubuntu/Debian)**

```bash
sudo apt install python3.10 python3.10-venv python3.10-dev
```

To use it:

```bash
python3.10 -m venv musclex_env
```

### **macOS**

```bash
brew install python@3.10
```

To use it:

```bash
python3.10 -m venv musclex_env
```

### **Windows**

- Install Python 3.10 from [python.org](https://www.python.org/downloads/release/python-3100/)
- To create a virtual environment:

```cmd
py -3.10 -m venv musclex_env
```



## Updating MuscleX

Update via pip:

```bash
pip install -U musclex
```

Without updating dependencies:

```bash
pip install -U --no-deps musclex
```

From GitHub:

```bash
pip install --upgrade git+https://github.com/biocatiit/musclex.git
```



## System-wide Install (Not Recommended)

If needed:

```bash
pip install musclex
```

> Note: This may interfere with other Python packages on your system. Use with caution.
