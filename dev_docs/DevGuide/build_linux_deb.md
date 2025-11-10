# Building Linux DEB Package for MuscleX

This guide documents the complete process of building a standalone DEB package for MuscleX on Linux, including all troubleshooting steps and solutions.

## Table of Contents
- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Build Process](#build-process)
- [Troubleshooting](#troubleshooting)
- [Installation and Usage](#installation-and-usage)
- [Best Practices](#best-practices)

---

## Overview

The DEB package is a standalone installer for Debian/Ubuntu systems that includes:
- Python interpreter
- All dependencies (PySide6, numpy, scikit-learn, etc.)
- MuscleX application code
- Desktop launcher integration

**Final Package Size**: ~368MB (includes complete runtime environment)

---

## Prerequisites

### Required Tools
```bash
# Install required packages
sudo apt-get install fakeroot dpkg-deb

# Optional: for package validation
sudo apt-get install lintian
```

### Python Environment
- **Python Version**: 3.10 (required for PySide6 6.7.2 compatibility)
- **Virtual Environment**: Use project's venv with all dependencies installed

**Important**: Do NOT use Python 3.13+ as PySide6 6.7.2 does not support it.

### Required Files
Located in `dev_docs/linux/`:
- `make_deb_installer.py` - DEB packaging script
- `musclex.desktop` - Desktop entry file
- `control` - Package metadata
- `AppIcon.icns` - Application icon

---

## Build Process

### Step 1: Activate Correct Python Environment

```bash
cd /home/alex/VSCode/musclex
source venv/bin/activate  # Uses Python 3.10
python --version  # Verify: should show 3.10.x
```

### Step 2: Install PyInstaller

```bash
pip install pyinstaller
```

### Step 3: Update PyInstaller Spec File

Ensure `musclex_linux.spec` includes all necessary configurations:

```python
# -*- mode: python -*-
from PyInstaller.utils.hooks import collect_data_files

block_cipher = None

# Collect sklearn data files (CSS, HTML templates, etc.)
sklearn_datas = collect_data_files('sklearn')

a = Analysis(['musclex/main.py'],
             pathex=['.'],
             binaries=[],
             datas=[
                 ('musclex/tests/testImages', 'testImages'),
                 ('musclex/tests/testResults', 'testResults'),
                 ('musclex/tests/test_images', 'test_images'),
                 ('musclex/tests/test_logs', 'test_logs')
             ] + sklearn_datas,  # Add sklearn data files
             hiddenimports=[
                 'PyMca5',
                 'PySide6',
                 'PySide6.QtCore',
                 'PySide6.QtGui',
                 'PySide6.QtWidgets'
             ],
             hookspath=['hooks'],
             runtime_hooks=[],
             excludes=['tcl', 'zmq', 'IPython'],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher)

pyz = PYZ(a.pure, a.zipped_data, cipher=block_cipher)

exe = EXE(pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          name='musclex',
          debug=False,
          strip=False,
          upx=True,
          console=True)
```

**Key Points**:
- `collect_data_files('sklearn')`: Includes CSS/HTML files needed by scikit-learn
- `hiddenimports`: Explicitly declare PySide6 modules
- `datas`: Include test data and resources

### Step 4: Build Executable with PyInstaller

```bash
cd /home/alex/VSCode/musclex
source venv/bin/activate
python -m PyInstaller --clean -y musclex_linux.spec
```

**Output**: `dist/musclex` (~371MB single-file executable)

**Build Time**: ~3-5 minutes

### Step 5: Verify Desktop Entry File

Ensure `dev_docs/linux/musclex.desktop` has correct Exec command:

```ini
[Desktop Entry]
Version=1.0
Name=MuscleX
GenericName=MuscleX
Exec=/usr/bin/musclex gui
Terminal=false
Type=Application
Icon=/usr/share/icons/AppIconMusclex.icns
Encoding=UTF-8
Categories=Science;Biology;Physics
```

**Critical**: `Exec=/usr/bin/musclex gui` (not just `musclex`)

### Step 6: Build DEB Package

```bash
cd /home/alex/VSCode/musclex/dev_docs/linux
export PYTHONPATH=/home/alex/VSCode/musclex
python3 make_deb_installer.py
```

**Output**: `musclex-1.26.0_amd64(linux).deb` (~368MB)

### Step 7: Verify Package Structure

```bash
dpkg -c musclex-1.26.0_amd64\(linux\).deb | head -20
```

Expected structure:
```
./usr/bin/musclex                           # Executable
./usr/share/applications/musclex.desktop    # Desktop launcher
./usr/share/icons/AppIconMusclex.icns      # Icon
./DEBIAN/control                            # Package metadata
```

---

## Troubleshooting

### Problem 1: ModuleNotFoundError: No module named 'PySide6'

**Symptoms**:
```
ModuleNotFoundError: No module named 'PySide6'
```

**Cause**: PySide6 not declared as hidden import in spec file

**Solution**:
Add to `hiddenimports` in `musclex_linux.spec`:
```python
hiddenimports=['PyMca5', 'PySide6', 'PySide6.QtCore', 'PySide6.QtGui', 'PySide6.QtWidgets']
```

---

### Problem 2: PySide6 Installation Fails

**Symptoms**:
```
ERROR: Could not find a version that satisfies the requirement PySide6==6.7.2
```

**Cause**: Using Python 3.13+, which PySide6 6.7.2 doesn't support

**Solution**:
Use Python 3.10:
```bash
# Check Python version
python --version  # Should be 3.10.x

# If wrong version, activate correct environment
source venv/bin/activate
```

---

### Problem 3: FileNotFoundError for sklearn CSS files

**Symptoms**:
```
FileNotFoundError: [Errno 2] No such file or directory: 
'/tmp/_MEIq2PwOG/sklearn/utils/_repr_html/estimator.css'
```

**Cause**: PyInstaller doesn't collect non-Python data files by default

**Solution**:
Add to spec file:
```python
from PyInstaller.utils.hooks import collect_data_files
sklearn_datas = collect_data_files('sklearn')

# Then add to datas parameter
datas=[...] + sklearn_datas
```

---

### Problem 4: Application Icon Doesn't Launch GUI

**Symptoms**:
- Clicking desktop icon shows help message instead of GUI
- Terminal shows: "Please specify the program shortcut that you want to run"

**Cause**: Desktop entry missing `gui` argument

**Solution**:
Update `musclex.desktop`:
```ini
Exec=/usr/bin/musclex gui  # Add 'gui' parameter
```

---

### Problem 5: Package Size Too Large

**Expected Size**: 350-400MB (normal for bundled Qt6 application)

**If significantly larger**:
- Check if debug symbols included (set `strip=False` in spec)
- Verify `excludes` list removes unnecessary modules
- Consider removing test data from production builds

---

### Problem 6: PYTHONPATH Error When Building DEB

**Symptoms**:
```
ModuleNotFoundError: No module named 'musclex'
```

**Solution**:
Set PYTHONPATH before running build script:
```bash
export PYTHONPATH=/home/alex/VSCode/musclex
python3 make_deb_installer.py
```

---

## Installation and Usage

### Installation

```bash
# Install package
sudo dpkg -i musclex-1.26.0_amd64\(linux\).deb

# Verify installation
which musclex
dpkg -L musclex
```

### Usage Methods

#### 1. Command Line

```bash
# Launch GUI launcher
musclex gui

# Launch specific modules
musclex eq          # Equator
musclex di          # Diffraction
musclex pt          # Projection Traces
musclex xv          # X-Ray Viewer

# Headless mode
musclex eq -h -i image.tif -s config.json
```

#### 2. Application Menu

- Open application menu
- Search for "MuscleX"
- Click icon (automatically runs `musclex gui`)
- Located in: Science → Biology

### Uninstallation

```bash
sudo dpkg -r musclex
```

---

## Best Practices

### ✅ Do

1. **Use Correct Python Version**
   - Use Python 3.10 (project's target version)
   - Verify with `python --version` before building

2. **Declare All GUI Dependencies**
   - Add PySide6/PyQt modules to `hiddenimports`
   - Include all submodules (QtCore, QtGui, QtWidgets)

3. **Collect Data Files**
   - Use `collect_data_files()` for libraries with resources
   - Test executable before packaging DEB

4. **Test Desktop Integration**
   - Verify `Exec` command in `.desktop` file
   - Test both CLI and GUI launch methods

5. **Clean Build Each Time**
   - Use `--clean` flag with PyInstaller
   - Remove old `build/` and `dist/` directories

### ❌ Don't

1. **Don't Use Wrong Python Version**
   - Avoid Python 3.13+ (PySide6 incompatibility)
   - Don't mix system Python with venv

2. **Don't Skip Testing**
   - Always test executable before DEB packaging
   - Test on clean system without Python

3. **Don't Forget Hidden Imports**
   - PyInstaller can't detect all dynamic imports
   - Always test imports from third-party libraries

4. **Don't Omit Data Files**
   - CSS, icons, templates need explicit collection
   - Check library documentation for data files

---

## File Size Comparison

| Package Type | Size | Contents |
|--------------|------|----------|
| Source (tar.gz) | 85MB | Python source code only |
| Executable (musclex) | 371MB | Python + all dependencies |
| DEB Package | 368MB | Packaged executable + metadata |

**Why is DEB smaller than tar.gz contains more?**
- tar.gz includes documentation, tests, examples
- DEB only includes runtime code and resources
- PyInstaller optimizes and removes unused code

---

## Validation Checklist

Before distributing the DEB package:

- [ ] Package builds without errors
- [ ] Executable runs: `musclex gui`
- [ ] All modules accessible via CLI
- [ ] Desktop icon appears in application menu
- [ ] Clicking icon launches GUI launcher
- [ ] No Python installation required
- [ ] Works on clean Ubuntu/Debian system
- [ ] Package size is reasonable (~350-400MB)
- [ ] Version number correct in `--version` output

---

## Quick Reference

### Complete Build Command Sequence

```bash
# 1. Setup environment
cd /home/alex/VSCode/musclex
source venv/bin/activate

# 2. Build executable
python -m PyInstaller --clean -y musclex_linux.spec

# 3. Build DEB package
cd dev_docs/linux
export PYTHONPATH=/home/alex/VSCode/musclex
python3 make_deb_installer.py

# 4. Verify
ls -lh musclex-1.26.0_amd64\(linux\).deb
```

### Test Installation

```bash
# Install
sudo dpkg -i musclex-1.26.0_amd64\(linux\).deb

# Test CLI
musclex gui

# Test modules
musclex eq
musclex --version

# Uninstall
sudo dpkg -r musclex
```

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.26.0 | 2025-11-05 | Initial documented build process with PySide6 |
| 1.23.2 | Previous | Last version before this documentation |

---

## Additional Resources

- [PyInstaller Documentation](https://pyinstaller.org/en/stable/)
- [Debian Packaging Guide](https://www.debian.org/doc/manuals/maint-guide/)
- [Desktop Entry Specification](https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html)
- [MuscleX Main Documentation](https://musclex.readthedocs.io/)

---

## Support

For issues with DEB package building:
1. Check this troubleshooting guide
2. Verify Python version (must be 3.10)
3. Check PyInstaller warnings in `build/musclex_linux/warn-musclex_linux.txt`
4. Submit issues to: https://github.com/biocatiit/musclex/issues
