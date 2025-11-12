# Windows Build Resources

This folder contains Windows-specific build resources for MuscleX.

## Contents

- **AppIcon.ico** - Application icon for Windows executables and installers
- **convert_icon.py** - Script to convert the macOS .icns to Windows .ico format

## Icon Usage

The `AppIcon.ico` file is used in:

1. **PyInstaller** (`musclex_win32.spec`):
   - Embedded in `musclex-main.exe`
   - Embedded in `musclex-launcher.exe`
   - Displays in: Desktop shortcuts, taskbar, File Explorer, Control Panel

2. **Advanced Installer**:
   - Application folder shortcuts
   - Desktop shortcuts
   - Start Menu entries
   - Control Panel program list

## Regenerating the Icon

If you need to update the icon from the source `.icns` file:

```bash
# Using conda environment
cd dev_docs/win
conda activate musclex-dev
python convert_icon.py
```

Or:

```bash
# Using system Python with Pillow installed
cd dev_docs/win
python3 convert_icon.py
```

The script will read `../linux/AppIcon.icns` and generate a new `AppIcon.ico` with multiple resolutions (16x16, 32x32, 48x48, 64x64, 128x128, 256x256).

## Building Windows Executables

See the main documentation:
- [PyInstaller Build Guide](../DevGuide/pyinstaller.md)
- [Advanced Installer Guide](../DevGuide/advanced_installer.md)

Quick command:
```cmd
pyinstaller --clean -y musclex_win32.spec
```

The icon will be automatically embedded into the executables during the build process.

