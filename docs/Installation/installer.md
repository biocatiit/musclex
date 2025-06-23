# Install via Installers

## Install via Windows Installer

For users who prefer not to manage Python environments, we provide a pre-built installer for Windows.

### Steps

1. Download the `.exe` installer from the [Releases](https://github.com/biocatiit/musclex/releases) or from [SourceForge](https://sourceforge.net/projects/musclex/files/)
2. Make sure no older version of MuscleX is installed to avoid conflicts
3. Double-click the installer to run
4. Follow the installation wizard
5. Use the shortcut on the Desktop or Start Menu to launch MuscleX
6. Run the test using the **Run Tests** button in the launcher

> This includes all dependencies and the GUI.



## Install via macOS Installer

Currently, macOS builds are not signed, so you may need to bypass security warnings. Two options are available: `.pkg` (deprecated) and `.dmg`.

### DMG Image (Recommended)

1. Download the `.dmg` file from the [Releases](https://github.com/biocatiit/musclex/releases) or [SourceForge](https://sourceforge.net/projects/musclex/files/)

2. Open the `.dmg` and drag the MuscleX app into the **Applications** folder

3. If you see an error message like "damaged app":

   ```bash
   cd /path/to/MuscleX
   xattr -cr musclex.app
   ```

4. Eject and delete the `.dmg` after installation

### PKG Installer (Deprecated)

- The `.pkg` installer is no longer produced. Use `.dmg` or Docker when possible.



## Install via Linux Packages (No Longer Supported)

### AppImage

1. Download the `.AppImage` file from [SourceForge](https://sourceforge.net/projects/musclex/files/)

2. In terminal:

   ```bash
   chmod u+x musclex-1.15.7-x86_64.AppImage
   ./musclex-1.15.7-x86_64.AppImage
   ```

#### Run using AppImage on Fedora, Debian, Arch, CentOS, or Ubuntu

- Download the AppImage from [SourceForge](https://sourceforge.net/projects/musclex/files/)

- Open terminal where the AppImage is located

- Run:

  ```bash
  chmod u+x musclex-1.15.7-x86_64.AppImage
  ./musclex-1.15.7-x86_64.AppImage
  ```

If you encounter the error:

```
AppImages require FUSE to run.
```

Refer to [this troubleshooting guide](https://docs.appimage.org/user-guide/troubleshooting/fuse.html).

If you encounter a GTK-related error when launching the AppImage, try launching the GUI interface with:

```bash
gui
```

instead of calling a module like `qf` directly.

### DEB Package

1. Download the `.deb` file from [SourceForge](https://sourceforge.net/projects/musclex/files/)

2. In terminal:

   ```bash
   sudo dpkg --install musclex-1.20_amd64(linux).deb
   ```



## Verify Installation

To verify CLI installation:

```bash
musclex --help
```

Try a sample module:

```bash
musclex dc
```

You may also launch the GUI from your system's launcher or application folder.
