# Install via Installers

## Install via Windows Installer

For users who prefer not to manage Python environments, we provide a pre-built installer for Windows.

### Steps

1. Download the `.exe` installer:
   - **Version 1.26.0 and later**: [GitHub Releases](https://github.com/biocatiit/musclex/releases)
   - **Version 1.24.0 and earlier**: [SourceForge](https://sourceforge.net/projects/musclex/files/)
2. Make sure no older version of MuscleX is installed to avoid conflicts
3. Double-click the installer to run
4. Follow the installation wizard
5. Use the shortcut on the Desktop or Start Menu to launch MuscleX
6. Run the test using the **Run Tests** button in the launcher

> This includes all dependencies and the GUI.



## Install via macOS DMG Image

> **Note**: For version 1.24.0 and later, DMG Image are not available. Please use [pip](pip.md) or [conda](conda.md) instead.

For older versions (1.23.2 and earlier), macOS builds are not signed, so you may need to bypass security warnings. 

1. Download the `.dmg` file:
   - **Version 1.23.2 and earlier**: [SourceForge](https://sourceforge.net/projects/musclex/files/)

2. Open the `.dmg` and drag the MuscleX app into the **Applications** folder

3. If you see an error message like "damaged app":

   ```bash
   cd /path/to/MuscleX
   xattr -cr musclex.app
   ```

4. Eject and delete the `.dmg` after installation

### PKG Installer (Deprecated)

- The `.pkg` installer is no longer produced. Use `.dmg` or Docker when possible.



## Install via Linux Packages DEB Package

1. Download the `.deb` file from [GitHub Releases](https://github.com/biocatiit/musclex/releases)

2. In terminal:

   ```bash
   sudo dpkg --install musclex-1.27.0_amd64(linux).deb
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
