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
<!-- > **Note**: For version 1.24.0 and later, DMG Image are not available. Please use [pip](pip.md) or [conda](conda.md) instead. -->

>For older versions (1.23.2 and earlier), macOS builds are not signed, so you may need to bypass security warnings. 

1. Download the `.dmg` file:
   - Download the `.dmg` file from [GitHub Releases](https://github.com/biocatiit/musclex/releases)

   - **Version 1.23.2 and earlier**: [SourceForge](https://sourceforge.net/projects/musclex/files/)

2. Open the `.dmg` and drag the MuscleX app into the **Applications** folder

3. If you see an error message like "damaged app" or an "unidentified developer" warning, run in Terminal:

   ```bash
   xattr -cr /Applications/MuscleX.app
   ```

   Alternatively, right-click the app → hold the **Option** key → click **Open** → confirm.

4. Eject and delete the `.dmg` after installation

### Troubleshooting `QLocalServer::listen: Name error` on macOS

If launching the `xv` module shows `QLocalServer::listen: Name error`, the temporary Unix-domain socket path used by Qt may be too long. This can happen when the temporary directory or working directory path is long.

In the active Terminal session, set `TMPDIR` to a shorter temporary directory before starting MuscleX:

```bash
export TMPDIR=/tmp
musclex xv
```

The GUI should open without the `Name error`. This setting applies only to the current Terminal session.

Alternatively, start MuscleX from a shorter working directory:

```bash
cd /tmp
musclex xv
```

If the error persists after a previous crash, quit any running MuscleX processes and check for a stale MuscleX socket in the temporary directory. Remove only a socket you have identified as belonging to MuscleX; do not clear all of `/tmp`.

### PKG Installer (Deprecated)

- The `.pkg` installer is no longer produced. Use `.dmg` or Docker when possible.



## Install via Linux Packages DEB Package

One-line install (removes any existing version, fetches and installs the latest `.deb`):

```bash
dpkg -s musclex >/dev/null 2>&1 && sudo apt remove -y musclex && sudo apt autoremove -y; tmp=$(mktemp /tmp/musclex_XXXXXX.deb); latest_url=$(curl -s https://api.github.com/repos/biocatiit/musclex/releases | grep browser_download_url | grep linux.deb | head -n 1 | cut -d '"' -f 4) && wget -O "$tmp" "$latest_url" && chmod a+r "$tmp" && sudo apt install -y "$tmp" && rm -f "$tmp"
```

**Alternative (manual install):**

1. Download the `.deb` file from [GitHub Releases](https://github.com/biocatiit/musclex/releases)

2. In terminal:

   ```bash
   sudo dpkg --install musclex-1.27.x_amd64(linux).deb
   ```
   Change '1.27.x' to the real version you download.


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
