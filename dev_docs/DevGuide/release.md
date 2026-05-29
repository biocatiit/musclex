# Publish a New Release of MuscleX

## Contents
* [Process outline](#process-outline)
* [Prepare a Release](#prepare-a-release)
  * [Create a source distribution](#create-a-source-distribution)
  * [Create a Windows installer](#create-a-windows-installer)
  * [Create a Mac OS X App Bundle](#create-a-mac-os-x-app-bundle)
  * [Create a Docker distribution](#create-a-docker-distribution)
  * [Update documents](#update-documents)
* [Publish a Release](#publish-a-release)
  * [Upload packages to SourceForge](#upload-packages-to-sourceforge)
  * [Upload the project to PyPI](#upload-the-project-to-pypi)
  * [Publish the release on GitHub](#publish-the-release-on-github)
  * [Update the information of the new release on Zenodo](#update-the-information-of-the-new-release-on-zenodo)
  * [Create App Image using App Image Installer](#create-app-image-using-app-image-installer)
  * [Create a conda package](#create-a-conda-package)

## Process outline
1. Generate pickle testing files (deprecated), update test files.
2. Test release (GUI == Headless, Current == Previous) using the testing module.
3. Create release tag and enter release notes on Github.
4. Update DOI on Zenodo (if it was not done automatically).
5. Create pip distribution and upload to PyPI.
6. Create conda distribution and upload it to Anaconda Cloud.
7. Create docker distribution and upload to Docker Hub.
8. Update documentation on Readthedocs.
9. Create Windows and Mac standalone distributions and upload to Sourceforge (major releases only).
10. Create a snap distribution and upload to SnapStore.
11. Test release on each distribution.

## Prepare a Release

### GUI Baseline Testing

Before running the automated test suite, GUI baselines must be regenerated whenever the processing logic or CSV output changes. The test suite compares headless output to these baselines to verify that GUI and headless modes produce identical results.

The test image directories and their settings files are located at:
```
musclex/tests/testImages/
├── EIGERimages/          # QF + EQ + DI test images (qfsettings.json, eqsettings.json, disettings.json)
├── MARimages/            # QF + EQ + DI test images
├── PILATUSimages/        # QF + EQ + DI test images
├── EIGER_PT_Convex_Hull_Vertical/   # PT test images (ptsettings.json)
├── MAR_PT_Convex_Hull_Vertical/     # PT test images
└── PT_FittingGaussians_Horizontal/  # PT test images
```

GUI baselines are stored as `summary.csv` inside the `*_results_gui/` subdirectory of each dataset folder:
- `qf_results_gui/summary.csv` — compared by QF headless tests
- `pt_results_gui/summary.csv` — compared by PT headless tests

#### Steps to regenerate GUI baselines

For each module and each dataset directory:

1. Launch the corresponding MuscleX GUI application (e.g. Quadrant Folding, Projection Traces).
2. Open the test image directory (e.g. `musclex/tests/testImages/EIGERimages/`).
3. **Load Settings**: use the module's "Load Settings" button/menu to load the settings file already present in the directory (e.g. `qfsettings.json`, `ptsettings.json`). This ensures the GUI uses the same parameters as headless mode.
4. Process all images in the directory (use "Process All" or equivalent).
5. The GUI writes results to `*_results/` by default (e.g. `qf_results/`). Rename that folder to `*_results_gui/` (e.g. `qf_results_gui/`) so the test suite can find it as the GUI baseline.
6. Confirm that `*_results_gui/summary.csv` now exists.
7. Repeat for every dataset directory listed above.

**Modules and their baseline directories:**

| Module | GUI App | Settings file | Baseline dir |
|---|---|---|---|
| Quadrant Folding (QF) | Quadrant Folding | `qfsettings.json` | `qf_results_gui/` |
| Projection Traces (PT) | Projection Traces | `ptsettings.json` | `pt_results_gui/` |

> EQ and DI do not currently maintain GUI baselines; their tests compare against committed headless baselines only.

#### Run the full automated test suite

After regenerating all GUI baselines, run:

```bash
python -m unittest musclex.tests.musclex_tester.MuscleXGlobalTester -v
```

All 18 tests should pass. Key checks performed:
- **Headless vs. committed baseline** — verifies no regression against the last accepted run.
- **Headless vs. GUI baseline** — verifies that GUI and headless produce identical `summary.csv` output.
- **Schema tests** — verifies that `qfsettings.json` / `eqsettings.json` keys match the declared bindings.

If tests fail after a code change, update the committed headless baselines in `musclex/tests/testResults/` by copying the newly generated headless `summary.csv` files and committing them together with the code change.

---

### Generate pickle testing files (deprecated)
Generate the testing files with the right version number (if you release 1.15.7, change the version number in `__init__.py` first).
Go to the `test_utils.py` directory in the `tests` folder and run:
```
python test_utils.py testrecord
```
(see "Installation -> Testing" documentation, section 'testrecord' for more details about this funtion)
Once all the files have been generated, build the app and test to see if it is working. 

Then take all the folders (di, eq, qf, pt, dc...) and put them in a bigger file named `pickle_tests_v1.15.7` with your version. The name format is very important to be downloaded after.

Compress the folder using zip.

Upload it on SourceForge.

### Update the test files
Update `environment_tester.sh` and `tests/test_logs/release.log`
To know the pip packages necessary, run for example (`libraries` is in `dev_docs/DevGuide/release-info`, you need to update it if you add new libraries to MuscleX):
```
pip list | grep -f libraries
```
Copy and paste this list in `tests/release.log` in "Pip versions detail".

Go to `tests/environment_tester.py` and modify the "python_version" and "pip_details" variables accordingly.

Once all the different distributions have been tested, update again release.log before creating the packages for the release.

### Create a source distribution
Change the version string and any other things related in to this
release in `setup.py` and run
```
python setup.py sdist
```
You'll get a file `musclex-<version>.tar.gz` in folder `dist`. Check
if it can be properly installed with *pip* on different platforms.
```
pip install dist/musclex-<version>.tar.gz
```

### Create a Linux installer (Deb package)
1. [Create Stand-alone Program for Linux with PyInstaller][1]
2. Make a Deb package go to `dev_docs/linux` and run `python make_deb_installer.py` (requires fakeroot, dpkg-deb and lintian)

### Create a Windows installer
1. [Create Stand-alone Program for Windows with PyInstaller][1]
2. [Make a Windows installer (MSI)][2]

> If any changes are made to the source code in the source distribution
  during these steps, check if they work in any previous sections.

### Create a Mac OS X App Bundle
New pkg MacOS installation no longer being produced, we still do the DMG installation but if possible, please use the docker version.

Pyinstaller and DMG image have been done on BioCAT's MacBook Air 2015 Intel i7, on user Jules.
1. [Create Stand-alone Program for Mac OS X with PyInstaller][1]
2. [Build a DMG Image or pkg file][3]

> If any changes are made to the source code in the source distribution
  during these steps, check if they work in any previous sections.

### Create a Docker distribution
1. Copy 'Dockerfile' and 'musclex.sh' to an empty folder from  biocatiit/musclex  
2. Build the Docker image: 
> docker build -t biocat/musclex . 
3. Upload the docker image to Docker-hub
> docker login --username=iitbiocat

> docker images

> docker tag be048eb52814 biocat/musclex:1-14.4

> docker push biocat/musclex:1-14.4
> docker push biocat/musclex

Note: The first push is to save a tagged version, and the second push is to update the `latest` version available on docker (pulled by default when no version is specified).

4. Test:
> ./musclex.sh

5. Record pip package versions:
> pip freeze > pipversions_1-14.4.txt

Copy this file into dev_docs/DevGuide/release-info

### Update documents
[Update docs for Read-the-Docs][4].

## Publish a Release
### Upload packages to SourceForge
Upload the installers for Windows, Linux and Mac OS X to the SourceForge repo.
See our [SourceForge repository][5]. Or use other tools for uploading
(see details [here][6]).

### Upload the project to PyPI
See [Uploading your Project to PyPI][7]. Be careful of this step, because
one version number are only allowed to be used once for uploading one
source distribution.

> python setup.py sdist  
> twine upload dist/*  

### Publish the release on GitHub
Publish the release [here][8].

### Update the information of the new release on Zenodo
Edit [here][9]. **Authors** are generated according to contributors of
the GitHub repo. Change them properly according to [Project Credits][10].

### Create App Image using App Image Installer

Note: AppImage created on the Muscle computer. The compilation might fail on Lethocerus.
IMPORTANT: You need to be in an environment using Python 3.8 and with all the libraries necessary to make MuscleX work.

- Place `musclex/requirements.txt`, `musclex/musclex` and `musclex/musclex/main.py` in `musclex/AppImageBuilder` Folder.
- Change version inside `AppImageBuilder.yml`.
- Open terminal and run the following command:
```
appimage-builder
```
Note: if the command doesn't exist, you can download the appimage-builder [here][12].
- For additional details refer [here][11]
- Pay attention to the PYTHONPATH in AppImageBuilder.yml, it varies depending on different python version.  
- AppImageBuilder.yml could also be re-generated by command 'appimage-builder --generate' according to the refer [here][11]  


[1]:pyinstaller.md
[2]:advanced_installer.md
[3]:build_mac_dmg.md
[4]:update_docs.md
[5]:https://sourceforge.net/projects/musclex/files/
[6]:https://sourceforge.net/p/forge/documentation/Release%20Files%20for%20Download/#scp
[7]:https://realpython.com/pypi-publish-python-package/  
[8]:https://github.com/biocatiit/musclex/releases
[9]:https://doi.org/10.5281/zenodo.8200611
[10]:https://musclex.readthedocs.io/en/latest/credits.html
[11]:https://appimage-builder.readthedocs.io/en/latest/examples/pyqt.html
[12]:https://appimage-builder.readthedocs.io/en/latest/intro/install.html


### Build and upload Conda Packages (via GitHub Actions)

Conda packages are built for all platforms (linux-64, win-64, osx-64, osx-arm64) using GitHub Actions. Two workflows handle this process:

- **Build Conda Packages** (`conda-build.yml`): Builds packages on all 4 platforms and saves them as GitHub Artifacts.
- **Upload Conda Packages** (`conda-upload.yml`): Downloads previously built artifacts and uploads them to Anaconda Cloud (biocat_IIT).

#### Prerequisites

- The pip package must be built and uploaded to PyPI first (the conda build pulls the source from PyPI).
- The `ANACONDA_TOKEN` secret must be configured in the GitHub repository settings (Settings > Secrets and variables > Actions).
- Ensure dependencies in `meta.yaml` are accurate and up to date before triggering a build.

#### 1. Build the Conda Packages

1. Go to the GitHub repository **Actions** tab.
2. Select the **"Build Conda Packages"** workflow.
3. Click **"Run workflow"** and fill in:
   - **version**: The version to build (e.g., `1.27.0`). Must match the version on PyPI.
   - **build_number**: `0` for a new version, increment for rebuilds of the same version.
4. Wait for all 4 platform jobs to complete. The workflow automatically patches `meta.yaml` with the correct version, sha256 (computed from the PyPI tarball), and build number.

#### 2. Test the Packages Locally

1. Download the built artifacts from the completed workflow run page on GitHub.
2. Test locally:
```bash
conda create -n test-musclex python=3.10
conda activate test-musclex
conda install /path/to/downloaded/musclex-*.tar.bz2
```
3. Verify the main functionalities and run the tests.

#### 3. Upload to Anaconda Cloud

Once testing passes:

1. Go to the GitHub repository **Actions** tab.
2. Select the **"Upload Conda Packages"** workflow.
3. Click **"Run workflow"** and fill in:
   - **run_id**: The Run ID of the build workflow (found in the URL: `/actions/runs/<run_id>`).
   - **version**: Must match the version used in the build.
   - **build_number**: Must match the build number used in the build.
4. The workflow downloads all 4 platform artifacts and uploads them to Anaconda Cloud under the `biocat_IIT` user. If a package for a platform already exists, it is skipped (not overwritten).

#### 4. Verify

After upload, test installation from Anaconda Cloud:
```bash
conda create -n verify-musclex python=3.10
conda activate verify-musclex
conda install -c biocat_IIT musclex=<version>
```

#### Notes

- If a build for one platform fails, the other platforms still complete (`fail-fast: false`).
- To rebuild a failed platform, re-run the build workflow and then upload only the new artifacts.
- If you need to overwrite an existing package on Anaconda Cloud, delete it manually via the [Anaconda Cloud dashboard](https://anaconda.org/biocat_IIT/musclex/files) before uploading.
- Build artifacts are retained for 90 days on GitHub.

## Old steps 

### Update documents in docs_archive
Create a copy of the current docs version in docs_archive and rename it as docs_versionNumber.

Not needed anymore since readthedocs allows to retrieve archives directly from the website. See https://readthedocs.org/projects/musclex/downloads/. 
