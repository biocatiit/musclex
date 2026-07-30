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
    * [Troubleshoot a release missing from Zenodo](#troubleshoot-a-release-missing-from-zenodo)
  * [Create App Image using App Image Installer](#create-app-image-using-app-image-installer)
  * [Publish PyPI and Conda packages](#publish-pypi-and-conda-packages)
    * [Standalone package workflows](#standalone-package-workflows)

## Process outline
1. Generate pickle testing files (deprecated), update test files.
2. Test release (GUI == Headless, Current == Previous) using the testing module.
3. Create release tag and enter release notes on Github.
4. Update DOI on Zenodo (if it was not done automatically).
5. Run the combined workflow to publish the pip and conda distributions.
6. Create docker distribution and upload to Docker Hub.
7. Update documentation on Readthedocs.
8. Create Windows and Mac standalone distributions and upload to Sourceforge (major releases only).
9. Create a snap distribution and upload to SnapStore.
10. Test release on each distribution.

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
PyPI and Conda packages are published together by the manually triggered
workflow described in [Publish PyPI and Conda packages](#publish-pypi-and-conda-packages).
Be careful with this step because a version uploaded to PyPI cannot be
replaced.

### Publish the release on GitHub
Publish the release [here][8].

### Update the information of the new release on Zenodo
Edit [here][9]. **Authors** are generated according to contributors of
the GitHub repo. Change them properly according to [Project Credits][10].

#### Troubleshoot a release missing from Zenodo

Zenodo normally creates a new record after a GitHub release is published. If
the release is public on GitHub but does not appear at all under the enabled
`biocatiit/musclex` repository in Zenodo, the publication webhook may have
been missed. The **Sync now** button refreshes repository access, but it does
not import a missed release.

To send a new publication event without deleting the tag or release assets:

1. In **Zenodo > GitHub**, switch `biocatiit/musclex` off, wait a few seconds,
   and switch it on again. This reinstalls or refreshes the Zenodo webhook.
2. Temporarily convert the existing GitHub release to a draft, then publish
   the same release again. Replace `vX.Y.Z` below with the release tag:

```bash
gh auth login

RELEASE_ID=$(gh api repos/biocatiit/musclex/releases/tags/vX.Y.Z --jq .id)

gh api --method PATCH \
  repos/biocatiit/musclex/releases/$RELEASE_ID \
  -F draft=true

gh api --method PATCH \
  repos/biocatiit/musclex/releases/$RELEASE_ID \
  -F draft=false \
  -F prerelease=false
```

This preserves the existing release, tag, description, and uploaded assets.
Wait several minutes, then check the repository's release list in Zenodo.

Do **not** rerun the `Build MuscleX` workflow to troubleshoot this problem.
Its `create-release` job deletes an existing GitHub release before creating a
new draft, which is unnecessary and may discard the existing release state.

If the release is still missing, a repository administrator can inspect
**GitHub > Settings > Webhooks > Zenodo > Recent deliveries**. GitHub only
allows recent webhook deliveries to be redelivered for a limited time.

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


### Publish PyPI and Conda packages

The manually triggered **Publish PyPI and Conda Packages**
(`publish-packages.yml`) workflow orchestrates the three reusable package
workflows. It publishes the source distribution to PyPI, builds Conda packages
for `linux-64`, `win-64`, `osx-64`, and `osx-arm64`, uploads them to the
`biocat_IIT` Anaconda Cloud account, and verifies the uploads. This is the
convenient default for a normal release; each underlying workflow also remains
manually runnable for troubleshooting and partial reruns.

#### Prerequisites

- The GitHub release tag must exist and match the version in
  `musclex/__init__.py`.
- The `PYPI_TOKEN` secret must be available to the `pypi` GitHub environment.
- The `ANACONDA_TOKEN` secret must be configured in the GitHub repository settings (Settings > Secrets and variables > Actions).
- Ensure dependencies in `meta.yaml` are accurate and up to date before triggering a build.

#### Run the publishing workflow

1. Go to the GitHub repository **Actions** tab.
2. Select the **Publish PyPI and Conda Packages** workflow.
3. Click **"Run workflow"** and fill in:
   - **release_ref**: The complete GitHub release tag, such as `v2.1.0`.
   - **conda_build_number**: `0` for a new version; increment it only when
     rebuilding Conda packages for an existing version.
4. Confirm the workflow checks out the requested tag and validates that its
   package version matches.
5. If the `pypi` environment requires approval, approve the PyPI publication.
6. Wait for the source distribution and all four Conda packages to be
   published and verified.

The combined workflow calls `pypi-upload.yml`, `conda-build.yml`, and
`conda-upload.yml` in order. Conda uploads do not begin unless all four
platform builds succeed. Build artifacts remain available from the same
workflow run for troubleshooting.

#### Standalone package workflows

Use the original manually triggered workflows when only one stage needs to be
run:

- **Upload Package to PyPI** (`pypi-upload.yml`) accepts `release_ref`, such
  as `v2.1.0`.
- **Build Conda Packages** (`conda-build.yml`) accepts `version` without the
  leading `v`, `build_number`, and an optional `release_ref`. Its completed
  run provides the artifacts and run ID needed by the upload workflow.
- **Upload Conda Packages** (`conda-upload.yml`) accepts the Conda build
  `run_id`, `version`, and `build_number`.

#### Test a Conda package locally

1. Download the built artifacts from the completed workflow run page on GitHub.
2. Test locally:

```bash
conda create -n test-musclex python=3.10
conda activate test-musclex
conda install /path/to/downloaded/musclex-*.tar.bz2
```

3. Verify the main functionalities and run the tests.

#### Verify installation from Anaconda Cloud

After upload, test installation from Anaconda Cloud:

```bash
conda create -n verify-musclex python=3.10
conda activate verify-musclex
conda install -c biocat_IIT musclex=<version>
```

#### Notes

- If a build for one platform fails, the other platforms still complete (`fail-fast: false`).
- Conda uploads start only after every platform build succeeds.
- If the combined workflow fails after PyPI has already been published, use
  the standalone **Build Conda Packages** and **Upload Conda Packages**
  workflows to resume. PyPI does not allow replacing an existing file.
- If you need to overwrite an existing package on Anaconda Cloud, delete it manually via the [Anaconda Cloud dashboard](https://anaconda.org/biocat_IIT/musclex/files) before uploading.
- Build artifacts are retained for 90 days on GitHub.

## Old steps 

### Update documents in docs_archive
Create a copy of the current docs version in docs_archive and rename it as docs_versionNumber.

Not needed anymore since readthedocs allows to retrieve archives directly from the website. See https://readthedocs.org/projects/musclex/downloads/. 
