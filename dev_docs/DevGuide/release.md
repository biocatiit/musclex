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

## Process outline
1. Generate pickle testing files, update test files.
2. Test release (GUI == Headless, Current == Previous) using the testing module.
3. Create release tag and enter release notes on Github.
4. Update DOI on Zenodo (if it was not done automatically).
5. Create pip distribution and upload to PyPI.
6. Create docker distribution.
7. Update documentation on Readthedocs.
8. Create AppImage, Deb, Windows, and Mac standalone distributions and upload to Sourceforge (major releases only).
9. Test release on each distribution.

## Prepare a Release
### Generate pickle testing files
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
To know the pip packages necessary, run for example:
```
pip list | grep -f requirements
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
New mac os installation no longer being produced, please use the docker version
1. [Create Stand-alone Program for Mac OS X with PyInstaller][1]
2. [Build a DMG Image or pkg file][3]

> If any changes are made to the source code in the source distribution
  during these steps, check if they work in any previous sections.

### Create a Deb package

Making the .deb installer uses pyinstaller (tested on 5.6.1).

1. Install fakeroot, lintian: sudo apt-get install fakeroot lintian
2. [Build the standalone program using pyinstaller][1]
3.  In the dev_docs/linux directory run `python make_deb_installer.py`
9)  Rename the package appropriate (e.g. RAW-2.0.0-linux-amd64.deb)

Current build notes:
- Using Ubuntu 14.04 LTS
- On linux requires wxpython 4.0.4 (later versions don't package right with pyinstaller)
- With conda on linux, 4.0.4 requires python 3.7
- Requires pyinstaller 4.1 or earlier?
- Using raw_build environment on the virtualbox machine.

Note: If installer is built on Ubuntu 14.04 LTS it works on Debian 8-10 and Ubuntu 14-18.
If installer is built on Debian 8 it works on Debian 8-10 and Ubuntu 16-18.

Need wxpython < 4.1 on Ubuntu 16.04?

Useful resources for building .deb package:
https://plashless.wordpress.com/2013/08/25/a-short-debian-packaging-case-gui-apps-gpl-pyinstaller/
https://plashless.wordpress.com/2013/08/31/app-icons/
https://plashless.wordpress.com/2013/08/29/creating-new-mime-types-in-a-shortcut-debian-packaging/
https://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-description
https://linuxconfig.org/easy-way-to-create-a-debian-package-and-local-package-repository
https://martin.hoppenheit.info/blog/2016/where-to-put-application-icons-on-linux/
https://www.howtoforge.com/tutorial/how-to-convert-packages-between-deb-and-rpm/

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
Copy this file into Documents/DevGuide/release-info

### Update documents in docs_archive
Create a copy of the current docs version in docs_archive and rename it as docs_versionNumber.

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
- Place musclex/requirements.txt, musclex/musclex and musclex/musclex/main.py in musclex/AppImageBuilder Folder.
- Open terminal and run the following command
```
appimage-builder
```
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
[9]:https://doi.org/10.5281/zenodo.1195050
[10]:https://musclex.readthedocs.io/en/latest/credits.html
[11]:https://appimage-builder.readthedocs.io/en/latest/examples/pyqt.html
