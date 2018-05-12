# Publish a New Release of MuscleX

## Contents
* [Prepare a Release](#prepare-a-release)
  * [Create a source distribution](#create-a-source-distribution)
  * [Create a Windows installer](#create-a-windows-installer)
  * [Create a Mac OS X App Bundle](#create-a-mac-os-x-app-bundle)
  * [Update documents](#update-documents)
* [Publish a Release](#publish-a-release)
  * [Upload packages to SourceForge](#upload-packages-to-sourceforge)
  * [Upload the project to PyPI](#upload-the-project-to-pypi)
  * [Publish the release on GitHub](#publish-the-release-on-github)
  * [Update the information of the new release on Zenodo](#update-the-information-of-the-new-release-on-zenodo)

## Prepare a Release
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

### Create a Windows installer
1. [Create Stand-alone Program for Windows with PyInstaller][1]
2. [Make a Windows installer (MSI)][2]

> If any changes are made to the source code in the source distribution
  during these steps, check if they work in any previous sections.

### Create a Mac OS X App Bundle
1. [Create Stand-alone Program for Mac OS X with PyInstaller][1]
2. [Build a DMG Image][3]

> If any changes are made to the source code in the source distribution
  during these steps, check if they work in any previous sections.

### Update documents
[Update docs for Read-the-Docs][4].

## Publish a Release
### Upload packages to SourceForge
Upload the installers for Windows and Mac OS X to the SourceForge repo.
See our [SourceForge repository][5]. Or use other tools for uploading
(see details [here][6]).

### Upload the project to PyPI
See [Uploading your Project to PyPI][7]. Be careful of this step, because
one version number are only allowed to be used once for uploading one
source distribution.

### Publish the release on GitHub
Pubulish the release [here][8].

### Update the information of the new release on Zenodo
Edit [here][9]. **Authors** are generated according to contributors of
the GitHub repo. Change them properly according to [Project Credits][10].


[1]:pyinstaller.md
[2]:advanced_installer.md
[3]:build_mac_dmg.md
[4]:update_docs.md
[5]:https://sourceforge.net/projects/musclex/files/
[6]:https://sourceforge.net/p/forge/documentation/Release%20Files%20for%20Download/#scp
[7]:https://packaging.python.org/tutorials/distributing-packages/#uploading-your-project-to-pypi
[8]:https://github.com/biocatiit/musclex/releases
[9]:https://doi.org/10.5281/zenodo.1195050
[10]:https://musclex.readthedocs.io/en/latest/credits.html