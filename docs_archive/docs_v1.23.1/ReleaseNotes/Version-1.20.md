# Version-1.20

Release Date : 10/31/2022

### Updates

- Bug fixes (opencv to opencv-headless, UI bugs...)
- Now able to read and analyze hdf5 files (single or multiple images) without converting them to tiff
- "Save a compressed version of the image" checkbox (available when the function can produce an image) 
- Improved hdf5 to tiff converter
- Testing available on all distributions (global testing only)
- Downloading the testing pickle files dynamically in order to make the software lighter
- Fix of mask threshold for Eiger images in quadrant folder
- Deep rearrangement of the code (pylint clean, folders and files rearranged)
- Documentation update (rootless Docker)

```eval_rst
.. note:: Version tested on Python 3.8 - on Ubuntu 20.04 and 22.04, macOS 12.6, Windows 10 and Windows 11.
```
