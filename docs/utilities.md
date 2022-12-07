# Utilities

This section regroups the programs maintained by BioCAT and useful for X-ray images' users.

## EIGER Viewer

This is a small program used to display TIFF and HDF5 files and able to play images in a folder (or h5 file) as a video. A second tab shows a selected slice (or integrated slice) of the image as a graph.

* [Github code]()

* Deb package installer: [EIGER Viewer on SourceForge](https://sourceforge.net/projects/eiger-viewer/)

* Pip install: `pip install eiger-viewer`

```eval_rst
.. note:: Only available on Linux (Deb package and pip install).
command`.
```

## Chiplot Analyze

Chiplot-analyze is a utility program designed to take as input 1D traces of the integrated intensity along rectangular boxed shaped regions aligned along either the equator, the meridian, or along a layer line using the program  [FIT2D](http://www.esrf.eu/computing/scientific/FIT2D/) and saved as “chiplot” files. Chiplot files are an ASCIII format and are easily readable in various ways. What chiplot-analyze does is take one of these traces and splits into two halves containing symmetrical diffraction patterns from the left or right (if trace is from the equator or a layer line) or top and bottom (if trace is along the meridian). You can then subtract a continuous background for the trace  using a convex hull algorithm and save the background subtracted trace in a new file.  The background subtracted trace can then be input into various peak fitting programs for further analysis. In the Irving lab this is usually the [Fityk program](http://fityk.nieto.pl/) which allows defining custom peak functions. The final thing chiplot-analyze can do is to calculate  the centroid and integrated of  user defined diffraction peaks and save the results to a file.

* [Github code](https://github.com/biocatiit/chiplot-analyze)

* Windows, Mac, Deb package installers: [Chiplot Analyze on SourceForge](https://sourceforge.net/projects/chiplot-analyze/)

* Pip install: `pip install chiplot-analyze`

## HDF5 to TIFF Converter

Simple program to convert HDF5 files to TIFF images.

* [Github code]()

* Windows installer: [HDF5 to TIFF Converter on SourceForge](https://sourceforge.net/projects/hdf5-to-tiff-converter/)

* Also available inside the MuscleX code. Click [here](AppSuite/WorkingWithHdf5Images/Working-with-hdf5-images.md) to learn more.

## BioXTAS RAW

BioXTAS RAW is a GUI based, free, open-source Python program for reduction and analysis of small-angle X-ray solution scattering (SAXS) data. The software is designed for biological SAXS data. It is available on windows, macOS (and OS X), and Linux. It provides an alternative to closed source programs such as Primus and Scatter for primary data analysis. Because it can calibrate, mask, and integrate images it also provides an alternative to synchrotron beamline pipelines that scientists can install on their own computers and use both at home and at the beamline.

[Learn more about BioXTAS RAW](https://bioxtas-raw.readthedocs.io/en/latest/)
