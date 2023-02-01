# Utilities

This section regroups the programs maintained by BioCAT and useful for X-ray images users.

## Chiplot Analyze

Chiplot-analyze is a utility program designed to take as input 1D traces of the integrated intensity along rectangular boxed shaped regions aligned along either the equator, the meridian, or along a layer line using the program  [FIT2D](http://www.esrf.eu/computing/scientific/FIT2D/) and saved as “chiplot” files. Chiplot files are an ASCIII format and are easily readable in various ways. What chiplot-analyze does is take one of these traces and splits into two halves containing symmetrical diffraction patterns from the left or right (if trace is from the equator or a layer line) or top and bottom (if trace is along the meridian). You can then subtract a continuous background for the trace  using a convex hull algorithm and save the background subtracted trace in a new file.  The background subtracted trace can then be input into various peak fitting programs for further analysis. In the Irving lab this is usually the [Fityk program](http://fityk.nieto.pl/) which allows defining custom peak functions. The final thing chiplot-analyze can do is to calculate  the centroid and integrated of  user defined diffraction peaks and save the results to a file.

* [Github code](https://github.com/biocatiit/chiplot-analyze)

* Windows, Mac, Deb package installers: [Chiplot Analyze on SourceForge](https://sourceforge.net/projects/chiplot-analyze/)

* Pip install: `pip install chiplot-analyze`

## HDF5 to TIFF Converter

Simple program to convert HDF5 files to TIFF images.

* [Github code](https://github.com/biocatiit/musclex/blob/master/musclex/utils/hdf5_to_tiffs.py)

* [Pip version](https://pypi.org/project/hdf5-to-tiff/): `pip install hdf5-to-tiff`

* Windows installer and AppImage: [HDF5 to TIFF Converter on SourceForge](https://sourceforge.net/projects/hdf5-to-tiff-converter/)

* Also available inside the MuscleX code. Click [here](AppSuite/WorkingWithHdf5Images/Working-with-hdf5-images.md) to learn more.
