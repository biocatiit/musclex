# Overview

This section regroups the programs maintained by BioCAT and useful for X-ray images users.

## Chiplot Analyze

Chiplot-analyze is a utility program designed to take as input 1D traces of the integrated intensity along rectangular boxed shaped regions aligned along either the equator, the meridian, or along a layer line using the program  [FIT2D](http://www.esrf.eu/computing/scientific/FIT2D/) and saved as “chiplot” files. Chiplot files are an ASCIII format and are easily readable in various ways. What chiplot-analyze does is take one of these traces and splits into two halves containing symmetrical diffraction patterns from the left or right (if trace is from the equator or a layer line) or top and bottom (if trace is along the meridian). You can then subtract a continuous background for the trace  using a convex hull algorithm and save the background subtracted trace in a new file.  The background subtracted trace can then be input into various peak fitting programs for further analysis. In the Irving lab this is usually the [Fityk program](http://fityk.nieto.pl/) which allows defining custom peak functions. The final thing chiplot-analyze can do is to calculate  the centroid and integrated of  user defined diffraction peaks and save the results to a file.

* [Github code](https://github.com/biocatiit/chiplot-analyze)

* Windows, Mac, Deb package installers: [Chiplot Analyze on SourceForge](https://sourceforge.net/projects/chiplot-analyze/)

* Pip install: `pip install chiplot-analyze`

## DDF Processor

DDF Processor averages successive data points inside a DDF file, helping to combine repeated measurements stored in a single DDF dataset into a single, less-noisy trace. The user picks which columns to keep and how many points to average per bin, then exports the result as CSV, HTML, or XLSX.

* Launch from the MuscleX CLI: `musclex ddf`

* Available inside the MuscleX code: [Github code](https://github.com/biocatiit/musclex/blob/master/musclex/ui/ddf_processor.py)

Click [here](DDFProcessor/DDF-Processor-(ddf).md) to learn more about the program and how it works.

## HDF5 to TIFF Converter

```eval_rst
.. note:: Starting in MuscleX 1.22.0, H5 files can directly be read, processed, and analyzed by MuscleX modules.
```

Simple program to convert HDF5 files to TIFF images.

* Available inside the MuscleX code: [Github code](https://github.com/biocatiit/musclex/blob/master/musclex/utils/hdf5_to_tiffs.py)

* [Pip version](https://pypi.org/project/hdf5-to-tiff/): `pip install hdf5-to-tiff`

* Windows installer and AppImage: [HDF5 to TIFF Converter on SourceForge](https://sourceforge.net/projects/hdf5-to-tiff-converter/)

Click [here](WorkingWithHdf5Images/Working-with-hdf5-images.md) to learn more about the program and how it works.

## TIFF Compressor-Decompressor

Simple program to compress or decompress TIFF images. Uses tiff_lzw format (lossless).

* Available inside the MuscleX code: [Github code](https://github.com/biocatiit/musclex/blob/master/musclex/utils/tif_compressor.py)

* [Pip version](https://pypi.org/project/tif-compressor/): `pip install tif-compressor`

* Windows installer and AppImage: [TIFF Compressor-Decompressor on SourceForge](https://sourceforge.net/projects/tiff-compressor-decompressor/)

Click [here](CompressDecompressTif/Compress-decompress-tif-images.md) to learn more about the program and how it works.

## Convert Rectangle Image to Square Images

Script that pads rectangular detector images so they become square. This helps MuscleX find the diffraction center more accurately when the center is close to the edge of the detector. Input and output file paths are provided on the command line.

* Available inside the MuscleX code: [Github code](https://github.com/biocatiit/musclex/blob/master/musclex/utils/rec2sq.py)

Click [here](ConvertRectangleImagetoSquareImages/Convert-rect-to-square.md) to learn more about the program and how it works.

## Simple Add Image

Simple program to add (sum) a set of TIFF images, or to sum the frames inside an HDF5 file, into a single output image.

* Available inside the MuscleX code: [Github code](https://github.com/biocatiit/musclex/blob/master/musclex/utils/simple_add_image.py)

* [Pip version](https://pypi.org/project/strong-image-creation): `pip install strong-image-creation` (legacy package name; will be renamed in a future release)


A more advanced version of this program is available under the name "AISE".
