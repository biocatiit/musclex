# How to use

After launching Equator, select an image file or a `failedcases.txt` file with **File > Select a File (or Failed Cases)...** (`Ctrl+O`). Equator displays the image and processes it with the current settings. If valid cached results already exist for the current program version and preprocessing settings, the cached results are loaded instead of being recalculated.

## File and output options

Use **File > Process Current Folder** (`Ctrl+F`) or the **Process Current Folder** button to process all images in the current folder. For HDF5 input, use **Process Current H5 File** to process all frames in the selected HDF5 file.

Use **File > Change Output Directory...** if results, cache files, and failed-case lists should be written somewhere other than the input data directory. This is useful when the raw data directory is read-only or when you want to keep analysis output separate from the source images.

Use **File > Clear All Caches in Current Folder** (`Ctrl+D`) to remove existing Equator cache files from the selected output directory and force images to be processed again.

Use **File > Save Current Settings** (`Ctrl+S`) to export the current Equator processing and fitting settings to `eqsettings.json`. Calibration, blank image, and mask settings are not stored in this JSON file; they are managed by the shared workspace settings. Use **File > Load Settings...** (`Ctrl+L`) to load a saved Equator settings file and reprocess the current image.

## Image tab

The Image tab shows the processed image and overlays. Display options let you adjust intensity limits, log scaling, colormap, zoom, and overlay visibility for center, R-min, R-max, histogram, integrated area, and detected peaks. Display options do not modify the raw image data.

For shared center, rotation, calibration, restore/apply, and Double Zoom tools, see [Common Settings — Diffraction Center and Rotation](../Common-Settings.html#diffraction-center-and-rotation).

For empty cell subtraction and mask configuration, see [Common Settings — Empty Cell Image and Mask](../Common-Settings.html#empty-cell-image-and-mask). Equator-specific mask behavior is described in [Empty Cell Image and Mask](Blank-Image-and-Mask.html).

The Image Processing panel contains Equator-specific controls:

* **Set R-min** and **Set R-max** define the radial range used for the equatorial analysis. You can also use **Fixed R-min** and **Fixed R-max** to reuse explicit values.
* **Set Box Width** defines the integration area used to build the equatorial projection histogram. Use **Fixed Box Width** to keep the same integration area.
* **Find Orientation with Brightest Spots** and **Orientation Finding** control the method used for automatic rotation/orientation detection.
* **Mask Threshold** sets the value used to identify invalid or masked pixels during Equator processing.
* **Inpainting** fills invalid or masked pixels before processing. Use this cautiously; it is mainly for visualization and gap handling, not for changing raw quantitative measurements.
* **Rotate 90** rotates the result by 90 degrees. **Persist Rotation** keeps that rotation setting across images.
* **Reject** marks the current image as rejected. Rejected images are recorded in the summary output and failed-case list.

## Fitting tab

The Fitting tab shows the projection histogram, convex-hull-subtracted curve, detected peaks, and fitted model. Use the display checkboxes to show or hide the original histogram, convex hull result, fitting graph, peaks, Z line, and center X marker.

General fitting settings include the number of peaks on each side, model type (**Gaussian** or **Voigt**), skeletal muscle Z-line fitting, and optional extra peak fitting. Use **Start Manual Peak Selection** when the automatically detected peaks need correction.

The left and right fitting panels let you adjust per-side fitting parameters. **Use Previous Fit** can reuse parameters from the previous image as a starting point. Use **Refit current image** after changing fitting parameters, or **Refit current folder** to apply the current fitting configuration across the folder.

## Results and parameter editing

The Results tab summarizes the current image's fitting output, including peak areas, S10, d10 when calibration is available, I11/I10, fitting error, and related model parameters.

The Parameter Editor tab exposes the current lmfit parameters for advanced manual refitting. These edits are intended for the current refit session and do not persist as general settings.
