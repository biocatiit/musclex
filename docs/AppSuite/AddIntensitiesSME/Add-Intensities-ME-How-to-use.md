# How to use

When the program opens, a **Select Experiments** panel is shown. Use it to choose the experiments to load:

1. Click **Browse Folder…** to pick a parent directory containing your experiment subdirectories and/or HDF5 files.
2. The list of available experiments (subdirectories and `.h5`/`.hdf5` files found in the parent folder) is populated automatically. System folders such as `aime_results`, `aise_results`, and `calibration` are excluded.
3. Select the experiments to include using Ctrl/Shift-click, or use **Select All** / **Select None**.
4. Click **Load** to build the alignment table.

Once loaded, the program switches to the main table view.

## Table View

The alignment table lists every image across all loaded experiments. Each row corresponds to one image from one experiment. The table has two view modes, selectable via the top tab bar:

- **Group by Index** — rows are sorted and grouped by frame index (one group per frame number, containing one row per experiment). This is the primary view for summing corresponding frames.
- **Group by Exp** — rows are sorted and grouped by experiment, showing all frames within each experiment together.

Each row shows:

| Column | Description |
|---|---|
| Index | Frame index (shared across experiments) |
| Experiment | Experiment directory or file basename |
| Frame | Image filename |
| Original Center | Manually or automatically detected diffraction center |
| Center Mode | Whether the center was set manually or detected automatically |
| Dist from Base | Distance of this image's center from the global base center |
| Auto Center | Automatically computed center |
| Auto Center Difference | Distance between the manual and auto centers |
| Rotation | Rotation angle |
| Rotation Mode | Manual or automatic |
| Rot Diff from Base | Rotation difference from the global base |
| Auto Rotation | Automatically computed rotation |
| Auto Rot Difference | Difference between manual and auto rotations |
| Size | Image dimensions |
| Image Difference | Pixel-level difference score vs. the global base (computed on demand) |

Click a row to display that image in the viewer. Use the right-click context menu for per-row actions.

### Context Menu

Right-clicking a row (or a selection of rows) provides:

- **Set Center and Rotation** — opens a dialog to interactively set the center and rotation for the selected image.
- **Set as Global Base** — designates the selected image as the reference for all distance/rotation comparisons.
- **Ignore / Cancel Ignore** — excludes or re-includes the selected image(s) from summation and alignment detection.
- **Apply to Subsequent Images** — propagates the current image's center/rotation to all following images.

## Display Options

The display panel (right side, above the settings) controls how the image is rendered. These options do not affect processing:

- **Min / Max Intensity** — clamp the displayed intensity range.
- **Zoom In / Full** — draw a rectangle to zoom; the zoom level persists when navigating between images.
- **Persist Intensities** — keep the current min/max when moving to the next image.
- **Original Center** checkbox — overlays a green circle at the image's current center.
- **Global Base Center** checkbox — overlays a red crosshair at the global base center.

##### Double Zoom
Checking the **Double Zoom** box creates a 10× magnified inset in the top-right corner of the viewer, centred on the mouse pointer (20 × 20 pixels). This is useful for placing calibration points at sub-pixel accuracy. Click the image to freeze the inset, then click the exact point inside the inset to register it in the main image. Uncheck the box to hide the inset.

## Image Operations

The **Image Operations** collapsible panel controls processing options.

#### Compute Average Instead of Sum

When checked, each index group produces the pixel-wise average of its images rather than the sum.

#### Compress the Resulting Images

When checked, output files are written with a `_compressed` suffix and reduced file size.

#### Rotation Transform Mode

Controls how each image is rotated before summation:

- **Align to Make Equator Horizontal** — each image is rotated so that its equator is horizontal (absolute rotation correction).
- **Align to Base Image Rotation** — each image is rotated by the difference between its rotation and the global base rotation (relative correction).

## Alignment Panel

The alignment panel (below the image viewer settings) provides tools for detecting and correcting misalignment across experiments.

#### Detect Centers & Rotations

Automatically computes the diffraction center and rotation for every image. Results are written to the *Auto Center* and *Auto Rotation* columns. Rows where the detected values differ from the global base by more than the configured thresholds are highlighted.

#### Compute Image Difference

Computes a pixel-level similarity score between each image and the global base. Results appear in the *Image Difference* column.

#### Set Center and Rotation Dialog

Opened via right-click → *Set Center and Rotation* or the panel button. The dialog contains the image viewer and the following calibration tools:

##### Set Rotation and Center
Click two corresponding reflection peaks on opposite sides of the equator to define both the center and the rotation angle simultaneously. Press ESC to cancel.

##### Set Center By Chords
Click points along the circumference of the diffraction ring. Perpendicular bisectors of the chords are drawn in blue; their average intersection is taken as the center. Click the button again to confirm.

##### Set Center By Perpendiculars
Click pairs of symmetric reflection peaks (horizontal pairs first, then vertical pairs) to draw perpendicular lines. The average intersection is taken as the center. Click the button again to confirm.

##### Set Rotation Angle
Assumes the center is already correct. Move a line over the equator of the diffraction pattern and click to set the rotation. Press ESC to cancel.

##### Compute Center / Compute Orientation
The program can automatically compute the center or orientation without user input. The *Compute Center* and *Set Calibration Center* checkboxes are mutually exclusive, as are *Compute Orientation* and *Set Orientation*.

##### Apply to Subsequent Images
Propagates the center and/or rotation of the current image to all images that follow it in the table.

## Blank / Mask Settings

The **Blank & Mask** widget (below the display options) allows specifying an empty-cell image to subtract and/or a mask to apply before summation.

- **Select Empty Cell Image(s)** — choose a blank-cell image file.
- **Blank Image Weight** — multiplier applied to the blank image before subtraction.
- **Subtract Empty Cell Image** — enables subtraction during processing.
- **Draw Mask** — opens a pyFAI mask-drawing dialog.
- **Mask Threshold** — automatically masks pixels at or below the specified value (default −1).
- **Show Empty Cell Image / Show Mask** — toggles the viewer to display the blank or mask image.
- **Clamp Negative Values to 0** — replaces negative pixel values with 0 after blank subtraction (enabled only when negative values are detected).

## Summing Images

Click **Sum Images by Index** to start processing. Before the batch begins, a dialog prompts for an **output base name**. The program suggests a common prefix derived from the loaded image filenames; you can accept or change it. Output files are named `<base>_00001.tif`, `<base>_00002.tif`, etc., one per frame index group.

Images marked as *Ignored* are excluded from their group. If all images in a group are ignored, that group is skipped. The button changes to **Stop** during processing; clicking it again cancels the remaining jobs.

## File Format

For HDF5 sources, files must be in the same parent folder and their names must contain a number and the word `data`, with elements separated by `_`. For example: `P2_F5_849_1_094_data_000001.h5`.

For folders containing TIFF images, the last underscore-separated number in the filename is used as the frame index, and the preceding part identifies the experiment. For example, in `P2_F5_849_1_094_data_000001.tif`, the frame index is `000001` and the experiment group is `P2_F5_849_1_094_data`.

## Result Tab

Switch to the **Result** tab (top tab bar) after processing to inspect output images. The tab shows a table listing each result file with its filename, number of source images, total intensity, and processing date. Selecting a row displays the corresponding image in the viewer with the same display options as the main view.

Results are saved to `aime_results/` inside the parent directory. An `intensities.csv` file is also written there with per-file statistics (total intensity, number of pixels not masked, blank image weight, etc.).
