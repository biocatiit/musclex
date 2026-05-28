# How to use

When the program opens, a large **Click Here to Select an Image...** button is shown in the centre of the window. Click it and pick any image inside the folder you want to process (TIFF, CBF, HDF5, PNG, JPEG). AISE loads the whole folder (or the entire HDF5 file) and switches to the main table view. The output folder can be changed at any time via **File > Change Output Directory…**.

## Workflow Overview

A **Workflow Guide** dialog is shown on first launch (and can be reopened at any time via the **Workflow Guide** button in the top bar). The recommended steps are:

1. **Set the global (reference) image** — by default the first image is the reference. Right-click a row and choose *Set as Global Base* to change it.
2. **Correct the center of the global image** — adjust its diffraction center in the viewer. Right-click → *Apply to Subsequent Images* to propagate the correction.
3. **Correct the orientation (rotation) if needed** — adjust the rotation of the global image and propagate as above.
4. **Detect misalignment** — click *Detect Centers & Rotations* in the alignment panel. The table highlights rows whose center distance or rotation difference exceeds the configured thresholds. Pairwise pixel-level *Image Difference* scores are filled in automatically as a side effect (see [Alignment thresholds](#alignment-thresholds)).
5. **Correct misaligned images** — select a misaligned row and adjust its center/rotation. Right-click → *Apply to Subsequent Images* for progressive drift, or *Ignore* to exclude an image entirely.
6. **Repeat detection and correction** as needed.
7. **Define bins** — choose a grouping mode in the *Image Operations* panel:
   - **Bin Images**: pick a *Binning factor*. AISE creates sequential bins of that size automatically; changing the factor updates the bins immediately.
   - **Select Group Graphically**: highlight rows in the table, then right-click → *Group* to create a bin manually (≥ 2 rows required).
8. **Sum Images** — choose *Average* or *Sum* in *Image Operations*, then click the **Sum Images** button.
9. **Inspect results** — switch to the **Result** tab to browse and preview the output images.

## Table View

The alignment table lists every image in the loaded folder. Each row shows:

| Column | Description |
|---|---|
| Group | Bin assignment (coloured blue when grouped) |
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
- **Group** — creates a bin from the selected rows (requires ≥ 2 rows selected). Multi-row labels show the count, e.g. *Group (3 images)*.
- **Ignore / Cancel Ignore** — excludes or re-includes the selected image(s) from summation and alignment detection.
- **Apply to Subsequent Images** — propagates the current image's center/rotation to all following images.

Right-clicking the **Group** column cell of an existing group shows an **Ungroup** action.

## Display Options

The display panel (right side, above the settings) controls how the image is rendered. These options do not affect processing:

- **Min / Max Intensity** — clamp the displayed intensity range.
- **Zoom In / Full** — draw a rectangle to zoom; the zoom level persists when navigating between images.
- **Persist Intensities** — keep the current min/max when moving to the next image.
- **Original Center** checkbox — overlays a green circle at the image's current center.
- **Global Base Center** checkbox — overlays a red crosshair at the global base center.
- **Double Zoom** — see [Common Settings — Double Zoom](../Common-Settings.md#double-zoom).

## Image Operations

The **Image Operations** collapsible panel controls grouping and processing options.

#### Grouping Mode

- **Select Group Graphically** — bins are defined manually by selecting rows in the table and right-clicking → *Group*.
- **Bin Images** — reveals a **Binning factor** spinbox (range 2–256, default 2). Setting the factor to *N* automatically creates sequential bins of *N* images each; changing the factor updates the bins immediately. No extra confirmation button is needed.

#### Compute Average Instead of Sum

When checked, each bin produces the pixel-wise average of its images rather than the sum.

#### Compress the Resulting Images

When checked, output files are written with a `_compressed` suffix and reduced file size.

#### Rotation Transform Mode

Controls how each image is rotated before summation:

- **Align to Make Equator Horizontal** — each image is rotated so that its equator is horizontal (absolute rotation correction).
- **Align to Base Image Rotation** — each image is rotated by the difference between its rotation and the global base rotation (relative correction).

## Alignment Panel

The alignment panel (below the image viewer settings) provides tools for detecting and correcting misalignment across the image series.

#### Detect Centers & Rotations

Click this button (collapsible group **Detect Misaligned Images**) to start a batch detection of the diffraction center and rotation angle for every loaded image. Results are written to the *Auto Center* and *Auto Rotation* columns. The button changes to *Stop* during the run; clicking it again cancels the remaining jobs.

Rows whose detected values differ from the global base by more than the configured thresholds (see below) are highlighted.

### Alignment thresholds

Three thresholds live inside the *Detect Misaligned Images* group. Each has a checkbox (to enable/disable the highlight) and a spinbox (the value):

| Threshold | Unit | Default | Highlights rows whose… |
|---|---|---|---|
| **Auto Diff threshold** | px | 5 px | center deviates from the global base by more than this. |
| **Auto-Rot Diff threshold** | ° | 2° | auto-detected rotation differs from the global base by more than this. |
| **Image diff threshold** | (counts) | 80th percentile of all diff scores | pairwise pixel-diff score exceeds this. The default is recomputed automatically every time a new diff is available. |

Pairwise *Image Difference* scores are computed automatically in the background whenever a row's effective center or rotation changes (e.g. after Detect, or after manually setting center/rotation). There is no separate "Compute Image Difference" button — just wait for the column to fill in.

#### Set Center and Rotation Dialog

Opened via right-click → *Set Center and Rotation* or the panel button. The dialog reparents the image viewer, the **Center** group and the **Rotation Angle** group into a side-by-side layout for focused calibration of a single image. The tools inside are the same shared center/rotation controls used by Quadrant Folding, Equator, and Projection Traces — see [Common Settings — Diffraction Center and Rotation](../Common-Settings.md#diffraction-center-and-rotation) for the full description of:

- Quick Center and Rotation Angle, Set Center By Chords / Perpendiculars / Calibration, Set Center Manually
- Set Rotation Angle (interactive and manual)
- Compute Center / Compute Orientation, Fix Center, Restoring Automatic Settings
- Double Zoom for sub-pixel accuracy

Closing the dialog re-parents the viewer and the two groups back to the main window.

##### Apply to Subsequent Images (context-menu action)
Right-click → *Apply to Subsequent Images* propagates the **selected row's** current center and rotation to every row below it in the table. Use it after correcting an image whose center/rotation drifted from the prior images.

## Blank / Mask Settings

AISE uses the standard **Apply Empty Cell Image and Mask** panel — see [Common Settings — Empty Cell Image and Mask](../Common-Settings.md#empty-cell-image-and-mask) for the dialogs and behaviour. In AISE this panel is rendered just above the *Image Operations* group on the right side of the main window.

## Result Tab

Switch to the **Result** tab (top tab bar) after processing to inspect output images. The tab shows a table with four columns — *Filename*, *N Images*, *Total Intensity*, *Date* — listing each result file. Selecting a row displays the corresponding image in the viewer with the same display options as the main view.

## Output files

Results are written to `aise_results/` inside the output directory (the input folder by default; change via **File > Change Output Directory…**):

- `<group>_xxx.tif` — one TIFF per bin. The `_compressed` suffix is appended when **Compress the Resulting Images** is checked.
- `intensities.csv` — per-file statistics, appended on each batch. Columns: `Filename`, `Date`, `Original Image Intensity (Total)`, `Masked Image Intensity (Total)`, `Number of Pixels Not Masked`, `Masked Image Intensity (Average)`, `Blank Image Weight`, `Binning Factor`, `Drawn Mask`, `Computed Mask`.
