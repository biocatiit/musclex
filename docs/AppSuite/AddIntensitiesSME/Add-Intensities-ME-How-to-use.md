# How to use

When the program opens, a **Select Experiments** panel is shown. Use it to choose the experiments to load:

1. Click **Add Folder…** to pick a parent directory containing your experiment subdirectories and/or HDF5 files. You can click this button multiple times to accumulate experiments from several different parent directories; **Clear List** removes everything from the available list and lets you start over.
2. The list of available experiments (subdirectories and `.h5`/`.hdf5` files found in the parent folder) is populated automatically. System folders such as `aime_results`, `aise_results`, and `calibration` are excluded.
3. Select the experiments to include using Ctrl/Shift-click, or use **Select All** / **Select None**.
4. Click **Load** to build the alignment table.

Once loaded, the program switches to the main table view. The output folder can be changed at any time via **File > Change Output Directory…**.

## Workflow Overview

A **Workflow Guide** dialog is shown on first launch (and can be reopened at any time via the **Workflow Guide** button in the top bar). The recommended steps are:

1. **Browse and select experiments** — click *Add Folder…* to choose a parent directory, select one or more experiments from the list, then click *Load*. Repeat *Add Folder…* if you want experiments from more than one parent directory.
2. **Set the global (reference) image** — by default the first image is the reference. Right-click a row and choose *Set as Global Base* to change it.
3. **Detect misalignment** — click *Detect Centers & Rotations* in the alignment panel. The table highlights rows whose center distance or rotation difference exceeds the configured thresholds. Pairwise pixel-level *Image Difference* scores are filled in automatically as a side effect (see [Alignment thresholds](#alignment-thresholds)).
4. **Correct misaligned images** — select a misaligned row and adjust its center/rotation. Right-click → *Apply to Subsequent Images* for progressive drift, or *Ignore* to exclude an image entirely.
5. **Choose a table view** — switch between **Group by Index** and **Group by Exp** using the top tab bar. This is purely a re-ordering of rows in the table; summing always produces one output file per frame index regardless of the view.
6. **Sum Images** — choose *Average* or *Sum* in *Image Operations*, then click **Sum Images by Index**.
7. **Inspect results** — switch to the **Result** tab to browse and preview the output images.

## Table View

The alignment table lists every image across all loaded experiments. Each row corresponds to one image from one experiment. The table has two view modes, selectable via the top tab bar. **The two views only change how rows are sorted in the table**; the underlying summation always groups frames by index (frame *i* from every experiment goes into output file *i*).

- **Group by Index** — rows are sorted by frame index (one group per frame number, containing one row per experiment). Best for spot-checking that the same index across experiments is aligned.
- **Group by Exp** — rows are sorted by experiment, showing all frames within each experiment contiguously. Best for following drift within a single experiment.

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
- **Double Zoom** — see [Common Settings — Double Zoom](../Common-Settings.md#double-zoom).

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

AIME uses the standard **Apply Empty Cell Image and Mask** panel — see [Common Settings — Empty Cell Image and Mask](../Common-Settings.md#empty-cell-image-and-mask) for the dialogs and behaviour. In AIME this panel is rendered just above the *Image Operations* group on the right side of the main window.

## Summing Images

Click **Sum Images by Index** to start processing. Before the batch begins, a dialog prompts for an **output base name**. The program suggests a common prefix derived from the loaded image filenames; you can accept or change it. Output files are named `<base>_00001.tif`, `<base>_00002.tif`, etc., one per frame index group.

Images marked as *Ignored* are excluded from their group. If all images in a group are ignored, that group is skipped. The button changes to **Stop** during processing; clicking it again cancels the remaining jobs.

## File Format

For HDF5 sources, files must be in the same parent folder and their names must contain a number and the word `data`, with elements separated by `_`. For example: `P2_F5_849_1_094_data_000001.h5`.

For folders containing TIFF images, the last underscore-separated number in the filename is used as the frame index, and the preceding part identifies the experiment. For example, in `P2_F5_849_1_094_data_000001.tif`, the frame index is `000001` and the experiment group is `P2_F5_849_1_094_data`.

## Result Tab

Switch to the **Result** tab (top tab bar) after processing to inspect output images. The tab shows a table with four columns — *Filename*, *N Images*, *Total Intensity*, *Date* — listing each result file. Selecting a row displays the corresponding image in the viewer with the same display options as the main view.

## Output files

Results are written to `aime_results/` inside the output directory (the first parent directory by default; change via **File > Change Output Directory…**):

- `<base>_xxxxx.tif` — one TIFF per frame index group. The `_compressed` suffix is appended when **Compress the Resulting Images** is checked.
- `intensities.csv` — per-file statistics, appended on each batch. Columns: `Filename`, `Date`, `Original Image Intensity (Total)`, `Masked Image Intensity (Total)`, `Number of Pixels Not Masked`, `Masked Image Intensity (Average)`, `Blank Image Weight`, `Binning Factor`, `Drawn Mask`, `Computed Mask`.
