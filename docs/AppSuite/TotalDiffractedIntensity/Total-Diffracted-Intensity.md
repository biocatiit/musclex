# Total Diffracted Intensity



## Introduction

The Total Diffracted Intensity (TDI) tool is part of the [MuscleX](https://github.com/biocatiit/musclex) suite developed for analyzing X-ray diffraction images of biological samples, such as muscle fibers. In diffraction experiments, total diffraction intensity is a key quantitative measure — it reflects the overall scattering power of the sample, which is proportional to properties like sample mass, density, or structural order.

This tool allows users to:

- Define and apply custom masks to isolate regions of interest (e.g., remove background, beamstop artifacts).
- Compute and export total and average intensity values from each image in a folder for downstream analysis.

These metrics are valuable for comparing conditions (e.g., relaxed vs. contracted muscle), normalizing datasets, or identifying outliers during high-throughput imaging workflows.




## How to Use

### Launching the Program

- If MuscleX is installed:

  ```bash
  musclex tdi
  ```

- From source (developer mode):

  ```bash
  python3 -m musclex.ui.TotalDiffractionIntensity
  ```

> Required Python packages: `PyQt5`, `matplotlib`, `fabio`, `numpy`, `pandas`.



### User Interface Overview

The main TDI window (see {numref}`fig-tdi-window`) provides controls for loading images, adjusting display settings, masking, and batch processing.

```{figure} ../../images/TDI/tdi-window.png
:name: fig-tdi-window
:alt: TDI main window
TDI main window showing the image viewer and controls.
```

1. **Select an Image** — Click "Select an Image..." to open a diffraction image (TIFF or HDF5). The image appears in grayscale using the default intensity range.
2. **Set Intensity Display Range** — Adjust the Min intensity and Max intensity values to control the display range. This helps highlight weaker features or saturate bright spots for better contrast.
3. **Mask the Image** — Click "Mask The Image" to launch the interactive masking tool (see [Mask Definition and Usage](#mask-definition-and-usage)). You can define regions to exclude from analysis (e.g., noisy borders). The mask is saved as `tdi_mask.tif` and applied to all images.
4. **Navigate Images** — Use the `<<<` and `>>>` buttons to browse through images in the same folder.
5. **Process Folder** — Click "Process Folder" to compute and export results for all images. A CSV file containing total and average intensity values is generated in the `tdi_results/` subfolder.



## How It Works

### Image and Mask Loading

The program reads image data using the `fabio` library. If a mask is defined, it is applied as a binary filter (`1` = valid, `0` = excluded).

### Total Intensity Calculation

The image is multiplied by the mask to zero out unwanted pixels, and the pixel values in the valid region are summed:

```python
total_intensity = np.sum(masked_image)
```

### Average Intensity Calculation

The number of valid pixels is determined by subtracting the count of masked pixels. Average intensity is the total intensity divided by the number of unmasked pixels:

```python
unmasked_pixels = masked_image.size - np.sum(1 - mask)
avg_intensity = total_intensity / unmasked_pixels
```

### Export

Results are saved to `tdi_results/summary.csv` with the following columns:

| Column | Description |
|---|---|
| `ImageName` | Image filename |
| `MaskFileName` | Typically `tdi_mask.tif` |
| `TotalIntensity` | Sum of all unmasked pixel values |
| `AvgIntensity` | Average value over unmasked pixels |



## Mask Definition and Usage

### Overview

The Mask and Empty Cell Specification window provides a unified interface for defining masks that exclude undesired regions from X-ray diffraction images. It supports manual drawing, intensity-based masking, radial cropping, and subtraction of empty cell (blank) images. These masks ensure cleaner data and more reliable intensity quantification.



### Masking Window Features

The default mask window is shown in {numref}`fig-tdi-mask-window`.

```{figure} ../../images/TDI/mask-window.png
:name: fig-tdi-mask-window
:alt: Default mask configuration window
Default mask configuration window with no thresholds applied.
```

| Feature | Description |
|---|---|
| **Select Empty Cell Image(s)** | Load one or more background/blank images to subtract static artifacts like beamstops |
| **Draw Mask** | Open a drawing interface to manually exclude regions using polygon, rectangle, or brush |
| **Low Mask Threshold** | Excludes pixels below this intensity (e.g. detector gaps, shadowed areas) |
| **High Mask Threshold** | Excludes pixels above this intensity (e.g. beam center, saturation artifacts) |
| **Enable Mask Dilation** | Optional. Expands the mask area around low/high threshold regions using morphological dilation |
| **Rmin / Rmax** | Masks all pixels outside the selected radial range from the image center |
| **Subtract Empty Cell Image** | Applies scaled subtraction of selected empty cell image(s) before applying thresholds |
| **Clamp Negative Values to 0** | Replaces negative values after subtraction with 0 |
| **Show Image With Mask** | Dropdown view to preview the mask overlay with current settings applied |



### Mask Visualizations

Color coding in the preview overlay:

- **Green** = Low threshold mask
- **Blue** = High threshold mask
- **Red** = Manually drawn mask
- **Purple** = Rmin/Rmax radial mask

{numref}`fig-tdi-mask-thresholds` shows an example with both low and high threshold masks and a manually drawn region applied. {numref}`fig-tdi-mask-full` shows the result when dilation and radial masking are also enabled.

```{figure} ../../images/TDI/mask-with-low-and-high-thresholds.png
:name: fig-tdi-mask-thresholds
:alt: Low and high threshold mask with manually drawn region
Low and high threshold masks with a manually drawn exclusion region overlaid.
```

```{figure} ../../images/TDI/mask-with-rmin-rmax.png
:name: fig-tdi-mask-full
:alt: Combined mask with thresholds, dilation, and radial bounds
Full combined mask: intensity thresholds, dilation, and Rmin/Rmax radial mask applied simultaneously.
```

The manual mask drawing interface (see {numref}`fig-tdi-drawmask`) is launched by clicking **Draw Mask** and uses the pyFAI drawmask tool.

```{figure} ../../images/TDI/pyfai-drawmask.png
:name: fig-tdi-drawmask
:alt: pyFAI manual mask drawing interface
Manual mask drawing interface (pyFAI drawmask tool).
```



### Example Workflow

1. Open TDI and click **Mask The Image**.
2. In the mask window:
   - Optionally select a blank image for subtraction.
   - Enable **Low** and/or **High Mask Threshold** and set values.
   - (Optional) Check **Enable Mask Dilation** for either threshold.
   - (Optional) Enable **Rmin/Rmax** and specify radial bounds.
   - (Optional) Click **Draw Mask** to manually exclude areas.
3. Use **Show Image With Mask** to inspect the result.
4. Click **Save** to generate and store the combined mask.



### How Masks Are Combined

After configuration, the following masks are combined via binary AND:

```python
final_mask = (
    manual_mask *
    low_threshold_mask *
    high_threshold_mask *
    low_dilation_mask *
    high_dilation_mask *
    radial_mask *
    blank_subtraction_mask
)
```

This final mask is applied in downstream image analysis and intensity extraction.



## Developer Reference

| Component | File | Description |
|---|---|---|
| TDI integration | `musclex/ui/TotalDiffractionIntensity.py` | `maskButtonClicked()` launches the tool; `buildMask()` loads masks |
| Masking UI & logic | `musclex/ui/ImageMaskTool.py` | `ImageMaskerWindow` handles threshold sliders, dilation checkboxes, Rmin/Rmax, subtraction logic |
| Manual mask drawing | Called via **Draw Mask** (pyFAI drawmask tool) | |
| File handling | `musclex/utils/file_manager.py` | `getBlankImageAndMask`, `getMaskOnly` parse files from disk |
