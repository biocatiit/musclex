# Introduction

Add Intensities Multiple Experiments (AIME) is designed for experiments where corresponding frames from multiple separate folders or HDF5 files need to be summed or averaged together. Each group consists of one frame from each loaded experiment (matched by frame index), and the resulting images are saved to an `aime_results` folder in the selected output directory.

The program provides a table-driven workflow for inspecting and aligning images across experiments before processing. Each image's diffraction center and rotation can be detected automatically or set manually, and individual images can be excluded from summation.

### More Details
* [How to use](Add-Intensities-ME-How-to-use.md)
