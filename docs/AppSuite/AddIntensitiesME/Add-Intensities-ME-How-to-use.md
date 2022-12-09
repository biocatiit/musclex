# How to use

Once the program runs, you will have to select a folder containing multiple rounds or images from different experiments (it goes from 2 to 8 experiments simultaneously). The program will automatically, based on the files names, select the first image of each experiment. You will see a new window as below:

![-](../../images/AIME/aime_images.png)

Auto grouping is performed automatically and the images you see on the screen are directly processed and saved into `aime_results`.

* Select an input directory by pressing "Select Folder"

* What is displayed has been processed and is accessible in the `aime_results` folder. You can see the result in the 'Result' tab.
![-](../../images/AIME/aime_result.png)

* You can change the display options in order to have visually better images (won't affect the true result).

* You can also change the processing options: the number of exposures you want to add simultaneously, averaging the images instead of summing them, calibrating the common center and the rotation of each image (can be done automatically or manually for each image).

```eval_rst
.. note:: After clicking on a calibration button, you need to select an image by clicking on it before using the calibration function on this image.
```

* Use the arrows to go from one group of exposure to the other and process the results groups by groups, use the spinbox `Images #` to jump to a specific group of exposures, or click on `Process Current Folder` to process everything.

