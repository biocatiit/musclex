# How it works

When an image is selected, if the image has ever been processed with the same version of the program, the cache will be downloaded, so the image won’t be processed again. Users need to specify boxes for the desired integration area and initial peak positions to trigger image processing. If the boxes and peaks are specified for one image in a folder, all other images in the same folder will be processed using these boxes and peak positions.

## Image processing
The program will process an image by going through a series of steps in the following order. (This assumes that the boxes and peaks have been specified already)
### 1. Obtain the 1-D Projection from a box
In this process users just sum all pixel intensities vertically or horizontally depends on the shape of the box 

![-](/images/PT/1dproj.png)
### 2. Fit Model
In this example, the intensity is integrated along a layer line and projected onto a line perpendicular to the meridian. The sharp peak in the center is a meridional reflection. To fit the model, you need to specify the approximate peak locations. This model will try to fit a Gaussian model to every peak and other 3 Gaussians at the center. These 3 Gaussians at the center are overall background, meridian background, and meridian peak. Therefore, the results will be as in the intensity trace shown below. The overall background is the blue area, the background under the meridian is the yellow area and the meridional peak is the red area. The green line is the resulting fit summing all peak Gaussians while the red lines are the positions of the layer line peak centers after fitting.

![-](/images/PT/model.png)
### 3. Get Background Subtracted Projection
We try to remove all background and center peak, so in this process, the program will remove them by using 3 Gaussians from previous step.

Basically, the program will just subtract the 3 center Gaussians (colored) from the original projection (black line).

![-](/images/PT/subtract1.png)

Then, the result is

![-](/images/PT/subtract2.png)

### 4. Calculate all peak information
The main objective of this process is to find centroids of all peaks. In order to calculate peaks' centroids, the program needs to ...
1. Calculate baseline of each peak. By default, the baseline is the half-height of the peak, but you can change it manually later. 
2. Find the left and right intersection locations around the peak (black) and its baseline (yellow) as below
 <br/>![-](/images/PT/baseline.png)<br/>
3. Calculate the centroid by calculate the dot product of x (distance from center) and y (projected intensity) in range of the left and right intersections. Then, divide the result by the sum of y (projected intensity) along the range.
