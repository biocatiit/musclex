# Version-1.4

Release Date : Oct 24, 2017
### Updates
#### 1. Diffraction Centroids
- Convex hull range will be remember once it's set. This range will apply to the next image in the same folder
- Minor Bug Fixes : Zoom in function didn't work properly when the image is changed

#### 2. Circular Projection
- Fixed : application was crashed in some folder 
- Fixed : locations in maps were incorrect when number of images are more than 9999 images
- Fixed : error when a pixel on the maps is clicked, but image not found
- Maps will display information from the best ring instead of average ring. These information will be gathered from rings.csv. (The best ring is the ring model that produces lowest fitting error rate and angle sigma is less than 1 (To prevent picking uniform ring)
- Update summary.csv (remove average ring model and add number of rings)

#### 3. Projection Traces
- Add Convex hull background subtraction option when a box is added
- Now, you can name the box. You will be asked to put the name of the box and choose background subtraction method when a box is added
- Display convex hull range in each box tab
- Convex hull range can be set manually by pressing "Set Convex Hull Range" and select start and end point in the plot
- Add Zoom-in, Zoom-out, and Full Zoom out function in each box tab 
- You can moving around the plot when it's zoomed by dragging
- Add Distance from center at the status bar when the cursor hovers on the plot