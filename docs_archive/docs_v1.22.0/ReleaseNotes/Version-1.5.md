# Version-1.5

Release Date : Nov 8, 2017
### Updates
#### 1. All Packages
- Support both PyQt4 and PyQt5 (Python2)
- Fixed: warning from matplotlib due to future deprecation
- Fixed: warning from pandas due to future deprecation

#### 2. Projection Traces
- Fixed : Convex hull background area was not shown properly when there're some minus values

#### 3. Circular Projection
- Added : fixed R-min and R-max settings to image tab
- Apply fixed R-min and R-max to the next image when it's set
- Introducing new feature : Blank image and Mask button which allows user to select black image for background subtraction and generate mask for pyFAI azimuthal integration.

#### 4. Diffraction Centroids
- Remember zoom area for top and bottom diffraction when the image is changed (Reverted from 1.4)
- Remember start and end points for convex hull background subtraction for top and bottom diffraction when the image is changed