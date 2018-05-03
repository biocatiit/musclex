# Version-1.6

Release Date : Nov 17, 2017

### Updates
#### 1. Quadrant Folding
- Fixed : Core dump from SIGABRT/SIGSEGV when Circularly Symmetric Background Subtraction is applied, and R-min or R-max is changed to some value
- Added : 2 background subtraction methods : Roving Window and Smoothed (Gaussian/Box car)
- Added : Blank image and mask settings

#### 2. Bio-musclex
- Added : Blank image and mask settings

#### 3. Circular Projection
- Added : Create a HDF file option for batch mode. When a folder is selected, the program will ask you to select a HDF file, but if you do not have the HDF file, you can create a new one by specify start and point of x and y along with their step size <br/>[[/images/CP/create_HDF.png]]
