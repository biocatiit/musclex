# Introduction

Scanning diffraction imaging experiments attempt to determine the distribution of diffracting materials such as collagen, myelin, and amyloid structures as a function of position in a sample that is raster scanned in a microbeam. The diffracting materials at a given position in the sample will have varying degrees of orientation so that meridional reflections will be spread out into arcs whose angular width depends on the degree of orientation. In the isotropic case, these reflections would form complete rings.  The information that is desired to extract from these images is 1) the total amount of diffraction material which will be proportional to the integrated intensity 2) chemical identity of the diffracting material which can be determined from its d-spacing 3) orientation of the long axis of the diffracting material (which direction the molecules are “pointing”) and 4) degree of disorientation around this action, determined by the angular width of the meridional arcs. A scanning diffraction imaging experiment may produce many thousands of images so there is a need to process these as automatically as possible with little or no human intervention.
The Circular Projection program has been written in order to automatically detect individual diffraction rings and determine the structural parameters for this ring. 

This information includes ...
1.	The distance from center to the ring and the calculated d-spacing
2.	The standard deviation of the ring intensity distribution in the radial direction
3.	The integrated area of the ring
4.	The orientation angle of the ring
5.	The standard deviation of the intensity in the azimuthal direction i.e. the standard deviation of the orientation angle

![-](/images/CP/image_tab_s.png)
![-](/images/CP/batch_vec_s.png)

### More Details
* [How it works](Scanning-Diffraction--How-it-works.html)
* [How to use](Scanning-Diffraction--How-to-use.html)