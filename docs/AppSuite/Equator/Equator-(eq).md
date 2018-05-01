# Introduction

The purpose of the Equator program is to analyze the equatorial portion of muscle X-ray diffraction patterns. 

The Equator program is designed to:
* Determine the inter filament lattice spacing, d<sub>10</sub>
* Fit Voigt or Gaussian model functions to the diffraction peaks in order to estimate their integrated intensities
* Determine I<sub>11</sub>/I<sub>10</sub> intensity ratios
* Obtain estimates for σ<sub>d</sub> and σ<sub>s</sub> from the peak widths

![screenshot](/images/BM/ss.png)

The program does this with as little user intervention as possible in order to improve reproducibility , reduce operator bias and increase efficiency. It can operate on a whole directory of images and produce results in hours instead of many weeks required for manual processing. Not all patterns are amenable to this approach, however. Any failed cases are flagged for manual processing, either within the Equator program or using other manual approaches. Typically the program succeeds with ~90% of patterns showing diffraction.

### More Details
* [The Equatorial Diffraction Pattern from Striated Muscle](The-Equatorial-Diffraction-Pattern-from-Striated-Muscle.html)
* [How it works](Equator--How-it-works.html)
* [How to use](Equator--How-to-use.html)
* [Results file](Equator--Summary.html)