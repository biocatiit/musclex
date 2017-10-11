import musclexflibs.ccp13.ccp13 as ccp13
# import ccp13
import matplotlib.pyplot as plt
import numpy as np
import fabio
from tifffile import imsave
import os
# print ccp13.__doc__

img_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images/July10_148.tif.result.tif"
img = fabio.open(img_path).data
width = img.shape[1]
height = img.shape[0]

ad = np.ravel(img)
ad = np.array(ad, 'f')
b = np.array(ad, 'f')
rmin = 95.0
rmax = 998.0
bin_size = 10.
max_bin = int(np.ceil((rmax - rmin) / bin_size))
max_num = int(np.ceil(rmax * 2 * np.pi))

ccp13.bgcsym2(ad=ad, b=b,
             smoo=20,
             tens=10,
             pc1=0.05,
             pc2=0.15,
             npix=width,
             nrast=height,
             dmin=rmin,
             dmax=rmax,
             xc=width/2.,
             yc=height/2.,
             dinc=bin_size,
             csyb=np.zeros(max_bin, 'f'),
             csyd=np.zeros(max_bin, 'f'),
             ys=np.zeros(max_bin, 'f'),
             ysp=np.zeros(max_bin, 'f'),
             wrk=np.zeros(max_bin*9, 'f'),
             pixbin=np.zeros(max_num, 'f'),
             index_bn=np.zeros(max_num, 'f'),
             iprint=0,
             ilog=1,
)


before = np.array(ad, 'float32')
before = before.reshape((height,width))
imsave("img.tif", before)

after = np.array(b, 'float32')
after = after.reshape((height,width))
imsave("bg_cir.tif", after)

result = before-after
imsave("result_cir.tif", result)