import musclexflibs.ccp13.ccp13 as ccp13
# import ccp13
import matplotlib.pyplot as plt
import numpy as np
import fabio
from PyQt4 import QtGui
from tifffile import imsave
import os, sys
print ccp13.bckwin.__doc__

img_path = os.path.split(os.path.realpath(__file__))[0] + "/test_images/July10_148.tif.result.tif"
img = fabio.open(img_path).data
width = img.shape[1]
height = img.shape[0]

img = np.ravel(img)
buf = np.array(img, 'f')
b = np.zeros(len(buf), 'f')
iwid=10
jwid=10
isep=10
jsep=10
maxdim = width*height
maxwin = (iwid*2+1)*(jwid*2+1)
#
# ccp13.bgwsrt2(buf=buf,
#               b=b,
#               iwid=iwid,
#               jwid=jwid,
#               isep=isep,
#               jsep=jsep,
#               smoo=0.0,
#               tens=0.0,
#               pc1=0.05,
#               pc2=0.15,
#               npix=width,
#               nrast=height,
#               maxdim=maxdim,
#               maxwin=maxwin,
#               xb=np.zeros(maxdim, 'f'),
#               yb=np.zeros(maxdim, 'f'),
#               ys=np.zeros(maxdim, 'f'),
#               ysp=np.zeros(maxdim, 'f'),
#               wrk=np.zeros(9*maxdim, 'f'),
#               bw=np.zeros(maxwin, 'f'),
#               index_bn=np.zeros(maxwin, 'i'),
#               iprint=0,
#               ilog=1,
# )
#
# before = np.array(buf, 'float32')
# before = before.reshape((height,width))
# imsave("img_win.tif", before)
#
# after = np.array(b, 'float32')
# after = after.reshape((height,width))
# imsave("bg_win.tif", after)
#
# result = before-after
# imsave("result_win.tif", result)
