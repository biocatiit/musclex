"""
Copyright 1999 Illinois Institute of Technology

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL ILLINOIS INSTITUTE OF TECHNOLOGY BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of Illinois Institute
of Technology shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from Illinois Institute of Technology.
"""

import fabio
import numpy as np
from ..utils.file_manager import fullPath, ifHdfReadConvertless
from ..utils.image_processor import *

class XRayViewer:
    """
    A class for Quadrant Folding processing - go to process() to see all processing steps
    """
    def __init__(self, img_path, img_name, file_list=None, extension=''):
        """
        Initial value for QuadrantFolder object
        :param img_path: directory path of input image
        :param img_name: image file name
        """
        self.img_name = img_name
        if extension in ('.hdf5', '.h5'):
            index = next((i for i, item in enumerate(file_list[0]) if item == img_name), 0)
            self.orig_img = file_list[1][index]
        else:
            self.orig_img = fabio.open(fullPath(img_path, img_name)).data
        self.orig_img = ifHdfReadConvertless(img_name, self.orig_img)
        self.orig_img = self.orig_img.astype("float32")
        self.orig_image_center = None
        self.hist = []
        self.dl, self.db = 0, 0
        if self.orig_img.shape == (1043, 981):
            self.img_type = "PILATUS"
        else:
            self.img_type = "NORMAL"

    def getRotatedImage(self, angle, center):
        """
        Get rotated image by angle while image = original input image, and angle = self.info["rotationAngle"]
        """
        img = np.array(self.orig_img, dtype="float32")
        b, l = img.shape
        rotImg, _, _ = rotateImage(img, center, angle, self.img_type, -999)

        # Cropping off the surrounding part since we had already expanded the image to maximum possible extent in centerize image
        bnew, lnew = rotImg.shape
        db, dl = (bnew - b)//2, (lnew-l)//2
        final_rotImg = rotImg[db:bnew-db, dl:lnew-dl]
        self.dl, self.db = dl, db # storing the cropped off section to recalculate coordinates when manual center is given

        return final_rotImg
