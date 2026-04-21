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

import numpy as np

try:
    from ..utils.image_processor import *
    from ..utils.image_data import ImageData
except:  # for coverage
    from utils.image_processor import *
    from utils.image_data import ImageData


class XRayViewer:
    """
    A class for Quadrant Folding processing - go to process() to see all processing steps
    """
    def __init__(self, img, img_path=None, img_name=None):
        """
        Initialize viewer with an already-loaded image array.
        :param img: numpy ndarray image data
        :param img_path: directory containing the image (for TIFF metadata lookup)
        :param img_name: image filename (used to detect folded images)
        """
        self.orig_img = np.asarray(img).astype("float32")
        self.orig_image_center = None
        self.hist = []
        self.dl, self.db = 0, 0

        self.info = {}

        # Auto-detect quadrant-folded state. GUI may override via set_folded().
        self.is_folded = ImageData.detect_folded(img_path, img_name)

    def set_folded(self, is_folded):
        """Toggle quadrant-folded state. Forces center recomputation on next findCenter()."""
        self.is_folded = bool(is_folded)
        # Drop any cached center so findCenter() picks the correct one
        if 'center' in self.info:
            del self.info['center']
        self.orig_image_center = None

    def _geometric_center(self):
        h, w = self.orig_img.shape
        return (w / 2.0 - 0.5, h / 2.0 - 0.5)

    def getRotatedImage(self, angle, center):
        """
        Get rotated image by angle while image = original input image, and angle = self.info["rotationAngle"]
        """
        img = np.array(self.orig_img, dtype="float32")
        b, l = img.shape
        rotImg, _, _ = rotateImage(img, center, angle)

        # Cropping off the surrounding part since we had already expanded the image to maximum possible extent in centerize image
        bnew, lnew = rotImg.shape
        db, dl = (bnew - b)//2, (lnew-l)//2
        final_rotImg = rotImg[db:bnew-db, dl:lnew-dl]
        self.dl, self.db = dl, db # storing the cropped off section to recalculate coordinates when manual center is given

        return final_rotImg

    def findCenter(self):
        if 'center' in self.info:
            return
        # Quadrant-folded images are symmetric around the geometric center by
        # construction; auto-detection via getCenter() is unnecessary and less
        # accurate than just using the geometric center.
        if self.is_folded:
            self.info['center'] = self._geometric_center()
            self.orig_image_center = self.info['center']
            print("Folded image detected, using geometric center = " + str(self.info['center']))
            return
        print("Center is being calculated ... ")
        self.orig_image_center = getCenter(self.orig_img)
        self.orig_img, self.info['center'] = processImageForIntCenter(self.orig_img, self.orig_image_center)
        print("Done. Center = "+str(self.info['center']))