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

import musclex
import fabio
from os.path import exists, isfile
import pickle
from musclex.bio_utils.file_manager import fullPath
from musclex.bio_utils.image_processor import getCenter, getRotationAngle, rotateImage, getMaskThreshold, display_test
import copy
import numpy as np

class LayerLineProcessor():
    def __init__(self, dir_path, file_name):
        self.dir_path = dir_path
        self.filename = file_name
        self.orig_img = fabio.open(fullPath(dir_path, file_name)).data
        self.version = musclex.__version__
        cache = self.loadCache()
        if cache is None:
            # info dictionary will save all results
            # info dictionary will save all results
            self.info = {
                "mask_thres": getMaskThreshold(self.orig_img)
            }
        else:
            self.info = cache

        self.rotated_img = None

    def process(self, settings = {}):
        """
        All processing steps - all settings are provided by Layer Line Traces app as a dictionary
        """
        self.info.update(settings)
        self.findCenter()
        self.getRotationAngle()
        self.getHistograms()
        # self.fitModel()

    def findCenter(self):
        """
       Find center of the diffraction. The center will be kept in self.info["center"].
       Once the center is calculated, the rotation angle will be re-calculated, so self.info["rotationAngle"] is deleted
       """
        print "Center is being calculated..."
        if not self.info.has_key('center'):
            self.info['center'] = getCenter(self.orig_img)
            self.removeInfo('rotationAngle') # Remove rotationAngle from info dict to make it be re-calculated
        print "Done. Center is", self.info['center']

    def getRotationAngle(self):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal. The angle will be kept in self.info["rotationAngle"]
        Once the rotation angle is calculated, the rmin will be re-calculated, so self.info["rmin"] is deleted
        """
        print "Rotation Angle is being calculated..."
        if not self.info.has_key('rotationAngle'):
            if self.info.has_key("fixed_angle"):
                self.info['rotationAngle'] = self.info["fixed_angle"]
            else:
                center = self.info['center']
                img = copy.copy(self.orig_img)
                self.info['rotationAngle'] = getRotationAngle(img, center)
            self.removeInfo('rmin')  # Remove R-min from info dict to make it be re-calculated

        print "Done. Rotation Angle is", self.info['rotationAngle']

    def getRotatedImage(self, img=None, angle=None):
        """
        Get rotated image by angle. If the input params are not specified. image = original input image, angle = self.info["rotationAngle"]
        :param img: input image
        :param angle: rotation angle
        :return: rotated image
        """

        if img is None:
            img = copy.copy(self.orig_img)
        if angle is None:
            angle = self.info['rotationAngle']

        if self.rotated_img is None or self.rotated_img[0] != self.info["center"] or self.rotated_img[1] != self.info["rotationAngle"] or (self.rotated_img[2] != img).any():
            # encapsulate rotated image for using later as a list of [center, angle, original image, rotated image[
            self.rotated_img = [self.info["center"], angle, img,
                                rotateImage(img, self.info["center"], angle, self.info['mask_thres'])]

        return self.rotated_img[3]

    def getHistograms(self):
        if not self.info.has_key('hists') and self.info.has_key('boxes') and len(self.info['boxes']) > 0:
            boxes = self.info['boxes']
            img = self.getRotatedImage()
            hists = []
            for i,b in enumerate(boxes):
                x1 = b[0][0]
                y1 = b[0][1]
                x2 = b[0][0]+b[1][0]
                y2 = b[0][1]+b[1][1]
                area = img[y1:y2+1, x1:x2+1]
                hists.append(np.sum(area, axis=0))
            self.info['hists'] = hists


    def removeInfo(self, k=None):
        """
        Remove information from info dictionary by k as a key. If k is None, remove all information in the dictionary
        :param k: key of dictionary
        :return: -
        """
        if k is None:
            for k in self.info.keys():
                del self.info[k]
        else:
            if self.info.has_key(k): # remove from dictionary if the key exists
                del self.info[k]

    def loadCache(self):
        """
        Load info dict from cache. Cache file will be filename.info in folder "bm_cache"
        :return: cached info (dict)
        """
        cache_path = fullPath(self.dir_path, "ll_cache")
        cache_file = fullPath(cache_path, self.filename + '.info')

        if exists(cache_path) and isfile(cache_file):
            cinfo = pickle.load(open(cache_file, "rb"))
            if cinfo != None:
                if cinfo['program_version'] == self.version:
                    return cinfo
        return None
