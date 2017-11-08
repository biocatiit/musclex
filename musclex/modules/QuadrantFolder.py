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

import os
import fabio
import pickle
# import pyFAI
from scipy.ndimage.filters import gaussian_filter, convolve1d
from ..utils.file_manager import fullPath, createFolder
from ..utils.histogram_processor import *
from ..utils.image_processor import *
from skimage.morphology import white_tophat, disk
from tifffile import imsave
from scipy.interpolate import UnivariateSpline, PchipInterpolator
import musclex
import ccp13

# Make sure the cython part is compiled
# from subprocess import call
# call(["python setup2.py build_ext --inplace"], shell = True)

import musclex.modules.QF_utilities as qfu

class QuadrantFolder(object):
    """
    A class for Quadrant Folding processing - go to process() to see all processing steps
    """
    def __init__(self, img_path, img_name):
        """
        Initial value for QuadrantFolder object
        :param img_path: directory path of input image
        :param img_name: image file name
        """
        self.orig_img = fabio.open(fullPath(img_path, img_name)).data
        self.empty = False
        self.img_path = img_path
        self.img_name = img_name
        self.imgCache = {} # displayed images will be saved in this param
        self.ignoreFolds = set()
        self.version = musclex.__version__
        cache = self.loadCache() # load from cache if it's available

        # info dictionary will save all results
        if cache is not None:
            self.info = cache
        else:
            self.info = {
                'imgType' : str(self.orig_img.dtype)
            }

    def saveBackground(self):
        result = self.imgCache["BgSubFold"]
        avg_fold = self.info["avg_fold"]
        background = avg_fold-result
        fold_height = avg_fold.shape[0]
        fold_width = avg_fold.shape[1]

        top_left = background
        top_right = cv2.flip(background, 1)
        buttom_left = cv2.flip(background, 0)
        buttom_right = cv2.flip(buttom_left, 1)

        resultImg = np.zeros((fold_height * 2, fold_width * 2))
        resultImg[0:fold_height, 0:fold_width] = top_left
        resultImg[0:fold_height, fold_width:fold_width * 2] = top_right
        resultImg[fold_height:fold_height * 2, 0:fold_width] = buttom_left
        resultImg[fold_height:fold_height * 2, fold_width:fold_width * 2] = buttom_right

        result_path = fullPath(fullPath(self.img_path, "qf_results/bg"), self.img_name + ".bg.tif")
        createFolder(fullPath(self.img_path, "qf_results/bg"))
        resultImg = resultImg.astype("float32")
        imsave(result_path, resultImg)

    def cacheInfo(self):
        """
        Save info dict to cache. Cache file will be save as filename.info in folder "qf_cache"
        :return: -
        """
        cache_file = fullPath(fullPath(self.img_path, "qf_cache"), self.img_name + ".info")
        createFolder(fullPath(self.img_path, "qf_cache"))
        self.info['program_version'] = self.version
        pickle.dump(self.info, open(cache_file, "wb"))

    def loadCache(self):
        """
        Load info dict from cache. Cache file will be filename.info in folder "qf_cache"
        :return: cached info (dict)
        """
        cache_file = fullPath(fullPath(self.img_path, "qf_cache"), self.img_name+".info")
        if os.path.isfile(cache_file):
            info = pickle.load(open(cache_file, "rb"))
            if info != None:
                if info['program_version'] == self.version:
                    return info
                    # return None
        return None

    def deleteFromDict(self, dict, delStr):
        """
        Delete a key and value from dictionary
        :param dict: input dictionary
        :param delStr: deleting key
        :return: -
        """
        if delStr in dict.keys():
            del dict[delStr]

    def process(self, flags):
        """
        All processing steps - all flags are provided by Quadrant Folding app as a dictionary
        settings must have ...
        ignore_folds - ignored quadrant = quadrant that will not be averaged
        bgsub - background subtraction method (-1 = no bg sub, 0 = Circular, 1 = 2D convex hull, 2 = white-top-hat)
        mask_thres - pixel value that won't be averaged (deplicated)
        sigmoid - merging gradient
        other backgound subtraction params - cirmin, cirmax, nbins, tophat1, tophat2
        """
        print self.img_name+" is being processed..."
        self.info.update(flags)
        self.initParams()
        self.findCenter()
        self.rotateImg()
        self.calculateAvgFold()
        self.getRminmax()
        self.applyBackgroundSubtraction()
        self.mergeImages()
        self.generateResultImage()

        if not flags.has_key("no_cache"):
            self.cacheInfo()
            self.saveBackground()

    def initParams(self):
        """
        Initial some parameters in case GUI doesn't specified
        """
        if not self.info.has_key('mask_thres'):
            self.info['mask_thres'] = getMaskThreshold(self.orig_img)
        if not self.info.has_key('ignore_folds'):
            self.info['ignore_folds'] = set()
        if not self.info.has_key('bgsub'):
            self.info['bgsub'] = 0
        if not self.info.has_key('sigmoid'):
            self.info['sigmoid'] = 0.05


    def findCenter(self):
        """
       Find center of the diffraction. The center will be kept in self.info["center"].
       Once the center is calculated, the rotation angle will be re-calculated, so self.info["rotationAngle"] is deleted
       """
        if 'center' in self.info.keys():
            return
        print "Center is being calculated ... "
        self.info['center'] = getCenter(self.orig_img)
        self.deleteFromDict(self.info, 'rotationAngle')
        print "Done. Center =", self.info['center']


    def rotateImg(self):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal. The angle will be kept in self.info["rotationAngle"]
        Once the rotation angle is calculated, the average fold will be re-calculated, so self.info["avg_fold"] is deleted
        """
        if not self.empty and 'rotationAngle' not in self.info.keys():
            print "Rotation Angle is being calculated ... "
            center = self.info['center']
            img = copy.copy(self.orig_img)
            self.info['rotationAngle'] = getRotationAngle(img, center)
            self.deleteFromDict(self.info, 'avg_fold')
            print "Done. Rotation Angle is", self.info['rotationAngle'],"degree"

        self.imgCache['rotateImg'] = copy.copy(self.getRotatedImage())

    def getRotatedImage(self):
        """
        Get rotated image by angle while image = original input image, and angle = self.info["rotationAngle"]
        """
        img = np.array(self.orig_img, dtype="float32")
        rotate_img = rotateImage(img,self.info["center"], self.info["rotationAngle"], self.info['mask_thres'])
        return rotate_img

    def getFoldNumber(self, x, y):
        """
        Get quadrant number by coordinates x, y (top left = 0, top right = 1, bottom left = 2, bottom right = 3)
        :param x: x coordinate
        :param y: y coordinate
        :return: coordinate number
        """
        center = self.info['center']
        center_x = center[0]
        center_y = center[1]

        if x < center_x and y < center_y:
            return 0
        if x >= center_x and y < center_y:
            return 1
        if x < center_x and y >= center_y:
            return 2
        if x >= center_x and y >= center_y:
            return 3

    def applyAngularBGSub(self):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        copy_img = copy.copy(self.info['avg_fold'])
        center = [copy_img.shape[1]-1, copy_img.shape[0]-1]
        npt_rad = int(distance(center,(0,0)))

        ai = pyFAI.AzimuthalIntegrator(detector="agilent_titan")
        ai.setFit2D(100, center[0], center[1])
        mask = np.zeros((copy_img.shape[0], copy_img.shape[1]))

        start_p = self.info["cirmin"] # minimum value of circular background subtraction pixel range in percent
        end_p = self.info["cirmax"] # maximum value of circular background subtraction pixel range in percent
        rmin = self.info["rmin"] # minimum radius for background subtraction
        rmax = self.info["rmax"] # maximum radius for background subtraction
        theta_size = self.info["bin_theta"] # bin size in degree
        nBins = 90/theta_size

        I2D = []
        for deg in range(180, 271):
            x, I = ai.integrate1d(copy_img, npt_rad, mask=mask, unit="r_mm", azimuth_range=(deg, deg+1))
            I2D.append(I)

        I2D = np.array(I2D)

        sub_tr = []
        for i in range(nBins):
            # loop in each theta range
            subr = []
            theta1 = i * theta_size
            theta2 = (i+1) * theta_size
            if i+1 == nBins:
                theta2 += 1

            for r in range(0, I2D.shape[1]):
                # Get azimuth line on each radius (in theta range)
                rad = I2D[theta1:theta2,r]

                if start_p == end_p:
                    percentile = int(round(start_p * len(rad) / 100.))
                    rad = np.array(sorted(rad)[percentile: percentile+1])
                else:
                    s = int(round(start_p * len(rad) / 100.))
                    e = int(round(end_p * len(rad) / 100.))
                    if s == e:
                        rad = sorted(rad)[s: s+1]
                    else:
                        rad = np.array(sorted(rad)[s: e])

                # Get mean value of pixel range
                subr.append(np.mean(rad))

            subr_hist = subr[rmin:rmax + 1]
            hist_x = list(range(0, len(subr_hist)))

            # Get pchip line from subtraction histogram
            hull_x, hull_y = getHull(hist_x, subr_hist)
            y_pchip = np.array(pchip(hull_x, hull_y, hist_x))

            subr_hist = np.concatenate((np.zeros(rmin), y_pchip))
            subr_hist = np.concatenate((subr_hist, np.zeros(len(subr) - rmax)))

            sub_tr.append(subr_hist)


        # Create Angular background from subtraction lines (pchipline in each bin)
        bg_img = qfu.createAngularBG(copy_img.shape[1], copy_img.shape[0], np.array(sub_tr, dtype=np.float32), nBins)

        result = copy_img - bg_img
        result -= result.min()

        # Subtract original average fold by background
        self.info['bgimg1'] = result

    def applyCircularlySymBGSub2(self):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        fold = copy.copy(self.info['avg_fold'])
        center = [fold.shape[1] + .5, fold.shape[0] + .5]

        img = self.makeFullImage(fold)
        img = img.astype("float32")
        width = img.shape[1]
        height = img.shape[0]

        ad = np.ravel(img)
        ad = np.array(ad, 'f')
        b = np.array(ad, 'f')
        rmin = float(self.info['rmin'])
        rmax = float(self.info['rmax'])
        bin_size = float(self.info["radial_bin"])
        smooth = self.info['smooth']
        tension = self.info['tension']
        max_bin = int(np.ceil((rmax - rmin) / bin_size))
        max_num = int(np.ceil(rmax * 2 * np.pi))
        pc1 = self.info['cirmin']/100.
        pc2 = self.info['cirmax']/100.

        csyb = np.zeros(max_bin, 'f')
        csyd = np.zeros(max_bin, 'f')
        ys = np.zeros(max_bin, 'f')
        ysp = np.zeros(max_bin, 'f')
        wrk = np.zeros(max_bin * 9, 'f')
        pixbin = np.zeros(max_num, 'f')
        index_bn = np.zeros(max_num, 'f')

        ccp13.bgcsym2(ad=ad, b=b,
                      smoo=smooth,
                      tens=tension,
                      pc1=pc1,
                      pc2=pc2,
                      npix=width,
                      nrast=height,
                      dmin=rmin,
                      dmax=rmax,
                      xc=width/2.,
                      yc=height/2.,
                      dinc=bin_size,
                      csyb=csyb,
                      csyd=csyd,
                      ys=ys,
                      ysp=ysp,
                      wrk=wrk,
                      pixbin=pixbin,
                      index_bn=index_bn,
                      iprint=0,
                      ilog=1,
                      )

        print "BG CREATED"
        background = copy.copy(b)
        background[np.isnan(background)] = 0.
        background = np.array(background, 'float32')
        background = background.reshape((height, width))
        background = background[:fold.shape[0], :fold.shape[1]]
        result = np.array(fold - background, dtype=np.float32)
        result = qfu.replaceRmin(result, int(rmin), 0.)

        self.info['bgimg1'] = result


    def applyCircularlySymBGSub(self):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        copy_img = copy.copy(self.info['avg_fold'])
        center = [copy_img.shape[1] - .5, copy_img.shape[0] - .5]
        npt_rad = int(distance(center, (0, 0)))

        ai = pyFAI.AzimuthalIntegrator(detector="agilent_titan")
        ai.setFit2D(100, center[0], center[1])
        mask = np.zeros((copy_img.shape[0], copy_img.shape[1]))

        start_p = self.info["cirmin"]  # minimum value of circular background subtraction pixel range in percent
        end_p = self.info["cirmax"]  # maximum value of circular background subtraction pixel range in percent
        rmin = self.info["rmin"]  # minimum radius for background subtraction
        rmax = self.info["rmax"]  # maximum radius for background subtraction
        radial_bin = self.info["radial_bin"]
        smooth = self.info['smooth']
        tension = self.info['tension']

        # I2D = []
        # for deg in range(180, 271):
        #     if deg == 180:
        #         x, I = ai.integrate1d(copy_img, npt_rad, mask=mask, unit="r_mm", azimuth_range=(deg, deg + 0.5))
        #     elif deg == 270:
        #         x, I = ai.integrate1d(copy_img, npt_rad, mask=mask, unit="r_mm", azimuth_range=(deg - 0.5, deg))
        #     else:
        #         x, I = ai.integrate1d(copy_img, npt_rad, mask=mask, unit="r_mm", azimuth_range=(deg - 0.5, deg + 0.5))
        #     I2D.append(I)

        # I2D, tth, chi = ai.integrate2d(copy_img, npt_rad, npt_azim=3600, unit="r_mm", method="csr_ocl")
        # I2D = np.array(I2D[0:900])
        # fig = plt.figure()
        # ax = fig.add_subplot(111)
        # ax.cla()
        # ax.imshow(I2D)
        # fig.show()

        # I2D = np.array(I2D)
        # xs = []
        # ys = []
        #
        # for r in np.arange(rmin, rmax, radial_bin):
        #     pixels = I2D[:,r:r+radial_bin]
        #     pixels = sorted(np.ravel(pixels))
        #     start_ind = int(round(len(pixels) * start_p / 100.))
        #     end_ind = int(round(len(pixels) * end_p / 100.))
        #     if start_ind >= end_ind:
        #         val = pixels[start_ind]
        #     else:
        #         val = np.mean(pixels[start_ind:end_ind])
        #     xs.append(r+radial_bin/2.)
        #     ys.append(val)

        max_pts = (2.*np.pi*rmax / 4. + 10) * radial_bin
        nBin = int((rmax-rmin)/radial_bin)

        xs, ys = qfu.getCircularDiscreteBackground(np.array(copy_img, np.float32), rmin, start_p, end_p, radial_bin, nBin, max_pts)

        max_distance = int(round(distance(center, (0,0)))) + 10
        sp = UnivariateSpline(xs, ys, s=smooth)
        newx = np.arange(rmin, rmax)
        interpolate = sp(newx)

        newx = np.arange(0, max_distance)
        newy = list(np.zeros(rmin))
        newy.extend(list(interpolate))
        newy.extend(np.zeros(max_distance-rmax))
        # fig = plt.figure()
        # ax = fig.add_subplot(111)
        # ax.plot(xs, ys, "ro", label = "orig")
        # ax.plot(newx, newy, label = "smooth")
        # ax.legend()
        # fig.show()

        self.info['bg_line'] = [xs, ys, newx, newy]
        # Create background from spline line
        background = qfu.createCircularlySymBG(copy_img.shape[1],copy_img.shape[0], np.array(newy, dtype=np.float32))

        result = copy_img - background
        # result -= result.min()

        # Subtract original average fold by background
        self.info['bgimg1'] = result
        #
        # result_path = fullPath(self.img_path, "qf_results")
        # createFolder(result_path)
        # background = background.astype("float32")
        # imsave(fullPath(result_path, "cir_bg.tif"), background)

    def getFirstPeak(self, hist):
        # Start from index 5 and go to the right until slope is less than -10
        for i in range(5, len(hist)/2):
            if hist[i] - hist[i-1] < -10:
                return i
        return 20

    def getRminmax(self):
        """
        get R-min and R-max for backgroun subtraction process. If these value is changed, background subtracted images need to be reproduced.
        """
        if self.info.has_key('rmin') and self.info.has_key('rmax'):
            return

        print "R-min and R-max is being calculated."

        if self.info.has_key('fixed_rmin') and self.info.has_key('fixed_rmax'):
            self.info['rmin'] = self.info['fixed_rmin']
            self.info['rmax'] = self.info['fixed_rmax']
        else:
            copy_img = copy.copy(self.info['avg_fold'])
            center = [copy_img.shape[1] - 1, copy_img.shape[0] - 1]
            npt_rad = int(distance(center, (0, 0)))

            # Get 1D azimuthal integration histogram
            ai = pyFAI.AzimuthalIntegrator(detector="agilent_titan")
            ai.setFit2D(100, center[0], center[1])
            x, totalI = ai.integrate1d(copy_img, npt_rad, unit="r_mm", azimuth_range=(180, 270))

            self.info['rmin'] = int(round(self.getFirstPeak(totalI) * 1.5))
            self.info['rmax'] = int(round((min(copy_img.shape[0], copy_img.shape[1]) - 1) * .8))

        self.deleteFromDict(self.info, 'bgimg1') # remove "bgimg1" from info to make it reprocess
        self.deleteFromDict(self.info, 'bgimg2') # remove "bgimg1" from info to make it reprocess
        print "Done. R-min is",self.info['rmin']," and R-max is", self.info['rmax']

    def apply2DConvexhull(self):
        """
        Apply 2D Convex hull Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        copy_img = copy.copy(self.info['avg_fold'])

        rmin = self.info['rmin']
        rmax = self.info['rmax']
        center = [copy_img.shape[1] - 1, copy_img.shape[0] - 1]

        hist_x = list(np.arange(rmin, rmax + 1))
        pchiplines = []

        det = "agilent_titan"
        npt_rad = int(distance(center, (0, 0)))
        ai = pyFAI.AzimuthalIntegrator(detector=det)
        ai.setFit2D(100, center[0], center[1])

        for deg in np.arange(180, 271, 1):
            if deg == 180 :
                x, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", azimuth_range=(180, 180.5))
            elif deg == 270:
                x, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", azimuth_range=(269.5, 270))
            else:
                x, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", azimuth_range=(deg-0.5, deg+0.5))

            hist_y = list(I[hist_x])

            hull_x, hull_y = getHull(hist_x, hist_y)
            y_pchip = pchip(hull_x, hull_y, hist_x)
            pchiplines.append(y_pchip)

        #
        # for deg in np.arange(0, 91, 1):
        #     # for each 1 degree from 0 to 90, get azimuth histogram
        #     hist_y = []
        #
        #     for r in range(rmin, rmax+1):
        #         x = int(round(1.* r * np.math.cos(np.deg2rad(deg))))
        #         y = int(round(1.* r * np.math.sin(np.deg2rad(deg))))
        #         if x > copy_img.shape[1] or y > copy_img.shape[0]:
        #             break
        #         hist_y.append(copy_img[copy_img.shape[0]-y-1,copy_img.shape[1]-x-1])
        #
        #     # get 1D convex hull pchip line for each azimuth histogram
        #     hull_x, hull_y = getHull(hist_x, hist_y)
        #     y_pchip = pchip(hull_x, hull_y, hist_x)
        #     pchiplines.append(y_pchip)

        # Smooth each histogram by radius
        pchiplines = np.array(pchiplines, dtype="float32")
        pchiplines2 = convolve1d(pchiplines, [1,2,1], axis=0)/4.

        # fig = plt.figure()
        # ax = fig.add_subplot(111)
        # ax.cla()
        # ax.imshow(pchiplines)
        # fig.show()
        #
        # fig = plt.figure()
        # ax = fig.add_subplot(111)
        # ax.cla()
        # ax.imshow(pchiplines2)
        # fig.show()

        # Produce Background from each pchip line
        background = qfu.make2DConvexhullBG2(pchiplines2, copy_img.shape[1], copy_img.shape[0], center[0], center[1], rmin, rmax)

        # Smooth background image by gaussian filter
        s = 10
        w = 4
        t = (((w - 1.) / 2.) - 0.5) / s
        background = gaussian_filter(background, sigma=s, truncate=t)

        # Subtract original average fold by background
        result = copy_img - background

        # imsave(fullPath(self.img_path,self.img_name)+".bg.tif", background)
        # imsave(fullPath(self.img_path, self.img_name) + ".orig.tif", np.array(copy_img, dtype="float32"))
        # result -= result.min()

        self.info['bgimg1'] = result
        # print "Done."

    def calculateAvgFold(self):
        """
        Calculate an average fold for 1-4 quadrants. Quadrants are splitted by center and rotation
        """
        if 'avg_fold' not in self.info.keys():
            self.deleteFromDict(self.info, 'rmin')
            self.deleteFromDict(self.info, 'rmax')
            self.imgResultForDisplay = None
            center = self.info['center']
            center_x = center[0]
            center_y = center[1]
            rotate_img = copy.copy(self.imgCache['rotateImg'])

            print "Quadrant folding is being processed..."
            img_width = rotate_img.shape[1]
            img_height = rotate_img.shape[0]
            fold_width = min(center[0], img_width-center[0])
            fold_height = min(center[1], img_height-center[1])

            # Get each fold, and flip them to the same direction
            top_left = rotate_img[center_y-fold_height:center_y, center_x-fold_width:center_x]
            top_right = rotate_img[center_y-fold_height:center_y, center_x:center_x+fold_width]
            top_right = cv2.flip(top_right,1)
            buttom_left = rotate_img[center_y:center_y+fold_height, center_x-fold_width:center_x]
            buttom_left = cv2.flip(buttom_left,0)
            buttom_right = rotate_img[center_y:center_y+fold_height, center_x:center_x+fold_width]
            buttom_right = cv2.flip(buttom_right,1)
            buttom_right = cv2.flip(buttom_right,0)

            # Add all folds which are not ignored
            quadrants = []
            if 0 not in self.info["ignore_folds"]:
                quadrants.append(top_left)
            if 1 not in self.info["ignore_folds"]:
                quadrants.append(top_right)
            if 2 not in self.info["ignore_folds"]:
                quadrants.append(buttom_left)
            if 3 not in self.info["ignore_folds"]:
                quadrants.append(buttom_right)

            # Get average fold from all folds
            self.get_avg_fold(quadrants,fold_height,fold_width)

            if 'resultImg' in self.imgCache.keys():
                del self.imgCache['resultImg']

            print "Done."

    def get_avg_fold(self, quadrants, fold_height, fold_width):
        """
        Get average fold from input
        :param quadrants: 1-4 quadrants
        :param fold_height: quadrant height
        :param fold_width: quadrant width
        :return:
        """
        result = np.zeros((fold_height, fold_width))

        if len(self.info["ignore_folds"]) < 4:
            # if self.info['pixel_folding']:
            # avarage fold by pixel to pixel by cython
            result = qfu.get_avg_fold_float32(np.array(quadrants, dtype="float32"), len(quadrants), fold_height, fold_width,
                                                  self.info['mask_thres'])
            # else:
            #     result = np.mean( np.array(quadrants), axis=0 )

        self.info['avg_fold'] = result

    def applyBackgroundSubtraction(self):
        """
        Apply background subtraction by user's choice. There are 2 images produced in this process
        - bgimg1 : image after applying background subtraction INSIDE merge radius
        - bgimg2 : image after applying background subtraction OUTSIDE merge radius
        """
        print "Background Subtraction is being processed..."

        # Produce bgimg1
        if not self.info.has_key("bgimg1"):
            avg_fold = np.array(self.info['avg_fold'], dtype="float32")
            if self.info["bgsub"] == 0:
                self.info["bgimg1"] = avg_fold # if self.info["bgsub"] is -1, original average fold will be used
            elif self.info["bgsub"] == 1:
                self.apply2DConvexhull()
            elif self.info["bgsub"] == 2:
                self.applyCircularlySymBGSub2()
                # self.applyCircularlySymBGSub()
            elif self.info["bgsub"] == 3:
                self.info["bgimg1"] = white_tophat(avg_fold, disk(self.info["tophat1"]))
            elif self.info["bgsub"] == 4:
                self.applyAngularBGSub()

            self.deleteFromDict(self.imgCache, "BgSubFold")

        # Produce bgimg2
        if not self.info.has_key("bgimg2"):
            avg_fold = np.array(self.info['avg_fold'], dtype="float32")
            if self.info["bgsub"] == 0:
                self.info["bgimg2"] = avg_fold # if self.info["bgsub"] is -1, original average fold will be used
            else:
                self.info["bgimg2"] = white_tophat(avg_fold, disk(self.info["tophat2"]))
            self.deleteFromDict(self.imgCache, "BgSubFold")

        print "Done"

    def mergeImages(self):
        """
        Merge bgimg1 and bgimg2 at merge radius, with sigmoid as a merge gradient param.
        The result of merging will be kept in self.info["BgSubFold"]
        :return:
        """
        print "Merging images..."

        if not self.imgCache.has_key("BgSubFold"):
            img1 = np.array(self.info["bgimg1"], dtype="float32")
            img2 = np.array(self.info["bgimg2"], dtype="float32")
            sigmoid = self.info["sigmoid"]
            center = [img1.shape[1]-1, img1.shape[0]-1]
            rad = self.info["rmax"] - 10

            # Merge 2 images at merge radius using sigmoid as merge gradient
            self.imgCache['BgSubFold'] = qfu.combine_bgsub_float32(img1, img2, center[0], center[1], sigmoid, rad)
            self.deleteFromDict(self.imgCache, "resultImg")

            # display_test(img1, "in",500)
            # display_test(img2, "out",500)
            # display_test(self.imgCache['BgSubFold'], "result",500)

        print "Done."

    def generateResultImage(self):
        """
        Put 4 self.info["BgSubFold"] together as a result image
        :return:
        """
        if 'resultImg' not in self.imgCache.keys():
            print "Generating result image from avarage fold..."
            self.imgCache['resultImg'] = self.makeFullImage(copy.copy(self.imgCache['BgSubFold']))
            print "Done."

    def makeFullImage(self, fold):
        """
        Flip + rotate 4 folds and combine them to 1 image
        :param fold:
        :return: result image
        """

        fold_height = fold.shape[0]
        fold_width = fold.shape[1]

        top_left = fold
        top_right = cv2.flip(fold, 1)

        buttom_left = cv2.flip(fold, 0)
        buttom_right = cv2.flip(buttom_left, 1)

        resultImg = np.zeros((fold_height * 2, fold_width * 2))
        resultImg[0:fold_height, 0:fold_width] = top_left
        resultImg[0:fold_height, fold_width:fold_width * 2] = top_right
        resultImg[fold_height:fold_height * 2, 0:fold_width] = buttom_left
        resultImg[fold_height:fold_height * 2, fold_width:fold_width * 2] = buttom_right

        return resultImg
