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
from ..utils.file_manager import fullPath, createFolder, getBlankImageAndMask, getMaskOnly
from ..utils.histogram_processor import *
from ..utils.image_processor import *
from skimage.morphology import white_tophat, disk
from tifffile import imsave
from scipy.interpolate import UnivariateSpline, PchipInterpolator
import musclex
import ccp13
from pyFAI.azimuthalIntegrator import AzimuthalIntegrator

# Make sure the cython part is compiled
# from subprocess import call
# call(["python setup2.py build_ext --inplace"], shell = True)

import musclex.modules.QF_utilities as qfu

class QuadrantFolder(object):
    """
    A class for Quadrant Folding processing - go to process() to see all processing steps
    """
    def __init__(self, img_path, img_name, parent):
        """
        Initial value for QuadrantFolder object
        :param img_path: directory path of input image
        :param img_name: image file name
        """
        self.orig_img = fabio.open(fullPath(img_path, img_name)).data
        if self.orig_img.shape == (1043, 981):
            self.img_type = "PILATUS"
        else:
            self.img_type = "NORMAL"
        self.empty = False
        self.img_path = img_path
        self.img_name = img_name
        self.imgCache = {} # displayed images will be saved in this param
        self.ignoreFolds = set()
        self.version = musclex.__version__
        cache = self.loadCache() # load from cache if it's available

        self.initImg = None
        self.centImgTransMat = None # Centerize image transformation matrix
        self.center_before_rotation = None # we need the center before rotation is applied each time we rotate the image
        self.rotMat = None # store the rotation matrix used so that any point specified in current co-ordinate system can be transformed to the base (original image) co-ordinate system
        self.centerChanged = False
        self.parent = parent
        self.masked = False

        # info dictionary will save all results
        if cache is not None:
            self.info = cache
        else:
            self.info = {
                'imgType' : str(self.orig_img.dtype)
            }


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
                print("Cache version " + info['program_version'] + " did not match with Program version " + self.version)
                print("Invalidating cache and reprocessing the image")
                    # return None
        return None

    def delCache(self):
        """
        Delete cache
        :return: -
        """
        cache_path = fullPath(self.img_path, "qf_cache")
        cache_file = fullPath(cache_path, self.img_name + '.info')
        if os.path.exists(cache_path) and os.path.isfile(cache_file):
            os.remove(cache_file)

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
        print(str(self.img_name) + " is being processed...")
        self.updateInfo(flags)
        self.initParams()
        self.applyBlankImageAndMask()
        self.findCenter()
        self.centerizeImage()
        self.rotateImg()
        self.calculateAvgFold()
        self.getRminmax()
        self.applyBackgroundSubtraction()
        self.mergeImages()
        self.generateResultImage()

        if "no_cache" not in flags:
            self.cacheInfo()

        self.parent.statusPrint("")

    def updateInfo(self, flags):
        if flags['orientation_model'] is None:
            if 'orientation_model' not in self.info:
                flags['orientation_model'] = 0
            else:
                del flags['orientation_model']
        self.info.update(flags)

    def initParams(self):
        """
        Initial some parameters in case GUI doesn't specified
        """
        if 'mask_thres' not in self.info:
            self.info['mask_thres'] = 0 #getMaskThreshold(self.orig_img, self.img_type)
        if 'ignore_folds' not  in self.info:
            self.info['ignore_folds'] = set()
        if 'bgsub' not in self.info:
            self.info['bgsub'] = 0
        if 'sigmoid' not in self.info:
            self.info['sigmoid'] = 0.05

    def applyBlankImageAndMask(self):
        if 'blank_mask' in self.info and self.info['blank_mask'] and not self.masked:
            img = np.array(self.orig_img, 'float32')
            blank, mask = getBlankImageAndMask(self.img_path)
            maskOnly = getMaskOnly(self.img_path)
            # blank = None
            if blank is not None:
                img = img - blank
            if mask is not None:
                img[mask > 0] = self.info['mask_thres'] - 1.
            if maskOnly is not None:
                print("Applying mask only image")
                img[maskOnly > 0] = self.info['mask_thres'] - 1

            self.orig_img = img
            self.masked = True

    def findCenter(self):
        """
       Find center of the diffraction. The center will be kept in self.info["center"].
       Once the center is calculated, the rotation angle will be re-calculated, so self.info["rotationAngle"] is deleted
       """
        self.parent.statusPrint("Finding Center...")
        if 'center' in self.info:
            self.centerChanged = False
            return
        self.centerChanged = True
        if 'calib_center' in self.info:
            self.info['center'] = self.info['calib_center']
            return
        if 'manual_center' in self.info:
            center = self.info['manual_center']
            if self.rotMat is not None:
                center = np.dot(cv2.invertAffineTransform(self.rotMat), [center[0] + self.dl, center[1] + self.db, 1])
                self.info['manual_center'] = center
            self.info['center'] = self.info['manual_center']
            return
        print("Center is being calculated ... ")
        self.orig_img, self.orig_image_center = processImageForIntCenter(self.orig_img, getCenter(self.orig_img), self.img_type, self.info["mask_thres"])
        self.info['center'] = self.orig_image_center
        print("Done. Center = "+str(self.info['center']))


    def rotateImg(self):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal. The angle will be kept in self.info["rotationAngle"]
        Once the rotation angle is calculated, the average fold will be re-calculated, so self.info["avg_fold"] is deleted
        """
        self.parent.statusPrint("Finding Rotation Angle...")
        if 'manual_rotationAngle' in self.info:
            self.info['rotationAngle'] = self.info['manual_rotationAngle']
            del self.info['manual_rotationAngle']
            self.deleteFromDict(self.info, 'avg_fold')
        elif "mode_angle" in self.info:
            print("Using mode orientation {}".format(self.info["mode_angle"]))
            self.info['rotationAngle'] = self.info["mode_angle"]
            self.deleteFromDict(self.info, 'avg_fold')
        elif not self.empty and 'rotationAngle' not in self.info.keys():
            print("Rotation Angle is being calculated ... ")
            # Selecting disk (base) image and corresponding center for determining rotation as for larger images (formed from centerize image) rotation angle is wrongly computed
            _, center = self.parent.getExtentAndCenter()
            img = copy.copy(self.initImg) if self.initImg is not None else copy.copy(self.orig_img)
            self.info['rotationAngle'] = getRotationAngle(img, center, self.info['orientation_model'])
            self.deleteFromDict(self.info, 'avg_fold')
        print("Done. Rotation Angle is " + str(self.info['rotationAngle']) +" degree")
            
    def centerizeImage(self):
        """
        Create an enlarged image such that image center is at the center of new image
        """
        self.parent.statusPrint("Centererizing image...")
        if not self.centerChanged:
            return
        center = self.info['center']
        if self.centImgTransMat is not None and 'calib_center' not in self.info:
            # convert center in initial img coordinate system
            M = self.centImgTransMat
            M[0,2] = -1*M[0,2]
            M[1,2] = -1*M[1,2]
            center = [center[0], center[1], 1]
            center = np.dot(M, center)
            if 'manual_center' in self.info:
                self.info['manual_center'] = (int(center[0]), int(center[1]))
            if 'calib_center' in self.info:
                self.info['calib_center'] = (int(center[0]), int(center[1]))

        center = (int(center[0]), int(center[1]))
        if self.initImg is None:
            # While centerizing image use the first image after reading from file and processing for int center
            self.initImg = self.orig_img
            print("Dimension of initial image before centerize ", self.orig_img.shape)
        img = self.initImg
        print("Dimension of image before centerize ", img.shape)

        b, l = img.shape
        if self.parent.newImgDimension is None:
            dim = int(2.8*max(l, b))
            self.parent.newImgDimension = dim
        else:
            dim = self.parent.newImgDimension
        new_img = np.zeros((dim,dim))
        new_img[0:b,0:l] = img
        
        #Translate image to appropriate position
        transx = int(((dim/2) - center[0]))
        transy = int(((dim/2) - center[1]))
        M = np.float32([[1,0,transx],[0,1,transy]])
        self.centImgTransMat = M
        rows,cols = new_img.shape
        mask_thres = self.info["mask_thres"]

        if self.img_type == "PILATUS":
            if mask_thres == -999:
                mask_thres = getMaskThreshold(img, self.img_type)
            mask = np.zeros((new_img.shape[0], new_img.shape[1]), dtype=np.uint8)
            mask[new_img <= mask_thres] = 255
            translated_Img = cv2.warpAffine(new_img, M, (cols, rows))
            translated_mask = cv2.warpAffine(mask, M, (cols, rows))
            translated_mask[translated_mask > 0.] = 255
            translated_Img[translated_mask > 0] = mask_thres
        else:
            translated_Img = cv2.warpAffine(new_img,M,(cols,rows))
        
        self.orig_img = translated_Img
        self.info['center'] = (int(dim / 2), int(dim / 2))
        self.center_before_rotation = (int(dim / 2), int(dim / 2))
        print("Dimension of image after centerize ", self.orig_img.shape)
        

    def getRotatedImage(self):
        """
        Get rotated image by angle while image = original input image, and angle = self.info["rotationAngle"]
        """
        img = np.array(self.orig_img, dtype="float32")

        center = self.info["center"]
        if self.center_before_rotation is not None:
            center = self.center_before_rotation
        else:
            self.center_before_rotation = center

        b, l = img.shape
        rotImg, newCenter, self.rotMat = rotateImage(img,center, self.info["rotationAngle"], self.img_type, self.info['mask_thres'])

        # Cropping off the surrounding part since we had already expanded the image to maximum possible extent in centerize image
        bnew, lnew = rotImg.shape
        db, dl = (bnew - b)//2, (lnew-l)//2
        final_rotImg = rotImg[db:bnew-db, dl:lnew-dl]
        self.info["center"] = (newCenter[0]-dl, newCenter[1]-db)
        self.dl, self.db = dl, db # storing the cropped off section to recalculate coordinates when manual center is given

        return final_rotImg

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

        ai = AzimuthalIntegrator(detector="agilent_titan")
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
            x, I = ai.integrate1d(copy_img, npt_rad, mask=mask, unit="r_mm", method="csr", azimuth_range=(deg, deg+1))
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
        max_bin = int(np.ceil((rmax - rmin) / bin_size))*10
        max_num = int(np.ceil(rmax * 2 * np.pi))*10
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
                      xc=width/2.-.5,
                      yc=height/2.-.5,
                      dinc=bin_size,
                      csyb=csyb,
                      csyd=csyd,
                      ys=ys,
                      ysp=ysp,
                      wrk=wrk,
                      pixbin=pixbin,
                      index_bn=index_bn,
                      iprint=0,
                      ilog=6,
                      )

        background = copy.copy(b)
        background[np.isnan(background)] = 0.
        background = np.array(background, 'float32')
        background = background.reshape((height, width))
        background = background[:fold.shape[0], :fold.shape[1]]
        result = np.array(fold - background, dtype=np.float32)
        result = qfu.replaceRmin(result, int(rmin), 0.)

        self.info['bgimg1'] = result

    def applySmoothedBGSub(self, type='gauss'):
        fold = copy.copy(self.info['avg_fold'])

        img = self.makeFullImage(fold)
        img = img.astype("float32")
        width = img.shape[1]
        height = img.shape[0]

        img = np.ravel(img)
        buf = np.array(img, 'f')
        maxfunc = len(buf)
        cback = np.zeros(maxfunc, 'f')
        b = np.zeros(maxfunc, 'f')
        smbuf = np.zeros(maxfunc, 'f')
        vals = np.zeros(20, 'f')

        if type == 'gauss':
            vals[0] = self.info['fwhm']
            vals[1] = self.info['cycles']
            vals[2] = float(self.info['rmin'])
            vals[3] = float(self.info['rmax'])
            vals[4] = width / 2. - .5
            vals[5] = height / 2. - .5
            vals[6] = img.min() - 1

            options = np.zeros((10, 10), 'S')
            options[0] = ['G', 'A', 'U', 'S', 'S', '', '', '', '', '']
            options = np.array(options, dtype='S')
        else:
            vals[0] = self.info['boxcar_x']
            vals[1] = self.info['boxcar_y']
            vals[2] = self.info['cycles']
            vals[3] = float(self.info['rmin'])
            vals[4] = float(self.info['rmax'])
            vals[5] = width / 2. - .5
            vals[6] = height / 2. - .5

            options = np.zeros((10, 10), 'S')
            options[0] = ['B', 'O', 'X', 'C', 'A', '', '', '', '', '']
            options = np.array(options, dtype='S')

        npix = width
        nrast = height
        xb = np.zeros(npix, 'f')
        yb = np.zeros(npix, 'f')
        ys = np.zeros(npix, 'f')
        ysp = np.zeros(npix, 'f')
        sig = np.zeros(npix, 'f')
        wrk = np.zeros(9 * npix, 'f')
        iflag = np.zeros(npix * nrast, 'f')
        ilog = 6

        ccp13.bcksmooth(buf=buf,
                        cback=cback,
                        b=b,
                        smbuf=smbuf,
                        vals=vals,
                        options=options,
                        xb=xb,
                        yb=yb,
                        ys=ys,
                        ysp=ysp,
                        sig=sig,
                        wrk=wrk,
                        iflag=iflag,
                        ilog=ilog,
                        nrast=nrast)

        background = copy.copy(b)
        background[np.isnan(background)] = 0.
        background = np.array(background, 'float32')
        background = background.reshape((height, width))
        background = background[:fold.shape[0], :fold.shape[1]]
        result = np.array(fold - background, dtype=np.float32)
        result = qfu.replaceRmin(result, int(self.info['rmin']), 0.)

        self.info['bgimg1'] = result


    def applyRovingWindowBGSub(self):
        """
        Apply Roving Window background subtraction
        :return:
        """
        fold = copy.copy(self.info['avg_fold'])
        center = [fold.shape[1] + .5, fold.shape[0] + .5]

        img = self.makeFullImage(fold)
        width = img.shape[1]
        height = img.shape[0]
        img = np.ravel(img)
        buf = np.array(img, 'f')
        b = np.zeros(len(buf), 'f')
        iwid = self.info['win_size_x']
        jwid = self.info['win_size_y']
        isep = self.info['win_sep_x']
        jsep = self.info['win_sep_y']
        smooth = self.info['smooth']
        tension = self.info['tension']
        pc1 = self.info['cirmin'] / 100.
        pc2 = self.info['cirmax'] / 100.

        maxdim = width * height
        maxwin = (iwid * 2 + 1) * (jwid * 2 + 1)

        ccp13.bgwsrt2(buf=buf,
                      b=b,
                      iwid=iwid,
                      jwid=jwid,
                      isep=isep,
                      jsep=jsep,
                      smoo=smooth,
                      tens=tension,
                      pc1=pc1,
                      pc2=pc2,
                      npix=width,
                      nrast=height,
                      maxdim=maxdim,
                      maxwin=maxwin,
                      xb=np.zeros(maxdim, 'f'),
                      yb=np.zeros(maxdim, 'f'),
                      ys=np.zeros(maxdim, 'f'),
                      ysp=np.zeros(maxdim, 'f'),
                      wrk=np.zeros(9 * maxdim, 'f'),
                      bw=np.zeros(maxwin, 'f'),
                      index_bn=np.zeros(maxwin, 'i'),
                      iprint=0,
                      ilog=6,
                      )

        background = copy.copy(b)
        background[np.isnan(background)] = 0.
        background = np.array(background, 'float32')
        background = background.reshape((height, width))
        background = background[:fold.shape[0], :fold.shape[1]]
        result = np.array(fold - background, dtype=np.float32)
        result = qfu.replaceRmin(result, int(self.info['rmin']), 0.)

        self.info['bgimg1'] = result


    def applyCircularlySymBGSub(self):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        copy_img = copy.copy(self.info['avg_fold'])
        center = [copy_img.shape[1] - .5, copy_img.shape[0] - .5]
        npt_rad = int(distance(center, (0, 0)))

        ai = AzimuthalIntegrator(detector="agilent_titan")
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
        for i in range(5, int(len(hist)/2)):
            if hist[i] - hist[i-1] < -10:
                return i
        return 20

    def getRminmax(self):
        """
        get R-min and R-max for backgroun subtraction process. If these value is changed, background subtracted images need to be reproduced.
        """
        self.parent.statusPrint("Finding Rmin and Rmax...")
        print("R-min and R-max is being calculated.")

        if 'fixed_rmin' in self.info and 'fixed_rmax' in self.info:
            if 'rmin' in self.info and 'rmax' in self.info:
                if self.info['rmin'] == self.info['fixed_rmin'] and self.info['rmax'] == self.info['fixed_rmax']:
                    return
            self.info['rmin'] = self.info['fixed_rmin']
            self.info['rmax'] = self.info['fixed_rmax']
        elif 'rmin' in self.info and 'rmax' in self.info:
            return
        else:
            copy_img = copy.copy(self.info['avg_fold'])
            center = [copy_img.shape[1] - 1, copy_img.shape[0] - 1]
            npt_rad = int(distance(center, (0, 0)))

            # Get 1D azimuthal integration histogram
            ai = AzimuthalIntegrator(detector="agilent_titan")
            ai.setFit2D(100, center[0], center[1])
            integration_method = pyFAI.method_registry.IntegrationMethod.select_one_available("csr", 1)
            x, totalI = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method=integration_method, azimuth_range=(180, 270))

            self.info['rmin'] = int(round(self.getFirstPeak(totalI) * 1.5))
            self.info['rmax'] = int(round((min(copy_img.shape[0], copy_img.shape[1]) - 1) * .8))

        self.deleteFromDict(self.info, 'bgimg1') # remove "bgimg1" from info to make it reprocess
        self.deleteFromDict(self.info, 'bgimg2') # remove "bgimg1" from info to make it reprocess
        print("Done. R-min is "+str(self.info['rmin']) + " and R-max is " + str(self.info['rmax']))

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
        ai = AzimuthalIntegrator(detector=det)
        ai.setFit2D(100, center[0], center[1])

        for deg in np.arange(180, 271, 1):
            if deg == 180 :
                x, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method="csr", azimuth_range=(180, 180.5))
            elif deg == 270:
                x, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method="csr", azimuth_range=(269.5, 270))
            else:
                x, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method="csr", azimuth_range=(deg-0.5, deg+0.5))

            hist_y = I[int(rmin):int(rmax+1)]
            hist_y = list(np.concatenate((hist_y, np.zeros(len(hist_x) - len(hist_y)))))
            #hist_y = list(I[hist_x])

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


    def calculateAvgFold(self):
        """
        Calculate an average fold for 1-4 quadrants. Quadrants are splitted by center and rotation
        """
        self.parent.statusPrint("Calculating Avg Fold...")
        if 'avg_fold' not in self.info.keys():
            self.deleteFromDict(self.info, 'rmin')
            self.deleteFromDict(self.info, 'rmax')
            self.imgResultForDisplay = None
            rotate_img = copy.copy(self.getRotatedImage())
            center = self.info['center']
            center_x = int(center[0])
            center_y = int(center[1])

            print("Quadrant folding is being processed...")
            img_width = rotate_img.shape[1]
            img_height = rotate_img.shape[0]
            fold_width = max(int(center[0]), img_width-int(center[0]))
            fold_height = max(int(center[1]), img_height-int(center[1]))

            # Get each fold, and flip them to the same direction
            top_left = rotate_img[max(center_y-fold_height,0):center_y, max(center_x-fold_width,0):center_x]
            top_right = rotate_img[max(center_y-fold_height,0):center_y, center_x:center_x+fold_width]
            top_right = cv2.flip(top_right,1)
            buttom_left = rotate_img[center_y:center_y+fold_height, max(center_x-fold_width,0):center_x]
            buttom_left = cv2.flip(buttom_left,0)
            buttom_right = rotate_img[center_y:center_y+fold_height, center_x:center_x+fold_width]
            buttom_right = cv2.flip(buttom_right,1)
            buttom_right = cv2.flip(buttom_right,0)

            # Add all folds which are not ignored
            quadrants = np.ones((4, fold_height, fold_width), rotate_img.dtype) * (self.info['mask_thres'] - 1.)
            for i, quad in enumerate([top_left, top_right, buttom_left, buttom_right]):
                quadrants[i][-quad.shape[0]:, -quad.shape[1]:] = quad
            remained = np.ones(4, dtype=bool)
            remained[list(self.info["ignore_folds"])] = False
            quadrants = quadrants[remained]

            # Get average fold from all folds
            self.get_avg_fold(quadrants,fold_height,fold_width)

            if 'resultImg' in self.imgCache.keys():
                del self.imgCache['resultImg']

            print("Done.")

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
        self.parent.statusPrint("Applying Background Subtraction...")
        print("Background Subtraction is being processed...")
        method = self.info["bgsub"]

        # Produce bgimg1
        if "bgimg1" not in self.info:
            avg_fold = np.array(self.info['avg_fold'], dtype="float32")
            if method == 'None':
                self.info["bgimg1"] = avg_fold # if method is None, original average fold will be used
            elif method == '2D Convexhull':
                self.apply2DConvexhull()
            elif method == 'Circularly-symmetric':
                self.applyCircularlySymBGSub2()
                # self.applyCircularlySymBGSub()
            elif method == 'White-top-hats':
                self.info["bgimg1"] = white_tophat(avg_fold, disk(self.info["tophat1"]))
            elif self.info['bgsub'] == 'Roving Window':
                self.applyRovingWindowBGSub()
            elif method == 'Smoothed-Gaussian':
                self.applySmoothedBGSub('gauss')
            elif method == 'Smoothed-BoxCar':
                self.applySmoothedBGSub('boxcar')
            else:
                self.info["bgimg1"] = avg_fold
            self.deleteFromDict(self.imgCache, "BgSubFold")

        # Produce bgimg2
        if "bgimg2" not in self.info:
            avg_fold = np.array(self.info['avg_fold'], dtype="float32")
            if method == 'None':
                self.info["bgimg2"] = avg_fold # if method is 'None', original average fold will be used
            else:
                self.info["bgimg2"] = white_tophat(avg_fold, disk(self.info["tophat2"]))
            self.deleteFromDict(self.imgCache, "BgSubFold")

        print("Done")

    def mergeImages(self):
        """
        Merge bgimg1 and bgimg2 at merge radius, with sigmoid as a merge gradient param.
        The result of merging will be kept in self.info["BgSubFold"]
        :return:
        """
        self.parent.statusPrint("Merging Images...")
        print("Merging images...")

        if "BgSubFold" not in self.imgCache:
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

        print("Done.")

    def generateResultImage(self):
        """
        Put 4 self.info["BgSubFold"] together as a result image
        :return:
        """
        self.parent.statusPrint("Generating Resultant Image...")
        print("Generating result image from avarage fold...")
        result = self.makeFullImage(copy.copy(self.imgCache['BgSubFold']))
        if 'rotate' in self.info and self.info['rotate']:
            result = np.rot90(result)
        self.imgCache['resultImg'] = result
        print("Done.")

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
