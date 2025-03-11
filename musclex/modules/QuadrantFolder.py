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
import pickle
import fabio
from scipy.ndimage.filters import gaussian_filter, convolve1d
from scipy.interpolate import UnivariateSpline
from skimage.morphology import white_tophat, disk
#import ccp13
from ..converted_fortran.converted_fortran import *
from pyFAI.method_registry import IntegrationMethod
from pyFAI.azimuthalIntegrator import AzimuthalIntegrator
from musclex import __version__
try:
    from . import QF_utilities as qfu
    from ..utils.file_manager import fullPath, createFolder, getBlankImageAndMask, getMaskOnly, ifHdfReadConvertless
    from ..utils.histogram_processor import *
    from ..utils.image_processor import *
except: # for coverage
    from modules import QF_utilities as qfu
    from utils.file_manager import fullPath, createFolder, getBlankImageAndMask, getMaskOnly, ifHdfReadConvertless
    from utils.histogram_processor import *
    from utils.image_processor import *

# Make sure the cython part is compiled
# from subprocess import call
# call(["python setup2.py build_ext --inplace"], shell = True)

class QuadrantFolder:
    """
    A class for Quadrant Folding processing - go to process() to see all processing steps
    """
    def __init__(self, img_path, img_name, parent, file_list=None, extension=''):
        """
        Initial value for QuadrantFolder object
        :param img_path: directory path of input image
        :param img_name: image file name
        """

        if extension in ('.hdf5', '.h5'):
            index = next((i for i, item in enumerate(file_list[0]) if item == img_name), 0)
            self.orig_img = file_list[1][index]
        else:
            try:
                self.orig_img = fabio.open(fullPath(img_path, img_name)).data
            except:
                exit
        self.orig_img = ifHdfReadConvertless(img_name, self.orig_img)
        self.orig_img = self.orig_img.astype("float32")
        self.orig_image_center = None
        self.dl, self.db = 0, 0
        self.empty = False
        self.img_path = img_path
        self.img_name = img_name
        self.imgCache = {} # displayed images will be saved in this param
        self.ignoreFolds = set()
        self.version = __version__
        cache = self.loadCache() # load from cache if it's available
        self.initImg = None
        self.centImgTransMat = None # Centerize image transformation matrix
        self.center_before_rotation = None # we need the center before rotation is applied each time we rotate the image
        self.rotMat = None # store the rotation matrix used so that any point specified in current co-ordinate system can be transformed to the base (original image) co-ordinate system
        self.centerChanged = False
        self.expandImg = 1
        self.origSize = self.orig_img.shape

        if parent is not None:
            self.parent = parent
        else:
            self.parent = self
        self.newImgDimension = None
        self.masked = False

        # info dictionary will save all results
        if cache is not None:
            self.info = cache
        else:
            self.info = {}

        #Nick Allison
        #Used for persisting the center when processing a folder of images that
        #need to have the same center.
        self.fixedCenterX = None
        self.fixedCenterY = None

        #Same thing for rotation
        self.fixedRot = None

    def cacheInfo(self):
        """
        Save info dict to cache. Cache file will be save as filename.info in folder "qf_cache"
        :return: -
        """
        cache_file = fullPath(fullPath(self.img_path, "qf_cache"), self.img_name + ".info")
        createFolder(fullPath(self.img_path, "qf_cache"))
        self.info['program_version'] = self.version

        with open(cache_file, "wb") as c:
            pickle.dump(self.info, c)

    def loadCache(self):
        """
        Load info dict from cache. Cache file will be filename.info in folder "qf_cache"
        :return: cached info (dict)
        """
        cache_file = fullPath(fullPath(self.img_path, "qf_cache"), self.img_name+".info")
        if os.path.isfile(cache_file):
            with open(cache_file, "rb") as c:
                info = pickle.load(c)
            if info is not None:
                if info['program_version'] == self.version:
                    return info
                print("Cache version " + info['program_version'] + " did not match with Program version " + self.version)
                print("Invalidating cache and reprocessing the image")
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

    def deleteFromDict(self, dicto, delStr):
        """
        Delete a key and value from dictionary
        :param dict: input dictionary
        :param delStr: deleting key
        :return: -
        """
        if delStr in dicto:
            del dicto[delStr]

    def readMaskFile(self):
        """
        Reads from a file that contains an upper bound and lower bound for
        pixel values.
        :return: minimum value(int), maximum value(int)
        """
        try:
            with open(os.path.join(self.img_path, "settings/maskthresh.txt"), "r") as file:
                lst = file.readlines()
            return float(lst[0]), float(lst[1])
        except:
            print("Ran into some problem reading from mask file.")
            return -1.0, -1.0

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
        if flags['fold_image'] == False:
            self.info['avg_fold'] = self.orig_img
            self.info['folded'] = False
            
            #self.initImg = self.orig_img
            
            # if self.initImg is not None:
            #     self.info['avg_fold'] = self.initImg
            # else:
            #     self.info['avg_fold'] = self.orig_img
        self.getRminmax()

        self.applyBackgroundSubtraction()
        self.mergeImages()
        self.generateResultImage()

        if "no_cache" not in flags:
            self.cacheInfo()


        self.parent.statusPrint("")

    def updateInfo(self, flags):
        """
        Update info dict using flags
        :param flags: flags
        :return: -
        """
        if flags['orientation_model'] is None:
            if 'orientation_model' not in self.info:
                flags['orientation_model'] = 0
            else:
                del flags['orientation_model']
        self.info.update(flags)
        if 'fixed_roi_rad' in self.info:
            self.info['roi_rad'] = self.info['fixed_roi_rad']

    def initParams(self):
        """
        Initial some parameters in case GUI doesn't specified
        """
        if 'mask_thres' not in self.info:
            self.info['mask_thres'] = getMaskThreshold(self.orig_img)
        if 'ignore_folds' not in self.info:
            self.info['ignore_folds'] = set()
        if 'bgsub' not in self.info:
            self.info['bgsub'] = 0
        if 'sigmoid' not in self.info:
            self.info['sigmoid'] = 0.05

    def applyBlankImageAndMask(self):
        """
        Apply the blank image and mask threshold on the orig_img
        :return: -
        """
        
        print("APPLY BLANK IMAGE AND MASK FUNCTION") #NICKA DEBUG

        if 'blank_mask' in self.info and self.info['blank_mask'] and not self.masked:
            img = np.array(self.orig_img, 'float32')
            blank, mask = getBlankImageAndMask(self.img_path)

            maskOnly = getMaskOnly(self.img_path)

            if maskOnly is not None:
                print("GET MASK ONLY MASK: ", maskOnly.shape) #NICKA DEBUG
            else: 
                print("GET MASK ONLY MASK IS NONE") #NICKA DEBUG
            if mask is not None:
                print("Mask Size is: ", mask.shape) #NICKA DEBUG
            else:
                print("Mask is none")
            if img is not None:
                print("IMAGE SIZE ", img.shape) #NICKA DEBUG
            else:
                print("IMAGE IS NONE") #NICKA DEBUG



            if blank is not None:
                img = img - blank
            if mask is not None:
                img[mask == 0] = self.info['mask_thres'] - 1.
            if maskOnly is not None:
                print("Applying mask only image")
                img[maskOnly == 0] = self.info['mask_thres'] - 1

            self.orig_img = img
            self.masked = True

    def findCenter(self):
        """
        Find center of the diffraction. The center will be kept in self.info["center"].
        Once the center is calculated, the rotation angle will be re-calculated, so self.info["rotationAngle"] is deleted
        """
        self.parent.statusPrint("Finding Center...")
        #Nick Allison
        #if the center needs to be persisted when doing a whole folder
        #, then use the persisted center
        if self.fixedCenterX is not None and self.fixedCenterY is not None:
            self.info['center'] = []
            self.info['center'].append(self.fixedCenterX)
            self.info['center'].append(self.fixedCenterY)
            self.centerChanged = False
            return
        if 'mask_thres' not in self.info:
            self.initParams()
        if 'center' in self.info:
            self.centerChanged = False
            print("CENTER IN QFP") #NiCKA DEBUG
            return
        self.centerChanged = True
        if 'calib_center' in self.info:
            self.info['center'] = self.info['calib_center']
            print("CALIB CENTER IN QFP") #NICKA DEBUG
            return
        if 'manual_center' in self.info:
            center = self.info['manual_center']
            if self.rotMat is not None:
                center = np.dot(cv2.invertAffineTransform(self.rotMat), [center[0] + self.dl, center[1] + self.db, 1])
                self.info['manual_center'] = center
            self.info['center'] = self.info['manual_center']
            print("MANUSAL CENTER IN QFP") #NICKA DEBUG
            return
        print("Center is being calculated ... ")
        self.orig_image_center = getCenter(self.orig_img)
        self.orig_img, self.info['center'] = processImageForIntCenter(self.orig_img, self.orig_image_center)
        print("Done. Center = "+str(self.info['center']))


    def rotateImg(self):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal. The angle will be kept in self.info["rotationAngle"]
        Once the rotation angle is calculated, the average fold will be re-calculated, so self.info["avg_fold"] is deleted
        """
        self.parent.statusPrint("Finding Rotation Angle...")
        #NickA: First if is for if the Fixed Rotation Angle GUI box is checked.
        if self.fixedRot is not None:
            self.info['rotationAngle'] = self.fixedRot
            self.deleteFromDict(self.info, 'avg_fold')
        elif 'manual_rotationAngle' in self.info:
            self.info['rotationAngle'] = self.info['manual_rotationAngle']
            del self.info['manual_rotationAngle']
            self.deleteFromDict(self.info, 'avg_fold')
        elif "mode_angle" in self.info:
            print(f'Using mode orientation {self.info["mode_angle"]}')
            self.info['rotationAngle'] = self.info["mode_angle"]
            self.deleteFromDict(self.info, 'avg_fold')
        elif not self.empty and 'rotationAngle' not in self.info.keys():
            print("Rotation Angle is being calculated ... ")
            # Selecting disk (base) image and corresponding center for determining rotation as for larger images (formed from centerize image) rotation angle is wrongly computed
            #_, center = self.parent.getExtentAndCenter()
            _, center = self.getExtentAndCenter()
            img = copy.copy(self.initImg) if self.initImg is not None else copy.copy(self.orig_img)
            if 'detector' in self.info:
                self.info['rotationAngle'] = getRotationAngle(img, center, self.info['orientation_model'], man_det=self.info['detector'])
            else:
                self.info['rotationAngle'] = getRotationAngle(img, center, self.info['orientation_model'])
            self.deleteFromDict(self.info, 'avg_fold')
        print("Done. Rotation Angle is " + str(self.info['rotationAngle']) +" degree")

    def getExtentAndCenter(self):
        """
        Give the extent and the center of the image in self.
        :return: extent, center
        """
        if self is None:
            return [0,0], (0,0)
        if self.orig_image_center is None and (self.fixedCenterX == None or self.fixedCenterY == None):
            self.findCenter()
            self.statusPrint("Done.")
        if self.fixedCenterX is not None and self.fixedCenterY is not None:
            center = []
            center.append(self.fixedCenterX)
            center.append(self.fixedCenterY)
        elif 'calib_center' in self.info:
            center = self.info['calib_center']
        elif 'manual_center' in self.info:
            center = self.info['manual_center']
        else:
            center = self.orig_image_center
        extent = [self.info['center'][0] - center[0], self.info['center'][1] - center[1]]


        return extent, center

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
            # This is the max dimension in the case beamline is in a corner and image rotated to 45 degrees
            qf_w, qf_h = 2.8*(l-center[0]), 2.8*(b-center[1])
            max_side = max(max(l,b), max(qf_w, qf_h))
            dim = int(self.expandImg*max_side)
            self.parent.newImgDimension = dim
        else:
            dim = self.parent.newImgDimension
        new_img = np.zeros((dim,dim)).astype("float32")
        try:
            new_img[0:b,0:l] = img
        except:
            print("Centerize Image : Dimension mismatched. Please report error and the steps leading up to it.")
        

        #Translate image to appropriate position
        transx = int(((dim/2) - center[0]))
        transy = int(((dim/2) - center[1]))
        M = np.float32([[1,0,transx],[0,1,transy]])
        self.centImgTransMat = M
        rows, cols = new_img.shape
        # mask_thres = self.info["mask_thres"]

        # if self.img_type == "PILATUS":
        #     if mask_thres == -999:
        #         mask_thres = getMaskThreshold(img, self.img_type)
        #     mask = np.zeros((new_img.shape[0], new_img.shape[1]), dtype=np.uint8)
        #     mask[new_img <= mask_thres] = 255
        #     cv2.setNumThreads(1) # Added to prevent segmentation fault due to cv2.warpAffine
        #     translated_Img = cv2.warpAffine(new_img, M, (cols, rows))
        #     translated_mask = cv2.warpAffine(mask, M, (cols, rows))
        #     translated_mask[translated_mask > 0.] = 255
        #     translated_Img[translated_mask > 0] = mask_thres
        # else:
        cv2.setNumThreads(1) # Added to prevent segmentation fault due to cv2.warpAffine

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
        rotImg, newCenter, self.rotMat = rotateImage(img, center, self.info["rotationAngle"])

        # Cropping off the surrounding part since we had already expanded the image to maximum possible extent in centerize image
        bnew, lnew = rotImg.shape
        db, dl = (bnew - b)//2, (lnew-l)//2
        final_rotImg = rotImg[db:bnew-db, dl:lnew-dl]
        if self.fixedCenterX is None and self.fixedCenterY is None:
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
        return -1

    def applyAngularBGSub(self):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        copy_img = copy.copy(self.info['avg_fold'])
        center = [copy_img.shape[1]-1, copy_img.shape[0]-1]
        npt_rad = int(distance(center,(0,0)))

        if 'detector' in self.info:
            det = find_detector(copy_img, man_det=self.info['detector'])
        else:
            det = find_detector(copy_img)

        ai = AzimuthalIntegrator(detector=det)
        ai.setFit2D(100, center[0], center[1])
        mask = np.zeros((copy_img.shape[0], copy_img.shape[1]))

        start_p = self.info["cirmin"] # minimum value of circular background subtraction pixel range in percent
        end_p = self.info["cirmax"] # maximum value of circular background subtraction pixel range in percent
        rmin = self.info["rmin"] # minimum radius for background subtraction
        rmax = self.info["rmax"] # maximum radius for background subtraction
        theta_size = self.info["bin_theta"] # bin size in degree
        nBins = 90/theta_size

        I2D = []
        integration_method = IntegrationMethod.select_one_available("csr", dim=1, default="csr", degradable=True)
        for deg in range(180, 271):
            _, I = ai.integrate1d(copy_img, npt_rad, mask=mask, unit="r_mm", method=integration_method, azimuth_range=(deg, deg+1))
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
        # fold = copy.copy(self.info['avg_fold'])
        # # center = [fold.shape[1] + .5, fold.shape[0] + .5]

        # img = self.makeFullImage(fold)
        # img = img.astype("float32")
        # width = img.shape[1]
        # height = img.shape[0]

        # ad = np.ravel(img)
        # ad = np.array(ad, 'f')
        # b = np.array(ad, 'f')
        # rmin = float(self.info['rmin'])
        # rmax = float(self.info['rmax'])
        # bin_size = float(self.info["radial_bin"])
        # smoo = self.info['smooth']
        # tension = self.info['tension']
        # max_bin = int(np.ceil((rmax - rmin) / bin_size))*10
        # max_num = int(np.ceil(rmax * 2 * np.pi))*10
        # pc1 = self.info['cirmin']/100.
        # pc2 = self.info['cirmax']/100.

        # csyb = np.zeros(max_bin, 'f')
        # csyd = np.zeros(max_bin, 'f')
        # ys = np.zeros(max_bin, 'f')
        # ysp = np.zeros(max_bin, 'f')
        # wrk = np.zeros(max_bin * 9, 'f')
        # pixbin = np.zeros(max_num, 'f')
        # index_bn = np.zeros(max_num, 'f')

        # ccp13.bgcsym2(ad=ad, b=b,
        #               smoo=smoo,
        #               tens=tension,
        #               pc1=pc1,
        #               pc2=pc2,
        #               npix=width,
        #               nrast=height,
        #               dmin=rmin,
        #               dmax=rmax,
        #               xc=width/2.-.5,
        #               yc=height/2.-.5,
        #               dinc=bin_size,
        #               csyb=csyb,
        #               csyd=csyd,
        #               ys=ys,
        #               ysp=ysp,
        #               wrk=wrk,
        #               pixbin=pixbin,
        #               index_bn=index_bn,
        #               iprint=0,
        #               ilog=6,
        #               maxbin=max_bin,
        #               maxnum=max_num)

        # background = copy.copy(b)
        # background[np.isnan(background)] = 0.
        # background = np.array(background, 'float32')
        # background = background.reshape((height, width))
        # background = background[:fold.shape[0], :fold.shape[1]]
        # result = np.array(fold - background, dtype=np.float32)
        # result = qfu.replaceRmin(result, int(rmin), 0.)

        # self.info['bgimg1'] = result

        fold = copy.copy(self.info["avg_fold"])

        img = self.makeFullImage(fold)
        img = img.astype("float32")
        width = img.shape[1]
        height = img.shape[0]

        ad = np.ravel(img)
        rmin = float(self.info["rmin"])
        rmax = float(self.info["rmax"])
        bin_size = float(self.info["radial_bin"])
        smoo = self.info["smooth"]
        pc1 = self.info["cirmin"] / 100.0
        pc2 = self.info["cirmax"] / 100.0

        # Call the new background subtraction function
        background = replicate_bgcsym2(
            AD=ad, 
            width=width, 
            height=height, 
            dmin=rmin, 
            dmax=rmax, 
            xc=width / 2.0 - 0.5, 
            yc=height / 2.0 - 0.5, 
            bin_size=bin_size, 
            smooth=smoo, 
            tension=self.info["tension"], 
            pc1=pc1, 
            pc2=pc2
        )

        background = copy.copy(background)
        background[np.isnan(background)] = 0.
        background = np.array(background, 'float32')
        background = background.reshape((height, width))
        background = background[:fold.shape[0], :fold.shape[1]]
        result = np.array(fold - background, dtype=np.float32)
        result = qfu.replaceRmin(result, int(rmin), 0.)
        self.info['bgimg1'] = result



    def applySmoothedBGSub(self, typ='gauss'):
        """
        Apply the background substraction smoothed, with default type to gaussian.
        :param typ: type of the substraction
        """
        # fold = copy.copy(self.info['avg_fold'])

        # img = self.makeFullImage(fold)
        # img = img.astype("float32")
        # width = img.shape[1]
        # height = img.shape[0]

        # img = np.ravel(img)
        # buf = np.array(img, 'f')
        # maxfunc = len(buf)
        # cback = np.zeros(maxfunc, 'f')
        # b = np.zeros(maxfunc, 'f')
        # smbuf = np.zeros(maxfunc, 'f')
        # vals = np.zeros(20, 'f')

        # if typ == 'gauss':
        #     vals[0] = self.info['fwhm']
        #     vals[1] = self.info['cycles']
        #     vals[2] = float(self.info['rmin'])
        #     vals[3] = float(self.info['rmax'])
        #     vals[4] = width / 2. - .5
        #     vals[5] = height / 2. - .5
        #     vals[6] = img.min() - 1

        #     options = np.zeros((10, 10), 'S')
        #     options[0] = ['G', 'A', 'U', 'S', 'S', '', '', '', '', '']
        #     options = np.array(options, dtype='S')
        # else:
        #     vals[0] = self.info['boxcar_x']
        #     vals[1] = self.info['boxcar_y']
        #     vals[2] = self.info['cycles']
        #     vals[3] = float(self.info['rmin'])
        #     vals[4] = float(self.info['rmax'])
        #     vals[5] = width / 2. - .5
        #     vals[6] = height / 2. - .5

        #     options = np.zeros((10, 10), 'S')
        #     options[0] = ['B', 'O', 'X', 'C', 'A', '', '', '', '', '']
        #     options = np.array(options, dtype='S')

        # npix = width
        # nrast = height
        # xb = np.zeros(npix, 'f')
        # yb = np.zeros(npix, 'f')
        # ys = np.zeros(npix, 'f')
        # ysp = np.zeros(npix, 'f')
        # sig = np.zeros(npix, 'f')
        # wrk = np.zeros(9 * npix, 'f')
        # iflag = np.zeros(npix * nrast, 'f')
        # ilog = 6

        # ccp13.bcksmooth(buf=buf,
        #                 cback=cback,
        #                 b=b,
        #                 smbuf=smbuf,
        #                 vals=vals,
        #                 options=options,
        #                 xb=xb,
        #                 yb=yb,
        #                 ys=ys,
        #                 ysp=ysp,
        #                 sig=sig,
        #                 wrk=wrk,
        #                 iflag=iflag,
        #                 ilog=ilog,
        #                 nrast=nrast,
        #                 npix=npix)

        # background = copy.copy(b)
        # background[np.isnan(background)] = 0.
        # background = np.array(background, 'float32')
        # background = background.reshape((height, width))
        # background = background[:fold.shape[0], :fold.shape[1]]
        # result = np.array(fold - background, dtype=np.float32)
        # result = qfu.replaceRmin(result, int(self.info['rmin']), 0.)

        # self.info['bgimg1'] = result

        #--------------------------------NEW ROVING WINDOW BG SUB--------------------------------

        fold = np.copy(self.info["avg_fold"])
        
        img = self.makeFullImage(fold)
        center = self.info["center"]

        if "roi_rad" in self.info: # if roi_rad is specified, use it
            roi_rad = int(self.info["roi_rad"])
            center_x = int(center[0])
            center_y = int(center[1])
            img = img[center_y - roi_rad:center_y + roi_rad, center_x - roi_rad:center_x + roi_rad]

        img = img.astype("float32")
        width = img.shape[1]
        height = img.shape[0]
       

        # Prepare input image
        #img = img.ravel().astype("float32")

        # Prepare options and parameter values based on 'typ'
        if typ == "gauss":
            filter_type = 'gaussian'
            kernel_size = (self.info["fwhm"], self.info["fwhm"])
            if kernel_size[0] % 2 == 0:
                kernel_size = (kernel_size[0] + 1, kernel_size[1] + 1)
            print("kernel size", kernel_size)
            sigmaX = 0
        else:
            filter_type = 'boxcar'
            kernel_size = (self.info["boxcar_x"], self.info["boxcar_y"])
            sigmaX = 0  # Set to zero for boxcar filter

        tension = self.info["tension"]
        edge_background = None  # You can provide edge background if available

        # Call bcksmooth function
        print("kernel size", kernel_size)
        result = replicate_bcksmooth(
            image=img,
            max_iterations=self.info["cycles"],
            filter_type=filter_type,
            kernel_size=kernel_size,
            sigmaX=sigmaX,
            tension=tension,
            edge_background=edge_background,
        )

        background = copy.copy(result)
        background[np.isnan(background)] = 0.0
        background = np.array(background, "float32")
        background = background.reshape((height, width))
        # replacing values that fall outside the roi_rad with the original values fromthe image
        print("background shape before padding", background.shape)
        print("fold shape", fold.shape)
        if "roi_rad" in self.info:
            background = background[:height//2, :width//2]
            pad_y = max((fold.shape[0] - background.shape[0]), 0)
            pad_x = max((fold.shape[1] - background.shape[1]), 0)
            background = np.pad(background, ((pad_y, 0), (pad_x, 0)), 'constant', constant_values=0)
        else: 
            background = background[:fold.shape[0], :fold.shape[1]]
       
        result = np.array(fold - background, dtype=np.float32)
        # replacing negative values with 0
        result = np.where(result < 0, 0, result)
        # positive values are obtained by computing the difference between the result and the corresponding value in the original image if the correspondinv value in the result is positive
        result = qfu.replaceRmin(result, int(self.info["rmin"]), 0.0)

        self.info["bgimg1"] = result


    def applyRovingWindowBGSub(self):
        """
        Apply Roving Window background subtraction
        :return:
        """
        # fold = copy.copy(self.info['avg_fold'])
        # # center = [fold.shape[1] + .5, fold.shape[0] + .5]

        # img = self.makeFullImage(fold)
        # width = img.shape[1]
        # height = img.shape[0]
        # img = np.ravel(img)
        # buf = np.array(img, 'f')
        # b = np.zeros(len(buf), 'f')
        # iwid = self.info['win_size_x']
        # jwid = self.info['win_size_y']
        # isep = self.info['win_sep_x']
        # jsep = self.info['win_sep_y']
        # smoo = self.info['smooth']
        # tension = self.info['tension']
        # pc1 = self.info['cirmin'] / 100.
        # pc2 = self.info['cirmax'] / 100.

        # maxdim = width * height
        # maxwin = (iwid * 2 + 1) * (jwid * 2 + 1)

        # ccp13.bgwsrt2(buf=buf,
        #               b=b,
        #               iwid=iwid,
        #               jwid=jwid,
        #               isep=isep,
        #               jsep=jsep,
        #               smoo=smoo,
        #               tens=tension,
        #               pc1=pc1,
        #               pc2=pc2,
        #               npix=width,
        #               nrast=height,
        #               maxdim=maxdim,
        #               maxwin=maxwin,
        #               xb=np.zeros(maxdim, 'f'),
        #               yb=np.zeros(maxdim, 'f'),
        #               ys=np.zeros(maxdim, 'f'),
        #               ysp=np.zeros(maxdim, 'f'),
        #               wrk=np.zeros(9 * maxdim, 'f'),
        #               bw=np.zeros(maxwin, 'f'),
        #               index_bn=np.zeros(maxwin, 'i'),
        #               iprint=0,
        #               ilog=6)

        # background = copy.copy(b)
        # background[np.isnan(background)] = 0.
        # background = np.array(background, 'float32')
        # background = background.reshape((height, width))
        # background = background[:fold.shape[0], :fold.shape[1]]
        # result = np.array(fold - background, dtype=np.float32)
        # result = qfu.replaceRmin(result, int(self.info['rmin']), 0.)

        # self.info['bgimg1'] = result

        #---------------------NEW VERSION OF ROVING WINDOW BACKGROUND SUBTRACTION---------------------#

        fold = copy.copy(self.info["avg_fold"])
        # center = [fold.shape[1] + .5, fold.shape[0] + .5]

        img = self.makeFullImage(fold)
        center = self.info["center"]

        if "roi_rad" in self.info: # if roi_rad is specified, use it

            roi_rad = int(self.info["roi_rad"])
            center_x = int(center[0])
            center_y = int(center[1])
            img = img[center_y - roi_rad:center_y + roi_rad, center_x - roi_rad:center_x + roi_rad]

        width = img.shape[1]
        height = img.shape[0]
        img = np.ravel(img)
        buf = np.array(img, "f")
        iwid = self.info["win_size_x"]
        jwid = self.info["win_size_y"]
        isep = self.info["win_sep_x"]
        jsep = self.info["win_sep_y"]
        smoo = self.info["smooth"]
        tension = self.info["tension"]
        pc1 = self.info["cirmin"] / 100.0
        pc2 = self.info["cirmax"] / 100.0

        maxdim = width * height
        maxwin = (iwid * 2 + 1) * (jwid * 2 + 1)

        # Prepare additional parameters for replicate_bgwsrt2
        xb = np.zeros(maxdim, dtype='f')
        yb = np.zeros(maxdim, dtype='f')
        ys = np.zeros(maxdim, dtype='f')  # Check if needed
        ysp = np.zeros(maxdim, dtype='f') # Check if needed
        wrk = np.zeros(9 * maxdim, dtype='f')  # Workspace array
        bw = np.zeros(maxwin, dtype='f')  # Background window array
        index_bn = np.zeros(maxwin, dtype='int')  # Check if needed
        b = np.zeros(maxdim, dtype='f')  # Background array
        # Call the replicate_bgwsrt2 function
        b = replicate_bgwsrt2(buf, b, iwid, jwid, isep, jsep, smoo, tension, pc1, pc2, width, height, maxdim, maxwin, xb, yb, ys, ysp, wrk, bw, index_bn, 0, 6)
        b= b.reshape((height, width))
      
        if "roi_rad" in self.info:
            b = b[:height//2, :width//2]
            pad_y = max((fold.shape[0] - b.shape[0]), 0)
            pad_x = max((fold.shape[1] - b.shape[1]), 0)
            b = np.pad(b, ((pad_y, 0), (pad_x, 0)), 'constant', constant_values=0)

        else: 
            b = b[:fold.shape[0], :fold.shape[1]]
      
        result = np.array(fold - b, dtype=np.float32)
        result = qfu.replaceRmin(result, int(self.info["rmin"]), 0.0)

        self.info["bgimg1"] = result

    def applyCircularlySymBGSub(self):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        copy_img = copy.copy(self.info['avg_fold'])
        center = [copy_img.shape[1] - .5, copy_img.shape[0] - .5]
        # npt_rad = int(distance(center, (0, 0)))

        # ai = AzimuthalIntegrator(detector="agilent_titan")
        # ai.setFit2D(100, center[0], center[1])
        # mask = np.zeros((copy_img.shape[0], copy_img.shape[1]))

        start_p = self.info["cirmin"]  # minimum value of circular background subtraction pixel range in percent
        end_p = self.info["cirmax"]  # maximum value of circular background subtraction pixel range in percent
        rmin = self.info["rmin"]  # minimum radius for background subtraction
        rmax = self.info["rmax"]  # maximum radius for background subtraction
        radial_bin = self.info["radial_bin"]
        smoo = self.info['smooth']
        # tension = self.info['tension']

        max_pts = (2.*np.pi*rmax / 4. + 10) * radial_bin
        nBin = int((rmax-rmin)/radial_bin)

        xs, ys = qfu.getCircularDiscreteBackground(np.array(copy_img, np.float32), rmin, start_p, end_p, radial_bin, nBin, max_pts)

        max_distance = int(round(distance(center, (0,0)))) + 10
        sp = UnivariateSpline(xs, ys, s=smoo)
        newx = np.arange(rmin, rmax)
        interpolate = sp(newx)

        newx = np.arange(0, max_distance)
        newy = list(np.zeros(rmin))
        newy.extend(list(interpolate))
        newy.extend(np.zeros(max_distance-rmax))

        self.info['bg_line'] = [xs, ys, newx, newy]
        # Create background from spline line
        background = qfu.createCircularlySymBG(copy_img.shape[1],copy_img.shape[0], np.array(newy, dtype=np.float32))

        result = copy_img - background
        # result -= result.min()

        # Subtract original average fold by background
        self.info['bgimg1'] = result

    def getFirstPeak(self, hist):
        """
        Find the first peak using the histogram.
        Start from index 5 and go to the right until slope is less than -10
        :param hist: histogram
        """
        for i in range(5, int(len(hist)/2)):
            if hist[i] - hist[i-1] < -10:
                return i
        return 20

    def getRminmax(self):
        """
        Get R-min and R-max for background subtraction process. If these value is changed, background subtracted images need to be reproduced.
        """
        self.parent.statusPrint("Finding Rmin and Rmax...")
        print("R-min and R-max is being calculated...")

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
            if 'detector' in self.info:
                det = find_detector(copy_img, man_det=self.info['detector'])
            else:
                det = find_detector(copy_img)

            ai = AzimuthalIntegrator(detector=det)
            ai.setFit2D(100, center[0], center[1])
            integration_method = IntegrationMethod.select_one_available("csr", dim=1, default="csr", degradable=True)
            _, totalI = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method=integration_method, azimuth_range=(180, 270))

            #self.info['rmin'] = int(round(self.getFirstPeak(totalI) * 1.5))
            self.info['rmin'] = int(round(getFirstVallay(totalI) - 10))
            self.info['rmax'] = int(round((min(copy_img.shape[0], copy_img.shape[1]) - 1) * .8))

        self.deleteFromDict(self.info, 'bgimg1') # remove "bgimg1" from info to make it reprocess
        self.deleteFromDict(self.info, 'bgimg2') # remove "bgimg2" from info to make it reprocess
        print("Done. R-min is "+str(self.info['rmin']) + " and R-max is " + str(self.info['rmax']))

    def apply2DConvexhull(self): # Deprecated, removed from MuscleX
        """
        Apply 2D Convex hull Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        copy_img_orig = copy.copy(self.info['avg_fold'])
        copy_img = copy.copy(self.info['avg_fold'])

        rmin = self.info['rmin']
        rmax = self.info['rmax']
        center = [copy_img.shape[1] - 1, copy_img.shape[0] - 1]

        # Downsampling
        scale=4
        copy_img = downsample(copy_img, scale=scale)
        base_img = np.ones((copy_img.shape[0], copy_img.shape[1]), dtype=np.uint8)
        rmin = rmin // scale
        rmax = rmax // scale
        center = [center[0]//scale, center[1]//scale]

        hist_x = list(np.arange(rmin, rmax + 1))
        pchiplines = []

        det = "agilent_titan"
        npt_rad = int(distance(center, (0, 0)))
        ai = AzimuthalIntegrator(detector=det)
        ai.setFit2D(100, center[0], center[1])

        integration_method = IntegrationMethod.select_one_available("csr", dim=1, default="csr", degradable=True)
        for deg in np.arange(180, 271, 1):
            if deg == 180 :
                #_, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method=integration_method, azimuth_range=(180, 180.5))
                start_deg = 180
                end_deg = 180.5
            elif deg == 270:
                #_, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method=integration_method, azimuth_range=(269.5, 270))
                start_deg=269.5
                end_deg=270
            else:
                #_, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method=integration_method, azimuth_range=(deg-0.5, deg+0.5))
                start_deg=deg-0.5
                end_deg=deg+0.5

            # Integrate the image and base image to get the volume
            _, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method=integration_method, azimuth_range=(start_deg, end_deg))
            _, Ib = ai.integrate1d(base_img, npt_rad, unit="r_mm", method=integration_method, azimuth_range=(start_deg, end_deg))
            # Divide by the volume
            I = np.divide(I, Ib, out=np.zeros_like(I), where=Ib!=0)

            hist_y = I[int(rmin):int(rmax+1)]
            hist_y = list(np.concatenate((hist_y, np.zeros(len(hist_x) - len(hist_y)))))
            #hist_y = list(I[hist_x])

            hull_x, hull_y = getHull(hist_x, hist_y)
            y_pchip = pchip(hull_x, hull_y, hist_x)
            pchiplines.append(y_pchip)

        # Smooth each histogram by radius
        pchiplines = np.array(pchiplines, dtype="float32")
        pchiplines2 = convolve1d(pchiplines, [1,2,1], axis=0)/4.

        # Smooth between neighboring histograms
        pchiplines3 = weighted_neighborhood_average(pchiplines2, weights=[0.25, 0.5, 0.25])

        # Produce Background from each pchip line
        background = qfu.make2DConvexhullBG2(pchiplines3, copy_img.shape[1], copy_img.shape[0], center[0], center[1], rmin, rmax)

        # Smooth background image by gaussian filter
        s = 10
        w = 4
        t = (((w - 1.) / 2.) - 0.5) / s
        background = gaussian_filter(background, sigma=s, truncate=t)

        background = upsample(background)
        background = pad_to_shape(background, copy_img_orig.shape)

        # Subtract original average fold by background
        result = copy_img_orig - background

        self.info['bgimg1'] = result

    def calculateAvgFold(self):
        """
        Calculate an average fold for 1-4 quadrants. Quadrants are splitted by center and rotation
        """
        self.parent.statusPrint("Calculating Avg Fold...")
        if 'avg_fold' not in self.info.keys():
            self.deleteFromDict(self.info, 'rmin')
            self.deleteFromDict(self.info, 'rmax')
            # self.imgResultForDisplay = None
            rotate_img = copy.copy(self.getRotatedImage())
            center = self.info['center']
            center_x = int(center[0])
            center_y = int(center[1])

            print("CENTER X: ", center_x) #NICKA DEBUG
            print("CENTER Y: ", center_y) #NICKA DEBUG

            print("Quadrant folding is being processed...")
            img_width = rotate_img.shape[1]
            img_height = rotate_img.shape[0]
            fold_width = max(int(center[0]), img_width-int(center[0])) # max(max(int(center[0]), img_width-int(center[0])), max(int(center[1]), img_height-int(center[1])))
            fold_height = max(int(center[1]), img_height-int(center[1])) # fold_width

            if img_width > center_x or img_height > center_y:
                print("ALERT: THE CENTER IS NOT INSIDE OF THE IMAGE!  THIS WILL LIKELY CAUSE AN ERROR") #Make future debugging easier

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
            if 'resultImg' in self.imgCache:
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
            # average fold by pixel to pixel by cython
            result = qfu.get_avg_fold_float32(np.array(quadrants, dtype="float32"), len(quadrants), fold_height, fold_width,
                                                self.info['mask_thres'])
            # else:
            #     result = np.mean( np.array(quadrants), axis=0 )

        self.info['avg_fold'] = result
        self.info['folded'] = True

    def applyBackgroundSubtraction(self):
        """
        Apply background subtraction by user's choice. There are 2 images produced in this process
        - bgimg1 : image after applying background subtraction INSIDE merge radius
        - bgimg2 : image after applying background subtraction OUTSIDE merge radius
        """
        self.parent.statusPrint("Applying Background Subtraction...")
        print("Background Subtraction is being processed...")
        method = self.info["bgsub"]
        print("Method = ", method) #NICKA DEBUG
        print("bgimg1 in self.info: ", 'bgimg1' in self.info) #NICKA DEBUG
        print("bgimg2 in self.info: ", 'bgimg2' in self.info) #NICKA DEBUG

        # Produce bgimg1
        if "bgimg1" not in self.info:
            avg_fold = np.array(self.info['avg_fold'], dtype="float32")
            if method == 'None':
                self.info["bgimg1"] = avg_fold # if method is None, original average fold will be used
            elif method == '2D Convexhull': # option has been commented out in the gui
                self.apply2DConvexhull()
            elif method == 'Circularly-symmetric':
                self.applyCircularlySymBGSub2()
                # self.applyCircularlySymBGSub()
            elif method == 'White-top-hats':
                self.info["bgimg1"] = white_tophat(avg_fold, disk(self.info["tophat1"]))
            elif method == 'Roving Window':
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

        print("Done.")

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

        print("Done.")

    def generateResultImage(self):
        """
        Put 4 self.info["BgSubFold"] together as a result image
        :return:
        """
        self.parent.statusPrint("Generating Resultant Image...")
        print("Generating result image from average fold...")
        result = self.makeFullImage(copy.copy(self.imgCache['BgSubFold']))
        if 'rotate' in self.info and self.info['rotate']:
            result = np.rot90(result)
        result[np.isnan(result)] = 0.
        if 'roi_rad' in self.info:
            center = result.shape[0]/2, result.shape[1]/2
            rad = self.info['roi_rad']
            result = result[max(int(center[1]-rad), 0):min(int(center[1]+rad), result.shape[1]), max(int(center[0]-rad), 0):min(int(center[0]+rad), result.shape[0])]
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

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        print(text)