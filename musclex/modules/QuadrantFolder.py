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
    from ..utils.file_manager import fullPath, createFolder, getBlankImageAndMask, getMaskOnly
    from ..utils.histogram_processor import *
    from ..utils.image_processor import *
    from ..utils.background_search import *
    from ..utils.image_data import ImageData
except: # for coverage
    from modules import QF_utilities as qfu
    from utils.file_manager import fullPath, createFolder, getBlankImageAndMask, getMaskOnly
    from utils.histogram_processor import *
    from utils.image_processor import *
    from utils.background_search import *
    from utils.image_data import ImageData

# Make sure the cython part is compiled
# from subprocess import call
# call(["python setup2.py build_ext --inplace"], shell = True)

# Invalid pixel threshold constant
# Pixels with values <= this threshold are considered invalid (masked/gap pixels)
# and are excluded from averaging calculations
INVALID_PIXEL_THRESHOLD = -1

class QuadrantFolder:
    """
    A class for Quadrant Folding processing - go to process() to see all processing steps
    """
    def __init__(self, image_data: ImageData, parent=None):
        """
        Initialize QuadrantFolder with ImageData container.
        
        :param image_data: ImageData container with image data and preprocessing
        :param parent: GUI/owner for status updates
        """
        # Get working image from ImageData (with blank/mask already applied)
        self.orig_img = image_data.get_working_image()
        self.img_path = str(image_data.img_path)
        self.img_name = image_data.img_name
        
        # Store reference to ImageData for fingerprint checking
        self._image_data = image_data
        
        # Initialize state
        self.orig_image_center = None
        self.dl, self.db = 0, 0
        self.empty = False
        self.imgCache = {} # displayed images will be saved in this param
        self.ignoreFolds = set()
        self.version = __version__
        cache = self.loadCache() # load from cache if it's available
        self.initImg = None
        self.centImgTransMat = None # Centerize image transformation matrix
        self.rotMat = None # store the rotation matrix used so that any point specified in current co-ordinate system can be transformed to the base (original image) co-ordinate system
        self.centerChanged = False
        self.expandImg = 1
        self.origSize = self.orig_img.shape

        if parent is not None:
            self.parent = parent
        else:
            self.parent = self
        
        self.newImgDimension = None

        # info dictionary will save all results (loaded from cache or empty)
        if cache is not None:
            self.info = cache
        else:
            self.info = {}

        # Configure info from ImageData
        if image_data.detector:
            self.info['detector'] = image_data.detector
        if image_data.orientation_model is not None:
            self.info['orientation_model'] = image_data.orientation_model

        # Runtime variables for current processing run
        # Get center and rotation from ImageData (it handles manual vs auto internally)
        # Don't set yet - will be set in findCenter()/getRotationAngle() during process()
        self.rotation = None
        
        # Working variables (set during processing)
        self.center = None       # Center of current (transformed) image - will be updated by transformImage()
        
        self.curr_dims = None

        #This is what all transformations will be done on
        self.start_img = copy.copy(self.orig_img)

    def cacheInfo(self):
        """
        Save info dict to cache. Cache file will be save as filename.info in folder "qf_cache"
        :return: -
        """
        cache_file = fullPath(fullPath(self.img_path, "qf_cache"), self.img_name + ".info")
        createFolder(fullPath(self.img_path, "qf_cache"))
        self.info['program_version'] = self.version
        
        # Save processing fingerprint for cache validation
        # (uses ImageData's fingerprint - includes config files, manual settings, preprocessing flags)
        self.info['processing_fingerprint'] = self._image_data.get_fingerprint()

        with open(cache_file, "wb") as c:
            pickle.dump(self.info, c)

    def loadCache(self):
        """
        Load info dict from cache. Only validates program version.
        All other validations (config files, center, rotation) are done in process().
        
        :return: cached info (dict) or None if cache doesn't exist or version mismatch
        """
        cache_file = fullPath(fullPath(self.img_path, "qf_cache"), self.img_name+".info")
        if os.path.isfile(cache_file):
            with open(cache_file, "rb") as c:
                info = pickle.load(c)
            if info is not None:
                if info['program_version'] == self.version:
                    return info  # ✅ Only check version
                print("Cache version " + info['program_version'] + " did not match with Program version " + self.version)
                print("Invalidating cache and reprocessing the image")
        return None
    
    def _clearDependentCaches(self):
        """
        Clear QuadrantFolder's processing results cache.
        
        Called when ImageData's fingerprint changes (indicating that
        preprocessing, geometry, or configuration has changed).
        """
        print("  Clearing processing results cache")
        self.deleteFromDict(self.info, 'avg_fold')
        self.deleteFromDict(self.info, 'rmin')
        self.deleteFromDict(self.info, 'rmax')

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
        All processing steps. All flags are provided by Quadrant Folding app as a dictionary
        """
        print(str(self.img_name) + " is being processed...")

        self.updateInfo(flags)
        self.initParams()
        
        # Note: Blank/mask preprocessing is already applied by ImageData.get_working_image()
        # No need to apply again here (would cause double subtraction of blank image)
        
        # Determine center and rotation to use for this run
        self.findCenter()        # Sets self.center from ImageData
        self.getRotationAngle()  # Sets self.rotation (if not already set by GUI)
        
        # ==========================================
        # Unified validation point: Check if processing parameters changed
        # ==========================================
        current_fingerprint = self._image_data.get_fingerprint()
        cached_fingerprint = self.info.get('processing_fingerprint', {})
        
        if current_fingerprint != cached_fingerprint:
            # Use ImageData's fingerprint comparison
            has_changes, changes = ImageData.compare_fingerprints(
                cached_fingerprint, current_fingerprint
            )
            
            if has_changes:
                print("Processing parameters changed. Clearing dependent caches.")
                print(f"  Changed items: {changes}")
                
                # Clear QuadrantFolder's processing results
                self._clearDependentCaches()
                
                # Update fingerprint in cache
                self.info['processing_fingerprint'] = current_fingerprint
        
        # ==========================================
        # Continue normal processing
        # ==========================================
        self.transformImage()
        self.calculateAvgFold()
        self.getRminmax()
        self.createMask()

        # try:
        
        self.createArtificialData()
        self.smoothFold()
        self.downsampleImage()
        self.applyBackgroundSubtraction()
        # except Exception as e:
        #     print("ERROR: Background subtraction failed.")
        #     print(e)
        self.applyBackgroundSubtractionSynthetic()
        self.generateResultImage()
        self.evaluateResult()

        if "no_cache" not in flags:
            self.cacheInfo()


        self.parent.statusPrint("")


    def updateInfo(self, flags):
        """
        Update info dict using flags
        :param flags: flags
        :return: -
        """
        # Get fresh image from ImageData (reflects latest blank/mask settings)
        # This ensures checkbox changes take effect without recreating QuadrantFolder
        self.orig_img = self._image_data.get_working_image()
        self.start_img = copy.copy(self.orig_img)  # Keep start_img in sync
        
        # Clear old transform matrices since we reset to fresh image
        # Without this, transforms would accumulate incorrectly on each process()
        if 'transform' in self.info:
            del self.info['transform']
        if 'inv_transform' in self.info:
            del self.info['inv_transform']

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
        Note: mask_thres removed - now using INVALID_PIXEL_THRESHOLD constant for invalid pixels
        """
        if 'ignore_folds' not in self.info:
            self.info['ignore_folds'] = set()
        if 'bgsub' not in self.info:
            self.info['bgsub'] = 'None'



    def findCenter(self):
        """
        Get center from ImageData (handles manual vs auto internally with caching).
        
        Sets self.center (will be updated to transformed center by transformImage()).
        """
        self.parent.statusPrint("Finding Center...")
        
        # Get center from ImageData (it handles manual/auto/cache internally)
        center = self._image_data.center
        self.center = center
        print(f"Using center from ImageData: {self.center}")


    def getRotationAngle(self):
        """
        Get rotation from ImageData (handles manual vs auto internally with caching).
        
        Sets self.rotation.
        """
        self.parent.statusPrint("Finding Rotation Angle...")
        
        # Get rotation from ImageData (it handles manual/auto/cache internally)
        rotation = self._image_data.rotation
        self.rotation = rotation
        print(f"Using rotation from ImageData: {self.rotation}°")

    def getExtentAndCenter(self):
        """
        Give the extent and the center of the image in self.
        :return: extent, center
        """
        # If center already exists, return it with zero extent
        if self.center is not None:
            return [0, 0], self.center
        
        # Otherwise, find the center first
        if self.orig_image_center is None:
            self.findCenter()
            self.statusPrint("Done.")
        
        # Now self.center should be set
        if self.center is not None:
            center = self.center
        else:
            # Fallback
            center = self.orig_image_center
        
        # Calculate extent (usually [0, 0] now)
        extent = [0, 0]
        print("EXTENT=", extent)
        print("CENTER=", center)

        return extent, center

    def transformImage(self):
        """
        Applies translation, scaling and rotation to the original image.
        This performs the functions previously done by CenterizeImage and RotateImage
        
        Uses self.rotation and self.center from findCenter()/getRotationAngle() to build
        a single composite transformation matrix that's applied to orig_img.
        """
        h_o, w_o = self.origSize
        x, y = self.center
        
        # Get angle from runtime variable (set by getRotationAngle)
        angle = self.rotation if self.rotation is not None else 0.0

        # Scaling factor (currently disabled, always 1.0)
        scale = 1.0
        self.info['scale'] = scale

        # Translation to move center to image center
        tx = (w_o/2) - scale * x
        ty = (h_o/2) - scale * y
        self.new_tx, self.new_ty = tx, ty

        # Build M1: scale + translate to center
        M1 = np.array([[scale, 0, tx],
                       [0, scale, ty]], dtype=np.float32)
        self.centImgTransMat = M1

        # Build M2: rotate around image center
        M2 = cv2.getRotationMatrix2D((w_o/2, h_o/2), angle, 1)

        # Compose transformations: M_total = M2 @ M1 (apply M1 first, then M2)
        # Convert to 3x3 for matrix multiplication
        M1_3x3 = np.vstack([M1, [0, 0, 1]])
        M2_3x3 = np.vstack([M2, [0, 0, 1]])
        M_total_3x3 = M2_3x3 @ M1_3x3
        
        # Convert back to 2x3
        transform = M_total_3x3[:2, :]

        # Set transformation matrices
        self.info['transform'] = transform
        self.info['inv_transform'] = cv2.invertAffineTransform(transform)

        # Apply transformation to image
        self.orig_img = cv2.warpAffine(self.orig_img, transform, (w_o, h_o))

        self.old_center = self.center
        # After transformation, center is at the middle of the transformed image
        self.center = (w_o//2, h_o//2)


    def getFoldNumber(self, x, y):
        """
        Get quadrant number by coordinates x, y (top left = 0, top right = 1, bottom left = 2, bottom right = 3)
        :param x: x coordinate
        :param y: y coordinate
        :return: coordinate number
        """
        center = self.center
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
    
    def smoothFold(self):
        """
        Apply a guided filter to remove noise 
        """
        self.info['avg_fold'] = self.info['avg_fold'].astype(np.float32)
        self.info["avg_fold_with_syn"] = self.info["avg_fold_with_syn"].astype(np.float32)

        if 'smooth_image' in self.info and self.info['smooth_image']:
            self.info['_avg_fold'] = cv2.ximgproc.guidedFilter(
                guide=self.info['avg_fold'],
                src=self.info['avg_fold'],
                radius=7,
                eps=1
            )
            self.info['_avg_fold_with_syn'] = cv2.ximgproc.guidedFilter(
                guide=self.info["avg_fold_with_syn"],
                src=self.info["avg_fold_with_syn"],
                radius=7,
                eps=1
            )
        else:
            self.info['_avg_fold'] = self.info['avg_fold']
            self.info['_avg_fold_with_syn'] = self.info['avg_fold_with_syn']


    def downsampleImage(self):
        self.info['_rmin'] = self.info['rmin'] // self.info['downsample'] if 'downsample' in self.info else self.info['rmin']
        self.info['_rmax'] = self.info['rmax'] // self.info['downsample'] if 'downsample' in self.info else self.info['rmax']
        self.info['_center'] = (self.center[0] // self.info['downsample'], self.center[1] // self.info['downsample']) if 'downsample' in self.info else self.center
        

        if 'downsample' in self.info and self.info['downsample'] > 1:
            factor = self.info['downsample']

            h, w = self.origSize
            new_w = w // factor
            new_h = h // factor
            h, w = self.info['_avg_fold'].shape[:2]
            new_w = w // factor
            new_h = h // factor

            self.info['_avg_fold'] = cv2.resize(self.info['_avg_fold'], (new_w, new_h), interpolation=cv2.INTER_AREA)
            self.info['_avg_fold_with_syn'] = cv2.resize(self.info['_avg_fold_with_syn'], (new_w, new_h), interpolation=cv2.INTER_AREA)


   
    def upsampleImage(self, img):
        factor = self.info['downsample']
        if 'downsample' in self.info and self.info['downsample'] > 1:
            h, w = img.shape[0] * factor, img.shape[1] * factor
            upsampled_img = cv2.resize(img, (w, h), interpolation=cv2.INTER_CUBIC)
            return upsampled_img
        else:
            return img
    
        
        
    def applyAngularBGSub(self):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg']
        """

        copy_img = copy.copy(self.info['_avg_fold'])
        center = [copy_img.shape[1]-1, copy_img.shape[0]-1]
        npt_rad = int(distance(center,(0,0)))
        det = find_detector(copy_img, man_det=self.info['detector']) if 'detector' in self.info else find_detector(copy_img)
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
        self.info['bgimg'] = result

    def applyCircularlySymBGSub2(self, fold, rmin, bgsub=1):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg']
        """

        img = self.makeFullImage(fold)
        img = img.astype("float32")
        width = img.shape[1]
        height = img.shape[0]

        ad = np.ravel(img)
        rmax = width+1

        if bgsub==2:
            bin_size = float(self.info["radial_bin2"])
            smoo = self.info["smooth2"]
            pc1 = self.info["cirmin2"] / 100.0
            pc2 = self.info["cirmax2"] / 100.0
            tension =self.info["tension2"]
        else:
            bin_size = float(self.info["radial_bin"])
            smoo = self.info["smooth"]
            pc1 = self.info["cirmin"] / 100.0
            pc2 = self.info["cirmax"] / 100.0
            tension =self.info["tension"]

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
            tension=tension,
            pc1=pc1,
            pc2=pc2
        )

        background = copy.copy(background)
        background[np.isnan(background)] = 0.
        background = np.array(background, dtype=np.float32)
        background = background.reshape((height, width))
        background = background[:fold.shape[0], :fold.shape[1]]
        
        return background



    def applySmoothedBGSub(self, fold, center, typ='gauss', bgsub=1):
        """
        Apply the Iterative Low Pass Filter Background Subtraction.
        :param typ: type of the subtraction, default to 'gauss', other option is 'boxcar'
        """


        img = self.makeFullImage(fold)

        if "roi_rad" in self.info: # if roi_rad is specified, use it
            roi_rad = int(self.info["roi_rad"])
            center_x = int(center[0])
            center_y = int(center[1])
            img = img[center_y - roi_rad:center_y + roi_rad, center_x - roi_rad:center_x + roi_rad]

        img = img.astype("float32")
        width = img.shape[1]
        height = img.shape[0]

        # Prepare options and parameter values based on 'typ'
        if typ == "gauss":
            filter_type = 'gaussian'
            kernel_size = (self.info["fwhm2"], self.info["fwhm2"]) if bgsub==2 else  (self.info["fwhm"], self.info["fwhm"])
            if kernel_size[0] % 2 == 0:
                kernel_size = (kernel_size[0] + 1, kernel_size[1] + 1)
            sigmaX = 0
        else:
            filter_type = 'boxcar'
            kernel_size = (self.info["boxcar_x2"], self.info["boxcar_y2"]) if bgsub==2 else (self.info["boxcar_x"], self.info["boxcar_y"])
            sigmaX = 0  # Set to zero for boxcar filter

        tension = None # Not used in the function
        edge_background = None  # You can provide edge background if available
        cycles = self.info["cycles2"] if bgsub==2 else self.info["cycles"]
        # Call bcksmooth function

        res = replicate_bcksmooth(
            image=img,
            max_iterations=cycles,
            filter_type=filter_type,
            kernel_size=kernel_size,
            sigmaX=sigmaX,
            tension=tension,
            edge_background=edge_background,
        )

        background = copy.copy(res)
        background[np.isnan(background)] = 0.0
        background = np.array(background, "float32")
        background = background.reshape((height, width))
        # replacing values that fall outside the roi_rad with the original values fromthe image

        if "roi_rad" in self.info:
            background = background[:height//2, :width//2]
            pad_y = max((fold.shape[0] - background.shape[0]), 0)
            pad_x = max((fold.shape[1] - background.shape[1]), 0)
            background = np.pad(background, ((pad_y, 0), (pad_x, 0)), 'constant', constant_values=0)
        else:
            background = background[:fold.shape[0], :fold.shape[1]]
        return background

    def applyWhiteTophat(self, img, radius):
        """
        Fast white top-hat using OpenCV
        """
        img32 = np.asarray(img, dtype=np.float32)
        r = int(round(radius))
        ksize = max(1, 2 * r + 1)
        kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (ksize, ksize))
        tophat = cv2.morphologyEx(img32, cv2.MORPH_TOPHAT, kernel, borderType=cv2.BORDER_REPLICATE)
        return img-tophat

    def applyRovingWindowBGSub(self, fold, center, bgsub=1):
        """
        Apply Roving Window background subtraction
        :return:
        """

        img = self.makeFullImage(fold)

        if "roi_rad" in self.info: # if roi_rad is specified, use it
            roi_rad = int(self.info["roi_rad"])
            center_x = int(center[0])
            center_y = int(center[1])
            img = img[center_y - roi_rad:center_y + roi_rad, center_x - roi_rad:center_x + roi_rad]

        width = img.shape[1]
        height = img.shape[0]
        img = np.ravel(img)
        buf = np.array(img, "f")

        if bgsub==2:
            iwid = self.info["win_size_x2"]
            jwid = self.info["win_size_y2"]
            isep = self.info["win_sep_x2"]
            jsep = self.info["win_sep_y2"]
            smoo = self.info["smooth2"]
            tension = self.info["tension2"]
            pc1 = self.info["cirmin2"] / 100.0
            pc2 = self.info["cirmax2"] / 100.0
        else:
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

        return b

    def applyCircularlySymBGSub(self):
        """
        Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg']
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
        self.info['bgimg'] = result

    def getRminmax(self):
        """
        Get R-min and R-max for background subtraction process. If these value is changed, background subtracted images need to be reproduced.
        """
        self.parent.statusPrint("Finding Rmin...")
        print("R-min is being calculated...")

        if 'fixed_rmin' in self.info:
            self.info['rmin'] = self.info['fixed_rmin']
        
        if 'fixed_rmax' in self.info:
            self.info['rmax'] = self.info['fixed_rmax']

        if 'rmin' in self.info and 'rmax' in self.info:
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
            
            if 'rmin' not in self.info:
                self.info['rmin'] = int(round(getFirstPeak(totalI) + 5))

            if 'rmax' not in self.info:
                # self.info['rmax'] = int(round((min(copy_img.shape[0], copy_img.shape[1]) - 1)))
                self.info['rmax'] = getDetectorEdge(totalI, end=(min(copy_img.shape[0], copy_img.shape[1]) - 1)) - 20
            
        self.deleteFromDict(self.info, 'bgimg') # remove "bgimg" from info to make it reprocess
        print(f"Done. R-min is {str(self.info['rmin'])} and R-max is set to max: {str(self.info['rmax'])}")

    def createMask(self):
        """
        Create a mask for the image based on the rmin and rmax values. The mask is stored in self.info['mask'].
        """

        mask = self._rminrmax_mask() * self._equator_peaks_mask() * self._equator_center_beam_mask() * self._equator_mask() 
        
        self.info['mask'] = mask.astype(int)

    def _rminrmax_mask(self):
        mask = create_circular_mask(self.orig_img.shape[0], self.orig_img.shape[1], inside=True, \
                                    radius=self.info['rmax'])
        mask *= create_circular_mask(self.orig_img.shape[0], self.orig_img.shape[1], inside=False, \
                                    radius=self.info['rmin'])

        return mask.astype(int)
    
    def _equator_mask(self):
        meridian = get_projection(self.orig_img, orientation=1, gap=2, offset=200)
        eq_fwhm = int(find_fwhm(meridian, rel_height=0.5))

        m1_peak = find_m_peak_auto(self.orig_img, m=1, rmin=self.info['rmin'])
        y_length = ((m1_peak * 2 - 10) +  eq_fwhm) //2
        y_length = max(30, y_length)
        print(f'Equator rectangle mask. M1 {m1_peak}. EQ FWHM: {eq_fwhm}. Average EQ Mask length {y_length}.')

        mask = create_rectangle_mask(self.orig_img.shape[0], self.orig_img.shape[1], x_length=self.orig_img.shape[1], y_length=y_length)
        return mask.astype(int)
    
    def _equator_peaks_mask(self):
        equator = get_projection(self.orig_img, gap=2, orientation=0, half=True) # right half

        peak_positions, _ = find_n_most_prominent_peaks(equator, n=self.info.get('n_peaks', 4))

        fwhm_values = []
        for peak in peak_positions:
            fwhm = find_fwhm(equator, peak_index=peak, rel_height=0.5)
            fwhm_values.append(fwhm)
        if len(fwhm_values) > 0:
            peak_width = int(np.mean(fwhm_values))*2
        else:
            peak_width = 30

        print(f'Equator peaks mask. Peak positions: {peak_positions}. Average FWHM: {peak_width//2}.')
        mask = create_peak_mask(self.orig_img.shape, peak_positions=peak_positions, peak_width=peak_width)
        return mask
    
    def _equator_center_beam_mask(self):
        height, width = self.orig_img.shape

        equator = get_projection(self.orig_img, gap=2, orientation=0, half=True)
        beam_width = find_first_valley(equator, start=self.info['rmin']+5) # TODO
        beam_width = 20 if not beam_width else beam_width
        print(f'Equator center beamstop mask width: {beam_width}')

        mask = create_circular_mask(height, width, inside=False, radius=beam_width)
        return mask.astype(int)

    def createArtificialData(self):
        AMP = 0.01
        SIGMA_X_DIV = 5
        SIGMA_Y_DIV = 20
        FREQ = 'medium'

        i0, i1 = find_i0_i1_peaks_auto(self.orig_img, rmin=30)
        i0 = 100 if abs(i0-100) > 50 else i0
        m1 = find_m_peak_auto(self.orig_img, m=1, rmin=30)
        m1 = 50 if abs(m1-50) > 50 else m1

        if FREQ == 'sparce':
            offset_x = int(i0 / 2)
            step_x = int(i0*2)
            offset_y = int(m1 / 2)
            step_y = int(m1*2)
        elif FREQ == 'medium':
            offset_x = int(i0 / 2)
            step_x = int(i0)
            offset_y = int(m1 / 2)
            step_y = int(m1)
        elif FREQ == 'dense':
            offset_x = int(i0 / 4)
            step_x = int(i0 / 2)
            offset_y = int(m1 / 4)
            step_y = int(m1 / 2)
    
        grid = get_grid(image_shape=self.orig_img.shape, 
            step_x=step_x, step_y=step_y, 
            offset_x=offset_x, offset_y=offset_y, fold=True)
        
        equator_half = get_projection(self.orig_img, gap=2, orientation=0, half=True)

        amplitude = equator_half[i0] * AMP * i0
        amplitude = 4000 if amplitude < 4000 else amplitude

        sigma_x = i0 / SIGMA_X_DIV / (2 * np.sqrt(2 * np.log(2)))
        sigma_y = m1 / SIGMA_Y_DIV / (2 * np.sqrt(2 * np.log(2)))

        gauss_data = GaussianArtificialData(image_shape=self.orig_img.shape)
        gauss_data.define_parameters(sigma_x=sigma_x, sigma_y=sigma_y, amplitude=amplitude)
        gauss_data.pre_compute_kernel()

        mask_data = MaskArtificialData(image_shape=self.orig_img.shape)
        mask_data.define_parameters(sigma_x=sigma_x, sigma_y=sigma_y, mult=2)

        for point in grid:
            gauss_data.create_image(point[0], point[1])
            mask_data.create_image(point[0], point[1])

        gauss_data.apply_intencity_decrease()

        self.info["synthetic_data"] = gauss_data.get_faded_image()
        self.info["synthetic_mask"] = mask_data.get_mask()
        syn_data_top_left = self.info["synthetic_data"][:self.info['avg_fold'].shape[0], :self.info['avg_fold'].shape[1]]
        self.info["avg_fold_with_syn"] = self.info['avg_fold'] + syn_data_top_left



    def apply2DConvexhull(self, copy_img, rmin, step=1):
        """
        Apply 2D Convex hull Background Subtraction to average fold, and save the result to self.info['bgimg']
        """
        center = [copy_img.shape[1] - 1, copy_img.shape[0] - 1]
        rmax = copy_img.shape[0] + 10

        hist_x = list(np.arange(rmin, rmax + 1))
        pchiplines = []

        det = "agilent_titan"
        npt_rad = int(distance(center, (0, 0)))
        ai = AzimuthalIntegrator(detector=det)
        ai.setFit2D(100, center[0], center[1])

        integration_method = IntegrationMethod.select_one_available("csr", dim=1, default="csr", degradable=True)
        step = 1 if step not in [0.5, 1, 2, 3, 5, 9, 10, 15, 18] else step
        for deg in np.arange(180, 270 + step, step):

            if deg == 180 :
                start_deg = 180
                end_deg = 180 + step/2
            elif deg >= 270:
                start_deg=270 - step/2
                end_deg=270
            else:
                start_deg=deg-step/2
                end_deg=deg+step/2

            # Integrate the image and base image to get the volume
            _, I = ai.integrate1d(copy_img, npt_rad, unit="r_mm", method=integration_method, azimuth_range=(start_deg, end_deg), correctSolidAngle=False)
            hist_y = I[int(rmin):int(rmax+1)]
            hist_y = list(np.concatenate((hist_y, np.zeros(len(hist_x) - len(hist_y)))))

            hull_x, hull_y = getHull(hist_x, hist_y)
            y_pchip = pchip(hull_x, hull_y, hist_x)
            pchiplines.append(y_pchip)

        # Smooth each histogram by radius
        pchiplines = np.array(pchiplines, dtype="float32")
        pchiplines2 = convolve1d(pchiplines, [1,2,1], axis=0)/4.

        # Smooth between neighboring histograms
        pchiplines3 = weighted_neighborhood_average(pchiplines2, weights=[0.25, 0.5, 0.25])

        # Produce Background from each pchip line
        background = qfu.make2DConvexhullBG2(pchiplines3, copy_img.shape[1], copy_img.shape[0], center[0], center[1], rmin, rmax, step)

        # Smooth background image by gaussian filter
        s = 10
        w = 4
        t = (((w - 1.) / 2.) - 0.5) / s
        background = gaussian_filter(background, sigma=s, truncate=t)
        return background

    def calculateAvgFold(self):
        """
        Calculate an average fold for 1-4 quadrants. Quadrants are splitted by center and rotation
        """
        self.parent.statusPrint("Calculating Avg Fold...")
        rotate_img = self.orig_img
        center = self.center
        center_x = int(center[0])
        center_y = int(center[1])

        # get top left quandrant
        img_height = rotate_img.shape[0]
        img_width = rotate_img.shape[1]
        fold_width = max(int(center[0]), img_width-int(center[0]))
        fold_height = max(int(center[1]), img_height-int(center[1]))

        if img_width < center_x or img_height < center_y:
            print("ALERT: THE CENTER IS NOT INSIDE OF THE IMAGE!  THIS WILL LIKELY CAUSE AN ERROR")

        top_left = rotate_img[max(center_y-fold_height,0):center_y, max(center_x-fold_width,0):center_x]
        
        # Use top left quadrant as average fold if folding is disabled
        if self.info['fold_image'] == False:
            print("Folding is disabled. Using top left quadrant as average fold...")
            self.info['folded'] = False
            self.info['avg_fold'] = top_left

        elif 'avg_fold' not in self.info.keys():
            self.deleteFromDict(self.info, 'rmin')
            self.deleteFromDict(self.info, 'rmax')
            print("Quadrant folding is being processed...")        

            top_right = rotate_img[max(center_y-fold_height,0):center_y, center_x:center_x+fold_width]
            top_right = cv2.flip(top_right,1)

            buttom_left = rotate_img[center_y:center_y+fold_height, max(center_x-fold_width,0):center_x]
            buttom_left = cv2.flip(buttom_left,0)

            buttom_right = rotate_img[center_y:center_y+fold_height, center_x:center_x+fold_width]
            buttom_right = cv2.flip(buttom_right,1)
            buttom_right = cv2.flip(buttom_right,0)

            # Add all folds which are not ignored
            # Initialize quadrants array with invalid threshold (marks empty/invalid pixels)
            quadrants = np.ones((4, fold_height, fold_width), rotate_img.dtype) * INVALID_PIXEL_THRESHOLD
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
            # Pass invalid threshold: pixels <= threshold are excluded from averaging
            result = qfu.get_avg_fold_float32(np.array(quadrants, dtype="float32"), len(quadrants), fold_height, fold_width,
                                                INVALID_PIXEL_THRESHOLD)

        self.info['avg_fold'] = result
        self.info['folded'] = True

    def applyBackgroundSubtraction(self):
        """
        Apply background subtraction by user's choice.
        - bgimg : fold after applying background subtraction
        """
        self.parent.statusPrint("Applying Background Subtraction...")
        method = self.info["bgsub"]
        print(f"Background Subtraction is being processed... Method: {method}")


        # Produce bgimg
        if "bgimg" not in self.info:
            tmp_avg_fold = np.array(self.info['_avg_fold'], dtype="float32")
            avg_fold = np.array(self.info['avg_fold'], dtype="float32")
            tmp_rmin = self.info['_rmin']
            rmin = self.info['rmin']
            tmp_center = self.info['_center']

            # bg is the downsampled background image
            if method == 'None':
                bg = np.zeros_like(avg_fold)
            elif method == '2D Convexhull':
                bg = self.apply2DConvexhull(tmp_avg_fold, tmp_rmin, self.info['deg1'])
            elif method == 'Circularly-symmetric':
                bg = self.applyCircularlySymBGSub2(tmp_avg_fold, tmp_rmin)
            elif method == 'White-top-hats':
                bg = self.applyWhiteTophat(tmp_avg_fold, self.info["tophat1"])
            elif method == 'Roving Window':
                bg = self.applyRovingWindowBGSub(tmp_avg_fold, tmp_center)
            elif method == 'Smoothed-Gaussian':
                bg = self.applySmoothedBGSub(tmp_avg_fold, tmp_center, 'gauss')
            elif method == 'Smoothed-BoxCar':
                bg = self.applySmoothedBGSub(tmp_avg_fold, tmp_center, 'boxcar')
            else:
                bg = avg_fold
                method = 'None'
            
            # upsample background and subtract from original image if downsampling is used
            if 'downsample' in self.info and self.info['downsample'] > 1 and method != 'None':
                print(f"Upsampling background from downsampled version by factor {self.info['downsample']}...")
                bg = self.upsampleImage(bg)
            
            if method != 'None':
                result = np.array(avg_fold - bg, dtype=np.float32)
                result = qfu.replaceRmin(result, int(rmin), 0.)
            else:
                result = avg_fold
            self.info["bgimg"] = result
        
        self.imgCache['BgSubFold'] = copy.copy(self.info["bgimg"])
        self.deleteFromDict(self.imgCache, "resultImg")
        print("Done.")

    def applyBackgroundSubtractionSynthetic(self):
        """
        Apply background subtraction by user's choice.
        - bgimg : fold after applying background subtraction
        """
        print("Applying Background Subtraction to synthetic data... method: " + self.info["bgsub"])
        method = self.info["bgsub"]
        tmp_rmin = self.info['_rmin']
        tmp_center = self.info['_center']

        avg_fold_with_syn = np.array(self.info['avg_fold_with_syn'], dtype="float32")
        tmp_avg_fold_with_syn = np.array(self.info['_avg_fold_with_syn'], dtype="float32")
    
        if method == 'None':
            bg_syn = np.zeros_like(tmp_avg_fold_with_syn)
        elif method == '2D Convexhull':
            bg_syn = self.apply2DConvexhull(tmp_avg_fold_with_syn, tmp_rmin, self.info['deg1'])
        elif method == 'Circularly-symmetric':
            bg_syn = self.applyCircularlySymBGSub2(tmp_avg_fold_with_syn, tmp_rmin)
        elif method == 'White-top-hats':
            bg_syn = self.applyWhiteTophat(tmp_avg_fold_with_syn, self.info["tophat1"])
        elif method == 'Roving Window':
            bg_syn = self.applyRovingWindowBGSub(tmp_avg_fold_with_syn, tmp_center)
        elif method == 'Smoothed-Gaussian':
            bg_syn = self.applySmoothedBGSub(tmp_avg_fold_with_syn, tmp_center, 'gauss')   
        elif method == 'Smoothed-BoxCar':
            bg_syn = self.applySmoothedBGSub(tmp_avg_fold_with_syn, tmp_center, 'boxcar')
        else:
            bg_syn = tmp_avg_fold_with_syn
            method = 'None'
        
        if 'downsample' in self.info and self.info['downsample'] > 1 and method != 'None':
            bg_syn = self.upsampleImage(bg_syn)
        
        if method != 'None':
            result = np.array(avg_fold_with_syn - bg_syn, dtype=np.float32)
            result = qfu.replaceRmin(result, int(tmp_rmin), 0.)
        else:
            result = avg_fold_with_syn
        self.info["bgimg_syn"] = result
    

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

        scale = 1 if 'scale' not in self.info else self.info['scale']

        h, w = result.shape
        center = (w//2, h//2)
        M = cv2.getRotationMatrix2D(center, 0, 1/scale)

        result_scaled = cv2.warpAffine(result, M, (w, h))

        self.imgCache['resultImg'] = result_scaled
        print("Done.")


    def evaluateResult(self):
        """
        Evaluate the result by calculating the loss metrics on the result image and background. 
        """
        self.parent.statusPrint("Evaluating Result...")
        print("Evaluating result image...")
        if 'resultImg' not in self.imgCache:
            print("Result image not found. Please generate the result image first.")
            return

        result = self.imgCache['resultImg']
        bg = self.orig_img - result
        baseline = get_radial_average_rmax(result+bg, self.info['rmax'], band_width=30)*0.2
        syn_srt = self.info.get('synthetic_data', None)
        syn_mask = self.info.get('synthetic_mask', None)
        syn_fold = self.info.get('bgimg_syn', None)
        syn_img = self.makeFullImage(syn_fold) if syn_fold is not None else None

        metrics = full_eval_metrics(result, bg, art_img=syn_img, art_str=syn_srt, mask_art=syn_mask, \
                                    mask_equator=self.info['mask'], baseline_value=baseline)
        
        print("Evaluation Metrics:")
        for key, value in metrics.items():
            print(f"{key}: {value:.4f}")




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

        bottom_left = cv2.flip(fold, 0)
        bottom_right = cv2.flip(bottom_left, 1)

        resultImg = np.zeros((fold_height * 2, fold_width * 2))
        resultImg[0:fold_height, 0:fold_width] = top_left
        resultImg[0:fold_height, fold_width:fold_width * 2] = top_right
        resultImg[fold_height:fold_height * 2, 0:fold_width] = bottom_left
        resultImg[fold_height:fold_height * 2, fold_width:fold_width * 2] = bottom_right

        return resultImg

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        print(text)
