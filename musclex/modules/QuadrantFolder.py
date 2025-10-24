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
except: # for coverage
    from modules import QF_utilities as qfu
    from utils.file_manager import fullPath, createFolder, getBlankImageAndMask, getMaskOnly
    from utils.histogram_processor import *
    from utils.image_processor import *

# Make sure the cython part is compiled
# from subprocess import call
# call(["python setup2.py build_ext --inplace"], shell = True)

class QuadrantFolder:
    """
    A class for Quadrant Folding processing - go to process() to see all processing steps
    """
    def __init__(self, img, img_path, img_name, parent, suppress_signals=False):
        """
        Initialize QuadrantFolder with an already-loaded image array, plus metadata.
        :param img: numpy ndarray image data
        :param img_path: directory path for caches and outputs
        :param img_name: display/file name used for caches and outputs
        :param parent: GUI/owner for status updates
        :param suppress_signals: If True, suppress GUI signals (e.g., during batch processing to avoid race conditions)
        """
        self.orig_img = np.asarray(img).astype("float32")
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
        
        # Flag to suppress signals during batch processing to avoid race conditions
        self.suppress_signals = suppress_signals
        self.newImgDimension = None

        # info dictionary will save all results
        if cache is not None:
            self.info = cache
        else:
            self.info = {}

        self.info.setdefault("transform", None)

        # To display rotation angle relative to the original image.
        self.info.setdefault("angle_to_origin", 0.0)

        # To display cursor point in original image.
        self.info.setdefault("inv_transform", None)

        # Current center (will be set by findCenter or manually by GUI)
        self.center = None
        
        # Current rotation (will be set by process or manually by GUI)
        self.rotation = None
        
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
        
        # Save configuration fingerprint for cache validation
        self.info['config_fingerprint'] = self._getConfigFingerprint()

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
                    # Validate cache against current configuration fingerprint
                    current_fingerprint = self._getConfigFingerprint()
                    cached_fingerprint = info.get('config_fingerprint', {})
                    
                    if current_fingerprint != cached_fingerprint:
                        changes = self._diffFingerprints(cached_fingerprint, current_fingerprint)
                        print("Configuration changed. Invalidating cache and reprocessing.")
                        if changes:
                            print(f"  Changed items: {changes}")
                        return None
                    
                    return info
                print("Cache version " + info['program_version'] + " did not match with Program version " + self.version)
                print("Invalidating cache and reprocessing the image")
        return None
    
    def _getConfigFingerprint(self):
        """
        Generate a fingerprint of all configuration files that affect processing.
        Returns a dict with file paths and their modification times and sizes.
        """
        from pathlib import Path
        
        fingerprint = {}
        settings_dir = Path(self.img_path) / "settings"
        
        # List of config files to track
        config_files = [
            'blank_image_settings.json',
            'mask.tif',
            'mask_config.json',
            '.blank_image_disabled',
            '.mask_disabled'
        ]
        
        for config_file in config_files:
            file_path = settings_dir / config_file
            if file_path.exists():
                # Use modification time + file size as fingerprint
                # (faster than hashing large files like mask.tif)
                stat = file_path.stat()
                fingerprint[config_file] = {
                    'mtime': stat.st_mtime,
                    'size': stat.st_size
                }
            else:
                # Track that file doesn't exist (important for disabled flags)
                fingerprint[config_file] = None
        
        return fingerprint
    
    def _diffFingerprints(self, old_fp, new_fp):
        """
        Compare two fingerprints and return a list of what changed.
        """
        changes = []
        all_keys = set(old_fp.keys()) | set(new_fp.keys())
        
        for key in all_keys:
            old_val = old_fp.get(key)
            new_val = new_fp.get(key)
            
            if old_val != new_val:
                if old_val is None:
                    changes.append(f"{key} (added)")
                elif new_val is None:
                    changes.append(f"{key} (removed)")
                else:
                    changes.append(f"{key} (modified)")
        
        return changes

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

    def _emit_center_signal(self, center):
        """
        Emit signal to update GUI with center coordinates.
        """
        if self.parent and not self.suppress_signals:
            # Ensure center is a tuple for signal consistency
            center_tuple = tuple(center) if not isinstance(center, tuple) else center
            self.parent.eventEmitter.imageCenterChangedSignal.emit(center_tuple)


    def add_transform(self, M):
        prev_transform = self.info.get("transform")

        if prev_transform is not None:
            self.info["transform"] = self.merge_warps((prev_transform, M))
        else:
            self.info["transform"] = M

        # To display cursor point in original image.
        self.info["inv_transform"] = cv2.invertAffineTransform(self.info["transform"])

    def merge_warps(self, warps):
        M_total = np.eye(3, dtype=np.float32)

        for W in warps:
            # Convert 2x3 -> 3x3
            W_h = np.vstack([W, [0, 0, 1]])
            # Multiply (note: last one in list applies last)
            M_total = W_h @ M_total

        # Convert back to 2x3
        M = M_total[:2, :]
        return M

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
        self.getRotationAngle()
        self.transformImage()
        self.calculateAvgFold()
        if flags['fold_image'] == False:
            self.info['folded'] = False

            # get top left quandrant
            #rotate_img = copy.copy(self.getRotatedImage())
            rotate_img = copy.copy(self.start_img)
            center = self.center
            center_x = int(center[0])
            center_y = int(center[1])
            img_height = rotate_img.shape[0]
            img_width = rotate_img.shape[1]
            fold_width = max(int(center[0]), img_width-int(center[0])) # max(max(int(center[0]), img_width-int(center[0])), max(int(center[1]), img_height-int(center[1])))
            fold_height = max(int(center[1]), img_height-int(center[1])) # fold_width
            top_left = rotate_img[max(center_y-fold_height,0):center_y, max(center_x-fold_width,0):center_x]

            self.info['avg_fold'] = top_left


            #self.initImg = self.orig_img

            # if self.initImg is not None:
            #     self.info['avg_fold'] = self.initImg
            # else:
            #     self.info['avg_fold'] = self.orig_img
        self.getRminmax()
        self.getTransitionRad()

        try:
            self.applyBackgroundSubtraction()
        except Exception as e:
            print("ERROR: Background subtraction failed.")
            print(e)
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
        self.orig_img = copy.copy(self.start_img)

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
            self.info['mask_thres'] = getMaskThreshold(self.start_img)
        if 'ignore_folds' not in self.info:
            self.info['ignore_folds'] = set()
        if 'bgsub' not in self.info:
            self.info['bgsub'] = 'None'
        if 'bgsub2' not in self.info:
            self.info['bgsub2'] = 'None'


    def applyBlankImageAndMask(self):
        """
        Apply the blank image and mask threshold on the orig_img
        :return: -
        """

        # Check if we need to apply blank image or mask
        should_apply_blank = 'blank_mask' in self.info and self.info['blank_mask']
        should_apply_mask = 'apply_mask' in self.info and self.info['apply_mask']
        
        if should_apply_blank or should_apply_mask:
            img = np.array(self.start_img, 'float32')
            
            # Apply blank image if enabled
            if should_apply_blank:
                blank, _, blank_weight = getBlankImageAndMask(self.img_path, return_weight=True)
                if blank is not None:
                    img = img - blank * blank_weight
                    print(f"Applied blank image subtraction with weight: {blank_weight}")
            
            # Apply mask if enabled
            if should_apply_mask:
                mask = getMaskOnly(self.img_path)
                if mask is not None:
                    print("Applying mask from mask.tif")
                    img[mask == 0] = self.info['mask_thres'] - 1

            self.orig_img = img


    def findCenter(self):
        """
        Find the center in original image coordinates
        Sets self.center based on: pre-set (manual) > cached auto_center > calculate new
        GUI will set self.center before calling this if manual mode is active
        """
        self.parent.statusPrint("Finding Center...")
        
        # Priority 1: Use pre-set center (manual mode, set by GUI)
        if self.center is not None:
            print(f"Using pre-set center: {self.center}")
            return
        
        # Priority 2: Use cached auto center
        if 'auto_center' in self.info:
            self.center = tuple(self.info['auto_center'])
            print(f"Using cached auto center: {self.center}")
            self._emit_center_signal(self.center)
            return

        # Priority 3: Calculate new center and cache it
        print("Calculating new center...")
        self.orig_image_center = getCenter(self.orig_img)
        self.orig_img, calculated_center = processImageForIntCenter(self.orig_img, self.orig_image_center)

        # Cache the calculated center (for next time)
        self.info['auto_center'] = tuple(calculated_center)
        # Set as current center
        self.center = tuple(calculated_center)
        print(f"Calculated and cached center: {self.center}")
        self._emit_center_signal(self.center)


    def getRotationAngle(self):
        """
        Figures out the rotation angle to use on the image.
        Uses self.rotation for manual rotation, or calculates and caches auto rotation.
        """
        self.parent.statusPrint("Finding Rotation Angle...")

        # Priority 1: Use manual rotation if set by GUI
        if self.rotation is not None:
            print(f"Using manual rotation: {self.rotation}")
            return
        
        # Priority 2: Use cached auto rotation
        if 'auto_rotation' in self.info:
            self.rotation = self.info['auto_rotation']
            print(f"Using cached auto rotation: {self.rotation}")
            return

        print("Rotation Angle is being calculated ... ")
        # Selecting disk (base) image and corresponding center for determining rotation as for larger images (formed from centerize image) rotation angle is wrongly computed
        _, center = self.getExtentAndCenter()
        img = copy.copy(self.initImg) if self.initImg is not None else copy.copy(self.orig_img)
        if 'detector' in self.info:
            calculated_rotation = getRotationAngle(img, center, self.info['orientation_model'], man_det=self.info['detector'])
        else:
            calculated_rotation = getRotationAngle(img, center, self.info['orientation_model'])

        # Cache the calculated rotation (for next time)
        self.info['auto_rotation'] = calculated_rotation
        # Set as current rotation
        self.rotation = calculated_rotation

        self.deleteFromDict(self.info, 'avg_fold')
        print("Done. Rotation Angle is " + str(calculated_rotation) +" degree")

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
        Applies Tranlation, scaling and rotation to the original image.
        This performs the functions previously done by CenterizeImage and RotateImage
        """
        h_o, w_o = self.orig_img.shape
        orig_x, orig_y = w_o//2, h_o//2
        x, y = self.center

        # corners = [
        #     (-x,      -y),
        #     ( w_o - x,  -y),
        #     ( w_o - x,   h_o - y),
        #     (-x,       h_o - y)
        # ]

        angle = self.rotation if self.rotation is not None else 0.0

        # cos, sin = math.cos(angle * math.pi / 180), math.sin(angle * math.pi / 180)

        # rot_pts = [
        #     (cx * cos - cy * sin, cx * sin + cy * cos)
        #     for cx, cy in corners
        # ]


        # max_rx = max([abs(x) for x, y in rot_pts])
        # max_ry = max([abs(y) for x, y in rot_pts])


        # fit‐to‐frame scale (never upscale beyond 1.0)
        # jiongjiong: Disable scale.
        # scale = min((w_o/2) / max_rx, (h_o/2) / max_ry, 1.0)
        scale = 1.0

        self.info['scale'] = scale

        tx = (w_o/2) - scale * x
        ty = (h_o/2) - scale * y

        self.new_tx, self.new_ty = tx, ty

        # build M1 = S(fit_scale) about center + recenter
        M1  = np.array([[scale, 0,         tx],
                        [0,         scale, ty]],
                    dtype=np.float32)

        self.centImgTransMat = M1
        self.add_transform(M1)

        # cent_img = cv2.warpAffine(self.orig_img, M1, (w_o, h_o))

        M2 = cv2.getRotationMatrix2D(
            (w_o/2, h_o/2),
            angle,
            1
        )

        self.add_transform(M2)

        transform = self.info.get("transform")

        assert transform is not None, f"transform is None after adding {M1} and {M2}!"

        self.orig_img = cv2.warpAffine(self.orig_img, transform, (w_o, h_o))

        self.info.setdefault("angle_to_origin", 0.0)
        self.info["angle_to_origin"] += angle

        # Only emit signal if not suppressed (e.g., during batch processing)
        if self.parent and not self.suppress_signals:
            self.parent.eventEmitter.angleChangedSignal.emit(self.info["angle_to_origin"])

        # new_center = [x - (tx * cos - ty * sin), y - (tx * sin + ty * cos)]

        self.old_center = self.center
        # After transformation, center is at the middle of the transformed image
        self.center = (w_o//2, h_o//2)

        self.rotation = 0.0

    def getRotatedImage(self):
        """
        Get rotated image by angle while image = original input image, and angle = self.rotation
        """
        img = np.array(self.orig_img, dtype="float32")
        center = self.center
        if self.center_before_rotation is not None:
            center = self.center_before_rotation
        else:
            self.center_before_rotation = center

        h, w = img.shape
        angle = self.rotation if self.rotation is not None else 0.0
        rotImg, newCenter, self.rotMat = rotateImage(img, center, angle)

        # Cropping off the surrounding part since we had already expanded the image to maximum possible extent in centerize image
        hnew, wnew = rotImg.shape
        dh, dw = (hnew - h)//2, (wnew-w)//2
        final_rotImg = rotImg[dh:h + dh, dw:w + dw]
        self.center = (newCenter[0]-dw, newCenter[1]-dh)
        self.dl, self.db = dw, dh # storing the cropped off section to recalculate coordinates when manual center is given

        self.curr_dims = final_rotImg.shape
        return final_rotImg


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

    def applyCircularlySymBGSub2(self, bgsub=1):
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
        background = np.array(background, 'float32')
        background = background.reshape((height, width))
        background = background[:fold.shape[0], :fold.shape[1]]
        result = np.array(fold - background, dtype=np.float32)
        result = qfu.replaceRmin(result, int(rmin), 0.)
        return result



    def applySmoothedBGSub(self, typ='gauss', bgsub=1):
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
        center = self.center

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

        return result


    def applyRovingWindowBGSub(self, bgsub=1):
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
        center = self.center

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

        result = np.array(fold - b, dtype=np.float32)
        result = qfu.replaceRmin(result, int(self.info["rmin"]), 0.0)

        return result

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
        self.parent.statusPrint("Finding Rmin...")
        print("R-min is being calculated...")

        if 'fixed_rmin' in self.info:
            self.info['rmin'] = self.info['fixed_rmin']
        elif 'rmin' in self.info:
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

            self.info['rmin'] = int(round(self.getFirstPeak(totalI) + 10))
            self.info['rmax'] = int(round((min(copy_img.shape[0], copy_img.shape[1]) - 1)))

        self.deleteFromDict(self.info, 'bgimg1') # remove "bgimg1" from info to make it reprocess
        self.deleteFromDict(self.info, 'bgimg2') # remove "bgimg2" from info to make it reprocess
        print("Done. R-min is "+str(self.info['rmin']))
        if 'rmax' in self.info:
            print(" and R-max is set to max: " + str(self.info['rmax']))

    def getTransitionRad(self):
        if 'transition_radius' not in self.info or self.info['transition_radius']  < 0:
            self.info['transition_radius'] = self.orig_img.shape[0] // 5
            self.info['transition_delta'] = 60
        print(f"[INFO] Transition Radius: {self.info['transition_radius']} Transition Delta: {self.info['transition_delta']}")

    def apply2DConvexhull(self, copy_img, rmin, step=1):
        """
        Apply 2D Convex hull Background Subtraction to average fold, and save the result to self.info['bgimg1']
        """
        center = [copy_img.shape[1] - 1, copy_img.shape[0] - 1]
        rmax = copy_img.shape[0] + 10

        # print(f"[IK] ConvexHull: rmin {rmin}, rmax {rmax}, step deg: {step}")
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

        # Subtract original average fold by background
        result = copy_img - background
        return result

    def calculateAvgFold(self):
        """
        Calculate an average fold for 1-4 quadrants. Quadrants are splitted by center and rotation
        """
        self.parent.statusPrint("Calculating Avg Fold...")
        if 'avg_fold' not in self.info.keys():
            self.deleteFromDict(self.info, 'rmin')
            self.deleteFromDict(self.info, 'rmax')
            # self.imgResultForDisplay = None
            rotate_img = self.orig_img #copy.copy(self.getRotatedImage())
            center = self.center
            center_x = int(center[0])
            center_y = int(center[1])

            print("Quadrant folding is being processed...")
            img_width = rotate_img.shape[1]
            img_height = rotate_img.shape[0]
            fold_width = max(int(center[0]), img_width-int(center[0])) # max(max(int(center[0]), img_width-int(center[0])), max(int(center[1]), img_height-int(center[1])))
            fold_height = max(int(center[1]), img_height-int(center[1])) # fold_width

            if img_width < center_x or img_height < center_y:
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
        method = self.info["bgsub"]
        method2 = self.info["bgsub2"]
        print(f"Background Subtraction is being processed... In method: {method}, Out method: {method2}")


        # Produce bgimg1
        if "bgimg1" not in self.info:
            avg_fold = np.array(self.info['avg_fold'], dtype="float32")
            if method == 'None':
                self.info["bgimg1"] = avg_fold # if method is None, original average fold will be used
            elif method == '2D Convexhull':
                self.info["bgimg1"] = self.apply2DConvexhull(avg_fold, self.info['rmin'], self.info['deg1'])
            elif method == 'Circularly-symmetric':
                self.info["bgimg1"] = self.applyCircularlySymBGSub2(bgsub=1)
            elif method == 'White-top-hats':
                self.info["bgimg1"] = white_tophat(avg_fold, disk(self.info["tophat1"]))
            elif method == 'Roving Window':
                self.info["bgimg1"] = self.applyRovingWindowBGSub(bgsub=1)
            elif method == 'Smoothed-Gaussian':
                self.info["bgimg1"] = self.applySmoothedBGSub('gauss', bgsub=1)
            elif method == 'Smoothed-BoxCar':
                self.info["bgimg1"] = self.applySmoothedBGSub('boxcar', bgsub=1)
            else:
                self.info["bgimg1"] = avg_fold
            self.deleteFromDict(self.imgCache, "BgSubFold")

        # Produce bgimg2
        if "bgimg2" not in self.info:
            avg_fold = np.array(self.info['avg_fold'], dtype="float32")
            if method2 == 'None':
                self.info["bgimg2"] = avg_fold # if method is 'None', original average fold will be used
            elif method2 == '2D Convexhull':
                self.info["bgimg2"] = self.apply2DConvexhull(avg_fold, self.info['transition_radius']-self.info['transition_delta']//2-1, self.info['deg2'])
            elif method == 'Circularly-symmetric':
                self.info["bgimg2"] = self.applyCircularlySymBGSub2(bgsub=2)
            elif method2 == 'White-top-hats':
                self.info["bgimg2"] = white_tophat(avg_fold, disk(self.info["tophat2"]))
            elif method == 'Roving Window':
                self.info["bgimg2"] = self.applyRovingWindowBGSub(bgsub=2)
            elif method2 == 'Smoothed-Gaussian':
                self.info["bgimg2"] = self.applySmoothedBGSub('gauss', bgsub=2)
            elif method2 == 'Smoothed-BoxCar':
                self.info["bgimg2"] = self.applySmoothedBGSub('boxcar', bgsub=2)
            else:
                self.info["bgimg2"] = avg_fold
            self.deleteFromDict(self.imgCache, "BgSubFold")
        print("Done.")

    def mergeImages(self):
        """
        Merge bgimg1 and bgimg2 at merge radius, with sigmoid as a merge gradient param.
        The result of merging will be kept in self.info["BgSubFold"]
        :return:
        """
        self.parent.statusPrint("Merging Images...")

        if "BgSubFold" not in self.imgCache:
            img1 = np.array(self.info["bgimg1"], dtype="float32")
            img2 = np.array(self.info["bgimg2"], dtype="float32")

            center = [img1.shape[1]-1, img1.shape[0]-1]
            rad = self.info["transition_radius"]
            delta = self.info["transition_delta"]

            # Merge 2 images at merge radius using transition radius and delta
            self.imgCache['BgSubFold'] = qfu.combine_bgsub_linear_float32(img1, img2, center[0], center[1], rad, delta)

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

        scale = 1 if 'scale' not in self.info else self.info['scale']

        h, w = result.shape
        center = (w//2, h//2)
        M = cv2.getRotationMatrix2D(center, 0, 1/scale)

        result_scaled = cv2.warpAffine(result, M, (w, h))

        self.imgCache['resultImg'] = result_scaled
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
