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
import hashlib
import json
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
    from ..utils.file_manager import fullPath, createFolder
    from ..utils.histogram_processor import *
    from ..utils.image_processor import *
    from ..utils.image_data import ImageData
except: # for coverage
    from modules import QF_utilities as qfu
    from utils.file_manager import fullPath, createFolder
    from utils.histogram_processor import *
    from utils.image_processor import *
    from utils.image_data import ImageData

# Make sure the cython part is compiled
# from subprocess import call
# call(["python setup2.py build_ext --inplace"], shell = True)

# Invalid pixel threshold constant
# Pixels with values <= this threshold are considered invalid (masked/gap pixels)
# and are excluded from averaging calculations
INVALID_PIXEL_THRESHOLD = -1

# Bumped whenever the on-disk cache schema changes in a way that older
# caches must be discarded (e.g. switched away from storing image arrays
# in the pickle to fingerprint-only caches that point at the canonical
# qf_results/<name>_folded.tif). Older caches without this exact value
# are dropped on load and the image is reprocessed.
CACHE_FORMAT_VERSION = 2

# Filename suffixes that the fast-path will try (in order) to reload a
# previously-computed result image. Both must be UNCROPPED (cropping is
# destructive and would break GUI coordinate handling on reload). LZW
# compression is lossless, so a compressed tif round-trips bit-for-bit.
FAST_PATH_RESULT_SUFFIXES = ("_folded.tif", "_folded_compressed.tif")

class QuadrantFolder:
    """
    A class for Quadrant Folding processing - go to process() to see all processing steps
    """
    def __init__(self, image_data: ImageData, parent=None, output_dir=None):
        """
        Initialize QuadrantFolder with ImageData container.
        
        :param image_data: ImageData container with image data and preprocessing
        :param parent: GUI/owner for status updates
        :param output_dir: directory for cache/results writes (defaults to input dir)
        """
        # Get working image from ImageData (with blank/mask already applied)
        self.orig_img = image_data.get_working_image()
        self.img_path = str(image_data.img_path)
        self.output_dir = output_dir if output_dir else self.img_path
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

    # Keys excluded from the persistent disk cache.
    #
    # Two categories:
    #   1) Transient per-session selections (ROI). Dragging/editing should
    #      not silently affect future sessions or other images. If the user
    #      wants the same ROI across images they tick "Persist ROI size",
    #      which pushes the value as a flag at processing time.
    #
    #   2) Image-bearing intermediates (avg_fold / bgimg1 / bgimg2 / bg_line).
    #      The canonical persisted image is qf_results/<name>_folded.tif;
    #      caching arrays in the pickle is pure duplication and the main
    #      reason the on-disk cache used to be ~12 MB/img. With a complete
    #      parameter fingerprint the fast-path can decide validity from
    #      params alone and just reload the tif.
    _NON_CACHED_KEYS = (
        # Transient ROI
        'roi_w', 'roi_h',
        # Image-bearing intermediates (now persisted only via _folded.tif)
        'avg_fold', 'bgimg1', 'bgimg2', 'bg_line',
    )

    # Image-array keys: these are outputs of processing, not inputs, so
    # they must not enter the fingerprint (otherwise the fingerprint
    # would change every run even if the params didn't).
    _IMAGE_ARRAY_KEYS = ('avg_fold', 'bgimg1', 'bgimg2', 'bg_line')

    # Keys whose values aren't part of the user's processing intent
    # (program metadata, transform matrices that are recomputed every
    # run, the fingerprint itself, or runtime bookkeeping). Including
    # them in the fingerprint would cause spurious mismatches.
    _NON_FINGERPRINT_KEYS = (
        'program_version',
        'cache_format_version',
        'processing_fingerprint',
        'transform',
        'inv_transform',
        'centImgTransMat',
        'folded',
    )

    def cacheInfo(self):
        """
        Save info dict to cache (qf_cache/<name>.info).

        With CACHE_FORMAT_VERSION 2, the file holds only scalar parameters,
        small matrices, and a comprehensive fingerprint. Image-bearing
        intermediates (avg_fold, bgimg1/2, bg_line) are NOT pickled; the
        canonical image lives at qf_results/<name>_folded.tif.
        """
        cache_file = fullPath(fullPath(self.output_dir, "qf_cache"), self.img_name + ".info")
        createFolder(fullPath(self.output_dir, "qf_cache"))
        self.info['program_version'] = self.version
        self.info['cache_format_version'] = CACHE_FORMAT_VERSION

        # Build the on-disk dict without non-cached keys.
        to_dump = {k: v for k, v in self.info.items() if k not in self._NON_CACHED_KEYS}
        with open(cache_file, "wb") as c:
            pickle.dump(to_dump, c)

    def loadCache(self):
        """
        Load info dict from cache.

        Returns None if:
          - cache doesn't exist
          - program version mismatch (handled the same way as before)
          - cache_format_version mismatch (older caches that contained
            image arrays — they're now stale)
        """
        cache_file = fullPath(fullPath(self.output_dir, "qf_cache"), self.img_name+".info")
        if not os.path.isfile(cache_file):
            return None
        try:
            with open(cache_file, "rb") as c:
                info = pickle.load(c)
        except Exception as e:
            print(f"Failed to read cache for {self.img_name}: {e}; treating as no cache.")
            return None
        if info is None:
            return None
        if info.get('program_version') != self.version:
            print("Cache version " + str(info.get('program_version')) +
                  " did not match with Program version " + self.version)
            print("Invalidating cache and reprocessing the image")
            return None
        if info.get('cache_format_version') != CACHE_FORMAT_VERSION:
            print(f"Cache format version mismatch (cache={info.get('cache_format_version')}, "
                  f"expected={CACHE_FORMAT_VERSION}); discarding old cache for {self.img_name}.")
            return None
        return info

    def delCache(self):
        """
        Delete cache
        :return: -
        """
        cache_path = fullPath(self.output_dir, "qf_cache")
        cache_file = fullPath(cache_path, self.img_name + '.info')
        if os.path.exists(cache_path) and os.path.isfile(cache_file):
            os.remove(cache_file)

    # ==================== Fingerprint ====================

    def _normalize_for_fingerprint(self, value):
        """
        Recursively normalize a value into something json-serializable AND
        deterministic across runs. In particular: sets become sorted
        tuples, tuples/lists are recursed into, dicts have their keys
        sorted, floats/ints/strings/bools/None pass through, anything
        else is stringified.
        """
        if isinstance(value, dict):
            return {k: self._normalize_for_fingerprint(value[k]) for k in sorted(value.keys(), key=str)}
        if isinstance(value, set):
            return sorted([self._normalize_for_fingerprint(v) for v in value], key=str)
        if isinstance(value, (list, tuple)):
            return [self._normalize_for_fingerprint(v) for v in value]
        if isinstance(value, (str, bool)) or value is None:
            return value
        if isinstance(value, (int, float)):
            return value
        # numpy scalars -> python; numpy arrays -> stringified shape (matrices etc. shouldn't be in fingerprint)
        try:
            return value.item()
        except Exception:
            return repr(value)

    def computeFingerprint(self, info_for_fp=None):
        """
        Build the comprehensive fingerprint used to decide whether the
        canonical _folded.tif on disk is still valid for the current
        request.

        Combines:
          - ImageData fingerprint (mask/blank checksums, manual center/
            rotation, preprocessing flags, detector, orientation_model)
          - All keys in info that aren't bookkeeping or image arrays.
            ROI (roi_w/roi_h) is INCLUDED here even though it's
            excluded from _NON_CACHED_KEYS -- it affects the result
            and must invalidate the fast-path when changed; the cache
            stamps the fingerprint that was computed *with* ROI, and
            ROI is re-applied from flags on every load.

        Returns a hex-digest string so it can be compared/printed cheaply.
        """
        info = info_for_fp if info_for_fp is not None else self.info

        params = {
            k: self._normalize_for_fingerprint(v)
            for k, v in info.items()
            if (k not in self._NON_FINGERPRINT_KEYS
                and k not in self._IMAGE_ARRAY_KEYS)
        }

        payload = {
            'cache_format_version': CACHE_FORMAT_VERSION,
            'program_version': self.version,
            'image_data': self._normalize_for_fingerprint(self._image_data.get_fingerprint()),
            'params': params,
        }
        try:
            blob = json.dumps(payload, sort_keys=True, default=str).encode('utf-8')
        except Exception:
            # Defensive: any unhashable corner case -> fall back to repr
            blob = repr(payload).encode('utf-8')
        return hashlib.sha1(blob).hexdigest()

    def fastPathCandidates(self):
        """
        Return the on-disk paths that the fast-path will try, in order.

        We accept both _folded.tif and _folded_compressed.tif: either is
        a faithful, full-size copy of resultImg.

        The filename convention matches the GUI/headless save path,
        which strips the original image extension before appending the
        suffix (e.g. "img.tif" -> "img_folded.tif").
        """
        result_dir = fullPath(self.output_dir, 'qf_results')
        base, _ = os.path.splitext(self.img_name)
        return [fullPath(result_dir, base + suffix)
                for suffix in FAST_PATH_RESULT_SUFFIXES]

    def deleteFromDict(self, dicto, delStr):
        """
        Delete a key and value from dictionary
        :param dict: input dictionary
        :param delStr: deleting key
        :return: -
        """
        if delStr in dicto:
            del dicto[delStr]

    def process(self, flags):
        """
        Run all processing steps for the current image. The pipeline is:

            updateInfo -> initParams -> findCenter -> getRotationAngle ->
            (fast-path check: load _folded.tif if fingerprint matches) ->
            transformImage -> calculateAvgFold -> [fold_image bypass] ->
            getRminmax -> getTransitionRad -> applyBackgroundSubtraction ->
            mergeImages -> generateResultImage -> (stamp fingerprint + cache)

        The actual result tif is written by the GUI / headless caller
        (they choose between cropped / compressed variants); next
        session's fast-path picks up whichever uncropped variant they
        wrote (see FAST_PATH_RESULT_SUFFIXES).

        flags keys (notable):
            ignore_folds - quadrants excluded from averaging
            bgsub        - background-subtraction method (in-radius)
            bgsub2       - background-subtraction method (out-radius)
            sigmoid      - merge gradient between bgimg1 and bgimg2
            cirmin/cirmax/tophat1/tophat2/...  - method-specific params
            no_cache     - if present, skip writing the disk cache
            no_fast_path - if present, force a full reprocess even when
                           the cached fingerprint matches the current one

        Returns True if a full processing pass was performed, False if
        the fast-path was used. Callers use this to decide whether
        downstream work that depends on slow-path-only intermediates
        (saveBackground needs avg_fold + BgSubFold) is needed.
        """
        print(str(self.img_name) + " is being processed...")

        self.updateInfo(flags)
        self.initParams()

        # Note: Blank/mask preprocessing is already applied by ImageData.get_working_image()
        # No need to apply again here (would cause double subtraction of blank image)

        # Determine center and rotation to use for this run. These are
        # required even on the fast-path so the GUI's coordinate
        # conversions (click handlers, ROI overlays) keep working.
        self.findCenter()        # Sets self.center from ImageData
        self.getRotationAngle()  # Sets self.rotation (if not already set by GUI)

        # ==========================================
        # Fast-path: if all parameters match what produced the canonical
        # _folded.tif on disk, just reload that tif and skip the
        # expensive pipeline entirely.
        # ==========================================
        if 'no_fast_path' not in flags and self._tryFastLoad():
            self.parent.statusPrint("")
            return False

        # ==========================================
        # Slow path: full pipeline
        # ==========================================
        self.transformImage()
        self.calculateAvgFold()
        if flags['fold_image'] == False:
            self.info['folded'] = False

            # get top left quandrant
            #rotate_img = copy.copy(self.getRotatedImage())
            rotate_img = copy.copy(self.orig_img)
            center = self.center
            center_x = int(center[0])
            center_y = int(center[1])
            img_height = rotate_img.shape[0]
            img_width = rotate_img.shape[1]
            fold_width = max(int(center[0]), img_width-int(center[0])) # max(max(int(center[0]), img_width-int(center[0])), max(int(center[1]), img_height-int(center[1])))
            fold_height = max(int(center[1]), img_height-int(center[1])) # fold_width
            top_left = rotate_img[max(center_y-fold_height,0):center_y, max(center_x-fold_width,0):center_x]

            self.info['avg_fold'] = top_left

        self.getRminmax()
        self.getTransitionRad()

        self.applyBackgroundSubtraction()
        self.mergeImages()
        self.generateResultImage()

        # The actual tif write is left to the GUI / headless caller --
        # they decide which variant (compressed / cropped) the user wants.
        # As long as the caller writes one of FAST_PATH_RESULT_SUFFIXES
        # the next session can avoid recomputation entirely.
        if "no_cache" not in flags:
            # Stamp the fingerprint into info immediately before pickling
            # so the next session can decide fast-path validity from the
            # cache alone. Skipped in no_cache mode so test fixtures that
            # snapshot the full info dict aren't affected by this field.
            self.info['processing_fingerprint'] = self.computeFingerprint()
            try:
                self.cacheInfo()
            except (OSError, IOError) as e:
                # Cache write failed (likely due to concurrent access), but processing succeeded
                # Log the error but don't fail the entire processing
                print(f"Warning: Failed to write cache for {self.img_name}: {e}")
                import traceback
                traceback.print_exc()

        self.parent.statusPrint("")
        return True

    def _tryFastLoad(self):
        """
        If cache fingerprint matches current params AND a usable result
        tif exists on disk (uncropped _folded.tif or _folded_compressed.tif),
        populate imgCache['resultImg'] from it, set up runtime state,
        and return True.

        Returns False if either condition fails (forces full reprocess).
        """
        cached_fp = self.info.get('processing_fingerprint')
        if not cached_fp:
            return False

        current_fp = self.computeFingerprint()
        if cached_fp != current_fp:
            print(f"Fingerprint changed for {self.img_name}; running full processing.")
            return False

        result_path = None
        for candidate in self.fastPathCandidates():
            if os.path.isfile(candidate):
                result_path = candidate
                break
        if result_path is None:
            print(f"Cached fingerprint matches but no usable result tif found "
                  f"in {fullPath(self.output_dir, 'qf_results')} for {self.img_name}; "
                  f"running full processing.")
            return False

        try:
            img = fabio.open(result_path).data.astype("float32")
        except Exception as e:
            print(f"Failed to load cached result {result_path}: {e}; running full processing.")
            return False

        # Reproduce the runtime transformation that transformImage()
        # would have written to info, so coordinate-conversion code in
        # the GUI keeps working. transformImage() also moves
        # self.center to the middle of the transformed image, which
        # downstream UI code expects.
        self.transformImage()

        # Only the displayed image is restored; intermediates like
        # BgSubFold / avg_fold / bgimg1 / bgimg2 are NOT recovered.
        # Callers that need those (e.g. saveBackground) must check the
        # process() return value and skip on the fast-path.
        self.imgCache['resultImg'] = img

        print(f"Fast-path: loaded {result_path}")
        return True


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

        # Transient keys (ROI) are fully reconstructed from flags every
        # processing pass. info.update() only adds/overwrites, so without
        # stripping first a stale value (e.g. from an old on-disk cache,
        # or from a previous pass within this QF instance) would linger
        # and silently keep cropping.
        #
        # QuadrantFolder only speaks roi_w/roi_h. Translation of any
        # caller-side "preference" names (e.g. fixed_roi_* in
        # qfsettings.json) into roi_w/roi_h must happen BEFORE flags
        # arrive here -- see QuadrantFoldingh.getFlags(). Keeping the
        # translation out of this module is what lets per-image cache
        # stay clean: only the actual ROI used to produce this image's
        # _folded.tif lives in info, and it's stripped here every pass.
        old_roi = (self.info.get('roi_w'), self.info.get('roi_h'))
        for key in self._NON_CACHED_KEYS:
            self.info.pop(key, None)

        self.info.update(flags)

        # In-memory caches that may have been populated by a previous
        # process() call on this same QF instance are invalidated when
        # the resolved ROI changes. The on-disk cache no longer keeps
        # bgimg1/bgimg2 (see _NON_CACHED_KEYS) so their info-level
        # deletion has been dropped here.
        new_roi = (self.info.get('roi_w'), self.info.get('roi_h'))
        if old_roi != new_roi:
            for key in ('BgSubFold', 'resultImg'):
                self.deleteFromDict(self.imgCache, key)

    def initParams(self):
        """
        Initial some parameters in case GUI doesn't specified
        Note: mask_thres removed - now using INVALID_PIXEL_THRESHOLD constant for invalid pixels
        """
        if 'ignore_folds' not in self.info:
            self.info['ignore_folds'] = set()
        if 'bgsub' not in self.info:
            self.info['bgsub'] = 'None'
        if 'bgsub2' not in self.info:
            self.info['bgsub2'] = 'None'


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
        h_o, w_o = self.orig_img.shape
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

        img = self.makeFullImage(fold)

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
        Calculate an average fold for 1-4 quadrants. Quadrants are
        splitted by center and rotation.

        Always recomputes (no info-cache short-circuit) since image
        arrays no longer live in the disk cache; if process() reached
        this point we already failed the fingerprint fast-path and
        therefore want a fresh fold.
        """
        self.parent.statusPrint("Calculating Avg Fold...")
        # rmin/rmax are derived from avg_fold; force re-derivation.
        self.deleteFromDict(self.info, 'rmin')
        self.deleteFromDict(self.info, 'rmax')

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
            # else:
            #     result = np.mean( np.array(quadrants), axis=0 )

        self.info['avg_fold'] = result
        self.info['folded'] = True

    def applyBackgroundSubtraction(self):
        """
        Apply background subtraction by user's choice. There are 2 images produced in this process
        - bgimg1 : image after applying background subtraction INSIDE merge radius
        - bgimg2 : image after applying background subtraction OUTSIDE merge radius

        Always recomputes (no info-cache short-circuit) since bgimg1/bgimg2
        are no longer persisted to disk and process() only reaches here on
        the slow path.
        """
        self.parent.statusPrint("Applying Background Subtraction...")
        method = self.info["bgsub"]
        method2 = self.info["bgsub2"]
        print(f"Background Subtraction is being processed... In method: {method}, Out method: {method2}")

        # Produce bgimg1
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
        if 'roi_w' in self.info and 'roi_h' in self.info:
            cy = result.shape[0] / 2
            cx = result.shape[1] / 2
            half_w = self.info['roi_w'] / 2
            half_h = self.info['roi_h'] / 2
            result = result[
                max(int(cy - half_h), 0):min(int(cy + half_h), result.shape[0]),
                max(int(cx - half_w), 0):min(int(cx + half_w), result.shape[1])
            ]

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
