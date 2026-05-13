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
import json
import pickle
import hashlib
import fabio
import time
import multiprocessing as mp
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
    from ..utils.background_search import *
    from ..utils.image_data import ImageData
    from ..utils.fold_symmetry import _compute_fold_symmetry
except: # for coverage
    from modules import QF_utilities as qfu
    from utils.file_manager import fullPath, createFolder
    from utils.histogram_processor import *
    from utils.image_processor import *
    from utils.background_search import *
    from utils.image_data import ImageData
    from utils.fold_symmetry import _compute_fold_symmetry

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
        # self.dl, self.db = 0, 0
        # self.empty = False
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

        self._bg_raw_metrics_cache = {}
        self._bg_raw_metrics_cache_image_id = self._get_bg_cache_image_id()

    def _get_bg_cache_image_id(self):
        return f"{self.img_path}/{self.img_name}"

    def _prepare_bg_raw_metrics_cache(self):
        image_id = self._get_bg_cache_image_id()
        if self._bg_raw_metrics_cache_image_id != image_id:
            self._bg_raw_metrics_cache = {}
            self._bg_raw_metrics_cache_image_id = image_id
        return self._bg_raw_metrics_cache

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

    def _check_stop(self):
        if self.parent is not None and getattr(self.parent, "stop_process", False):
            self.info['stopped'] = True
            return True
        return False

    def _clearDependentCaches(self):
        """
        Clear QuadrantFolder's processing results cache.

        Called when ImageData's fingerprint changes (indicating that
        preprocessing, geometry, or configuration has changed).
        """
        print("  Clearing processing results cache")
        self.deleteFromDict(self.imgCache, 'avg_fold')
        self.deleteFromDict(self.info, 'rmin')
        self.deleteFromDict(self.info, 'rmax')

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
            transformImage -> calculateAvgFold -> getRminmax ->
            createMask -> createArtificialData -> smoothFold -> downsampleImage ->
            searchBackground -> applyBackgroundSubtraction ->
            applyBackgroundSubtractionSynthetic -> [if transition: getTransitionRad ->
            applyTransitionBackgroundSubtraction -> applyTransitionBackgroundSubtractionSynthetic ->
            mergeImages] -> generateResultImage -> evaluateResult -> (stamp fingerprint + cache)

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

        self.info.pop('stopped', None)
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
        self.getRminmax()
        self.createMask()
        self.createArtificialData()
        self.smoothFold()
        self.downsampleImage()
        if self._check_stop():
            return
        self.searchBackground()
        if self._check_stop():
            return
        self.applyBackgroundSubtraction()
        self.applyBackgroundSubtractionSynthetic()

        if self.info['bg_options'] == 1: # Transition
            self.getTransitionRad()
            self.applyTransitionBackgroundSubtraction()
            self.applyTransitionBackgroundSubtractionSynthetic()
            self.mergeImages()

        self.generateResultImage()
        self.evaluateResult()

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

        # In-memory caches invalidated when the resolved ROI changes.
        new_roi = (self.info.get('roi_w'), self.info.get('roi_h'))
        if old_roi != new_roi:
            for key in ('BgSubFold', 'resultImg'):
                self.deleteFromDict(self.imgCache, key)

        if self.info.get('force_recalc_bg'):
            # Force recompute of background subtraction results (batch manual mode)
            self.deleteFromDict(self.info, 'result_bg')
            for key in (
                'BgSubFold', 'BgFold',
                'BgSubFold_in', 'BgFold_in',
                'BgSubFold_out', 'BgFold_out',
                'BgSubFold_syn', 'BgFold_syn',
                'BgSubFold_syn_in', 'BgFold_syn_in',
                'BgSubFold_syn_out', 'BgFold_syn_out',
            ):
                self.deleteFromDict(self.imgCache, key)

        if 'result_bg' not in self.info:
            self.info['result_bg'] = {}
            self.info['result_bg'].setdefault('method', None)
            self.info['result_bg'].setdefault('final_params', None)
            self.info['result_bg'].setdefault('optimized', None)
            self.info['result_bg'].setdefault('downsampled', None)
            self.info['result_bg'].setdefault('loss', None)
            self.info['result_bg'].setdefault('metrics_normalized', None)
            self.info['result_bg'].setdefault('metrics_raw', None)
            self.info['result_bg'].setdefault('mean_metric_values', None)
            self.info['result_bg'].setdefault('metric_weights', None)
            self.info['result_bg'].setdefault('selected_configuration_name', None)
            self.info['result_bg'].setdefault('symmetry', None)

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
        self.imgCache['avg_fold'] = self.imgCache['avg_fold'].astype(np.float32)
        self.imgCache["avg_fold_with_syn"] = self.imgCache["avg_fold_with_syn"].astype(np.float32)

        if 'smooth_image' in self.info and self.info['smooth_image']:
            self.imgCache['_avg_fold'] = cv2.ximgproc.guidedFilter(
                guide=self.imgCache['avg_fold'],
                src=self.imgCache['avg_fold'],
                radius=7,
                eps=1
            )
            self.imgCache['_avg_fold_with_syn'] = cv2.ximgproc.guidedFilter(
                guide=self.imgCache["avg_fold_with_syn"],
                src=self.imgCache["avg_fold_with_syn"],
                radius=7,
                eps=1
            )
        else:
            self.imgCache['_avg_fold'] = self.imgCache['avg_fold']
            self.imgCache['_avg_fold_with_syn'] = self.imgCache['avg_fold_with_syn']


    def downsampleImage(self):
        self.imgCache['_rmin'] = self.info['rmin'] // self.info['downsample'] if 'downsample' in self.info else self.info['rmin']
        self.imgCache['_rmax'] = self.info['rmax'] // self.info['downsample'] if 'downsample' in self.info else self.info['rmax']
        self.imgCache['_center'] = (self.center[0] // self.info['downsample'], self.center[1] // self.info['downsample']) if 'downsample' in self.info else self.center

        if 'downsample' in self.info and self.info['downsample'] > 1:
            factor = self.info['downsample']

            h, w = self.imgCache['_avg_fold'].shape[:2]
            new_w = w // factor
            new_h = h // factor
            if w % factor != 0:
                subtract = w % factor
                self.imgCache['_avg_fold'] = self.imgCache['_avg_fold'][:, subtract:]
            if h % factor != 0:
                subtract = h % factor
                self.imgCache['_avg_fold'] = self.imgCache['_avg_fold'][subtract:, :]

            self.imgCache['_avg_fold'] = cv2.resize(self.imgCache['_avg_fold'], (new_w, new_h), interpolation=cv2.INTER_AREA)
            self.imgCache['_avg_fold_with_syn'] = cv2.resize(self.imgCache['_avg_fold_with_syn'], (new_w, new_h), interpolation=cv2.INTER_AREA)

    def upsampleImage(self, img):
        factor = self.info['downsample']
        if 'downsample' in self.info and self.info['downsample'] > 1:
            h, w = img.shape[0] * factor, img.shape[1] * factor
            upsampled_img = cv2.resize(img, (w, h), interpolation=cv2.INTER_CUBIC)
            return upsampled_img
        else:
            return img
        

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
            copy_img = copy.copy(self.imgCache['avg_fold'])
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
                self.info['rmax'] = getDetectorEdge(totalI, end=(min(copy_img.shape[0], copy_img.shape[1]) - 1)) - 20
            
        self.deleteFromDict(self.imgCache, 'BgSubFold') # remove "BgSubFold" from imgCache to make it reprocess
        print(f"Done. R-min is {str(self.info['rmin'])} and R-max is set to max: {str(self.info['rmax'])}")

    def getTransitionRad(self):
        transition_radius = self.info.get('transition_radius', -1)
        if transition_radius is None or float(transition_radius) < 0:
            self.info['transition_radius'] = self.orig_img.shape[0] // 5

        transition_delta = self.info.get('transition_delta', -1)
        if transition_delta is None or float(transition_delta) < 0:
            self.info['transition_delta'] = 60
        
        self.deleteFromDict(self.imgCache, 'BgSubFold_out') # remove "BgSubFold_out" from imgCache to make it reprocess


    def createMask(self):
        """
        Create a mask for the image based on rmin, rmax, and other parameters.
        Combines multiple mask types via element-wise multiplication.
        """
        h, w = self.imgCache['avg_fold'].shape[0]*2, self.imgCache['avg_fold'].shape[1]*2
        fullImg = makeFullImage(self.imgCache['avg_fold'])
        
        masks = {
            'rminrmax': self._create_rminrmax_mask(h, w),
            'equator': self._create_equator_mask(h, w, fullImg),
            'peaks': self._create_equator_peaks_mask(h, w, fullImg),
            'beam': self._create_equator_center_beam_mask(h, w, fullImg),
            'layer_lines': self._create_layer_lines_mask(h, w, fullImg),
        }
        
        # Combine all masks
        mask = np.ones((h, w), dtype=int)
        for name, m in masks.items():
            if m is None:
                print(f"Warning: {name} mask is None, skipping")
                continue
            mask *= m
        
        self.imgCache['mask'] = mask.astype(int)

    def _create_rminrmax_mask(self, h, w):
        """Create radial min/max mask."""
        mask = create_circular_mask(h, w, inside=True, radius=self.info['rmax'])
        mask *= create_circular_mask(h, w, inside=False, radius=self.info['rmin'])
        return mask.astype(int)

    def _create_equator_mask(self, h, w, fullImg):
        meridian = get_projection(fullImg, orientation=1, gap=2, offset=200)
        eq_fwhm = int(find_fwhm(meridian, rel_height=0.5))

        m1_peak = find_m_peak_auto(fullImg, m=1, rmin=self.info['rmin'])
        auto_y_height = ((m1_peak * 2) +  eq_fwhm) //2
        auto_y_height = max(30, auto_y_height)
        y_height = int(self.info.get('equator_mask_height', auto_y_height))
        self.info['equator_mask_height'] = y_height
        mask = create_rectangle_mask(h, w, x_length=w, y_height=y_height)
        return mask.astype(int)
        
    def _create_equator_peaks_mask(self, h, w, fullImg):

        equator = get_projection(fullImg, gap=2, orientation=0, half=True) # right half

        peak_positions, _ = find_n_most_prominent_peaks(equator, n=self.info.get('n_peaks', 4))

        fwhm_values = []
        for peak in peak_positions:
            fwhm = find_fwhm(equator, peak_index=peak, rel_height=0.5)
            fwhm_values.append(fwhm)
        if len(fwhm_values) > 0:
            peak_width = int(np.mean(fwhm_values))*2
        else:
            peak_width = 30

        mask = create_peak_mask((h, w), peak_positions=peak_positions, peak_width=peak_width)
        return mask
    
    def _create_equator_center_beam_mask(self, h, w, fullImg):
        h, w = self.imgCache['avg_fold'].shape[0]*2, self.imgCache['avg_fold'].shape[1]*2
        fullImg = makeFullImage(self.imgCache['avg_fold'])


        equator = get_projection(fullImg, gap=2, orientation=0, half=True)
        auto_beam_width = find_first_valley(equator, start=self.info['rmin']+5) # TODO
        auto_beam_width = 20 if not auto_beam_width else auto_beam_width
        beam_width = int(self.info.get('equator_center_beam_width', auto_beam_width))
        self.info['equator_center_beam_width'] = beam_width
        mask = create_circular_mask(h, w, inside=False, radius=beam_width)
        return mask.astype(int)
    
    def _create_layer_lines_mask(self, h, w, fullImg):
        m1_peak = int(self.info.get('m1', 0) or 0)
        if m1_peak <= 0:
            m1_peak = find_m_peak_auto(fullImg, m=1, rmin=self.info['rmin'])
        self.info['m1'] = m1_peak
        layer_lines = get_layer_lines(m1_peak, num_lines=9)

        if isinstance(self.info.get('layer_line_width', None), (int, float)) and self.info.get('layer_line_width'):
            mask_width = int(self.info['layer_line_width'])
        else:
            fwhm_values = []
            meridian = get_projection(fullImg, orientation=1, gap=2, half=True)
            peaks = find_n_most_prominent_peaks(meridian, n=len(layer_lines))

            for i in range(min(len(layer_lines), len(peaks))):
                fwhm1 = find_fwhm(meridian, peak_index= layer_lines[i], rel_height=0.5)
                fwhm2 = find_fwhm(meridian, peak_index= peaks[i], rel_height=0.5)
                print(f"Layer line {i+1} at position {layer_lines[i]} has FWHM from layer line: {fwhm1} and FWHM from peak: {fwhm2}")
                fwhm = int(max(fwhm1, fwhm2))
                fwhm_values.append(fwhm)

            mask_width = max(5, max(fwhm_values) if len(fwhm_values) > 0 else 5)
                
        self.info['layer_line_width'] = mask_width
        mask = create_layer_lines_mask(h, w, layer_lines=layer_lines, width_line=mask_width)
        return mask


    def _ensure_synthetic_gaussian_params(self, fullImg, i0, m1):
        AMP = 0.01
        SIGMA_X_DIV = 5.0
        SIGMA_Y_DIV = 10.0

        amplitude = float(self.info.get('synthetic_amplitude', 0.0))
        if amplitude <= 0.0:
            equator_half = get_projection(fullImg, gap=2, orientation=0, half=True)
            amplitude = equator_half[i0] * AMP * i0
            amplitude = 500 if amplitude < 500 else amplitude
            self.info['synthetic_amplitude'] = amplitude

        # `synthetic_sigma_x` / `synthetic_sigma_y` are actual Gaussian sigmas (std dev, pixels)
        sigma_x = float(self.info.get('synthetic_sigma_x', 0.0))
        sigma_y = float(self.info.get('synthetic_sigma_y', 0.0))
        if sigma_x <= 0.0:
            sigma_x = i0 / SIGMA_X_DIV / (2 * np.sqrt(2 * np.log(2)))
            self.info['synthetic_sigma_x'] = sigma_x
        if sigma_y <= 0.0:
            sigma_y = m1 / SIGMA_Y_DIV / (2 * np.sqrt(2 * np.log(2)))
            self.info['synthetic_sigma_y'] = sigma_y

        return amplitude, sigma_x, sigma_y

    def ensureSyntheticGaussianDefaults(self):
        if 'avg_fold' not in self.imgCache:
            return None

        fullImg = makeFullImage(self.imgCache['avg_fold'])
        i0, _ = find_i0_i1_peaks_auto(fullImg, rmin=30)
        i0 = 100 if abs(i0-100) > 50 else i0
        m1 = find_m_peak_auto(fullImg, m=1, rmin=30)
        m1 = 50 if abs(m1-50) > 50 else m1

        return self._ensure_synthetic_gaussian_params(fullImg, i0=i0, m1=m1)

    def createArtificialData(self):
        freq = str(self.info.get('freq', 'medium')).lower()

        fullImg = makeFullImage(self.imgCache['avg_fold'])

        i0, i1 = find_i0_i1_peaks_auto(fullImg, rmin=30)
        i0 = 100 if abs(i0-100) > 50 else i0
        m1 = find_m_peak_auto(fullImg, m=1, rmin=30)
        m1 = 50 if abs(m1-50) > 50 else m1

        if freq == 'sparse':
            offset_x = int(i0 / 2)
            step_x = int(i0*2)
            offset_y = int(m1 / 2)
            step_y = int(m1*2)
        elif freq == 'medium':
            offset_x = int(i0 / 2)
            step_x = int(i0)
            offset_y = int(m1 / 2)
            step_y = int(m1)
        elif freq == 'dense':
            offset_x = int(i0 / 4)
            step_x = int(i0 / 2)
            offset_y = int(m1 / 4)
            step_y = int(m1 / 2)
        else:
            offset_x = int(i0 / 2)
            step_x = int(i0)
            offset_y = int(m1 / 2)
            step_y = int(m1)
    
        grid = get_grid(image_shape=fullImg.shape, 
            step_x=step_x, step_y=step_y, 
            offset_x=offset_x, offset_y=offset_y, fold=True)
        
        amplitude, sigma_x, sigma_y = self._ensure_synthetic_gaussian_params(fullImg, i0=i0, m1=m1)

        # print(f"Creating synthetic data with amplitude: {amplitude}, sigma_x: {sigma_x}, sigma_y: {sigma_y}, step_x: {step_x}, step_y: {step_y}")

        gauss_data = GaussianArtificialData(image_shape=fullImg.shape)
        gauss_data.define_parameters(sigma_x=sigma_x, sigma_y=sigma_y, amplitude=amplitude)
        gauss_data.pre_compute_kernel()

        mask_data = MaskArtificialData(image_shape=fullImg.shape)
        mask_data.define_parameters(sigma_x=sigma_x, sigma_y=sigma_y, mult_x=3, mult_y=6)

        for point in grid:
            gauss_data.create_image(point[0], point[1])
            mask_data.create_image(point[0], point[1])

        gauss_data.apply_intencity_decrease()

        self.imgCache["synthetic_data"] = gauss_data.get_faded_image()
        self.imgCache["synthetic_mask"] = mask_data.get_mask()
        syn_data_top_left = self.imgCache["synthetic_data"][:self.imgCache['avg_fold'].shape[0], :self.imgCache['avg_fold'].shape[1]]
        self.imgCache["avg_fold_with_syn"] = self.imgCache['avg_fold'] + syn_data_top_left


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
        rotate_img = self.orig_img
        center = self.center
        center_x = int(center[0])
        center_y = int(center[1])

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
            self.imgCache['folded'] = False
            self.imgCache['avg_fold'] = top_left

        elif 'avg_fold' not in self.imgCache.keys():
            self.deleteFromDict(self.info, 'rmin')
            self.deleteFromDict(self.info, 'rmax')
            print("Quadrant folding is being processed...")

        top_right = rotate_img[max(center_y-fold_height,0):center_y, center_x:center_x+fold_width]
        top_right = cv2.flip(top_right,1)

        bottom_left = rotate_img[center_y:center_y+fold_height, max(center_x-fold_width,0):center_x]
        bottom_left = cv2.flip(bottom_left,0)

        bottom_right = rotate_img[center_y:center_y+fold_height, center_x:center_x+fold_width]
        bottom_right = cv2.flip(bottom_right,1)
        bottom_right = cv2.flip(bottom_right,0)

        # Add all folds which are not ignored
        # Initialize quadrants array with invalid threshold (marks empty/invalid pixels)
        quadrants = np.ones((4, fold_height, fold_width), rotate_img.dtype) * INVALID_PIXEL_THRESHOLD
        for i, quad in enumerate([top_left, top_right, bottom_left, bottom_right]):
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

        self.imgCache['avg_fold'] = result
        self.info['folded'] = True


    def searchBackground(self):
        """
        Search for background subtraction method and apply it to average fold.
        """

        result_path = fullPath(self.img_path, "qf_cache")
        createFolder(result_path)

        kwargs = self._build_bg_search_kwargs()

        def _apply_selected_configuration(method, params, loss_value, downsample_factor, config_name='-'):
            for key, value in params.items():
                self.info[f"{key}"] = value
            self.info['result_bg']['final_params'] = params
            self.info['result_bg']['optimized'] = False
            self.info['result_bg']['method'] = method
            self.info['result_bg']['loss'] = loss_value
            self.info['result_bg']['downsampled'] = downsample_factor
            self.info['result_bg']['selected_configuration_name'] = config_name if config_name else '-'
            self.info['bgsub'] = method


        selection = self._select_background_configuration(kwargs, _apply_selected_configuration)
        best_method = selection['best_method']

        if best_method is None and 'optimize' in self.info and self.info['optimize']:
            best_method = self._optimize_background(kwargs)

        if best_method is None:
            self._apply_existing_or_default_bg()

    def _build_bg_search_kwargs(self):
        raw_metrics_cache = self._prepare_bg_raw_metrics_cache()
        folded_image = makeFullImage(self.imgCache['avg_fold'])
        return {
            'steps': self.info['steps'],
            'early_stop': self.info['early_stop'],
            'max_iterations': self.info['max_iterations'],
            'evaluation_baseline': self.info['evaluation_baseline'],
            'tmp_avg_fold': self.imgCache['_avg_fold'],
            'avg_fold': self.imgCache['avg_fold'],
            'tmp_rmin': self.imgCache['_rmin'],
            'rmin': self.info['rmin'],
            'tmp_center': self.imgCache['_center'],
            'tmp_avg_fold_with_syn': self.imgCache['_avg_fold_with_syn'],
            'avg_fold_with_syn': self.imgCache['avg_fold_with_syn'],
            'orig_img': folded_image,
            'rmax': self.info['rmax'],
            'mask': self.imgCache['mask'],
            'synthetic_data': self.imgCache['synthetic_data'],
            'synthetic_mask': self.imgCache['synthetic_mask'],
            'downsample_factor': self.info['downsample'],
            'smooth_image': self.info.get('smooth_image', False),
            'mean_metric_values': self.info.get('mean_metric_values', None),
            'metric_weights': self.info.get('metric_weights', None),
            'freq': self.info.get('freq', None),
            'synthetic_amplitude': self.info.get('synthetic_amplitude', None),
            'synthetic_sigma_x': self.info.get('synthetic_sigma_x', None),
            'synthetic_sigma_y': self.info.get('synthetic_sigma_y', None),
            'equator_mask_height': self.info.get('equator_mask_height', None),
            'n_peaks': self.info.get('n_peaks', None),
            'equator_center_beam_width': self.info.get('equator_center_beam_width', None),
            'layer_line_width': self.info.get('layer_line_width', None),
            'm1': self.info.get('m1', None),
            'image_id': self._get_bg_cache_image_id(),
            'raw_metrics_cache': raw_metrics_cache,
        }

    def _values_from_params(self, method, params):
        param_keys = list(method_params[method].keys())
        ordered_values = [None] * len(param_keys)
        for idx, key in enumerate(param_keys):
            pos = method_order[method].index(idx)
            ordered_values[pos] = params[key]
        return ordered_values

    def _normalize_params(self, method, cfg_params):
        params = {}
        cfg_params = cfg_params if isinstance(cfg_params, dict) else {}
        for key, default_value in method_params[method].items():
            value = cfg_params.get(key, default_value)
            try:
                if isinstance(default_value, int):
                    value = int(round(float(value)))
                elif isinstance(default_value, float):
                    value = float(value)
            except Exception:
                value = default_value
            params[key] = value
        return params

    def _select_background_configuration(self, kwargs, apply_selected_configuration):

        manual_assignments = self.info.get('manual_background_assignments', {})
        auto_configs = self.info.get('background_configurations', [])
        batch_processing = self.info.get('batch_processing', False)
        use_auto_configs = (
            batch_processing
            and bool(self.info.get('choose_configurations_auto', False))
            and isinstance(auto_configs, list)
            and len(auto_configs) > 0
        )

        assigned_cfg = manual_assignments.get(self.img_name, None)
        use_manual_assignment = batch_processing and isinstance(manual_assignments, dict) and assigned_cfg is not None

        reuse_saved_default_optimization = (
            not batch_processing
            and isinstance(auto_configs, list)
            and not use_auto_configs
            and not use_manual_assignment
            and ('optimize' not in self.info or not self.info['optimize'])
            and (self.info['result_bg']['method'] is None or self.info['result_bg']['method'] == 'None')
            and (self.info['bgsub'] is None or self.info['bgsub'] == 'None')
        )

        best_method = None

        if use_manual_assignment and isinstance(assigned_cfg, dict):
            assigned_method = str(assigned_cfg.get('method', '') or '').strip()
            assigned_name = str(assigned_cfg.get('name', '') or '').strip()
            assigned_params = assigned_cfg.get('params', {})

            if assigned_method in method_params and isinstance(assigned_params, dict):
                eval_params = self._normalize_params(assigned_method, assigned_params)
                values = self._values_from_params(assigned_method, eval_params)
                try:
                    loss, _ = process_file(values=values, method=assigned_method, **kwargs)
                except Exception as e:
                    print(f"Manual assigned configuration for '{self.img_name}' is invalid: {e}")
                else:
                    print(
                        f"Using manually assigned configuration for '{self.img_name}': "
                        f"{assigned_name or assigned_method} (method: {assigned_method}, loss: {loss})"
                    )
                    apply_selected_configuration(
                        assigned_method,
                        eval_params,
                        loss,
                        config_name=assigned_name if assigned_name else '-'
                    )
                    best_method = assigned_method

        elif use_auto_configs or reuse_saved_default_optimization:
            self.parent.statusPrint("Evaluating saved background configurations...")
            best_loss = np.inf
            best_params = None
            best_method = None
            best_name = None

            for cfg in auto_configs:
                if not isinstance(cfg, dict):
                    continue
                method = str(cfg.get('method', '') or '').strip()
                if method not in method_params:
                    continue

                cfg_name = str(cfg.get('name', '') or '').strip()
                cfg_params = cfg.get('params', {}) if isinstance(cfg.get('params', {}), dict) else {}

                # Fill missing parameters from defaults and coerce types.
                eval_params = self._normalize_params(method, cfg_params)

                values = self._values_from_params(method, eval_params)
                try:
                    loss, _ = process_file(values=values, method=method, **kwargs)
                except Exception as e:
                    print(f"Skipping invalid background configuration '{cfg_name or method}': {e}")
                    continue

                print(f"Configuration '{cfg_name or method}' -> method: {method}, loss: {loss}")
                if loss < best_loss:
                    best_loss = loss
                    best_params = eval_params
                    best_method = method
                    best_name = cfg_name

            if best_method is not None and best_params is not None:
                print(f"Best saved configuration: {best_name or '-'} ({best_method}) with loss {best_loss}")
                apply_selected_configuration(
                    best_method,
                    best_params,
                    best_loss,
                    config_name=best_name if best_name else '-'
                )
            else:
                # Fallback to existing behavior if provided configs are invalid.
                print("No valid saved background configuration found. Falling back to existing processing mode.")
                self.info['result_bg']['selected_configuration_name'] = '-'

        return {'best_method': best_method}

    def _optimize_background(self, kwargs):
        self.parent.statusPrint(f"Background subtraction methods to optimize: {self.info['methods']}")
        self.parent.statusPrint("Running optimization...")
        kwargs['raw_metrics_cache'] = self._prepare_bg_raw_metrics_cache()
        kwargs['image_id'] = self._get_bg_cache_image_id()

        # Keep one CPU core free for the GUI/main thread so the app remains responsive.
        # Also never spawn more workers than methods to evaluate.
        cpu_count = max(1, mp.cpu_count())
        reserved_for_ui = 1 if cpu_count > 1 else 0
        n_proc = max(1, min(len(self.info['methods']), cpu_count - reserved_for_ui))

        from functools import partial
        outputs = []
        func = partial(optimize_mp_wrapper, **kwargs)
        methods = list(self.info['methods'])
        with mp.Pool(processes=n_proc) as pool:
            async_results = [pool.apply_async(func, (method,)) for method in methods]

            total = len(async_results)
            done = 0
            last_report_t = 0.0
            start = time.time()

            while done < total:
                done = sum(1 for ar in async_results if ar.ready())

                if self._check_stop():
                    try:
                        pool.terminate()
                        pool.join()
                    finally:
                        self.info['optimize'] = False
                    return None

                # Allow GUI hosts to pump pending UI events while optimization runs.
                # (No-op in headless mode.)
                if hasattr(self.parent, 'processPendingEvents'):
                    try:
                        self.parent.processPendingEvents()
                    except Exception:
                        pass

                now = time.time()
                # Throttle status updates to avoid flooding
                if now - last_report_t > 30:
                    self.parent.statusPrint(
                        f"Optimizing background subtraction... {done}/{total} method(s) complete. Elasped time: {int(now - start)}s"
                    )
                    last_report_t = now
                time.sleep(0.5)

            outputs = [ar.get() for ar in async_results]

        best_loss = np.inf
        best_params = None
        best_method = None
        for result in outputs:
            cache_updates = result.get('raw_metrics_cache_updates', {})
            if isinstance(cache_updates, dict) and cache_updates:
                self._bg_raw_metrics_cache.update(cache_updates)
            loss = result['best_loss']
            params = result['best_params']
            method = result['method']
            print(f"Method: {method}. Loss: {loss}. Best params: {params}")

            for key, value in params.items():
                self.info[f"{key}"] = value

            if loss < best_loss:
                best_loss = loss
                best_params = params
                best_method = method

        print(f"Best params: {best_params}.  {best_loss} for method {best_method}")

        self.info['result_bg']['final_params'] = best_params
        self.info['result_bg']['optimized'] = True
        self.info['result_bg']['method'] = best_method
        self.info['result_bg']['loss'] = best_loss
        self.info['result_bg']['selected_configuration_name'] = '-'

        # resuse the best params for further processing
        self.info['bgsub'] = best_method
        for key, value in best_params.items():
            self.info[f"{key}"] = value

        self.info['optimize'] = False  # reset optimization flag after applying best params
        return best_method

    def _apply_existing_or_default_bg(self):
        if self.info['result_bg']['method'] is not None and self.info['result_bg']['final_params'] is not None:
            method = self.info['result_bg']['method']
            params = self.info['result_bg']['final_params']
            print(f"Using previously used background subtraction method: {method} with params: {params}")
            for key, value in params.items():
                self.info[f"{key}"] = value
            self.info['bgsub'] = method
            if 'selected_configuration_name' not in self.info['result_bg']:
                self.info['result_bg']['selected_configuration_name'] = '-'
        else:
            print("No previous background subtraction method found. Using current settings.")
            self.info['result_bg']['optimized'] = False
            self.info['result_bg']['method'] = self.info['bgsub']
            self.info['result_bg']['selected_configuration_name'] = '-'

            params_keys = list(method_params[self.info['bgsub']].keys())
            params = {params_keys[i]: self.info[f"{params_keys[i]}"] for i in range(len(params_keys))}

            self.info['result_bg']['final_params'] = params


    def applyBackgroundSubtraction(self):
        """
        Apply background subtraction by user's choice.
        - BgSubFold : fold after applying background subtraction
        """
        self.parent.statusPrint("Applying Background Subtraction...")
        method = self.info["bgsub"]
        print(f"Background Subtraction is being processed... Method: {method}")

        if "BgSubFold" not in self.imgCache:
            tmp_avg_fold = np.array(self.imgCache['_avg_fold'], dtype="float32")
            avg_fold = np.array(self.imgCache['avg_fold'], dtype="float32")
            tmp_rmin = self.imgCache['_rmin']
            rmin = self.info['rmin']
            tmp_center = self.imgCache['_center']

            params = {}
            for key in method_params[method].keys():
                params[key] = self.info[key]

            result = applyBackgroundRemoval(method, tmp_avg_fold, avg_fold, tmp_rmin, rmin, \
                           tmp_center, params, downsample_factor=self.info['downsample'])
            bg = avg_fold - result

            print(f"bg shape: {bg.shape}, result shape: {result.shape}")

        self.imgCache['BgSubFold_in'] = copy.copy(result)
        self.imgCache['BgFold_in'] = copy.copy(bg)
        self.imgCache['BgSubFold'] = copy.copy(result)
        self.imgCache['BgFold'] = copy.copy(bg)
        self.deleteFromDict(self.imgCache, "resultImg")
        print("Done.")

    def applyTransitionBackgroundSubtraction(self):
        """
        Apply background subtraction by user's choice for outer area.
        - BgSubFold_out : fold after applying background subtraction
        """
        self.parent.statusPrint("Applying Background Subtraction (Outside transition radius)...")
        method = self.info["bgsub_out"]
        print(f"Background Subtraction is being processed... Method: {method}")

        if "BgSubFold_out" not in self.imgCache:
            tmp_avg_fold = np.array(self.imgCache['_avg_fold'], dtype="float32")
            avg_fold = np.array(self.imgCache['avg_fold'], dtype="float32")
            tmp_rmin = self.imgCache['_rmin']
            rmin = self.info['rmin']
            tmp_center = self.imgCache['_center']

            params = {}
            for key in method_params[method].keys():
                params[key] = self.info[key+"_out"]
                # print(f"Parameter for bgsub_out: {key}_out = {self.info[key + '_out']}")

            result = applyBackgroundRemoval(method, tmp_avg_fold, avg_fold, tmp_rmin, rmin, \
                           tmp_center, params, downsample_factor=self.info['downsample'])
            bg = avg_fold - result

        self.imgCache['BgSubFold_out'] = result
        self.imgCache['BgFold_out'] = bg

        self.deleteFromDict(self.imgCache, "resultImg")
        self.deleteFromDict(self.imgCache, "BgSubFold")
        self.deleteFromDict(self.imgCache, "BgFold")
        print("Done.")

    def applyBackgroundSubtractionSynthetic(self):
        """
        Apply background subtraction by user's choice.
        - BgSubFold_syn : fold after applying background subtraction
        """
        print("Applying Background Subtraction to synthetic data... method: " + self.info["bgsub"])
        method = self.info["bgsub"]
        tmp_rmin = self.imgCache['_rmin']
        tmp_center = self.imgCache['_center']

        avg_fold_with_syn = np.array(self.imgCache['avg_fold_with_syn'], dtype="float32")
        tmp_avg_fold_with_syn = np.array(self.imgCache['_avg_fold_with_syn'], dtype="float32")

        params = {}
        for key in method_params[method].keys():
            params[key] = self.info[key]
            
        result = applyBackgroundRemoval(method, tmp_avg_fold_with_syn, avg_fold_with_syn, tmp_rmin, None, tmp_center, params, downsample_factor=self.info['downsample'])
        bg_syn = avg_fold_with_syn - result
        
        if method != 'None':
            result = qfu.replaceRmin(result, int(tmp_rmin), 0.)

        self.imgCache['BgFold_syn_in'] = copy.copy(bg_syn)
        self.imgCache['BgSubFold_syn_in'] = copy.copy(result)
        self.imgCache['BgFold_syn'] = copy.copy(bg_syn)
        self.imgCache['BgSubFold_syn'] = copy.copy(result)

    def applyTransitionBackgroundSubtractionSynthetic(self):
        method = self.info["bgsub_out"]
        print("Applying Background Subtraction to synthetic data... method: " + method)
        tmp_rmin = self.imgCache['_rmin']
        tmp_center = self.imgCache['_center']

        avg_fold_with_syn = np.array(self.imgCache['avg_fold_with_syn'], dtype="float32")
        tmp_avg_fold_with_syn = np.array(self.imgCache['_avg_fold_with_syn'], dtype="float32")

        params = {}
        for key in method_params[method].keys():
            params[key] = self.info[key+"_out"]
            
        result = applyBackgroundRemoval(method, tmp_avg_fold_with_syn, avg_fold_with_syn, tmp_rmin, None, tmp_center, params, downsample_factor=self.info['downsample'])
        bg_syn = avg_fold_with_syn - result
        
        # if method != 'None':
        #     result = qfu.replaceRmin(result, int(tmp_rmin), 0.)

        self.imgCache['BgFold_syn_out'] = copy.copy(bg_syn)
        self.imgCache['BgSubFold_syn_out'] = copy.copy(result)

        self.deleteFromDict(self.imgCache, "BgFold_syn")
        self.deleteFromDict(self.imgCache, "BgSubFold_syn")


    def mergeImages(self):
        """
        Merge bgimg1 and bgimg2 at merge radius, with sigmoid as a merge gradient param.
        The result of merging will be kept in self.info["BgSubFold"]
        :return:
        """
        self.parent.statusPrint("Merging Images...")
        method = self.info["bgsub"]
        tmp_rmin = self.imgCache['_rmin']


        if "BgSubFold" not in self.imgCache:
            img_in = np.array(self.imgCache["BgFold_in"], dtype="float32")
            img_out = np.array(self.imgCache["BgFold_out"], dtype="float32")

            center = [img_in.shape[1]-1, img_in.shape[0]-1]
            rad = self.info["transition_radius"]
            delta = self.info["transition_delta"]

            # Merge 2 images at merge radius using transition radius and delta
            self.imgCache['BgFold'] = qfu.combine_bgsub_linear_float32(img_in, img_out, center[0], center[1], rad, delta)
            avg_fold = np.array(self.imgCache['avg_fold'], dtype="float32")
            result = avg_fold - self.imgCache['BgFold']

            if method != 'None':
                result = qfu.replaceRmin(result, int(tmp_rmin), 0.)

            self.imgCache['BgSubFold'] = result
            self.deleteFromDict(self.imgCache, "resultImg")

        if "BgFold_syn" not in self.imgCache:
            img_in = np.array(self.imgCache["BgFold_syn_in"], dtype="float32")
            img_out = np.array(self.imgCache["BgFold_syn_out"], dtype="float32")

            center = [img_in.shape[1]-1, img_in.shape[0]-1]
            rad = self.info["transition_radius"]
            delta = self.info["transition_delta"]

            # Merge 2 images at merge radius using transition radius and delta
            self.imgCache['BgFold_syn'] = qfu.combine_bgsub_linear_float32(img_in, img_out, center[0], center[1], rad, delta)
            avg_fold = np.array(self.imgCache['avg_fold_with_syn'], dtype="float32")
            result = avg_fold - self.imgCache['BgFold_syn']

            if method != 'None':
                result = qfu.replaceRmin(result, int(tmp_rmin), 0.)

            self.imgCache['BgSubFold_syn'] = result
        print("Done.")
        
    def generateResultImage(self):
        """
        Put BgSubFold_in, BgSubFold_out, BgSubFold_syn_in, BgSubFold_syn_out together as a result image
        :return:
        """
        self.parent.statusPrint("Generating Resultant Image...")
        print("Generating result image from average fold...")

        result = copy.copy(self.imgCache['BgSubFold'])
        result = makeFullImage(result)
        result_scaled = self._applyTransformations(result)
        self.imgCache['resultImg'] = result_scaled
        bg = makeFullImage(copy.copy(self.imgCache['BgFold']))
        bg_scaled = self._applyTransformations(bg)
        self.imgCache['resultBg'] = bg_scaled


        if self.info["bgsub"] == 'None':
            self.imgCache['resultFolded'] = result_scaled
        else:
            self.imgCache['resultFolded'] = result_scaled + bg_scaled

        print("Done.")


    def _applyTransformations(self, result):
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
        return result_scaled
        

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
        bg = self.imgCache.get('resultBg', None)

        baseline = self.info.get('evaluation_baseline', None)
        if baseline is None or float(baseline) <= 0.0:
            baseline = get_radial_average_rmax(result + bg, self.info['rmax'], band_width=30) * 0.2
            self.info['evaluation_baseline'] = baseline
        baseline = max(float(baseline), 0.0001)
        syn_srt = self.imgCache.get('synthetic_data', None)
        syn_mask = self.imgCache.get('synthetic_mask', None)
        syn_fold = self.imgCache.get('BgSubFold_syn', None)
        gen_mask = self.imgCache.get('mask', None)
        syn_img = makeFullImage(syn_fold) if syn_fold is not None else None

        kwargs = {
            'dimg': result,
            'dbg': bg,
            'baseline': baseline,
            'syn_img': syn_img,
            'syn_srt': syn_srt,
            'syn_mask': syn_mask,
            'gen_mask': gen_mask,
            'mean_metric_values': self.info.get('mean_metric_values', None),
            'metric_weights': self.info.get('metric_weights', None),
        }
        eval_result = evaluate_loss(**kwargs, return_details=True)
        self.info['result_bg']['loss'] = eval_result.get('loss', None)
        self.info['result_bg']['metrics_normalized'] = eval_result.get('metrics_normalized', {})
        self.info['result_bg']['metrics_raw'] = eval_result.get('metrics_raw', {})
        self.info['result_bg']['mean_metric_values'] = self.info.get('mean_metric_values', None)
        self.info['result_bg']['metric_weights'] = eval_result.get('metric_weights', None)
        self.info['result_bg']['intensity'] = np.sum(bg)

        # Fold-symmetry score (normalised). Computed from the *original*
        # pre-transform image so the score reflects what folding actually saw;
        # this is the same input shape _compute_fold_symmetry expects (it
        # re-runs the translate+rotate itself).
        try:
            if self._image_data is not None:
                sym_img = self._image_data.get_working_image()
                sym_center = self._image_data.center
                sym_rotation = self._image_data.rotation
            else:
                sym_img = None
                sym_center = self.orig_image_center
                sym_rotation = self.rotation if self.rotation is not None else 0.0
            sym = _compute_fold_symmetry(sym_img, sym_center, sym_rotation)
            self.info['result_bg']['symmetry'] = sym.get('fold_std_norm', None)
        except Exception as _sym_err:
            print(f"Symmetry calculation failed: {_sym_err}")
            self.info['result_bg']['symmetry'] = None

        print("Evaluation complete. Loss: ", self.info['result_bg']['loss'])


    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        print(text)
