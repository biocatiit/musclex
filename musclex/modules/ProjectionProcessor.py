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

import copy
import pickle
from os.path import exists, isfile
from dataclasses import dataclass, field
from typing import Optional, Dict, List, Tuple
import numpy as np
from lmfit import Model, Parameters
from lmfit.models import GaussianModel, VoigtModel
from sklearn.metrics import r2_score
import fabio
from musclex import __version__
try:
    from ..utils.file_manager import fullPath, createFolder, ifHdfReadConvertless, getBlankImageAndMask, getMaskOnly
    from ..utils.histogram_processor import getPeakInformations, convexHull
    from ..utils.image_processor import *
    from ..utils.image_data import ImageData
except: # for coverage
    from utils.file_manager import fullPath, createFolder, ifHdfReadConvertless, getBlankImageAndMask, getMaskOnly
    from utils.histogram_processor import getPeakInformations, convexHull
    from utils.image_processor import *
    from utils.image_data import ImageData


@dataclass
class ProcessingBox:
    """
    Data container for a single processing box.
    Encapsulates all data and results for one box region.
    """
    # === Basic Configuration ===
    name: str
    coordinates: tuple  # ((x1, x2), (y1, y2), ...) or complex oriented format
    type: str  # 'h' (horizontal), 'v' (vertical), 'oriented'
    bgsub: int  # 0=gaussian fitting, 1=convex hull, 2=none
    
    # === Processing Parameters ===
    peaks: List[float] = field(default_factory=list)
    merid_bg: bool = False
    hull_range: Optional[Tuple[float, float]] = None  # (start, end)
    param_bounds: Dict[str, Dict] = field(default_factory=dict)
    use_common_sigma: bool = False
    peak_tolerance: float = 2.0
    sigma_tolerance: float = 100.0  # Changed to percentage (default 100%)
    
    # === Fixed Parameters (for GMM fitting) ===
    # Dict mapping peak index -> fixed value
    fixed_center: Dict[int, float] = field(default_factory=dict)
    fixed_sigma: Dict[int, float] = field(default_factory=dict)
    fixed_amplitude: Dict[int, float] = field(default_factory=dict)
    fixed_common_sigma: Optional[float] = None  # For GMM equal variance mode
    
    # === Intermediate Results ===
    hist: Optional[np.ndarray] = None
    hist2: Optional[np.ndarray] = None
    
    # === Fitting Results ===
    fit_results: Optional[Dict] = None
    
    # === Final Results ===
    moved_peaks: Optional[List[float]] = None
    subtracted_hist: Optional[np.ndarray] = None
    baselines: Optional[List[float]] = None
    centroids: Optional[np.ndarray] = None
    widths: Optional[List[float]] = None
    areas: Optional[List[float]] = None
    
    def clear_results(self, from_stage='fit'):
        """
        Clear results from a specific processing stage onwards.
        This maintains the dependency chain integrity.
        
        :param from_stage: Starting stage ('hist', 'hist2', 'fit', 'peaks')
        """
        stages = {
            'hist': ['hist', 'hist2', 'fit_results', 'moved_peaks', 
                    'subtracted_hist', 'baselines', 'centroids', 'widths', 'areas'],
            'hist2': ['hist2', 'fit_results', 'moved_peaks', 
                     'subtracted_hist', 'baselines', 'centroids', 'widths', 'areas'],
            'fit': ['fit_results', 'moved_peaks', 'subtracted_hist', 
                   'baselines', 'centroids', 'widths', 'areas'],
            'peaks': ['moved_peaks', 'baselines', 'centroids', 'widths', 'areas']
        }
        
        list_attrs = ['peaks', 'moved_peaks', 'baselines', 'widths', 'areas']
        for attr in stages.get(from_stage, []):
            if attr in list_attrs:
                setattr(self, attr, [] if attr == 'peaks' else None)
            else:
                setattr(self, attr, None)


@dataclass
class ProcessingState:
    """
    Complete state container for the projection processing pipeline.
    Replaces the old self.info dictionary with a structured, type-safe design.
    """
    # === Metadata ===
    version: str
    filename: str
    dir_path: str
    
    # === All Processing Boxes ===
    boxes: Dict[str, ProcessingBox] = field(default_factory=dict)
    
    # === Global Settings ===
    mask_thres: Optional[float] = None
    detector: Optional[str] = None
    orientation_model: Optional[object] = None  # Not serialized, rebuilt at runtime
    lambda_sdd: Optional[float] = None
    
    # === Special: Main Peak Information ===
    main_peak_info: Dict = field(default_factory=dict)
    # Structure: {box_name: {'bg_sigma': ..., 'bg_amplitude': ..., ...}}
    
    # === Image-level Reject Status ===
    rejected: bool = False
    
    # === Image-level Comments ===
    comments: str = ""


class ProjectionProcessor:
    """
    A class for Bio-Muscle processing - go to process() to see all processing steps
    """
    def __init__(self, image_data: ImageData):
        """
        Initialize ProjectionProcessor with ImageData container.
        
        :param image_data: ImageData container with image data and preprocessing
        """
        self.dir_path = str(image_data.img_path)
        self.filename = image_data.img_name
        
        # Get working image from ImageData (with blank/mask already applied)
        self.orig_img = image_data.get_working_image()
        
        # Cache the raw image (before any rotation)
        # This allows process() to be called multiple times without cumulative rotation
        self._raw_img = self.orig_img.copy()
        
        # Store reference to ImageData
        self._image_data = image_data
        
        # Initialize runtime state
        self.rotated_img = None
        self.version = __version__
        self.rotMat = None
        
        # Initialize ProcessingState (replaces self.info)
        self.state = ProcessingState(
            version=__version__,
            filename=self.filename,
            dir_path=self.dir_path
        )
        
        # Load cache
        cached_state = self.loadCache()
        if cached_state:
            self.state = cached_state
        
        # Initialize mask threshold if not set
        if self.state.mask_thres is None:
            self.state.mask_thres = getMaskThreshold(self.orig_img)
        
        # Configure from ImageData
        if image_data.detector:
            self.state.detector = image_data.detector
        if image_data.orientation_model is not None:
            self.state.orientation_model = image_data.orientation_model
        
        # Note: center and rotation are now dynamic properties from ImageData
    
    @property
    def boxes(self) -> Dict[str, ProcessingBox]:
        """
        Convenient access to all processing boxes.
        Returns the boxes dictionary from state.
        """
        return self.state.boxes
    
    
    @property
    def center(self):
        """
        Get center from ImageData dynamically.
        Returns (x, y) tuple. Falls back to geometric center if not set.
        """
        if self._image_data.center:
            return self._image_data.center
        # Default: geometric center of image
        return (self.orig_img.shape[1] / 2 - 0.5, self.orig_img.shape[0] / 2 - 0.5)
    
    @property
    def rotation(self):
        """
        Get rotation angle from ImageData dynamically.
        Returns float (degrees). Falls back to 0 if not set.
        """
        if self._image_data.rotation is not None:
            return self._image_data.rotation
        return 0.0

    def addBox(self, name, box, typ, bgsub):
        """
        Add a box to state. If it exists and it changed, clear all old results.
        :param name: box name
        :param box: box coordinates
        :param typ: box type 'v' as vertical, 'h' as horizontal, 'oriented'
        :param bgsub: background subtraction method 0=gaussian, 1=convex hull, 2=none
        :return:
        """
        if name in self.boxes:
            # Box exists - check if coordinates changed
            existing_box = self.boxes[name]
            coords_changed = existing_box.coordinates != box
            oriented_changed = (typ == 'oriented' and 
                              len(box) > 6 and len(existing_box.coordinates) > 6 and 
                              box[-1] != existing_box.coordinates[-1])
            
            if coords_changed or oriented_changed:
                # Coordinates changed - clear all results
                existing_box.coordinates = box
                existing_box.type = typ
                existing_box.bgsub = bgsub
                existing_box.clear_results(from_stage='hist')
            else:
                # Just update config
                existing_box.type = typ
                existing_box.bgsub = bgsub
        else:
            # New box - create it
            self.boxes[name] = ProcessingBox(
                name=name,
                coordinates=box,
                type=typ,
                bgsub=bgsub
            )

    def addPeaks(self, name, peaks):
        """
        Add peaks to a box.
        :param name: box name
        :param peaks: peaks list
        :return:
        """
        if name not in self.boxes:
            print(f"Warning: box name '{name}' is invalid.")
            return
        
        box = self.boxes[name]
        
        # Check if peaks actually changed
        if box.peaks == peaks:
            return
        
        # Update peaks
        box.peaks = peaks
        
        # Clear parameter bounds as they're no longer valid
        box.param_bounds = {}
        
        # Clear fixed parameters (stored in ProcessingBox)
        box.fixed_center = {}
        box.fixed_sigma = {}
        box.fixed_amplitude = {}
        box.fixed_common_sigma = None
        
        # Clear downstream results (fitting onwards)
        box.clear_results(from_stage='fit')

    def _get_param_bounds(self, box_name: str, param_name: str):
        """
        Helper to read stored per-parameter bounds if available.
        Returns (min, max) or (None, None).
        """
        if box_name not in self.boxes:
            return (None, None)
        
        box = self.boxes[box_name]
        b = box.param_bounds.get(param_name)
        if not isinstance(b, dict):
            return (None, None)
        return (b.get('min', None), b.get('max', None))

    def _set_param_bounds(self, box_name: str, param_name: str, bmin: float, bmax: float):
        """
        Helper to persist per-parameter bounds.
        """
        if box_name not in self.boxes:
            return
        
        box = self.boxes[box_name]
        box.param_bounds[param_name] = {'min': float(bmin), 'max': float(bmax)}

    def removePeaks(self, name):
        """
        Remove all peaks from a box
        :param name: box name
        :return:
        """
        if name in self.boxes:
            box = self.boxes[name]
            box.peaks = []
            box.clear_results(from_stage='fit')

    def _preprocess(self):
        """
        Stage 1: Preprocessing (always executed, even when using cache).
        
        This stage prepares the image for processing:
        - Resets to raw image
        - Sets mask threshold
        - Applies rotation if needed
        """
        # Reset to raw image to avoid cumulative rotation on repeated process() calls
        self.orig_img = self._raw_img.copy()
        
        # Ensure mask_thres is set (fallback to auto-detection)
        if self.state.mask_thres is None:
            self.state.mask_thres = getMaskThreshold(self.orig_img)
        
        # ==================== Unified Rotation Logic ====================
        # Rotate the image once at the beginning if needed
        # Quadrant folded images should NOT be rotated (they are already aligned)
        # Normal images should be rotated if rotation angle is set
        should_rotate = not self._image_data.quadrant_folded and self.rotation != 0
        
        if should_rotate:
            # Rotate around diffraction center (center position remains unchanged)
            self.orig_img = rotateImageAboutPoint(self.orig_img, self.center, self.rotation)
            self.rotMat = cv2.getRotationMatrix2D(tuple(self.center), self.rotation, 1)
            print(f"Image rotated by {self.rotation}° around center {self.center}")
        # ================================================================
    
    def _compute(self):
        """
        Stage 2: Main computation pipeline (can be skipped when using cache).
        
        This stage performs the expensive computations:
        - Histogram generation
        - Convex hull application
        - Model fitting
        - Peak detection
        
        Note: Blank/mask preprocessing is already applied by ImageData.get_working_image()
        No need to apply again here (would cause double subtraction of blank image)
        """
        self.getHistograms()
        self.applyConvexhull()
        self.updateRotationAngle()
        self.fitModel()
        self.getBackgroundSubtractedHistograms()
        self.getPeakInfos()
    
    def process(self, no_cache: bool = False, use_existing_cache: bool = False):
        """
        Execute all processing steps.
        
        Settings should be written directly to self.state before calling this method.
        Box configurations are managed by ProcessingBox objects in self.boxes.
        
        Three-stage processing pipeline:
        1. Preprocessing (always run) - image preparation and rotation
        2. Main computation (skipped if use_existing_cache=True) - expensive calculations
        3. Caching (unless no_cache=True) - save results to disk
        
        :param no_cache: If True, skip caching results to disk
        :param use_existing_cache: If True, skip expensive computation and use cached results
                                   (preprocessing like rotation is still applied)
        """
        # Stage 1: Preprocessing (always run)
        self._preprocess()
        
        # Stage 2: Main computation (skip if using cache)
        if use_existing_cache:
            print("Using cached computation results: skipping expensive processing pipeline")
        else:
            self._compute()
        
        # Stage 3: Cache results to disk
        if not no_cache:
            self.cacheInfo()

    def getHistograms(self):
        """
        Obtain projected intensity for each box.
        
        Note: Image rotation is now done in process(), so we directly use self.orig_img
        which is already rotated if needed.
        """
        for name, box in self.boxes.items():
            # Skip if histogram already computed
            if box.hist is not None:
                # Still need to clear downstream stages
                box.hist2 = None
                continue
            
            # Use the current image (already rotated in process() if needed)
            img = copy.copy(self.orig_img)
            b = box.coordinates
            t = box.type

            if t == 'oriented':
                # rotate bottom left to new origin, then get top right
                # the box center
                cx, cy = b[6]
                rot_angle = b[5]
                img = rotateImageAboutPoint(img, (cx, cy), rot_angle)

            # y is shape[0], x is shape[1]?
            x1 = np.max((int(b[0][0]), 0))
            x2 = np.min((int(b[0][1]), img.shape[1]))
            y1 = np.max((int(b[1][0]), 0))
            y2 = np.min((int(b[1][1]), img.shape[0]))

            area = img[y1:y2+1, x1:x2+1]
            if t in ('h', 'oriented'):
                hist = np.sum(area, axis=0)
            else:
                hist = np.sum(area, axis=1)

            box.hist = hist
            box.hist2 = None  # Clear next stage

    def applyConvexhull(self):
        """
        Apply Convex hull to the projected intensity if background subtraction method is 1 (Convex hull)
        :return:
        """
        for name, box in self.boxes.items():
            # Skip if hist2 already computed
            if box.hist2 is not None:
                continue
            
            # Skip if no histogram
            if box.hist is None:
                continue

            if box.bgsub == 1 and box.peaks and len(box.peaks) > 0:
                # apply convex hull to the left and right if peaks are specified
                hist = box.hist
                peaks = box.peaks
                coords = box.coordinates
                start_x = coords[0][0]
                start_y = coords[1][0]
                
                if box.type == 'h':
                    centerX = self.center[0] - start_x
                elif box.type == 'oriented':
                    centerX = coords[6][0] - start_x
                else:
                    centerX = self.center[1] - start_y
                centerX = int(round(centerX))
                
                right_hist = hist[centerX:]
                left_hist = hist[:centerX][::-1]
                min_len = min(len(right_hist), len(left_hist))

                if box.hull_range is None:
                    start = max(min(peaks) - 15, 10)
                    end = min(max(peaks) + 15, min_len)
                    box.hull_range = (start, end)

                # find start and end points
                (start, end) = box.hull_range

                left_ignore = np.array([(i <= self.state.mask_thres) for i in left_hist])
                right_ignore = np.array([(i <= self.state.mask_thres) for i in right_hist])
                if not any(left_ignore) and not any(right_ignore):
                    left_ignore = None
                    right_ignore = None

                left_hull = convexHull(left_hist, start, end, ignore=left_ignore)[::-1]
                right_hull = convexHull(right_hist, start, end, ignore=right_ignore)

                box.hist2 = np.append(left_hull, right_hull)
            else:
                # use original histogram and apply threshold
                box.hist2 = copy.copy(box.hist)
                box.hist2[box.hist2 <= self.state.mask_thres] = self.state.mask_thres

            # Clear downstream results
            box.fit_results = None

    def updateRotationAngle(self):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal.
        The angle is obtained dynamically from ImageData property.
        """
        # Rotation is now a dynamic property from ImageData
        # Just access it to trigger calculation if needed
        _ = self.rotation

    def fitModel(self):
        """
        Fit model to histogram
        Fit results will be kept in box.fit_results.
        """
        for name, box in self.boxes.items():
            # Skip if histogram data not available for this box
            if box.hist2 is None:
                continue
            
            # Skip if no peaks or already fitted
            if not box.peaks or len(box.peaks) == 0 or box.fit_results is not None:
                continue
            
            hist = np.array(box.hist2)
            peaks = box.peaks
            coords = box.coordinates
            start_x = coords[0][0]
            start_y = coords[1][0]

            x = np.arange(0, len(hist))

            int_vars = {
                'x' : x
            }

            # Initial Parameters
            params = Parameters()

            # Init Center X
            if box.type == 'h':
                init_center = self.center[0] - start_x
            elif box.type == 'oriented':
                init_center = coords[6][0] - start_x
            else:
                init_center = self.center[1] - start_y

            init_center = int(round(init_center))
            params.add('centerX', init_center, min=init_center - 1., max=init_center + 1.)

            if box.bgsub == 1:
                # Convex hull has been applied, so we don't need to fit 3 gaussian anymore
                int_vars['bg_line'] = 0
                int_vars['bg_sigma'] = 1
                int_vars['bg_amplitude'] = 0
                int_vars['center_sigma1'] = 1
                int_vars['center_amplitude1'] = 0
                int_vars['center_sigma2'] = 1
                int_vars['center_amplitude2'] = 0
            else:
                # Init linear background
                # params.add('bg_line', 0, min=0)
                int_vars['bg_line'] = 0

                if name in self.state.main_peak_info:
                    main_info = self.state.main_peak_info[name]
                    params.add('bg_sigma', main_info['bg_sigma'], min=1, max=len(hist)*2+1.)
                    params.add('bg_amplitude', main_info['bg_amplitude'], min=-1, max=sum(hist)+1.)
                    params.add('center_sigma1', main_info['center_sigma1'], min=1, max=len(hist)+1.)
                    params.add('center_amplitude1', main_info['center_amplitude1'], min=-1, max=sum(hist) + 1.)
                    params.add('center_sigma2', main_info['center_sigma2'], min=1, max=len(hist)+1.)
                    params.add('center_amplitude2', main_info['center_amplitude2'], min=-1, max=sum(hist)+1.)
                
                else:
                # Init background params
                    params.add('bg_sigma', len(hist)/3., min=1, max=len(hist)*2+1.)
                    params.add('bg_amplitude', 0, min=-1, max=sum(hist)+1.)

                    if box.merid_bg:
                        # Init Meridian params1
                        params.add('center_sigma1', 15, min=1, max=len(hist)+1.)
                        params.add('center_amplitude1', sum(hist) / 20., min=-1, max=sum(hist) + 1.)
                    else:
                        int_vars['center_sigma1'] = 1
                        int_vars['center_amplitude1'] = 0

                    # Init Meridian params2
                    params.add('center_sigma2',5 , min=1, max=len(hist)+1.)
                    params.add('center_amplitude2', sum(hist) / 20., min=-1, max=sum(hist)+1.)

            # Init peaks params
            # Check if GMM mode (shared sigma) is enabled for this box
            use_gmm = box.use_common_sigma
            
            # Fixed params directly from ProcessingBox
            fixed_center_for_box = box.fixed_center
            fixed_sigma_for_box = box.fixed_sigma
            fixed_amplitude_for_box = box.fixed_amplitude
            fixed_common_sigma_for_box = box.fixed_common_sigma
            
            # Check if hull_ranges exist to constrain peak search range
            # Get peak tolerance from settings (default 2.0 for backward compatibility)
            default_search_dist = box.peak_tolerance
            # Sigma tolerance percentage for initializing sigma/common_sigma bounds when none are stored
            # (used as ± tolerance% around the default initial value 5)
            sigma_tol_percent = box.sigma_tolerance
            hull_constraint = None
            if box.hull_range is not None:
                hull_start, hull_end = box.hull_range
                # hull_ranges: (start, end) are both positive numbers
                # Valid ranges: [start, end] for positive peaks, [-end, -start] for negative peaks
                hull_constraint = (hull_start, hull_end)
            
            if use_gmm:
                # GMM mode: all peaks share one sigma
                # Check if common_sigma is fixed
                if fixed_common_sigma_for_box is not None:
                    params.add('common_sigma', fixed_common_sigma_for_box, vary=False)
                else:
                    # Bounds-driven common_sigma: prefer persisted bounds; otherwise default [1, 50]
                    orig_cs_min, orig_cs_max = self._get_param_bounds(name, 'common_sigma')
                    cs_min, cs_max = orig_cs_min, orig_cs_max
                    if cs_min is None:
                        # Calculate tolerance as percentage of default value 5.0
                        cs_min = max(0.0, 5.0 * (1.0 - float(sigma_tol_percent) / 100.0))
                    if cs_max is None:
                        cs_max = 5.0 * (1.0 + float(sigma_tol_percent) / 100.0)
                    if cs_min > cs_max:
                        cs_min, cs_max = cs_max, cs_min
                    if cs_min == cs_max:
                        cs_min = max(0.0, cs_min - 0.5)
                        cs_max = cs_max + 0.5
                    # Persist defaults if missing
                    if orig_cs_min is None and orig_cs_max is None:
                        self._set_param_bounds(name, 'common_sigma', cs_min, cs_max)
                    params.add('common_sigma', 5, min=cs_min, max=cs_max)
                
                for j, p in enumerate(peaks):
                    # Per-peak bounds (scheme B):
                    # Prefer persisted bounds for p_j; otherwise initialize from Peak Tolerance.
                    stored_min, stored_max = self._get_param_bounds(name, f'p_{j}')
                    base_min = (p - default_search_dist) if stored_min is None else stored_min
                    base_max = (p + default_search_dist) if stored_max is None else stored_max

                    # Apply hull constraint by clamping bounds (not recomputing from p)
                    if hull_constraint is not None:
                        hull_start, hull_end = hull_constraint
                        if p > 0:
                            clamp_min, clamp_max = hull_start, hull_end
                        else:
                            clamp_min, clamp_max = -hull_end, -hull_start
                        p_min = max(base_min, clamp_min)
                        p_max = min(base_max, clamp_max)
                    else:
                        p_min, p_max = base_min, base_max

                    # Safety: ensure valid min/max
                    if p_min > p_max:
                        p_min, p_max = p_max, p_min
                    if p_min == p_max:
                        p_min -= 0.5
                        p_max += 0.5

                    # Persist bounds if they didn't exist yet (initialization step)
                    if stored_min is None or stored_max is None:
                        self._set_param_bounds(name, f'p_{j}', p_min, p_max)
                    
                    # Position: check if fixed
                    if j in fixed_center_for_box:
                        params.add('p_' + str(j), fixed_center_for_box[j], vary=False)
                    else:
                        params.add('p_' + str(j), p, min=p_min, max=p_max)
                    
                    # NOTE: In GMM mode, we do NOT add individual sigma parameters
                    # The layerlineModelGMM function will use common_sigma directly
                    # No sigma{j} parameters needed!
                    #
                    # However, for UI consistency (Parameter Editor), we still initialize
                    # per-peak sigma{i} bounds if missing so they can be displayed/edited.
                    s_name = f'sigma{j}'
                    orig_s_min, orig_s_max = self._get_param_bounds(name, s_name)
                    if orig_s_min is None and orig_s_max is None:
                        # Calculate tolerance as percentage of default value 5.0
                        s_min = max(0.0, 5.0 * (1.0 - float(sigma_tol_percent) / 100.0))
                        s_max = 5.0 * (1.0 + float(sigma_tol_percent) / 100.0)
                        if s_min > s_max:
                            s_min, s_max = s_max, s_min
                        if s_min == s_max:
                            s_min = max(0.0, s_min - 0.5)
                            s_max = s_max + 0.5
                        self._set_param_bounds(name, s_name, s_min, s_max)
                    
                    # Amplitude: check if fixed (no upper bound - amplitude is unconstrained)
                    if j in fixed_amplitude_for_box:
                        params.add('amplitude' + str(j), fixed_amplitude_for_box[j], vary=False, min=-1)
                    else:
                        params.add(f'amplitude{j}', sum(hist)/10., min=-1)
            else:
                # Original mode: each peak has independent sigma
                for j,p in enumerate(peaks):
                    # Per-peak bounds (scheme B): same as GMM branch.
                    stored_min, stored_max = self._get_param_bounds(name, f'p_{j}')
                    base_min = (p - default_search_dist) if stored_min is None else stored_min
                    base_max = (p + default_search_dist) if stored_max is None else stored_max

                    if hull_constraint is not None:
                        hull_start, hull_end = hull_constraint
                        if p > 0:
                            clamp_min, clamp_max = hull_start, hull_end
                        else:
                            clamp_min, clamp_max = -hull_end, -hull_start
                        p_min = max(base_min, clamp_min)
                        p_max = min(base_max, clamp_max)
                    else:
                        p_min, p_max = base_min, base_max

                    if p_min > p_max:
                        p_min, p_max = p_max, p_min
                    if p_min == p_max:
                        p_min -= 0.5
                        p_max += 0.5

                    if stored_min is None or stored_max is None:
                        self._set_param_bounds(name, f'p_{j}', p_min, p_max)
                    
                    # Position: check if fixed
                    if j in fixed_center_for_box:
                        params.add('p_' + str(j), fixed_center_for_box[j], vary=False)
                    else:
                        params.add('p_' + str(j), p, min=p_min, max=p_max)
                    
                    # Sigma: check if fixed
                    if j in fixed_sigma_for_box:
                        params.add('sigma' + str(j), fixed_sigma_for_box[j], vary=False)
                    else:
                        # Bounds-driven sigma: prefer persisted bounds; otherwise default [1, 50]
                        s_name = f'sigma{j}'
                        orig_s_min, orig_s_max = self._get_param_bounds(name, s_name)
                        s_min, s_max = orig_s_min, orig_s_max
                        if s_min is None:
                            # Calculate tolerance as percentage of default value 5.0
                            s_min = max(0.0, 5.0 * (1.0 - float(sigma_tol_percent) / 100.0))
                        if s_max is None:
                            s_max = 5.0 * (1.0 + float(sigma_tol_percent) / 100.0)
                        if s_min > s_max:
                            s_min, s_max = s_max, s_min
                        if s_min == s_max:
                            s_min = max(0.0, s_min - 0.5)
                            s_max = s_max + 0.5
                        if orig_s_min is None and orig_s_max is None:
                            self._set_param_bounds(name, s_name, s_min, s_max)
                        params.add(s_name, 5, min=s_min, max=s_max)
                    
                    # Amplitude: check if fixed (no upper bound - amplitude is unconstrained)
                    if j in fixed_amplitude_for_box:
                        params.add('amplitude' + str(j), fixed_amplitude_for_box[j], vary=False, min=-1)
                    else:
                        params.add(f'amplitude{j}', sum(hist)/10., min=-1)

            # Fit model - choose model based on mode
            if use_gmm:
                # Use GMM model that accepts common_sigma directly
                model = Model(layerlineModelGMM, nan_policy='propagate', independent_vars=int_vars.keys())
            else:
                # Use standard model with independent sigmas
                model = Model(layerlineModel, nan_policy='propagate', independent_vars=int_vars.keys())
            
            result = model.fit(hist, verbose=False, params=params, **int_vars)
            if result is not None:
                result_dict = result.values
                int_vars.pop('x')
                result_dict.update(int_vars)
                
                # Add explicit flag for GMM mode
                result_dict['use_common_sigma'] = use_gmm
                
                # Ensure complete data regardless of mode
                if use_gmm:
                    # GMM mode: common_sigma exists, copy to all individual sigmas
                    common_sigma_value = result_dict.get('common_sigma', 10.0)
                    for j in range(len(peaks)):
                        result_dict[f'sigma{j}'] = common_sigma_value
                else:
                    # Non-GMM mode: calculate common_sigma as mean of individual sigmas
                    sigmas = [result_dict.get(f'sigma{j}', 10.0) for j in range(len(peaks))]
                    result_dict['common_sigma'] = np.mean(sigmas) if sigmas else 10.0
                
                # Calculate error using the appropriate model function
                if use_gmm:
                    # For GMM mode, use layerlineModelGMM
                    predicted = layerlineModelGMM(x, **result_dict)
                else:
                    # For non-GMM mode, use layerlineModel
                    predicted = layerlineModel(x, **result_dict)
                result_dict['error'] = 1. - r2_score(hist, predicted)
                
                # Persist fixed flags into fit_results so UI reflects reality.
                if fixed_common_sigma_for_box is not None:
                    result_dict['common_sigma_fixed'] = True
                if isinstance(fixed_center_for_box, dict):
                    for idx, val in fixed_center_for_box.items():
                        key = f'p_{idx}'
                        if key in result_dict:
                            result_dict[key] = val
                        result_dict[f'{key}_fixed'] = True
                if isinstance(fixed_sigma_for_box, dict):
                    for idx, val in fixed_sigma_for_box.items():
                        key = f'sigma{idx}'
                        if key in result_dict:
                            result_dict[key] = val
                        result_dict[f'{key}_fixed'] = True
                if isinstance(fixed_amplitude_for_box, dict):
                    for idx, val in fixed_amplitude_for_box.items():
                        key = f'amplitude{idx}'
                        if key in result_dict:
                            result_dict[key] = val
                        result_dict[f'{key}_fixed'] = True
                
                if name in self.state.main_peak_info:
                    main_info = self.state.main_peak_info[name]
                    if main_info.get('bg_sigma_lock', False):
                        result_dict['bg_sigma'] = main_info['bg_sigma']
                    if main_info.get('bg_amplitude_lock', False):
                        result_dict['bg_amplitude'] = main_info['bg_amplitude']
                    if main_info.get('center_sigma1_lock', False):
                        result_dict['center_sigma1'] = main_info['center_sigma1']   
                    if main_info.get('center_amplitude1_lock', False):
                        result_dict['center_amplitude1'] = main_info['center_amplitude1']
                    if main_info.get('center_sigma2_lock', False):
                        result_dict['center_sigma2'] = main_info['center_sigma2']
                    if main_info.get('center_amplitude2_lock', False):
                        result_dict['center_amplitude2'] = main_info['center_amplitude2']
                
                # Save fit results to box
                box.fit_results = result_dict
                box.subtracted_hist = None  # Clear downstream results
                
                # Print fitting results
                print("Box : "+ str(name))
                print(f"Mode: {'GMM (Common Sigma)' if use_gmm else 'Independent Sigma'}")
                print(f"Number of free parameters: {result.nvarys}")
                print(f"Chi-square: {result.chisqr:.6f}")
                print(f"Reduced chi-square: {result.redchi:.6f}")
                if use_gmm:
                    common_sigma_val = result_dict.get('common_sigma', 'N/A')
                    print(f"  Common Sigma = {common_sigma_val}")
                else:
                    sigmas = [result_dict.get(f'sigma{i}', 'N/A') for i in range(min(3, len(peaks)))]
                    print(f"  Individual Sigmas (first 3): {sigmas}")
                print("Fitting Result : " + str(result_dict))
                print("Fitting Error : " + str(result_dict['error']))
                if hasattr(result, 'aic') and hasattr(result, 'bic'):
                    print(f"AIC: {result.aic:.2f}, BIC: {result.bic:.2f}")
                print("---")


    def getBackgroundSubtractedHistograms(self):
        """
        Get Background Subtracted Histograms by subtracting the background from fitting model
        :return:
        """
        for name, box in self.boxes.items():
            # Skip if already computed or no fit results
            if box.subtracted_hist is not None or box.fit_results is None:
                continue
            
            # Skip if no hist2
            if box.hist2 is None:
                continue
            
            if box.bgsub == 2:  # no background, so no subtraction
                box.subtracted_hist = box.hist2
            else:
                # Get subtracted histogram if fit result exists
                hist = box.hist2
                xs = np.arange(0, len(hist))
                background = layerlineModelBackground(xs, **box.fit_results)
                box.subtracted_hist = hist - background
            
            # Clear downstream results
            box.moved_peaks = None

    def getPeakInfos(self):
        """
        Get peaks' information including baseline and centroids
        :return:
        """
        for name, box in self.boxes.items():
            # Skip if no subtracted histogram
            if box.subtracted_hist is None:
                continue
            
            # Skip if no fit results
            if box.fit_results is None:
                continue

            ### Find real peak locations in the box (not distance from center)
            model = box.fit_results
            hist = box.subtracted_hist
            
            if box.moved_peaks is None:
                peaks = box.peaks
                moved = []
                for p in peaks:
                    globalpeak = int(round(model['centerX']+p))
                    if 0 <= globalpeak <= len(hist):
                        moved.append(globalpeak)
                
                # Use Gaussian fit positions directly (more accurate than movePeaks)
                # This respects fixed peaks and avoids noise-induced position errors
                box.moved_peaks = moved
                box.baselines = None  # Clear downstream results

            peaks = box.moved_peaks
            ### Calculate Baselines
            if box.baselines is None:
                baselines = []
                for p in peaks:
                    baselines.append(hist[p]*0.5)
                box.baselines = baselines
                box.centroids = None  # Clear downstream results

            baselines = box.baselines

            if box.centroids is None:
                results = getPeakInformations(hist, peaks, baselines)
                box.centroids = np.array(results['centroids']) - model['centerX']
                box.widths = results['widths']
                box.areas = results['areas']
                print("Box : "+ str(name))
                print("Centroid Result : " + str(results))
                print("---")

    def setGaussCenter(self, box_name, peak_num, new_center):
        """
        Fix a peak center position for fitting.
        :param box_name: box name (str)
        :param peak_num: peak index (int)
        :param new_center: fixed center value (float)
        """
        new_center = float(str(new_center))
        if box_name in self.boxes:
            self.boxes[box_name].fixed_center[peak_num] = new_center
            self.removeInfo(box_name, 'fit_results')

    def setGaussSig(self, box_name, peak_num, new_sigma):
        """
        Fix a Gaussian sigma for fitting and clear previous fit.
        :param box_name: box name (str)
        :param peak_num: peak index (int)
        :param new_sigma: fixed sigma value (float)
        """
        new_sigma = float(str(new_sigma))
        if box_name in self.boxes:
            self.boxes[box_name].fixed_sigma[peak_num] = new_sigma
            self.removeInfo(box_name, 'fit_results')

    def setBaseline(self, box_name, peak_num, new_baseline):
        """
        Change baseline and clear centroid and width for the specific box and peak
        :param box_name: box name (str)
        :param peak_num: peak name (int)
        :param new_baseline: new baseline value or percentage (str)
        :return:
        """
        new_baseline = str(new_baseline)
        baselines = self.boxes[box_name].baselines
        peak = self.boxes[box_name].moved_peaks[peak_num]
        hist = self.boxes[box_name].subtracted_hist
        height = hist[peak]
        if "%" in new_baseline:
            # if new_baseline contain "%", baseline value will use this as percent of peak height
            percent = float(new_baseline.rstrip("%"))
            baseline = height * percent / 100.
        elif len(new_baseline) == 0:
            # if new_baseline is empty, baseline will be half-height
            baseline = float(height * .5)
        else:
            baseline = float(new_baseline)

        if height > baseline:
            baselines[peak_num] = baseline
            self.removeInfo(box_name, 'centroids')
            self.removeInfo(box_name, 'widths')
            self.removeInfo(box_name, 'areas')


    def removeInfo(self, name, k=None):
        """
        Remove information from box. If k is None, remove the entire box.
        :param name: box name
        :param k: specific result key to clear (e.g. 'fit_results', 'centroids')
        :return: -
        """
        if name not in self.boxes:
            return
        
        if k is None:
            # Remove entire box
            del self.boxes[name]
        else:
            # Clear specific result
            box = self.boxes[name]
            # Map old dict keys to box attributes
            key_map = {
                'hists': 'hist',
                'hists2': 'hist2',
                'fit_results': 'fit_results',
                'subtracted_hists': 'subtracted_hist',
                'moved_peaks': 'moved_peaks',
                'baselines': 'baselines',
                'centroids': 'centroids',
                'widths': 'widths',
                'areas': 'areas',
                'param_bounds': 'param_bounds',
            }
            
            if k in key_map:
                attr = key_map[k]
                if attr in ['moved_peaks', 'baselines', 'widths', 'areas']:
                    setattr(box, attr, None)
                elif attr == 'param_bounds':
                    setattr(box, attr, {})
                else:
                    setattr(box, attr, None)

    def loadCache(self) -> Optional[ProcessingState]:
        """
        Load ProcessingState from cache. Cache file will be filename.cache in folder "pt_cache"
        :return: cached ProcessingState or None
        """
        cache_path = fullPath(self.dir_path, "pt_cache")
        cache_file = fullPath(cache_path, self.filename + '.cache')
        
        if not exists(cache_file):
            return None
        
        try:
            with open(cache_file, 'rb') as f:
                state = pickle.load(f)
            
            # Version check
            if isinstance(state, ProcessingState) and state.version == self.version:
                return state
            else:
                version = state.version if isinstance(state, ProcessingState) else "unknown"
                print(f"Cache version {version} did not match with Program version {self.version}")
                print("Invalidating cache and reprocessing the image")
                return None
        except Exception as e:
            print(f"Failed to load cache: {e}")
            return None

    def cacheInfo(self):
        """
        Save ProcessingState to cache. Cache file will be saved as filename.cache in folder "pt_cache"
        :return: -
        """
        cache_path = fullPath(self.dir_path, 'pt_cache')
        createFolder(cache_path)
        cache_file = fullPath(cache_path, self.filename + '.cache')
        
        with open(cache_file, 'wb') as f:
            pickle.dump(self.state, f, protocol=pickle.HIGHEST_PROTOCOL)


def layerlineModel(x, centerX, bg_line, bg_sigma, bg_amplitude, center_sigma1, center_amplitude1,
                    center_sigma2, center_amplitude2, **kwargs):
    """
    Model for fitting layer line pattern
    :param x: x axis
    :param centerX: center of x axis
    :param bg_line: linear background
    :param bg_sigma: background sigma
    :param bg_amplitude: background amplitude
    :param center_sigma1: meridian background sigma
    :param center_amplitude1: meridian background amplitude
    :param center_sigma2: meridian sigma
    :param center_amplitude2: meridian amplitude
    :param kwargs: other peaks properties
    :return:
    """
    #### Background and Meridian
    result = layerlineModelBackground(x, centerX, bg_line, bg_sigma, bg_amplitude, center_sigma1, center_amplitude1, center_sigma2, center_amplitude2,**kwargs)
    #### Other peaks
    i = 0
    while 'p_'+str(i) in kwargs:
        p = kwargs['p_'+str(i)]
        sigma = kwargs['sigma'+str(i)]
        amplitude = kwargs['amplitude' + str(i)]
        if 'gamma' + str(i) in kwargs:
            gamma = kwargs['gamma' + str(i)]

            mod = VoigtModel()
            result += mod.eval(x=x, amplitude=amplitude, center=centerX + p, sigma=sigma, gamma=gamma)
            # result += mod.eval(x=x, amplitude=amplitude, center=centerX - p, sigma=sigma, gamma=-gamma)
        else:
            mod = GaussianModel()
            result += mod.eval(x=x, amplitude=amplitude, center=centerX + p, sigma=sigma)
            # result += mod.eval(x=x, amplitude=amplitude, center=centerX - p, sigma=sigma)

        i += 1
    return result

def layerlineModelGMM(x, centerX, bg_line, bg_sigma, bg_amplitude, center_sigma1, center_amplitude1,
                      center_sigma2, center_amplitude2, common_sigma, **kwargs):
    """
    GMM version: Model for fitting layer line pattern with common sigma for all peaks
    :param x: x axis
    :param centerX: center of x axis
    :param bg_line: linear background
    :param bg_sigma: background sigma
    :param bg_amplitude: background amplitude
    :param center_sigma1: meridian background sigma
    :param center_amplitude1: meridian background amplitude
    :param center_sigma2: meridian sigma
    :param center_amplitude2: meridian amplitude
    :param common_sigma: shared sigma for all peaks (GMM constraint)
    :param kwargs: other peaks properties (p_i, amplitude_i, optionally gamma_i)
    :return:
    """
    #### Background and Meridian
    result = layerlineModelBackground(x, centerX, bg_line, bg_sigma, bg_amplitude, 
                                     center_sigma1, center_amplitude1, 
                                     center_sigma2, center_amplitude2, **kwargs)
    
    #### Other peaks - all using common_sigma
    i = 0
    while 'p_'+str(i) in kwargs:
        p = kwargs['p_'+str(i)]
        amplitude = kwargs['amplitude' + str(i)]
        
        if 'gamma' + str(i) in kwargs:
            gamma = kwargs['gamma' + str(i)]
            mod = VoigtModel()
            result += mod.eval(x=x, amplitude=amplitude, center=centerX + p, 
                             sigma=common_sigma, gamma=gamma)
        else:
            mod = GaussianModel()
            result += mod.eval(x=x, amplitude=amplitude, center=centerX + p, 
                             sigma=common_sigma)  # All peaks use common_sigma
        i += 1
    
    return result

def layerlineModelBackground(x, centerX, bg_line, bg_sigma, bg_amplitude, center_sigma1, center_amplitude1,
                            center_sigma2, center_amplitude2,**kwargs):
    """
    Model for fitting layer line pattern
    :param x: x axis
    :param centerX: center of x axis
    :param bg_line: linear background
    :param bg_sigma: background sigma
    :param bg_amplitude: background amplitude
    :param center_sigma1: meridian background sigma
    :param center_amplitude1: meridian background amplitude
    :param center_sigma2: meridian sigma
    :param center_amplitude2: meridian amplitude
    :param kwargs: nothing
    :return:
    """
    return layerlineBackground(x, centerX, bg_line, bg_sigma, bg_amplitude) + \
           meridianBackground(x, centerX, center_sigma1, center_amplitude1) + \
           meridianGauss(x, centerX, center_sigma2, center_amplitude2)


def layerlineBackground(x, centerX, bg_line, bg_sigma, bg_amplitude, **kwargs):
    """
    Model for largest background of layer line pattern
    :param x: x axis
    :param centerX: center of x axis
    :param bg_line: linear background
    :param bg_sigma: background sigma
    :param bg_amplitude: background amplitude
    :param kwargs: nothing
    :return:
    """
    mod = GaussianModel()
    return  mod.eval(x=x, amplitude=bg_amplitude, center=centerX, sigma=bg_sigma) + bg_line

def meridianBackground(x, centerX, center_sigma1, center_amplitude1, **kwargs):
    """
    Model for background of meridian of layer line pattern
    :param x: x axis
    :param centerX: center of x axis
    :param center_sigma1: meridian background sigma
    :param center_amplitude1: meridian background amplitude
    :param kwargs: nothing
    :return:
    """
    mod = GaussianModel()
    return mod.eval(x=x, amplitude=center_amplitude1, center=centerX, sigma=center_sigma1)

def meridianGauss(x, centerX, center_sigma2, center_amplitude2, **kwargs):
    """
    Model for background of layer line pattern
    :param x: x axis
    :param centerX: center of x axis
    :param center_sigma2: meridian sigma
    :param center_amplitude2: meridian amplitude
    :param kwargs:
    :return:
    """
    mod = GaussianModel()
    return mod.eval(x=x, amplitude=center_amplitude2, center=centerX, sigma=center_sigma2)
