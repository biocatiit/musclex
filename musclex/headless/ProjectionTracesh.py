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
import sys
import json
import pickle
import traceback
from os.path import exists, splitext
import numpy as np
from musclex import __version__
from ..utils.file_manager import fullPath, getImgFiles, createFolder
from ..utils.image_processor import getMaskThreshold
from ..modules.ProjectionProcessor import ProjectionProcessor
from ..csv_manager import PT_CSVManager

class BoxDetails:
    """
    This class is for Popup window when a box is added
    """
    def __init__(self, current_box_names, oriented=False):
        self.box_names = current_box_names
        self.oriented = oriented

class ProjectionTracesh:
    """
    This class is for Projection Traces headless mode.
    
    Note: Box configurations are now managed directly by ProcessingBox objects
    in self.projProc.boxes, following the same architecture as the GUI.
    """
    def __init__(self, filename, inputsettings, delcache, settingspath=os.path.join('musclex', 'settings', 'ptsettings.json'), lock=None, dir_path=None, imgList=None, currentFileNumber=None, fileList=None, ext=None):
        self.lock = lock
        self.current_file = 0
        self.calSettings = None
        self.projProc = None
        self.csvManager = None
        self.masked = False
        self.mask_thres = -999
        self.centerx = None
        self.centery = None
        # Note: center_func removed - center is now managed by ImageData (matching GUI architecture)
        self.refit = False

        self.version = __version__
        if dir_path is not None:
            self.dir_path, self.imgList, self.current_file, self.fileList, self.ext = dir_path, imgList, currentFileNumber, fileList, ext
        else:
            self.dir_path, self.imgList, self.current_file, self.fileList, self.ext = getImgFiles(str(filename), headless=True)
        if len(self.imgList) == 0:
            self.inputerror()
            return
        self.inputsettings=inputsettings
        self.delcache=delcache
        self.settingspath=settingspath

        fileName = self.imgList[self.current_file]
        file=fileName+'.info'
        cache_path = os.path.join(self.dir_path, "qf_cache", file)
        cache_exist=os.path.isfile(cache_path)
        if self.delcache:
            if cache_exist:
                os.remove(cache_path)

        self.onImageSelect()

    def onImageSelect(self):
        """
        Triggered when a new image is selected.
        Loads saved settings and creates ProjectionProcessor with boxes.
        """
        savedParams = self.getSavedBoxesAndPeaks()
        cache = self.loadBoxesAndPeaks()
        
        # Determine source of box configuration
        # is_from_cache flag tells onImageChanged whether peaks need mirroring
        if cache is not None and not self.delcache:
            box_config = cache
            is_from_cache = True  # Cache has already mirrored peaks
        elif savedParams is not None:
            box_config = savedParams
            self.calSettings = savedParams
            is_from_cache = False  # Settings file has only half peaks
        else:
            box_config = None
            is_from_cache = False
        
        # Extract global settings from config
        if box_config is not None:
            self.centerx = box_config.get('centerx')
            self.centery = box_config.get('centery')
            self.mask_thres = box_config.get('mask_thres', -999)
        
        self.onImageChanged(box_config, is_from_cache=is_from_cache)

    def onImageChanged(self, box_config=None, is_from_cache=False):
        """
        Called when image changes (e.g. to next image in batch).
        Creates a new ProjectionProcessor and populates boxes from config.
        
        :param box_config: Dictionary containing box configurations from cache or settings file
                           Expected format (NEW): {'boxes': {'name': {...}}, 'centerx': ..., 'centery': ..., 'mask_thres': ...}
        :param is_from_cache: If True, peaks are already mirrored (from cache). If False, need to mirror (from settings file).
        """
        # Load image data
        import fabio
        from ..utils.image_data import ImageData
        from ..utils.file_manager import fullPath
        from ..modules.ProjectionProcessor import ProcessingBox
        
        img_name = self.imgList[self.current_file]
        img_full_path = fullPath(self.dir_path, img_name)
        img = fabio.open(img_full_path).data
        
        # Create ImageData object
        image_data = ImageData(img, self.dir_path, img_name)
        
        # Create ProjectionProcessor with ImageData
        self.projProc = ProjectionProcessor(image_data)
        # Headless mode: disable global rotation
        # Box coordinates in ptsettings.json are based on the original (unrotated) image
        # So we skip the global rotation to maintain coordinate consistency
        self.projProc._image_data.set_manual_rotation(0)
        
        if self.mask_thres == -999:
            self.mask_thres = getMaskThreshold(self.projProc.orig_img)
        
        # Set center from config if available, otherwise use ImageData's auto-calculated center
        if self.centerx is not None and self.centery is not None:
            # Use center from config (stored from previous GUI session)
            self.projProc._image_data.set_manual_center((self.centerx, self.centery))
        
        # Get final center from ImageData (manual or auto-calculated)
        center = self.projProc._image_data.center
        self.centerx, self.centery = center
        
        # Populate boxes from config (NEW FORMAT: boxes contain all info as objects)
        if box_config is not None and 'boxes' in box_config:
            boxes_dict = box_config['boxes']
            
            for box_name, box_dict in boxes_dict.items():
                # Create ProcessingBox from dict (new format with all properties)
                box = ProcessingBox(
                    name=box_dict.get('name', box_name),
                    coordinates=box_dict.get('coordinates'),
                    type=box_dict.get('type', 'h'),
                    bgsub=box_dict.get('bgsub', 0),
                    peaks=box_dict.get('peaks', []).copy() if box_dict.get('peaks') else [],
                    merid_bg=box_dict.get('merid_bg', False),
                    hull_range=tuple(box_dict['hull_range']) if box_dict.get('hull_range') else None,
                    param_bounds=box_dict.get('param_bounds', {}).copy() if box_dict.get('param_bounds') else {},
                    use_common_sigma=box_dict.get('use_common_sigma', False),
                    peak_tolerance=box_dict.get('peak_tolerance', 2.0),
                    sigma_tolerance=box_dict.get('sigma_tolerance', 100.0)
                )
                
                # Expand peaks by mirroring ONLY if loading from settings file
                # Cache already has mirrored peaks, so skip mirroring to avoid double-mirror
                if not is_from_cache:
                    self._expand_peaks_mirrored(box)
                else:
                    print(f"  [onImageChanged] Box '{box.name}': Loaded from cache, peaks already mirrored (count={len(box.peaks)})")
                
                self.projProc.boxes[box_name] = box
        
        # Process new image
        self.processImage()

    def _expand_peaks_mirrored(self, box):
        """
        Expand peaks in a ProcessingBox by mirroring the first half.
        
        User-selected peaks (first half) are mirrored to create symmetric peaks.
        For example: [10, 20, 30] -> [10, 20, 30, -10, -20, -30]
        
        Modifies box.peaks in-place.
        
        Note: hull_range is NOT mirrored because it's already a symmetric concept:
        hull_range = (start, end) means distance from center, applied to both sides.
        
        Args:
            box: ProcessingBox with user-selected peaks (first half only)
        """
        if not box.peaks:
            print(f"  [_expand_peaks_mirrored] Box '{box.name}': No peaks to expand")
            return
        
        # Mirror peaks: first half stays, add mirrored second half
        user_peaks = box.peaks  # Already only the first half
        mirrored_peaks = [-p for p in user_peaks]
        box.peaks = user_peaks + mirrored_peaks
        
        print(f"  [_expand_peaks_mirrored] Box '{box.name}': {len(user_peaks)} user peaks → {len(box.peaks)} total peaks")
        print(f"    User selected: {user_peaks}")
        print(f"    After mirroring: {box.peaks}")
        
        # hull_range doesn't need mirroring - it's already symmetric
        # (start, end) defines distance ranges from center for both positive and negative sides

    def processImage(self):
        """
        Process Image by applying settings and calling process() of ProjectionProcessor.
        Then, write data to CSV and export histograms.
        """
        if self.projProc is None:
            return
        self.applySettings()
        try:
            self.projProc.process()
        except Exception:
            print('Unexpected error')
            msg = 'Please report the problem with error message below and the input image\n\n'
            msg += "Error : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
            print(msg)
            raise

        # acquire the lock
        if self.lock is not None:
            self.lock.acquire()
        
        self.cacheBoxesAndPeaks()
        
        # Use boxes directly from projProc - no conversion needed
        self.csvManager = PT_CSVManager(self.dir_path, self.projProc.boxes)
        self.csvManager.loadSummary()
        self.csvManager.writeNewData(self.projProc)
        self.exportHistograms()
        
        # release the lock
        if self.lock is not None:
            self.lock.release()

    def exportHistograms(self):
        """
        Export both original histograms and background subtracted histograms if Export All Projections is checked
        :return:
        """
        if self.projProc:
            path = fullPath(self.dir_path, os.path.join('pt_results', '1d_projections'))
            createFolder(path)
            fullname = str(self.projProc.filename)
            filename, _ = splitext(fullname)
            orig_hists = {name: box.hist for name, box in self.projProc.boxes.items() if box.hist is not None}
            subtr_hists = {name: box.subtracted_hist for name, box in self.projProc.boxes.items() if box.subtracted_hist is not None}

            for k in orig_hists.keys():
                hist = orig_hists[k]
                xs = np.arange(len(hist))
                f = open(fullPath(path, filename+'_box_'+str(k)+'_original.txt'), 'w')
                coords = zip(xs, hist)
                f.write("\n".join(list(map(lambda c : str(c[0])+"\t"+str(c[1]), coords))))
                if k in subtr_hists:
                    sub_hist = subtr_hists[k]
                    f = open(fullPath(path, filename+'_box_' + str(k) + '_subtracted.txt'), 'w')
                    coords = zip(xs, sub_hist)
                    f.write("\n".join(list(map(lambda c: str(c[0]) + "\t" + str(c[1]), coords))))

    def cacheBoxesAndPeaks(self):
        """
        Save the boxes and peaks in the cache file.
        Uses NEW FORMAT: boxes contain all info as objects (matching GUI).
        """
        # Extract box configurations from ProcessingBox objects (NEW FORMAT)
        boxes = {}
        
        for name, box in self.projProc.boxes.items():
            boxes[name] = {
                'name': box.name,
                'coordinates': box.coordinates,
                'type': box.type,
                'bgsub': box.bgsub,
                'peaks': box.peaks,
                'merid_bg': box.merid_bg,
                'hull_range': box.hull_range,
                'param_bounds': box.param_bounds if hasattr(box, 'param_bounds') else {},
                'use_common_sigma': box.use_common_sigma if hasattr(box, 'use_common_sigma') else False,
                'peak_tolerance': box.peak_tolerance if hasattr(box, 'peak_tolerance') else 2.0,
                'sigma_tolerance': box.sigma_tolerance if hasattr(box, 'sigma_tolerance') else 100.0
            }
        
        cache = {
            'boxes': boxes,
            'centerx': self.centerx,
            'centery': self.centery,
            'mask_thres': self.mask_thres
        }
        cache_dir = fullPath(self.dir_path, 'pt_cache')
        createFolder(cache_dir)
        cache_file = fullPath(cache_dir, 'boxes_peaks.info')
        pickle.dump(cache, open(cache_file, "wb"))

    def loadBoxesAndPeaks(self):
        """
        Load the boxes and peaks stored in the cache file, if it exists
        """
        cache_file = fullPath(fullPath(self.dir_path, 'pt_cache'), 'boxes_peaks.info')
        if exists(cache_file):
            cache = pickle.load(open(cache_file, "rb"))
            if cache is not None:
                return cache
        return None

    def getSavedBoxesAndPeaks(self):
        """
        Import json saved boxes
        """
        settingspath=self.settingspath
        if self.inputsettings:
            try:
                with open(settingspath, 'r') as f:
                    settings = json.load(f)
                # for b in settings["boxes"].items():
                #         if len(b[1][-1]) > 3:
                #             settings["boxes"][b[0]] = np.array(b[1][-1])
            except Exception:
                self.statusPrint("Can't load setting file")
                self.inputsettings = False
                settings = None
            return settings

    def applySettings(self):
        """
        Apply current settings directly to ProjectionProcessor state.
        
        Note: Box configurations are managed by ProcessingBox objects in projProc.boxes.
        This method writes global settings directly to projProc.state.
        """
        if self.projProc is None:
            return
        
        # Mask threshold
        self.projProc.state.mask_thres = self.mask_thres

        # Handle refit flag - clear fit results directly
        if self.refit:
            for box in self.projProc.boxes.values():
                box.clear_results(from_stage='fit')
            self.refit = False

        # Calibration settings
        if self.calSettings is not None:
            if 'type' in self.calSettings:
                if self.calSettings["type"] == "img":
                    self.projProc.state.lambda_sdd = self.calSettings["silverB"] * self.calSettings["radius"]
                elif self.calSettings["type"] == "cont":
                    self.projProc.state.lambda_sdd = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings["pixel_size"]
            
            if "center" in self.calSettings:
                # Set calibration center on ImageData (overrides any previous center)
                self.projProc._image_data.set_manual_center(self.calSettings["center"])
            
            if "detector" in self.calSettings:
                # Write to state
                self.projProc.state.detector = self.calSettings["detector"]

    def inputerror(self):
        """
        Display input error to screen
        """
        self.statusPrint('Invalid Input')
        self.statusPrint("Please select non empty failedcases.txt or an image\n\n")

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        if text != "":
            pid = os.getpid()
            ptext = "[Process "+str(pid)+"] "+str(text)
            print(ptext)
        else:
            print(text)
