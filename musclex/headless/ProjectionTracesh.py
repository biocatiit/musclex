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
        if cache is not None and not self.delcache:
            box_config = cache
        elif savedParams is not None:
            box_config = savedParams
            self.calSettings = savedParams
        else:
            box_config = None
        
        # Extract global settings from config
        if box_config is not None:
            self.centerx = box_config.get('centerx')
            self.centery = box_config.get('centery')
            self.mask_thres = box_config.get('mask_thres', -999)
        
        self.onImageChanged(box_config)

    def onImageChanged(self, box_config=None):
        """
        Called when image changes (e.g. to next image in batch).
        Creates a new ProjectionProcessor and populates boxes from config.
        
        :param box_config: Dictionary containing box configurations from cache or settings file
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
        
        # Populate boxes from config directly into projProc.boxes
        if box_config is not None:
            boxes_dict = box_config.get('boxes', {})
            types_dict = box_config.get('types', {})
            bgsubs_dict = box_config.get('bgsubs', {})
            peaks_dict = box_config.get('peaks', {})
            merid_bg_dict = box_config.get('merid_bg', {})
            hull_ranges_dict = box_config.get('hull_ranges', {})
            
            for box_name, coords in boxes_dict.items():
                box = ProcessingBox(
                    name=box_name,
                    coordinates=coords,
                    type=types_dict.get(box_name, 'h'),
                    bgsub=bgsubs_dict.get(box_name, 0),
                    peaks=peaks_dict.get(box_name, []),
                    merid_bg=merid_bg_dict.get(box_name, False),
                    hull_range=hull_ranges_dict.get(box_name)
                )
                self.projProc.boxes[box_name] = box
        
        # Process new image
        self.processImage()

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
        Extracts box configurations from ProcessingBox objects.
        """
        # Extract box configurations from ProcessingBox objects
        boxes = {}
        peaks = {}
        types = {}
        bgsubs = {}
        merid_bg = {}
        hull_ranges = {}
        
        for name, box in self.projProc.boxes.items():
            boxes[name] = box.coordinates
            peaks[name] = box.peaks
            types[name] = box.type
            bgsubs[name] = box.bgsub
            merid_bg[name] = box.merid_bg
            if box.hull_range is not None:
                hull_ranges[name] = box.hull_range
        
        cache = {
            'boxes': boxes,
            'peaks': peaks,
            'types': types,
            'bgsubs': bgsubs,
            'merid_bg': merid_bg,
            'hull_ranges': hull_ranges,
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
