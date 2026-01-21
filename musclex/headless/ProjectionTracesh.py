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
from ..utils.image_processor import getMaskThreshold, getCenter, processImageForIntCenter
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
    This class is for Projection Traces GUI Object
    """
    def __init__(self, filename, inputsettings, delcache, settingspath=os.path.join('musclex', 'settings', 'ptsettings.json'), lock=None, dir_path=None, imgList=None, currentFileNumber=None, fileList=None, ext=None):
        self.lock = lock
        self.current_file = 0
        self.calSettings = None
        self.projProc = None
        self.csvManager = None
        self.masked = False
        self.allboxes = {}
        self.boxes_on_img = {}
        self.boxtypes = {}
        self.bgsubs = {}
        self.merid_bg = {}
        self.peaks = {}
        self.hull_ranges = {}
        self.mask_thres = -999
        self.centerx = None
        self.centery = None
        self.center_func = None
        self.rotated = False
        self.rotationAngle = 0
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

        if self.inputsettings:
            self.getSettings()
        self.onImageSelect()

    def onImageSelect(self):
        """
        Triggered when a new image is selected
        :return:
        """
        # self.dir_path, self.imgList, self.current_file, self.fileList, self.ext = getImgFiles(fullfilename)
        savedParams = self.getSavedBoxesAndPeaks()
        cache = self.loadBoxesAndPeaks()
        if cache is not None and not self.delcache:
            self.allboxes = cache['boxes']
            self.peaks = cache['peaks']
            self.boxtypes = cache['types']
            self.bgsubs = cache['bgsubs']
            self.merid_bg = cache['merid_bg']
            self.hull_ranges = cache['hull_ranges']
            self.centerx = cache['centerx']
            self.centery = cache['centery']
            self.center_func = cache['center_func']
            self.mask_thres = cache['mask_thres']
        elif savedParams is not None:
            self.allboxes = savedParams['boxes']
            self.peaks = savedParams['peaks']
            self.boxtypes = savedParams['types']
            self.bgsubs = savedParams['bgsubs']
            self.merid_bg = savedParams['merid_bg']
            self.hull_ranges = savedParams['hull_ranges']
            self.centerx = savedParams['centerx']
            self.centery = savedParams['centery']
            self.center_func = savedParams['center_func']
            self.mask_thres = savedParams['mask_thres']
            self.calSettings = savedParams
        else:
            self.allboxes = {}
            self.peaks = {}
        self.onImageChanged()

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new ProjectionProcessor object for the new image if cache is available
        Process the new image if there's no cache.
        """
        # Load image data
        import fabio
        from ..utils.image_data import ImageData
        from ..utils.file_manager import fullPath
        
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
        
        # Get center from ImageData instead of info
        center = self.projProc._image_data.center
        self.centerx, self.centery = center
        
        if self.center_func is None:
            self.center_func = 'init'
        elif self.center_func != 'init':
            self.updateCenter() # do not update fit results for 'init' mode
        # Process new image
        self.processImage()

    def updateCenter(self, refit=True):
        """
        Update the image center
        :return:
        """
        if self.center_func == 'automatic':
            self.projProc.orig_img, center = processImageForIntCenter(self.projProc.orig_img, getCenter(self.projProc.orig_img))
            self.centerx, self.centery = center
        elif self.center_func == 'quadrant_fold': # default to quadrant folded
            self.centerx = self.projProc.orig_img.shape[1] / 2. - 0.5
            self.centery = self.projProc.orig_img.shape[0] / 2. - 0.5
        elif self.center_func == 'init': # loading from cache - center already set above
            pass  # centerx and centery already set from ImageData

        self.projProc.cache = None
        self.refit = refit

    def processImage(self):
        """
        Process Image by getting all settings and call process() of ProjectionTraces object
        Then, write data and update UI
        """
        if self.projProc is None:
            return
        settings = self.getSettings()
        try:
            self.projProc.process(settings)
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
        
        # Convert old-style boxes/peaks dicts to ProcessingBox objects
        from ..modules.ProjectionProcessor import ProcessingBox
        processing_boxes = {}
        for box_name, coords in self.allboxes.items():
            box_type = self.boxtypes.get(box_name, 'h')  # default to horizontal
            box_peaks = self.peaks.get(box_name, [])
            processing_boxes[box_name] = ProcessingBox(
                name=box_name,
                coordinates=coords,
                type=box_type,
                bgsub=0,  # default gaussian fitting
                peaks=box_peaks
            )
        
        self.csvManager = PT_CSVManager(self.dir_path, processing_boxes)
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
        Save the boxes and peaks in the cache file
        """
        cache = {
            'boxes' : self.allboxes,
            'peaks' : self.peaks,
            'types' : self.boxtypes,
            'bgsubs' : self.bgsubs,
            'merid_bg' : self.merid_bg,
            'hull_ranges' : self.hull_ranges,
            'centerx' : self.centerx,
            'centery' : self.centery,
            'center_func' : self.center_func,
            'mask_thres' : self.mask_thres
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

    def getSettings(self):
        """
        Give the current settings
        :return: settings
        """
        settings = {}
        # add boxes
        settings['boxes'] = self.allboxes

        # add box types
        settings['types'] = self.boxtypes

        # add bgsub methods
        settings['bgsubs'] = self.bgsubs

        # add meridian bg on/off
        settings['merid_bg'] = self.merid_bg

        # add peaks location
        settings['peaks'] = self.peaks

        # add hull ranges
        settings['hull_ranges'] = self.hull_ranges

        # add blank image and mask
        # if 'blank_mask' in self.projProc.info:
        #     settings['blank_mask'] = self.projProc.info['blank_mask']
        
        settings['mask_thres'] = self.mask_thres

        if self.refit:
            settings['refit'] = self.refit
            self.refit = False

        if self.center_func == 'manual' or self.rotated:
            settings['rotated'] = True
            self.rotated = True
            if self.rotationAngle != 0:
                settings['rotationAngle'] = self.rotationAngle

        if self.calSettings is not None:
            if 'type' in self.calSettings:
                if self.calSettings["type"] == "img":
                    settings["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
                elif self.calSettings["type"] == "cont":
                    settings["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings["pixel_size"]
            if "center" in self.calSettings and self.center_func != 'manual':
                settings["center"] = self.calSettings["center"]
                # Set center on ImageData instead of info (info is read-only)
                self.projProc._image_data.set_manual_center(self.calSettings["center"])
            # else: No need to delete - center will auto-calculate if not manually set
            
            if "detector" in self.calSettings:
                # Write to state, not info (info is read-only)
                self.projProc.state.detector = self.calSettings["detector"]

        return settings

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
