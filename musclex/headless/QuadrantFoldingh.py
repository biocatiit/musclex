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

import sys
import json
import os
from os.path import splitext
import traceback
import fabio
import pandas as pd
from PIL import Image
from musclex import __version__
try:
    from ..utils.file_manager import *
    from ..utils.background_search import makeFullImage
    from ..utils.image_processor import *
    from ..utils.qf_defaults import build_default_flags
    from ..modules.QuadrantFolder import QuadrantFolder
    from ..csv_manager.QF_CSVManager import QF_CSVManager
except: # for coverage
    from utils.file_manager import *
    from utils.background_search import makeFullImage
    from utils.image_processor import *
    from utils.qf_defaults import build_default_flags
    from modules.QuadrantFolder import QuadrantFolder
    from csv_manager.QF_CSVManager import QF_CSVManager

class QuadrantFoldingh:
    """
    Window displaying all information of a selected image.
    This window contains 2 tabs : image, and result
    """
    def __init__(self, filename, inputsettings, delcache, settingspath=os.path.join('musclex', 'settings', 'qfsettings.json'), lock=None, dir_path=None, imgList=None, currentFileNumber=None, fileList=None, ext=None, output_dir=None):
        """
        :param filename: selected file name
        :param inputsettings: flag for input setting file
        :param delcache: flag for deleting cache
        :param settingspath: setting file directory
        """
        self.version = __version__
        self.quadFold = None # QuadrantFolder object
        self.img_zoom = None # zoom location of original image (x,y range)
        self.default_img_zoom = None # default zoom calculated after processing image
        self.default_result_img_zoom = None # default result image zoom calculated after processing image
        self.result_zoom = None # zoom location of result image (x,y range)
        self.function = None # current active function
        self.updated = {'img': False, 'result': False} # update state of 2 tabs
        self.BGImages = []
        self.calSettings = None
        self.ignoreFolds = set()
        self.csv_bg = None
        self.orientationModel = None
        self.modeOrientation = None
        self.newImgDimension = None
        self.lock = lock
        if dir_path is not None:
            self.dir_path, self.imgList, self.currentFileNumber, self.fileList, self.ext = dir_path, imgList, currentFileNumber, fileList, ext
        else:
            self.dir_path, self.imgList, self.currentFileNumber, self.fileList, self.ext = getImgFiles(str(filename), headless=True)
        self.numberOfFiles = len(self.imgList)
        if len(self.imgList) == 0:
            self.inputerror()
            return
        self.inputsettings=inputsettings
        self.delcache=delcache
        self.settingspath=settingspath
        if output_dir:
            self.output_dir = output_dir
        elif os.access(self.dir_path, os.W_OK):
            self.output_dir = self.dir_path
        else:
            print(f"Error: input directory is not writable and no output directory was specified.\n"
                  f"  Input : {self.dir_path}\n"
                  f"  Fix   : re-run with -o <output_dir>", flush=True)
            sys.exit(1)

        fileName = self.imgList[self.currentFileNumber]
        file=fileName+'.info'
        cache_path = os.path.join(self.output_dir, "qf_cache", file)
        cache_exist=os.path.isfile(cache_path)
        if self.delcache:
            if cache_exist:
                os.remove(cache_path)
        
        # Load the image using fabio
        img_full_path = fullPath(self.dir_path, fileName)
        img = fabio.open(img_full_path).data
        
        # Load calibration settings if needed (for flags/parameters, not center)
        if self.inputsettings:
            try:
                import json
                with open(self.settingspath, 'r') as f:
                    self.calSettings = json.load(f)
            except:
                self.calSettings = None

        # Create ImageData with manual center/rotation from center_settings.json
        from ..utils.image_data import ImageData
        from ..utils.settings_manager import SettingsManager
        settings_manager = SettingsManager(self.dir_path)
        manual_center = settings_manager.get_center(fileName)
        manual_rotation = settings_manager.get_rotation(fileName)
        image_data = ImageData(img, self.dir_path, fileName,
                               center=manual_center, rotation=manual_rotation,
                               settings_manager=settings_manager)
        
        # Create QuadrantFolder with ImageData
        self.quadFold = QuadrantFolder(image_data, self, output_dir=self.output_dir)

        self.onImageChanged()

    def inputerror(self):
        """
        Display input error to screen
        """
        self.statusPrint('Invalid Input')
        self.statusPrint("Please select non empty failedcases.txt or an image\n\n")

    def ableToProcess(self):
        """
        Check if image can be processed
        """
        return self.quadFold is not None

    def deleteInfo(self, delList):
        """
        Remove input keys from info dict of current QuadrantFolder object
        :param delList: list of keys
        """
        if self.ableToProcess():
            for inf in delList:
                if inf in self.quadFold.info.keys():
                    del self.quadFold.info[inf]

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new QuadrantFolder object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        fileName = self.imgList[self.currentFileNumber]
        file=fileName+'.info'
        cache_path = os.path.join(self.output_dir, "qf_cache", file)
        cache_exist=os.path.isfile(cache_path)

        if 'ignore_folds' in self.quadFold.info:
            self.ignoreFolds = self.quadFold.info['ignore_folds']

        # self.updateParams()
        self.markFixedInfo(self.quadFold.info)
        self.statusPrint("Settings in onImageChange before update")
        self.statusPrint(self.calSettings)

        # Process new image
        self.processImage()

        self.statusPrint('---------------------------------------------------')

        if self.inputsettings and cache_exist and not self.delcache:
            self.statusPrint('cache exists, provided setting file was not used ')
        elif self.inputsettings and (not cache_exist or self.delcache):
            self.statusPrint('setting file provided and used for fitting')
        elif not self.inputsettings and cache_exist and not self.delcache:
            self.statusPrint('cache exist, no fitting was performed')
        elif not self.inputsettings and (self.delcache or not cache_exist):
            self.statusPrint('fitting with default settings')

        self.statusPrint('---------------------------------------------------')

    def markFixedInfo(self, currentInfo):
        """
        Deleting the center for appropriate recalculation
        """
        if 'center' in currentInfo:
            del currentInfo['center']

    def getExtentAndCenter(self):
        """
        Give the extent and center of the image
        """
        if self.quadFold is None:
            return [0,0], (0,0)
        if self.quadFold.orig_image_center is None:
            self.quadFold.findCenter()
            self.statusPrint("Done.")
        
        # Use quadFold.center (which can be manual or auto)
        center = self.quadFold.center if self.quadFold.center is not None else self.quadFold.orig_image_center

        extent = [self.quadFold.center[0] - center[0], self.quadFold.center[1] - center[1]]
        return extent, center

    def processImage(self):
        """
        Process Image by getting all flags and call process() of QuadrantFolder object
        Then, write data and save result tif.

        QuadrantFolder.process() returns True for the slow path and False
        for the fast-path (cached fingerprint matched, _folded.tif reused).
        On the fast-path we still re-emit the user-requested tif variant
        but skip background regeneration.
        """
        if self.ableToProcess():
            flags = self.getFlags()
            self.statusPrint("Flags in processImage:")
            self.statusPrint(flags)
            try:
                full_process = self.quadFold.process(flags)
            except Exception:
                self.statusPrint('Unexpected error')
                msg = 'Please report the problem with error message below and the input image\n\n'
                msg += "Error : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
                self.statusPrint(msg)
                raise

            self.updateParams()
            # acquire the lock
            if self.lock is not None:
                self.lock.acquire()
            self.csvManager = QF_CSVManager(self.output_dir)
            self.csvManager.writeNewData(self.quadFold)
            # release the lock
            if self.lock is not None:
                self.lock.release()

            # Save result to folder qf_results
            if 'resultImg' in self.quadFold.imgCache:
                result_path = fullPath(self.output_dir, 'qf_results')
                createFolder(result_path)

                result_file = str(join(result_path, self.imgList[self.currentFileNumber]))
                result_file, _ = splitext(result_file)
                img = self.quadFold.imgCache['resultImg']

                img = img.astype("float32")
                if 'compressed' in self.quadFold.info and not self.quadFold.info['compressed']:
                    result_file += '_folded.tif'
                    fabio.tifimage.tifimage(data=img).write(result_file)
                else:
                    result_file += '_folded_compressed.tif'
                    tif_img = Image.fromarray(img)
                    tif_img.save(result_file, compression='tiff_lzw')
                # bg.tif from a previous session is still on disk on the
                # fast-path, and BgSubFold / avg_fold weren't reconstructed
                # so saveBackground is skipped.
                if full_process:
                    self.saveBackground()

    def saveBackground(self):
        """
        Save the background image in bg folder
        """
        info = self.quadFold.info
        result = self.quadFold.imgCache["BgSubFold"]
        # avg_fold lives in imgCache, not info (matches QuadrantFoldingGUI.saveBackground).
        avg_fold = self.quadFold.imgCache.get("avg_fold", None)
        if avg_fold is None:
            # On the fast-path BgSubFold / avg_fold are not reconstructed; bg.tif
            # from a previous session is still on disk so nothing to save here.
            return
        background = avg_fold-result
        resultImg = makeFullImage(background)

        if 'rotate' in info and info['rotate']:
            resultImg = np.rot90(resultImg)

        filename = self.imgList[self.currentFileNumber]
        bg_path = fullPath(self.output_dir, os.path.join("qf_results", "bg"))
        result_path = fullPath(bg_path, filename + ".bg.tif")

        # create bg folder
        createFolder(bg_path)
        resultImg = resultImg.astype("float32")
        # imsave(result_path, resultImg)
        fabio.tifimage.tifimage(data=resultImg).write(result_path)

        total_inten = np.sum(resultImg)
        csv_path = join(bg_path, 'background_sum.csv')
        if self.csv_bg is None:
            # create csv file to save total intensity for background
            if exists(csv_path):
                self.csv_bg = pd.read_csv(csv_path)
            else:
                self.csv_bg = pd.DataFrame(columns=['Name', 'Sum'])
            self.csv_bg = self.csv_bg.set_index('Name')

        if filename in self.csv_bg.index:
            self.csv_bg = self.csv_bg.drop(index=filename)

        self.csv_bg.loc[filename] = pd.Series({'Sum':total_inten})
        self.csv_bg.to_csv(csv_path)

    def updateParams(self):
        """
        Update the parameters
        """
        info = self.quadFold.info
        if 'orientation_model' in info:
            self.orientationModel = info['orientation_model']
        self.getExtentAndCenter()

    def getFlags(self):
        """
        Get all flags for QuadrantFolder process() from widgets
        :return: flags (dict)
        """
        flags = build_default_flags()

        flags['orientation_model'] = self.orientationModel
        flags['ignore_folds'] = self.ignoreFolds
        # mask_thres removed from QuadrantFolder (uses INVALID_PIXEL_THRESHOLD
        # constant now, see QuadrantFolder.initParams docstring); blank_mask
        # is also no longer consumed by QF (ImageData applies blank/mask
        # before the image reaches QF). Both intentionally omitted.
        flags['rotate'] = False
        flags['fold_image'] = True
        flags['bg_options'] = 0 # default to "Manual Setting | One Method"

        if self.calSettings is not None:
            flags.update(self.calSettings)
        if 'center' in flags:
            flags.pop('center')

        # Defensive: strip caller-injected runtime state if it ever made
        # it into qfsettings.json (older GUI versions, or a hand-edited
        # file). headless is always a fresh, single-image run -- there
        # is no batch in flight, no manual BG batch assignment, and no
        # "force recompute" trigger. Letting any of these come through
        # would change BG selection logic mid-run.
        for key in ('batch_processing', 'force_recalc_bg',
                    'manual_background_assignments'):
            flags.pop(key, None)

        # Translate the user-preference name 'fixed_roi_*' (used by
        # qfsettings.json to express "persist this ROI across images")
        # into the processing-parameter name 'roi_w/h' that QuadrantFolder
        # actually consumes. We pop the original keys so QuadrantFolder
        # never sees the preference name -- per-image qf_cache should
        # only describe the actual ROI used, not the user's
        # cross-image preference.
        fixed_w = flags.pop('fixed_roi_w', None)
        fixed_h = flags.pop('fixed_roi_h', None)
        if fixed_w is not None and fixed_h is not None and fixed_w > 0 and fixed_h > 0:
            flags['roi_w'] = fixed_w
            flags['roi_h'] = fixed_h

        return flags

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

    def setCalibrationImage(self):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or force to open
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        settingspath=self.settingspath
        if self.inputsettings:
            try:
                with open(settingspath, 'r') as f:
                    self.calSettings = json.load(f)
            except Exception:
                self.statusPrint("Can't load setting file")
                self.inputsettings = False
                self.calSettings = None
            if self.calSettings is None:
                self.inputsettings = False
        # Center is managed by ImageData (center_settings.json → auto_geometry_cache → auto-calculate)
