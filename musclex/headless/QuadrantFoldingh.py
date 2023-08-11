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
    from ..utils.image_processor import *
    from ..modules.QuadrantFolder import QuadrantFolder
    from ..csv_manager.QF_CSVManager import QF_CSVManager
except: # for coverage
    from utils.file_manager import *
    from utils.image_processor import *
    from modules.QuadrantFolder import QuadrantFolder
    from csv_manager.QF_CSVManager import QF_CSVManager

class QuadrantFoldingh:
    """
    Window displaying all information of a selected image.
    This window contains 2 tabs : image, and result
    """
    def __init__(self, filename, inputsettings, delcache, settingspath=os.path.join('musclex', 'settings', 'qfsettings.json'), lock=None, dir_path=None, imgList=None, currentFileNumber=None, fileList=None, ext=None):
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
        fileName = self.imgList[self.currentFileNumber]
        file=fileName+'.info'
        cache_path = os.path.join(self.dir_path, "qf_cache", file)
        cache_exist=os.path.isfile(cache_path)
        if self.delcache:
            if cache_exist:
                os.remove(cache_path)
        self.quadFold = QuadrantFolder(self.dir_path, fileName, self, self.fileList, self.ext)

        if self.inputsettings:
            self.setCalibrationImage()
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
        cache_path = os.path.join(self.dir_path, "qf_cache", file)
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

        if not self.inputsettings and 'calib_center' in currentInfo:
            del currentInfo['calib_center']

    def getExtentAndCenter(self):
        """
        Give the extent and center of the image
        """
        if self.quadFold is None:
            return [0,0], (0,0)
        if self.quadFold.orig_image_center is None:
            self.quadFold.findCenter()
            self.statusPrint("Done.")
        if 'calib_center' in self.quadFold.info:
            center = self.quadFold.info['calib_center']
        elif 'manual_center' in self.quadFold.info:
            center = self.quadFold.info['manual_center']
        else:
            center = self.quadFold.orig_image_center

        extent = [self.quadFold.info['center'][0] - center[0], self.quadFold.info['center'][1] - center[1]]
        return extent, center

    def processImage(self):
        """
        Process Image by getting all flags and call process() of QuadrantFolder object
        Then, write data and update UI
        """
        if self.ableToProcess():
            flags = self.getFlags()
            self.statusPrint("Flags in processImage:")
            self.statusPrint(flags)
            try:
                self.quadFold.process(flags)
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
            self.csvManager = QF_CSVManager(self.dir_path)
            self.csvManager.writeNewData(self.quadFold)
            # release the lock
            if self.lock is not None:
                self.lock.release()

            # Save result to folder qf_results
            if 'resultImg' in self.quadFold.imgCache:
                result_path = fullPath(self.dir_path, 'qf_results')
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
                # metadata = json.dumps([True, self.quadFold.initImg.shape])
                # imsave(result_file, img, description=metadata)
                self.saveBackground()

    def saveBackground(self):
        """
        Save the background image in bg folder
        """
        info = self.quadFold.info
        result = self.quadFold.imgCache["BgSubFold"]
        avg_fold = info["avg_fold"]
        background = avg_fold-result
        resultImg = self.quadFold.makeFullImage(background)

        if 'rotate' in info and info['rotate']:
            resultImg = np.rot90(resultImg)

        filename = self.imgList[self.currentFileNumber]
        bg_path = fullPath(self.dir_path, os.path.join("qf_results", "bg"))
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
        if self.calSettings is not None and 'center' in self.calSettings and 'calib_center' in info:
            # Update cal settings center with the corresponding coordinate in original (or initial) image
            # so that it persists correctly on moving to next image
            self.calSettings['center'] = info['calib_center']
        self.getExtentAndCenter()

    def getFlags(self):
        """
        Get all flags for QuadrantFolder process() from widgets
        :return: flags (dict)
        """
        flags = {}

        flags['orientation_model'] = self.orientationModel
        flags['ignore_folds'] = self.ignoreFolds
        # default values, same as QuadrantFoldingGUI.py default
        flags['bgsub'] = 'None'
        flags["cirmin"] = 0.0
        flags["cirmax"] = 25.0
        flags['win_size_x'] = 10
        flags['win_size_y'] = 10
        flags['win_sep_x'] = 10
        flags['win_sep_y'] = 10
        flags["bin_theta"] = 30
        flags['radial_bin'] = 10
        flags['smooth'] = 0.1
        flags['tension'] = 1.0
        flags["tophat1"] = 5
        flags['tophat2'] = 20
        flags['mask_thres'] = getMaskThreshold(self.quadFold.orig_img)
        flags['sigmoid'] = 0.1
        flags['fwhm'] = 10
        flags['boxcar_x'] = 10
        flags['boxcar_y'] = 10
        flags['cycles'] = 5
        flags['blank_mask'] = False
        flags['rotate'] = False

        if self.calSettings is not None:
            flags.update(self.calSettings)
        if 'center' in flags:
            flags.pop('center')
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
            if self.calSettings is not None and 'center' in self.calSettings:
                self.quadFold.info['calib_center'] = self.calSettings['center']
            else:
                self.inputsettings = False
            if 'manual_center' in self.quadFold.info:
                del self.quadFold.info['manual_center']
            if 'center' in self.quadFold.info:
                del self.quadFold.info['center']
        else:
            if self.quadFold is not None and 'calib_center' in self.quadFold.info:
                del self.quadFold.info['calib_center']
            if self.quadFold is not None and 'center' in self.quadFold.info:
                del self.quadFold.info['center']
