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
from tifffile import imsave
import json
import matplotlib.patches as patches
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.QuadrantFolder import QuadrantFolder
from .pyqt_utils import *
from os.path import split, splitext
import musclex
import traceback
import pandas as pd
from ..CalibrationSettings import CalibrationSettings

class QuadrantFoldingh(QMainWindow):
    """
    Window displaying all information of a selected image.
    This window contains 2 tabs : image, and result
    """

    def __init__(self, filename, inputsettings, delcache, settingspath='musclex/settings/qfsettings.json'):
        """
        :param filename: selected file name
        :param inputsettings: flag for input setting file
        :param delcache: flag for deleting cache
        :param settingspath: setting file directory
        """
        self.version = musclex.__version__
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
        
        self.dir_path, self.imgList, self.currentFileNumber = getImgFiles(str(filename))
        if len(self.imgList) == 0:
            self.inputerror()
            return
        self.inputsettings=inputsettings
        self.delcache=delcache
        self.settingspath=settingspath

        self.onImageChanged()

    def inputerror(self):
        # Display input error to screen
        print('Invalid Input')
        print("Please select non empty failedcases.txt or an image\n\n")

    def ableToProcess(self):
        # Check if image can be processed
        return (self.quadFold is not None)

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
        cache_path = os.path.join(self.dir_path, "qf_cache",file)
        cache_exist=os.path.isfile(cache_path)
        if self.delcache:
            if os.path.isfile(cache_path):
                os.remove(cache_path)

        self.quadFold = QuadrantFolder(self.dir_path, fileName, self)

        flags = None
        flags = self.getFlags()
        print("Settings in onImageChange before update")
        print(flags)

        # Process new image
        self.processImage()

        print('---------------------------------------------------')

        if self.inputsettings and cache_exist and not self.delcache:
            print('cache exists, provided setting file was not used ')
        elif self.inputsettings and (not cache_exist or self.delcache):
            print('setting file provided and used for fitting')
        elif not self.inputsettings and cache_exist and not self.delcache:
            print('cache exist, no fitting was performed')
        elif not self.inputsettings and (self.delcache or not cache_exist):
            print('fitting with default settings')
        
        print('---------------------------------------------------')

    def getExtentAndCenter(self):
        if self.quadFold is None:
            return [0,0], (0,0)
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
            print("Flags in processImage:")
            print(flags)

            try:
                self.quadFold.process(flags)
            except Exception as e:
                print('Unexpected error')
                msg = 'Please report the problem with error message below and the input image\n\n'
                msg += "Error : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
                print(msg)
                raise

            self.updateParams()

            # Save result to folder qf_results
            if 'resultImg' in self.quadFold.imgCache.keys():
                result_path = fullPath(self.dir_path, 'qf_results')
                createFolder(result_path)

                result_file = str(join(result_path, self.imgList[self.currentFileNumber]))
                result_file, _ = splitext(result_file)
                img = self.quadFold.imgCache['resultImg']

                # if self.quadFold.info['imgType'] == 'float32':
                #     img = get16bitImage(img)
                # else:
                #     img = img.astype(self.quadFold.info['imgType'])
                img = img.astype("float32")

                metadata = json.dumps([True, self.quadFold.initImg.shape])
                result_file += '_folded.tif'
                imsave(result_file, img, description=metadata)

                self.saveBackground()

    def saveBackground(self):
        info = self.quadFold.info
        result = self.quadFold.imgCache["BgSubFold"]
        avg_fold = info["avg_fold"]
        background = avg_fold-result
        resultImg = self.quadFold.makeFullImage(background)

        if 'rotate' in info and info['rotate']:
            resultImg = np.rot90(resultImg)

        filename = self.imgList[self.currentFileNumber]
        bg_path = fullPath(self.dir_path, "qf_results/bg")
        result_path = fullPath(bg_path, filename + ".bg.tif")

        # create bg folder
        createFolder(bg_path)
        resultImg = resultImg.astype("float32")
        imsave(result_path, resultImg)

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
        info = self.quadFold.info
        if 'orientation_model' in info:
            self.orientationModel = info['orientation_model']
        if self.calSettings is not None and 'center' in self.calSettings and 'calib_center' in info:
            # Update cal settings center with the corresponding coordinate in original (or initial) image
            # so that it persists correctly on moving to next image
            self.calSettings['center'] = info['calib_center']
        extent, center = self.getExtentAndCenter()
        cx, cy = center
        cxr, cyr = self.quadFold.info['center']
        xlim, ylim = self.quadFold.initImg.shape
        xlim, ylim = int(xlim/2), int(ylim/2)
        self.default_img_zoom = [(cx-xlim, cx+xlim), (cy-ylim, cy+ylim)]
        self.default_result_img_zoom = [(cxr-xlim, cxr+xlim), (cyr-ylim, cyr+ylim)]

    def getFlags(self):
        """
        Get all flags for QuadrantFolder process() from widgets
        :return: flags (dict)
        """
        flags = {}

        flags['orientation_model'] = self.orientationModel
        flags["ignore_folds"] = self.ignoreFolds
        flags['bgsub'] = 'None' # No background substraction in headless mode
        settingspath=self.settingspath

        if self.inputsettings==True:    
                
            try:
                with open(settingspath) as f:
                    flags.update(json.load(f))
            except:
                print("Can't load setting file")
                self.inputsettings=False
                flags.update({"center": [1030.22, 1015.58], "radius": 686, "silverB": 5.83803, "type": "img"})
        # else:
        #     flags.update({"center": [1030.22, 1015.58], "radius": 686, "silverB": 5.83803, "type": "img"})

        if 'center' in flags:
            flags.pop('center')

        return flags

    def processFolder(self):
        fileList = os.listdir(self.dir_path)
        self.imgList = []
        for f in fileList:
            if isImg(fullPath(self.dir_path, f)):
                self.imgList.append(f)

        self.imgList.sort()
        self.numberOfFiles = len(self.imgList)

        flags = self.getFlags()
        print("\nCurrent Settings")
        if 'center' in flags.keys():
            print("\n  - Center : " + str(flags["center"]))
        if len(self.ignoreFolds) > 0:
            print("\n  - Ignore Folds : " + str(list(self.ignoreFolds)))
        print("\n  - Mask Threshold : " + str(flags["mask_thres"]))

        if flags['bgsub'] != 'None':
            if 'fixed_rmin' in flags.keys():
                print("\n  - R-min : " + str(flags["fixed_rmin"]))
                print("\n  - R-max : " + str(flags["fixed_rmax"]))

            if flags['bgsub'] in ['Circularly-symmetric', 'Roving Window']:
                print("\n  - Pixel Range (Percentage) : " + str(flags["cirmin"]) + "% - "+str(flags["cirmax"])+"%")

            if flags['bgsub'] == 'Circularly-symmetric':
                print("\n  - Radial Bin : " + str(flags["radial_bin"]))
                print("\n  - Smooth : " + str(flags["smooth"]))
            elif flags['bgsub'] == 'White-top-hats':
                print("\n  - Tophat (inside R-max) : " + str(flags["tophat1"]))
            elif flags['bgsub'] == 'Smoothed-Gaussian':
                print("\n  - FWHM : " + str(flags["fwhm"]))
                print("\n  - Number of cycle : " + str(flags["cycles"]))
            elif flags['bgsub'] == 'Smoothed-BoxCar':
                print("\n  - Box car width : " + str(flags["boxcar_x"]))
                print("\n  - Box car height : " + str(flags["boxcar_y"]))
                print("\n  - Number of cycle : " + str(flags["cycles"]))

            print("\n  - Tophat (outside R-max) : " + str(flags["tophat2"]))
            print("\n  - Merge Gradient : " + str(flags["sigmoid"]))

        print('\n\nAre you sure you want to process ' + str(self.numberOfFiles) + ' image(s) in this Folder? \nThis might take a long time.')

    def fileNameChanged(self):
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            fileName = self.filenameLineEdit.text().strip()
        elif selected_tab == 1:
            fileName = self.filenameLineEdit2.text().strip()
        if fileName not in self.imgList:
            return
        self.currentFileNumber = self.imgList.index(fileName)
        self.onImageChanged()

    def statusPrint(self, text):
        print(text)
