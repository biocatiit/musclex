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
import copy
import json
import os, shutil
from .pyqt_utils import *
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
from os.path import split
import traceback
import webbrowser
from ..utils.file_manager import fullPath, getImgFiles, getStyleSheet, getBlankImageAndMask
from ..modules.EquatorImage import EquatorImage, getCardiacGraph
from ..modules.QuadrantFolder import QuadrantFolder
from ..ui.QuadrantFoldingGUI import QuadrantFoldingGUI
from ..utils.image_processor import *
from ..csv_manager import EQ_CVSManager, EQ_CSVManager2
from ..ui.EQ_FittingTab import EQ_FittingTab
import musclex
from .BlankImageSettings import BlankImageSettings
from ..utils import logger

class EquatorWindowh:
    """
    Window displaying all information of a selected image.
    This window contains 3 tabs : image, fitting, results
    """
    def __init__(self, mainWin, filename, inputsettings, delcache, settingspath='musclex/settings/eqsettings.json'):
        """
        :param filename: selected file name
        :param inputsettings: flag for input setting file
        :param delcache: flag for deleting cache
        :param settingspath: setting file directory
        """
        
        self.version = musclex.__version__
        self.editableVars = {}
        self.bioImg = None  # Current EquatorImage object
        self.default_img_zoom = None  # default zoom calculated after processing image
        #self.img_zoom = None  # Params for x and y ranges of displayed image in image tab
        self.graph_zoom = None # Params for x and y ranges of displayed graph in fitting tab
        self.function = None  # Current active function
        
        self.in_batch_process = False
        self.fixedIntArea = None
        self.orientationModel = None
        self.modeOrientation = None
        self.dir_path, self.imgList, self.currentImg = getImgFiles(str(filename))
        if len(self.imgList) == 0:
            self.inputerror()
            return
        self.csvManager = EQ_CVSManager(self.dir_path)  # Create a CSV Manager object
        self.csvManager2 = EQ_CSVManager2(self.dir_path)
        self.inputsettings=inputsettings
        self.delcache=delcache
        self.settingspath=settingspath
        
        
        self.onImageChanged() # Toggle window to process current image
        
  

    def inputerror(self):
        # Display input error to screen
        print('Invalid Input')
        print("Please select non empty failedcases.txt or an image\n\n")




    def onImageChanged(self):
        """
        This will create a new EquatorImage object for the new image 
        Process the new image if there's no cache.
        """
        
        fileName = self.imgList[self.currentImg]
        file=fileName+'.info'
        cache_path = os.path.join(self.dir_path, "eq_cache",file)
        cache_exist=os.path.isfile(cache_path)
        if self.delcache:
            if os.path.isfile(cache_path):
                os.remove(cache_path)
                
            
        #prevInfo = self.bioImg.info if self.bioImg is not None else None
        self.bioImg = EquatorImage(self.dir_path, fileName, self)
        
        self.bioImg.skeletalVarsNotSet = not ('isSkeletal' in self.bioImg.info and self.bioImg.info['isSkeletal'])
   
        settings = None
        settings = self.getSettings()
        print("Settings in onImageChange before update")
        print(settings)
        
        

        # Process new image
        if 'paramInfo' in settings:
            paramInfo=settings['paramInfo']
            #settings.pop('paramInfo')
            self.processImage(paramInfo)
        else:

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



    
    def processImage(self, paramInfo=None):
        """
        Process Image by getting all settings and call process() of EquatorImage object
        Then, write data 
        """
        if self.bioImg is None:
            return
        
        settings = self.getSettings()
        print("Settings in processImage:")
        print(settings)
        try:
                
            self.bioImg.process(settings, paramInfo)
        except Exception as e:
            
            
            print('Unexpected error')
            msg = 'Please report the problem with error message below and the input image\n\n'
            msg += "Error : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
            print(msg)
        
            raise
        

        self.updateParams()
        self.csvManager.writeNewData(self.bioImg)
        self.csvManager2.writeNewData(self.bioImg)
        
        


    def updateParams(self):
        info = self.bioImg.info
        if 'orientation_model' in info:
            self.orientationModel = info['orientation_model']
     
        if self.bioImg.quadrant_folded:
            cx, cy = self.bioImg.info['center']
            xlim, ylim = self.bioImg.initialImgDim
            xlim, ylim = int(xlim/2), int(ylim/2)
            self.default_img_zoom = [(cx-xlim, cx+xlim), (cy-ylim, cy+ylim)]



    def getSettings(self):
        """
        Get all settings for EquatorImage process() from widgets
        :return: settings (dict)
        """
        settings = {}
        settingspath=self.settingspath
      
        if self.inputsettings==True:    
                
            try:
                with open(settingspath) as f:
                    settings=json.load(f)
            except:
                print("Can't load setting file")
                self.inputsettings=False
                settings={"left_fix_sigmac": 1.0, "right_fix_sigmac": 1.0, \
                    "orientation_model": 0, "model": "Gaussian", "isSkeletal": False, \
                    "mask_thres": 0.0,  "90rotation": False,\
                        "blank_mask": False}
        else:
            settings={"left_fix_sigmac": 1.0, "right_fix_sigmac": 1.0, \
                    "orientation_model": 0, "model": "Gaussian", "isSkeletal": False, \
                    "mask_thres": 0.0,  "90rotation": False,\
                        "blank_mask": False}

        for k in settings.keys():
            if self.isDynamicParameter(k):
                settings.pop(k)
      

        return settings





    def isDynamicParameter(self, paramName):
        '''
        Checks whether parameter is dynamically handelled by fitting mechanism
        :param paramName: Name of the parameter to be checked
        :return: bool True if it is in the dynamic parameter list
        '''

        dynamicParams = ['Speak', 'left_area', 'right_area']
        for p in dynamicParams:
            if p in paramName:
                return True
        return False


  
        

    def statusPrint(self, text):
        print(text)

   