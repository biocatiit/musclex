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

import logging
import json
from csv import writer
from matplotlib import scale as mscale
from matplotlib import transforms as mtransforms
from matplotlib.ticker import Formatter, AutoLocator
import pandas as pd
import numpy as np
from numpy import ma
from .pyqt_utils import *
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
from ..csv_manager import DI_CSVManager

class DSpacingScale(mscale.ScaleBase):
    """
    D Spacing scale class
    """
    name = 'dspacing'
    def __init__(self, axis, **kwargs):
        mscale.ScaleBase.__init__(self)
        self.lambda_sdd = kwargs.pop('lambda_sdd', 1501.45)

    def get_transform(self):
        """
        Give the D Spacing tranform object
        """
        return self.DSpacingTransform(self.lambda_sdd)

    def set_default_locators_and_formatters(self, axis):
        """
        Override to set up the locators and formatters to use with the
        scale.  This is only required if the scale requires custom
        locators and formatters.  Writing custom locators and
        formatters is rather outside the scope of this example, but
        there are many helpful examples in ``ticker.py``.

        In our case, the Mercator example uses a fixed locator from
        -90 to 90 degrees and a custom formatter class to put convert
        the radians to degrees and put a degree symbol after the
        value::
        """
        class DSpacingFormatter(Formatter):
            """
            D Spacing formatter
            """
            def __init__(self, lambda_sdd):
                Formatter.__init__(self)
                self.lambda_sdd = lambda_sdd
            def __call__(self, x, pos=None):
                if x == 0:
                    return "\u221E"
                else:
                    return "%.2f" % (self.lambda_sdd / x)

        axis.set_major_locator(AutoLocator())
        axis.set_major_formatter(DSpacingFormatter(self.lambda_sdd))
        axis.set_minor_formatter(DSpacingFormatter(self.lambda_sdd))

    def limit_range_for_scale(self, vmin, vmax, minpos):
        """
        Override to limit the bounds of the axis to the domain of the
        transform.  In the case of Mercator, the bounds should be
        limited to the threshold that was passed in.  Unlike the
        autoscaling provided by the tick locators, this range limiting
        will always be adhered to, whether the axis range is set
        manually, determined automatically or changed through panning
        and zooming.
        """
        return max(vmin, 1), vmax

    class DSpacingTransform(mtransforms.Transform):
        """
        There are two value members that must be defined.
        ``input_dims`` and ``output_dims`` specify number of input
        dimensions and output dimensions to the transformation.
        These are used by the transformation framework to do some
        error checking and prevent incompatible transformations from
        being connected together.  When defining transforms for a
        scale, which are, by definition, separable and have only one
        dimension, these members should always be set to 1.
        """
        input_dims = 1
        output_dims = 1
        is_separable = True
        has_inverse = True
        def __init__(self, lambda_sdd):
            mtransforms.Transform.__init__(self)
            self.lambda_sdd = lambda_sdd

        def transform_non_affine(self, a):
            """
            This transform takes an Nx1 ``numpy`` array and returns a
            transformed copy.  Since the range of the Mercator scale
            is limited by the user-specified threshold, the input
            array must be masked to contain only valid values.
            ``matplotlib`` will handle masked arrays and remove the
            out-of-range data from the plot.  Importantly, the
            ``transform`` method *must* return an array that is the
            same shape as the input array, since these values need to
            remain synchronized with values in the other dimension.
            """
            masked = ma.masked_where(a <= 0, a)
            if masked.mask.any():
                return self.lambda_sdd / masked
            else:
                return self.lambda_sdd / a

        def inverted(self):
            """
            Override this method so matplotlib knows how to get the
            inverse transform for this transform.
            """
            return DSpacingScale.InvertedDSpacingTransform(
                self.lambda_sdd)

    class InvertedDSpacingTransform(mtransforms.Transform):
        """
        Inverted of the previous class
        """
        input_dims = 1
        output_dims = 1
        is_separable = True
        has_inverse = True
        def __init__(self, lambda_sdd):
            mtransforms.Transform.__init__(self)
            self.lambda_sdd = lambda_sdd

        def transform_non_affine(self, a):
            """
            See previous class
            """
            masked = ma.masked_where(a <= 0, a)
            if masked.mask.any():
                return np.flipud(self.lambda_sdd / masked)
            else:
                return np.flipud(self.lambda_sdd / a)

        def inverted(self):
            """
            See previous class
            """
            return DSpacingScale.DSpacingTransform(self.lambda_sdd)

mscale.register_scale(DSpacingScale)

class DIImageWindowh():
    """
    A class to process Scanning diffraction on a file
    """
    def __init__(self, image_name = "", dir_path = "", inputflags=False, delcache=False, inputflagpath='musclex/settings/disettings.json', process_folder=False,imgList = None):
        self.fileName = image_name
        self.filePath = dir_path
        self.inputflag=inputflags
        self.delcache=delcache
        self.inputflagfile=inputflagpath

        self.csvManager = DI_CSVManager(dir_path)
        self.imgList = []
        self.numberOfFiles = 0
        self.currentFileNumber = 0

        self.cirProj = None
        self.calSettings = None
        self.mask = None
        self.function = None
        self.checkable_buttons = []
        self.fixed_hull_range = None
        self.ROI = None
        self.merged_peaks = None
        self.orientationModel = None
        self.in_batch_process = False
        self.pixelDataFile = None

        self.stop_process = False
        self.intensityRange = []
        self.updatingUI = False
        self.ring_colors = []

        self.m1_selected_range = 0
        self.update_plot = {'m1_partial_hist': True,
                            'm1_hist': True,
                            'm2_diff': True,
                            'image_result': True,
                            'results_text': True
                            }
        self.logger = None
        self.onNewFileSelected(imgList)
        if process_folder and len(self.imgList) > 0:
            self.processFolder()
        elif len(self.imgList) > 0:
            self.onImageChanged()

    def generateRingColors(self):
        """
        Generate colors for the rings
        """
        possible_vals = [0, 255]
        self.ring_colors = []
        for b in possible_vals:
            for g in possible_vals:
                for r in possible_vals:
                    if b==0 and g==0 and r==0:
                        continue
                    self.ring_colors.append([b,g,r])

    def processFolder(self):
        """
        Process current folder
        """
        ## Popup confirm dialog with settings
        nImg = len(self.imgList)
        print('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'
        flags = self.getFlags()
        text += "\nCurrent Settings"
        text += "\n - Partial integration angle range : "+ str(flags['partial_angle'])
        if 'orientation_model' in flags:
            text += "\n - Orientation Model : "+ flags['orientation_model']
        if 'ROI' in flags:
            text += "\n - ROI : "+ str(flags['ROI'])
        if 'fixed_hull' in flags:
            text += "\n - R-min & R-max : "+ str(flags['fixed_hull'])
        text += '\n\nAre you sure you want to process ' + str(nImg) + ' image(s) in this Folder? \nThis might take a long time.'

        log_path = fullPath(self.filePath, 'log')
        if not exists(log_path):
            os.makedirs(log_path)

        current = time.localtime()
        filename = "CirProj_""%02d" % current.tm_year + "%02d" % current.tm_mon + "%02d" % current.tm_mday + \
                    "_" + "%02d" % current.tm_hour + "%02d" % current.tm_min + "%02d" % current.tm_sec + ".log"
        filename = fullPath(log_path, filename)
        self.logger = logging.getLogger('di')
        self.logger.setLevel(logging.DEBUG)
        self.logger.propagate = False

        # create a file handler
        handler = logging.FileHandler(filename)
        handler.setLevel(logging.DEBUG)

        # create a logging format
        formatter = logging.Formatter('%(asctime)s: %(message)s')
        handler.setFormatter(formatter)

        # add the handlers to the self.logger
        self.logger.addHandler(handler)
        self.logger.addFilter(logging.Filter(name='di'))

        ## Process all images and update progress bar
        self.in_batch_process = True
        self.stop_process = False
        for _ in range(nImg):
            if self.stop_process:
                break
            self.nextImage()

        self.in_batch_process = False
        self.folder_processed = True

    def getFlags(self, imgChanged=True):
        """
        Give the flags for the object associated
        """
        if self.inputflag:
            try:
                with open(self.inputflagfile) as f:
                    flags=json.load(f)
            except Exception:
                print("Can't load setting file")
                self.inputflag=False
                flags={"partial_angle": 90, "orientation_model": "GMM3", "90rotation": False}
        else:
            flags={"partial_angle": 90, "orientation_model": "GMM3", "90rotation": False}

        return flags

    def onNewFileSelected(self, imgList):
        """
        Used when a new file is selected
        """
        if imgList is not None:
            self.imgList = imgList
        else:
            self.imgList, _ = getFilesAndHdf(self.filePath)

        self.imgList.sort()
        self.numberOfFiles = len(self.imgList)
        if len(self.fileName) > 0:
            self.currentFileNumber = self.imgList.index(self.fileName)
        else:
            self.currentFileNumber = 0

    def onImageChanged(self):
        """
        When the image is changed, process the scanning diffraction again
        """
        file=self.fileName+'.info'
        cache_path = os.path.join(self.filePath, "di_cache",file)
        cache_exist=os.path.isfile(cache_path)
        if self.delcache:
            if os.path.isfile(cache_path):
                print('cache is deleted')
                os.remove(cache_path)
        fileName = self.imgList[self.currentFileNumber]
        print("current file is "+fileName)
        self.cirProj = ScanningDiffraction(self.filePath, fileName, logger=self.logger)
        self.processImage(True)
        print('---------------------------------------------------')

        if self.inputflag and cache_exist and not self.delcache:
            print('cache exists, provided setting file was not used ')
        elif self.inputflag and (not cache_exist or self.delcache):
            print('setting file provided and used for fitting')
        elif not self.inputflag and cache_exist and not self.delcache:
            print('cache exist, no fitting was performed')
        elif not self.inputflag and (self.delcache or not cache_exist):
            print('fitting with default settings')
            print('default settings are "partial_angle": 90, "orientation_model": "GMM3", "90rotation": False')

        print('---------------------------------------------------')

    def processImage(self, imgChanged=False):
        """
        Process the scanning diffraction
        """
        if self.cirProj is not None:
            flags = self.getFlags(imgChanged)
            self.cirProj.process(flags)
            self.updateParams()
            self.csvManager.write_new_data(self.cirProj)

    def create_circular_mask(self, h, w, center, radius):
        """
        Create a circular mask
        """
        Y, X = np.ogrid[:h, :w]
        dist_from_center = np.sqrt((X - center[0]) ** 2 + (Y - center[1]) ** 2)

        mask = dist_from_center > radius
        return mask

    def addPixelDataToCsv(self, grid_lines):
        """
        Add pixel data to csv
        """
        if self.pixelDataFile is None:
            self.pixelDataFile = self.filePath + '/di_results/BackgroundSummary.csv'
            if not os.path.isfile(self.pixelDataFile):
                header = ['File Name', 'Average Pixel Value (Outside rmin or mask)', 'Number of Pixels (Outside rmin or mask)']
                f = open(self.pixelDataFile, 'a')
                csv_writer = writer(f)
                csv_writer.writerow(header)
                f.close()

        csvDF = pd.read_csv(self.pixelDataFile)
        recordedFileNames = set(csvDF['File Name'].values)

        # Compute the average pixel value and number of pixels outside rmin/mask
        _, mask = getBlankImageAndMask(self.filePath)
        img = copy.copy(self.cirProj.original_image)
        if mask is not None:
            numberOfPixels = np.count_nonzero(mask == 0)
            averagePixelValue = np.average(img[mask == 0])
        else:
            h,w = img.shape
            rmin = self.cirProj.info['start_point']
            cir_mask = self.create_circular_mask(h,w,center=self.cirProj.info['center'], radius=rmin)
            # Exclude grid lines in computation
            print("Gird Lines Coordinates ", grid_lines)
            cir_mask[grid_lines] = 0
            numberOfPixels = np.count_nonzero(cir_mask)
            averagePixelValue = np.average(img[cir_mask])

        if self.cirProj.filename in recordedFileNames:
            csvDF.loc[csvDF['File Name'] == self.cirProj.filename, 'Average Pixel Value'] = averagePixelValue
            csvDF.loc[csvDF['File Name'] == self.cirProj.filename, 'Number of Pixels'] = numberOfPixels
        else:
            next_row_index = csvDF.shape[0]
            csvDF.loc[next_row_index] = [self.cirProj.filename, averagePixelValue, numberOfPixels]
        csvDF.to_csv(self.pixelDataFile, index=False)

    def setMinMaxIntensity(self, img, minInt, maxInt, minIntLabel, maxIntLabel):
        """
        Set the min and max intensity
        """
        min_val = img.min()
        max_val = img.max()
        self.intensityRange = [min_val, max_val-1, min_val+1, max_val]
        minInt.setMinimum(self.intensityRange[0])
        minInt.setMaximum(self.intensityRange[1])
        maxInt.setMinimum(self.intensityRange[2])
        maxInt.setMaximum(self.intensityRange[3])
        step = max(1., (max_val-min_val)/100)
        minInt.setSingleStep(step)
        maxInt.setSingleStep(step)
        minIntLabel.setText("Min intensity (" + str(min_val) + ")")
        maxIntLabel.setText("Max intensity (" + str(max_val) + ")")

        if img.dtype == 'float32':
            decimal = 2
        else:
            decimal = 0

        maxInt.setDecimals(decimal)
        minInt.setDecimals(decimal)

        if maxInt.value() == 1. and minInt.value() == 0.:
            self.updatingUI = True
            minInt.setValue(min_val)
            maxInt.setValue(max_val*0.1)
            self.updatingUI = False

    def updateParams(self):
        """
        Update the parameters
        """
        info = self.cirProj.info
        if 'fixed_hull' in info:
            self.fixed_hull_range = info['fixed_hull']
        if 'merged_peaks' in info:
            self.merged_peaks = info['merged_peaks']
        if self.ROI is None and info['ROI'] != [info['start_point'], info['rmax']]:
            self.ROI = info['ROI']

    def nextImage(self):
        """
        When next image is clicked
        """
        self.currentFileNumber = (self.currentFileNumber + 1) % self.numberOfFiles
        self.onImageChanged()
