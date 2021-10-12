from .pyqt_utils import *
import matplotlib.patches as patches
from numpy import ma
from matplotlib import scale as mscale
from matplotlib import transforms as mtransforms
from matplotlib.colors import LogNorm, Normalize
import logging
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
from ..CalibrationSettings import CalibrationSettings
from ..csv_manager import CP_CSVManager
import musclex
from .BlankImageSettings import BlankImageSettings
from csv import writer
import pandas as pd
import numpy as np
import json

class DSpacingScale(mscale.ScaleBase):
    name = 'dspacing'

    def __init__(self, axis, **kwargs):
        mscale.ScaleBase.__init__(self)
        self.lambda_sdd = kwargs.pop('lambda_sdd', 1501.45)

    def get_transform(self):
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
        from matplotlib.ticker import Formatter, AutoLocator
        class DSpacingFormatter(Formatter):
            def __init__(self, lambda_sdd):
                Formatter.__init__(self)
                self.lambda_sdd = lambda_sdd
            def __call__(self, x, pos=None):
                if x == 0:
                    return u"\u221E"
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
        # There are two value members that must be defined.
        # ``input_dims`` and ``output_dims`` specify number of input
        # dimensions and output dimensions to the transformation.
        # These are used by the transformation framework to do some
        # error checking and prevent incompatible transformations from
        # being connected together.  When defining transforms for a
        # scale, which are, by definition, separable and have only one
        # dimension, these members should always be set to 1.
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
        input_dims = 1
        output_dims = 1
        is_separable = True
        has_inverse = True

        def __init__(self, lambda_sdd):
            mtransforms.Transform.__init__(self)
            self.lambda_sdd = lambda_sdd

        def transform_non_affine(self, a):
            masked = ma.masked_where(a <= 0, a)
            if masked.mask.any():
                return np.flipud(self.lambda_sdd / masked)
            else:
                return np.flipud(self.lambda_sdd / a)

        def inverted(self):
            return DSpacingScale.DSpacingTransform(self.lambda_sdd)

mscale.register_scale(DSpacingScale)

class CPImageWindowh():
    def __init__(self, image_name = "", dir_path = "", inputflags=False, delcache=False, inputflagpath='musclex/settings/disettings.json', process_folder=False,imgList = None):
        
        # import pdb
        # pdb.set_trace()
        self.fileName = image_name
        self.filePath = dir_path
        self.inputflag=inputflags
        self.delcache=delcache
        self.inputflagfile=inputflagpath

        
        self.csvManager = CP_CSVManager(dir_path)
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

        self.m1_selected_range = 0
        self.update_plot = {'m1_partial_hist': True,
                            'm1_hist': True,
                            'm2_diff': True,
                            'image_result': True,
                            'results_text': True
                            }
        #self.intesityRange = [0, 1, 1, 2]
        #self.mainWin = None
        self.logger = None

        # self.generateRingColors()
        # self.setConnections()
        # self.setCalibrationImage()
        self.onNewFileSelected(imgList)
        if process_folder and len(self.imgList) > 0:
            self.processFolder()
        elif len(self.imgList) > 0:
            self.onImageChanged()

    def generateRingColors(self):
        possible_vals = [0, 255]
        self.ring_colors = []
        for b in possible_vals:
            for g in possible_vals:
                for r in possible_vals:
                    if b==0 and g==0 and r==0:
                        continue
                    self.ring_colors.append([b,g,r])

    
    def batchProcBtnToggled(self):
        if self.processFolderButton.isChecked():
            if not self.in_batch_process:
                self.processFolderButton.setText("Stop")
                self.processFolder()
        else:
            self.stop_process = True

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
        if self.calSettings is not None:
            if "center" in self.calSettings:
                text += "\n  - Calibration Center : " + str(self.calSettings["center"])
            if self.calSettings["type"] == "img":
                text += "\n  - Silver Behenate : " + str(self.calSettings["silverB"]) +" nm"
                text += "\n  - Sdd : " + str(self.calSettings["radius"]) + " pixels"
            else:
                text += "\n  - Lambda : " + str(self.calSettings["lambda"]) +" nm"
                text += "\n  - Sdd : " + str(self.calSettings["sdd"]) + " mm"
                text += "\n  - Pixel Size : " + str(self.calSettings["pixel_size"]) + " nm"
        text += '\n\nAre you sure you want to process ' + str(nImg) + ' image(s) in this Folder? \nThis might take a long time.'
       


        # If "yes" is pressed
        
        if True:

        
            log_path = fullPath(self.filePath, 'log')
            if not exists(log_path):
                os.makedirs(log_path)

            current = time.localtime()
            filename = "CirProj_""%02d" % current.tm_year + "%02d" % current.tm_mon + "%02d" % current.tm_mday + \
                       "_" + "%02d" % current.tm_hour + "%02d" % current.tm_min + "%02d" % current.tm_sec + ".log"
            filename = fullPath(log_path, filename)
            self.logger = logging.getLogger('cp')
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
            self.logger.addFilter(logging.Filter(name='cp'))

            ## Process all images and update progress bar
            self.in_batch_process = True
            self.stop_process = False
            for i in range(nImg):
                if self.stop_process:
                    break
                self.nextImage()
                
            self.in_batch_process = False
            self.folder_processed = True
        else:
            self.folder_processed = False

        

    def keyPressEvent(self, event):
        key = event.key()

        if key == Qt.Key_Right:
            self.nextImage()
        elif key == Qt.Key_Left:
            self.prevImage()
        elif key == Qt.Key_Escape:
            self.refreshAllTabs()

    def closeEvent(self, ev):
        if self.mainWin is not None:
            self.mainWin.removeWidget(self)

    def getFlags(self, imgChanged=True):
        

        
        if self.inputflag==True:    
                
            try:
                with open(self.inputflagfile) as f:
                    flags=json.load(f)
            except:
                print("Can't load setting file")
                self.inputflag=False
                flags={"partial_angle": 90, "orientation_model": "GMM3", "90rotation": False}
        else:
            flags={"partial_angle": 90, "orientation_model": "GMM3", "90rotation": False}

        # flags['partial_angle'] = self.partialRange.value()
        # if self.merged_peaks is not None and self.persistRingsChkBx.isChecked():
        #     print("Persisting rings at {}..".format(self.merged_peaks))
        #     flags['merged_peaks'] = self.merged_peaks
        #     flags['m1_rings'] = self.merged_peaks
        #     flags['m2_rings'] = self.merged_peaks
        #     flags['model_peaks'] = self.merged_peaks
        #     flags['persist_rings'] = True
        # if self.ROI is not None and (self.persistROIChkBx.isChecked() or not imgChanged):
        #     flags['ROI'] = self.ROI
        # if self.orientationModel is not None:
        #     flags['orientation_model'] = self.orientationModel
        # flags['90rotation'] = self.rotation90ChkBx.isChecked()
        # if self.calSettings is not None:
        #     if self.calSettings["type"] == "img":
        #         flags["center"] = self.calSettings["center"]
        #         flags["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
        #     else:
        #         flags["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings["pixel_size"]
        #         if "center" in self.calSettings:
        #             flags["center"] = self.calSettings["center"]

        # if self.fixed_hull_range is not None and (self.persistROIChkBx.isChecked() or not imgChanged):
        #     flags['fixed_hull'] = self.fixed_hull_range
       

        return flags

    def maxIntChanged(self):
        if self.cirProj is not None and not self.updatingUI:
            if self.maxInt.value() < self.minInt.value():
                self.maxInt.setValue(self.minInt.value()+1)
            self.update_plot['m1_partial_hist'] = True
            self.updateUI()

    def minIntChanged(self):
        if self.cirProj is not None and not self.updatingUI:
            if self.maxInt.value() < self.minInt.value():
                self.minInt.setValue(self.maxInt.value()-1)
            self.update_plot['m1_partial_hist'] = True
            self.updateUI()

    def onNewFileSelected(self, imgList):
        

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
        file=self.fileName+'.info'
        cache_path = os.path.join(self.filePath, "cp_cache",file)
        cache_exist=os.path.isfile(cache_path)
        if self.delcache:
            if os.path.isfile(cache_path):
                print('cache is deleted')
                os.remove(cache_path)
        fileName = self.imgList[self.currentFileNumber]
        print("current file is "+fileName)
        fileFullPath = fullPath(self.filePath, fileName)
        self.cirProj = ScanningDiffraction(self.filePath, fileName, logger=self.logger)
        #self.setMinMaxIntensity(self.cirProj.original_image, self.minInt, self.maxInt, self.minIntLabel, self.maxIntLabel)
        # Calculating grid lines to exclude in pixel data computation
        #grid_lines = np.where(self.cirProj.original_image < 0)
        # if self.rotation90ChkBx.isEnabled():
        #     self.rotation90ChkBx.setChecked('90rotation' in self.cirProj.info and self.cirProj.info['90rotation'])
        self.processImage(True)
        # self.updateStatusBar(fileFullPath + ' (' + str(self.currentFileNumber + 1) + '/' + str(
        #     self.numberOfFiles) + ') is processed.')
        # self.addPixelDataToCsv(grid_lines)
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
        if self.cirProj is not None:
            # QApplication.setOverrideCursor(Qt.WaitCursor)
            flags = self.getFlags(imgChanged)
            self.cirProj.process(flags)
            # QApplication.restoreOverrideCursor()
            self.updateParams()
            self.csvManager.write_new_data(self.cirProj)
            # self.refreshAllTabs()
            # self.updateUI()

    def create_circular_mask(self, h, w, center, radius):
        Y, X = np.ogrid[:h, :w]
        dist_from_center = np.sqrt((X - center[0]) ** 2 + (Y - center[1]) ** 2)

        mask = dist_from_center > radius
        return mask

    def addPixelDataToCsv(self, grid_lines):
        if self.pixelDataFile == None:
            self.pixelDataFile = self.filePath + '/cp_results/BackgroundSummary.csv'
            if not os.path.isfile(self.pixelDataFile):
                header = ['File Name', 'Average Pixel Value (Outside rmin or mask)', 'Number of Pixels (Outside rmin or mask)']
                f = open(self.pixelDataFile, 'a')
                csv_writer = writer(f)
                csv_writer.writerow(header)
                f.close()

        csvDF = pd.read_csv(self.pixelDataFile)
        recordedFileNames = set(csvDF['File Name'].values)

        # Compute the average pixel value and number of pixels outside rmin/mask
        blank, mask = getBlankImageAndMask(self.filePath)
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
        info = self.cirProj.info
        if 'fixed_hull' in info:
            self.fixed_hull_range = info['fixed_hull']
        if 'merged_peaks' in info:
            self.merged_peaks = info['merged_peaks']
        if self.ROI is None and info['ROI'] != [info['start_point'], info['rmax']]:
            self.ROI = info['ROI']
        # if self.orientationModel is None:
        #     # if 'orientation_model' in info:
        #     #     self.orientationCmbBx.setCurrentIndex(
        #     #         self.orientationCmbBx.findText(info['orientation_model']))
        #     self.orientationModel = str(self.orientationCmbBx.currentText())

    def updateStatusBar(self, text):
        QApplication.processEvents()
        self.imagePathLabel.setText(text)
        QApplication.processEvents()

    def updateUI(self):
        if self.cirProj is None:
            return

        selected_tab = self.tabWidget.currentIndex()

        if selected_tab == 0:
            self.updateImageTab()
        elif selected_tab == 1:
            self.updateMethod1Tab()
        elif selected_tab == 2:
            self.updateMethod2Tab()
        elif selected_tab == 3:
            self.updateResultsTab()

    def partialRangeChanged(self):
        if self.updatingUI or self.cirProj is None:
            return
        self.cirProj.info['partial_angle'] = self.partialRange.value()
        self.cirProj.removeInfo('m1_rings')
        self.processImage()

    def prevImage(self):
        self.currentFileNumber = (self.currentFileNumber - 1) % self.numberOfFiles
        self.onImageChanged()

    def nextImage(self):
        self.currentFileNumber = (self.currentFileNumber + 1) % self.numberOfFiles
        self.onImageChanged()

    def fileNameChanged(self):
        fileName = str(self.filenameLineEdit.text()).strip()
        if fileName not in self.imgList:
            return
        self.currentFileNumber = self.imgList.index(fileName)
        self.onImageChanged()

    def zoomFigure(self, figure, canvas, direction, x, y):
        if self.cirProj is None:
            return

        #
        # display_size = figure.get_size_inches() * figure.dpi
        # display_height = display_size[0]
        # display_width = display_size[1]
        # ax = figure.add_subplot(111)
        # original_height = ax.dataLim.height
        # original_width = ax.dataLim.width
        # current_xlim = ax.get_xlim()
        # current_ylim = ax.get_ylim()
        # current_y_size = max(current_ylim) - min(current_ylim)
        # current_x_size = max(current_xlim) - min(current_xlim)
        # ratioY = float(display_height) / float(current_y_size)
        # ratioX = float(display_width) / float(current_x_size)
        # py = y / ratioY + min(current_ylim)
        # px = x / ratioX + min(current_xlim)
        # new_width = int(current_y_size * (1.0 - (direction * 0.1)))
        # new_height = int(current_x_size * (1.0 - (direction * 0.1)))
        # new_width = min(new_width, original_width)
        # new_height = min(new_height, original_height)
        # new_width = max(new_width, 50)
        # new_height = max(new_height, new_width * original_height / original_width)
        # clicked_x_percentage = x / float(display_width)
        # clicked_y_percentage = y / float(display_height)
        # new_xlim = (int(px - (clicked_x_percentage * new_width)), int(self.img_zoom[0] + new_width))
        # new_ylim = (int(py - (clicked_y_percentage * new_height)), int(self.img_zoom[1] + new_height))
        # ax.set_xlim(new_xlim)
        # ax.set_ylim(new_ylim)
        # canvas.draw()

    def wheelOnImg(self, ev):
        direction = ev.delta() / 120
        x = ev.pos().x()
        y = ev.pos().y()
        self.zoomFigure(self.displayImgFigure, self.displayImgCanvas, direction, x, y)

    def refreshMethod2Tab(self):
        self.update_plot['m2_diff'] = True
        self.updateUI()

    def refreshAllTabs(self):
        self.function = None
        for b in self.checkable_buttons:
            b.setChecked(False)
        for k in self.update_plot.keys():
            self.update_plot[k] = True

    def m1_update_plots(self):
        self.update_plot['m1_partial_hist'] = True
        self.update_plot['m1_hist'] = True
        self.updateUI()

    def next_range_pushed(self):
        self.m1_selected_range += 1
        self.update_plot['m1_partial_hist'] = True
        self.updateUI()

    def prev_range_pushed(self):
        self.m1_selected_range -= 1
        self.update_plot['m1_partial_hist'] = True
        self.updateUI()

    def getZoomedImage(self, img):
        if not any(self.img_zoom):
            h,w = img.shape[:2]
            self.img_zoom = [0,0,w,h]
        return img[ self.img_zoom[1]:self.img_zoom[3], self.img_zoom[0]:self.img_zoom[2]]

    def draw_angle_lines(self, img, center, angle, arange):
        scale = img.shape[1] / 2
        angle_line = [(int(center[0] - (scale * np.cos(angle))), int(center[1] - (scale * np.sin(angle)))),
                      (int(center[0] + (scale * np.cos(angle))), int(center[1] + (scale * np.sin(angle))))]
        range1 = [(int(center[0] - (scale * np.cos(arange[0]))), int(center[1] - (scale * np.sin(arange[0])))),
                  (int(center[0] + (scale * np.cos(arange[0]))), int(center[1] + (scale * np.sin(arange[0]))))]
        range2 = [(int(center[0] - (scale * np.cos(arange[1]))), int(center[1] - (scale * np.sin(arange[1])))),
                  (int(center[0] + (scale * np.cos(arange[1]))), int(center[1] + (scale * np.sin(arange[1]))))]

        cv2.line(img, angle_line[0], angle_line[1], (255, 0, 0), 5)
        cv2.line(img, range1[0], range1[1], (0, 255, 255), 5)
        cv2.line(img, range2[0], range2[1], (0, 255, 255), 5)

    def updateImageTab(self):
        img = copy.copy(self.cirProj.original_image)
        if self.blankChkBx.isChecked():
            blank, mask = getBlankImageAndMask(self.filePath)
            if blank is not None:
                img = img - blank

        #img = getBGR(get8bitImage(img, min=self.minInt.value(), max=self.maxInt.value()))

        ax = self.displayImgAxes
        ax.cla()
        if self.logScaleIntChkBx.isChecked():
            ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.minInt.value()), vmax=self.maxInt.value()))
        else:
            ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.minInt.value(), vmax=self.maxInt.value()))
        ax.set_facecolor('black')

        if self.rotation90ChkBx.isEnabled():
            self.rotation90ChkBx.setChecked('90rotation' in self.cirProj.info and self.cirProj.info['90rotation'])

        center = (int(np.round(self.cirProj.info['center'][0])), int(np.round(self.cirProj.info['center'][1])))

        if self.displayRingsChkbx.isChecked() and 'fitResult' in self.cirProj.info.keys():
            fitResult = self.cirProj.info['fitResult']
            peaks = self.cirProj.info['model_peaks']
            num_peaks = len(peaks) + 1

            # TODO: Correction factor for sigma
            h = 2

            for i in range(1, num_peaks):
                radius = fitResult['u' + str(i)]
                sigmad = fitResult['sigmad' + str(i)]

                if radius - h * sigmad > 0:
                    ax.add_patch(
                        patches.Circle(tuple(center), int(round(radius - h*sigmad)), linewidth=2, edgecolor=tuple(np.array(self.ring_colors[(i-1)%len(self.ring_colors)])/255.), facecolor='none'))

                    ax.add_patch(
                        patches.Circle(tuple(center), int(round(radius + h*sigmad)), linewidth=2, edgecolor=tuple(np.array(self.ring_colors[(i-1)%len(self.ring_colors)])/255.), facecolor='none'))


        if 'ring_models' in self.cirProj.info and 'ring_errors' in self.cirProj.info and len(self.cirProj.info['ring_errors']) > 0:
            models = self.cirProj.info['ring_models']
            errors = self.cirProj.info['ring_errors']
            best_ind = min(errors.items(), key=lambda err:err[1])[0]
            #model = models[best_ind]
            model = self.cirProj.info.get('average_ring_model', models[best_ind])
            if model['sigma'] < 1. and errors[best_ind] < 1.:
                self.angleChkBx.setEnabled('average_ring_model' in self.cirProj.info.keys())
                if self.angleChkBx.isChecked():
                    # Draw angle lines
                    angle = model['u']
                    arange = (angle - model['sigma'], angle + model['sigma'])
                    scale = img.shape[1] / 2
                    angle_line = [
                        (int(round(center[0] - (scale * np.cos(angle)))), int(round(center[0] + (scale * np.cos(angle))))),
                         (int(round((center[1] - (scale * np.sin(angle))))), int(round((center[1] + (scale * np.sin(angle))))))]

                    range1 = [
                        (int(round(center[0] - (scale * np.cos(arange[0])))), int(round(center[0] + (scale * np.cos(arange[0]))))),
                        (int(round((center[1] - (scale * np.sin(arange[0]))))), int(round((center[1] + (scale * np.sin(arange[0]))))))]

                    range2 = [
                        (int(round(center[0] - (scale * np.cos(arange[1])))), int(round(center[0] + (scale * np.cos(arange[1]))))),
                        (int(round((center[1] - (scale * np.sin(arange[1]))))), int(round((center[1] + (scale * np.sin(arange[1]))))))]

                    ax.plot(angle_line[0], angle_line[1], color=(1,0,0))
                    ax.plot(range1[0], range1[1], color=(1,0.5,.5))
                    ax.plot(range2[0], range2[1], color=(1,0.5,.5))

        if self.centerChkbx.isChecked():
            ax.add_patch(
                patches.Circle(tuple(center), 3, linewidth=2, edgecolor='w', facecolor='r'))

        if self.rminmaxChkBx.isChecked():
            ax.add_patch(patches.Circle(tuple(center), self.cirProj.info['start_point'], linewidth=2, edgecolor='y',
                                        facecolor='none'))
            ax.add_patch(patches.Circle(tuple(center), self.cirProj.info['rmax'], linewidth=2, edgecolor='y',
                                        facecolor='none'))

        if self.roiChkBx.isChecked():
            roi = self.cirProj.info['ROI']
            ax.add_patch(patches.Wedge(tuple(center), roi[1], 0, 360, width=roi[1]-roi[0], fc='r', alpha=0.25))

        ax.set_ylim((0, img.shape[0]))
        ax.set_xlim((0, img.shape[1]))
        ax.invert_yaxis()
        self.displayImgFigure.tight_layout()
        self.displayImgCanvas.draw()

    def updateMethod1Tab(self):
        if 'm1_partial_hists' in self.cirProj.info.keys() and 'partial_ranges' in self.cirProj.info.keys() and \
                self.update_plot['m1_partial_hist']:
            partial_ranges = self.cirProj.info['partial_ranges']
            self.m1_selected_range %= len(partial_ranges)
            selected_range = partial_ranges[self.m1_selected_range]
            str_info = "Range : " + str(selected_range)
            hist = self.cirProj.info['m1_partial_hists'][self.m1_selected_range]
            hull = self.cirProj.info['m1_partial_hulls'][self.m1_selected_range]
            ax = self.m1_partial_hist_axes
            ax.cla()

            if self.m1OriginalHistChkbx.isChecked():
                ax.plot(hist, color='b')

            ax.plot(hull, color='g')

            if 'm1_partial_peaks' in self.cirProj.info.keys():
                peaks = self.cirProj.info['m1_partial_peaks'][self.m1_selected_range]
                str_info += "   Peaks : "+str(peaks)
                for p in peaks:
                    ax.plot([p, p], [0, max(hist)], color='r')

            end_plot = len(hist)
            start_plot = 0
            if self.skipFirstPeakChkBx.isChecked() and 'start_point' in self.cirProj.info.keys():

                if 'merged_peaks' in self.cirProj.info.keys() and len(self.cirProj.info['merged_peaks']) > 0:
                    merged_rings = self.cirProj.info['merged_peaks']
                    last_ring = max(merged_rings)
                    first_ring = min(merged_rings)
                    end_plot = int(last_ring * 1.4)
                    start_plot = int(first_ring * 0.4)

            if self.m1OriginalHistChkbx.isChecked():
                max_peak = max(hist[start_plot:end_plot]) * 1.1
            else:
                max_peak = max(hull[start_plot:end_plot]) * 1.1

            ax.set_xlim(start_plot, end_plot)
            ax.set_ylim(0 , max_peak)
            ax.set_title(str_info)
            ax.set_xlabel('Radial distance')
            ax.set_ylabel('Intensity')
            # self.m1_partial_hist_figure.tight_layout()
            self.m1_partial_hist_canvas.draw()

            img = copy.copy(self.cirProj.original_image)

            if self.blankChkBx.isChecked():
                blank, mask = getBlankImageAndMask(self.filePath)
                if blank is not None:
                    img = img - blank

            img = get8bitImage(img, min=self.minInt.value(), max=self.maxInt.value())

            center = (int(np.round(self.cirProj.info['center'][0])), int(np.round(self.cirProj.info['center'][1])))
            radius = int(distance((0,0),(img.shape[1],img.shape[0])))
            mask = np.zeros((img.shape[0], img.shape[1]), dtype=np.uint8)
            cv2.ellipse(mask, center, axes=(radius, radius), angle=0, startAngle=selected_range[0],
                        endAngle=selected_range[1], color=255,
                        thickness=-1)
            # img[mask > 0] += 25
            img = getBGR(img)
            r, g, b = cv2.split(img)
            red_panel = r.astype(np.int)
            red_panel[mask > 0] += 50
            red_panel[red_panel>255] = 255
            r = red_panel.astype(r.dtype)
            img = cv2.merge((r, g, b))
            ax = self.m1_img_axes
            ax.cla()
            ax.imshow(img)
            # self.m1_img_fig.tight_layout()
            self.m1_img_canvas.draw()
            self.update_plot['m1_partial_hist'] = False

        if 'orig_hists' in self.cirProj.info.keys() and 'm1_rings' in self.cirProj.info.keys() and \
                self.update_plot['m1_hist'] and 'hull_hist' in self.cirProj.info.keys():
            hist = self.cirProj.info['orig_hists']
            hull = self.cirProj.info['hull_hist']
            m1_rings = self.cirProj.info['m1_rings']
            ax = self.m1_hist_axes
            self.m1_hist_figure.subplots_adjust(top=0.90, bottom=0.20)
            ax.cla()
            for p in m1_rings:
                ax.plot([p, p], [0, max(hist)], color='r')

            if self.m1OriginalHistChkbx.isChecked():
                ax.plot(hist, color='b')
            else:
                hist = copy.copy(hull)

            ax.plot(hull, color='g')

            end_plot = len(hist)
            start_plot = 0
            if self.skipFirstPeakChkBx.isChecked() and 'start_point' in self.cirProj.info.keys():

                if 'merged_peaks' in self.cirProj.info.keys() and len(self.cirProj.info['merged_peaks']) > 0:
                    merged_rings = self.cirProj.info['merged_peaks']
                    last_ring = max(merged_rings)
                    first_ring = min(merged_rings)
                    end_plot = int(last_ring * 1.4)
                    start_plot = int(first_ring * 0.4)

            max_peak = max(hist[start_plot:end_plot]) * 1.1
            ax.set_xlim(start_plot, end_plot)
            ax.set_ylim(0, max_peak)
            ax.set_title('Peaks : '+str(m1_rings))
            ax.set_xlabel('Radial distance (Pixels)')
            ax.set_ylabel('Intensity')
            # self.m1_hist_figure.tight_layout()
            self.m1_hist_canvas.draw()
            self.update_plot['m1_hist'] = False
        if 'partial_angle' in self.cirProj.info.keys():
            self.updatingUI = True
            self.partialRange.setValue(self.cirProj.info['partial_angle'])
            self.updatingUI = False

    def updateMethod2Tab(self):
        if self.update_plot['m2_diff']:
            ax = self.m2_cent_diff_axes
            self.m2_cent_diff_fig.subplots_adjust(bottom=0.20)
            ax.cla()

            if self.method2ComboBx.currentIndex()==0 and 'tophat_2dintegration' in self.cirProj.info.keys():
                hist = self.cirProj.info['tophat_2dintegration'][0]
                ax.imshow(hist)
            elif self.method2ComboBx.currentIndex()==1 and 'm2_central_difference' in self.cirProj.info.keys():
                cent_diff = self.cirProj.info['m2_central_difference']
                ax.imshow(cent_diff)
            elif self.method2ComboBx.currentIndex()==2 and 'central_log' in self.cirProj.info.keys():
                hist = self.cirProj.info['central_log']
                ax.imshow(hist)

            x_lim = ax.get_xlim()
            y_lim = ax.get_ylim()

            if self.runsChkBx.isChecked() and 'm2_runs_dict' in self.cirProj.info.keys():
                runs_dict = self.cirProj.info['m2_runs_dict']
                for k in runs_dict.keys():
                    for run in runs_dict[k]:
                        ax.plot([run[0][1], run[1][1]], [run[0][0], run[1][0]], 'r')

            str_peak = ""
            if self.ringsChkBx.isChecked() and 'm2_rings' in self.cirProj.info.keys():
                rings = self.cirProj.info['m2_rings']
                for ring in rings.keys():
                    ax.plot([ring, ring], [0.1, 359], color = 'w' , lw = 2)
                # str_peak += '\nPeaks : '+str(rings)
            ax.set_xlim(x_lim)
            ax.set_ylim(y_lim)

            ax.set_xlabel('Radial distance (Pixels)'+str_peak)
            ax.set_ylabel('Angle')
            self.m2_cent_diff_fig.tight_layout()
            self.m2_cent_diff_canvas.draw()
            self.update_plot['m2_diff'] = False

    def swapCheckBoxes(self):
        hide = (self.graph_cmbbx.currentIndex() != 0)
        self.dspacing_chkbx.setHidden(hide)
        self.skip_first_peak_chkbx.setHidden(hide)
        self.original_hist_chkbx.setHidden(hide)
        self.rings_chkbx.setHidden(hide)
        self.hull_hist_chkbx.setHidden(hide)
        self.fit_hist_chkbx.setHidden(hide)
        self.selectPeaks.setHidden(hide)
        self.average_ring_chkbx.setHidden(not hide)
        self.ring_hists_chkbx.setHidden(not hide)
        self.g_model_chkbx.setHidden(not hide or not self.orientationModel.startswith('GMM'))

    def updateResultsTab(self):
        self.swapCheckBoxes()

        if self.graph_cmbbx.currentIndex() == 0:
            if 'model_peaks' in self.cirProj.info.keys():
                model_peaks = self.cirProj.info['model_peaks']
            else:
                model_peaks = []
            original_hist = self.cirProj.info['orig_hists']
            start_point = self.cirProj.info['start_point']
            ax = self.result_graph_axes
            ax.cla()

            # lines = []
            labels = []

            start_plot = 0
            end_plot = len(original_hist)

            if len(model_peaks) > 0:
                start_plot = int(min(model_peaks)*0.4)
                end_plot = int(max(model_peaks) * 1.4)

            max_peak = 0
            if self.original_hist_chkbx.isChecked():
                line, = ax.plot(original_hist, color='b')
                # lines.append(line)
                labels.append('Original')
                max_peak = max(original_hist[start_plot:end_plot])

            if self.hull_hist_chkbx.isChecked():
                hull_hist = self.cirProj.info['hull_hist']
                line, = ax.plot(hull_hist, color='m')
                # lines.append(line)
                labels.append('No BG')
                max_peak = max(max(hull_hist), max_peak)

            if self.fit_hist_chkbx.isChecked() and 'fitResult' in self.cirProj.info.keys():
                fit_result = self.cirProj.info['fitResult']
                x = np.array(range(start_point, len(original_hist)))
                fit_hist = GMM_any(x = x, params=fit_result)

                if fit_hist is not None:
                    line, = ax.plot(x, fit_hist, color='g')
                    # lines.append(line)
                    labels.append('Fit Model')
                    max_peak = max(max(fit_hist), max_peak)

            if self.skip_first_peak_chkbx.isChecked():
                max_peak = max_peak * 1.1
            else:
                max_peak = max(ax.get_ylim())
                start_plot = 0
                end_plot = len(original_hist)
            ax.set_ylim(0, max_peak)

            if self.rings_chkbx.isChecked() and len(model_peaks) > 0:
                for i in range(len(model_peaks)):
                    line = ax.axvline(model_peaks[i], color=tuple(np.array(self.ring_colors[i%len(self.ring_colors)])/255.))
                    # lines.append(line)
                labels.append('Merged Rings')

            self.dspacing_chkbx.setEnabled('lambda_sdd' in self.cirProj.info)
            if 'lambda_sdd' in self.cirProj.info and self.dspacing_chkbx.isChecked():
                ax.set_xlim(model_peaks[0] / 2, end_plot)
                ax.set_xscale('dspacing', lambda_sdd=self.cirProj.info['lambda_sdd'])
                ax.set_xlabel('d-spacing (nm)')
            else:
                ax.set_xlim(start_plot, end_plot)
                ax.set_xlabel('Radial distance')

            ax.set_ylabel('Intensity')
            # ax.legend(lines, labels)
            # self.update_plot['image_result'] = False

        elif self.orientationModel.startswith('GMM'):
            model = self.orientationModel
            self.g_model_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())
            self.ring_hists_chkbx.setEnabled('ring_hists' in self.cirProj.info.keys())
            self.average_ring_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())

            ax = self.result_graph_axes
            ax.cla()
            ax.set_xlabel("Radian")
            ax.set_ylabel("Intensity")

            if 'ring_hists' in self.cirProj.info.keys():
                ring_hists = self.cirProj.info['ring_hists']
                x = np.arange(0, 2 * np.pi, 2 * np.pi / 360)
                if self.ring_hists_chkbx.isChecked():
                    for i in range(len(ring_hists)):
                        ax.plot(x, ring_hists[i], color = tuple(np.array(self.ring_colors[i%len(self.ring_colors)])/255.))

                if 'ring_models' in self.cirProj.info.keys() and self.g_model_chkbx.isChecked():
                    ring_models = self.cirProj.info['ring_models']
                    ring_errors = self.cirProj.info['ring_errors']
                    for i in ring_models.keys():
                        if ring_errors[i] < 1.5:
                            gauss = (orientation_GMM2 if model == "GMM2" else orientation_GMM3)(x=x, **ring_models[i])
                            ax.plot(x, gauss, color='g')
                            u1 = ring_models[i]['u']
                            u2 = u1 - np.pi if u1 >= np.pi else u1 + np.pi
                            ax.plot((u1, u1), (0, max(gauss)), color='y')
                            ax.plot((u2, u2), (0, max(gauss)), color='y')

                if 'average_ring_model' in self.cirProj.info.keys() and self.average_ring_chkbx.isChecked():
                    mod = self.cirProj.info['average_ring_model']
                    gauss = (orientation_GMM2 if model == "GMM2" else orientation_GMM3)(x=x, **mod)
                    u1 = mod['u']
                    u2 = u1 - np.pi if u1 >= np.pi else u1 + np.pi
                    ax.plot(x, gauss, color='k')
                    ax.plot((u1, u1), (0, max(gauss)), color='r')
                    ax.plot((u2, u2), (0, max(gauss)), color='r')

        elif self.orientationModel.startswith('Herman'):
            self.ring_hists_chkbx.setEnabled('ring_hists' in self.cirProj.info.keys())
            self.average_ring_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())

            ax = self.result_graph_axes
            ax.cla()
            ax.set_xlabel("Radian")
            ax.set_ylabel("Herman Orientation Factor")

            if 'ring_hists' in self.cirProj.info.keys():
                x = np.arange(0, 2 * np.pi, np.pi / 180)
                if 'ring_models' in self.cirProj.info.keys() and self.ring_hists_chkbx.isChecked():
                    ring_models = self.cirProj.info['ring_models']
                    for i in ring_models:
                        ax.plot(x, ring_models[i]['HoFs'], color='g')
                        u1 = ring_models[i]['u']
                        ax.plot((u1, u1), (-0.5, 1), color='y')

                if 'average_ring_model' in self.cirProj.info.keys() and self.average_ring_chkbx.isChecked():
                    mod = self.cirProj.info['average_ring_model']
                    ax.plot(x, mod['HoFs'], color='k')
                    u1 = mod['u']
                    ax.plot((u1, u1), (-0.5, 1), color='r')

        else: # Max Intensity
            self.ring_hists_chkbx.setEnabled('ring_hists' in self.cirProj.info.keys())
            self.average_ring_chkbx.setEnabled('average_ring_model' in self.cirProj.info.keys())

            ax = self.result_graph_axes
            ax.cla()
            ax.set_xlabel("Radian")
            ax.set_ylabel("Intensity")

            if 'ring_hists' in self.cirProj.info.keys():
                ring_hists = self.cirProj.info['ring_hists']
                x = np.arange(0, 2 * np.pi, np.pi / 180)
                if 'ring_models' in self.cirProj.info.keys() and self.ring_hists_chkbx.isChecked():
                    ring_models = self.cirProj.info['ring_models']
                    print(ring_models, len(ring_hists))
                    for i, idx in enumerate(ring_models):
                        ax.plot(x, ring_hists[i], color = tuple(np.array(self.ring_colors[i%len(self.ring_colors)])/255.))
                        u1 = ring_models[idx]['u']
                        ax.plot((u1, u1), (0, max(ring_hists[i])), color='y')
                        ax.plot((u1 + np.pi, u1 + np.pi), (0, max(ring_hists[i])), color='y')

                if 'average_ring_model' in self.cirProj.info.keys() and self.average_ring_chkbx.isChecked():
                    mod = self.cirProj.info['average_ring_model']
                    ax.plot(x, mod['hist'], color='k')
                    u1 = mod['u']
                    ax.plot((u1, u1), (0, max(mod['hist'])), color='r')
                    ax.plot((u1 + np.pi, u1 + np.pi), (0, max(mod['hist'])), color='r')

        self.result_graph_figure.tight_layout()
        self.result_graph_canvas.draw()

        processing_results_text = "Total Intensity : "+ str(self.cirProj.info['area'])
        processing_results_text += "\n\nFitting Results :"
        if 'fitResult' in self.cirProj.info.keys():
                fit_result = self.cirProj.info['fitResult']
                n = int(len(fit_result.keys())/3)
                for i in range(1, n+1):
                    processing_results_text += "\nPeak "+str(i)+': '
                    processing_results_text += "\tcenter(pixel) : "+str(fit_result['u'+str(i)])+'\n'
                    if 'peak_ds' in self.cirProj.info:
                        processing_results_text += "\tcenter(nm) : " + str(self.cirProj.info['peak_ds'][i-1]) + '\n'
                    processing_results_text += "\tarea  : " + str(fit_result['alpha' + str(i)]) + '\n'
                    processing_results_text += "\tsigmad : " + str(fit_result['sigmad' + str(i)]) + '\n'

        self.processing_results.setText(processing_results_text)

        if 'ring_models' in self.cirProj.info.keys() and len(self.cirProj.info['ring_models']) > 0:
            models = self.cirProj.info['ring_models']
            errors = self.cirProj.info['ring_errors']

            rings_info = "Rings Information : \n"

            for i in models.keys():
                m = models[i]
                rings_info += "Ring " + str(i + 1) + " : \n"
                rings_info += "\tAngle : " + str(m['u']) + " rads. " + str(convertRadtoDegrees(m['u'])) + "degrees\n"
                if self.orientationModel.startswith('GMM'):
                    angle_range = (m['u'] - m['sigma'], m['u'] + m['sigma'])
                    rings_info += "\tRange: " + str(angle_range) + " rads"
                    rings_info += " or " + str((convertRadtoDegrees(angle_range[0]), convertRadtoDegrees(angle_range[1]))) + " degrees\n"
                    rings_info += "\tSigma : "+ str(m['sigma'])+ "\n"
                    rings_info += "\tIntensity : "+ str(m['alpha'])+ "\n"
                    rings_info += "\tFitting Error : "+ str(errors[i])+ "\n\n"

            rings_info += "\nAverage Angle : \n"
            if 'average_ring_model' in self.cirProj.info.keys():
                model = self.cirProj.info['average_ring_model']
                rings_info += " - Angle : " + str(model['u']) + " rads. " + str(
                    convertRadtoDegrees(model['u'])) + "degrees\n"
                if self.orientationModel.startswith('GMM'):
                    angle_range = (model['u'] - model['sigma'], model['u'] + model['sigma'])
                    rings_info += " - Standard deviation : " + str(model['sigma']) + "\n"
                    rings_info += " - Range: " + str(angle_range) + " rads"
                    rings_info += " or " + str(
                        (convertRadtoDegrees(angle_range[0]), convertRadtoDegrees(angle_range[1]))) + " degrees\n"
                    rings_info += " - Intensity: " + str(model['alpha']) + "\n"
            else:
                if 'ring_models' in self.cirProj.info.keys() and len(self.cirProj.info['ring_models']) > 0:
                    rings_info += "Model can't be fitted. Rings are uniform\n"
                else:
                    rings_info += "N/A\n"
        else:
            rings_info = "Rings Information : N/A"

        self.rings_results.setText(rings_info)

            # self.update_plot['results_text'] = False

    def mousePressEvent(self, event):
        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()
