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
import numpy as np
from lmfit import Model, Parameters
from lmfit.models import GaussianModel, VoigtModel
from sklearn.metrics import r2_score
import fabio
from musclex import __version__
try:
    from ..utils.file_manager import fullPath, createFolder, ifHdfReadConvertless, getBlankImageAndMask, getMaskOnly
    from ..utils.histogram_processor import movePeaks, getPeakInformations, convexHull
    from ..utils.image_processor import *
except: # for coverage
    from utils.file_manager import fullPath, createFolder, ifHdfReadConvertless, getBlankImageAndMask, getMaskOnly
    from utils.histogram_processor import movePeaks, getPeakInformations, convexHull
    from utils.image_processor import *

class ProjectionProcessor:
    """
    A class for Bio-Muscle processing - go to process() to see all processing steps
    """
    def __init__(self, dir_path, file_name, file_list=None, extension=''):
        self.dir_path = dir_path
        self.filename = file_name
        if extension in ('.hdf5', '.h5'):
            index = next((i for i, item in enumerate(file_list[0]) if item == file_name), 0)
            img = file_list[1][index]
        else:
            img = fabio.open(fullPath(dir_path, file_name)).data
        # if img.shape[1] > img.shape[0]: # image is longer than it is wide
        #     img = cv2.copyMakeBorder(img, top=int((img.shape[1]-img.shape[0])/2), bottom=int((img.shape[1]-img.shape[0])/2), left=0, right=0, borderType=cv2.BORDER_CONSTANT)
        # else:
        #     img = cv2.copyMakeBorder(img, top=0, bottom=0, left=int((img.shape[0]-img.shape[1])/2), right=int((img.shape[0]-img.shape[1])/2), borderType=cv2.BORDER_CONSTANT)
        # img -= img.min()
        self.orig_img = img
        self.orig_img = ifHdfReadConvertless(self.filename, self.orig_img)
        self.orig_img = self.orig_img.astype("float32")
        self.rotated_img = None
        self.rotated = False
        self.version = __version__
        self.masked = False
        self.fixed_sigma = {}
        cache = self.loadCache()
        self.fixed_center = {}
        self.rotMat = None  # store the rotation matrix used so that any point specified in current co-ordinate system can be transformed to the base (original image) co-ordinate system
        if cache is None:
            # info dictionary will save all results
            self.info = {
                'box_names' : set(),
                'boxes' : {},
                'types' : {},
                'hists' : {},
                'peaks' : {},
                'bgsubs' : {},
                'merid_bg' : {},
                'hull_ranges':{},
                'hists2': {},
                'fit_results':{},
                'subtracted_hists' : {},
                'moved_peaks':{},
                'baselines':{},
                'centroids':{},
                'widths': {},
                'areas': {},
                'centerx': self.orig_img.shape[0] / 2 - 0.5,
                'centery': self.orig_img.shape[1] / 2 - 0.5,
                'rotationAngle' : 0
            }
        else:
            self.info = cache

    def addBox(self, name, box, typ, bgsub):
        """
        Add a box to info. If it exists and it changed, clear all old result
        :param name: box name
        :param box: box coordinates
        :param typ: box typ 'v' ad vertical, 'h' as horizontal
        :param bgsub: background subtraction method 0 = fitting gaussians, 1 = convex hull
        :return:
        """
        box_names = self.info['box_names']
        if name in box_names and typ == 'oriented' and self.info['boxes'][name][-1] != box[-1]:
            self.removeInfo(name)
            self.addBox(name, box, typ, bgsub)
        elif name in box_names and self.info['boxes'][name] != box:
            self.removeInfo(name)
            self.addBox(name, box, typ, bgsub)
        else:
            box_names.add(name)
            self.info['boxes'][name] = box
            self.info['types'][name] = typ
            self.info['bgsubs'][name] = bgsub

    def addPeaks(self, name, peaks):
        """
        Add peaks to a box.
        :param name: box name
        :param peaks: peaks
        :return:
        """
        box_names = self.info['box_names']
        if name in box_names:
            all_peaks = self.info['peaks']
            if name in all_peaks and all_peaks[name] == peaks:
                return
            all_peaks[name] = peaks
            skip_list = ['box_names', 'boxes', 'types', 'peaks', 'hists', 'bgsubs', 'merid_bg']
            for k in self.info.keys():
                if k not in skip_list:
                    self.removeInfo(name, k)
        else:
            print("Warning : box name is invalid.")

    def removePeaks(self, name):
        """
        Remove all peaks from a box
        :param name: box name
        :return:
        """
        skip_list = ['box_names', 'boxes', 'types', 'bgsubs', 'merid_bg']
        for k in self.info.keys():
            if k not in skip_list:
                self.removeInfo(name, k)

    def process(self, settings={}):
        """
        All processing steps - all settings are provided by Projection Traces app as a dictionary
        """
        self.updateSettings(settings)
        self.applyBlankImageAndMask()
        self.getHistograms()
        self.applyConvexhull()
        self.updateRotationAngle()
        self.fitModel()
        self.getBackgroundSubtractedHistograms()
        self.getPeakInfos()
        if 'no_cache' not in settings:
            self.cacheInfo()

    def applyBlankImageAndMask(self):
        """
        Apply the blank image and mask threshold on the orig_img
        :return: -
        """
        if 'blank_mask' in self.info and self.info['blank_mask'] and not self.masked:
            img = np.array(self.orig_img, 'float32')
            blank, mask = getBlankImageAndMask(self.dir_path)
            maskOnly = getMaskOnly(self.dir_path)
            if blank is not None:
                img = img - blank
            if mask is not None:
                img[mask > 0] = self.info['mask_thres'] - 1.
            if maskOnly is not None:
                print("Applying mask only image")
                img[maskOnly > 0] = self.info['mask_thres'] - 1
            
            self.info['hists'] = {}
            self.orig_img = img
            self.masked = True

    def updateSettings(self, settings):
        """
        Update info dict using settings
        :param settings: calibration settings
        :return: -
        """
        if 'boxes' in settings:
            new_boxes = settings['boxes']
            types = settings['types']
            bgsubs = settings['bgsubs']
            old_boxes = self.info['boxes']
            all_name = list(new_boxes.keys())
            all_name.extend(list(old_boxes.keys()))
            all_name = set(all_name)
            for name in all_name:
                if name in new_boxes.keys():
                    if 'refit' in settings:
                        self.removeInfo(name, 'fit_results')
                    self.addBox(name, new_boxes[name], types[name], bgsubs[name])
                else:
                    self.removeInfo(name)
            del settings['boxes']
            del settings['types']
            del settings['bgsubs']

        if 'peaks' in settings:
            new_peaks = settings['peaks']
            old_peaks = self.info['peaks']
            all_name = list(new_peaks.keys())
            all_name.extend(list(old_peaks.keys()))
            all_name = set(all_name)
            for name in all_name:
                if name in new_peaks.keys():
                    self.addPeaks(name, new_peaks[name])
                else:
                    self.removePeaks(name)
            del settings['peaks']

        if 'hull_ranges' in settings:
            new = settings['hull_ranges']
            current = self.info['hull_ranges']
            current.update(new)
            del settings['hull_ranges']

        if 'rotated' in settings:
            self.rotated = settings['rotated']
        else:
            self.rotated = False

        self.info.update(settings)

        if 'centerx' not in self.info or 'centery' not in self.info:
            self.info['centerx'] = self.orig_img.shape[0] / 2 - 0.5
            self.info['centery'] = self.orig_img.shape[1] / 2 - 0.5
        
        if 'mask_thres' not in self.info:
            if 'mask_thres' in settings:
                self.info['mask_thres'] = settings['mask_thres']
            else:
                self.info['mask_thres'] = getMaskThreshold(self.orig_img)
        if 'blank_mask' in settings:
            self.info['blank_mask'] = settings['blank_mask']

    def getHistograms(self):
        """
        Obtain projected intensity for each box
        """
        box_names = self.info['box_names']
        if len(box_names) > 0:
            boxes = self.info['boxes']
            types = self.info['types']
            hists = self.info['hists']
            for name in box_names:
                if name in hists:
                    continue
                t = types[name]
                if self.rotated:
                    img = self.getRotatedImage()
                else:
                    img = copy.copy(self.orig_img)

                if name not in hists:
                    b = boxes[name]

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

                    hists[name] = hist
                    self.removeInfo(name, 'hists2')

    def applyConvexhull(self):
        """
        Apply Convex hull to the projected intensity if background subtraction method is 1 (Convex hull)
        :return:
        """
        box_names = self.info['box_names']
        if len(box_names) > 0:
            boxes = self.info['boxes']
            all_peaks = self.info['peaks']
            hists = self.info['hists']
            bgsubs = self.info['bgsubs']
            hists2 = self.info['hists2']
            types = self.info['types']
            hull_ranges = self.info['hull_ranges']
            for name in box_names:
                if name in hists2:
                    continue

                if bgsubs[name] == 1 and name in all_peaks and len(all_peaks[name]) > 0:
                    # apply convex hull to the left and right if peaks are specified
                    box = boxes[name]
                    hist = hists[name]
                    peaks = all_peaks[name]
                    start_x = box[0][0]
                    start_y = box[1][0]
                    if types[name] == 'h':
                        centerX = self.info['centerx'] - start_x
                    elif types[name] == 'oriented':
                        centerX = box[6][0] - start_x
                    else:
                        centerX = self.info['centery'] - start_y
                    centerX = int(round(centerX))
                    right_hist = hist[centerX:]
                    left_hist = hist[:centerX][::-1]
                    min_len = min(len(right_hist), len(left_hist))

                    if name not in hull_ranges:
                        start = max(min(peaks) - 15, 10)
                        end = min(max(peaks) + 15, min_len)
                        hull_ranges[name] = (start, end)

                    # find start and end points
                    (start, end) = hull_ranges[name]

                    left_ignore = np.array([(i <= self.info['mask_thres']) for i in left_hist])
                    right_ignore = np.array([(i <= self.info['mask_thres']) for i in right_hist])
                    if not any(left_ignore) and not any(right_ignore):
                        left_ignore = None
                        right_ignore = None

                    left_hull = convexHull(left_hist, start, end, ignore=left_ignore)[::-1]
                    right_hull = convexHull(right_hist, start, end, ignore=right_ignore)

                    hists2[name] = np.append(left_hull, right_hull)
                else:
                    # use original histogram and apply threshold
                    hists2[name] = copy.copy(hists[name])
                    hists2[name][hists2[name] <= self.info['mask_thres']] = self.info['mask_thres']

                self.removeInfo(name, 'fit_results')

    def updateRotationAngle(self):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal.
        The angle will be kept in self.info["rotationAngle"]
        """
        if 'rotationAngle' not in self.info:
            center = (self.info['centerx'], self.info['centery'])
            img = copy.copy(self.orig_img)
            if 'detector' in self.info:
                self.info['rotationAngle'] = getRotationAngle(img, center, man_det=self.info['detector'])
            else:
                self.info['rotationAngle'] = getRotationAngle(img, center)

    def fitModel(self):
        """
        Fit model to histogram
        Fit results will be kept in self.info["fit_results"].
        """
        box_names = self.info['box_names']
        all_hists = self.info['hists2']
        bgsubs = self.info['bgsubs']
        all_peaks = self.info['peaks']
        all_boxes = self.info['boxes']
        fit_results = self.info['fit_results']

        for name in box_names:
            hist = np.array(all_hists[name])

            if name not in all_peaks or len(all_peaks[name]) == 0 or name in fit_results:
                continue

            peaks = all_peaks[name]
            box = all_boxes[name]
            start_x = box[0][0]
            start_y = box[1][0]

            x = np.arange(0, len(hist))

            int_vars = {
                'x' : x
            }

            # Initial Parameters
            params = Parameters()

            # Init Center X
            if self.info['types'][name] == 'h':
                # init_center = self.orig_img.shape[1] / 2 - 0.5 - start_x
                init_center = self.info['centerx'] - start_x
            elif self.info['types'][name] == 'oriented':
                init_center = box[6][0] - start_x
            else:
                # init_center = self.orig_img.shape[0] / 2 - 0.5 - start_y
                init_center = self.info['centery'] - start_y

            init_center = int(round(init_center))
            params.add('centerX', init_center, min=init_center - 1., max=init_center + 1.)

            if bgsubs[name] == 1:
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

                if 'main_peak_info' in self.info and name in self.info['main_peak_info']:
                    params.add('bg_sigma', self.info['main_peak_info'][name]['bg_sigma'], min=1, max=len(hist)*2+1.)
                    params.add('bg_amplitude', self.info['main_peak_info'][name]['bg_amplitude'], min=-1, max=sum(hist)+1.)
                    params.add('center_sigma1', self.info['main_peak_info'][name]['center_sigma1'], min=1, max=len(hist)+1.)
                    params.add('center_amplitude1', self.info['main_peak_info'][name]['center_amplitude1'], min=-1, max=sum(hist) + 1.)
                    params.add('center_sigma2', self.info['main_peak_info'][name]['center_sigma2'], min=1, max=len(hist)+1.)
                    params.add('center_amplitude2', self.info['main_peak_info'][name]['center_amplitude2'], min=-1, max=sum(hist)+1.)
                
                else:
                # Init background params
                    params.add('bg_sigma', len(hist)/3., min=1, max=len(hist)*2+1.)
                    params.add('bg_amplitude', 0, min=-1, max=sum(hist)+1.)

                    if self.info['merid_bg'][name]:
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
            for j,p in enumerate(peaks):
                # if j in self.fixed_center:
                #     params.add('p_' + str(j), self.fixed_center[j])
                # else:
                params.add('p_' + str(j), p, min=p - 10., max=p + 10.)
                if j in self.fixed_sigma:
                    params.add('sigma' + str(j), self.fixed_sigma[j], vary=False)
                else:
                    params.add('sigma' + str(j), 10, min=1, max=50.)
                # if j in self.fixed_center:
                #     params.add('fix' + str(j), self.fixed_center[j])
                params.add('amplitude' + str(j), sum(hist)/10., min=-1)
                # params.add('gamma' + str(j), 0. , min=0., max=30)

            # Fit model
            model = Model(layerlineModel, nan_policy='propagate', independent_vars=int_vars.keys())
            result = model.fit(hist, verbose=False, params=params, **int_vars)
            if result is not None:
                result_dict = result.values
                int_vars.pop('x')
                result_dict.update(int_vars)
                result_dict['error'] = 1. - r2_score(hist, layerlineModel(x, **result_dict))
                
                if 'main_peak_info' in self.info and name in self.info['main_peak_info']:
                    if self.info['main_peak_info'][name]['bg_sigma_lock'] == True:
                        result_dict['bg_sigma'] = self.info['main_peak_info'][name]['bg_sigma']
                    if self.info['main_peak_info'][name]['bg_amplitude_lock'] == True:
                        result_dict['bg_amplitude'] = self.info['main_peak_info'][name]['bg_amplitude']
                    if self.info['main_peak_info'][name]['center_sigma1_lock'] == True:
                        result_dict['center_sigma1'] = self.info['main_peak_info'][name]['center_sigma1']   
                    if self.info['main_peak_info'][name]['center_amplitude1_lock'] == True:
                        result_dict['center_amplitude1'] = self.info['main_peak_info'][name]['center_amplitude1']
                    if self.info['main_peak_info'][name]['center_sigma2_lock'] == True:
                        result_dict['center_sigma2'] = self.info['main_peak_info'][name]['center_sigma2']
                    if self.info['main_peak_info'][name]['center_amplitude2_lock'] == True:
                        result_dict['center_amplitude2'] = self.info['main_peak_info'][name]['center_amplitude2']
                
                self.info['fit_results'][name] = result_dict
                self.removeInfo(name, 'subtracted_hists')
                for i in self.fixed_center:
                    if 'p_'+str(i) in self.info['fit_results'][name]:
                        self.info['fit_results'][name]['p_'+str(i)] = self.fixed_center[i]
                print("Box : "+ str(name))
                print("Fitting Result : " + str(self.info['fit_results'][name]))
                print("Fitting Error : " + str(self.info['fit_results'][name]['error']))
                print("---")


    def getBackgroundSubtractedHistograms(self):
        """
        Get Background Subtracted Histograms by subtract the original histogram by background from fitting model
        :return:
        """
        box_names = self.info['box_names']
        all_hists = self.info['hists2']
        fit_results = self.info['fit_results']
        subt_hists = self.info['subtracted_hists']
        bgsubs = self.info['bgsubs']
        for name in box_names:
            if name in subt_hists or name not in fit_results:
                continue
            if bgsubs[name] == 2: # no background, so there should be no subtraction
                subt_hists[name] = all_hists[name]
                self.removeInfo(name, 'moved_peaks')
            else:
                # Get subtracted histogram if fit result exists
                fit_result = fit_results[name]
                hist = all_hists[name]
                xs = np.arange(0, len(hist))
                background = layerlineModelBackground(xs, **fit_result)
                subt_hists[name] = hist-background
                self.removeInfo(name, 'moved_peaks')

    def getPeakInfos(self):
        """
        Get peaks' infomation including baseline and centroids
        :return:
        """
        box_names = self.info['box_names']
        all_hists = self.info['subtracted_hists']
        fit_results = self.info['fit_results']
        moved_peaks = self.info['moved_peaks']
        all_baselines = self.info['baselines']
        all_centroids = self.info['centroids']
        all_widths = self.info['widths']
        all_areas = self.info['areas']

        for name in box_names:
            if name not in all_hists:
                continue

            ### Find real peak locations in the box (not distance from center)
            model = fit_results[name]
            hist = all_hists[name]
            if name not in moved_peaks:
                peaks = self.info['peaks'][name]
                moved = []
                for p in peaks:
                    globalpeak = int(round(model['centerX']+p))
                    if 0 <= globalpeak <= len(hist):
                        moved.append(globalpeak)
                    # else:
                    #     moved.append(int(round(model['centerX']-p)))
                moved = movePeaks(hist, moved, 10)
                moved_peaks[name] = moved
                self.removeInfo(name, 'baselines')

            peaks = moved_peaks[name]
            ### Calculate Baselines
            if name not in all_baselines:
                baselines = []
                for p in peaks:
                    baselines.append(hist[p]*0.5)
                all_baselines[name] = baselines
                self.removeInfo(name, 'centroids')

            baselines = all_baselines[name]

            if name not in all_centroids:
                results = getPeakInformations(hist, peaks, baselines)
                all_centroids[name] = results['centroids'] - model['centerX']
                all_widths[name] = results['widths']
                all_areas[name] = results['areas']
                print("Box : "+ str(name))
                print("Centroid Result : " + str(results))
                print("---")

    def setGaussCenter(self, box_name, peak_num, new_center):
        new_center = float(str(new_center))
        self.fixed_center[peak_num] = new_center
        self.removeInfo(box_name, 'fit_results')
        #self.fitModel()

    def setGaussSig(self, box_name, peak_num, new_sigma):
        """
        Change Gaussian Sigma and clear previous fit
        :param box_name: box name (str)
        :param peak_num: peak name (int)
        :param mew_sigma: new sigma value or percentage (str)
        :return:
        """
        new_sigma = float(str(new_sigma))
        self.fixed_sigma[peak_num] = new_sigma
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
        baselines = self.info['baselines'][box_name]
        peak = self.info['moved_peaks'][box_name][peak_num]
        hist = self.info['subtracted_hists'][box_name]
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

    def getRotatedImage(self, img=None, angle=None):
        """
        Get rotated image by angle. If the input params are not specified. image = original input image, angle = self.info["rotationAngle"]
        :param img: input image
        :param angle: rotation angle
        :return: rotated image
        """
        if img is None:
            img = copy.copy(self.orig_img)
        if angle is None:
            angle = self.info['rotationAngle']
        if '90rotation' in self.info and self.info['90rotation'] is True:
            angle = angle - 90 if angle > 90 else angle + 90

        if self.rotated_img is None or self.rotated_img[0] != (self.info["centerx"], self.info["centery"]) or self.rotated_img[1] != self.info["rotationAngle"] or (self.rotated_img[2] != img).any():
            # encapsulate rotated image for using later as a list of [center, angle, original image, rotated image[
            center = (self.info["centerx"], self.info["centery"])
            if "orig_center" in self.info:
                center = self.info["orig_center"]
            else:
                self.info["orig_center"] = center

            rotImg, (self.info["centerx"], self.info["centery"]), self.rotMat = rotateImage(img, center, angle)
            self.rotated_img = [(self.info["centerx"], self.info["centery"]), angle, img, rotImg]

        return self.rotated_img[3]

    def removeInfo(self, name, k=None):
        """
        Remove information from info dictionary by k as a key. If k is None, remove all information in the dictionary
        :param name: box name
        :param k: key of dictionary
        :return: -
        """
        if k is not None and k in self.info:
            d = self.info[k]
            if isinstance(d, dict) and name in d:
                del d[name]

        if k is None:
            keys = list(self.info.keys())
            for key in keys:
                d = self.info[key]
                if key == 'box_names':
                    d.remove(name)
                else:
                    self.removeInfo(name, key)

    def loadCache(self):
        """
        Load info dict from cache. Cache file will be filename.info in folder "pt_cache"
        :return: cached info (dict)
        """
        cache_path = fullPath(self.dir_path, "pt_cache")
        cache_file = fullPath(cache_path, self.filename + '.info')
        if exists(cache_path) and isfile(cache_file):
            cinfo = pickle.load(open(cache_file, "rb"))
            if cinfo is not None:
                if cinfo['program_version'] == self.version:
                    return cinfo
                print("Cache version " + cinfo['program_version'] + " did not match with Program version " + self.version)
                print("Invalidating cache and reprocessing the image")
        return None

    def cacheInfo(self):
        """
        Save info dict to cache. Cache file will be save as filename.info in folder "pt_cache"
        :return: -
        """
        cache_path = fullPath(self.dir_path, 'pt_cache')
        createFolder(cache_path)
        cache_file = fullPath(cache_path, self.filename + '.info')

        self.info["program_version"] = self.version
        pickle.dump(self.info, open(cache_file, "wb"))


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
