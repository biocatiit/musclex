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
from os import makedirs
from os.path import isfile, exists
import numpy as np
import json
import pickle
import tifffile
from lmfit import Model, Parameters
from lmfit.models import VoigtModel, GaussianModel
from sklearn.metrics import r2_score, mean_squared_error
from pyFAI.method_registry import IntegrationMethod
from pyFAI.azimuthalIntegrator import AzimuthalIntegrator
import fabio
from musclex import __version__
try:
    from ..utils.file_manager import fullPath, getBlankImageAndMask, getMaskOnly, ifHdfReadConvertless
    from ..utils.histogram_processor import *
    from ..utils.image_processor import *
except: # for coverage
    from utils.file_manager import fullPath, getBlankImageAndMask, getMaskOnly, ifHdfReadConvertless
    from utils.histogram_processor import *
    from utils.image_processor import *
from collections import deque

import matplotlib.pyplot as plt
from scipy.signal import savgol_filter
from scipy.interpolate import BSpline, make_interp_spline

class EquatorImage:
    """
    A class for Bio-Muscle processing - go to process() to see all processing steps
    """
    def __init__(self, dir_path, filename, parent, file_list=None, extension=''):
        """
        Initial value for EquatorImage object
        :param dir_path: directory path of input image
        :param filename: image file name
        """
        self.sigmaS = 0.0001
        self.dir_path = dir_path
        self.filename = filename
        if extension in ('.hdf5', '.h5'):
            index = next((i for i, item in enumerate(file_list[0]) if item == filename), 0)
            self.orig_img = file_list[1][index]
        else:
            self.orig_img = fabio.open(fullPath(dir_path, filename)).data
        self.orig_img = ifHdfReadConvertless(self.filename, self.orig_img)
        self.orig_img = self.orig_img.astype("float32")
        self.image = None
        self.skeletalVarsNotSet = False
        self.extraPeakVarsNotSet = False

        self.fitting_error = 0.2
        
        self.quadrant_folded = False
        if filename.endswith(".tif"):
            with tifffile.TiffFile(fullPath(dir_path, filename)) as tif:
                if "ImageDescription" in tif.pages[0].tags:
                    metadata = tif.pages[0].tags["ImageDescription"].value
            if 'folded' in filename:
                self.quadrant_folded = True
                self.initialImgDim = self.orig_img.shape
            else:
                try:
                    self.quadrant_folded, self.initialImgDim = json.loads(metadata)
                except Exception:
                    print(filename, " file is not quadrant folded")

        self.rotated_img = None
        self.version = __version__
        cache = self.loadCache()
        self.rotMat = None  # store the rotation matrix used so that any point specified in current co-ordinate system can be transformed to the base (original image) co-ordinate system
        if cache is None:
            # info dictionary will save all results
            self.info = {
                "mask_thres" : 0 #getMaskThreshold(self.orig_img)
            }
        else:
            self.info = cache
        if parent is not None:
            self.parent = parent
        else:
            self.parent = self

    def process(self, settings, paramInfo=None):
        """
        All processing steps - all settings are provided by bio-muscle app as a dictionary
        settings must have ...
        nPeaks - number of peaks (int)
        model - "Voigt" or "Gaussian" (str)
        sigmac - (float)
        isSkeletal - is it skeletal muscle (boolean)
        """
        print("settings in process eqimg\n")
        print(settings)
        self.updateInfo(settings)
        self.applyBlankAndMask()
        self.findCenter()
        self.getRotationAngle()
        self.calculateRmin()
        self.getIntegrateArea()
        self.getHistogram()
        self.applyConvexhull()
        self.getPeaks()
        self.managePeaks()
        if paramInfo is not None:
            self.processParameters(paramInfo)
        else:
            self.fitModel()
        if "no_cache" not in settings:
            self.saveCache()
        self.parent.statusPrint("")

    def removeInfo(self, k=None):
        """
        Remove information from info dictionary by k as a key. If k is None, remove all information in the dictionary
        :param k: key of dictionary
        :return: -
        """
        if k is None:
            keys = list(self.info.keys())
            for key in keys:
                del self.info[key]
        else:
            if k in self.info: # remove from dictionary if the key exists
                del self.info[k]

    def updateInfo(self, settings):
        """
        Update info dict using settings
        :param settings: calibration settings
        :return: -
        """
        if settings['orientation_model'] is None:
            if 'orientation_model' not in self.info or self.info['orientation_model'] is None:
                settings['orientation_model'] = 0
            else:
                del settings['orientation_model']
        self.info.update(settings)
        if 'fixed_rmax' in self.info:
            self.info['rmax'] = self.info['fixed_rmax']
            print("R-max is fixed as " + str(self.info['rmax']))
            
    def fill_sensor_gaps_propagate(self, image, threshold):
        if isinstance(image, str):
            image = cv2.imread(image, cv2.IMREAD_GRAYSCALE)

        # Create a binary mask for sensor gaps
        mask = image <= threshold

        # Dilate the sensor gap mask using a 3x3 kernel twice
        kernel = np.ones((3, 3), np.uint8)
        dilated_mask = cv2.dilate(mask.astype(np.uint8), kernel, iterations=2)

        # Create a difference mask by subtracting the original sensor gap mask from the dilated one
        difference_mask = dilated_mask - mask

        # Multiply the original image by the difference mask
        masked_image = image * difference_mask
        dialated_masked_image = image * difference_mask

        # Convolve this masked image by a 3x3 smoothing filter but only within the difference mask
        kernel = np.ones((3, 3), np.float32) / 9 * 2
        convolved_image = cv2.filter2D(masked_image, -1, kernel)
        convolved_image = convolved_image * difference_mask

        # Prepare the output image
        output_image = np.copy(image)

        # A mask to keep track of filled areas
        filled_mask = np.zeros_like(image, dtype=bool)

        # Initialize a queue for flood fill propagation
        queue = deque()
        # Enqueue all the seed points
        seeds = np.argwhere((difference_mask == 1))

        for seed in seeds:
            queue.append((seed[0], seed[1]))
            filled_mask[seed[0], seed[1]] = True
            output_image[seed[0]-1:seed[0]+2, seed[1]-1:seed[1]+2] = convolved_image[seed[0], seed[1]]
            output_image[seed[0]-2:seed[0]+3, seed[1]-2:seed[1]+3] = convolved_image[seed[0], seed[1]]

        # Directions for the 8-connected neighborhood
        directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

        # Propagate the fill
        while queue:
            x, y = queue.popleft()

            for dx, dy in directions:
                nx, ny = x + dx, y + dy
                if 0 <= nx < image.shape[0] and 0 <= ny < image.shape[1]:
                    if not filled_mask[nx, ny] and mask[nx, ny]:
                        output_image[nx, ny] = output_image[x, y]
                        filled_mask[nx, ny] = True
                        queue.append((nx, ny))

        # Perform final smoothing
        kernel = np.ones((5, 5), np.float32) / 25
        convolved_image = cv2.filter2D(output_image, -1, kernel)
        output_image = np.where(dilated_mask, convolved_image, image)

        return np.array(output_image, dtype='float32')
            

    def applyBlankAndMask(self):
        """
        Subtract the original image with blank image and set pixels in mask below the mask threshold
        """
        img = np.array(self.orig_img, dtype='float32')
        if 'blank_mask' in self.info and self.info['blank_mask']:
            blank, mask = getBlankImageAndMask(self.dir_path)
            maskOnly = getMaskOnly(self.dir_path)
            if blank is not None:
                img = img - blank
            if mask is not None:
                # img[mask>0] = self.info['mask_thres']-1
                img = img * mask
            if maskOnly is not None:
                print("Applying mask only image")
                # img[maskOnly>0] = self.info['mask_thres']-1
                img = img * maskOnly

        self.image = img

    def findCenter(self):
        """
        Find center of the diffraction. The center will be kept in self.info["center"].
        Once the center is calculated, the rotation angle will be re-calculated, so self.info["rotationAngle"] is deleted
        """
        if self.quadrant_folded:
            self.info['center'] = self.orig_img.shape[1] / 2, self.orig_img.shape[0] / 2
            print("QF Center is " + str(self.info['center']))
            return
        self.parent.statusPrint("Finding Center...")
        print("Center is being calculated...")
        if 'center' not in self.info:
            if 'calib_center' in self.info:
                print("Using Calibration Center")
                self.info['center'] = self.info['calib_center']
                return
            if 'fillGapLines' in self.info and self.info['fillGapLines'] == True:
                self.image, self.info['center'] = processImageForIntCenter(self.image, getCenter(self.image))
            else:
                self.orig_img, self.info['center'] = processImageForIntCenter(self.orig_img, getCenter(self.orig_img))
            self.removeInfo('rotationAngle') # Remove rotationAngle from info dict to make it be re-calculated
        else:
            if self.rotMat is not None:
                center = self.info['center']
                center = np.dot(cv2.invertAffineTransform(self.rotMat), [center[0], center[1], 1])
                self.info['orig_center'] = (center[0], center[1])
        print("Done. Center is " + str(self.info['center']))

    def getRotationAngle(self):
        """
        Find rotation angle of the diffraction. Turn the diffraction equator to be horizontal. The angle will be kept in self.info["rotationAngle"]
        Once the rotation angle is calculated, the rmin will be re-calculated, so self.info["rmin"] is deleted
        """
        self.parent.statusPrint("Finding Rotation Angle...")

        if self.quadrant_folded:
            print("Quadrant folded image: ignoring rotation angle computation")
            self.info['rotationAngle'] = 0
            return

        if "fixed_angle" in self.info:
            self.info['rotationAngle'] = self.info["fixed_angle"]
            print("RotationAngle is fixed as " + str(self.info['fixed_angle']))
            return

        print("Rotation Angle is being calculated...")
        if 'rotationAngle' not in self.info:
            center = self.info['center']
            img = copy.copy(self.image)
            if 'detector' in self.info:
                self.info['rotationAngle'] = getRotationAngle(img, center, self.info['orientation_model'], man_det=self.info['detector'])
            else:
                self.info['rotationAngle'] = getRotationAngle(img, center, self.info['orientation_model'])
            self.removeInfo('rmin')  # Remove R-min from info dict to make it be re-calculated

        if "mode_angle" in self.info:
            print(f'Using mode orientation {self.info["mode_angle"]}')
            self.info['rotationAngle'] = self.info["mode_angle"]

        print("Done. Rotation Angle is " + str(self.info['rotationAngle']))

    def calculateRmin(self):
        """
        Calculate R-min of the diffraction. The R-min will be kept in self.info["rmin"]
        Once the R-min is calculated, the integrated area (Box Width) will be re-calculated, so self.info["int_area"] is deleted
        """
        self.parent.statusPrint("Calculating Rmin...")
        if 'fixed_rmin' in self.info:
            self.info['rmin'] = self.info['fixed_rmin']
            print("R-min is fixed as " + str(self.info['rmin']))
            return

        print("R-min is being calculated...")
        if 'rmin' not in self.info:
            if 'fillGapLines' in self.info and self.info['fillGapLines'] == True:
                img = copy.copy(self.image)
            else:
                img = copy.copy(self.orig_img)
            center = self.info['center']

            if 'detector' in self.info:
                det = find_detector(img, man_det=self.info['detector'])
            else:
                det = find_detector(img)

            corners = [(0, 0), (img.shape[1], 0), (0, img.shape[0]), (img.shape[1], img.shape[0])]
            npt_rad = int(round(max([distance(center, c) for c in corners])))
            ai = AzimuthalIntegrator(detector=det)
            ai.setFit2D(100, center[0], center[1])
            integration_method = IntegrationMethod.select_one_available("csr", dim=1, default="csr", degradable=True)
            _, I = ai.integrate1d(img, npt_rad, unit="r_mm", method=integration_method) # Get 1D Azimuthal integrated histogram
            self.info['rmin'] = getFirstVallay(I) # R-min is value before the first valley
            self.removeInfo('int_area')  # Remove integrated area from info dict to make it be re-calculated

        print("Done. R-min is " + str(self.info['rmin']))

    def getRotatedImage(self, img=None, angle=None):
        """
        Get rotated image by angle. If the input params are not specified. image = original input image, angle = self.info["rotationAngle"]
        :param img: input image
        :param angle: rotation angle
        :return: rotated image
        """
        if img is None:
            img = copy.copy(self.image)
        if angle is None:
            angle = self.info['rotationAngle']
        if '90rotation' in self.info and self.info['90rotation'] is True:
            angle = angle - 90 if angle > 90 else angle + 90

        if self.rotated_img is not None:
            centersNotEq = self.rotated_img[0] != self.info["center"] if type(self.rotated_img[0] != self.info["center"]) == bool else (self.rotated_img[0] != self.info["center"]).any()
        if self.rotated_img is None or centersNotEq or self.rotated_img[1] != self.info["rotationAngle"] or (self.rotated_img[2] != img).any():
            # encapsulate rotated image for using later as a list of [center, angle, original image, rotated image[

            center = self.info["center"]
            if "orig_center" in self.info:
                center = self.info["orig_center"]
                # print("orig_center",self.info['orig_center'])
            else:
                self.info["orig_center"] = center

            rotImg, self.info["center"], self.rotMat = rotateImage(img, center, angle)
            self.rotated_img = [self.info["center"], angle, img, rotImg]

        return self.rotated_img[3]

    def getIntegrateArea(self):
        """
        Calculate Integrated Area (Box width) of the diffraction. The integrated area will be start and end points in y direction for getting histogram.
        The Integrated Area will be kept in self.info["int_area"]
        Once the Integrated Area is calculated, the histograms will be re-calculated, so self.info["hist"] is deleted
        """
        self.parent.statusPrint("Calculating Integrated Area...")
        print("Integrated Area is being calculated...")
        if 'int_area' not in self.info:
            center = self.info['center']
            if 'fixed_int_area' in self.info: # integrated area is fixed by users
                self.info['int_area'] = self.info['fixed_int_area']
            else:
                rmin = self.info['rmin']
                img = getCenterRemovedImage(copy.copy(self.image), tuple(center), rmin) # remove center location

                rotate_img = self.getRotatedImage(img) # rotate image
                center = self.info["center"] #since rotation might change center
                init_range = int(round(rmin * 1.5)) # specify initial guess by using 150% or R-min
                top = max(0, int(center[1]) - init_range)
                bottom = min(int(center[1]) + init_range, rotate_img.shape[0])
                area = rotate_img[top:bottom, :]
                hist = np.sum(area, axis=1)  # Get the horizontal histogram from the initital area
                hull = convexHull(hist) # Apply convexhull

                if len(hist) > 0:
                    max_loc = np.argmax(hull)
                    r = 1
                    l = 1
                    # Find 0 on the left and right
                    while max_loc - l >= 0 and max_loc + r < len(hull) and (
                                    hull[max_loc - l] != 0 or hull[max_loc + r] != 0):
                        if hull[max_loc - l] != 0:
                            l += 1
                        if hull[max_loc + r] != 0:
                            r += 1

                    if max_loc + r < center[1] or max_loc - l > center[1] or abs(r + l) < rmin * .7:
                        self.info['int_area'] = (
                        int(round(center[1] - rmin * .7)), int(round(center[1] + rmin * .7)) + 1)
                    else:
                        center = center[1] - rmin + max_loc
                        self.info['int_area'] = (int(round(center - l * 1.2)), int(round(center + r * 1.2)) + 1)
                else:
                    # if convex hull does not work properly, integration area will be 70% of R-min
                    self.info['int_area'] = (int(round(center[1] - rmin * .7)), int(round(center[1] + rmin * .7)) + 1)

            self.removeInfo('hist') # Remove histograms from info dict to make it be re-calculated

        print("Done. Integrated Area is " + str(self.info['int_area']))

    def getHistogram(self):
        """
        Getting original histogram of the diffraction in the integrated area. Histogram will be kept in self.info["hist"]
        Once getting histogram is done, the background subtracted histogram will be re-calculated, so self.info["hulls"] is deleted
        """
        self.parent.statusPrint("Getting Histogram...")
        print("Getting Histogram...")
        if 'hist' not in self.info:
            int_area = self.info['int_area']
            img = self.getRotatedImage()
            self.info['hist'] = np.sum(img[int_area[0]:int_area[1], :], axis=0)
            self.removeInfo('hulls')  # Remove background subtracted histogram from info dict to make it be re-calculated
        
        if 'use_smooth_alg' in self.info and self.info['use_smooth_alg'] == True:
            margin = self.info['smooth_margin']
            smoothing_window = self.info['smoothing_window']

            y_filled, y_smoothed, y_interpolated, interp_x, interp_y = self.interpolate_sensor_gap(np.arange(len(self.info['hist'])), 
                                                                                                   self.info['hist'],smoothing_window=smoothing_window, 
                                                                                                   sampling_interval=10, 
                                                                                                   spline_degree=3, 
                                                                                                   margin=margin)
            

            
            # y_replaced = self.info['hist'].copy()
            # y_replaced[self.info['hist'] < 0] = y_filled[self.info['hist'] < 0]
            
            # y_replaced = self.fill_gaps(self.info['hist'], y_filled, margin=100)
            
            # plt.figure(figsize=(10, 6))
            # plt.plot(self.info['hist'], label='Original Histogram', color='red')
            # plt.plot(y_filled, label='Replaced Data', linestyle='--')
            # plt.plot(y_interpolated, label='Interpolated Data', linestyle='-.')
            # plt.xlim(400, 600)
            # plt.xlabel('Index')
            # plt.ylabel('Value')
            # plt.title('Filled Gaps in Data')
            # plt.legend()
            # plt.show()
            
            self.info['hist'] = y_filled
        
        print("Done.")

    def applyConvexhull(self):
        """
       Getting backgound subtracted histogram by applying Convex hull algorithm. Background Subtracted Histogram will be kept in self.info["hulls"]
       This will provide left, right, and both separated by centerX
       Once getting background subtracted histogram is done, the temp peaks will be re-calculated, so self.info["tmp_peaks"] is deleted
       """
        self.parent.statusPrint("Applying Convex Hull...")
        print("Applying Convexhull...")
        if 'hulls' not in self.info:
            center = self.info['center']
            shapes = self.image.shape
            if 'rmax' in self.info:
                rmax = self.info['rmax']
            else:
                rmax = int(min(center[0], center[1], shapes[1] - center[0], shapes[0] - center[1]) * 0.8)
            rmin = self.info['rmin']
            hist = copy.copy(self.info['hist'])
            int_area = self.info['int_area']
            img = self.getRotatedImage()
            # remove lines in the box width that are under the -1 value (on the gap)
            k, l = 0, 0
            while np.sum(img[int_area[0] + k, :]) <= self.info['mask_thres']:
                k += 1
            while np.sum(img[int_area[1] - l, :]) <= self.info['mask_thres']:
                l += 1
            if int_area[0] + k >= int_area[1] - l:
                # cancel it and compute hull anyway 
                k, l = 0, 0
            img_area = self.getRotatedImage()[int_area[0] + k:int_area[1] - l, :]
            # min_val = self.orig_img.min()
            # if self.img_type == "PILATUS":
            #     histo = np.histogram(self.orig_img, 3, (min_val, min_val+3))
            #     max_ind = np.argmax(histo[0])
            #     self.info['mask_thres'] = histo[1][max_ind]
            # else:
            #     self.info['mask_thres'] = min_val - 1. #getMaskThreshold(self.orig_img, self.img_type)
            if 'use_smooth_alg' not in self.info or self.info['use_smooth_alg'] == False:
                ignore = np.array([any(img_area[:, i] <= (self.info['mask_thres'])) for i in range(img_area.shape[1])])
                if any(ignore):
                    left_ignore = ignore[:int(center[0])]
                    left_ignore = left_ignore[::-1]
                    right_ignore = ignore[int(center[0]):]
                else:
                    left_ignore = None
                    right_ignore = None
                if all(ignore):
                    print('Failed to select ignored gaps: using original histogram for convexhull')
                    left_ignore = None
                    right_ignore = None
            else:
                left_ignore = None
                right_ignore = None

            left_hist = hist[:int(center[0])][::-1]
            right_hist = hist[int(center[0]):]
            right_hull = convexHull(right_hist, start_p=rmin, end_p=rmax, ignore=right_ignore) # Apply Convex hull for right histogram
            left_hull = convexHull(left_hist, start_p=rmin, end_p=rmax, ignore=left_ignore) # Apply Convex hull for left histogram

            hull = copy.copy(list(left_hull[::-1]))
            hull.extend(list(right_hull[:]))

            self.info['hulls'] = {'right': right_hull,
                                  'left': left_hull,
                                  'all': hull}

            self.info['hists'] = {'right': right_hist,
                                  'left': left_hist,
                                  'all': hist}

            self.removeInfo('tmp_peaks') # Remove temp peaks from info dict to make it be re-calculated
            
        print("Done.")
        
    def find_gaps(self, y_with_gaps, margin=3):
        # Find gap regions
        gaps = []
        n = len(y_with_gaps)
        
        if 'gaps' in self.info:
            for gap in self.info['gaps']:
                gaps.append(gap)

        # Identify indices where the signal is negative
        negative_indices = np.where(y_with_gaps < 0)[0]

        if len(negative_indices) == 0:
            return gaps

        # Initialize the first gap
        start_idx = negative_indices[0]
        end_idx = start_idx

        # Iterate over negative indices to find contiguous gaps
        for i in range(1, len(negative_indices)):
            if negative_indices[i] == negative_indices[i - 1] + 1:
                end_idx = negative_indices[i]
            else:
                # Expand the current gap using the margin
                expanded_start = max(0, start_idx - margin)
                expanded_end = min(n - 1, end_idx + margin)
                gaps.append((expanded_start, expanded_end))

                # Start a new gap
                start_idx = negative_indices[i]
                end_idx = start_idx

        # Add the last gap
        expanded_start = max(0, start_idx - margin)
        expanded_end = min(n - 1, end_idx + margin)
        gaps.append((expanded_start, expanded_end))

        return gaps
    
    def create_mask(self, gaps):
        mask = []

        for gap in gaps:
            # Add all integers from start to end (inclusive) to the mask
            mask.extend(range(gap[0], gap[1] + 1))

        return np.array(mask)
    
    def custom_smooth(self, y_with_gaps, gaps, window_length=51, polyorder=3):
        """Smooth the signal while ignoring gaps."""
        y_smoothed = np.zeros_like(y_with_gaps)
        # non_zero_indices = np.where(y_with_gaps != 0)[0]
        zero_indices = self.create_mask(gaps)
        non_zero_indices = np.setdiff1d(np.arange(len(y_with_gaps)), zero_indices)

        # Apply smoothing only on non-gap sections
        y_smoothed_non_zero = savgol_filter(y_with_gaps[non_zero_indices], window_length, polyorder)

        # Insert the smoothed values back into the full array
        y_smoothed[non_zero_indices] = y_smoothed_non_zero

        return y_smoothed

    def interpolate_sensor_gap(self, x, y_with_gaps, smoothing_window=51, smoothing_polyorder=3, sampling_interval=10, spline_degree=3, margin=0):
        """Interpolate sensor gaps using B-spline interpolation."""
        
        gaps = self.find_gaps(y_with_gaps, margin=margin)
        self.info['gaps'] = gaps
        
        gap_starts = np.array([start for start, end in gaps])
        gap_ends = np.array([end for start, end in gaps])
        
        
        # Smooth the profile, ignoring gaps
        y_smoothed = self.custom_smooth(y_with_gaps, gaps, window_length=smoothing_window, polyorder=smoothing_polyorder)

        # Find gap regions
        # gaps = np.where(y_with_gaps < 0)[0]
        # gap_start_indices = np.unique(np.hstack([np.where(np.diff(gaps) > 1)[0] + 1, [0]]))
        # gap_ends = np.unique(np.hstack([[len(gaps)-1], np.where(np.diff(gaps) > 1)[0]]))

        # gap_starts = gaps[gap_start_indices]
        # gap_ends = gaps[gap_ends]
        
        # print(gap_starts, gap_ends)
        
        # Sample points from the smoothed signal, avoiding gaps
        sample_indices = []
        for i in range(0, len(x), sampling_interval):
            if all((i < start or i > end) for start, end in zip(gap_starts, gap_ends)):
                sample_indices.append(i)

        interp_x = x[sample_indices]
        interp_y = y_smoothed[sample_indices]

        # Use B-spline interpolation on the sampled points
        bspline = make_interp_spline(interp_x, interp_y, k=spline_degree)

        # Create the fully interpolated signal based only on the B-spline
        y_interpolated = bspline(x)

        # Fill the gaps using the B-spline interpolation
        y_filled = y_with_gaps.copy()
        for start, end in zip(gap_starts, gap_ends):
            y_filled[start-margin:end + 1 + margin] = bspline(x[start - margin:end + 1 + margin])

        return y_filled, y_smoothed, y_interpolated, interp_x, interp_y
    
    def fill_gaps(self, y_with_gaps, y_filled, margin=0):
        # Convert to numpy arrays for easier manipulation
        y_with_gaps = np.array(y_with_gaps)
        y_with_gaps_copy = y_with_gaps.copy()
        y_filled = np.array(y_filled)

        # Find the indices where the gaps are (values are 0)
        gap_indices = np.where(y_with_gaps <= 0)[0]

        if len(gap_indices) == 0:
            # No gaps found, return the original y_with_gaps
            return y_with_gaps

        # Iterate through each gap index
        for idx in gap_indices:
            # Determine the start and end indices for copying with the margin
            start_idx = max(0, idx - margin)
            end_idx = min(len(y_with_gaps), idx + margin + 1)
            
            print(start_idx, end_idx)

            # Copy the region from y_filled to y_with_gaps
            y_with_gaps[start_idx:end_idx] = y_filled[start_idx:end_idx]
        
        plt.figure(figsize=(10, 6))
        plt.plot(y_with_gaps, label='Filled Data')
        plt.plot(y_with_gaps_copy, label='Original Data', linestyle='--')
        plt.plot(y_filled, label='Interpolated Data', linestyle='-.')
        plt.xlim(400, 600)
        plt.xlabel('Index')
        plt.ylabel('Value')
        plt.title('Filled Gaps in Data')
        plt.legend()
        plt.show()
        
        # Return the filled y_with_gaps

        return y_with_gaps


    # y_filled, y_smoothed, y_interpolated, interp_x, interp_y = interpolate_sensor_gap(x, y_with_gaps, sampling_interval=10, spline_degree=3)

    def getPeaks(self):
        """
        Finding Temp Peaks from background subtracted histogram. Temp peaks are just peaks which are found by algorithm.
        This might miss some peaks or detect to many peaks because of noise. These peaks will be managed again in the next step.
        Temp peaks will be kept in self.info["tmp_peaks"].
        Once getting Temp peaks is done, the real peaks will be re-calculated, so self.info["peaks"] is deleted
        """
        self.parent.statusPrint("Finding Peaks...")
        print("Finding Peaks...")
        if 'tmp_peaks' not in self.info:
            left_peaks = getPeaksFromHist(self.info['hulls']['left'])
            right_peaks = getPeaksFromHist(self.info['hulls']['right'])
            self.info['tmp_peaks'] = {'left': left_peaks, 'right': right_peaks}
            self.removeInfo('peaks')  # Remove real peaks from info dict to make it be re-calculated

        print("Done. Peaks are found : " + str(self.info['tmp_peaks']))

    def managePeaks(self):
        """
        Getting real peaks from temp peaks. Temp peaks will be considered as real peaks if it in the Hexagonal Pattern location.
        Real peaks will be kept in self.info["peaks"].
        Once getting real peaks is done, the fitting results will be re-calculated, so self.info["fit_results"] is deleted
        """
        self.parent.statusPrint("Selecting Peaks...")
        print("Model Peaks are being selected...")
        if 'peaks' not in self.info:
            left_peaks = sorted(movePeaks(self.info['hulls']['left'], sorted(self.info['tmp_peaks']['left']), 5))
            right_peaks = sorted(movePeaks(self.info['hulls']['right'], sorted(self.info['tmp_peaks']['right']), 5))
            first_left, first_right = self.findFirstSymmetricPeaks(left_peaks, right_peaks)

            self.removeInfo('fit_results')  # Remove fit results from info dict to make it be re-calculated

            if first_left is None:
                print('WARNING: '+str(self.filename) + '- no effective peaks detected. Model will not be fit.')
                return

            self.hexagonalPattern(first_left, first_right, left_peaks, right_peaks)
        print("Done. Selected Peaks are" + str(self.info['peaks']))

    def findFirstSymmetricPeaks(self, left_peaks, right_peaks):
        """
        Get first symatric peaks from left and right
        :param left_peaks: peaks on the left histogram (list)
        :param right_peaks: peaks on the right histogram (list)
        :return: first symmetric peaks, return None if there are no symmetric peaks
        """
        dist_thres = 20 # Threshold for difference between distance of left and right peaks
        for lp in left_peaks:
            for rp in right_peaks:
                if abs(lp - rp) < dist_thres:
                    return lp, rp
        return None, None

    def hexagonalPattern(self, first_left, first_right, left_peaks, right_peaks):
        """
        Set all peaks information after apply Hexagonal Pattern including ..
        - S values i.e S10, S11, S20, ...
        - Peaks
        - Missed Peaks

        :param first_left: first symmetric peak on the left (int)
        :param first_right: first symmetric peak on the right (int)
        :param left_peaks: peaks on the left histogram (list)
        :param right_peaks: peaks on the right histogram  (list)
        :return:
        """
        if "nPeaks" not in self.info:
            self.info["nPeaks"] = 2
        allpeaks = {'left': [], 'right': []}
        missed_peaks = {'left': [], 'right': []}
        allpeaks['left'].append(first_left)
        allpeaks['right'].append(first_right)
        max_loc = min(len(self.info["hulls"]['right']), len(self.info["hulls"]['left']))

        # self.info['peak'] = [symP, peak]
        S10 = int((first_left + first_right) / 2.)
        self.info['S'] = [S10]

        # Find other S values based on S10 and theta function
        SList = []
        for i in range(1, int(self.info["nPeaks"])):
            s_pos = int(np.round(S10 * theta(i)))
            if s_pos > max_loc:
                break
            SList.append(s_pos)

        maximum_nPeaks = 2
        # Find peaks correponding to S values
        for i, S in enumerate(SList):
            self.info['S'].append(S)
            if i + 2 > self.info["nPeaks"]:
                break
            SLeft = min(left_peaks, key=lambda p: abs(p - S))
            SRight = min(right_peaks, key=lambda p: abs(p - S))
            if abs(SLeft - S) > 10 or abs(SRight - S) > 10 or SLeft in allpeaks['left'] or SRight in allpeaks['right']:
                missed_peaks['left'].append(S)
                missed_peaks['right'].append(S)
                allpeaks['left'].append(S)
                allpeaks['right'].append(S)
            else:
                if SLeft not in allpeaks['left'] and SRight not in allpeaks['right']:
                    allpeaks['left'].append(SLeft)
                    allpeaks['right'].append(SRight)
            maximum_nPeaks += 2

        allpeaks['left'] = allpeaks['left'][0:int(maximum_nPeaks / 2)]
        allpeaks['right'] = allpeaks['right'][0:int(maximum_nPeaks / 2)]
        self.info['S'] = self.info['S'][0:int(maximum_nPeaks / 2)]
        # self.info['S20'] = center[0]+int(S20)

        self.info['peaks'] = allpeaks
        # self.info['nPeaks'] = maximum_nPeaks
        self.info['MissedPeaks'] = missed_peaks

    def fitModel(self):
        """
        Fit model to background subtracted histogram by using S10, peak location as initial guess
        Fit results will be kept in self.info["fit_results"].
        """
        self.parent.statusPrint("Fitting Model...")
        if 'peaks' not in self.info:
            # model cannot be fitted if peaks are not found
            return
        print("Fitting Model ...")
        if 'fit_results' not in self.info:
            left_hull = self.info['hulls']['left']
            right_hull = self.info['hulls']['right']
            left_peaks = self.info['peaks']['left']
            right_peaks = self.info['peaks']['right']

            S = self.info['S']

            left_widths = self.getPeakWidths('left')
            right_widths = self.getPeakWidths('right')

            right_height = [right_hull[right_peaks[i]] for i in range(len(S))]
            left_height = [left_hull[left_peaks[i]] for i in range(len(S))]

            # init sigma D
            sigmaD = max(left_widths)

            # initial sigma S
            sigmaS = self.sigmaS

            # init areas
            left_areas = [left_height[i] * left_widths[i] * np.sqrt(2 * np.pi) for i in range(len(left_peaks))]
            right_areas = [right_height[i] * right_widths[i] * np.sqrt(2 * np.pi) for i in range(len(right_peaks))]

            hull_hist = self.info['hulls']['all']
            x = np.arange(0, len(hull_hist))
            histNdarray = np.array(hull_hist)
            # total_area = sum(histNdarray)
            centerX = self.info['center'][0]

            margin = 10.
            S0=0

            # Add all fitting variables to params
            params = Parameters()
            params.add("centerX", centerX, min=centerX - margin, max=centerX + margin)
            params.add("S10", S[0], min=S[0] - margin, max=S[0] + margin)
            params.add("S0", S0, min=-0.001,  max=0.001)

            for (i, e) in enumerate(left_areas):
                params.add("left_area" + str(i + 1), max(e, 100), min=0)
            for (i, e) in enumerate(right_areas):
                params.add("right_area" + str(i + 1), max(e, 100), min=0)

            left_sigmac = self.info['left_fix_sigmac'] if 'left_fix_sigmac' in self.info else self.info['left_sigmac']

            # Add independent variables to int_vars
            int_vars = {
                'x': x,
                'model': self.info["model"],
                'isSkeletal': self.info['isSkeletal'],
                'isExtraPeak': self.info['isExtraPeak'],
                # 'gamma': 1.0
                'extraGaussCenter': None,
                'extraGaussSig': None,
                'extraGaussArea': None,
            }

            # Set initial parameters or independent parameters on each side
            for side in ['left', 'right']:

                # If params are fixed, add them to int_vars
                if side+'_fix_sigmac' in self.info.keys():
                    int_vars[side+'_sigmac'] = self.info[side+'_fix_sigmac']
                else:
                    params.add(side+'_sigmac', self.info[side+'_sigmac'],  min=-10., max=10)

                if side+'_fix_sigmas' in self.info.keys():
                    int_vars[side+'_sigmas'] = self.info[side+'_fix_sigmas']
                else:
                    params.add(side+'_sigmas', sigmaS,  min=-10., max=10)

                if side+'_fix_sigmad' in self.info.keys():
                    int_vars[side+'_sigmad'] = self.info[side+'_fix_sigmad']
                else:
                    params.add(side+'_sigmad', sigmaD, min=0, max=30.)

                if side+'_fix_gamma' in self.info.keys():
                    int_vars[side+'_gamma'] = self.info[side+'_fix_gamma']
                else:
                    init_gamma = np.sqrt(left_sigmac ** 2 + (sigmaD * theta(1)) ** 2 + (sigmaS * (theta(1) ** 2)) ** 2)
                    params.add(side+'_gamma', init_gamma, min=0, max=init_gamma * 5.0 + 1.)

                if self.info['isSkeletal']:
                    self.skeletalVarsNotSet = False
                    if side+'_fix_zline' in self.info:
                        int_vars[side+'_zline'] = self.info[side+'_fix_zline']
                    else:
                        init_z = 1.5
                        params.add(side+'_zline', S[0] * init_z, min=S[0] * init_z - 10, max=S[0] * init_z + 10)

                    if side+'_fix_sigz' in self.info:
                        int_vars[side+'_sigmaz'] = self.info[side+'_fix_sigz']
                    else:
                        params.add(side+'_sigmaz', 8., min=0., max=20.)

                    if side+'_fix_intz' in self.info:
                        int_vars[side+'_intz'] = self.info[side+'_fix_intz']
                    else:
                        init_intz = max(left_areas[0] / 5., right_areas[0] / 5.)
                        params.add(side+'_intz', init_intz, min=0, max=init_intz*4.+1.)

                    if side+'_fix_gammaz' in self.info:
                        int_vars[side+'_gammaz'] = self.info[side+'_fix_gammaz']
                    else:
                        params.add(side+'_gammaz', 8., min=-5., max=30.)
                    
                    if self.info['isExtraPeak']:
                        self.extraPeakVarsNotSet = False
                        if side+'_fix_zline_EP' in self.info:
                            int_vars[side+'_zline_EP'] = self.info[side+'_fix_zline_EP']
                        else:
                            init_z_ep = 2.3
                            params.add(side+'_zline_EP', S[0] * init_z_ep, min=S[0] * init_z - 10, max=S[0] * init_z_ep + 10)

                        if side+'_fix_sigz_EP' in self.info:
                            int_vars[side+'_sigmaz_EP'] = self.info[side+'_fix_sigz_EP']
                        else:
                            params.add(side+'_sigmaz_EP', 8., min=0., max=20.)

                        if side+'_fix_intz_EP' in self.info:
                            int_vars[side+'_intz_EP'] = self.info[side+'_fix_intz_EP']
                        else:
                            init_intz_ep = max(left_areas[0] / 5., right_areas[0] / 5.)
                            params.add(side+'_intz_EP', init_intz_ep, min=0, max=init_intz_ep*4.+1.)

                        if side+'_fix_gammaz_EP' in self.info:
                            int_vars[side+'_gammaz_EP'] = self.info[side+'_fix_gammaz_EP']
                        else:
                            params.add(side+'_gammaz_EP', 8., min=-5., max=30.)
                    else:
                        self.extraPeakVarsNotSet = True
                        int_vars[side+'_zline_EP'] = 0
                        int_vars[side+'_sigmaz_EP'] = 0
                        int_vars[side+'_intz_EP'] = 0
                        int_vars[side+'_gammaz_EP'] = 0

                else:
                    # set all z line variables as independent
                    self.skeletalVarsNotSet = True
                    self.extraPeakVarsNotSet = True
                    int_vars[side+'_zline'] = 0
                    int_vars[side+'_sigmaz'] = 0
                    int_vars[side+'_intz'] = 0
                    int_vars[side+'_gammaz'] = 0
                    int_vars[side+'_zline_EP'] = 0
                    int_vars[side+'_sigmaz_EP'] = 0
                    int_vars[side+'_intz_EP'] = 0
                    int_vars[side+'_gammaz_EP'] = 0

            # Bias K
            if 'fix_k' in self.info:
                int_vars['k'] = self.info['fix_k']
            else:
                params.add('k', 0., min=-1, max=max(histNdarray.max(),1.))

            # Fit model
            model = Model(cardiacFit, nan_policy='propagate', independent_vars=int_vars.keys())
            min_err = 999999999
            final_result = None

            # for method in ['leastsq', 'lbfgsb', 'powell', 'cg', 'slsqp', 'nelder', 'cobyla', 'tnc']:
            for method in ['leastsq']:
                # WARNING this fit function might give different results depending on the operating system
                result = model.fit(histNdarray, verbose = False, method=method, params=params, **int_vars)
                if result is not None:
                    res = result.values
                    res.update(int_vars)
                    err = mean_squared_error(histNdarray, cardiacFit(**res))
                    if err < min_err:
                        min_err = err
                        final_result = result

            if final_result is not None :
                fit_result = final_result.values
                fit_result.update(int_vars)
                fit_result["fiterror"] = 1. - r2_score(cardiacFit(**fit_result), histNdarray)
                del fit_result['x']
                left_areas = [fit_result['left_area' + str(i + 1)] for i in range(len(left_peaks))]
                right_areas = [fit_result['right_area' + str(i + 1)] for i in range(len(right_peaks))]

                if len(left_areas) < 2 or len(right_areas) < 2:
                    return
                #### Get Ratio between I10 and I11 ####
                fit_result['left_ratio'] = 1.*(left_areas[1] / left_areas[0])
                fit_result['right_ratio'] = 1.*(right_areas[1] / right_areas[0])
                avg_area_ratios = (fit_result['left_ratio'] + fit_result['right_ratio']) / 2.
                fit_result['avg_ratio'] = avg_area_ratios
                fit_result['left_areas'] = left_areas
                fit_result['right_areas'] = right_areas

                centerX = fit_result["centerX"]
                S10 = fit_result["S10"]
                S0 = fit_result["S0"]
                model_peaks = [centerX + S0 - S10 * theta(i) for i in range(len(left_peaks))]
                model_peaks.extend([centerX + S0 + S10 * theta(i) for i in range(len(right_peaks))])

                fit_result['model_peaks'] = sorted(model_peaks)
                all_S = [S10 * theta(i) for i in range(len(left_peaks))]
                fit_result['all_S'] = all_S

                if "lambda_sdd" in self.info.keys():
                    fit_result['d10'] = self.info["lambda_sdd"] / fit_result['S10']

                self.info['fit_results'] = fit_result
                self.saveParamInfo(params, int_vars, fit_result)

        if 'fit_results' in self.info:
            print("Done. Fitting Results : " + str(self.info['fit_results']))
            if self.info['fit_results']['fiterror'] > self.fitting_error:
                print("WARNING : High Fitting Error")
        else:
            print("Model cannot be fitted.")

    def saveParamInfo(self, params, int_vars, fit_result):
        '''
        Save information of parameter editor to the info
        :param params: this is non fixed parameters used while fitting
        :param int_vars: this is a dictionary of independent or fixed parameters used while fitting
        :param fit_result: this contains the fitted values of the parameters
        :return:
        '''
        paramInfo={}
        for p in params.keys():
            param = params[p]
            paramInfo[p] = {}
            paramInfo[p]['fixed'] = False
            paramInfo[p]['val'] = fit_result[p]
            paramInfo[p]['min'] = param.min
            paramInfo[p]['max'] = param.max

        for p in int_vars.keys():
            if p == 'x':
                continue
            paramInfo[p] = {}
            paramInfo[p]['fixed'] = True
            paramInfo[p]['val'] = int_vars[p]
            if not isinstance(int_vars[p], bool) and isinstance(int_vars[p], (float, int)):
                paramInfo[p]['min'] = int_vars[p] - 10
                paramInfo[p]['max'] = int_vars[p] + 10
        self.info['paramInfo'] = paramInfo

    def processParameters(self, paramInfo):
        '''
        Fit routine to fit parameters specified in parameter editor works differently compared to the usual fitting but uses the same underlying fitting functions
        :param paramInfo: information from parameter editor as dictionary
        :return:
        '''
        self.parent.statusPrint("Fitting Model...")
        left_peaks = self.info['peaks']['left']
        right_peaks = self.info['peaks']['right']

        hull_hist = self.info['hulls']['all']
        x = np.arange(0, len(hull_hist))
        histNdarray = np.array(hull_hist)

        params = Parameters()
        int_vars = {}

        for p in paramInfo.keys():
            pinfo = paramInfo[p]
            if pinfo['fixed']:
                int_vars[p] = pinfo['val']
            else:
                params.add(p, pinfo['val'], min=pinfo['min'], max=pinfo['max'])

        # When using previous fit to update
        S = self.info['S']

        left_hull = self.info['hulls']['left']
        right_hull = self.info['hulls']['right']

        left_widths = self.getPeakWidths('left')
        right_widths = self.getPeakWidths('right')

        right_height = [right_hull[right_peaks[i]] for i in range(len(S))]
        left_height = [left_hull[left_peaks[i]] for i in range(len(S))]

        # init areas
        left_areas = [left_height[i] * left_widths[i] * np.sqrt(2 * np.pi) for i in range(len(left_peaks))]
        right_areas = [right_height[i] * right_widths[i] * np.sqrt(2 * np.pi) for i in range(len(right_peaks))]

        for (i, e) in enumerate(left_areas):
            if "left_area" + str(i + 1) not in paramInfo:
                params.add("left_area" + str(i + 1), max(e, 100), min=0)
        for (i, e) in enumerate(right_areas):
            if "right_area" + str(i + 1) not in paramInfo:
                params.add("right_area" + str(i + 1), max(e, 100), min=0)

        if self.info['isSkeletal'] and self.skeletalVarsNotSet:
            # If zline is checked and use previous fit used, initialize the zline parameters if not set previously
            for side in ['left', 'right']:
                init_z = 1.5
                params.add(side + '_zline', S[0] * init_z, min=S[0] * init_z - 10, max=S[0] * init_z + 10)
                params.add(side + '_sigmaz', 8., min=0., max=20.)
                init_intz = max(left_areas[0] / 5., right_areas[0] / 5.)
                params.add(side + '_intz', init_intz, min=0, max=init_intz * 4. + 1.)
                params.add(side + '_gammaz', 8., min=-5., max=30.)
        if self.info['isExtraPeak'] and self.extraPeakVarsNotSet:
            # If EP is checked and use previous fit used, initialize the zline parameters if not set previously
            for side in ['left', 'right']:
                init_z_ep = 2.3
                params.add(side + '_zline_EP', S[0] * init_z_ep, min=S[0] * init_z_ep - 10, max=S[0] * init_z_ep + 10)
                params.add(side + '_sigmaz_EP', 8., min=0., max=20.)
                init_intz_ep = max(left_areas[0] / 5., right_areas[0] / 5.)
                params.add(side + '_intz_EP', init_intz_ep, min=0, max=init_intz_ep * 4. + 1.)
                params.add(side + '_gammaz_EP', 8., min=-5., max=30.)

        int_vars['x'] = x

        # Fit model
        model = Model(cardiacFit, nan_policy='propagate', independent_vars=int_vars.keys())
        min_err = 999999999
        final_result = None

        # for method in ['leastsq', 'lbfgsb', 'powell', 'cg', 'slsqp', 'nelder', 'cobyla', 'tnc']:
        for method in ['leastsq']:
            result = model.fit(histNdarray, verbose=False, method=method, params=params, **int_vars)
            if result is not None:
                res = result.values
                res.update(int_vars)
                err = mean_squared_error(histNdarray, cardiacFit(**res))
                if err < min_err:
                    min_err = err
                    final_result = result

        if final_result is not None:
            fit_result = final_result.values
            fit_result.update(int_vars)
            fit_result["fiterror"] = 1. - r2_score(cardiacFit(**fit_result), histNdarray)
            del fit_result['x']
            left_areas = [fit_result['left_area' + str(i + 1)] for i in range(len(left_peaks))]
            right_areas = [fit_result['right_area' + str(i + 1)] for i in range(len(right_peaks))]
            Speaks = [fit_result['Speak' + str(i+1)] if 'Speak' + str(i+1) in fit_result else 0 for i in range(max(len(left_peaks), len(right_peaks)))]

            if len(left_areas) < 2 or len(right_areas) < 2:
                return
            #### Get Ratio between I10 and I11 ####
            fit_result['left_ratio'] = 1. * (left_areas[1] / left_areas[0])
            fit_result['right_ratio'] = 1. * (right_areas[1] / right_areas[0])
            avg_area_ratios = (fit_result['left_ratio'] + fit_result['right_ratio']) / 2.
            fit_result['avg_ratio'] = avg_area_ratios
            fit_result['left_areas'] = left_areas
            fit_result['right_areas'] = right_areas

            centerX = fit_result["centerX"]
            S10 = fit_result["S10"]
            S0 = fit_result["S0"]
            model_peaks = [centerX + S0 - S10 * theta(i) + Speaks[i] for i in range(len(left_peaks))]
            model_peaks.extend([centerX + S0 + S10 * theta(i) + Speaks[i] for i in range(len(right_peaks))])

            fit_result['model_peaks'] = sorted(model_peaks)
            all_S = [S10 * theta(i) for i in range(len(left_peaks))]
            fit_result['all_S'] = all_S

            if "lambda_sdd" in self.info.keys():
                fit_result['d10'] = self.info["lambda_sdd"] / fit_result['S10']

            self.info['fit_results'] = fit_result
            self.saveParamInfo(params, int_vars, fit_result)
            self.saveCache()

        if 'fit_results' in self.info:
            print("Done. Fitting Results : " + str(self.info['fit_results']))
            if self.info['fit_results']['fiterror'] > self.fitting_error:
                print("WARNING : High Fitting Error")
        else:
            print("Model cannot be fitted.")

    def getPeakWidths(self, side):
        """
        Get initial peaks' widths from histogram and peaks on speicific side
        :param side: "left" or "right"
        :return: peaks' widths list
        """
        widthList = []
        peaks = self.info['peaks'][side]
        missed = self.info['MissedPeaks'][side]
        hist = self.info['hulls'][side]
        for p in peaks:
            if p in missed:
                # if peak is missing, init width as 7
                widthList.append(7)
            else:
                i, j = self.findPeakRange(p, hist)
                r = max((j - i), 5)
                w = 1. * r / 2.3548
                widthList.append(w)
        return widthList

    def findPeakRange(self, peak, hist):
        """
        Find location of Full width at half maximum of peak
        :param peak: (int)
        :param hist: (list or numpy.array)
        :return: left and right point of Full width at half maximum
        """
        i = j = peak
        midPeakHeight = hist[peak] / 2.
        thresh = np.mean(hist)
        while midPeakHeight <= hist[i] and hist[i] > thresh:
            i -= 1
        while midPeakHeight <= hist[j] and hist[j] > thresh:
            j += 1
        return i, j

    def loadCache(self):
        """
        Load info dict from cache. Cache file will be filename.info in folder "eq_cache"
        :return: cached info (dict)
        """
        cache_path = fullPath(self.dir_path, "eq_cache")
        cache_file = fullPath(cache_path, self.filename + '.info')

        if exists(cache_path) and isfile(cache_file):
            cinfo = pickle.load(open(cache_file, "rb"))
            if cinfo is not None:
                if cinfo['program_version'] == self.version:
                    return cinfo
                print("Cache version " + cinfo['program_version'] + " did not match with Program version " + self.version)
                print("Invalidating cache and reprocessing the image")
        return None

    def saveCache(self):
        """
        Save info dict to cache. Cache file will be save as filename.info in folder "eq_cache"
        :return: -
        """
        cache_path = fullPath(self.dir_path, "eq_cache")
        cache_file = fullPath(cache_path, self.filename + '.info')

        # Create cache path if it does not exist
        if not exists(cache_path):
            makedirs(cache_path)

        self.info['program_version'] = self.version
        pickle.dump(self.info, open(cache_file, "wb"))

    def delCache(self):
        """
        Delete cache
        :return: -
        """
        cache_path = fullPath(self.dir_path, "eq_cache")
        cache_file = fullPath(cache_path, self.filename + '.info')
        if exists(cache_path) and isfile(cache_file):
            os.remove(cache_file)

    def statusPrint(self, text):
        """
        Print the text in the window or in the terminal depending on if we are using GUI or headless.
        :param text: text to print
        :return: -
        """
        print(text)

def cardiacFit(x, centerX, S0, S10, model, isSkeletal, isExtraPeak, k,
                left_sigmad, left_sigmas, left_sigmac, left_gamma, left_intz, left_sigmaz, left_zline, left_gammaz,
                left_zline_EP, left_sigmaz_EP, left_intz_EP, left_gammaz_EP,
                right_sigmad, right_sigmas, right_sigmac, right_gamma, right_intz, right_sigmaz, right_zline, right_gammaz,
                right_zline_EP, right_sigmaz_EP, right_intz_EP, right_gammaz_EP,
                extraGaussCenter, extraGaussSig, extraGaussArea, **kwargs):
    """
    Using for fitting model by lmfit
    :param x: x range (list)
    :param centerX: x value of center (int)
    :param S10: (float)
    :param sigmad: (float) for each side
    :param sigmas: (float) for each side
    :param sigmac: (float) for each side
    :param model: "Voigt" or "Gaussian"
    :param gamma: use for Voigt model (float) for each side
    :param isSkeletal: (boolean)
    :param k: linear background
    :param intz: intensity of z line (float) for each side
    :param sigmaz: sigma of z line (float) for each side
    :param zline: center of z line (float) for each side
    :param kwargs: area1, area2, ..., areaN as parameters or areas as a list
    :return:
    """
    if kwargs is not None:
        if 'left_areas' in kwargs and 'right_areas' in kwargs:
            left_areas = kwargs['left_areas']
            right_areas = kwargs['right_areas']
        else:
            left_areas_dict = {}
            right_areas_dict = {}
            for kv in kwargs.items():
                key = kv[0]
                value = kv[1]
                if 'left_area' in key:
                    left_areas_dict[int(key[9:])] = value
                if 'right_area' in key:
                    right_areas_dict[int(key[10:])] = value

            left_areas = sorted(left_areas_dict.items(), key=lambda kv: kv[0])
            left_areas = [v for (_, v) in left_areas]
            right_areas = sorted(right_areas_dict.items(), key=lambda kv: kv[0])
            right_areas = [v for (_, v) in right_areas]

        speaks_dict = {}
        for k1 in range(1, max(len(left_areas), len(right_areas)) + 1):
            speaks_dict[k1] = 0
        for kv in kwargs.items():
            key = kv[0]
            value = kv[1]
            if 'Speak' in key:
                speaks_dict[int(key[5:])] = value

        Speaks = sorted(speaks_dict.items(), key=lambda kv: kv[0])
        Speaks = [v for (_, v) in Speaks]
        result = cardiacSide(model, 'left', x, centerX, S0, S10, left_sigmac, left_sigmad, left_sigmas, left_gamma, left_areas, Speaks, extraGaussCenter, extraGaussSig, extraGaussArea)
        result += cardiacSide(model, 'right', x, centerX, S0, S10, right_sigmac, right_sigmad, right_sigmas, right_gamma,
                             right_areas, Speaks, extraGaussCenter, extraGaussSig, extraGaussArea)
        if isSkeletal:
            if model == "Gaussian":
                mod = GaussianModel()
                result += mod.eval(x=x, amplitude=left_intz, center=centerX + S0 - left_zline,
                                   sigma=left_sigmaz)
                result += mod.eval(x=x, amplitude=right_intz, center=centerX + S0 + right_zline,
                                   sigma=right_sigmaz)
                if isExtraPeak:
                    result += mod.eval(x=x, amplitude=left_intz_EP, center=centerX + S0 - left_zline_EP,
                                   sigma=left_sigmaz_EP)
                    result += mod.eval(x=x, amplitude=right_intz_EP, center=centerX + S0 + right_zline_EP,
                                   sigma=right_sigmaz_EP)
            elif model == "Voigt":
                mod = VoigtModel()
                result += mod.eval(x=x, amplitude=left_intz, center=centerX + S0 + left_zline,
                                   sigma=left_sigmaz, gamma=left_gammaz)
                result += mod.eval(x=x, amplitude=right_intz, center=centerX + S0 - right_zline,
                                   sigma=right_sigmaz, gamma=right_gammaz)
                if isExtraPeak:
                    result += mod.eval(x=x, amplitude=left_intz_EP, center=centerX + S0 + left_zline_EP,
                                   sigma=left_sigmaz_EP, gamma=left_gammaz_EP)
                    result += mod.eval(x=x, amplitude=right_intz_EP, center=centerX + S0 - right_zline_EP,
                                   sigma=right_sigmaz_EP, gamma=right_gammaz_EP)
        return result + k
    return 0

def cardiacSide(model, side, x, centerX, S0, S10, sigmac, sigmad, sigmas,
                gamma, areas, Speak, extraGaussCenter, extraGaussSig, extraGaussArea):
    """
    Using for fitting model by lmfit
    """
    for (i, _) in enumerate(areas):
        if side == 'left':
            hk = i
            p = centerX + S0 - S10 * theta(hk) + Speak[i]
        else:
            hk = i
            p = centerX + S0 + S10 * theta(hk) + Speak[i]

        sigmahk = np.sqrt(sigmac ** 2 + (sigmad * theta(hk)) ** 2 + (sigmas * (theta(hk) ** 2)) ** 2)

        if model == "Gaussian":
            mod = GaussianModel()
            if i == 0:
                result = mod.eval(x=x, amplitude=areas[i], center=p, sigma=sigmahk)
            else:
                result += mod.eval(x=x, amplitude=areas[i], center=p, sigma=sigmahk)
        elif model == "Voigt":
            mod = VoigtModel()
            if i == 0:
                result = mod.eval(x=x, amplitude=areas[i], center=p, sigma=sigmahk, gamma=gamma)
            else:
                result += mod.eval(x=x, amplitude=areas[i], center=p, sigma=sigmahk, gamma=gamma)

    if extraGaussSig is not None and extraGaussCenter is not None and extraGaussCenter != 'None':
        mod = GaussianModel()
        result += mod.eval(x=x, amplitude=extraGaussArea, center=extraGaussCenter, sigma=extraGaussSig)

    return result

def cardiacFit_old(x, centerX, S10, sigmad, sigmas, sigmac, model, gamma, isSkeletal, intz, sigmaz, zline, gammaz, **kwargs):
    """
    Using for fitting model by lmfit
    :param x: x range (list)
    :param centerX: x value of center (int)
    :param S10: (float)
    :param sigmad: (float)
    :param sigmas: (float)
    :param sigmac: (float)
    :param model: "Voigt" or "Gaussian"
    :param gamma: use for Voigt model (float)
    :param isSkeletal: (boolean)
    :param intz: intensity of z line
    :param sigmaz: sigma of z line
    :param zline: center of z line
    :param kwargs: area1, area2, ..., areaN as parameters or areas as a list
    :return:
    """
    if kwargs is not None:
        result = None
        if 'areas' in kwargs:
            areas = kwargs['areas']
        else:
            areas_dict = {}
            for kv in kwargs.items():
                if 'area' in kv[0]:
                    areas_dict[int(kv[0][4:])] = kv[1]

            areas = sorted(areas_dict.items(), key=lambda kv: kv[0])
            areas = [v for (k, v) in areas]

        nPeaks = int(len(areas)/2)

        for (i, _) in enumerate(areas):
            if i < nPeaks:
                hk = (nPeaks) - i - 1
                p = centerX - S10 * theta(hk)
            else:
                hk = i - (nPeaks)
                p = centerX + S10 * theta(hk)

            sigmahk = np.sqrt(sigmac ** 2 + (sigmad * theta(hk)) ** 2 + (sigmas * (theta(hk) ** 2)) ** 2)

            if model == "Gaussian":
                mod = GaussianModel()
                if i == 0:
                    result = mod.eval(x=x, amplitude=areas[i], center=p, sigma=sigmahk)
                else:
                    result += mod.eval(x=x, amplitude=areas[i], center=p, sigma=sigmahk)

            elif model == "Voigt":
                mod = VoigtModel()
                if i == 0:
                    result = mod.eval(x=x, amplitude=areas[i], center=p, sigma=sigmahk, gamma=gamma)
                else:
                    result += mod.eval(x=x, amplitude=areas[i], center=p, sigma=sigmahk, gamma=gamma)

        if isSkeletal:
            if model == "Gaussian":
                mod = GaussianModel()
                result += mod.eval(x=x, amplitude=intz, center=centerX + zline,
                                   sigma=sigmaz)
                result += mod.eval(x=x, amplitude=intz, center=centerX - zline,
                                   sigma=sigmaz)
            elif model == "Voigt":
                mod = VoigtModel()
                result += mod.eval(x=x, amplitude=intz, center=centerX + zline,
                                   sigma=sigmaz, gamma=gammaz)
                result += mod.eval(x=x, amplitude=intz, center=centerX - zline,
                                   sigma=sigmaz, gamma=gammaz)
        return result
    return 0

def getCardiacGraph(x, fit_results):
    """
    Give the cardiac graph based on the fit results.
    :param fit_results: fit results
    """
    plot_params = {
        'centerX': fit_results['centerX'],
        'S0': fit_results["S0"],
        'S10': fit_results['S10'],
        'model': fit_results['model'],
        'isSkeletal': fit_results['isSkeletal'],
        'isExtraPeak': fit_results['isExtraPeak'],
        'left_areas': fit_results['left_areas'],
        'right_areas': fit_results['right_areas'],

        'left_sigmac': fit_results['left_sigmac'],
        'left_sigmad': fit_results['left_sigmad'],
        'left_sigmas': fit_results['left_sigmas'],
        'left_gamma': fit_results['left_gamma'],
        'left_zline': fit_results['left_zline'],
        'left_sigmaz': fit_results['left_sigmaz'],
        'left_intz': fit_results['left_intz'],
        'left_gammaz': fit_results['left_gammaz'],
        'left_zline_EP': fit_results['left_zline_EP'],
        'left_sigmaz_EP': fit_results['left_sigmaz_EP'],
        'left_intz_EP': fit_results['left_intz_EP'],
        'left_gammaz_EP': fit_results['left_gammaz_EP'],

        'right_sigmac': fit_results['right_sigmac'],
        'right_sigmad': fit_results['right_sigmad'],
        'right_sigmas': fit_results['right_sigmas'],
        'right_gamma': fit_results['right_gamma'],
        'right_zline': fit_results['right_zline'],
        'right_sigmaz': fit_results['right_sigmaz'],
        'right_intz': fit_results['right_intz'],
        'right_gammaz': fit_results['right_gammaz'],
        'right_zline_EP': fit_results['right_zline_EP'],
        'right_sigmaz_EP': fit_results['right_sigmaz_EP'],
        'right_intz_EP': fit_results['right_intz_EP'],
        'right_gammaz_EP': fit_results['right_gammaz_EP'],

        'k': fit_results['k'],
        'extraGaussCenter': fit_results['extraGaussCenter'],
        'extraGaussSig': fit_results['extraGaussSig'],
        'extraGaussArea': fit_results['extraGaussArea'],
    }
    if 'Speaks' in fit_results:
        plot_params['Speaks'] = fit_results['Speaks']
    return cardiacFit(x=x, **plot_params)

def theta(h, k=-1):
    """
    Theta value that used for distance calculation. S10 * theta
    :param h: index 1
    :param k: index 2, if k is not specified, h will be index count by peak from center
    :return: theta value
    """
    if k == -1:
        # add more coords if necessary
        SList = [(1, 0), (1, 1), (2, 0), (2, 1), (3, 0), (2, 2), (3, 1), (4, 0), (3, 2), (4, 1),
                 (5, 0), (3, 3), (4, 2), (5, 1), (6, 0), (4, 3), (5, 2), (6, 1), (4, 4), (5, 3),
                 (7, 0), (6, 2), (7, 1), (5, 4), (6, 3), (8, 0), (7, 2), (8, 1), (5, 5), (6, 4),
                 (7, 3), (8, 2), (6, 5), (7, 4), (8, 3), (6, 6), (7, 5), (8, 4), (7, 6), (8, 5),
                 (7, 7), (8, 6), (8, 7), (8, 8)]

        hk = SList[h]
        return theta(hk[0], hk[1])
    return np.sqrt(float(h ** 2 + k ** 2 + h * k))
