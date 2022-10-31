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
import math
import time
from os.path import exists
import collections
import pickle
import numpy as np
import fabio
from lmfit import Model, Parameters
from lmfit.models import GaussianModel
from scipy.integrate import simps
from sklearn.metrics import r2_score
from pyFAI.azimuthalIntegrator import AzimuthalIntegrator
import musclex
from ..utils.file_manager import fullPath, createFolder, getBlankImageAndMask, ifHdfReadConvertless
from ..utils.histogram_processor import *
from ..utils.image_processor import *

class ScanningDiffraction:
    """
    A class for Scanning Diffraction processing - go to process() to see all processing steps
    """
    def __init__(self, filepath, filename, file_list=None, extension='', logger = None):
        if extension in ('.hdf5', '.h5'):
            index = next((i for i, item in enumerate(file_list[0]) if item == filename), 0)
            original_image = file_list[1][index]
        else:
            original_image = fabio.open(fullPath(filepath, filename)).data
        original_image = ifHdfReadConvertless(filename, original_image)
        self.original_image = original_image.astype("float32")
        if self.original_image.shape == (1043, 981):
            self.img_type = "PILATUS"
        else:
            self.img_type = "NORMAL"
        self.filepath = filepath
        self.filename = filename
        self.logger = logger
        self.version = musclex.__version__
        self.noBGImg = getImgAfterWhiteTopHat(self.original_image)
        self.info = self.loadCache()

    def loadCache(self):
        """
        Load the cache and return it if it exists, else return an empty dict.
        :return: cache info
        """
        cache_path = fullPath(self.filepath, "di_cache")
        cache_file = fullPath(cache_path, self.filename+'.info')
        if exists(cache_file):
            info = pickle.load(open(cache_file, "rb"))
            if info is not None:
                return info
        return {}

    def cacheInfo(self):
        """
        Save the cache info in a file named di_cache to help execute the prrgram
        faster the next time a same image is processed.
        """
        cache_path = fullPath(self.filepath, 'di_cache')
        createFolder(cache_path)
        cache_file = fullPath(cache_path, self.filename + '.info')
        self.info['program_version'] = self.version
        pickle.dump(self.info, open(cache_file, "wb"))

    def log(self, msg):
        """
        print and log msg to file
        :param msg: logged msg
        :return:
        """
        print(str(msg))
        if self.logger is not None:
            self.logger.debug(msg)

    def process(self, flags={}):
        """
        All processing steps
        """
        self.updateInfo(flags)
        self.log("----------------------------------------------------------------------")
        self.log(fullPath(self.filepath, self.filename)+" is being processed ...")
        self.findCenter()
        self.get2DIntegrations()
        self.processPartialIntegrationMethod()
        self.processCentralDiffMethod()
        self.mergeRings()

        if len(self.info['merged_peaks']) > 0:
            self.fitModel()
            self.processRings()
        else:
            self.log("WARNING : no effective rings detected.")
            self.removeInfo('fitResult')
            self.removeInfo('ring_hists')
            self.removeInfo('average_ring_model')
            self.removeInfo('ring_models')
            self.removeInfo('ring_errors')

        self.log(fullPath(self.filepath, self.filename) + " has been processed.")
        self.cacheInfo()

    def removeInfo(self, k = None):
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

    def updateInfo(self, flags):
        """
        Update info dict using flags
        :param flags: flags
        :return: -
        """
        depn_lists = {
            '2dintegration': ['fixed_hull', 'merged_peaks'],
            'ring_hists': ['ROI', 'orientation_model', '90rotation', 'merged_peaks'],
            'fitResult' : ['m1_rings', 'm2_rings', 'merged_peaks'],
            'model_peaks' : ['m1_rings', 'm2_rings','merged_peaks'],
            'm2_runs_dict' : ['m2_rings'],
            'm2_central_difference' : ['m2_rings'],
            'merged_rings' : ['m2_rings'],
            'central_log' : ['m2_rings'],
            'partial_ranges' : ['m1_rings'],
            'm1_partial_hist' : ['m1_rings'],
            'm1_partial_hulls' : ['m1_rings'],
            'm1_partial_peaks' : ['m1_rings']
        }
        for key, params in depn_lists.items():
            for flag in params:
                if flag in flags and (flag not in self.info or flags[flag] != self.info[flag]):
                    self.removeInfo(key)
                    break
        self.info.update(flags)

    def findCenter(self):
        """
        Get center of the diffraction
        :return:
        """
        if 'center' not in self.info.keys():
            img = get8bitImage(copy.copy(self.original_image))
            self.original_image, self.info['center'] = processImageForIntCenter(img, getCenter(img), self.img_type)
            self.log("Center has been calculated. center is "+str(self.info['center']))

    def get2DIntegrations(self):
        """
        Get 2D integrations and azimuthal histrogram
        :return:
        """
        if '2dintegration' not in self.info.keys():
            blank, mask = getBlankImageAndMask(self.filepath)
            img = copy.copy(self.original_image)
            noBGImg = copy.copy(self.noBGImg)
            if blank is not None:
                img = img - blank
                noBGImg = getImgAfterWhiteTopHat(img)

            center = self.info['center']

            if img.shape == (1043, 981):
                det = "pilatus1m"  # Sensor used for diffraction_mapping_bone is pilatus1m
                # rmax = max(img.shape[0] - center[1], img.shape[1] - center[0], center[0], center[1])
                rmax = max(img.shape[0] / 2, img.shape[1] / 2)
                min_endpoint = int(round(min(img.shape[0] - center[1], img.shape[1] - center[0], center[0], center[1]) *.75))
                # img = self.getFilledImage(img)
            else:
                det = "agilent_titan"  # This detector has the same size (2048,2048)
                rmax = int(round(min(img.shape[0] - center[1], img.shape[1] - center[0], center[0], center[1]) *.75))
                min_endpoint = rmax

            corners = [(0, 0), (img.shape[1], 0), (0, img.shape[0]), (img.shape[1], img.shape[0])]
            npt_rad = int(round(max([distance(center, c) for c in corners])))
            ai = AzimuthalIntegrator(detector=det)
            ai.setFit2D(100, center[0], center[1])

            I2D, tth, chi = ai.integrate2d(copy.copy(self.original_image), npt_rad, 360, unit="r_mm", method="csr", mask=mask)
            I2D2, tth2, chi2 = ai.integrate2d(noBGImg, npt_rad, 360, unit="r_mm", method="csr", mask=mask)

            _, I = ai.integrate1d(copy.copy(self.original_image), npt_rad, unit="r_mm", method="csr", mask=mask)
            _, I2 = ai.integrate1d(img, npt_rad, unit="r_mm", method="csr", mask=mask)

            self.info['2dintegration'] = [I2D, tth, chi]
            self.info['tophat_2dintegration'] = [I2D2, tth2, chi2]

            hists = list(I)

            if 'fixed_hull' in self.info:
                rmin = self.info['fixed_hull'][0]
                rmax = self.info['fixed_hull'][1]
            else:
                rmin = getFirstVallay(hists)

            hists = list(I2)
            self.info['rmax'] = rmax
            self.info['min_endpoint'] = min_endpoint
            hull = self.getConvexhull(np.array(hists))
            self.info['orig_hists'] = hists
            self.info['start_point'] = rmin
            self.info['hull_hist'] = hull
            self.info['area'] = simps(hull)
            self.info['simple_total_intensity'] = self.roiIntensity(center, rmin, rmax)
            if 'ROI' not in self.info:
                self.info['ROI'] = [rmin, rmax]
            if 'persist_rings' not in self.info:
                self.removeInfo('m1_rings')
                self.removeInfo('m2_rings')
            self.log('2D integration has been calculated.')

    def roiIntensity(self, center, rmin, rmax):
        """
        Simply add up all intensity together within ROI.
        """
        img = copy.copy(self.original_image)
        h, w = img.shape
        xc, yc = np.meshgrid(range(w), range(h))
        eucld = ((xc - center[0]) ** 2 + (yc - center[1]) ** 2) ** 0.5
        return sum(img[(rmin <= eucld) & (eucld < rmax)])

    def processPartialIntegrationMethod(self): #### Method 1 ####
        """
        Process image by 1st mether : Partial Integration method
        Get radial histograms from each degree range, find peaks from each range, then merge them
        :return:
        """
        if 'm1_rings' not in self.info.keys():
            self.log("=== Partial Integration Method is being processed...")
            # Method 1: multiple conical integrations
            ref_angle = 90
            if 'partial_angle' in self.info.keys():
                ref_angle = self.info['partial_angle']

            ranges, histograms = self.get_partial_integrations(ref_angle)
            self.info['partial_ranges'] = ranges
            self.info['m1_partial_hists'] = histograms

            hulls = []
            all_peaks = []
            partial_peaks = []

            for h in histograms:
                hull = self.getConvexhull(np.array(h))
                peaks = self.findPeaksFromHist(hull)
                all_peaks.extend(peaks)
                partial_peaks.append(peaks)
                hulls.append(hull)

            self.info['m1_partial_hulls'] = hulls
            self.info['m1_partial_peaks'] = partial_peaks

            histnp = np.array(self.info['hull_hist'])
            indexes = self.select_peaks(sorted(all_peaks), times_threshold=2, distance_threshold=15)

            all_peaks_list = sorted(indexes.keys())
            peakList1 = sorted([p for p in indexes.keys() if indexes[p] >= len(ranges)/4])
            peakList2 = sorted([p for p in indexes.keys() if indexes[p] < len(ranges)/4])

            if len(peakList2) > 0:
                sigmaList = self.getInitSigma(histnp, all_peaks_list)
                normSigma = sigmaList / np.max(sigmaList)
                normHeight = [histnp[p]/ np.max(histnp) for p in all_peaks_list]
                areas = [normHeight[i] * normSigma[i] for i in range(len(normHeight))]
                normAreas = areas/np.max(areas)

                peakList2 = sorted([peakList2[i] for i in range(len(peakList2)) if normAreas[all_peaks_list.index(peakList2[i])] > 0.2])
                peakList1.extend(peakList2)

            all_peaks_list = sorted(peakList1)

            self.log("Selected peaks = " + str(all_peaks_list))
            self.info['m1_rings'] = all_peaks_list
            self.removeInfo('merged_peaks')

    def processCentralDiffMethod(self): #### Method 2 ####
        """
        Process image by 2nd method : Central difference Method
        Get 2D histogram, find the log of central difference, then find the straight line for rings
        :return:
        """
        if 'm2_rings' not in self.info.keys():
            self.log("=== Central Difference Method is being processed...")
            I2D = self.info['tophat_2dintegration'][0]
            h = 10  # Displacement of columns for difference
            central_difference = self.get_central_difference(I2D, h)
            central_difference = central_difference.clip(0, central_difference.max()) + 0.1
            central_log = np.log(central_difference)
            self.info['central_log'] = central_log

            dict_runs = self.get_runs_from_image(central_log)

            selected_rings = self.group_runs_by_ring(dict_runs)
            selected_rings = {k: v for k, v in selected_rings.items() if v[1] - v[0] >= 5}
            self.log("Selected peaks = "+ str(sorted(selected_rings.keys())))

            self.info['m2_runs_dict'] = dict_runs
            self.info['m2_central_difference'] = central_difference
            self.info['m2_rings'] = selected_rings
            self.removeInfo('merged_rings')

    def mergeRings(self):
        """
        Merge all rings together. If multiple rings have similar location, count them as one and use the average location
        :return:
        """
        if 'merged_peaks' not in self.info.keys():
            self.log("=== Merging rings ...")
            m1_rings = copy.copy(self.info['m1_rings'])
            m2_rings = copy.copy(self.info['m2_rings'])
            all_peaks_list = m1_rings
            all_peaks_list.extend(m2_rings.keys())
            all_peaks_list = self.select_peaks(all_peaks_list, distance_threshold = 15)
            merged_peaks = sorted([p for p in all_peaks_list.keys() if self.info['start_point'] < p and p <= self.info['rmax']])
            self.log("Merged rings = "+str(merged_peaks))
            self.info['merged_peaks'] = merged_peaks
            self.removeInfo('fitResult')

    def getInitSigma(self, hist, peaks):
        """
        Get Initial Sigmas of all peaks by using half-maximum height width
        :param hist: input histogram
        :param peaks: list of peaks
        :return:
        """
        sigmaList = []
        for p in peaks:
            peak_height = hist[p]
            r = 1
            l = 1
            while p-l > 0 and p+r < len(hist) and (hist[p-l] > peak_height/2. or hist[p+r] > peak_height/2.) :
                if hist[p-l] > peak_height/2.:
                    l += 1
                if hist[p+r] > peak_height/2.:
                    r += 1
            w = r+l
            sigmaList.append(w*2./2.35482)

        return sigmaList

    def fitModel(self):
        """
        Fit model to azimuthal integration
        """
        if 'fitResult' not in self.info.keys():
            self.log("=== Fitting model ...")
            self.removeInfo('ring_hists')
            self.removeInfo('average_ring_model')
            self.removeInfo('ring_models')
            self.removeInfo('ring_errors')
            merged_peaks = self.info['merged_peaks']
            hists_np = np.array(self.info['hull_hist'])
            sigmaList = self.getInitSigma(hists_np, merged_peaks)

            # Fit several times and take the minimum error result
            temp_model = []
            temp_error = []
            # methods = ['leastsq', 'lbfgsb', 'cg', 'tnc', 'slsqp']
            methods = ['leastsq']

            for (i, _) in enumerate(methods):
                start = time.time()
                t_m, error = fitGMMv2(hists_np, merged_peaks, sigmaList, methods[i])
                temp_model.append(t_m)
                self.log("Fitting Results = " + str(t_m))
                self.log("Error for method "+ str(methods[i])+ " = "+ str(error))
                self.log("Time for method "+ str(methods[i])+ " = "+str(time.time() - start))
                temp_error.append(error)
                if error < 0.05:
                    break

            best_ind = np.argmin(temp_error)
            result = temp_model[best_ind]
            self.log("Selected Fitting Method = "+str(methods[best_ind]))

            # Check if peaks are guessed right
            model_peaks = [result['u' + str(i)] for i in range(1, len(merged_peaks) + 1)]
            more_info = {'model_peaks': model_peaks, 'fitResult': result, 'minimize_method': methods[best_ind]}
            self.info.update(more_info)

        if 'lambda_sdd' in self.info.keys() and 'model_peaks' in self.info.keys() and len(self.info['model_peaks']) > 0:
            self.info['peak_ds'] = [(1.0 / p) * self.info['lambda_sdd'] for p in self.info['model_peaks']]

    def processRings(self):
        """
        Process rings to get informations of rings i.e. rotation, intensity
        :return:
        """
        if 'ring_hists' not in self.info.keys():
            self.log("=== Rings information is being processed ...")
            if 'orientation_model' not in self.info:
                self.info['orientation_model'] = "GMM3"
            if self.info['orientation_model'] == "Max Intensity":
                self.maxIntensityOrientation()
            elif self.info['orientation_model'] == "GMM2":
                self.processOrientation2()
            elif self.info['orientation_model'] == "GMM3":
                self.processOrientation2(model='GMM3')
            elif self.info['orientation_model'] == "Herman Factor (Half Pi)":
                self.HermanOrientation(ir='h')
            elif self.info['orientation_model'] == "Herman Factor (Pi)":
                self.HermanOrientation()

            if 'average_ring_model' in self.info:
                if '90rotation' in self.info and self.info['90rotation']:
                    self.info['average_ring_model']['u'] += np.pi/2

    def get_partial_integrations(self, ref_angle):
        """
        Get azimuthal integration by angle range
        :param ref_angle: angle range i.e. (30,60)
        :return:
        """
        histograms = []
        # Generate the angle ranges in tuples
        ranges = [(x, x + ref_angle) for x in range(0, 360, ref_angle)]
        ranges.extend([(x, x + ref_angle) for x in range(int(ref_angle / 2), 360 + int(ref_angle / 2), ref_angle)])
        ranges = sorted(ranges, key=lambda se: se[0])

        blank, mask = getBlankImageAndMask(self.filepath)
        img = copy.copy(self.original_image)
        if blank is not None:
            img = img - blank

        if img.shape == (1043, 981):
            det = "pilatus1m"  # This detector has the size (1043, 981)
        else:
            det = "agilent_titan"

        center = self.info['center']
        corners = [(0, 0), (img.shape[1], 0), (0, img.shape[0]), (img.shape[1], img.shape[0])]
        npt_rad = int(round(max([distance(center, c) for c in corners])))
        ai = AzimuthalIntegrator(detector=det)
        ai.setFit2D(100, center[0], center[1])

        # Compute histograms for each range
        for a_range in ranges:
            _, I = ai.integrate1d(img, npt_rad, unit="r_mm", method="csr", azimuth_range=a_range, mask=mask)
            histograms.append(I)

        return ranges, histograms

    def get_central_difference(self, I2D, h):
        """
        Compute central difference
        """
        central = I2D * 2
        right_shift = -1 * np.roll(I2D, -h, axis=1)
        left_shift = -1 * np.roll(I2D, h, axis=1)
        result_r = np.add(central, right_shift)
        return np.add(result_r, left_shift)

    def getConvexhull(self, hist):
        """
        Get backgrouns subtracted histogram by applying convex hull
        :param hist:
        :param rmax:
        :return:
        """
        if 'fixed_hull' in self.info:
            start = self.info['fixed_hull'][0]
            end = self.info['fixed_hull'][1]
            return convexHull(hist, start_p=start, end_p=end)
        else:
            shist = smooth(hist, 30)
            rmax = self.info['rmax']
            start = getFirstVallay(list(hist))
            end = len(hist)
            for i in range(len(shist)-2, 0, -1):
                if shist[i+1]-shist[i] > 0:
                    end = int(round(i*0.8))
                    break
            if end-start < 100:
                end = self.info['min_endpoint']
            return convexHull(hist, start_p=start, end_p=min(end, rmax))

    def get_runs_from_image(self, central_difference):
        """
        Get runs for 2nd method
        :param central_difference:
        :return:
        """
        runs = collections.defaultdict(list)
        lenght_threshold = 60
        value_threshold = np.median(central_difference, axis=0)
        step = 10
        for i in range(step, len(value_threshold) - step):
            value_threshold[i] = np.mean(value_threshold[i - step:i + step])
        value_threshold = [1 if v < 1 else v for v in value_threshold]
        distance_threshold = 8

        for c in range(central_difference.shape[1]):
            run = []
            last_pos = 0
            for r in range(central_difference.shape[0]):
                if central_difference[r][c] > value_threshold[c]:
                    if r - last_pos > distance_threshold:
                        if len(run) > lenght_threshold:
                            line = [run[0], run[len(run) - 1]]
                            runs[c].append(line)
                        run = []

                    last_pos = r
                    run.append((r, c))
            if len(run) > lenght_threshold:
                line = [run[0], run[len(run) - 1]]
                runs[c].append(line)
        return runs

    def group_runs_by_ring(self, runs):
        """
        Find rings from input runs
        :param runs:
        :return:
        """
        result_rings = {}
        distance_threshold = 1
        i = 0
        while i < self.original_image.shape[1]:
            if i in runs:
                dist = 0
                start = i
                end = i
                for c in range(1, self.original_image.shape[1] - i):

                    if i + c in runs:
                        end = i + c
                        dist = 0
                    else:
                        dist += 1

                    if dist > distance_threshold:
                        result_rings[(end + start) / 2] = (start, end)
                        i += c
                        break
            i += 1

        return result_rings

    def findPeaksFromHist(self, orig_hist, min_dist = 30):
        """
        Get all peaks from histogram
        :param orig_hist: input histogram
        :param min_dist: if distance between two peaks is less than min_dist
        :return:
        """
        peak_list = getPeaksFromHist(orig_hist, width_thres=10)
        peak_list = movePeaks(orig_hist, peak_list)
        peak_list2 = self.select_peaks(peak_list, times_threshold=1, distance_threshold=min_dist)
        return sorted(peak_list2)

    def select_peaks(self, peaks, times_threshold = 1, distance_threshold = 10, round_val = True):
        """
        Select peaks from list of peaks . Peaks with similar location will be merge into one as average location
        :param peaks: list of peaks
        :param times_threshold: peak which its number of appears less than times_threshold will be ignored
        :param distance_threshold: minimum distance between peaks
        :return:
        """
        aux_results = {}
        results = {}

        # Create dictionary for peaks with value equal to number of matches
        for peak in peaks:
            aux_results[peak] = aux_results.setdefault(peak, 0) + 1

        # Group by same peak using distance_threshold
        l_peaks = sorted(aux_results.keys())

        prev = 0
        for (i, _) in enumerate(l_peaks):
            aux_freq = aux_results[l_peaks[i]]
            if l_peaks[i] - prev < distance_threshold:
                freq = 0
                if prev in results:
                    freq = results[prev]
                    results.pop(prev, None)

                value = 1.*(prev*freq+l_peaks[i]*aux_freq)/(freq+aux_freq)
                results[value] = aux_freq + freq
                prev = value
            else:
                results[l_peaks[i]] = aux_freq
                prev = l_peaks[i]

        if round_val:
            return {int(round(k)): v for k, v in results.items() if v >= times_threshold}
        else:
            return {k: v for k, v in results.items() if v >= times_threshold}

    def getDspacing(self, peaks):
        """
        Find D-spacing from peaks
        :param peaks: list
        :return:
        """
        if len(peaks) == 1:
            return peaks[0]

        # Number of hypothesis
        h = 4
        min_di = 1
        errors = []

        for i in range(1, h + 1):
            d = peaks[0] / i
            error = 0.
            for p in peaks:
                mult = 1. * p / d
                error += abs((p-d*round(mult)))/d
            print("distance: " + str(d) + " error: " +str(error))
            errors.append(error)
            if error < errors[min_di-1]:
                min_di = i

        return 1. * peaks[0] / min_di

    def get_closer_peak(self, model, num_peaks, p, dist):
        """
        Get the peak that close to p by distance
        :param model: model containing all peaks
        :param num_peaks: number of peaks
        :param p: peak location
        :param dist: minimum distance
        :return:
        """
        for i in range(1, num_peaks + 1):
            if abs(model['u' + str(i)] - p) < dist:
                return model['u' + str(i)], model['sigmad' + str(i)]
        return -1, -1

    def get_missed_rings_by_distance(self, num_peaks, dist, limit):
        """
        Get missed ring by using d-spacing
        :param num_peaks: number of peaks
        :param dist: d-spacing
        :param limit: limit number of rings
        :return:
        """
        distance_threshold = dist / 3
        peaks = []
        sigmas = []
        typ = []
        sigma_default = 3
        model_result = self.info['fitResult']

        # Limit rings to inside image
        image_lenght = len(self.info['orig_hists']) - self.info['start_point']

        if limit * dist > image_lenght:
            limit = int(image_lenght/dist)

        if limit < 10:
            limit = 0
            for limit in range(1, 10):
                if limit * dist > image_lenght:
                    break

        limit = min(10 ,limit)

        for i in range(1, limit):
            location = dist * i
            p_found, sigma_found = self.get_closer_peak(model_result, num_peaks, location, distance_threshold)
            if p_found == -1:
                # Include non-existing peak
                peaks.append(location)
                sigmas.append(sigma_default)
                typ.append({'non-existing': -1})
            else:
                peaks.append(p_found)
                sigmas.append(sigma_found)
                typ.append({'existing': 0})

        return peaks, sigmas, typ

    def get_ring_angle_range(self, ring_radius, img_center, shape):
        """
        Get ring angle range
        :param ring_radius: radius of ring
        :param img_center: image center
        :param shape: image shape
        :return:
        """
        angle_1 = 0
        angle_2 = 0
        x = 1
        y = 1
        ref_x = min(img_center[0], shape[0] - img_center[0])
        ref_y = min(img_center[1], shape[1] - img_center[1])
        if ref_x < img_center[0]:
            x = -1
        if ref_y < img_center[1]:
            y = -1

        if ring_radius > ref_x:
            angle_1 = np.arccos(x * ref_x / ring_radius)
        if ring_radius > ref_y:
            angle_2 = y * np.arcsin(ref_y / ring_radius)
        ring_range = abs(convertRadtoDegrees(angle_1) - convertRadtoDegrees(angle_2))
        return ring_range

    def removeValleys2(self, deep_hist, orig_hist):
        """
        Remove valley from ring hist (pilatus line)
        :param hists: radial histogram
        :return:
        """
        end_points = []
        start_points = []

        deep_hist = copy.copy(deep_hist)
        orig_hist = copy.copy(orig_hist)
        # deep_smooth_hist = smooth(deep_hist, 5) + 0.0000000001 # Prevent divide by zero
        i = 1

        while i < len(orig_hist)-1:
            # Find start point of a valley by its slope
            if deep_hist[i] < 0:

                # Move left for 2 to start before valley
                start_ind = max(0, i-2)

                # Pass valley
                while i < len(deep_hist)-1 and deep_hist[i] < 0:
                    i = i + 1

                # Move right for 1 to end after valley
                end_ind = min(i+2, len(orig_hist)-1)
                i = end_ind

                if start_ind == 0 or end_ind - start_ind < 2:
                    sub = orig_hist[end_ind]
                elif end_ind == len(orig_hist)-1:
                    sub = orig_hist[start_ind]
                else:
                    xs = np.linspace(0, end_ind - start_ind + 1, end_ind - start_ind + 1)
                    val_s = orig_hist[start_ind]
                    val_e = orig_hist[end_ind]
                    m = (val_e-val_s)/len(xs)
                    sub = np.array([m*x+val_s for x in xs])

                orig_hist[start_ind: end_ind+1] = sub
                start_points.append(start_ind)
                end_points.append(end_ind)

            i = i + 1

        return orig_hist


    def removeValleys(self, hists):
        """
        Remove valley from ring hist (pilatus line)
        :param hists: radial histogram
        :return:
        """
        hist2 = copy.copy(hists)
        smooth_hist = smooth(hist2)
        thres = np.mean(hists) * 0.8
        i = 1

        while i < len(hist2):
            if hist2[i] < thres:
                start_ind = i - 1
                while i < len(hist2) and hist2[i] < thres:
                    i = i + 1

                while i < len(hist2) and ((1.*(hist2[start_ind] - hist2[i])/hist2[start_ind] > 0.1) or smooth_hist[i] - smooth_hist[i - 1] > 20. ):
                    i = i + 1

                start_ind = max(0, start_ind - 3)
                end_ind = min(i, len(hist2) - 1)

                if start_ind == 0:
                    sub = hist2[end_ind]
                elif end_ind >= len(hist2) - 1:
                    sub = hist2[start_ind]
                    i = end_ind
                else:
                    xs = np.linspace(0, end_ind - start_ind + 1, end_ind - start_ind + 1)
                    val_s = smooth_hist[start_ind]
                    val_e = smooth_hist[end_ind]
                    m = (val_e-val_s)/len(xs)
                    sub = np.array([m*x+val_s for x in xs])
                hist2[start_ind: end_ind+1] = sub
            i = i + 1

        return hist2

    def process_rings(self):
        """
        Process all rings
        :return:
        """
        peaks = self.info['model_peaks']
        result = []

        center = self.info['center']
        l = int(min(self.original_image.shape[0] - center[1], self.original_image.shape[1] - center[0], center[0], center[1]))
        I2D = self.info['2dintegration'][0]

        histogram1D = self.info['hull_hist']
        rings = []
        ring_peaks = []
        ring_hists = []

        # Check for non-existing peaks and include them
        d = self.getDspacing(peaks)
        self.log("D-spacing = " + str(d))
        self.info['distance'] = d
        limit = int(np.round(max(peaks)/d * 1.5))
        rads, sigmas, type_rings = self.get_missed_rings_by_distance(len(peaks), d, limit)

        for (i, _) in enumerate(rads):
            h = 1
            rad_min = int(np.around(rads[i] - h * sigmas[i]))
            rad_max = int(np.around(rads[i] + h * sigmas[i]))

            if rad_max < rad_min:
                rad_max, rad_min = rad_min, rad_max
            rad_max = min(rad_max, len(histogram1D))
            rad_min = max(rad_min, 0)

            # Added concern of partial rings -> divide by number of degrees
            if rads[i] > l:
                degrees = self.get_ring_angle_range(rads[i], center, self.original_image.shape)
            else:
                degrees = 360
            # Compute the area under each peak
            rings.append(simps(histogram1D[rad_min:rad_max]) / degrees)

            # Add type of ring and angle range if not complete
            if type_rings[i] != -1:
                type_rings[i] = degrees

            # Compute the azimuthal integration for each ring from 2D integration
            hists = [np.sum(I2D[j, rad_min:rad_max], axis=0) for j in range(360)]
            hists = np.array(hists)
            hists2 = copy.copy(hists)

            # Filled pilatus valleys
            if self.original_image.shape == (1043, 981):
                hists2 = self.removeValleys(hists2)

            hists = hists - min(hists2)
            hists[hists < 0] = 0
            hists2 = hists2-min(hists2)
            result.append((np.arange(0, 2 * np.pi, 2 * np.pi / 360), np.array(hists2)))

            # Compute peak of each ring
            ring_peaks.append(max(hists2))
            ring_hists.append(hists)


        self.info['rings_area'] = rings
        self.info['rings_peaks'] = ring_peaks
        self.info['radius'] = rads
        self.info['sigmas'] = sigmas
        self.info['type_rings'] = type_rings
        self.info['ring_hists'] = ring_hists
        return result

    def find_nearest(self, array, value):
        """
        Find index of array which has the nearest value
        """
        idx = (np.abs(array - value)).argmin()
        return idx

    def get_ring_model(self, hist):
        """
        Fit gaussian model to rings
        :param hist:
        :return:
        """
        # Smooth histogram to find parameters easier
        # hist = (hist[0], np.convolve(hist[1], [0.5, 1, 0.5])[1:361] / 2)
        ring_hist = smooth(hist[1], 20)
        hist = (hist[0], ring_hist)

        index = np.argmax(hist[1])
        u1 = hist[0][index]
        u2 = (u1 + np.pi)%(2*np.pi)
        d = 180

        # If peak is not in first half make a round sum
        min_x = index - d
        max_x = index + d
        if min_x > 0 and max_x < 360:
            alpha1 = hist[1][range(min_x, max_x)].sum() * 2 * np.pi / len(hist[1])
        else :
            if min_x < 0:
                min_x += 360
            if max_x >= 360:
                max_x -= 360
            alpha1 = hist[1][range(0, max_x)].sum() * 2 * np.pi / len(hist[1])
            alpha1 += hist[1][range(min_x, 360)].sum() * 2 * np.pi / len(hist[1])

        sigma1 = alpha1 / hist[1][index] / np.sqrt(2 * np.pi)
        sigma1 = sigma1 if not math.isnan(sigma1) else 0.5
        # Fit model using same gaussian
        x = hist[0]

        # TODO: Use the model with constrains as for fitGMMv2
        # Call orientation_GMM2
        model = Model(orientation_GMM2, independent_vars='x')
        max_height = np.max(hist[1])

        init_u = min(u1, u2)
        params = Parameters()
        params.add("u",  init_u, min=0, max=np.pi)
        params.add("sigma", sigma1, min=0, max=np.pi*2)
        params.add("alpha", alpha1, min=0, max=alpha1*5+0.0000001)
        params.add("bg", 0, min = -1, max = max_height+1)

        result = model.fit(hist[1], x=x, params = params, nan_policy='propagate')

        # Compute valley point and circular shift
        v_value = result.values['u'] + np.pi / 2
        v_point = self.find_nearest(hist[0], v_value)

        hist_shifted = np.roll(hist[1], -v_point)

        # Fit model again with shifted histogram
        # initial guess
        sigma = result.values['sigma']
        alpha = result.values['alpha']
        bg = result.values['bg']
        u1 = np.pi / 2.

        params = Parameters()
        params.add("u", u1, min=0, max=np.pi)
        params.add("sigma", sigma, min=0, max=np.pi*2)
        params.add("alpha", alpha, min=0, max=alpha*5+0.0000001)
        params.add("bg", bg, min = -1, max = max_height)

        model = Model(orientation_GMM2, independent_vars='x')
        result = model.fit(hist_shifted, x=x, params=params, nan_policy='propagate')
        result = result.values

        # Correction over shifted peaks
        result['u'] = result['u'] + v_value - np.pi

        return result

    def get_ring_model2(self, hist):
        """
        Fit gaussian model to rings
        :param hist:
        :return:
        """
        # Smooth histogram to find parameters easier
        hist[1] = smooth(hist[1], 20)

        # n_hist = len(hist[1])
        index = np.argmax(hist[1])
        u = hist[0][index]
        if u < np.pi / 2:
            u += np.pi
        elif u > 3 * np.pi / 2:
            u -= np.pi

        # Fit model using same gaussian
        x = hist[0]

        # Call orientation_GMM3
        model = Model(orientation_GMM3, independent_vars='x')
        max_height = np.max(hist[1])

        model.set_param_hint('u', value=u, min=np.pi/2, max=3*np.pi/2)
        model.set_param_hint('sigma', value=0.1, min=0, max=np.pi*2)
        model.set_param_hint('alpha', value=max_height*0.1/0.3989423, min=0)
        model.set_param_hint('bg', value=0, min=-1, max=max_height+1)

        result = model.fit(data=hist[1], x=x, params=model.make_params(), nan_policy='propagate')
        errs = abs(result.best_fit - result.data)
        if errs.mean() != 0:
            weights = errs / errs.mean() + 1
        else:
            weights=np.ones_like(errs)
        weights[weights > 3.] = 0
        result = model.fit(data=hist[1], x=x, params=result.params, weights=weights, nan_policy='propagate')

        return result.values

    def getRingHistograms(self):
        """
        Give the histogram of the different rings on the image.
        """
        histograms = []
        deep_valleys_hists = []

        # Filter of rings taken into account (complete)
        rings, idxs = [], []
        for i, p in enumerate(self.info['model_peaks']):
            if self.info['ROI'][0] <= p <= self.info['ROI'][1]:
                rings.append(int(round(p)))
                idxs.append(i)
        I2D = copy.copy(self.info['2dintegration'][0])
        I2D2 = copy.copy(I2D)
        if self.original_image.shape == (1043, 981):
            # if the image is from pilatus, replace gaps with large minus value to make deep valley
            I2D2[I2D2 <= 0] = -99999

        sigmas = [int(round(s)) for s in self.getInitSigma(self.info['hull_hist'], rings)]
        self.log("Peaks to check = "+ str(rings))
        for (i, _) in enumerate(rings):
            histograms.append(np.sum(I2D[:, rings[i]-sigmas[i]:rings[i]+sigmas[i]], axis=1))
            deep_valleys_hists.append(np.sum(I2D2[:, rings[i]-sigmas[i]:rings[i]+sigmas[i]], axis=1))

        if len(histograms) < 1:
            return [], [], []

        ring_hists, revised_hists = [], []
        for (i, _) in enumerate(histograms):

            # remove valleys and remove linear background
            hist = copy.copy(histograms[i])
            if self.original_image.shape == (1043, 981):
                deep_valleys_hist = deep_valleys_hists[i]
                hist = self.removeValleys2(deep_valleys_hist, hist)
            min_val = hist.min()
            hist = hist - min_val

            # Collect real histogram for displaying
            real_hist = np.array(histograms[i])
            real_hist -= min_val
            real_hist[real_hist < 0] = 0
            ring_hists.append(real_hist)
            revised_hists.append(hist)

        return ring_hists, revised_hists, idxs

    def processOrientation2(self, model='GMM2'):
        """
        Calculate ring orientation - get ring_hists, 'ring_models', 'ring_errors', 'average_ring_model'
        :return:
        """
        ring_hists, revised_hists, idx_dict = self.getRingHistograms()
        # Obtaining gaussian model of each ring histogram, means of gaussians and error over histogram
        model_dict = {}
        errors_dict = {}

        x = np.arange(0, 2 * np.pi, 2 * np.pi / 360)

        for i, hist in zip(idx_dict, revised_hists):

            # Fit orientation model
            if model == 'GMM2':
                model_dict[i] = self.get_ring_model([x, hist])
                errors_dict[i] = 1 - r2_score(orientation_GMM2(x=x, **model_dict[i]), hist)
            elif model == 'GMM3':
                model_dict[i] = self.get_ring_model2([x, hist])
                errors_dict[i] = 1 - r2_score(orientation_GMM3(x=x, **model_dict[i]), hist)

        self.info['ring_hists'] = ring_hists
        self.info['ring_models'] = model_dict
        self.info['ring_errors'] = errors_dict

        # Find avarage ranges ( ignore outliers )
        all_u1s = sorted([model_dict[i]['u'] for i in model_dict
            if errors_dict[i] < 1. and model_dict[i]['sigma'] < 1])
        u1_dict = self.select_peaks(all_u1s, times_threshold=1, distance_threshold=0.1, round_val=False)

        # Find an average ring model ( ignore models which produce high error )
        if len(u1_dict) > 0:
            best_angle = max(u1_dict.keys(), key=lambda a:u1_dict[a])
            average_result = {
                'u' : best_angle
            }
            sum_sigma = 0
            sum_alpha = 0
            nModels = 0
            for i in model_dict:
                if errors_dict[i] < 1. and abs(model_dict[i]['u']-best_angle) < 0.1:
                    sum_sigma += model_dict[i]['sigma']
                    sum_alpha += model_dict[i]['alpha']
                    nModels += 1
            if nModels > 0:
                average_result['sigma'] = 1. * sum_sigma / nModels
                average_result['alpha'] = 1. * sum_alpha / nModels
                average_result['bg'] = 0
                self.info['average_ring_model'] = average_result
                self.log("Average Ring Model : "+ str(average_result))
            else:
                self.log("No average model detected")

    def HoF(self, hist, mode='f'):
        """
        Calculate Herman Orientation Factors
        """
        Ints = []
        n_pi = len(hist) // 2  # number of hist unit in pi range
        n_hpi = n_pi // 2      # number of hist unit in half pi range
        for i in range(n_pi):
            I = hist[i:(i+n_pi)].copy()
            I[:i] += np.flipud(hist[:i])
            I[i:] += np.flipud(hist[(i+n_pi):])
            Ints.append(I)
        rads = np.linspace(0, np.pi, n_pi + 1)[:-1]
        denom = np.sin(rads)
        numer = (np.cos(rads)**2) * denom

        HoFs = np.zeros(hist.shape)
        for i in range(len(hist)):
            I = Ints[i] if i < n_pi else np.flipud(Ints[i - n_pi])
            if mode == 'f':
                HoFs[i] = ((I * numer).sum() / (I * denom).sum()) if i < n_pi else HoFs[i - n_pi]
            else:
                HoFs[i] = (I[:n_hpi] * numer[:n_hpi]).sum() / (I[:n_hpi] * denom[:n_hpi]).sum()
        return (3 * HoFs - 1) / 2

    def getRadOfMaxHoF(self, HoFs, mode, ratio=0.05):
        """
        Get the radian of the maximum Herman Orientation Factor
        :param HoFs:
        :param mode:
        """
        nHoFs = len(HoFs)
        num = int(nHoFs * ratio)
        if mode == 'f':
            HoFs = HoFs[:(nHoFs // 2)]
            num //= 2
        # get the indices of the top num largest HoFs
        idxs = sorted(np.arange(len(HoFs)), key=lambda i: HoFs[i])[-num:]
        idxs = sorted(idxs)
        # group the indices
        grps = [[idxs[0]]]
        for idx in idxs[1:]:
            if grps[-1][-1] == idx - 1:
                grps[-1].append(idx)
            else:
                grps.append([idx])
        # handle the round case
        if len(grps) > 1 and grps[0][0] == 0 and grps[-1][-1] == len(HoFs) - 1:
            grps[0] += [idx - len(HoFs) for idx in grps[-1]]
        # find the groups of max number of indices
        maxn = max(len(grp) for grp in grps)
        grps = [grp for grp in grps if len(grp) == maxn]
        opt_grp = sorted(grps, key=lambda g:HoFs[g].sum())[-1]
        opt_idx = np.mean(opt_grp) % len(HoFs)
        return 2 * np.pi * opt_idx / nHoFs

    def HermanOrientation(self, ir='f', thres=0.1):
        """
        Calculate Herman factors - get ring_hists,
        :param ir: integration radian, half Pi or Pi
        :return:
        """
        ROI = self.info['ROI']
        roi_hist = np.sum(self.info['2dintegration'][0][:, ROI[0]:ROI[1]], axis=1)
        roi_hist -= roi_hist.min()
        ring_hists, revised_hists, idx_dict = self.getRingHistograms()

        model_dict, errors_dict = {}, {}
        for i, hist in zip(idx_dict, revised_hists):

            # Calculate herman factors
            HoFs = self.HoF(hist, ir)
            u = self.getRadOfMaxHoF(HoFs, ir)
            model_dict[i] = {'u': u, 'HoFs': HoFs, 'sigma': 0, 'alpha': 0, 'bg': 0}
            errors_dict[i] = 1 + (HoFs.max() - thres) / (thres - 1)

        self.info['ring_hists'] = ring_hists
        self.info['ring_models'] = model_dict
        self.info['ring_errors'] = errors_dict

        roi_result = {'sigma': 0, 'alpha': 0, 'bg': 0}
        roiHoFs = self.HoF(roi_hist, ir)
        roi_result['u'] = self.getRadOfMaxHoF(roiHoFs, ir)
        roi_result['HoFs'] = roiHoFs
        self.info['average_ring_model'] = roi_result
        self.log("Herman orientation reuslt : "+ str(roi_result['u']))

    def maxIntensityOrientation(self):
        """
        Calculate the max intensity orientation.
        :return: -
        """
        ROI = self.info['ROI']
        roi_hist = np.sum(self.info['2dintegration'][0][:, ROI[0]:ROI[1]], axis=1)
        ring_hists, revised_hists, idx_dict = self.getRingHistograms()

        model_dict, errors_dict = {}, {}
        for i, hist in zip(idx_dict, revised_hists):

            # Calculate herman factors
            u = max(np.arange(180), key=lambda d: np.sum(hist[d:d + 1]) + np.sum(hist[d + 180:d + 181])) * np.pi / 180
            model_dict[i] = {'u': u, 'sigma': 0, 'alpha': 0, 'bg': 0}
            errors_dict[i] = 0

        self.info['ring_hists'] = ring_hists
        self.info['ring_models'] = model_dict
        self.info['ring_errors'] = errors_dict

        roi_result = {'sigma': 0, 'alpha': 0, 'bg': 0, 'hist': roi_hist}
        roi_result['u'] = max(np.arange(180), key=lambda d: np.sum(roi_hist[d:d + 1]) + \
            np.sum(roi_hist[d + 180:d + 181])) * np.pi / 180
        self.info['average_ring_model'] = roi_result
        self.log("Max intensity orientation : "+ str(roi_result['u']))

############################# Batch mode Results #############################

def toFloat(value):
    """
    Convert to float.
    :param value: the value that needs to be converted
    :return: x
    """
    try:
        x = float(value)
    except Exception:
        x = 0
    return x

def convertRadtoDegrees(rad):
    """
    Convert radian to degrees.
    :param rad: the value that needs to be converted
    :return: deg value
    """
    rad = toFloat(rad)
    return int(rad * 180 / np.pi)

############################# Gaussian Models #############################

def GM(x, u1, sigmad1, alpha1):
    """
    Gaussian model
    """
    return alpha1 * np.exp(-1. * (x - u1) ** 2 / (2 * sigmad1 ** 2)) * (1. / (np.sqrt(2 * np.pi) * sigmad1))

def GMM_any(x, params):
    """
    Gaussian model with params
    """
    n = int(len(params.keys())/3)
    result = GM(x, params['u1'], params['sigmad1'], params['alpha1'])
    for i in range(2, n+1):
        result += GM(x, params['u'+str(i)], params['sigmad'+str(i)], params['alpha'+str(i)])
    return result

def orientation_GMM2(x, u, sigma, alpha, bg):
    """
    Orientation Gaussian model
    """
    mod = GaussianModel()
    return mod.eval(x=x, amplitude=alpha, center=u, sigma=sigma) + mod.eval(x=x, amplitude=alpha, center=u+np.pi, sigma=sigma) + bg

def orientation_GMM2_v1(x, u1, u2, sigma, alpha):
    """
    Orientation Gaussian model
    """
    mod = GaussianModel()
    return mod.eval(x=x, amplitude=alpha, center=u1, sigma=sigma) + mod.eval(x=x, amplitude=alpha, center=u2, sigma=sigma)

def orientation_GMM3(x, u, sigma, alpha, bg):
    """
    Orientation Gaussian model
    """
    mod = GaussianModel()
    return mod.eval(x=x, amplitude=alpha, center=u, sigma=sigma) + \
        mod.eval(x=x, amplitude=alpha, center=u-np.pi, sigma=sigma) + \
        mod.eval(x=x, amplitude=alpha, center=u+np.pi, sigma=sigma) + bg

def fitGMMv2(hists_np, indexes, widthList, method='leastsq'):
    """
    Fit Gaussian model
    """
    parameters = []
    indexes = copy.copy(indexes)
    for index, width in zip(indexes, widthList):
        sigmad = 1. / np.sqrt(8 * np.log(2)) * width
        alpha = sigmad * hists_np[index] * np.sqrt(2 * np.pi)
        parameters.append((sigmad, alpha))

    x = np.array(range(len(hists_np)))
    pars = Parameters()
    gaussians = []
    mean_margin = 15
    for (i, _) in enumerate(indexes):
        prefix = 'g' + str(i + 1) + '_'
        gauss = GaussianModel(prefix=prefix)
        pars.update(gauss.make_params())
        pars[prefix + 'center'].set(indexes[i], min=indexes[i] - mean_margin, max=indexes[i] + mean_margin)
        pars[prefix + 'sigma'].set(parameters[i][0], min=1., max=1000.)
        pars[prefix + 'amplitude'].set(parameters[i][1], min=1, max=1000 * hists_np[indexes[i]])
        gaussians.append(gauss)

    model = gaussians[0]
    for i in range(1, len(gaussians)):
        model += gaussians[i]
    out = model.fit(hists_np, pars, x=x, method=method, nan_policy='propagate').values
    result = {}
    for i in range(len(indexes)):
        prefix = 'g' + str(i + 1) + '_'
        result['u' + str(i + 1)] = out[prefix + 'center']
        result['sigmad' + str(i + 1)] = out[prefix + 'sigma']
        result['alpha' + str(i + 1)] = out[prefix + 'amplitude']

    error = r2_score(GMM_any(x = x, params = result), hists_np)
    return result, error
