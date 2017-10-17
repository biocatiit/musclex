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

import musclex
import fabio
from os.path import exists, isfile
from lmfit import Model, Parameters
from lmfit.models import GaussianModel, SkewedGaussianModel, VoigtModel
import pickle
from ..utils.file_manager import fullPath, createFolder
from ..utils.histogram_processor import smooth, movePeaks, getPeakInformations
import copy
import numpy as np
from sklearn.metrics import r2_score

class ProjectionProcessor():
    def __init__(self, dir_path, file_name):
        self.dir_path = dir_path
        self.filename = file_name
        img = fabio.open(fullPath(dir_path, file_name)).data
        # img -= img.min()
        self.orig_img = img
        self.version = musclex.__version__
        cache = self.loadCache()
        if cache is None:
            # info dictionary will save all results
            self.info = {
                'box_numbers' : set(),
                'boxes' : {},
                'types' : {},
                'hists' : {},
                'peaks' : {},
                'fit_results':{},
                'subtracted_hists' : {},
                'moved_peaks':{},
                'baselines':{},
                'centroids':{},
                'widths': {}
            }
        else:
            self.info = cache

    def addBox(self, num, box, type):
        """
        Add a box to info. If it exists and it changed, clear all old result
        :param num: box number
        :param box: box coordinates
        :param type: box type 'v' ad vertical, 'h' as horizontal
        :return:
        """
        box_numbers = self.info['box_numbers']
        if num in box_numbers and self.info['boxes'][num] != box:
            self.removeInfo(num)
            self.addBox(num, box, type)
        else:
            box_numbers.add(num)
            self.info['boxes'][num] = box
            self.info['types'][num] = type

    def addPeaks(self, num, peaks):
        """
        Add peaks to a box.
        :param num: box number
        :param peaks: peaks
        :return:
        """
        box_numbers = self.info['box_numbers']
        if num in box_numbers:
            all_peaks = self.info['peaks']
            if all_peaks.has_key(num) and all_peaks[num] == peaks:
                return
            all_peaks[num] = peaks
            skip_list = ['box_numbers', 'boxes', 'types', 'peaks', 'hists']
            for k in self.info.keys():
                if k not in skip_list:
                    self.removeInfo(num, k)
        else:
            print "Warning : box number is invalid."

    def removePeaks(self, num):
        """
        Remove all peaks from a box
        :param num: box number
        :return:
        """
        skip_list = ['box_numbers', 'boxes']
        for k in self.info.keys():
            if k not in skip_list:
                self.removeInfo(num, k)

    def process(self, settings = {}):
        """
        All processing steps - all settings are provided by Projection Traces app as a dictionary
        """
        self.updateSettings(settings)
        self.getHistograms()
        self.fitModel()
        # self.getOtherResults()
        self.getBackgroundSubtractedHistograms()
        self.getPeakInfos()
        self.cacheInfo()

    def updateSettings(self, settings):
        if settings.has_key('boxes'):
            new_boxes = settings['boxes']
            types = settings['types']
            old_boxes = self.info['boxes']
            all_num = new_boxes.keys()
            all_num.extend(old_boxes.keys())
            all_num = set(all_num)
            for num in all_num:
                if num in new_boxes.keys():
                    self.addBox(num, new_boxes[num], types[num])
                else:
                    self.removeInfo(num)
            del settings['boxes']

        if settings.has_key('peaks'):
            new_peaks = settings['peaks']
            old_peaks = self.info['peaks']
            all_num = new_peaks.keys()
            all_num.extend(old_peaks.keys())
            all_num = set(all_num)
            for num in all_num:
                if num in new_peaks.keys():
                    self.addPeaks(num, new_peaks[num])
                else:
                    self.removePeaks(num)
            del settings['peaks']

        self.info.update(settings)

    def getHistograms(self):
        box_numbers = self.info['box_numbers']
        if len(box_numbers) > 0:
            boxes = self.info['boxes']
            types = self.info['types']
            hists = self.info['hists']
            img = copy.copy(self.orig_img)
            for num in box_numbers:
                if not hists.has_key(num):
                    t = types[num]
                    b = boxes[num]
                    x1 = b[0][0]
                    x2 = b[0][1]
                    y1 = b[1][0]
                    y2 = b[1][1]
                    area = img[y1:y2+1, x1:x2+1]
                    if t == 'h':
                        hist = np.sum(area, axis=0)
                    else:
                        hist = np.sum(area, axis=1)
                    hists[num] = hist

    def fitModel(self):
        """
        Fit model to histogram
        Fit results will be kept in self.info["fit_results"].
        """
        box_numbers = self.info['box_numbers']
        all_hists = self.info['hists']
        all_peaks = self.info['peaks']
        all_boxes = self.info['boxes']
        fit_results = self.info['fit_results']

        for num in box_numbers:
            hist = np.array(all_hists[num])

            if not all_peaks.has_key(num) or len(all_peaks[num]) == 0 or fit_results.has_key(num):
                continue

            peaks = all_peaks[num]
            box = all_boxes[num]
            start_x = box[0][0]
            start_y = box[1][0]

            x = np.arange(0, len(hist))

            int_vars = {
                'x' : x
            }

            # Initial Parameters
            params = Parameters()

            # Init Center X
            if self.info['types'][num] == 'h':
                init_center = self.orig_img.shape[1] / 2 - 0.5 - start_x
            else:
                init_center = self.orig_img.shape[0] / 2 - 0.5 - start_y
            params.add('centerX', init_center, min=init_center - 1., max=init_center + 1.)

            # Init background params
            params.add('bg_sigma', len(hist)/3., min=0, max=len(hist)*2+1.)
            params.add('bg_amplitude', sum(hist)/10., min=0, max=sum(hist)+1.)

            # Init Meridian params1
            params.add('center_sigma1', 50, min=0, max=len(hist)/2.+1.)
            params.add('center_amplitude1', sum(hist)/10., min=0., max=sum(hist)/2 + 1)

            # Init Meridian params2
            params.add('center_sigma2',5 , min=0, max=20)
            params.add('center_amplitude2', sum(hist) / 20., min=0., max=sum(hist) / 2 + 1)

            # Init peaks params
            for j,p in enumerate(peaks):
                params.add('p_' + str(j), p, min=p - 10., max=p + 10.)
                params.add('sigma' + str(j), 10, min=1, max=50)
                params.add('amplitude' + str(j), sum(hist)/20., min=0., max=sum(hist)/3. + 1)
                # params.add('gamma' + str(j), 0. , min=-3., max=3)

            # Fit model
            model = Model(layerlineModel, independent_vars=int_vars.keys())
            result = model.fit(hist, verbose=False, params=params, **int_vars)
            if result is not None:
                #
                # import matplotlib.pyplot as plt
                # fig = plt.figure()
                # ax = fig.add_subplot(111)
                # ax.cla()
                # ax.plot(hist, color='g')
                # ax.plot(layerlineModel(x, **result.values), color='b')
                # fig.show()
                model = result.values

                model['error'] = 1. - r2_score(hist, layerlineModel(x, **result.values))
                self.info['fit_results'][num] = model

                print "Box :", num
                print "Fitting Result :", result.values
                print "Fitting Error :", model['error']
                print "---"


    def getOtherResults(self):
        """
        Get other results from fitting model : distances in nm
        :return:
        """
        if self.info.has_key('fit_results') and self.info.has_key('lambda_sdd'):
            fit_results = self.info['fit_results']
            ds = []
            for params in fit_results:
                if params is not None:
                    ds.append(params['p']/self.info['lambda_sdd'])
                else:
                    ds.append(None)
            self.info['distances'] = ds

    def getBackgroundSubtractedHistograms(self):
        """
        Get Background Subtracted Histograms by subtract the original histogram by background from fitting model
        :return:
        """
        box_numbers = self.info['box_numbers']
        all_hists = self.info['hists']
        fit_results = self.info['fit_results']
        subt_hists = self.info['subtracted_hists']
        for num in box_numbers:
            if subt_hists.has_key(num) or not fit_results.has_key(num):
                continue

            # Get subtracted histogram if fit result exists
            fit_result = fit_results[num]
            hist = all_hists[num]
            xs = np.arange(0, len(hist))
            background = layerlineModelBackground(xs, **fit_result)
            subt_hists[num] = hist-background
            self.removeInfo(num, 'moved_peaks')

    def getPeakInfos(self):
        """
        Get peaks' infomation including baseline and centroids
        :return:
        """

        box_numbers = self.info['box_numbers']
        all_hists = self.info['subtracted_hists']
        fit_results = self.info['fit_results']
        moved_peaks = self.info['moved_peaks']
        all_baselines = self.info['baselines']
        all_centroids = self.info['centroids']
        all_widths = self.info['widths']

        for num in box_numbers:

            if not all_hists.has_key(num):
                continue

            ### Find real peak locations in the box (not distance from center)
            model = fit_results[num]
            hist = smooth(all_hists[num])

            if not moved_peaks.has_key(num):
                peaks = []
                i = 0
                while 'p_'+str(i) in model.keys():
                    peaks.append(int(round(model['centerX']+model['p_'+str(i)])))
                    i+=1
                peaks = movePeaks(hist, peaks, 5)
                moved_peaks[num] = peaks
                self.removeInfo(num, 'baselines')

            peaks = moved_peaks[num]

            ### Calculate Baselines
            if not all_baselines.has_key(num):
                baselines = []
                for p in peaks:
                    baselines.append(hist[p]*0.5)
                all_baselines[num] = baselines
                self.removeInfo(num, 'centroids')

            baselines = all_baselines[num]

            if not all_centroids.has_key(num):
                results = getPeakInformations(hist, peaks, baselines)
                all_centroids[num] = results['centroids'] - model['centerX']
                all_widths[num] = results['widths']

    def setBaseline(self, box_num, peak_num, new_baseline):
        """
        Change baseline and clear centroid and width for the specific box and peak
        :param box_num: box number (int)
        :param peak_num: peak number (int)
        :param new_baseline: new baseline value or percentage (str)
        :return:
        """
        new_baseline = str(new_baseline)
        baselines = self.info['baselines'][box_num]
        peak = self.info['moved_peaks'][box_num][peak_num]
        hist = self.info['subtracted_hists'][box_num]
        height = hist[peak]
        if "%" in new_baseline:
            # if new_baseline contain "%", baseline value will use this as percent of peak height
            percent = float(new_baseline.rstrip("%"))
            baseline = height * percent / 100.
        elif len(new_baseline) == 0:
            # if new_baseline is empty, baseline will by half-height
            baseline = float(height * .5)
        else:
            baseline = float(new_baseline)

        if height > baseline:
            baselines[peak_num] = baseline
            self.removeInfo(box_num, 'centroids')
            self.removeInfo(box_num, 'widths')

    def removeInfo(self, num, k = None):
        """
        Remove information from info dictionary by k as a key. If k is None, remove all information in the dictionary
        :param num: box number
        :param k: key of dictionary
        :return: -
        """
        ignore_list = ['lambda_sdd', 'program_version']

        if k is not None and self.info.has_key(k) and k not in ignore_list:
            d = self.info[k]
            if d.has_key(num):
                del d[num]

        if k is None:
            for k in self.info.keys():
                d = self.info[k]
                if k == 'box_numbers':
                    d.remove(num)
                else:
                    self.removeInfo(num, k)

    def loadCache(self):
        """
        Load info dict from cache. Cache file will be filename.info in folder "bm_cache"
        :return: cached info (dict)
        """
        cache_path = fullPath(self.dir_path, "ll_cache")
        cache_file = fullPath(cache_path, self.filename + '.info')

        if exists(cache_path) and isfile(cache_file):
            cinfo = pickle.load(open(cache_file, "rb"))
            if cinfo != None:
                if cinfo['program_version'] == self.version:
                    return cinfo
        return None

    def cacheInfo(self):
        """
        Save info dict to cache. Cache file will be save as filename.info in folder "qf_cache"
        :return: -
        """
        cache_path = fullPath(self.dir_path, 'll_cache')
        createFolder(cache_path)
        cache_file = fullPath(cache_path, self.filename + '.info')

        self.info["program_version"] = self.version
        pickle.dump(self.info, open(cache_file, "wb"))


def layerlineModel(x, centerX, bg_sigma, bg_amplitude, center_sigma1, center_amplitude1, center_sigma2, center_amplitude2, **kwargs):
    """
    Model for fitting layer line pattern
    :param x: x axis
    :param centerX: center of x axis
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
    result = layerlineModelBackground(x, centerX, bg_sigma, bg_amplitude, center_sigma1, center_amplitude1, center_sigma2, center_amplitude2)
    #### Other peaks
    # mod = SkewedGaussianModel()

    i = 0
    while 'p_'+str(i) in kwargs:
        p = kwargs['p_'+str(i)]
        sigma = kwargs['sigma'+str(i)]
        amplitude = kwargs['amplitude' + str(i)]
        if kwargs.has_key('gamma' + str(i)):
            gamma = kwargs['gamma' + str(i)]

            mod = VoigtModel()
            result += mod.eval(x=x, amplitude=amplitude, center=centerX + p, sigma=sigma, gamma=gamma)
            result += mod.eval(x=x, amplitude=amplitude, center=centerX - p, sigma=sigma, gamma=-gamma)
        else:
            mod = GaussianModel()
            result += mod.eval(x=x, amplitude=amplitude, center=centerX + p, sigma=sigma)
            result += mod.eval(x=x, amplitude=amplitude, center=centerX - p, sigma=sigma)

        i += 1

    return result

def layerlineModelBackground(x, centerX, bg_sigma, bg_amplitude, center_sigma1, center_amplitude1, center_sigma2, center_amplitude2, **kwargs):
    """
    Model for fitting layer line pattern
    :param x: x axis
    :param centerX: center of x axis
    :param bg_sigma: background sigma
    :param bg_amplitude: background amplitude
    :param center_sigma1: meridian background sigma
    :param center_amplitude1: meridian background amplitude
    :param center_sigma2: meridian sigma
    :param center_amplitude2: meridian amplitude
    :param kwargs: nothing
    :return:
    """
    return layerlineBackground(x, centerX, bg_sigma, bg_amplitude) + \
           meridianBackground(x, centerX, center_sigma1, center_amplitude1) + \
           meridianGauss(x, centerX, center_sigma2, center_amplitude2)


def layerlineBackground(x, centerX, bg_sigma, bg_amplitude, **kwargs):
    """
    Model for largest background of layer line pattern
    :param x: x axis
    :param centerX: center of x axis
    :param bg_sigma: background sigma
    :param bg_amplitude: background amplitude
    :param kwargs: nothing
    :return:
    """
    mod = GaussianModel()
    return  mod.eval(x=x, amplitude=bg_amplitude, center=centerX, sigma=bg_sigma)

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