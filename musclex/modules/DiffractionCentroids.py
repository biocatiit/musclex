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
import matplotlib.pyplot as plt
import fabio
import pyFAI
from lmfit import Parameters
from lmfit.models import VoigtModel
from os import makedirs
from os.path import isfile, exists
import pickle
from ..utils.file_manager import fullPath
from ..utils.image_processor import *
from ..utils.histogram_processor import *
import musclex

class DiffractionCentroids():
    """
    A class for Diffraction Centroids processing - go to process() to see all processing steps
    """
    def __init__(self, dir_path, imgList, grp_number, fixRanges, off_mer):
        """
        Initial value for DiffractionCentroids object
        :param dir_path: directory path of input images (str)
        :param imgList: all images in a group (list)
        :param grp_number: image group number - used for writing csv (int)
        :param fixRanges: fixed peak ranges configured my users
        :param off_mer: configuration of off-meridian peaks configured my users
        """
        self.avgImg = self.mergeImages(dir_path, imgList) # avarage all image in a group
        self.dir_path = dir_path
        self.version = musclex.__version__
        self.info = {
            "reject" : {"top":[], "bottom": [] }
        }
        self.cachefile, cinfo = self.loadCache(dir_path, imgList)
        self.info.update(cinfo)
        self.info["filelist"] = imgList
        self.info["grp_num"] = grp_number+1
        self.fixRanges = fixRanges
        self.init_off_mer = off_mer

    def mergeImages(self, dir_path, imgList):
        """
        Merge all image in imgList
        :param dir_path: directory of images
        :param imgList: all merging images
        :return:
        """
        imgList2 = []
        for fname in imgList:
            fullname = fullPath(dir_path, fname)
            img = fabio.open(fullname).data
            imgList2.append(img)
        return np.mean(imgList2, axis=0)

    def loadCache(self, dir_path, imgList):
        """
        Load info dict from cache. Cache file will be filename.info in folder "qf_cache"
        :param dir_path: directory of images
        :param imgList: input images
        :return: cached info (dict)
        """
        cache_path = fullPath(dir_path, 'dc_cache')
        cache_filename = imgList[0]+'_'+ imgList[-1]+ '.info'
        cachefile = fullPath(cache_path, cache_filename)
        info = {}

        if isfile(cachefile):
            cinfo = pickle.load(open(cachefile, "rb"))
            if cinfo != None:
                if cinfo['program_version'] == self.version:
                    info = cinfo

        return cachefile, info

    def process(self, flags):
        """
        All processing steps
        """
        imgList = self.info['filelist']
        print imgList[0],'...',imgList[-1],'are being processed...'
        self.info.update(flags)
        self.findCenter()
        self.findRotationAngle()
        self.calculateRmin()
        self.getIntegrateArea()
        self.setConvexhullPoints()
        self.getHistograms()
        self.getPeaks()
        self.correctPeaks()
        self.calculateBaselines()
        self.calculateCentroids()

        if self.init_off_mer is not None:
            self.getOffMeridianRanges()
            self.getOffMerRminmax()
            self.getOffMeridianHistograms()
            self.getOffMeridianPeaks()
            self.getOffMeridianBaselines()
            self.getOffMeridianInfos()

        if not self.info.has_key("no_cache"):
            self.cacheInfo()

    def removeInfo(self, k):
        """
        Remove k from info dict
        :param k: key of dictionary
        :return: -
        """
        if k == 'hists':
            for ky in ['top_hist', 'top_hull', 'bottom_hist', 'bottom_hull']:
                self.removeInfo(ky)
        if self.info.has_key(k):
            del self.info[k]

    def findCenter(self):
        """
        Find center of diffraction, and keep it in self.info["center"]
        This calculation will affect rotation angle, so self.info["rotationAngle"] will be removed
        """
        if self.info.has_key('center'):
            return
        self.info['center'] = getCenter(self.avgImg)
        print "center =", self.info['center']
        self.removeInfo('rotationAngle')

    def findRotationAngle(self):
        """
        Find rotation angle of diffraction, and keep it in self.info["rotationAngle"]
        This calculation will affect R-min, so self.info["rmin"] will be removed
        """
        if self.info.has_key('rotationAngle'):
            return
        center = self.info['center']
        self.info['rotationAngle'] = getRotationAngle(self.avgImg, center)
        print "rotation angle =", self.info['rotationAngle']
        self.removeInfo('rmin')

    def calculateRmin(self):
        """
        Find R-min of diffraction, and keep it in self.info["rmin"]
        This calculation will affect integrated area (meridian), so self.info["int_area"] will be removed
        """
        if self.info.has_key('rmin'):
            return
        img = copy.copy(self.avgImg)
        center = self.info['center']

        if img.shape == (1043, 981):
            det = "pilatus1m"
        else:
            det = "agilent_titan"

        corners = [(0, 0), (img.shape[1], 0), (0, img.shape[0]), (img.shape[1], img.shape[0])]
        npt_rad = int(round(max([distance(center, c) for c in corners])))
        ai = pyFAI.AzimuthalIntegrator(detector=det)
        ai.setFit2D(100, center[0], center[1])
        tth, I = ai.integrate1d(img, npt_rad, unit="r_mm")
        # tth, I = ai.integrate1d(img, npt_rad, unit="r_mm")
        self.info['rmin'] = getFirstVallay(I)
        print "R-min =", self.info['rmin']
        self.removeInfo('int_area')

    def getIntegrateArea(self):
        """
        Find intergrated area or meridian lines of diffraction, and keep it in self.info["int_area"]
        This calculation will affect start and end points of convexh hull applying points, so self.info["top_se"] and self.info["bottom_se"] will be removed
        """
        if self.info.has_key('int_area'):
            return
        center = self.info['center']
        rmin = self.info['rmin']
        img = getCenterRemovedImage(copy.copy(self.avgImg), self.info['center'], self.info['rmin']) # remove center location
        rotate_img = self.getRotatedImage(img) # rotate image
        l = max(0,center[0]-rmin) # initial guess for left line
        r = min(center[0]+rmin, rotate_img.shape[1]) # initial guess for right line
        area = rotate_img[:,l:r] # Get the area by initial guess
        hist = np.sum(area, axis=0) # find verital histogram
        hull = convexHull(hist)  # appl convex hull to the histogram

        if len(hist) > 0:
            max_loc = np.argmax(hull)
            r = 1
            l = 1
            # Find start and end points by finding first zero value from on both side
            while max_loc-l >= 0 and max_loc+r < len(hull) and (hull[max_loc-l] != 0 or hull[max_loc+r] != 0):
                if hull[max_loc-l] != 0:
                    l += 1
                if hull[max_loc+r] != 0:
                    r += 1

            # Assume that meridian size should not smaller than 50% or R-min
            if max_loc+r < center[0] or max_loc-l > center[0] or abs(r+l) < rmin*.5:
                self.info['int_area'] = (int(round(center[0] - rmin*.5)), int(round(center[0] + rmin*.5)) + 1)
            else:
                center = center[0]-rmin+max_loc
                self.info['int_area'] = (int(round(center-l*1.2)), int(round(center+r*1.2))+1)
        else:
            self.info['int_area'] = (int(round(center[0] - rmin * .5)), int(round(center[0] + rmin * .5)) + 1)
        print "integrated area =",self.info['int_area']
        self.removeInfo('top_se')
        self.removeInfo('bottom_se')

    def getRotatedImage(self, img = None, angle = None):
        """
        Get rotated image by angle. If the input params are not specified. image = original input image, angle = self.info["rotationAngle"]
        :param img: input image
        :param angle: rotation angle
        :return: rotated image
        """
        if img is None:
            img = copy.copy(self.avgImg)
        if angle is None:
            angle = self.info['rotationAngle']

        return rotateImage(img, self.info["center"], angle)

    def setConvexhullPoints(self):
        """
        Set start and and points for convex hull in both top and bottom side. Init by start = R-min and end = 0.7*half of image size.
        This values will be kept in self.info["top_se"] and self.info["bottom_se"]
        This calculation will affect histograms, so all histograms will be removed from self.info
        """
        if not self.info.has_key('top_se'):
            if self.info.has_key('top_fixed_se'):
                self.info['top_se'] = self.info['top_fixed_se']
            else:
                self.info['top_se'] = (self.info['rmin'], int(round(min(self.avgImg.shape[0] / 2, self.avgImg.shape[1] / 2) * 0.7)))
            self.removeInfo('top_hist')
            self.removeInfo('top_hull')
        if not self.info.has_key('bottom_se'):
            if self.info.has_key('bottom_fixed_se'):
                self.info['bottom_se'] = self.info['bottom_fixed_se']
            else:
                self.info['bottom_se'] = (self.info['rmin'], int(round(min(self.avgImg.shape[0]/2, self.avgImg.shape[1]/2) * 0.7)))
            self.removeInfo('bottom_hist')
            self.removeInfo('bottom_hull')

    def getHistograms(self):
        """
        Get original histograms and background subtracted histogram ( subtracted by convex hull )
        The original histogram will be kept in self.info["top_hist"] and self.info["bottom_hist"]
        The background subtracted histogram will be kept in self.info["top_hull"] and self.info["bottom_hull"]
        These changing will affect peak locations, so peaks will be removed from self.info
        """
        if self.info.has_key('top_hist') and self.info.has_key('top_hull') and self.info.has_key('bottom_hist') and self.info.has_key('bottom_hull'):
            return

        center_y = self.info['center'][1]
        int_area = self.info['int_area']
        img = self.getRotatedImage(copy.copy(self.avgImg), self.info['rotationAngle'])
        img_area = img[:,int_area[0]: int_area[1]]
        ignore = np.array([any(line < 0) for line in img_area])
        hist = np.sum(img_area, axis=1)

        top_hist, top_ignore, bottom_hist, bottom_ignore = self.splitHist(center_y, hist, ignore)

        if not (self.info.has_key('top_hist') or self.info.has_key('top_hull')):
            top_hull = convexHull(top_hist, start_p=self.info['top_se'][0], end_p=self.info['top_se'][1], ignore=top_ignore)
            self.info['top_hist'] = np.array(top_hist)
            self.info['top_hull'] = np.array(top_hull)
            self.removeInfo('pre_top_peaks')

        if not (self.info.has_key('bottom_hist') or self.info.has_key('bottom_hull')):
            bottom_hull = convexHull(bottom_hist, start_p=self.info['bottom_se'][0], end_p=self.info['bottom_se'][1], ignore=bottom_ignore)
            self.info['bottom_hist'] = np.array(bottom_hist)
            self.info['bottom_hull'] = np.array(bottom_hull)
            self.removeInfo('pre_bottom_peaks')

    def getPeaks(self):
        """
        Get pre peaks from histograms with specified fixed ranges or without. These peaks won't be used until it's correct.
        These pre peaks will be kept in self.info["pre_[side]_peaks"]
        This calculation will affect reak peaks, so peaks will be removed from self.info
        """
        if not self.info.has_key('pre_top_peaks'):
            if len(self.fixRanges) > 0:
                self.info['pre_top_peaks'] = self.getPeaksFromRanges(self.info['top_hull'], self.fixRanges)
            else:
                self.info['pre_top_peaks'] = getPeaksFromHist(self.info['top_hull'])
            self.removeInfo('top_peaks')

        if not self.info.has_key('pre_bottom_peaks'):
            if len(self.fixRanges) > 0:
                self.info['pre_bottom_peaks'] = self.getPeaksFromRanges(self.info['bottom_hull'], self.fixRanges)
            else:
                self.info['pre_bottom_peaks'] = getPeaksFromHist(self.info['bottom_hull'])
            self.removeInfo('bottom_peaks')

    def getPeaksFromRanges(self, hist, fix_ranges):
        """
        Get Peaks from specified peak ranges
        :param hist: background subtracted histogram
        :param fix_ranges: fixed ranges specified by users
        :return: -
        """
        results = []
        for fr in fix_ranges:
            r = fr[1]
            start = min(r[0], len(hist)-2)
            end = min(r[1], len(hist)-1)
            peak = start + np.argmax(hist[start:end + 1])
            results.append(peak)
        return results

    def correctPeaks(self):
        """
        Correct pre-peak locatons by moving them to the local maximum point.
        These result peaks are considered as real peaks. They will be kept in self.info["[side]_peaks"]
        Peaks location will affect baselines, so baselines will be removed from self.info
        :return:
        """
        if not self.info.has_key('top_peaks'):
            if len(self.fixRanges) == 0:
                moved_peaks = self.movePeaks(self.info['top_hull'], self.info['pre_top_peaks'])
                self.info['top_peaks'] = moved_peaks
                self.info['top_names'] = [str(p) for p in moved_peaks]
            else:
                self.info['top_peaks'] = self.info['pre_top_peaks']
                self.info['top_names'] = [self.fixRanges[i][0] for i in range(len(self.fixRanges))]
            self.removeInfo('top_baselines')

        print "Top peaks : "
        for i in range(len(self.info['top_peaks'])):
            print self.info['top_names'][i], ":", self.info['top_peaks'][i]

        if not self.info.has_key('bottom_peaks'):
            if len(self.fixRanges) == 0:
                moved_peaks = self.movePeaks(self.info['bottom_hull'], self.info['pre_bottom_peaks'])
                self.info['bottom_peaks'] = moved_peaks
                self.info['bottom_names'] = [str(p) for p in moved_peaks]
            else:
                self.info['bottom_peaks'] = self.info['pre_bottom_peaks']
                names = [self.fixRanges[i][0] for i in range(len(self.fixRanges))]
                self.info['bottom_names'] = names
            self.removeInfo('bottom_baselines')
        print "Bottom peaks : "
        for i in range(len(self.info['bottom_peaks'])):
            print self.info['bottom_names'][i], ":", self.info['bottom_peaks'][i]

    def movePeaks(self, hist, peaks, dist = 10):
        """
        Move peaks to their local maximum. Duplicated peak locations will be removed
        :param hist: input histogram
        :param peaks: approximate peak locations
        :param dist: maximum distance of local maximum
        :return: sorted moved peaks
        """
        peakList = []
        smooth_hist = smooth(hist)
        for p in peaks:
            new_peak = p
            while True:
                start = max(0, p-dist)
                end = min(len(hist), p+dist)
                new_peak = start + np.argmax(hist[start:end])

                if abs(p-new_peak) < 4:
                    break
                else:
                    left = min(p, new_peak)
                    right = max(p, new_peak)
                    if all(smooth_hist[left+1:right] > p):
                        break
                dist = dist/2
            peakList.append(new_peak)
        return sorted(list(set(peakList)))

    def calculateBaselines(self):
        """
        Find baselines of peaks. Initial with half-height of peaks
        Baselines will be kept in self..info["[side]_baselines"].
        This calulation might affact other infos : centroids width and intensity
        """
        if not self.info.has_key('top_baselines'):
            hist = self.info['top_hull']
            peaks = self.info['top_peaks']
            self.info['top_baselines'] = [hist[p] / 2. for p in peaks]
            self.removeInfo('top_centroids')
        print "Top baselines =", self.info['top_baselines']
        if not self.info.has_key('bottom_baselines'):
            hist = self.info['bottom_hull']
            peaks = self.info['bottom_peaks']
            self.info['bottom_baselines'] = [hist[p]/2. for p in peaks]
            self.removeInfo('bottom_centroids')
        print "Bottom baselines =", self.info['bottom_baselines']

    def calculateCentroids(self):
        """
        Calculate all other peaks infomation including centroid, width, and intensity(area)
        This results will be kept in self.info
        """
        if not self.info.has_key('top_centroids'):
            hist = self.info['top_hull']
            peaks = self.info['top_peaks']
            baselines = self.info["top_baselines"]
            results = getPeakInformations(hist, peaks, baselines)
            self.info['top_centroids'] = results['centroids']
            self.info['top_widths'] = results['widths']
            self.info['top_areas'] = results['areas']
        print "Top centroids =", self.info['top_centroids']
        if not self.info.has_key('bottom_centroids'):
            hist = self.info['bottom_hull']
            peaks = self.info['bottom_peaks']
            baselines = self.info["bottom_baselines"]
            results = getPeakInformations(hist, peaks, baselines)
            self.info['bottom_centroids'] = results['centroids']
            self.info['bottom_widths'] = results['widths']
            self.info['bottom_areas'] = results['areas']
        print "Bottom centroids =", self.info['bottom_centroids']

    def fitModel(self, hist, peaks, baselines):
        # JUST FOR TEST
        # Fit Voigt model to histogram using peaks and baselines
        # Currently, this function is not used by any process
        new_hist = np.zeros(len(hist))
        pars = Parameters()
        mean_margin = 3

        for i in range(len(peaks)):
            p = peaks[i]
            baseline = baselines[i]
            width, _ = getWidth(hist, p, baseline)
            new_hist[p - width:p + width] = hist[p - width:p + width]
            prefix = "v"+str(i + 1) + '_'
            init_amp = hist[p] * width * np.sqrt(2 * np.pi)
            voigt = VoigtModel(prefix=prefix)
            pars.add(prefix + 'center', p,  min=p - mean_margin, max=p + mean_margin)
            pars.add(prefix + 'sigma',width, min = 0, max = width*3.)
            pars.add(prefix + 'amplitude', init_amp, min = 0, max = init_amp*3.)
            pars.add(prefix + 'gamma', width,  min = 0, max = width*3.)
            pars.add(prefix + 'fwhm', baseline, min=0, max=baseline * 3.)
            pars.add(prefix + 'height',  hist[p], min=0, max= hist[p] * 3.)
            if i == 0:
                model = voigt
            else:
                model += voigt

        xs = np.arange(0, len(new_hist))
        result = model.fit(np.array(new_hist), params = pars, x = xs).values
        fig = plt.figure()
        ax = fig.add_subplot(111)
        ax.plot(hist)
        ax.plot(new_hist)
        ax.plot(model.eval(x=xs, **result))
        fig.show()
        print result

    def setBaseline(self, side, peak_num, new_baseline):
        """
        Set new baselines of meridina peaks by users. Remove centroids from self.info as it needs to be re-calculated.
        :param side: "top" or "bottom"
        :param peak_num: peak number from left to right (int)
        :param new_baseline: new baseline value or percent of peak height (str or float)
        """
        new_baseline = str(new_baseline)
        hist = self.info[side + "_hull"]
        peaks = self.info[side+ "_peaks"]
        baselines = self.info[side + "_baselines"]
        height = hist[peaks[peak_num]]
        if "%" in new_baseline:
            # if new_baseline contain "%", baseline value will use this as percent of peak height
            percent = float(new_baseline.rstrip("%"))
            baseline = height*percent/100.
        elif len(new_baseline) == 0:
            # if new_baseline is empty, baseline will by half-height
            baseline = float(height*.5)
        else:
            baseline = float(new_baseline)

        if height > baseline:
            baselines[peak_num] = baseline

        self.removeInfo(side+"_centroids")

    def setOffMerBaseline(self, quadrant, ind, new_baseline):
        """
        Set new baselines of off-meridian peaks by users. Remove other infos from self.info as it needs to be re-calculated.
        :param quadrant: "top_left", "top_right, "bottom_left", or "bottom_right"
        :param ind: peak number from left to right (int)
        :param new_baseline: new baseline value or percent of peak height (str or float)
        """
        new_baseline = str(new_baseline)
        hist = self.info["off_mer_hists"]["hulls"][quadrant]
        peak = self.info["off_mer_peaks"][quadrant][ind]
        baselines = self.info["off_mer_baselines"][quadrant]
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
            baselines[ind] = baseline

        self.removeInfo("off_mer_peak_info")

    def getOffMeridianRanges(self):
        """
        Get off-meridian ranges as position in the image from x1,x2,x3,x4 which are specified by users
        """
        if not self.info.has_key("x1"):
            centerX = self.info["center"][0]
            self.info["x1"] = centerX - self.init_off_mer["x1"]
            self.info["x2"] = centerX - self.init_off_mer["x2"]
            self.info["x3"] = centerX + self.init_off_mer["x3"]
            self.info["x4"] = centerX + self.init_off_mer["x4"]
            self.removeInfo("off_mer_rmin_rmax")

    def splitHist(self, center_y, hist, ignore):
        """
        Split histogram to top and bottom by center_y
        :param center_y: center location of histogram (int)
        :param hist: input histogram (list)
        :param ignore: ignored locations in histogram (list of boolean)
        :return: top_hist, top_ignore, bottom_hist, bottom_ignore (list)
        """
        top_hist = hist[center_y:]
        top_ignore = ignore[center_y:]
        bottom_hist = hist[:center_y]
        bottom_hist = bottom_hist[::-1]
        bottom_ignore = ignore[:center_y]
        bottom_ignore = bottom_ignore[::-1]
        return top_hist, top_ignore, bottom_hist, bottom_ignore

    def getOffMerRminmax(self):
        if not self.info.has_key('off_mer_rmin_rmax'):
            if self.info.has_key('fixed_offmer_hull_range'):
                self.info['off_mer_rmin_rmax'] = self.info['fixed_offmer_hull_range']
            else:
                rmin, rmax = self.initOffMeridianPeakRange()
                self.info['off_mer_rmin_rmax'] = (rmin, rmax)
            self.removeInfo("off_mer_hists")

    def getOffMeridianHistograms(self):
        """
        Produce histograms of off-meridian ranges.
        All histograms will be kept in self.info["off_mer_hists"]["hists"]
        All backgound subtracted histograms will be kept in self.info["off_mer_hists"]["hull"]
        """
        if not self.info.has_key("off_mer_hists"):
            center_y = self.info["center"][1]
            x1 = self.info["x1"]
            x2 = self.info["x2"]
            x3 = self.info["x3"]
            x4 = self.info["x4"]
            img = self.getRotatedImage()
            left_area = img[:, x2:x1]
            right_area = img[:, x3:x4]
            # left_ignore = np.array([any(line < 0) for line in left_area])
            # right_ignore = np.array([any(line < 0) for line in right_area])

            left_ignore = np.array([sum(np.array(line) < 0) > 0.1*len(line) for line in left_area])
            right_ignore = np.array([sum(np.array(line) < 0) > 0.1*len(line) for line in right_area])
            left_hist = np.sum(left_area, axis=1)
            right_hist = np.sum(right_area, axis=1)
            top_left_hist, top_left_ignore, bottom_left_hist, bottom_left_ignore = self.splitHist(center_y, left_hist,
                                                                                                  left_ignore)
            top_right_hist, top_right_ignore, bottom_right_hist, bottom_right_ignore = self.splitHist(center_y, right_hist,
                                                                                                  right_ignore)
            start, end = self.info['off_mer_rmin_rmax']
            top_left_hull = convexHull(top_left_hist, start_p=start, end_p=end, ignore=top_left_ignore)
            bottom_left_hull = convexHull(bottom_left_hist, start_p=start, end_p=end, ignore=bottom_left_ignore)
            top_right_hull = convexHull(top_right_hist, start_p=start, end_p=end, ignore=top_right_ignore)
            bottom_right_hull = convexHull(bottom_right_hist, start_p=start, end_p=end, ignore=bottom_right_ignore)

            self.info["off_mer_hists"] = {
                "hists" : {
                    "top_left" : np.array(top_left_hist),
                    "top_right": np.array(top_right_hist),
                    "bottom_left" : np.array(bottom_left_hist),
                    "bottom_right": np.array(bottom_right_hist)
                },
                "hulls": {
                    "top_left": np.array(top_left_hull),
                    "top_right": np.array(top_right_hull),
                    "bottom_left": np.array(bottom_left_hull),
                    "bottom_right": np.array(bottom_right_hull)
                }
            }
            self.removeInfo("off_mer_peaks")

    def getOffMeridianPeaks(self):
        """
        Get peak 51 and 59 from 4 quadrants by using peak ranges speicifed by users
        These peaks will be kept in self.info["off_mer_peaks"]
        This might affect baselines, so off_mer_baselines is removed from self.info
        """
        if not self.info.has_key("off_mer_peaks"):
            peaks = {}
            hulls = self.info["off_mer_hists"]["hulls"]
            peak_ranges = [("59",(self.init_off_mer["s59"], self.init_off_mer["e59"])),
                           ("51",(self.init_off_mer["s51"], self.init_off_mer["e51"]))]
            for k in hulls.keys():
                peaks[k] = self.getPeaksFromRanges(hulls[k], peak_ranges)
            self.info["off_mer_peaks"] = peaks
            self.removeInfo("off_mer_baselines")

    def getOffMeridianBaselines(self):
        """
        Get baselines of peak 51 and 59 from 4 quadrants. Init with half-height of peaks
        These baselines will be kept in self.info["off_mer_baselines"]
        This might affect peak infos, so off_mer_peak_info is removed from self.info
        """
        if not self.info.has_key("off_mer_baselines"):
            baselines = {}
            peaks = self.info["off_mer_peaks"]
            hulls = self.info["off_mer_hists"]["hulls"]
            for k in peaks.keys():
                baselines[k] = [hulls[k][p]*.5 for p in peaks[k]]
            self.info["off_mer_baselines"] = baselines
            self.removeInfo("off_mer_peak_info")

    def getOffMeridianInfos(self):
        """
        Get information of peak 51 and 59 from 4 quadrants including centroid, width, intersection with baseline, area (intensity)
        These info will be kept in self.info["off_mer_peak_info"]
        """
        if not self.info.has_key("off_mer_peak_info"):
            all_info = {}
            peaks = self.info["off_mer_peaks"]
            hulls = self.info["off_mer_hists"]["hulls"]
            baselines = self.info["off_mer_baselines"]

            for k in peaks.keys():
                peak_list = peaks[k]
                hull = hulls[k]
                baseline_list = baselines[k]
                results = getPeakInformations(hull, peak_list, baseline_list)
                all_info[k] = copy.copy(results)

            self.info["off_mer_peak_info"] = all_info

    def initOffMeridianPeakRange(self):
        """
        Find start and end points of peak 51 and 59 for applying convex hull
        """
        return int(round(0.9*self.init_off_mer["s59"])), int(round(1.1*self.init_off_mer["e51"]))

    def cacheInfo(self):
        """
        Save info dict to cache. Cache file will be save as filename.info in folder "qf_cache"
        :return: -
        """

        cache_path = fullPath(self.dir_path, 'dc_cache')

        if not exists(cache_path):
            makedirs(cache_path)

        self.info["program_version"] = self.version
        pickle.dump(self.info, open(self.cachefile, "wb"))
