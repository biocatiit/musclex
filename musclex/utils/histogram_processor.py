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

import numpy as np
import matplotlib.pyplot as plt

def convexHull(hist, start_p = 0, end_p = 99999999, ignore = None):
    """
    Apply 1D Convex hull to a histogram from left to right
    :param hist: input histogram (list or numpy array)
    :param start_p: start position of applying. The value of indexes before start_p will be 0 (int)
    :param end_p: end position of applying (int)
    :param ignore: specify ignore indexes in case the histogram has valleys from pilatus lines (list of boolean)
    :return: a histogram after convex hull is applied (list)
    """
    start_p = int(round(start_p))
    end_p = int(round(end_p))

    if end_p - start_p < 5 and (start_p !=0 or end_p != 99999999):
        return np.array(hist)

    hist = np.array(hist)

    if end_p > len(hist) :
        end_p = len(hist)

    hist_x = list(range(start_p, end_p))
    hist_y = np.array(hist[hist_x], dtype=np.float)

    if len(hist_x) < 5:
        return np.array(hist)

    hist_y2 = hist_y.copy()
    if ignore is not None:
        # hist2 = np.array(hist)
        # hist_y2 = smooth(hist2[hist_x], 5)
        ignore2 = ignore[hist_x]
        hist_y2[ignore2] = int(max(hist_y2)*1.5)
    # else:
    #     hist_y2 = smooth(hist_y, 5)

    hull_x, hull_y = getHull(hist_x, hist_y2)
    hull = getSubtractedHist(hist_x, hist_y, hull_x, hull_y)

    ret = list(np.zeros(start_p))
    ret.extend(hull)
    ret.extend(np.zeros(len(hist)-end_p))

    if ignore is not None:
        # i = 0
        # while i < len(ret):
        #     if ignore[i]:
        #         start_ind = i - 1
        #         while i < len(ret) and ignore[i]:
        #             i += 1
        #         start_ind = max(0, start_ind)
        #         end_ind = min(i, len(ret) - 1)
        #
        #         if start_ind == 0:
        #             sub = ret[end_ind]
        #         elif end_ind >= len(hist2) - 1:
        #             sub = np.empty(end_ind-start_ind+1)
        #             sub.fill(ret[start_ind])
        #             i = end_ind
        #         else:
        #             xs = np.linspace(0, end_ind - start_ind + 1, end_ind - start_ind + 1)
        #             val_s = ret[start_ind]
        #             val_e = ret[end_ind]
        #             m = (val_e - val_s) / len(xs)
        #             sub = np.array([m * x + val_s for x in xs])
        #         ret[start_ind: end_ind + 1] = sub
        #     i += 1
        sub = np.array(ret)
        sub[ignore] = 0
        ret = list(sub)

    return ret

def getHull(x_data, y_data):
    """
    Get Hull from histogram
    :param x_data: x values of the histogram (list)
    :param y_data: y values of the histogram (list)
    :return: x and y values of hull (list)
    """
    xhull = []
    yhull = []
    if len(x_data) == 0 or len(y_data) == 0:
        return xhull, yhull
    xhull.append(x_data[0])
    yhull.append(y_data[0])

    lasthullindex = 0

    points = len(y_data)
    while (lasthullindex < points - 1):
        slope = (y_data[lasthullindex + 1] - y_data[lasthullindex]) / (
            x_data[lasthullindex + 1] - x_data[lasthullindex])
        currenthullindex = lasthullindex + 1
        currenthully = y_data[lasthullindex]

        for i in range(currenthullindex + 1, points):
            extrapolation = currenthully + slope * (x_data[i] - x_data[lasthullindex])
            if y_data[i] < extrapolation:
                slope = ((y_data[i] - y_data[lasthullindex]) / (x_data[i] - x_data[lasthullindex]))
                currenthullindex = i

        # Store the hull points to be used for a spline fit
        xhull.append(x_data[currenthullindex])
        yhull.append(y_data[currenthullindex])
        lasthullindex = currenthullindex

    return xhull, yhull

def getSubtractedHist(xdata, ydata, xhull, yhull):
    """
    Apply Subtraction to original histogram by using a pchip line created from hull
    :param xdata: x values of original histogram (list)
    :param ydata: y values of original histogram (list)
    :param xhull: x values of hull (list)
    :param yhull: y values of hull (list)
    :return: Backgound subtracted histogram
    """
    if len(xdata) < 2 or len(ydata) < 2 or len(xhull) < 2 or len(yhull) < 2:
        return ydata

    if len(xhull) < 3 or len(yhull) < 3:
        segmentlx = xhull[0]
        segmentrx = xhull[1]
        leftindex = xdata.index(segmentlx)
        rightindex = xdata.index(segmentrx)
        segmently = ydata[leftindex]
        segmentry = ydata[rightindex]
        slope = (float(segmently) - float(segmentry)) / (float(leftindex) - float(rightindex))
        y_pchip = [segmently]
        for i in range(1, len(xdata)):
            val = segmently + slope * (i - leftindex)
            y_pchip.append(val)
    else:
        y_pchip = pchip(xhull, yhull, xdata) # Create a pchip line (curve) from hull

    suby = []
    for i in range(len(xdata)):
        val = ydata[i] - y_pchip[i]
        if val > 0:
            suby.append(ydata[i] - y_pchip[i])
        else:
            suby.append(0)
    return suby

def pchip(x, y, u):
    # calculate the first derivative at each section
    # there will be len(x)-1
    h = list()
    h0 = x[0]
    for h1 in x[1:]:
        h.append(h1 - h0)
        h0 = h1

    delta = list()
    for i in range(len(h)):
        delta.append((y[i + 1] - y[i]) / h[i])

    d = list()
    d.append(pchipend(h[0], h[1], delta[0], delta[1]))
    for i in range(1, len(x) - 1):
        d.append(pchipslopes(h[i - 1], h[i], delta[i - 1], delta[i]))

    d.append(pchipend(h[-1], h[-2], delta[-1], delta[-2]))

    # evaluate function
    pchipy = list()
    segmentlx = x[0]
    segmently = y[0]
    for i in range(len(delta)):
        segmentrx = x[i + 1]
        segmentry = y[i + 1]
        leftindex = u.index(segmentlx)
        rightindex = u.index(segmentrx)
        c = (3 * delta[i] - 2 * d[i] - d[i + 1]) / h[i]
        b = (d[i] - 2 * delta[i] + d[i + 1]) / (h[i] ** 2)
        dfloat = d[i]
        for j in u[leftindex:rightindex]:
            j = j - u[leftindex]
            pchipy.append(segmently + j * (dfloat + j * (c + j * b)))
        segmentlx = segmentrx
        segmently = segmentry

    # append the last point
    pchipy.append(y[-1])

    return pchipy

def pchipslopes(hm, h, deltam, delta):
    # PCHIPSLOPES  Slopes for shape-preserving Hermite cubic
    # pchipslopes(h,delta) computes d(k) = P(x(k)).
    #
    # Slopes at interior points
    # delta = diff(y)./diff(x).
    # d(k) = 0 if delta(k-1) and delta(k) have opposites
    #			signs or either is zero.
    # d(k) = weighted harmonic mean of delta(k-1) and
    #			delta(k) if they have the same sign.

    if sign(deltam) * sign(delta) > 0:
        w1 = 2 * h + hm
        w2 = h + 2 * hm
        return (w1 + w2) / (w1 / deltam + w2 / delta)
    else:
        return 0.0

def pchipend(h1, h2, del1, del2):
    # Noncentered, shape-preserving, three-point formula.
    d = ((2 * h1 + h2) * del1 - h1 * del2) / (h1 + h2)
    if sign(d) != sign(del1):
        d = 0
    elif (sign(del1) != sign(del2)) and (abs(d) > abs(3 * del1)):
        d = 3 * del1
    return d

def sign(d):
    if d > 0:
        return 1
    if d == 0:
        return 0
    if d < 0:
        return -1
    return None

#########################################################

def getPeaksFromHist(orig_hist, width_thres=0, height_thres=0):
    """
    Find peaks from histogram
    :param orig_hist: input histogram
    :param width_thres: Width threshold (default = 5)
    :param height_thres: Height threshold (default = mean value of histogram)
    :return: sorted peak list
    """
    if len(orig_hist) < 10:
        return []
    if width_thres == 0:
        width_thres = 5
    if height_thres == 0:
        height_thres = np.mean(orig_hist)

    hist = smooth(np.array(orig_hist), 20)
    hist[hist < height_thres] = height_thres
    hist = hist - min(hist)

    peak_hist1 = np.zeros((len(hist), 1))
    peak_list = []
    for i in range(4, len(hist) - 4):
        peak_hist1[i] = hist[i + 1] - hist[i - 1]

    for i in range(width_thres, len(peak_hist1) - width_thres):


        if all(peak_hist1[i - width_thres:i] > 0) and all(peak_hist1[i + 1:i + width_thres] < 0):
            peak_list.append(i)

    if len(peak_list) != 0:
        new_list = []
        derivate = np.zeros((len(peak_list), 1))
        for i in range(1, len(peak_list)):
            derivate[i] = peak_list[i] - peak_list[i - 1]
        i = 0
        while i < len(derivate) - 1:
            s = peak_list[i]
            c = 0
            if derivate[i + 1] == 1 and not i + 1 == len(derivate) - 1:
                j = i + 1
                while j <= len(derivate):
                    if derivate[j] == 1 and not j + 1 == len(derivate):
                        s += peak_list[j]
                        c += 1
                        j += 1
                    else:
                        s /= (c + 1)
                        i = j
                        break
            else:
                i += 1
            new_list.append(s)
        if not i == len(derivate) - 1:
            new_list.append(peak_list[i])
        peak_list = new_list

    peak_list.sort()

    return peak_list

def movePeaks(hist, peaks, dist=20):
    """
    Move peak to the local maximum point
    :param hist: input histogram
    :param peaks: approximate peak locations
    :param dist: maximum distance considered as local range
    :return: moved peaks
    """
    peakList = []
    smooth_hist = smooth(hist)
    # smooth_hist = hist[:]
    for pk in peaks:
        p = int(round(pk))
        while True:
            start = int(round(max(0, p - dist)))
            end = int(round(min(len(hist), p + dist)))
            if end < start:
                new_peak = p
                break
            new_peak = start + np.argmax(hist[int(start):int(end)])

            # if the local maximum is not far from initital peak, break
            if abs(p - new_peak) <= 5: #
                break
            else:
                left = min(p, new_peak)
                right = max(p, new_peak)

                # Check if between initial peak and local maximum has valley
                if all(smooth_hist[left + 1:right] > smooth_hist[p]):
                    break
            dist = dist / 2
        peakList.append(new_peak)
    return sorted(list(peakList))

def getFirstVallay(hist):
    """
    Find first valley from the radial histogram. This assumes that the first peak is the maximum peak.
    :param hist: input histogram
    :return: firstvalley
    """
    hist = smooth(hist)
    safe_range = (len(hist)//100, len(hist)//4)
    start = max(int(round(np.argmax(hist[:int(len(hist)/4)]))), 10) # Start searching from maximum peak
    start = min(start, safe_range[1]) # start point should be smaller than 50 pixel
    start = max(safe_range[0], start) # start point should be bigger than 20
    limit = min(int(start * 1.5), 100) # Set limit of the first valley as 150% of maximum peak location
    max_val = 0
    for i in range(start, limit):
        if hist[i] > max_val:
            max_val = hist[i]
        if hist[i] <= max_val*0.5:
            return i
    return limit

def getCentroid(hist, p, intersections):
    """
    Get centroid
    :param hist: input histogram (list)
    :param p: peak location (int)
    :param intersections: intersections of histogram and baseline at peak (tuple)
    :return: centroid
    """
    xs = np.arange(intersections[0], intersections[1] + 1)
    ys = hist[xs]
    numerator = np.dot(xs, ys)
    denominator = sum(ys)
    if denominator != 0:
        cent = numerator / denominator
    else:
        cent = p
    return cent

def getWidth(hist, max_loc, baseline):
    """
    Get peak width
    :param hist: input histogram
    :param max_loc: peak location
    :param baseline: baseline of peak
    :return: peak width
    """
    l = 1
    r = 1

    while max_loc - l > 0 and hist[max_loc - l] > baseline:
        l += 1
    while  max_loc + r < len(hist)-1 and hist[max_loc + r] > baseline:
        r += 1

    return l+r, (max_loc - l, max_loc + r)

def getPeakInformations(hist, peaks, baselines):
    """
    Get all peak informations including centroid, width, intersection with baseline, area (intensity)
    :param hist: input histogram (list)
    :param peaks: peak locations (list)
    :param baselines: baselines of peaks (list)
    :return: result with infomations (dict)
    """
    # self.fitModel(hist, peaks, baselines)
    centroids = []
    widths = []
    intersectionsList = []
    areas = []

    for i in range(len(peaks)):
        p = peaks[i]
        baseline = baselines[i]
        width, intersections = getWidth(hist, p, baseline)
        cent = getCentroid(hist, p, intersections)
        areas.append(hist[p] * width / (2.35 * 0.3989))
        centroids.append(cent)
        widths.append(width)
        intersectionsList.append(intersections)
    results = {}
    results["centroids"] = centroids
    results["widths"] = widths
    results["intersections"] = intersectionsList
    results["areas"] = areas
    return results

def smooth(x, window_len=10, window='hanning'): # From http://scipy-cookbook.readthedocs.io/items/SignalSmooth.html
    """smooth the data using a window with requested size.

    This method is based on the convolution of a scaled window with the signal.
    The signal is prepared by introducing reflected copies of the signal
    (with the window size) in both ends so that transient parts are minimized
    in the begining and end part of the output signal.

    input:
        x: the input signal
        window_len: the dimension of the smoothing window
        window: the type of window from 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'
            flat window will produce a moving average smoothing.

    output:
        the smoothed signal

    example:

    import numpy as np
    t = np.linspace(-2,2,0.1)
    x = np.sin(t)+np.random.randn(len(t))*0.1
    y = smooth(x)

    see also:

    numpy.hanning, numpy.hamming, numpy.bartlett, numpy.blackman, numpy.convolve
    scipy.signal.lfilter

    TODO: the window parameter could be the window itself if an array instead of a string
    """
    x = np.array(x)
    if x.ndim != 1:
        raise ValueError("smooth only accepts 1 dimension arrays.")

    if x.size < window_len:
        window_len = int(x.size/3)
        # raise ValueError, "Input vector needs to be bigger than window size."

    if window_len < 3:
        return x

    if not window in ['flat', 'hanning', 'hamming', 'bartlett', 'blackman']:
        raise ValueError("Window is on of 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'")

    s=np.r_[2*x[0]-x[window_len:1:-1], x, 2*x[-1]-x[-1:-window_len:-1]]
    #print(len(s))

    if window == 'flat': #moving average
        w = np.ones(window_len,'d')
    else:
        w = getattr(np, window)(window_len)
    y = np.convolve(w/w.sum(), s, mode='same')
    return y[window_len-1:-window_len+1]
