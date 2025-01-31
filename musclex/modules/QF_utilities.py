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

from numba import jit, cuda
from math import exp, sqrt, floor, ceil, atan
import numpy as np

#@jit(target_backend='cuda', nopython=True)
@jit
def get_avg_fold_float32(quadrants, nQuadrant, fold_height, fold_width, threshold):
    result = np.zeros((fold_height, fold_width))
    if nQuadrant > 0:
        for x in range(fold_width):
            for y in range(fold_height):
                sum_val = 0.0
                n_fold = 0
                for i in range(nQuadrant):
                    fold = quadrants[i]
                    if fold[y,x] > threshold:
                        # Case where there is an edge effect near gaps creating thin lines on the image
                        # Neighbor pixels considered as part of the gap: 2
                        if 2 < y < fold_height-2 and 2 < x < fold_width-2:
                            not_gap_edge = True
                            for k in range(y-2, y+3):
                                for l in range(x-2, x+3):
                                    if fold[k,l] < threshold:
                                        not_gap_edge = False
                            if not_gap_edge:
                                sum_val += fold[y,x]
                                n_fold += 1
                        else:
                            sum_val += fold[y,x]
                            n_fold += 1
                if n_fold == 0 :
                    result[y,x] = 0
                else:
                    result[y,x] = sum_val/n_fold
    return result

#@jit(target_backend='cuda', nopython=True)
@jit
def createAngularBG(width, height, subtr, nBins):
    backgound = np.zeros((height, width), dtype = np.float32)
    centerX = width - 1
    centerY = height - 1
    theta_size = 90./nBins

    for x in range(0, width):
        for y in range(0, height):
            rad = qfdistance(centerX, centerY, x, y)
            floor_rad = floor(rad)
            ceil_rad = ceil(rad)
            ifloor_rad = int(floor_rad)
            iceil_rad = int(ceil_rad)

            if ifloor_rad == iceil_rad:
                beta_rad = 0.5
                alpha_rad = 0.5
            else:
                alpha_rad = 1. - (rad - floor_rad)
                beta_rad = 1. - (ceil_rad - rad)

            deltax = float(abs(x - centerX))
            if deltax == 0.0:
                deg = 90.0
            else:
                deltay = float(abs(y - centerY))
                slope = deltay / deltax
                deg = atan(slope)*180.0/np.pi

            fbin = 1.*deg/theta_size
            ibin = int(round(fbin))

            if ibin == 0:
                backgound[y, x] = alpha_rad*subtr[ibin, ifloor_rad] + beta_rad*subtr[ibin, iceil_rad]
            elif ibin == nBins:
                backgound[y, x] = alpha_rad*subtr[ibin-1, ifloor_rad] + beta_rad*subtr[ibin-1, iceil_rad]
            else:
                floor_bin = floor(fbin)
                ceil_bin = ceil(fbin)
                alpha = 1. - (fbin - floor_bin)
                beta = 1. - (ceil_bin - fbin)

                if alpha == 1.0 and beta == 1.0:
                    alpha = 0.5
                    beta = 0.5
                    ifloor = int(floor_bin - 1.0)
                    iceil = int(ceil_bin)
                elif alpha > beta :
                    floor_bin = floor_bin - 0.5
                    ceil_bin = ceil_bin - 0.5
                    alpha = 1. - (fbin - floor_bin)
                    beta = 1. - (ceil_bin - fbin)
                    ifloor = int(floor(floor_bin))
                    iceil = int(floor(ceil_bin))
                else:
                    floor_bin = floor_bin + 0.5
                    ceil_bin = ceil_bin + 0.5
                    alpha = 1. - (fbin - floor_bin)
                    beta = 1. - (ceil_bin - fbin)
                    ifloor = int(floor(floor_bin))
                    iceil = int(floor(ceil_bin))

                backgound[y, x] = alpha * (alpha_rad * subtr[ifloor, ifloor_rad] + beta_rad * subtr[ifloor, iceil_rad])+ beta * (alpha_rad * subtr[iceil, ifloor_rad] + beta_rad * subtr[iceil, iceil_rad])

    return backgound

#@jit(target_backend='cuda', nopython=True)
@jit
def createCircularlySymBG(width, height, spline):
    backgound = np.zeros((height, width), dtype = np.float32)
    centerX = width - 0.5
    centerY = height - 0.5

    for x in range(width):
        for y in range(height):
            fx = float(x)
            fy = float(y)
            rad = sqrt((fx-centerX)**2+(fy-centerY)**2)
            ffloor = floor(rad)
            fceil = ceil(rad)
            alpha = 1.-(rad-ffloor)
            beta = 1.-(fceil-rad)
            ifloor = int(ffloor)
            iceil = int(fceil)

            if ifloor == iceil:
                alpha = 0.5
                beta = 0.5

            backgound[y, x] = alpha*spline[ifloor] + beta*spline[iceil]
    return backgound

#@jit(target_backend='cuda', nopython=True)
@jit
def replaceRmin(img, rmin, val):
    height = img.shape[0]
    width = img.shape[1]
    centerX = width
    centerY = height
    frmin = float(rmin)
    replace_val = float(val)

    for x in range(width-rmin-1, width):
        float_x = float(x)
        for y in range(height-rmin-1, height):
            float_y = float(y)
            distance = sqrt((float_x-centerX)**2+(float_y-centerY)**2)
            if distance <= frmin:
                img[y, x] = replace_val
    return img

#@jit(target_backend='cuda', nopython=True)
@jit
def getCircularDiscreteBackground(img, rmin, start_p, end_p, radial_bin, nBin, max_pts):
    height = img.shape[0]
    width = img.shape[1]
    xs = np.zeros(nBin, dtype = np.float32)
    ys = np.zeros(nBin, dtype = np.float32)
    all_pts = np.zeros(max_pts, dtype = np.float32)
    centerX = width - 0.5
    centerY = height - 0.5
    nPoints = 0

    for bin in range(0, nBin):
        nPoints = 0
        d1 = float(rmin + bin*radial_bin)
        d2 = d1 + float(radial_bin)

        # Get all points in a bin
        for x in range(width):
            float_x = float(x)
            for y in range(height):
                float_y = float(y)
                distance = sqrt((float_x-centerX)**2+(float_y-centerY)**2)
                if d1 <= distance and distance < d2 and nPoints < max_pts:
                    all_pts[nPoints] = img[y, x]
                    nPoints = nPoints+1

        # Sort all pixels
        sorted_pts = all_pts[:nPoints]
        sorted_pts.sort()

        # Get background value from all points (between percentage of start_p and end_p)
        start_ind = int(round(float(nPoints)*start_p/100))
        end_ind = int(round(float(nPoints)*end_p/100.))

        if start_ind < end_ind:
            sumVal = 0.0
            for i in range(start_ind, end_ind):
                sumVal = sumVal + sorted_pts[i]
            ys[bin] = sumVal/float(end_ind-start_ind)
        else:
            ys[bin] = all_pts[start_ind]

        xs[bin] = (d1+d2)/2.
    return xs, ys

#@jit(target_backend='cuda', nopython=True)
@jit
def make2DConvexhullBG2(pchipLines, width, height, centerX, centerY, rmin, rmax, step):
    backgound = np.zeros((height, width), dtype = np.float32)
    zero = 0.0

    for x in range(width):
        for y in range(height):

            rad = qfdistance(centerX, centerY, x, y)
            ceil_rad = ceil(rad)
            floor_rad = floor(rad)
            irad_ceil = int(ceil_rad)
            irad_floor = int(floor_rad)

            if irad_floor == irad_ceil:
                beta_rad = 0.5
                alpha_rad = 0.5
            else:
                alpha_rad = 1. - (rad - floor_rad)
                beta_rad = 1. - (ceil_rad - rad)

            deltax = float(abs(x - centerX))
            if deltax == zero:
                deg = 90.0 * (1/step)
            else:
                deltay = float(abs(y - centerY))
                slope = deltay / deltax
                deg = atan(slope)*180.0/np.pi
                deg = deg * (1/step)

            floor_deg = floor(deg)
            ceil_deg = ceil(deg)
            ifloor = int(floor_deg)
            iceil = int(ceil_deg)

            if ifloor == iceil:
                alpha = 0.5
                beta = 0.5
            else:
                alpha = 1. - (deg - floor_deg)
                beta = 1. - (ceil_deg - deg)

            if irad_ceil < rmax and irad_floor >= rmin:
                pos1 = irad_floor - rmin
                pos2 = irad_ceil - rmin
                backgound[y, x] = alpha * (alpha_rad * pchipLines[ifloor, pos1] + beta_rad * pchipLines[ifloor, pos2]) \
                                  + beta * (alpha_rad * pchipLines[iceil, pos1] + beta_rad * pchipLines[iceil, pos2])
    return backgound

#@jit(target_backend='cuda', nopython=True)
@jit
def combine_bgsub_float32(img1, img2, center_x, center_y, sigmoid_k, radius):
    img_height = img1.shape[0]
    img_width = img1.shape[1]
    result = np.zeros((img_height, img_width), dtype = np.float32)

    for x in range(img_width):
        for y in range(img_height):
            r = qfdistance(x, y, center_x, center_y)
            tophat_ratio = sigmoid(sigmoid_k, radius, r)
            radial_ratio = 1.0 - tophat_ratio
            tophat_val = tophat_ratio * img2[y,x]
            radial_val = radial_ratio * img1[y,x]
            result[y,x] = tophat_val+radial_val
    return result

@jit
def combine_bgsub_linear_float32(img1, img2, center_x, center_y, rad, delta):
    img_height = img1.shape[0]
    img_width = img1.shape[1]
    result = np.zeros((img_height, img_width), dtype = np.float32)

    for x in range(img_width):
        for y in range(img_height):
            r = qfdistance(x, y, center_x, center_y)
            tophat_ratio = linear(rad, delta, r)
            radial_ratio = 1.0 - tophat_ratio
            tophat_val = tophat_ratio * img2[y,x]
            radial_val = radial_ratio * img1[y,x]
            result[y,x] = tophat_val+radial_val
    return result


@jit
def combine_bgsub_float32_debug(img1, img2, center_x, center_y, sigmoid_k, radius):
    img_height = img1.shape[0]
    img_width = img1.shape[1]
    result = np.zeros((img_height, img_width), dtype = np.float32)
    debug1 = np.zeros((img_height, img_width), dtype = np.float32)
    debug2 = np.zeros((img_height, img_width), dtype = np.float32)
    debug3 = np.zeros((img_height, img_width), dtype = np.float32)
    debug4 = np.zeros((img_height, img_width), dtype = np.float32)
    debug5 = np.zeros((img_height, img_width), dtype = np.float32)

    for x in range(img_width):
        for y in range(img_height):
            r = qfdistance(x, y, center_x, center_y)
            tophat_ratio = sigmoid(sigmoid_k, radius, r)
            radial_ratio = 1.0 - tophat_ratio
            tophat_val = tophat_ratio * img2[y,x]
            radial_val = radial_ratio * img1[y,x]
            result[y,x] = tophat_val+radial_val

            debug1[y,x] = tophat_ratio
            debug2[y,x] = radial_ratio
            debug3[y,x] = tophat_val
            debug4[y,x] = radial_val
            debug5[y,x] = r

    return result, debug1, debug2, debug3, debug4, debug5

@jit
def combine_bgsub_float32_debug_v2(img1, img2, center_x, center_y, rad, delta):
    img_height = img1.shape[0]
    img_width = img1.shape[1]
    result = np.zeros((img_height, img_width), dtype = np.float32)
    debug1 = np.zeros((img_height, img_width), dtype = np.float32)
    debug2 = np.zeros((img_height, img_width), dtype = np.float32)
    debug3 = np.zeros((img_height, img_width), dtype = np.float32)
    debug4 = np.zeros((img_height, img_width), dtype = np.float32)
    debug5 = np.zeros((img_height, img_width), dtype = np.float32)

    for x in range(img_width):
        for y in range(img_height):
            r = qfdistance(x, y, center_x, center_y)
            tophat_ratio = linear(rad, delta, r)
            radial_ratio = 1.0 - tophat_ratio
            tophat_val = tophat_ratio * img2[y,x]
            radial_val = radial_ratio * img1[y,x]
            result[y,x] = tophat_val+radial_val

            debug1[y,x] = tophat_ratio
            debug2[y,x] = radial_ratio
            debug3[y,x] = tophat_val
            debug4[y,x] = radial_val
            debug5[y,x] = r

    return result, debug1, debug2, debug3, debug4, debug5


#@jit(target_backend='cuda', nopython=True)
@jit
def qfdistance(x1, y1, x2, y2):
    return sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)

#@jit(target_backend='cuda', nopython=True)
@jit
def sigmoid(k, x0, x):
    return 1.0 / (1.0 + exp(-k * (x - x0)))

@jit
def linear(rad, delta, x):
    result = (x - rad + delta//2) / (delta)
    return max(0.0, min(1.0, result))