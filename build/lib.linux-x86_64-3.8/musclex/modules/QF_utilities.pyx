#cython: boundscheck=False, wraparound=False, nonecheck=False
__author__ = 'Jiranun.J'
import numpy as np
cimport numpy as np
cimport cython
from libc.math cimport sqrt
from libc.math cimport exp

@cython.boundscheck(False)
@cython.wraparound(False)

#
# "ctypedef" assigns a corresponding compile-time type to np.uint8_t. For
# every type in the np module there's a corresponding compile-time
# type with a _t-suffix.
cpdef float distance(int x1, int y1, int x2, int y2):
    return sqrt((x1-x2)**2+(y1-y2)**2)


def get_avg_fold_int16(np.ndarray[np.uint16_t, ndim=3] quadrants, int nQuadrant, int fold_height, int fold_width, double threshold):
    cdef np.ndarray[double, ndim=2] result = np.zeros((fold_height, fold_width))
    cdef Py_ssize_t x, y, n_fold, i
    cdef double sum_val
    cdef np.ndarray[np.uint16_t, ndim=2] fold

    # cdef int end = center_y + box_width
    if nQuadrant > 0:
        for x in range(fold_width):
            for y in range(fold_height):
                sum_val = 0.0
                n_fold = 0
                for i in range(nQuadrant):
                    fold = quadrants[i]
                    if fold[y,x] > threshold :
                        sum_val += fold[y,x]
                        n_fold += 1
                if n_fold == 0 :
                    result[y,x] = 0
                else:
                    result[y,x] = sum_val/n_fold
    return result

def get_avg_fold_int8(np.ndarray[np.uint8_t, ndim=3] quadrants, int nQuadrant, int fold_height, int fold_width, int threshold):
    cdef np.ndarray[double, ndim=2] result = np.zeros((fold_height, fold_width))
    cdef Py_ssize_t x, y, n_fold, i
    cdef double sum_val
    cdef np.ndarray[np.uint8_t, ndim=2] fold

    # cdef int end = center_y + box_width
    if nQuadrant > 0:
        for x in range(fold_width):
            for y in range(fold_height):
                sum_val = 0.0
                n_fold = 0
                for i in range(nQuadrant):
                    fold = quadrants[i]
                    if fold[y,x] > threshold :
                        sum_val += fold[y,x]
                        n_fold += 1
                if n_fold == 0 :
                    result[y,x] = 0
                else:
                    result[y,x] = sum_val/n_fold
    return result

def get_avg_fold_float32(np.ndarray[np.float32_t, ndim=3] quadrants, int nQuadrant, int fold_height, int fold_width, np.float32_t threshold):
    cdef np.ndarray[double, ndim=2] result = np.zeros((fold_height, fold_width))
    cdef Py_ssize_t x, y, n_fold, i
    cdef double sum_val
    cdef np.ndarray[np.float32_t, ndim=2] fold

    # cdef int end = center_y + box_width
    if nQuadrant > 0:
        for x in range(fold_width):
            for y in range(fold_height):
                sum_val = 0.0
                n_fold = 0
                for i in range(nQuadrant):
                    fold = quadrants[i]
                    if fold[y,x] > threshold :
                        sum_val += fold[y,x]
                        n_fold += 1
                if n_fold == 0 :
                    result[y,x] = 0
                else:
                    result[y,x] = sum_val/n_fold
    return result

#
# def apply_radial_bgsub_int8(np.ndarray[np.uint8_t, ndim=2] img, np.ndarray[np.uint8_t, ndim=2] smooth_img, int center_x, int center_y, int radius, int threshold):
#     cdef int img_height = img.shape[0]
#     cdef int img_width = img.shape[1]
#     cdef np.ndarray[np.uint8_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.uint8)
#     cdef np.ndarray[np.uint8_t, ndim=2] radial_bgsub = np.full((radius,1), np.max(img), dtype = np.uint8)
#     cdef Py_ssize_t x, y
#     cdef int r
#     cdef np.uint8_t bgsub
#     cdef np.uint8_t val, min
#     cdef np.uint8_t zero = 0
#
#     for x in range(img_width):
#         for y in range(img_height):
#             val = smooth_img[y,x]
#             if val >= threshold:
#                 r = int(distance( x, y, center_x, center_y))
#                 min = radial_bgsub[r]
#                 if min > val:
#                     radial_bgsub[r] = val
#
#     for x in range(img_width):
#         for y in range(img_height):
#             val = img[y,x]
#             if val >= threshold:
#                 bgsub = 0
#                 r = int(distance( x, y, center_x, center_y))
#                 if radius > r:
#                     bgsub = radial_bgsub[r]
#
#                 if bgsub <= val:
#                     result[y,x] = val - bgsub
#                 else:
#                     result[y,x] = zero
#             else:
#                result[y,x] = val
#
#     return result
#
#
# def apply_radial_bgsub_int16(np.ndarray[np.uint16_t, ndim=2] img, np.ndarray[np.uint16_t, ndim=2] smooth_img, int center_x, int center_y, int radius, int threshold):
#     cdef int img_height = img.shape[0]
#     cdef int img_width = img.shape[1]
#     cdef np.ndarray[np.uint16_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.uint16)
#     cdef np.ndarray[np.uint16_t, ndim=2] radial_bgsub = np.full((radius,1), np.max(img), dtype = np.uint16)
#     cdef Py_ssize_t x, y
#     cdef int r
#     cdef np.uint16_t bgsub
#     cdef np.uint16_t val, min
#     cdef np.uint16_t zero = 0
#
#     for x in range(img_width):
#         for y in range(img_height):
#             val = smooth_img[y,x]
#             if val >= threshold:
#                 r = int(distance( x, y, center_x, center_y))
#                 min = radial_bgsub[r]
#                 if min > val:
#                     radial_bgsub[r] = val
#
#     for x in range(img_width):
#         for y in range(img_height):
#             val = img[y,x]
#             if val >= threshold:
#                 bgsub = 0
#                 r = int(distance( x, y, center_x, center_y))
#                 if radius > r:
#                     bgsub = radial_bgsub[r]
#
#                 if bgsub <= val:
#                     result[y,x] = val - bgsub
#                 else:
#                     result[y,x] = zero
#             else:
#                result[y,x] = val
#
#     return result


def apply_radial_bgsub_float32(np.ndarray[np.float32_t, ndim=2] img, np.ndarray[np.float32_t, ndim=2] smooth_img, int center_x, int center_y, int radius, int threshold):
    cdef int img_height = img.shape[0]
    cdef int img_width = img.shape[1]
    cdef np.ndarray[np.float32_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.float32)
    cdef np.ndarray[np.float32_t, ndim=2] radial_bgsub = np.full((radius,1), np.max(img), dtype = np.float32)
    cdef Py_ssize_t x, y
    cdef int r
    cdef np.float32_t bgsub
    cdef np.float32_t val, min
    cdef np.float32_t zero = 0.0

    for x in range(img_width):
        for y in range(img_height):
            val = smooth_img[y,x]
            if val >= threshold:
                r = int(distance( x, y, center_x, center_y))
                min = radial_bgsub[r]
                if min > val:
                    radial_bgsub[r] = val

    for x in range(img_width):
        for y in range(img_height):
            val = img[y,x]
            if val >= threshold:
                bgsub = 0.0
                r = int(distance( x, y, center_x, center_y))
                if radius > r:
                    bgsub = radial_bgsub[r]

                if bgsub <= val:
                    result[y,x] = val - bgsub
                else:
                    result[y,x] = zero
            else:
               result[y,x] = val

    return result


cdef float sigmoid(float k, int x0, float x):
    return 1.0/(1.0+exp(-k*(x-x0)))


def combine_bgsub_float32(np.ndarray[np.float32_t, ndim=2] img1, np.ndarray[np.float32_t, ndim=2] img2, int center_x, int center_y, float sigmoid_k, int radius):
    cdef int img_height = img1.shape[0]
    cdef int img_width = img1.shape[1]
    cdef np.ndarray[np.float32_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.float32)
    cdef int x, y
    cdef np.float32_t r
    cdef np.float32_t tophat_ratio, radial_ratio
    cdef np.float32_t tophat_val, radial_val, th_tmp

    for x in range(img_width):
        for y in range(img_height):
            r = distance( x, y, center_x, center_y)
            tophat_ratio = sigmoid(sigmoid_k, radius, r)
            radial_ratio = 1.0 - tophat_ratio
            tophat_val = tophat_ratio * img2[y,x]
            radial_val = radial_ratio * img1[y,x]
            result[y,x] = tophat_val+radial_val
    return result

# def combine_bgsub_uint16(np.ndarray[np.uint16_t, ndim=2] tophat_img, np.ndarray[np.uint16_t, ndim=2] radial_img, int center_x, int center_y, float sigmoid_k, int radius, int threshold):
#     cdef int img_height = tophat_img.shape[0]
#     cdef int img_width = tophat_img.shape[1]
#     cdef np.ndarray[np.uint16_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.uint16)
#     cdef Py_ssize_t x, y
#     cdef int r
#     cdef float tophat_ratio, radial_ratio
#     cdef np.uint16_t tophat_val, radial_val, th_tmp
#
#     for x in range(img_width):
#         for y in range(img_height):
#             th_tmp = tophat_img[y,x]
#             if th_tmp >= threshold:
#                 r = int(distance( x, y, center_x, center_y))
#                 tophat_ratio = sigmoid(sigmoid_k, radius, r)
#                 radial_ratio = 1.0 - tophat_ratio
#                 tophat_val = int(tophat_ratio * th_tmp)
#                 radial_val = int(radial_ratio * radial_img[y,x])
#                 result[y,x] = tophat_val+radial_val
#             else:
#                 result[y,x] = th_tmp
#
#     return result
#
# def combine_bgsub_uint8(np.ndarray[np.uint8_t, ndim=2] tophat_img, np.ndarray[np.uint8_t, ndim=2] radial_img, int center_x, int center_y, float sigmoid_k, int radius, int threshold):
#     cdef int img_height = tophat_img.shape[0]
#     cdef int img_width = tophat_img.shape[1]
#     cdef np.ndarray[np.uint8_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.uint8)
#     cdef Py_ssize_t x, y
#     cdef int r
#     cdef float tophat_ratio, radial_ratio
#     cdef np.uint8_t tophat_val, radial_val, th_tmp
#
#     for x in range(img_width):
#         for y in range(img_height):
#             th_tmp = tophat_img[y,x]
#             if th_tmp >= threshold:
#                 r = int(distance( x, y, center_x, center_y))
#                 tophat_ratio = sigmoid(sigmoid_k, radius, r)
#                 radial_ratio = 1.0 - tophat_ratio
#                 tophat_val = int(tophat_ratio * th_tmp)
#                 radial_val = int(radial_ratio * radial_img[y,x])
#                 result[y,x] = tophat_val+radial_val
#             else:
#                 result[y,x] = th_tmp
#
#     return result


def getCirSubtr_f32(np.ndarray[np.float32_t, ndim=2] img, np.ndarray[np.float32_t, ndim=1] subtr_hist, int radius):
    cdef int img_height = img.shape[0]
    cdef int img_width = img.shape[1]
    cdef np.ndarray[np.float32_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.float32)
    cdef Py_ssize_t x, y
    cdef int r
    cdef np.float32_t bgsub
    cdef np.float32_t val, min
    cdef np.float32_t zero = 0.0

    for x in range(img_width):
        for y in range(img_height):
            val = img[y,x]
            bgsub = 0.0
            r = int(distance( x, y, img_width, img_height))
            if radius > r:
                bgsub = subtr_hist[r]

            result[y, x] = val - bgsub
            # if bgsub <= val:
            #     result[y,x] = val - bgsub
            # else:
            #     result[y,x] = zero
    return result

def getCirSubtr_i16(np.ndarray[np.uint16_t, ndim=2] img, np.ndarray[np.uint16_t, ndim=1] subtr_hist, int radius):
    cdef int img_height = img.shape[0]
    cdef int img_width = img.shape[1]
    cdef np.ndarray[np.uint16_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.uint16)
    cdef Py_ssize_t x, y
    cdef int r
    cdef np.uint16_t bgsub
    cdef np.uint16_t val, min
    cdef np.uint16_t zero = 0

    for x in range(img_width):
        for y in range(img_height):
            val = img[y,x]
            bgsub = 0
            r = int(distance( x, y, img_width, img_height))
            if radius > r:
                bgsub = subtr_hist[r]

            # result[y, x] = val - bgsub
            if bgsub <= val:
                result[y,x] = val - bgsub
            else:
                result[y,x] = zero
    return result

def getCirSubtr_i8(np.ndarray[np.uint8_t, ndim=2] img, np.ndarray[np.uint8_t, ndim=1] subtr_hist, int radius):
    cdef int img_height = img.shape[0]
    cdef int img_width = img.shape[1]
    cdef np.ndarray[np.uint8_t, ndim=2] result = np.zeros((img_height, img_width), dtype = np.uint8)
    cdef Py_ssize_t x, y
    cdef int r
    cdef np.uint8_t bgsub
    cdef np.uint8_t val, min
    cdef np.uint8_t zero = 0

    for x in range(img_width):
        for y in range(img_height):
            val = img[y,x]
            bgsub = 0
            r = int(distance( x, y, img_width, img_height))
            if radius > r:
                bgsub = subtr_hist[r]

            # result[y, x] = val - bgsub
            if bgsub <= val:
                result[y,x] = val - bgsub
            else:
                result[y,x] = zero
    return result

from libc.math cimport abs, M_PI
from libc.math cimport round
from libc.math cimport atan
from libc.math cimport floor
from libc.math cimport ceil
# from numpy.math cimport rad2deg

@cython.cdivision(True)
def make2DConvexhullBG(np.ndarray[np.float32_t, ndim=2] pchipLines, int width, int height, int centerX, int centerY, int rmin, int rmax):
    cdef np.ndarray[np.float32_t, ndim=2] backgound = np.zeros((height, width), dtype = np.float32)
    cdef int x, y
    cdef int irad, irad_floor, irad_ceil
    cdef np.float32_t deg, slope, floor_deg, alpha, ceil_deg, beta, round_deg, deltay, deltax, subv, tmpdeg
    cdef np.float32_t rad, ceil_rad, floor_rad, alpha_rad, beta_rad
    cdef np.float32_t zero = 0.0
    cdef int intzero = 0
    cdef int pos1, pos2
    cdef int ifloor, iceil, ideg

    for x in range(width):
        for y in range(height):
            deltax = float(abs(x - centerX))
            if deltax == zero:
                deg = 90.0
            else:
                deltay = float(abs(y - centerY))
                slope = deltay / deltax
                deg = atan(slope)*180.0/M_PI

            rad = distance(centerX, centerY, x, y)
            irad = int(round(rad))
            ceil_rad = ceil(rad)
            floor_rad = floor(rad)
            irad_ceil = int(ceil_rad)
            irad_floor = int(floor_rad)
            alpha_rad = 1. - (rad-floor_rad)
            beta_rad = 1. - (ceil_rad-rad)

            round_deg = round(deg)

            if round_deg < deg:
                ceil_deg = round_deg + 0.5
                floor_deg = round_deg
            elif round_deg > deg:
                ceil_deg = round_deg
                floor_deg = round_deg - 0.5
            else:
                ceil_deg = round_deg
                floor_deg = round_deg

            # alpha = 1. - ((deg - floor_deg) * 2.0)
            # beta = 1. - ((ceil_deg - deg) * 2.0)

            ifloor = int(floor_deg / 0.5)
            iceil = int(ceil_deg / 0.5)
            tmpdeg = deg/0.5
            alpha = 1. - (tmpdeg - floor_deg / 0.5)
            beta = 1. - (ceil_deg / 0.5 - tmpdeg)
            ideg = int(round(deg/0.5))

            if irad_ceil <= rmax and irad_floor >= rmin:
                if alpha == 1. and beta == 1.:
                    if alpha_rad == 1. and beta_rad == 1.:
                        pos1 = irad - rmin
                        backgound[y,x] = pchipLines[ideg, pos1]
                    else:
                        pos1 = irad_floor - rmin
                        pos2 = irad_ceil - rmin
                        backgound[y, x] = alpha_rad * pchipLines[ideg, pos1] + beta_rad * pchipLines[ideg, pos2]
                else:
                    if alpha_rad == 1. and beta_rad == 1.:
                        pos1 = irad - rmin
                        backgound[y,x] = alpha * pchipLines[ifloor, pos1] + beta * pchipLines[iceil, pos1]
                    else:
                        pos1 = irad_floor - rmin
                        pos2 = irad_ceil - rmin
                        backgound[y, x] = alpha * (alpha_rad * pchipLines[ifloor, pos1] + beta_rad * pchipLines[ifloor, pos2]) \
                                          + beta * (alpha_rad * pchipLines[iceil, pos1] + beta_rad * pchipLines[iceil, pos2])
    return backgound

@cython.cdivision(True)
def make2DConvexhullBG2(np.ndarray[np.float32_t, ndim=2] pchipLines, int width, int height, int centerX, int centerY, int rmin, int rmax):
    cdef np.ndarray[np.float32_t, ndim=2] backgound = np.zeros((height, width), dtype = np.float32)
    cdef int x, y
    cdef int irad, irad_floor, irad_ceil
    cdef np.float32_t deg, slope, floor_deg, alpha, ceil_deg, beta, round_deg, deltay, deltax, subv, tmpdeg
    cdef np.float32_t rad, ceil_rad, floor_rad, alpha_rad, beta_rad
    cdef np.float32_t zero = 0.0
    cdef int intzero = 0
    cdef int pos1, pos2
    cdef int ifloor, iceil, ideg

    for x in range(width):
        for y in range(height):

            rad = distance(centerX, centerY, x, y)
            irad = int(round(rad))
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
                deg = 90.0
            else:
                deltay = float(abs(y - centerY))
                slope = deltay / deltax
                deg = atan(slope)*180.0/M_PI

            ideg = int(round(deg))
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


@cython.cdivision(True)
def createAngularBG(int width, int height, np.ndarray[np.float32_t, ndim=2] subtr, int nBins):
    cdef np.ndarray[np.float32_t, ndim=2] backgound = np.zeros((height, width), dtype = np.float32)
    cdef int x,y, irad, ideg
    cdef int centerX = width - 1
    cdef int centerY = height - 1
    cdef np.float32_t rad, deg, slope, deltax, deltay, bgsub, fbin, floor_bin, ceil_bin, alpha, beta, floor_rad, ceil_rad, alpha_rad, beta_rad
    cdef np.float32_t theta_size = 90./nBins
    cdef int ifloor, iceil, ibin, ifloor_rad, iceil_rad

    for x in range(0, width):
        for y in range(0, height):
            rad = distance(centerX, centerY, x, y)
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

            irad = int(round(rad))

            deltax = float(abs(x - centerX))
            bgsub = 0.0
            if deltax == 0.0:
                deg = 90.0
            else:
                deltay = float(abs(y - centerY))
                slope = deltay / deltax
                deg = atan(slope)*180.0/M_PI

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

@cython.cdivision(True)
def createCircularlySymBG(int width, int height, np.ndarray[np.float32_t, ndim=1] spline):
    cdef np.ndarray[np.float32_t, ndim=2] backgound = np.zeros((height, width), dtype = np.float32)
    cdef int x, y, ifloor, iceil, irad
    cdef np.float32_t centerX = width - 0.5
    cdef np.float32_t centerY = height - 0.5
    cdef np.float32_t rad, alpha, beta, ffloor, fceil
    cdef np.float32_t fx, fy

    for x in range(width):
        for y in range(height):
            fx = float(x)
            fy = float(y)
            rad = sqrt((fx-centerX)**2+(fy-centerY)**2)
            irad = int(round(rad))
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

@cython.cdivision(True)
def getCircularDiscreteBackground(np.ndarray[np.float32_t, ndim=2] img, int rmin, float start_p, float end_p, int radial_bin, int nBin, int max_pts):
    cdef int height = img.shape[0]
    cdef int width = img.shape[1]
    cdef np.ndarray[np.float32_t, ndim=1] xs = np.zeros(nBin, dtype = np.float32)
    cdef np.ndarray[np.float32_t, ndim=1] ys = np.zeros(nBin, dtype = np.float32)
    cdef np.ndarray[np.float32_t, ndim=1] all_pts = np.zeros(max_pts, dtype = np.float32)
    cdef np.float32_t centerX = width - 0.5
    cdef np.float32_t centerY = height - 0.5
    cdef int nPoints = 0
    cdef int rad, bin, start_ind, end_ind
    cdef int x, y, i
    cdef np.float32_t float_x, float_y, distance, d1, d2, sumVal

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
        # print sorted_pts

        # Get background value from all points (between percentage of start_p and end_p)
        start_ind = int(round(float(nPoints)*start_p/100))
        end_ind = int(round(float(nPoints)*end_p/100.))
        print nPoints, start_ind, end_ind

        if start_ind < end_ind:
            sumVal = 0.0
            for i in range(start_ind, end_ind):
                sumVal = sumVal + sorted_pts[i]
            ys[bin] = sumVal/float(end_ind-start_ind)
        else:
            ys[bin] = all_pts[start_ind]

        xs[bin] = (d1+d2)/2.

    return xs, ys

@cython.cdivision(True)
def replaceRmin(np.ndarray[np.float32_t, ndim=2] img, int rmin, double val):
    cdef int x, y
    cdef int height = img.shape[0]
    cdef int width = img.shape[1]
    cdef np.float32_t centerX = width
    cdef np.float32_t centerY = height
    cdef np.float32_t distance
    cdef np.float32_t frmin = float(rmin)
    cdef np.ndarray[np.float32_t, ndim=2] result = np.array(img, dtype = np.float32)
    cdef np.float32_t replace_val = float(val)

    for x in range(width-rmin-1, width):
        float_x = float(x)
        for y in range(height-rmin-1, height):
            float_y = float(y)
            distance = sqrt((float_x-centerX)**2+(float_y-centerY)**2)
            if distance <= frmin:
                result[y, x] = replace_val

    return result