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
import cv2
import numpy as np
# import cv2.cv as cv
import fabio
import pyFAI
from skimage.morphology import white_tophat
import math

def distance(pt1, pt2):
    """
    Get distance between 2 points
    :param pt1: first point (tuple or list of 2 values)
    :param pt2: second point (tuple or list of 2 values)
    :return: distance (float)
    """
    return np.sqrt((pt1[0]-pt2[0])**2+(pt1[1]-pt2[1])**2)

def get16bitImage(img):
    """
    Convert a image to uint16 image
    :param img: input image
    :return: uint16 image
    """
    max_val = img.max()
    min_val = img.min()
    if max_val == min_val:
        return (img * 65535. / min_val).astype('uint16')
    else:
        dev = max_val - min_val
        return (np.round(img*65535./dev)).astype('uint16')


def get8bitImage(img, min = None, max = None):
    """
    Convert a image to uint8 image
    :param img: input image
    :param min: min intensity
    :param max: max intensity
    :return: uint8 image
    """
    cimg = np.array(img, dtype=np.float)
    if max is None:
        max = img.max() * 0.5
    cimg[cimg > max] = max

    if min is None:
        min = img.min()
    cimg[cimg < min] = min

    cimg -= min
    min = 0
    max = cimg.max()

    if max <= min:
        img8bit = (cimg * 0.).astype('uint8')
    else:
        alpha = 255. / (max)
        img8bit = cv2.convertScaleAbs(cimg, alpha=alpha)

    return img8bit

def inverte(imagem):
    """
    Invert grey scale image
    :param imagem: input image
    :return: inverted image
    """
    imagem  =  (255-imagem)
    return imagem

def getThreshold(img, percent):
    """
    Get threshold value by using percentage of number of points
    :param img: input image
    :param percent: percentage of number of points higher threshold value
    :return: threshold value
    """
    bins = np.arange(img.max())
    hist, bins = np.histogram(img, bins)
    hist = np.array(hist)

    thrhold = 0
    dev = min(1000, img.max())
    for t in np.arange(0, img.max()-1, img.max()/dev):
        valueHist = np.sum(hist[int(t):int(img.max())])
        if (valueHist/(1.0*img.shape[0]*img.shape[1]))<percent:
            if valueHist < 100 and t > 1:
                thrhold = t-1
            else:
                thrhold = t
            # find number of pixel ---->  thrhold = t-1
            break
    return thrhold

def thresholdImg(img, percent, convert_type =  cv2.THRESH_BINARY_INV):
    """
    Apply thresholding by percent
    :param img: input image
    :param percent: percentage of number of points higher threshold value
    :param convert_type: convert type see http://docs.opencv.org/trunk/d7/d4d/tutorial_py_thresholding.html
    :return: threshold image
    """
    th = max(0, getThreshold(img, percent=percent)-1)
    ret, thres = cv2.threshold(img, th, 255, convert_type, dst=img)
    return thres

def bkImg(img, percent=0.01, morph=25):
    """
    Apply thresholding and morphology
    :param img: input image
    :param percent: percentage of number of points higher threshold value
    :param morph: morphology size
    :return: image
    """
    img = thresholdImg(img, percent)
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (morph, morph))
    img = cv2.morphologyEx(img, cv2.MORPH_OPEN, kernel)
    img = inverte(img)
    return img

def getBGR(img):
    """
    Convert grayscale image to RGB image
    :param img: grayscale image
    :return: RGB image
    """
    copy_img = copy.copy(img)
    copy_img = cv2.resize(copy_img, (int(copy_img.shape[1]), int(copy_img.shape[0])))
    return cv2.cvtColor(copy_img, cv2.COLOR_GRAY2BGR)

def getContours(img, n1=1, n2=2):
    ret = cv2.findContours(img, n1, n2)
    if len(ret) == 3:
        return ret[1]
    if len(ret) == 2:
        return ret[0]

def getCenter(img):
    """
    Find center of the diffraction.
    :param img: input image
    :return: center
    """
    img = get8bitImage(copy.copy(img))
    # cv2.imwrite('original.jpg', img)
    img = cv2.GaussianBlur(img, (5, 5), 0)
    # cv2.imwrite('blurred.jpg', img)
    init_center = None

    ##  Find init center by apply thresholding and fit ellipse to the contour which has the maximum size
    cimg = bkImg(copy.copy(img), 0.005, 50)
    # cv2.imwrite('background.jpg', cimg)
    # display_test(cimg, "threshold")

    contours = getContours(cimg)
    cnt = max(contours, key=lambda c: len(c))
    # print("Maximum contour: {}".format(cnt))
    if len(cnt) > 5:
        ellipse = cv2.fitEllipse(cnt)
        init_center = (ellipse[0][0], ellipse[0][1])
        # im = getBGR(img)
        # cv2.ellipse(im, ellipse, (0, 255, 0), 2)
        # cv2.circle(im, init_center, 2, (0, 0, 255), thickness=-1)
        # display_test(im, "img+ellipse")

    ## Find center by apply thresholding and fit ellipse to the contour of reflections and find the average center of reflections
    if init_center is not None:
        cimg = thresholdImg(copy.copy(img), 0.00015)
        # cv2.imwrite('thresholded.jpg', cimg)
        contours = getContours(cimg)

        # Find 2 biggest contours (reflections)
        cnts = sorted(contours, key=lambda c: len(c), reverse=True)[:6]
        reflections = []
        for i, cnt in enumerate(cnts):
            # Fit ellipse to 6 reflections if its size is bigger than 10
            if len(cnt) >= 10:
                ellipse = cv2.fitEllipse(cnt)
                center = ellipse[0]
                axes = ellipse[1]
                center = (center[0], center[1])
                reflections.append((center, np.pi*axes[0]*axes[1]))
                # im = getBGR(img)
                # cv2.ellipse(im, ellipse, color=(0, 255, 0), thickness=2)
                # cv2.circle(im, center, 2, (0, 0, 255), thickness=-1)
                # display_test(im, "img+e"+str(i))

        inds = np.arange(0, len(reflections))
        if len(reflections) > 1:
            r1 = None
            r2 = None
            min_diff = 99999
            for i in inds:
                other_inds = np.delete(inds, i)
                its_pair = min(other_inds, key=lambda k:abs(reflections[i][1]-reflections[k][1]))
                diff = abs(reflections[i][1]-reflections[its_pair][1])
                if diff < min_diff:
                    r1 = i
                    r2 = its_pair
                    min_diff = diff

            if r1 is not None and r2 is not None:
                x = ((reflections[r1][0][0]+reflections[r1][0][0]) / 2.)
                y = ((reflections[r1][0][1] + reflections[r1][0][1]) / 2.)
                if init_center is not None and distance(init_center, (x, y)) < 7:
                    # Return average center of reflections
                    return (x, y)

            # # Find the average center location from all fitting ellipses
            # if len(centers) % 2 != 0:
            #     # if number of center is odd, remove one reflection
            #     centers = centers[:-1]
            # sum_centers = np.sum(centers, axis=0)
            # x = int(round(1.* sum_centers[0] / len(centers)))
            # y = int(round(1.* sum_centers[1] / len(centers)))
            # if init_center is not None and distance(init_center, (x,y)) < 7:
            #     # Return average center of reflections
            #     return (x, y)

        return init_center

    # Find cener by using opencv moments. See http://docs.opencv.org/trunk/dd/d49/tutorial_py_contour_features.html
    nZero = cv2.findNonZero(img)  # copy_img)#
    if nZero is not None:
        copy_img = bkImg(copy.copy(img), 0.015, 30)
        m = cv2.moments(copy_img)
        if m['m00'] != 0:
            # initial center
            return ((m['m10'] / m['m00']), (m['m01'] / m['m00']))

    # Find Center by fitting circle in the image
    cimg = bkImg(copy.copy(img), 0.0015, 50)
    circles = cv2.HoughCircles(cimg, 3 , 1, 100,
                               param1=60, param2=20, minRadius=0, maxRadius=0) # 3 = cv2.HOUGH_GRADIENT
    # cv2.imwrite('circles.jpg', circles)
    if circles is not None:
        return (circles[0][0][0], circles[0][0][1])

    # If there's no method working return center of the image
    return (img.shape[1] / 2, img.shape[0] / 2)

def get_ring_model(hist):
    """
    Fit gaussian model to histogram
    :param hist:
    :return:
    """
    # Smooth histogram to find parameters easier
    from .histogram_processor import smooth
    hist[1] = smooth(hist[1], 20)

    n_hist = len(hist[1])
    index = np.argmax(hist[1])
    u = hist[0][index]
    if u < np.pi / 2:
        u += np.pi
    elif u > 3 * np.pi / 2:
        u -= np.pi

    # Fit model using same gaussian
    x = hist[0]

    # Call orientation_GMM3
    from lmfit.models import GaussianModel
    from lmfit import Model
    def orientation_GMM3(x, u, sigma, alpha, bg):
        mod = GaussianModel()
        return mod.eval(x=x, amplitude=alpha, center=u, sigma=sigma) + \
            mod.eval(x=x, amplitude=alpha, center=u-np.pi, sigma=sigma) + \
            mod.eval(x=x, amplitude=alpha, center=u+np.pi, sigma=sigma) + bg
    model = Model(orientation_GMM3, independent_vars='x')
    max_height = np.max(hist[1])

    model.set_param_hint('u', value=u, min=np.pi/2, max=3*np.pi/2)
    model.set_param_hint('sigma', value=0.1, min=0, max=np.pi*2)
    model.set_param_hint('alpha', value=max_height*0.1/0.3989423, min=0)
    model.set_param_hint('bg', value=0, min=-1, max=max_height+1)

    result = model.fit(data=hist[1], x=x, params=model.make_params())
    errs = abs(result.best_fit - result.data)
    weights = errs / errs.mean() + 1
    weights[weights > 3.] = 0
    result = model.fit(data=hist[1], x=x, params=result.params, weights=weights)

    '''import matplotlib.pyplot as plt
    plt.plot(x, Model(orientation_GMM3, independent_vars='x').eval(x=x, params=result.params))
    plt.show()'''

    return result.values

def HoF(hist, mode='f'):
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

def getRadOfMaxHoF(HoFs, mode, ratio=0.05):
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

def getRotationAngle(img, center, method=0):
    """
    Find rotation angle of the diffraction.
    :param img: input image
    :param center: center of the diffraction
    :return: rotation angle in degree
    """

    ## Find init angle by apply thresholding and fit ellipse to the contour which has the maximum size
    cimg = get8bitImage(copy.copy(img))
    cimg = cv2.GaussianBlur(cimg, (5, 5), 0)
    cimg = bkImg(copy.copy(cimg), 0.005, 50)
    init_angle = None
    contours = getContours(cimg)
    cnt = max(contours, key=lambda c: len(c))
    if len(cnt) > 5:
        ellipse = cv2.fitEllipse(cnt)
        init_angle = (ellipse[2]+90.) % 180
        init_angle = init_angle if init_angle <= 90. else 180. - init_angle

    # Find angle with maximum intensity from Azimuthal integration
    if img.shape == (1043, 981):
        det = "pilatus1m"
    else:
        det = "agilent_titan"

    corners = [(0, 0), (0, img.shape[1]), (img.shape[0], 0), (img.shape[0], img.shape[1])]
    npt_rad = int(round(max([distance(center, c) for c in corners])))
    ai = pyFAI.AzimuthalIntegrator(detector=det)
    ai.setFit2D(200, center[0], center[1])
    I2D, tth, chi = ai.integrate2d(img, npt_rad, 360, unit="r_mm", method="csr_ocl")
    I2D = I2D[:, :int(len(tth)/3.)]
    hist = np.sum(I2D, axis=1)  # Find a histogram from 2D Azimuthal integrated histogram, the x-axis is degree and y-axis is intensity
    sum_range = 0

    # import matplotlib.pyplot as plt
    # fig = plt.figure()
    # ax = fig.add_subplot(111)
    # ax.plot(hist)
    # ax.set_xlabel("Angle (degree)")
    # ax.set_ylabel("Intensity")
    # ax.set_title("Azimuthal Integration Histogram")
    # fig.show()

    # Find degree which has maximum intensity
    if method == 1: # gmm
        x = np.arange(0, 2 * np.pi, 2 * np.pi / 360)
        model_pars = get_ring_model([x, hist])
        max_degree = int(model_pars['u'] / np.pi * 180) % 180
    elif 2 <= method <= 3: # 'hof_f' or 'hof_h'
        HoFs = HoF(hist, 'f' if method == 2 else 'h')
        max_degree = int(getRadOfMaxHoF(HoFs, 'f' if method == 2 else 'h') / np.pi * 180) % 180
    else:  # Find the best degree by its intensity
        max_degree = max(np.arange(180), key=lambda d: np.sum(hist[d - sum_range:d + sum_range + 1]) + np.sum(
            hist[d + 180 - sum_range:d + 181 + sum_range]))

    # # If the degree and initial angle from ellipse are different, return ellipse angle instead
    if init_angle is not None and abs(max_degree-init_angle) > 20. and abs(180 - max_degree - init_angle)>20:
        return int(round(init_angle))

    #If max degree is obtuse return the acute angle equivalent of the same
    if max_degree > 90:
        return -1*(180-max_degree)
    if max_degree < -90:
        return (180+max_degree)

    # otherwise, return max degree
    return max_degree

def getCenterRemovedImage(img, center, rmin):
    """
    Remove center location in the image (replace by 0 (black value))
    :param img: input image
    :param center: center location (tuple or list)
    :param rmin: radius of the circle
    :return: image after center location is removed
    """
    center = (int(center[0]), int(center[1]))
    mask = np.zeros((img.shape[0], img.shape[1]), dtype=np.uint8)
    cv2.ellipse(mask, tuple(center), axes=(rmin, rmin), angle=0, startAngle=0,
                endAngle=360, color=255,
                thickness=-1)  # Draw a circle in mask
    img[mask > 0] = 0  # replace circle with 0
    return img

def getNewZoom(current, move, xmax, ymax, ymin=0):
        """
        Get new zoom location (x, and y ranges) by given current zoom, move vector and x,y maximum ranges
        :param current: current zoom location
        :param move: moving vector
        :param xmax: maximum x
        :param ymax: maximum y
        :param ymin: minimum y
        :return:
        """
        x1 = current[0][0] + move[0]
        x2 = current[0][1] + move[0]
        if x1 < 0:
            x1 = 0
            x2 = current[0][1] - current[0][0]
        if x2 >= xmax:
            x2 = xmax - 1
            x1 = x2 - (current[0][1] - current[0][0])

        y1 = current[1][0] + move[1]
        y2 = current[1][1] + move[1]
        if y1 < ymin:
            y1 = ymin
            y2 = y1 + (current[1][1] - current[1][0])
        if y2 > ymax:
            y2 = ymax
            y1 = y2 - (current[1][1] - current[1][0])

        return [(x1, x2), (y1, y2)]

def rotateImage(img, center, angle, img_type, mask_thres = -999):
    """
    Get rotated image by angle.
    :param img: input image
    :param angle: rotation angle
    :return: rotated image
    """
    if angle == 0:
        return img, center, None

    M = cv2.getRotationMatrix2D(tuple(center), angle, 1)
    size = max(img.shape[0], img.shape[1])

    # used for expanding the rotated image
    # im_max_shape = max(img.shape[1], img.shape[0])
    # print("max image shape: {}".format(im_max_shape))
    # im_center = (im_max_shape/2, im_max_shape/2)
    # translation = np.array(im_center) - np.array([img.shape[1]/2, img.shape[0]/2])
    # print(translation)
    # T = np.identity(3)
    # # T[0:1,2] = translation
    # T[0,2] = translation[0]
    # T[1,2] = translation[1]
    # M2 = np.identity(3)
    # print("M: {}".format(M))
    # M2[0:2,:] = M
    # print("M2: {}".format(M2))
    # M3 = np.dot(T, M2)
    # print("M3: {}".format(M3))
    # M1 = M3[0:2,:]
    # print("M1: {}".format(M1))

    if img_type == "PILATUS":
        img = img.astype('float32')
        if mask_thres == -999:
            mask_thres = getMaskThreshold(img, img_type)
        mask = np.zeros((img.shape[0], img.shape[1]), dtype=np.uint8)
        mask[img <= mask_thres] = 255
        rotated_img, center, rotMat = rotateNonSquareImage(img, angle, center)
        rotated_mask, _, _ = rotateNonSquareImage(mask, angle, center)
        rotated_mask[rotated_mask > 0.] = 255
        rotated_img[rotated_mask > 0] = mask_thres
        return rotated_img, center, rotMat
    else:
        return rotateNonSquareImage(img, angle, center)

def rotateImageAboutPoint(img, point, angle, img_type, mask_thres = -999):
    """
    Get rotated image by angle about a given point.
    :param img: input image
    :param point: point to be rotated about
    :param angle: rotation angle
    :return: rotated image
    """
    if angle == 0:
        return img

    M = cv2.getRotationMatrix2D(tuple(point), angle, 1)

    if img_type == "PILATUS":
        img = img.astype('float32')
        if mask_thres == -999:
            mask_thres = getMaskThreshold(img, img_type)
        mask = np.zeros((img.shape[0], img.shape[1]), dtype=np.uint8)
        mask[img <= mask_thres] = 255
        rotated_img = cv2.warpAffine(img, M, (img.shape[1],  img.shape[0]))
        rotated_mask = cv2.warpAffine(mask, M, (img.shape[1],  img.shape[0]))
        rotated_mask[rotated_mask > 0.] = 255
        rotated_img[rotated_mask > 0] = mask_thres
    else:
        rotated_img = cv2.warpAffine(img, M, (img.shape[1],  img.shape[0]))
    return rotated_img

def rotatePoint(origin, point, angle):
    """
    Rotate a point counterclockwise by a given angle around a given origin.

    The angle should be given in radians.
    """
    ox, oy = origin
    px, py = point

    qx = ox + np.cos(angle) * (px - ox) - np.sin(angle) * (py - oy)
    qy = oy + np.sin(angle) * (px - ox) + np.cos(angle) * (py - oy)
    return qx, qy

def getMaskThreshold(img, img_type):
    min_val = img.min()
    mask_thres = min_val - 1.
    if img_type == "PILATUS":
        hist = np.histogram(img, 3, (min_val, min_val+3))
        max_ind = np.argmax(hist[0])
        mask_thres = hist[1][max_ind]
    return mask_thres

###### White top hat image for Scanning Diffraction #########
def gaussian(x, a, mean, sigma):
    return a*np.exp((-1.*(x-mean)**2)/(2*(sigma**2)))

def getImgAfterWhiteTopHat(img, sigma=5):
    tmpKernel = 1. / sigma ** 2 * np.ones((sigma, sigma))
    dst = copy.copy(img)
    dst = np.array(dst, np.float32)
    for _ in range(2):
        dst = cv2.filter2D(dst, cv2.CV_32F, tmpKernel, anchor=(-1, -1))

    sigma = sigma * 6
    x = np.array(range(-int(sigma + 1), int(sigma + 1) + 1, 1))
    kernelX = gaussian(x, 1, 0, sigma)
    kernelXY = np.outer(kernelX, np.transpose(kernelX))
    tophat = white_tophat(dst, kernelXY)
    return tophat

def kernelXY(sigma=5):
    a = sigma * 6
    x = np.array(range(-int(a + 1), int(a + 1) + 1, 1))
    kernelX = 1. / (np.sqrt(2. * np.pi) * a) * np.exp(-(x - 0) ** 2. / (2. * a ** 2))
    return np.outer(kernelX, np.transpose(kernelX))


def display_test(img, name = "test", max_int = 100):
    """
    Display input image to screen. Just for test
    :param img: input image
    :param name: image name
    :return: -
    """
    max_side = max(img.shape[:2])
    ratio = 1.*650/max_side
    size = (int(img.shape[1]*ratio),int(img.shape[0]*ratio))
    img = get8bitImage(img, min=0.0, max=max_int)
    img = cv2.resize(img, size)
    cv2.imshow(name, img)

def averageImages(file_list, rotate=False):
    """
    open images and average them all
    :param file_list: list of image path (str)
    :return:
    """
    all_imgs = []
    dims_match, max_dim, max_img_center = checkDimensionsMatch(file_list)
    if not dims_match:
        return expandAndAverageImages(file_list, max_dim, max_img_center, rotate)
    for f in file_list:
        img = fabio.open(f).data
        if img.shape == (1043, 981):
            img_type = "PILATUS"
        else:
            img_type = "NORMAL"
        if rotate:
            print("Rotating and centering {}".format(f))
            center = getCenter(img)
            angle = getRotationAngle(img, center, method=0)
            img, center, _ = rotateImage(img, center, angle, img_type, mask_thres = -999)
        all_imgs.append(img)

    return np.mean(all_imgs, axis=0)

def expandAndAverageImages(file_list, max_dim, max_img_center, rotate):
    """
    open images, expand to largest size and average them all
    :param file_list: list of image path (str)
    :param max_dim: dimension of largest image
    :param max_img_center: center of largest image
    :return:
    """
    all_imgs=[]
    for f in file_list:
        img = fabio.open(f).data

        if img.shape == (1043, 981):
            img_type = "PILATUS"
        else:
            img_type = "NORMAL"

        # Expand Image to max size by padding the surrounding by zeros and center of all image coincides
        center = getCenter(img)
        expanded_img = np.zeros(max_dim)
        b, l = img.shape
        expanded_img[0:b, 0:l] = img
        transx = int((max_img_center[0] - center[0]))
        transy = int((max_img_center[1] - center[1]))
        M = np.float32([[1, 0, transx], [0, 1, transy]])
        img = cv2.warpAffine(expanded_img, M, max_dim)

        if rotate:
            print("Rotating and centering {}".format(f))
            angle = getRotationAngle(img, max_img_center, method=0)
            img, center, _ = rotateImage(img, max_img_center, angle, img_type, mask_thres = -999)
        all_imgs.append(img)

    return np.mean(all_imgs, axis=0)

def checkDimensionsMatch(file_list):
    """
    Check whether dimensions of all the images match
    :param file_list: list of image path (str)
    :return: True if dimensions match
    """
    dims = []
    for f in file_list:
        img = fabio.open(f).data
        dims.append(img.shape)
    max_dim = max(dims)
    index = dims.index(max_dim)
    max_img = fabio.open(file_list[index]).data
    center = getCenter(max_img)

    return dims.count(dims[0]) == len(dims), max_dim, center

def processImageForIntCenter(img, center, img_type, mask_thres = -999):
    """
    Translate image such that the new center is an integer
    :param file_list: original image and its center with decimals
    :return: translated image and nearest integer center
    """
    img=img.astype('float32')
    int_Center = (round(center[0]), round(center[1]))
    tx = int_Center[0] - center[0]
    ty = int_Center[1] - center[1]
    M = np.float32([[1,0,tx],[0,1,ty]])
    print("In process Image int center, translating original image by tx = " + str(tx) + " and ty = " + str(ty))
    rows,cols = img.shape

    if img_type == "PILATUS":
        if mask_thres == -999:
            mask_thres = getMaskThreshold(img, img_type)
        mask = np.zeros((img.shape[0], img.shape[1]), dtype=np.uint8)
        mask[img <= mask_thres] = 255
        translated_Img = cv2.warpAffine(img, M, (cols,rows))
        translated_mask = cv2.warpAffine(mask, M, (cols,rows))
        translated_mask[translated_mask > 0.] = 255
        translated_Img[translated_mask > 0] = mask_thres
    else:
        translated_Img = cv2.warpAffine(img,M,(cols,rows))
   
    return (translated_Img, int_Center)

def rotateNonSquareImage(img, angle, center1):
    """
    Rotates a non square image by first determining the appropriate square image and then rotating the image.
    :param file_list: original non square image, angle of rotation and center
    :return: rotated image and center with respect to new coordinate system
    """
    height, width = img.shape
    center = (width/2, height/2)

    rotation_mat = cv2.getRotationMatrix2D(center, angle, 1.)

    # rotation calculates the cos and sin, taking absolutes of those.
    abs_cos = abs(rotation_mat[0,0]) 
    abs_sin = abs(rotation_mat[0,1])

    # find the new width and height bounds
    bound_w = int(height * abs_sin + width * abs_cos)
    bound_h = int(height * abs_cos + width * abs_sin)

    # subtract old image center (bringing image back to origo) and adding the new image center coordinates
    rotation_mat[0, 2] += bound_w/2 - center[0]
    rotation_mat[1, 2] += bound_h/2 - center[1]

    maxB = max(bound_h, bound_w)

    center1 = [center1[0], center1[1], 1]
    center1 = np.dot(rotation_mat, center1)
    center2 = (int(center1[0]), int(center1[1]))
    
    # rotate image with the new bounds and translated rotation matrix
    rotated_img = cv2.warpAffine(img, rotation_mat, (maxB, maxB))
    return rotated_img, center2, rotation_mat
