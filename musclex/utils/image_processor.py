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
        valueHist = np.sum(hist[t:img.max()])
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
    img = cv2.GaussianBlur(img, (5, 5), 0)
    init_center = None

    ##  Find init center by apply thresholding and fit ellipse to the contour which has the maximum size
    cimg = bkImg(copy.copy(img), 0.005, 50)
    # display_test(cimg, "threshold")

    contours = getContours(cimg)
    cnt = max(contours, key=lambda c: len(c))
    if len(cnt) > 5:
        ellipse = cv2.fitEllipse(cnt)
        init_center = (int(round(ellipse[0][0])), int(round(ellipse[0][1])))
        # im = getBGR(img)
        # cv2.ellipse(im, ellipse, (0, 255, 0), 2)
        # cv2.circle(im, init_center, 2, (0, 0, 255), thickness=-1)
        # display_test(im, "img+ellipse")

    ## Find center by apply thresholding and fit ellipse to the contour of reflections and find the average center of reflections
    if init_center is not None:
        cimg = thresholdImg(copy.copy(img), 0.00015)
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
                center = (int(round(center[0])), int(round(center[1])))
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
                x = int(round((reflections[r1][0][0]+reflections[r1][0][0]) / 2.))
                y = int(round((reflections[r1][0][1] + reflections[r1][0][1]) / 2.))
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
            return (int(m['m10'] / m['m00']), int(m['m01'] / m['m00']))

    # Find Center by fitting circle in the image
    cimg = bkImg(copy.copy(img), 0.0015, 50)
    circles = cv2.HoughCircles(cimg, 3 , 1, 100,
                               param1=60, param2=20, minRadius=0, maxRadius=0) # 3 = cv2.HOUGH_GRADIENT
    if circles is not None:
        return (circles[0][0][0], circles[0][0][1])

    # If there's no method working return center of the image
    return (img.shape[1] / 2, img.shape[0] / 2)

def getRotationAngle(img, center):
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
    max_degree = max(np.arange(180), key=lambda d: np.sum(hist[d - sum_range:d + sum_range + 1]) + np.sum(
        hist[d + 180 - sum_range:d + 181 + sum_range]))  # Find the best degree by its intensity

    # If the degree and initial angle from ellipse are different, return ellipse angle instead
    if init_angle is not None and abs(max_degree-init_angle) > 20.:
        return int(round(init_angle))

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

def rotateImage(img, center, angle, mask_thres = -999):
    """
    Get rotated image by angle.
    :param img: input image
    :param angle: rotation angle
    :return: rotated image
    """
    if angle == 0:
        return img

    M = cv2.getRotationMatrix2D(tuple(center), angle, 1)
    # size = max(img.shape[0], img.shape[1])

    if img.shape == (1043, 981):
        img = img.astype('float32')
        if mask_thres == -999:
            mask_thres = getMaskThreshold(img)
        mask = np.zeros((img.shape[0], img.shape[1]), dtype=np.uint8)
        mask[img <= mask_thres] = 255
        rotated_img = cv2.warpAffine(img, M, (img.shape[1],  img.shape[0]))
        rotated_mask = cv2.warpAffine(mask, M, (img.shape[1],  img.shape[0]))
        rotated_mask[rotated_mask > 0.] = 255
        rotated_img[rotated_mask > 0] = mask_thres
        return rotated_img
    else:
        return cv2.warpAffine(img, M, (img.shape[1],  img.shape[0]))

def getMaskThreshold(img):
    min_val = img.min()
    mask_thres = min_val - 1.
    if img.shape == (1043, 981):
        hist = np.histogram(img, 3, (min_val, min_val+3))
        max_ind = np.argmax(hist[0])
        mask_thres = hist[1][max_ind]
    return mask_thres

###### White top hat image for circular projection #########
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

def averageImages(file_list):
    """
    open images and average them all
    :param file_list: list of image path (str)
    :return:
    """
    all_imgs = []
    for f in file_list:
        img = fabio.open(f).data
        all_imgs.append(img)

    return np.mean(all_imgs, axis=0)