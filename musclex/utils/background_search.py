
import os
import cv2
import numpy as np
from scipy.signal import find_peaks, peak_widths
from scipy import ndimage
from ..modules.QF_utilities import replaceRmin
from. background_search_utils import *
from ..converted_fortran.converted_fortran import *

# =============== Utils for Masking ===============

def get_projection(img, gap, orientation=0, avg=False, half=False, offset=0, center=None):
    """
    :param orientation: 1 for meridian, 0 for equator
    """
    if center is None:
        center = img.shape[0]//2, img.shape[1]//2
    
    if orientation == 0: 
        arr = img[center[0]+offset-gap//2:center[0]+offset+gap//2, :]
    else:
        arr = img[:, center[1]+offset-gap//2:center[1]+offset+gap//2]

    if avg:
        projection = np.mean(arr, axis=orientation)
    else:
        projection = np.sum(arr, axis=orientation)
    if half:
        projection = projection[len(projection)//2:]
    return projection

def find_m3_peak(meridian_half, rmin=10):
    peaks, properties = find_peaks(meridian_half[rmin:], prominence=1)
    peaks = [p+rmin for p in peaks]
    prominences = properties['prominences']
    sorted_indices = np.argsort(prominences)[::-1]
    top_2nd_peak_index = peaks[sorted_indices[1]]
    m3 =  top_2nd_peak_index
    print(f"Auto-detected m3: {m3} pixels")
    return int(m3)

def find_m_peak_auto(img, m=1, rmin=10, intensity=False):
    meridian_half = get_projection(img, gap=2, orientation=1, avg=True, half=True)
    m3 = find_m3_peak(meridian_half, rmin=rmin)
    m1 = int(m3/3)
    if intensity:
        if m == 3:
            return meridian_half[m3]
        else:
            return meridian_half[m1*m]
    if m == 3:
        return m3
    else:
        return m1*m

def find_fwhm(histogram, peak_index=None, rel_height=0.5):
    hist_array = np.array(histogram)
    
    if peak_index is None:
        peaks, _ = find_peaks(hist_array)
        if len(peaks) == 0:
            return None
        peak_index = peaks[np.argmax(hist_array[peaks])]
    try:
        widths, _, _, _ = peak_widths(
            hist_array, [peak_index], rel_height=rel_height
        )
        
        fwhm = widths[0]

    except Exception as e:
        print(f"Error calculating FWHM: {e}")
        return 5

    return fwhm

def find_n_most_prominent_peaks(histogram, n=3, rmin=0):
    histogram = histogram[rmin:]
    hist_array = np.array(histogram)
    peaks, properties = find_peaks(hist_array, prominence=1)
    prominences = properties['prominences']
    
    sorted_indices = np.argsort(prominences)[::-1]
    top_n_peak_indices = peaks[sorted_indices[:n]]
    
    top_n_peak_values = hist_array[top_n_peak_indices]
    top_n_peak_indices = [p+rmin for p in top_n_peak_indices]  # adjust indices
    return top_n_peak_indices, top_n_peak_values

def create_peak_mask(image_shape, peak_positions, peak_width=5, center=None):
    mask = np.ones(image_shape, dtype=bool)
    if center is not None:
        h_center, v_center = center
    else:
        h_center = image_shape[0] // 2
        v_center = image_shape[1] // 2

    for pos in peak_positions:
        mask[h_center - peak_width//2: h_center + peak_width//2, v_center - pos - peak_width//2:v_center - pos + peak_width//2] = False
        mask[h_center - peak_width//2: h_center + peak_width//2, v_center + pos - peak_width//2:v_center + pos + peak_width//2] = False
    return mask.astype(int)

def find_first_valley(histogram, start=0, end=None):
    hist_array = np.array(histogram)
    if end is None:
        end = len(hist_array)
    for i in range(start + 1, end):
        if hist_array[i] > hist_array[i - 1]:
            return i - 1
    return None

def get_radial_average_rmax(img, rmax, band_width=10):
    center_y, center_x = np.array(img.shape) / 2
    y, x = np.indices(img.shape)
    r = np.sqrt((x - center_x)**2 + (y - center_y)**2)
    mask = (r >= rmax - band_width) & (r < rmax)
    return np.mean(img[mask])

def find_i0_i1_peaks_auto(img, rmin=10, intensity=False):
    equator_half = get_projection(img, gap=2, orientation=0, avg=True, half=True)
    i0, i1 = find_i0_i1_peaks(equator_half, rmin=rmin)
    if intensity:
        return equator_half[i0], equator_half[i1]
    return i0, i1 

def find_i0_i1_peaks(equator_half, rmin=10):
    peaks, properties = find_peaks(equator_half[rmin:], prominence=1)
    peaks = [p+rmin for p in peaks]  # adjust indices
    prominences = properties['prominences']
    sorted_indices = np.argsort(prominences)[::-1]
    i0 = peaks[sorted_indices[0]]
    i1 = peaks[sorted_indices[1]]
    return i0, i1

# ========================= Masks ==========================


def create_rectangle_mask(height, width, x_length=None, y_length=None):
    center_x = width//2
    center_y = height//2

    x_length = width if x_length is None else x_length
    y_length = height if y_length is None else y_length
    mask = np.ones(shape=(height, width))
    mask[center_y-y_length//2:center_y+y_length//2, center_x-x_length//2:center_x+x_length//2] = 0
    return mask.astype(int)

def create_circular_mask(height, width, inside=True, diameter=None, radius=None):
    """
    Create a 2D numpy array mask with a circle of 1s in the center.
    
    Parameters:
    width (int): Width of the output array
    height (int): Height of the output array
    inside (bool): If True, mask is 1 inside the circle, 0 outside. If False, mask is 1 outside the circle, 0 inside.
    diameter (int): Diameter of the circle in pixels
    """
    y, x = np.ogrid[:height, :width]
    center_x = width // 2
    center_y = height // 2
    if radius is None:
        radius = diameter / 2

    mask = (x - center_x)**2 + (y - center_y)**2 <= radius**2
    mask = mask.astype(int)
    mask = mask if inside else 1-mask
    return mask




# ======================== Synthetic Structure ==========================
class ArtificialData:
    def __init__(self, image_shape=(1000, 1000), point_x=None, point_y=None):
        self.image_shape = image_shape
        self.height = self.image_shape[0]
        self.width = self.image_shape[1]

        self.center_x = self.width//2
        self.center_y = self.height//2

        # place data at the center of the image by default
        if point_x is None:
            self.point_x = self.center_x
        else:
            self.point_x = point_x
        if point_y is None:
            self.point_y = self.center_y
        else:
            self.point_y = point_y

class MaskArtificialData(ArtificialData):
    def __init__(self, image_shape=(1000, 1000)):
        super().__init__(image_shape)
        self.mask = np.zeros(shape=image_shape)
    
    def define_parameters(self, sigma_x=25, sigma_y=5, mult=3):
        self.sigma_x = sigma_x
        self.sigma_y = sigma_y
        self.mult = mult

        self.distance_x = int(self.sigma_x*self.mult)
        self.distance_y = int(self.sigma_y*self.mult)

    def create_image(self, point_x, point_y):
        self.point_x = point_x
        self.point_y = point_y
        self.mask[self.point_y-self.distance_y:self.point_y+self.distance_y, self.point_x-self.distance_x:self.point_x+self.distance_x] = 1

    def get_mask(self):
        return self.mask

class GaussianArtificialData(ArtificialData):
    def __init__(self, image_shape=(1000, 1000)):
        super().__init__(image_shape)
        self.image = np.zeros(shape=image_shape)
    
    def define_parameters(self, sigma_x=25, sigma_y=5, amplitude=5000):
        self.sigma_x = sigma_x
        self.sigma_y = sigma_y
        self.amplitude = amplitude

    def pre_compute_kernel(self):
        self.kernel_width = int(6 * self.sigma_x)  # 6 sigma should cover ~99.7% of distribution
        self.kernel_height = int(6 * self.sigma_y)
        kx = np.arange(self.kernel_width) - self.kernel_width // 2
        ky = np.arange(self.kernel_height) - self.kernel_height // 2
        KX, KY = np.meshgrid(kx, ky)
        self.gaussian_kernel = self.amplitude * np.exp(-(KX**2 / (2 * self.sigma_x**2) + KY**2 / (2 * self.sigma_y**2)))

    def create_image(self, point_x, point_y):
        self.point_x = point_x
        self.point_y = point_y

        x_start = max(0, self.point_x - self.kernel_width // 2)
        x_end = min(self.image.shape[1], self.point_x + self.kernel_width // 2)
        y_start = max(0, self.point_y - self.kernel_height // 2)
        y_end = min(self.image.shape[0], self.point_y + self.kernel_height // 2)

        kx_start = max(0, self.kernel_width // 2 - self.point_x)
        kx_end = kx_start + (x_end - x_start)
        ky_start = max(0, self.kernel_height // 2 - self.point_y)
        ky_end = ky_start + (y_end - y_start)

        self.image[y_start:y_end, x_start:x_end] += self.gaussian_kernel[ky_start:ky_end, kx_start:kx_end]

    def apply_intencity_decrease(self):
        h, w = self.image.shape[:2]
        y, x = np.ogrid[:h, :w]
        cy, cx = h / 2, w / 2
        dist_from_center = np.sqrt((y - cy)**2 + (x - cx)**2) 

        epsilon = 1e-6
        dist_from_center = np.maximum(dist_from_center, epsilon)

        self.radial_mask = 1/dist_from_center
        self.radial_mask = np.clip(self.radial_mask, 0, 1)

        self.faded_image = self.image * self.radial_mask
        return self.faded_image
    
    def get_radial_mask(self):
        return self.radial_mask

    def get_original_image(self):
        return self.image

    def get_faded_image(self):
        return self.faded_image
    
def get_grid(image_shape=(1000, 1000), \
             step_x=10, step_y=10, \
                    offset_x=None, offset_y=None, fold=True):
    """
    Grid for artificial data placement. Assumes QF image. 

    """
    width = image_shape[1]
    height = image_shape[0]

    if image_shape[1]%2 == 0:
        center_x_right = int(np.ceil(image_shape[1]/2))
        center_x_left = center_x_right - 1
    else:
        center_x_left = center_x_right = image_shape[1]//2
    
    if image_shape[0]%2 == 0:
        center_y_bottom = int(np.ceil(image_shape[0]/2))
        center_y_top = center_y_bottom - 1
    else:
        center_y_top = center_y_bottom = image_shape[0]//2
    
    grid = []

    if offset_x is None:
        offset_x = step_x//2
    if offset_y is None:
        offset_y = step_y//2

    # Top-left quadrant
    for i in range(center_x_left-offset_x, 0, -step_x):
        for j in range(center_y_top-offset_y, 0, -step_y):
            grid.append((i, j))
    
    if fold:
        return grid
    
    # Top-right quadrant
    for i in range(center_x_right+offset_x, width, step_x):
        for j in range(center_y_bottom+offset_y, height, step_y):
            grid.append((i, j))

    # Bottom-left quadrant
    for i in range(center_x_left-offset_x, 0, -step_x):
        for j in range(center_y_bottom+offset_y, height, step_y):
            grid.append((i, j))

    # Bottom-right quadrant
    for i in range(center_x_right+offset_x, width, step_x):
        for j in range(center_y_top-offset_y, 0, -step_y):
            grid.append((i, j))

    return grid


# ===================== Loss Metrics ======================


MEAN_METRIC_VALUES = {
    "SHARE_NON_BASELINE_MEAN": 0.35,
    "SHARE_NEG_GEN_MEAN": 0.08,
    "MSE_SYN_MEAN": 100,
    "SMOOTH_MEAN": 0.03,
    "SHARE_NEG_SYN_MEAN": 0.20,
    "SHARE_NEG_CON_MEAN": 0.07,
    }

WEIGHTS  = {
    "MSE": .1,
    "Share_Neg_Synthetic": 0.1,
    "Share_Neg_General": 0.0,
    "Share_Non_Baseline": 0.1,
    "Share_Neg_Connected": 0.3,
    "Smoothness": 0.4
}


def full_eval_metrics(dimg, dbg, syn_img, syn_str, syn_mask, gen_mask, baseline_value, min_neg_con_pixels=9, \
                      normalize_metrics=True):
    if syn_str is None or syn_mask is None:
        mse = 0
        share_neg_synthetic = 0
    else:
        mse = artificial_data_mse(syn_img, syn_str, syn_mask, gen_mask, normalize=True)
        share_neg_synthetic = share_neg_syn(syn_img, syn_str, syn_mask)
    
    share_neg_general = share_neg_gen(dimg, gen_mask)
    share_non_baseline_pixels = share_non_baseline(dimg, baseline_value, gen_mask, clip=[2,5])
    share_neg_connected = share_neg_con(dimg, gen_mask, min_pixels=min_neg_con_pixels)
    smoothness_value = smoothness(dimg, dbg, gen_mask)

    if normalize_metrics:
        mean_values = MEAN_METRIC_VALUES
        mse = mse / mean_values["MSE_SYN_MEAN"]
        share_neg_synthetic = share_neg_synthetic / mean_values["SHARE_NEG_SYN_MEAN"]
        share_neg_general = share_neg_general / mean_values["SHARE_NEG_GEN_MEAN"]
        share_non_baseline_pixels = share_non_baseline_pixels / mean_values["SHARE_NON_BASELINE_MEAN"]
        share_neg_connected = share_neg_connected / mean_values["SHARE_NEG_CON_MEAN"]
        smoothness_value = smoothness_value / mean_values["SMOOTH_MEAN"]

    loss = (mse * WEIGHTS["MSE"] +
            share_neg_synthetic * WEIGHTS["Share_Neg_Synthetic"] +
            share_neg_general * WEIGHTS["Share_Neg_General"] +
            share_non_baseline_pixels * WEIGHTS["Share_Non_Baseline"] +
            share_neg_connected * WEIGHTS["Share_Neg_Connected"] +
            smoothness_value * WEIGHTS["Smoothness"])

    metrics = {
        "MSE": mse,
        "Share_Neg_Synthetic": share_neg_synthetic,
        "Share_Neg_General": share_neg_general,
        "Share_Non_Baseline": share_non_baseline_pixels,
        "Share_Neg_Connected": share_neg_connected,
        "Smoothness": smoothness_value,
        "Loss": loss
    }

    return metrics



def artificial_data_mse(dimg, syn_str, syn_mask, gen_mask=None, normalize=True):
    dimg = dimg * syn_mask
    syn_str = syn_str * syn_mask    
    
    mse = np.mean((dimg - syn_str)**2)
    if normalize and gen_mask is not None:
        mse = mse / (np.sum(syn_mask) / np.sum(gen_mask)) * 0.02 
    return mse


def share_neg_syn(dimg, syn_str, syn_mask):
    dimg = dimg * syn_mask
    syn_str = syn_str * syn_mask
    neg_pixels = np.sum((dimg - syn_str) < 0)
    total_pixels = np.sum(syn_mask)
    share_neg = neg_pixels / total_pixels
    return share_neg


def share_neg_gen(dimg, mask_equator):
    dimg = dimg * mask_equator
    neg_pixels = np.sum(dimg < 0)
    total_pixels = np.sum(mask_equator)
    share_neg = neg_pixels / total_pixels
    return share_neg


def share_non_baseline(dimg, baseline_value, mask_equator, clip=[2,5]):
    baseline_value = np.clip(baseline_value, clip[0], clip[1])
    dimg = dimg * mask_equator
    non_baseline_pixels = np.sum(dimg > baseline_value)
    total_pixels = np.sum(mask_equator)
    share_non_baseline = non_baseline_pixels / total_pixels
    return share_non_baseline

def share_neg_con(dimg, mask_equator, min_pixels):

    dimg = dimg * mask_equator
    neg_mask = dimg < 0
    structure = ndimage.generate_binary_structure(2, 1)
    labeled, _ = ndimage.label(neg_mask, structure=structure)
    component_areas = np.bincount(labeled.ravel())
    labels_to_keep = np.where(component_areas >= min_pixels)[0]
    labels_to_keep = labels_to_keep[labels_to_keep != 0] 
    filtered_mask = np.isin(labeled, labels_to_keep)

    neg_pixels = np.sum(filtered_mask>0)
    total_pixels = filtered_mask.size
    share_neg = neg_pixels / total_pixels
    return share_neg

def smoothness(dimg, dbg, mask_equator):
    dy = np.abs(dbg[:-1, :] - dbg[1:, :])
    dy = dy * mask_equator[:-1, :]  # Mask only valid rows
    denominator = np.sum((dimg + dbg) * mask_equator)
    smoothness_value = np.sum(dy) / denominator if denominator > 0 else 0.0
    return smoothness_value


def evaluate_loss(dimg, dbg, syn_img, syn_srt, syn_mask, gen_mask, baseline):
    
    metrics = full_eval_metrics(dimg, dbg, syn_img=syn_img, syn_str=syn_srt, syn_mask=syn_mask, \
                                gen_mask=gen_mask, baseline_value=baseline)
    
    print("Evaluation Metrics:")
    for key, value in metrics.items():
        print(f"{key}: {value:.4f}")

# ========================= Background Removal ==========================

def upsampleImage(img, factor=2):
    if factor > 1:
        h, w = img.shape[0] * factor, img.shape[1] * factor
        img = cv2.resize(img, (w, h), interpolation=cv2.INTER_CUBIC)
    return img

def applyWhiteTophat(img, radius):
    """
    Fast white top-hat using OpenCV
    """
    img32 = np.asarray(img, dtype=np.float32)
    r = int(round(radius))
    ksize = max(1, 2 * r + 1)
    kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (ksize, ksize))
    tophat = cv2.morphologyEx(img32, cv2.MORPH_TOPHAT, kernel, borderType=cv2.BORDER_REPLICATE)
    return img-tophat

def applySmoothedBGSub(fold, center, fwhm=15, boxcar_x=15, boxcar_y=15, cycles=250, typ='gauss'):
    """
    Apply the Iterative Low Pass Filter Background Subtraction.
    :param typ: type of the subtraction, default to 'gauss', other option is 'boxcar'
    """

    img = makeFullImage(fold)

    # if "roi_rad" in self.info: # if roi_rad is specified, use it
    #     roi_rad = int(self.info["roi_rad"])
    #     center_x = int(center[0])
    #     center_y = int(center[1])
    #     img = img[center_y - roi_rad:center_y + roi_rad, center_x - roi_rad:center_x + roi_rad]

    img = img.astype("float32")
    width = img.shape[1]
    height = img.shape[0]

    if typ == "gauss":
        filter_type = 'gaussian'
        kernel_size = (fwhm, fwhm)
        if kernel_size[0] % 2 == 0:
            kernel_size = (kernel_size[0] + 1, kernel_size[1] + 1)
        sigmaX = 0
    else:
        filter_type = 'boxcar'
        kernel_size = (boxcar_x, boxcar_y)
        sigmaX = 0  # Set to zero for boxcar filter

    tension = None # Not used in the function
    edge_background = None  # You can provide edge background if available

    background = replicate_bcksmooth(
        image=img,
        max_iterations=cycles,
        filter_type=filter_type,
        kernel_size=kernel_size,
        sigmaX=sigmaX,
        tension=tension,
        edge_background=edge_background,
    )

    background[np.isnan(background)] = 0.0
    background = np.array(background, "float32")
    background = background.reshape((height, width))

    # # replacing values that fall outside the roi_rad with the original values fromthe image
    # if "roi_rad" in self.info:
    #     background = background[:height//2, :width//2]
    #     pad_y = max((fold.shape[0] - background.shape[0]), 0)
    #     pad_x = max((fold.shape[1] - background.shape[1]), 0)
    #     background = np.pad(background, ((pad_y, 0), (pad_x, 0)), 'constant', constant_values=0)
    # else:
    #     background = background[:fold.shape[0], :fold.shape[1]]

    background = background[:fold.shape[0], :fold.shape[1]]
    return background

def applyCircularlySymBGSub2(fold, rmin, radial_bin=10, smooth=5, pc1=0, pc2=25, tension=1):
    """
    Apply Circular Background Subtraction to average fold, and save the result to self.info['bgimg']
    """

    img = makeFullImage(fold)
    img = img.astype("float32")
    width = img.shape[1]
    height = img.shape[0]

    ad = np.ravel(img)
    rmax = width+1

    # Call the new background subtraction function
    background = replicate_bgcsym2(
        AD=ad,
        width=width,
        height=height,
        dmin=rmin,
        dmax=rmax,
        xc=width / 2.0 - 0.5,
        yc=height / 2.0 - 0.5,
        bin_size=radial_bin,
        smooth=smooth,
        tension=tension,
        pc1=pc1,
        pc2=pc2
    )

    background[np.isnan(background)] = 0.0
    background = np.array(background, dtype=np.float32)
    background = background.reshape((height, width))
    background = background[:fold.shape[0], :fold.shape[1]]
    
    return background

def applyRovingWindowBGSub(fold, center, win_size_x=15, win_size_y=15, win_sep_x=10, win_sep_y=10, smooth=0.1, tension=1, pc1=0, pc2=25, bgsub=1):
    """
    Apply Roving Window background subtraction
    :return:
    """

    img = makeFullImage(fold)

    # if "roi_rad" in self.info: # if roi_rad is specified, use it
    #     roi_rad = int(self.info["roi_rad"])
    #     center_x = int(center[0])
    #     center_y = int(center[1])
    #     img = img[center_y - roi_rad:center_y + roi_rad, center_x - roi_rad:center_x + roi_rad]

    width = img.shape[1]
    height = img.shape[0]
    img = np.ravel(img)
    buf = np.array(img, "f")

    pc1 = pc1 / 100.0
    pc2 = pc2 / 100.0

    maxdim = width * height
    maxwin = (win_size_x * 2 + 1) * (win_size_y * 2 + 1)

    # Prepare additional parameters for replicate_bgwsrt2
    xb = np.zeros(maxdim, dtype='f')
    yb = np.zeros(maxdim, dtype='f')
    ys = np.zeros(maxdim, dtype='f')  # Check if needed
    ysp = np.zeros(maxdim, dtype='f') # Check if needed
    wrk = np.zeros(9 * maxdim, dtype='f')  # Workspace array
    bw = np.zeros(maxwin, dtype='f')  # Background window array
    index_bn = np.zeros(maxwin, dtype='int')  # Check if needed
    b = np.zeros(maxdim, dtype='f')  # Background array

    b = replicate_bgwsrt2(buf, b, win_size_x, win_size_y, win_sep_x, win_sep_y, smooth, tension, pc1, pc2, width, height, maxdim, maxwin, xb, yb, ys, ysp, wrk, bw, index_bn, 0, 6)
    b= b.reshape((height, width))

    # if "roi_rad" in self.info:
    #     b = b[:height//2, :width//2]
    #     pad_y = max((fold.shape[0] - b.shape[0]), 0)
    #     pad_x = max((fold.shape[1] - b.shape[1]), 0)
    #     b = np.pad(b, ((pad_y, 0), (pad_x, 0)), 'constant', constant_values=0)
    # else:
    #     b = b[:fold.shape[0], :fold.shape[1]]

    b = b[:fold.shape[0], :fold.shape[1]]
    return b


def applyBackgroundRemoval(method, tmp_avg_fold, avg_fold, tmp_rmin, rmin, \
                           tmp_center, params, downsample_factor=1):
    """
    Apply background removal 
    """
    if method == 'None':
        bg = np.zeros_like(avg_fold)
    elif method == 'White-top-hats':
        bg = applyWhiteTophat(tmp_avg_fold, params["tophat"])
    elif method == 'Circularly-symmetric':
        bg = applyCircularlySymBGSub2(tmp_avg_fold, tmp_rmin, radial_bin=params["radial_bin"], smooth=params["smooth"], pc1=params["cirmin"], pc2=params["cirmax"], tension=params["tension"])
    elif method == 'Roving Window':
        bg = applyRovingWindowBGSub(tmp_avg_fold, tmp_center, win_size_x=params["win_size_x"], win_size_y=params["win_size_y"], win_sep_x=params["win_sep_x"], win_sep_y=params["win_sep_y"], smooth=params["smooth"], tension=params["tension"], pc1=params["cirmin"], pc2=params["cirmax"])
    elif method == 'Smoothed-Gaussian':
        bg = applySmoothedBGSub(tmp_avg_fold, tmp_rmin, params["fwhm"], params["cycles"], typ='gauss')
    elif method == 'Smoothed-Boxcar':
        bg = applySmoothedBGSub(tmp_avg_fold, tmp_rmin, params["boxcar_x"], params["boxcar_y"], params["cycles"], typ='boxcar')
    else:
        print(f"Unknown background removal method: {method}. Not applying background removal.")
        bg = np.zeros_like(avg_fold)
    
    if downsample_factor > 1 and method != 'None':
        # print(f"Upsampling background from downsampled version by factor {downsample_factor}...")
        bg = upsampleImage(bg, factor=downsample_factor)

    if method != 'None':
        result = np.array(avg_fold - bg, dtype=np.float32)
        result = replaceRmin(result, int(rmin), 0.)
    else:
        result = avg_fold
    return result

def makeFullImage(fold):
    """
    Flip + rotate 4 folds and combine them to 1 image
    :param fold:
    :return: result image
    """
    fold_height = fold.shape[0]
    fold_width = fold.shape[1]

    top_left = fold
    top_right = cv2.flip(fold, 1)

    bottom_left = cv2.flip(fold, 0)
    bottom_right = cv2.flip(bottom_left, 1)

    resultImg = np.zeros((fold_height * 2, fold_width * 2))
    resultImg[0:fold_height, 0:fold_width] = top_left
    resultImg[0:fold_height, fold_width:fold_width * 2] = top_right
    resultImg[fold_height:fold_height * 2, 0:fold_width] = bottom_left
    resultImg[fold_height:fold_height * 2, fold_width:fold_width * 2] = bottom_right

    return resultImg
    
# ========================= Background Search ==========================


def log(msg):
    """Print with PID so multiprocessing output can be distinguished."""
    print(f"[PID {os.getpid()}] {msg}", flush=True)

def optimize_mp_wrapper(method, **kwargs):
    # top‑level function → picklable by multiprocessing
    return optimize(method=method, **kwargs)

def optimize(method, **kwargs):
    
    steps = kwargs.get('steps', [10, 7, 5, 3, 1])
    early_stop = kwargs.get('early_stop', 0.0001)
    max_iterations = kwargs.get('max_iterations', 3)
    refine_params = kwargs.get('refine_params', -1)

    log(f">_ Optimizing with method: {method}")

    initial_params = list(method_params[method].values())
    bounds = list(method_bounds[method].values())
    log(f">_ Initial parameters: {initial_params}")
    log(f">_ Parameter bounds: {method_bounds[method]}")

    cur_params = initial_params.copy()
    all_results = []
    history = {}    

    param_order = method_order[method]
    
    log("\n>_ Main optimization loop:")
    # --- Main optimization loop ---
    for param_idx in param_order:
        iter = 0
        log(f"Optimizing parameter {param_idx+1}/{len(cur_params)}")
        best_loss = np.inf
        best_value = cur_params[param_idx]
        step_idx = 0
        no_improve_count = 0
        while step_idx < len(steps):
            step = steps[step_idx]
            if bounds[param_idx][1] < step - bounds[param_idx][0]:
                log(f" Step size {step} too large for parameter bounds, skipping.")
                step_idx += 1
                continue
            candidates = [best_value - step, best_value, best_value + step]
            candidates = [max(bounds[param_idx][0], min(bounds[param_idx][1], v)) for v in candidates]
            candidates = sorted(set(candidates))  # Remove duplicates
            improved = False
            log(f" Testing candidates: {candidates} with step size {step}")
            for v in candidates:
                test_params = cur_params.copy()
                test_params[param_idx] = v
                if tuple(test_params) in history:
                    loss = history[tuple(test_params)]
                else:
                    loss, result = process_file_with_timeout(test_params, method=method, **kwargs)
                    history[tuple(test_params)] = loss
                    all_results.extend(result)
                log(f"  Param {param_idx} = {v}, Loss = {loss:.6f}")
                if loss < best_loss - early_stop:
                    best_loss = loss
                    best_value = v
                    improved = True
            if improved:
                step_idx = 0  # Reset step size if improved
            else:
                step_idx += 1  # Reduce step size if no improvement
            iter += 1
            if iter >= max_iterations:
                log(f"Reached maximum iterations ({max_iterations}) for parameter {param_idx+1}. Stopping optimization for this parameter.")
                break
        cur_params[param_idx] = best_value
        log(f"Best value for param {param_idx+1}: {best_value}, Loss: {best_loss:.6f}")

    # --- Final refinement: grid search with last three step sizes ---
    log("\n>_ Refining best parameters with small grid search:")
    final_steps = steps[-2:]  # last two step sizes
    best_params = cur_params.copy()
    best_loss, result = process_file_with_timeout(best_params, method=method, **kwargs)
    improved = False

    if refine_params != 0:
        for step in final_steps:
            from itertools import product
            refine_indices = param_order[:refine_params] if refine_params > 0 else param_order
            param_ranges = []
            for i in range(len(best_params)):
                if i in refine_indices:
                    vals = [
                        max(bounds[i][0], min(bounds[i][1], best_params[i] + delta))
                        for delta in [-step, 0, step]
                    ]
                    param_ranges.append(sorted(set(vals)))
                else:
                    # Only keep the current value for non-refined params
                    param_ranges.append([best_params[i]])
            log(f" Refinement grid for step {step}: {param_ranges}")
            for candidate in product(*param_ranges):
                candidate = list(candidate)
                if tuple(candidate) in history:
                    loss = history[tuple(candidate)]
                else:
                    loss, result = process_file_with_timeout(candidate, method=method, **kwargs)
                    history[tuple(candidate)] = loss
                    all_results.extend(result)
                if loss < best_loss:
                    print(f"    Improved: {candidate} Loss: {loss:.6f}")
                    best_loss = loss
                    best_params = candidate
                    improved = True
    log(f"\nBest parameters after refinement: {best_params}, Loss: {best_loss:.6f}")

    params_keys = list(method_params[method].keys())
    best_params = {params_keys[i]: best_params[i] for i in range(len(best_params))}
    results = {
        'method': method,
        'best_params': best_params,
        'best_loss': best_loss,
        'all_results': all_results
    }
    return results



import signal

class TimeoutException(Exception):
    pass

def handler(signum, frame):
    raise TimeoutException()


def process_file_with_timeout(values, timeout=60000000, **kwargs):
    signal.signal(signal.SIGALRM, handler)
    signal.alarm(timeout)
    try:
        loss, result = process_file(values, **kwargs)
        signal.alarm(0)  # Disable alarm
        return loss, result
    except TimeoutException:
        log("Processing has timed out!")
        return 1000, []

def process_file(values=None, **kwargs):
    integers = kwargs.get('integers', True)
    method = kwargs.get('method', 'White-top-hats')

    if integers:
        values = [int(x) for x in values]
    else:
        values = [float(x) for x in values]

    values_str = "_".join([str(x) for x in values])
    result = []

    params = {}
    for key in method_params[method].keys():
        params[key] = values[method_order[method].index(list(method_params[method].keys()).index(key))]

    kwargs_bg = {
        'method': kwargs.get('method', 'White-top-hats'),
        'tmp_avg_fold': kwargs['tmp_avg_fold'],
        'avg_fold': kwargs['avg_fold'],
        'tmp_rmin': kwargs['tmp_rmin'],
        'rmin': kwargs['rmin'],
        'tmp_center': kwargs['tmp_center'],
        'params': params,
        'downsample_factor': kwargs['downsample_factor'],

    }

    dimg_fold = applyBackgroundRemoval(**kwargs_bg)

    kwargs_bg['tmp_avg_fold'] = kwargs['tmp_avg_fold_with_syn']
    kwargs_bg['avg_fold'] = kwargs['avg_fold_with_syn']
    dimg_fold_syn = applyBackgroundRemoval(**kwargs_bg)

    dimg = makeFullImage(dimg_fold)
    syn_dimg = makeFullImage(dimg_fold_syn)
    dbg = kwargs['orig_img'] - dimg
    baseline = get_radial_average_rmax(dimg+dbg, kwargs['rmax'], band_width=30)*0.2
    syn_srt = kwargs.get('synthetic_data', None)
    syn_mask = kwargs.get('synthetic_mask', None)
    gen_mask = kwargs.get('mask', None)

    metrics = full_eval_metrics(dimg, dbg, syn_img=syn_dimg, syn_str=syn_srt, syn_mask=syn_mask, \
                                gen_mask=gen_mask, baseline_value=baseline)
    
    # for metric in metrics:
    #     print(f"    - {metric}: {metrics[metric]:.10f}")
    metrics['params'] = values_str
    result.append(metrics)
    loss = metrics['Loss']

    return loss, result