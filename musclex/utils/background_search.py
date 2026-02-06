
import numpy as np
from scipy.signal import find_peaks, peak_widths
from scipy import ndimage

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


def full_eval_metrics(dimg, dbg, art_img, art_str, mask_art, mask_equator, baseline_value, min_neg_con_pixels=9, \
                      normalize_metrics=True, max_amp=0.01):
    if art_str is None or mask_art is None:
        mse = 0
        share_neg_synthetic = 0
    else:
        mse = artificial_data_mse(art_img, art_str, mask_art, mask_equator, normalize=True, max_amp=max_amp)
        share_neg_synthetic = share_neg_syn(art_img, art_str, mask_art)
    
    share_neg_general = share_neg_gen(dimg, mask_equator)
    share_non_baseline_pixels = share_non_baseline(dimg, baseline_value, mask_equator, clip=[2,5])
    share_neg_connected = share_neg_con(dimg, mask_equator, min_pixels=min_neg_con_pixels)
    smoothness_value = smoothness(dimg, dbg, mask_equator)

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



def artificial_data_mse(dimg, art_str, mask_art, mask_equator=None, normalize=True, max_amp=0.01):
    dimg = dimg * mask_art
    art_str = art_str * mask_art
    
    mse = np.mean((dimg - art_str)**2)
    if normalize and mask_equator is not None:
        mse = mse / (np.sum(mask_art) / np.sum(mask_equator)) * 0.02 
    return mse


def share_neg_syn(dimg, art_str, mask_art):
    dimg = dimg * mask_art
    art_str = art_str * mask_art
    neg_pixels = np.sum((dimg - art_str) < 0)
    total_pixels = np.sum(mask_art)
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


# ========================= Background Search ==========================