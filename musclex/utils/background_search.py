
import numpy as np
from scipy.signal import find_peaks, peak_widths


# =============== Parameters for Masking ===============

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




# ========================= Background Search ==========================