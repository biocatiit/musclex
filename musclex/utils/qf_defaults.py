"""Defaults and UI constants for Quadrant Folding background subtraction."""

from __future__ import annotations

from typing import List, Dict, Any

# Spinbox Ranges
RMIN_RMAX_RANGE = (-1, 5000)
EQUATOR_HEIGHT_RANGE = (1, 1000)
EQUATOR_CENTER_RANGE = (1, 1000)
LAYER_LINE_RANGE = (1, 1000)
BG_PARAM_RANGE = (1, 1000)
TOPHAT_RANGE = (1, 200)
RADIAL_BIN_RANGE = (1, 200)
ITERATIONS_RANGE = (1, 1000)
PIXEL_RANGE_LIMIT = (0, 100)  # percent

# Default Values
DEFAULT_RMIN_RMAX = -1
DEFAULT_EQUATOR_HEIGHT = 70
DEFAULT_EQUATOR_CENTER = 70
DEFAULT_LAYER_SPACING = 100
DEFAULT_LAYER_WIDTH = 5
DEFAULT_GAUSSIAN_FWHM = 15
DEFAULT_BOXCAR_SIZE = 15
DEFAULT_CYCLES = 250
DEFAULT_WINDOW_SIZE = 15
DEFAULT_WINDOW_SEP = 10
DEFAULT_PIXEL_MIN = 0
DEFAULT_PIXEL_MAX = 25
DEFAULT_THETA_BIN_INDEX = 4
DEFAULT_DEGREE_INDEX = 1
DEFAULT_RADIAL_BIN = 10
DEFAULT_SMOOTHING = 0.1
DEFAULT_TENSION = 1.0
DEFAULT_TOPHAT_SIZE = 50
DEFAULT_MAX_ITERATIONS = 10
DEFAULT_EARLY_STOP = 0.01
DEFAULT_MEAN_MSE = 100.0
DEFAULT_MEAN_NEG_SYN = 20.0
DEFAULT_MEAN_BASELINE = 35.0
DEFAULT_MEAN_NEG_CON = 7.0
DEFAULT_MEAN_SMOOTH = 30.0
DEFAULT_EVAL_BASELINE = 0.0
DEFAULT_AMP = 0.0
DEFAULT_SIGMA_X = 0.0
DEFAULT_SIGMA_Y = 0.0
DEFAULT_WEIGHT_MSE = 0.1
DEFAULT_WEIGHT_NEG_SYN = 0.3
DEFAULT_WEIGHT_BASELINE = 0.1
DEFAULT_WEIGHT_NEG_CON = 0.1
DEFAULT_WEIGHT_SMOOTH = 0.4

# Downsample and Frequency Options
DOWNSAMPLE_OPTIONS = ["1", "2", "4"]
DEFAULT_DOWNSAMPLE_INDEX = 1
THETA_BIN_OPTIONS = ["3", "5", "10", "15", "30", "45", "90"]
DEGREE_OPTIONS = ["0.5", "1", "2", "3", "5", "9", "10", "15"]
FREQ_OPTIONS = ["sparse", "medium", "dense"]
DEFAULT_FREQ = "medium"

# BG Subtraction Methods
BG_METHODS = [
    "None",
    "2D Convexhull",
    "Circularly-symmetric",
    "White-top-hats",
    "Smoothed-Gaussian",
    "Smoothed-BoxCar",
    "Roving Window",
]
OPTIMIZATION_METHODS = BG_METHODS[1:]  # Exclude 'None' from optimization options
DEFAULT_OPTIMIZATION_METHODS = ["Circularly-symmetric", "White-top-hats", "Smoothed-Gaussian"]
DEFAULT_OPTIMIZATION_STEPS = "100, 50, 25, 10, 5, 3, 1"


def parse_optimization_steps(raw: str | None = None) -> List[float]:
    """Parse optimization step sizes into a list of numbers."""
    steps_text = (raw or DEFAULT_OPTIMIZATION_STEPS).replace(";", ",").strip()
    if not steps_text:
        return []

    steps: List[float] = []
    for part in steps_text.split(","):
        val = part.strip()
        if not val:
            continue
        try:
            num = float(val)
        except ValueError:
            continue
        if num > 0:
            if abs(num - int(num)) < 1e-9:
                steps.append(int(num))
            else:
                steps.append(num)
    return steps


def build_default_flags() -> Dict[str, Any]:
    """Return default processing flags derived from the dialog defaults."""
    default_bin_theta = int(THETA_BIN_OPTIONS[DEFAULT_THETA_BIN_INDEX])
    default_degree = float(DEGREE_OPTIONS[DEFAULT_DEGREE_INDEX])
    default_downsample = int(DOWNSAMPLE_OPTIONS[DEFAULT_DOWNSAMPLE_INDEX])

    flags: Dict[str, Any] = {
        "bgsub": "None",
        "cirmin": DEFAULT_PIXEL_MIN,
        "cirmax": DEFAULT_PIXEL_MAX,
        "win_size_x": DEFAULT_WINDOW_SIZE,
        "win_size_y": DEFAULT_WINDOW_SIZE,
        "win_sep_x": DEFAULT_WINDOW_SEP,
        "win_sep_y": DEFAULT_WINDOW_SEP,
        "bin_theta": default_bin_theta,
        "radial_bin": DEFAULT_RADIAL_BIN,
        "smooth": DEFAULT_SMOOTHING,
        "tension": DEFAULT_TENSION,
        "tophat": DEFAULT_TOPHAT_SIZE,
        "fwhm": DEFAULT_GAUSSIAN_FWHM,
        "boxcar_x": DEFAULT_BOXCAR_SIZE,
        "boxcar_y": DEFAULT_BOXCAR_SIZE,
        "cycles": DEFAULT_CYCLES,
        "degree": default_degree,
        "downsample": default_downsample,
        "smooth_image": False,
        "optimize": False,
        "methods": list(DEFAULT_OPTIMIZATION_METHODS),
        "steps": parse_optimization_steps(DEFAULT_OPTIMIZATION_STEPS),
        "early_stop": DEFAULT_EARLY_STOP,
        "max_iterations": DEFAULT_MAX_ITERATIONS,
        "bgsub_out": "None",
        "cirmin_out": DEFAULT_PIXEL_MIN,
        "cirmax_out": DEFAULT_PIXEL_MAX,
        "win_size_x_out": DEFAULT_WINDOW_SIZE,
        "win_size_y_out": DEFAULT_WINDOW_SIZE,
        "win_sep_x_out": DEFAULT_WINDOW_SEP,
        "win_sep_y_out": DEFAULT_WINDOW_SEP,
        "bin_theta_out": default_bin_theta,
        "radial_bin_out": DEFAULT_RADIAL_BIN,
        "smooth_out": DEFAULT_SMOOTHING,
        "tension_out": DEFAULT_TENSION,
        "tophat_out": DEFAULT_TOPHAT_SIZE,
        "fwhm_out": DEFAULT_GAUSSIAN_FWHM,
        "boxcar_x_out": DEFAULT_BOXCAR_SIZE,
        "boxcar_y_out": DEFAULT_BOXCAR_SIZE,
        "cycles_out": DEFAULT_CYCLES,
        "mean_metric_values": {
            "MSE_SYN_MEAN": float(DEFAULT_MEAN_MSE),
            "SHARE_NEG_SYN_MEAN": float(DEFAULT_MEAN_NEG_SYN),
            "SHARE_NON_BASELINE_MEAN": float(DEFAULT_MEAN_BASELINE),
            "SHARE_NEG_CON_MEAN": float(DEFAULT_MEAN_NEG_CON),
            "SMOOTH_MEAN": float(DEFAULT_MEAN_SMOOTH),
        },
        "evaluation_baseline": float(DEFAULT_EVAL_BASELINE),
        "amp": float(DEFAULT_AMP),
        "sigma_x_div": float(DEFAULT_SIGMA_X),
        "sigma_y_div": float(DEFAULT_SIGMA_Y),
        "freq": DEFAULT_FREQ,
        "metric_weights": {
            "MSE": float(DEFAULT_WEIGHT_MSE),
            "Share_Neg_Synthetic": float(DEFAULT_WEIGHT_NEG_SYN),
            "Share_Non_Baseline": float(DEFAULT_WEIGHT_BASELINE),
            "Share_Neg_Connected": float(DEFAULT_WEIGHT_NEG_CON),
            "Smoothness": float(DEFAULT_WEIGHT_SMOOTH),
        },
        "equator_mask_height": DEFAULT_EQUATOR_HEIGHT,
        "equator_center_beam_width": DEFAULT_EQUATOR_CENTER,
        "m1": DEFAULT_LAYER_SPACING,
        "layer_line_width": DEFAULT_LAYER_WIDTH,
    }

    return flags
