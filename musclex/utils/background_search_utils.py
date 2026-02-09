method_bounds = {
    "None": {
    },
    "Average": {
    },
    "2D Convexhull": {
        "degree": [0.5, 2], # degree of polynomial fit
    },
    "Circularly-symmetric": {
        "smooth": [1, 5], # any positive number (larger numbers means more smoothing)
        "tension": [1, 5], 
        "radial_bin": [2, 200], # in pixels
        "cirmin": [0, 5], # in pixels
        "cirmax": [5, 40], # in pixels
    },
    "White-top-hats": {
        "tophat": [1, 150],
    },
    "Roving Window": {
        "tension": [1, 5],  # must be 1 <= t <= 5 (3 is a cubic spline)
        "win_size_x": [5, 100], # in pixels
        "win_size_y": [5, 100], # in pixels
        "win_sep_x": [2, 50], # in pixels
        "win_sep_y": [2, 50], # in pixels 
        "smooth": [0, 0],
        "cirmin": [0, 5],
        "cirmax": [5, 40], # in %
    },
    "Smoothed-Gaussian": {
        "fwhm": [1, 100], # in pixels
        "cycles": [40, 260],
    },
    "Smoothed-BoxCar": {
        "boxcar_x": [1, 100],
        "boxcar_y": [1, 100],
        "cycles": [40, 260],
    },
}

method_order = {
    "None": [],
    "Average": [],
    "2D Convexhull": [0],
    "Circularly-symmetric": [2,3,4,0,1],
    "White-top-hats": [0],
    "Roving Window": [1, 2, 3, 4, 6, 7, 5, 0],
    "Smoothed-Gaussian": [0,1],
    "Smoothed-BoxCar": [0,1,2],
}


method_params = {
    "None": {
    },
    "Average": {
    },
    "2D Convexhull": {
        "degree": 1,
    },
    "Circularly-symmetric": {
        "smooth": 1, # [0, 200] any positive number (larger numbers means more smoothing)
        "tension": 1, # NOT used. set as 1.
        "radial_bin": 10, # in pixels # 2-500
        "cirmin": 0, # in pixels 0-24
        "cirmax": 25, # in pixels 25-50
    },
    "White-top-hats": {
        "tophat": 70,
    },
    # multiple parameters
    "Roving Window": {
        "tension": 1,  # must be 1 <= t <= 5 (3 is a cubic spline)
        "win_size_x": 15, # in pixels
        "win_size_y": 15, # in pixels
        "win_sep_x": 10, # in pixels
        "win_sep_y": 10, # in pixels
        "smooth": 0,
        "cirmin": 0,
        "cirmax": 25, # in %

    },
    "Smoothed-Gaussian": {
        "fwhm": 10, # in pixels
        "cycles": 200,
    },
    "Smoothed-BoxCar": {
        "boxcar_x": 10,
        "boxcar_y": 10,
        "cycles": 200,
    },
}