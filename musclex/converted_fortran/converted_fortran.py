from scipy.interpolate import UnivariateSpline
from scipy.ndimage import gaussian_filter, uniform_filter
import scipy.interpolate as interpolate
from scipy.interpolate import UnivariateSpline
import numpy as np
from numba import jit
import math
from scipy.interpolate import UnivariateSpline
from numba import njit, gdb
import sys
import cv2 


@jit(nopython=True, parallel=True)
def intrvl(t, x, n):
    i = 0  # Adjusted for 0-based indexing

    if i >= n - 1:  # Adjusted condition
        i = (n - 1) // 2

    if t < x[i]:
        if t <= x[0]:  # Adjusted index
            return 0
        else:
            il = 1  # Adjusted index
            ih = i
    elif t <= x[i + 1]:
        return i
    elif t >= x[n - 2]:  # Adjusted index
        return n - 2  # Adjusted return value
    else:
        il = i + 1
        ih = n - 2  # Adjusted index

    while True:
        i = (il + ih) // 2
        if t < x[i]:
            ih = i
        elif t > x[i + 1]:
            il = i + 1
        else:
            return i



@jit(nopython=True, parallel=True)
def bcksmooth( # not used
    BUF,
    CBACK,
    B,
    SMBUF,
    VALS,
    OPTIONS,
    XB,
    YB,
    YS,
    YSP,
    SIG,
    WRK,
    IFLAG,
    MAXFUNC,
    ILOG,
    NPIX,
    NRAST,
):
    BOXCA = True
    GAUSS = False
    MERGE = False
    DOEDGE = False
    for i in range(10):
        if OPTIONS[i][0:5] == "BOXCA":
            BOXCA = True
            GAUSS = False
        elif OPTIONS[i][0:5] == "GAUSS":
            GAUSS = True
            BOXCA = False
        elif OPTIONS[i][0:5] == "MERGE":
            MERGE = True
        elif OPTIONS[i][0:4] == "DOED":
            DOEDGE = True
    PI = np.arccos(-1.0)
    J1UNIT = 13
    I1UNIT = 14
    I2UNIT = 15
    ITERM = 5
    IPRINT = 6
    if GAUSS:
        FWHM = int(VALS[0])
        CYCLES = int(VALS[1])
        DMIN = VALS[2]
        DMAX = VALS[3]
        XC1 = VALS[4]
        YC1 = VALS[5]
        LOWVAL = VALS[6]
        if MERGE and not DOEDGE:
            SMOO = VALS[7]
            TENS = VALS[8]
            MSIG = VALS[9]
        PWID = 2 * FWHM + 1
        RWID = PWID
        GCEN = FWHM + 1
        STDEV = FWHM / 2.354
    elif BOXCA:
        PWID = int(VALS[0]) * 2 + 1
        RWID = int(VALS[1]) * 2 + 1
        CYCLES = int(VALS[2])
        DMIN = VALS[3]
        DMAX = VALS[4]
        XC1 = VALS[5]
        YC1 = VALS[6]
        LOWVAL = VALS[7]
        if MERGE and not DOEDGE:
            SMOO = VALS[8]
            TENS = VALS[9]
            MSIG = VALS[10]
    if PWID * RWID > MAXFUNC:
        print("Smoothing function too big for array")
        return
    print("Calculating background...")
    if BOXCA:
        print("Smoothing function = Boxcar, width = ", PWID, " ,height = ", RWID)
    elif GAUSS:
        print("Smoothing function = Gaussian, FWHM = ", FWHM)
    print("Cycles = ", CYCLES)
    print("Pattern limits  Rmin = ", DMIN, " ,Rmax = ", DMAX)
    print("Centre = ", XC1, " , ", YC1)
    BOXCA = True
    GAUSS = False
    MERGE = False
    DOEDGE = False
    SMBUF = [0] * (PWID * RWID)
    if GAUSS:
        GTOT = 0.0
        ARG1 = 1.0 / np.sqrt(2.0 * PI)
        for I in range(PWID):
            for J in range(PWID):
                M = (J - 1) * PWID + I
                ARG2 = (
                    np.sqrt(float(I - GCEN) ** 2.0 + float(J - GCEN) ** 2.0) / STDEV
                ) ** 2.0
                SMBUF[M] = ARG1 * np.exp(-0.5 * ARG2)
                GTOT = GTOT + SMBUF[M]
        for I in range(PWID**2):
            SMBUF[I] = SMBUF[I] / GTOT
    elif BOXCA:
        for I in range(PWID * RWID):
            SMBUF[I] = 1.0 / float(PWID * RWID)
    for J in range(NRAST):
        for I in range(NPIX):
            M = (J - 1) * NPIX + I
            if BUF[M] < LOWVAL:
                BUF[M] = -1.0e30
            else:
                SQRAD = (float(I) - 0.5 - XC1) ** 2 + (float(J) - 0.5 - YC1) ** 2
                if SQRAD < DMIN**2 or SQRAD > DMAX**2:
                    BUF[M] = 0
    if GAUSS:
        for I in range(NPIX * NRAST):
            if IFLAG[I] == 2:
                CBACK[i] = B[i]
            else:
                CBACK[i] = BUF[i]
        for I in range(CYCLES):
            blur(BUF, CBACK, SMBUF, NPIX, NRAST, PWID, RWID, XC1, YC1, B, IFLAG, DOEDGE)
            for J in range(NPIX * NRAST):
                if B[J] > 0 and IFLAG[J] == 1:
                    CBACK[J] = BUF[J] - B[J]
            print("Completed cycle ", I)
    elif BOXCA:
        for I in range(NPIX * NRAST):
            if IFLAG[I] != 1:
                CBACK[I] = 0.0
        if MERGE and not DOEDGE:
            print("Smoothing = ", SMOO, " ,tension = ", TENS)
            print("Weight of imported background = ", MSIG)
            for J in range(NRAST):
                XA = 0.0
                IBCK = 0
                for I in range(NPIX):
                    M = (J - 1) * NPIX + I
                    XA = XA + 1.0
                    if IFLAG[M] == 1:
                        IBCK = IBCK + 1
                        XB[IBCK] = XA
                        YB[IBCK] = CBACK[M]
                        SIG[IBCK] = 1.0
                    elif IFLAG[M] == 2:
                        IBCK = IBCK + 1
                        XB[IBCK] = XA
                        YB[IBCK] = CBACK[M]
                        SIG[IBCK] = MSIG
                if IBCK > 0:
                    EPS = np.sqrt(2.0 / float(IBCK))
                    S = SMOO * float(IBCK)
                    curvs(
                        n=IBCK,
                        x=XB,
                        y=YB,
                        d=1.0,
                        isw=1,
                        s=S,
                        eps=EPS,
                        ys=YS,
                        ysp=YSP,
                        sigma=TENS,
                        temp=WRK,
                        ierr=0,
                    )
                XA = 0.0
                for I in range(NPIX):
                    M = (J - 1) * NPIX + I
                    XA = XA + 1.0
                    if IFLAG[M] > 0:
                        CBACK[M] = curv2(XA, IBCK, XB, YS, YSP, TENS)
                    elif IFLAG[M] == 0:
                        CBACK[M] = 0.0
        else:
            for I in range(NPIX * NRAST):
                if IFLAG[I] != 1:
                    CBACK[I] = 0.0
    return CBACK

@jit(forceobj=True, parallel=True)
def replicate_bcksmooth(image, max_iterations=10, kernel_size=(5, 5), sigmaX=0, tension=0.5,
              edge_background=None, filter_type='gaussian'):
    """
    Updated bcksmooth function with edge handling and filter selection.

    :param image: Input image.
    :param max_iterations: Maximum number of iterations for background estimation.
    :param kernel_size: Size of the filter kernel.
    :param sigmaX: Standard deviation for Gaussian blur.
    :param tension: Tension factor for the spline.
    :param edge_background: Background calculated by another method for edges.
    :param filter_radius: Radius used for filtering, to determine edge regions.
    :param filter_type: 'gaussian' for Gaussian blur or 'boxcar' for boxcar filter.
    :return: Processed image.
    """
    for _ in range(max_iterations):
        # Applying low-pass filter to get overestimated background
        blurred_background = blur_image(image, kernel_size, sigmaX, filter_type)

        diffraction_maxima = np.clip(image - blurred_background, 0, None)

        # Update the background by subtracting these maxima from the original data where the maxima are positive
        image -= diffraction_maxima

        # check convergence
        if np.all(diffraction_maxima == 0):
            print("Converged at iteration", _)
            break


    return image


def handle_edge_effects(final_background, edge_background, filter_radius):
    """
    Handle edge effects by replacing the final background with an edge-corrected version.

    :param final_background: Final background estimate.
    :param edge_background: Background estimated for the edge regions.
    :param filter_radius: Radius used for filtering, to determine edge regions.
    :return: Edge-corrected final background.
    """
    # Identify the edge regions using the filter_radius
    edge_mask = np.ones_like(final_background)
    edge_mask[:filter_radius, :] = 0
    edge_mask[-filter_radius:, :] = 0
    edge_mask[:, :filter_radius] = 0
    edge_mask[:, -filter_radius:] = 0

    # Replace the final background with the edge-corrected version
    corrected_background = np.where(edge_mask, edge_background, final_background)

    return corrected_background

def downsample_data(x, y, factor):
    """
    Downsample the data by selecting every nth point.

    :param x: x-coordinates.
    :param y: y-coordinates.
    :param factor: Downsampling factor (integer).
    :return: Downsampled x and y coordinates.
    """
    return x[::factor], y[::factor]


def merge_backgrounds_with_spline(x, y, z, tension):
    """
    Merge different background estimates using a smoothing spline under tension.

    :param x: x-coordinates for the spline.
    :param y: y-coordinates for the spline.
    :param z: z-coordinates for the spline.
    :param tension: Tension factor for the spline.
    :param downsampling_factor: Factor by which to downsample the data.
    :return: Merged background.
    """
    # Compute the spline on the downsampled data
    spline = interpolate.interp2d(x, y, z, kind='cubic')

    # Evaluate the spline on the original x coordinates
    return spline(x, y)

def blur_image(image, kernel_size=(5, 5), sigmaX=0, filter_type='gaussian'):
    """
    Apply Gaussian blur or boxcar filter to an image.

    :param image: Input image.
    :param kernel_size: Size of the filter kernel.
    :param sigmaX: Standard deviation for Gaussian blur.
    :param filter_type: 'gaussian' for Gaussian blur or 'boxcar' for boxcar filter.
    :return: Blurred image.
    """
    if filter_type == 'gaussian':
        return cv2.GaussianBlur(image, kernel_size, sigmaX)
    elif filter_type == 'boxcar':
        kernel = np.ones(kernel_size, np.float32) / np.prod(kernel_size)
        return cv2.filter2D(image, -1, kernel)
    else:
        raise ValueError("Invalid filter_type. Use 'gaussian' or 'boxcar'.")  


@jit(forceobj=False, parallel=True)
def replicate_bgcsym2(
    AD, width, height, dmin, dmax, xc, yc, bin_size, smooth, tension, pc1, pc2
):
    # Initialize the B array
    B = np.zeros((height, width))

    # Calculate distance for each pixel from the center
    Y, X = np.ogrid[:height, :width]
    distances = np.sqrt((X - xc) ** 2 + (Y - yc) ** 2).ravel()

    # Determine the number of bins and create bin edges
    n_bins = int(np.ceil((dmax - dmin) / bin_size))
    bin_edges = np.linspace(dmin, dmax, n_bins + 1)

    # Initialize arrays for spline fitting
    bin_means = np.zeros(n_bins)
    bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2

    # Binning and averaging
    for i in range(n_bins):
        # Get the indices of pixels falling into the current bin
        indices = (distances >= bin_edges[i]) & (distances < bin_edges[i + 1])

        # Extract the corresponding pixel values and sort them
        pixel_values = np.sort(AD[indices])

        # Calculate percentile range for background
        lower_percentile = int(pc1 * len(pixel_values))
        upper_percentile = int(pc2 * len(pixel_values))

        # Average the pixel values in the percentile range
        bin_means[i] = np.mean(pixel_values[lower_percentile:upper_percentile])

    # Handle cases where there are no pixels in a bin
    bin_means = np.nan_to_num(bin_means)

    # Spline fitting
    smoothing_factor = smooth * len(bin_centers)
    spline = UnivariateSpline(bin_centers, bin_means, s=smoothing_factor)

    # Interpolation to find background values
    # for i in range(height):
    #     for j in range(width):
    #         B[i, j] = spline(distances[i * width + j])
    B = spline(distances).reshape(height, width)

    return B

@jit(nopython=True, parallel=True)
def curvd(t, n, x, y, yp, sigma):
    im1 = intrvl(t, x, n)
    i = im1 + 1

    sigmap = abs(sigma) * (n - 1) / (x[n - 1] - x[0])

    del1 = t - x[im1]
    del2 = x[i] - t
    dels = x[i] - x[im1]
    sum = (y[i] - y[im1]) / dels

    if sigmap != 0:
        sigdel = sigmap * dels
        ss = snhcsh(sigdel, -1)
        c1 = snhcsh(sigmap * del1, 1)
        c2 = snhcsh(sigmap * del2, 1)
        curvd = sum + (yp[i] * (c1 - ss) - yp[im1] * (c2 - ss)) / (
            sigdel * sigmap * (1 + ss)
        )
    else:
        curvd = sum + (
            yp[i] * (2 * del1 * del1 - del2 * (del1 + dels))
            - yp[im1] * (2 * del2 * del2 - del1 * (del2 + dels))
        ) / (6 * dels)

    return curvd



@jit(nopython=True, parallel=True)
def ceez(del1, del2, sigma, c1, c2, c3, n):
    if n == 2:
        c1 = -1 / del1
        c2 = -c1
        return c1, c2
    if sigma != 0:
        del_ = del2 - del1
        c1 = -(del1 + del2) / (del1 * del2)
        c2 = del2 / (del1 * del_)
        c3 = -del1 / (del2 * del_)
        return c1, c2, c3
    else:
        dummy = 0
        coshm1 = 0
        coshm2 = 0
        delp = sigma * (del2 + del1) / 2
        delm = sigma * (del2 - del1) / 2
        sinhmp = 0
        sinhmm = 0
        denom = coshm1 * (del2 - del1) - 2 * del1 * delp * delm * (1 + sinhmp) * (
            1 + sinhmm
        )
        c1 = 2 * delp * delm * (1 + sinhmp) * (1 + sinhmm) / denom
        c2 = -coshm2 / denom
        c3 = coshm1 / denom
        return c1, c2, c3


@jit(nopython=True, parallel=True)
def curv2(t, n, x, y, yp, sigma):
    im1 = intrvl(t, x, n)
    i = im1 + 1

    sigmap = abs(sigma) * (n - 1) / (x[n - 1] - x[0])

    del1 = t - x[im1]
    del2 = x[i] - t
    dels = x[i] - x[im1]

    # Check for zero division in dels
    if dels == 0:
        return 0  # Or some other appropriate value or action

    sum = (y[i] * del1 + y[im1] * del2) / dels

    if sigmap != 0:
        sigdel = sigmap * dels

        # Check for zero division in sigdel
        if sigdel == 0:
            return sum  # Or some other appropriate value or action

        ss, _ = snhcsh(sigdel, -1)
        s1, _ = snhcsh(sigmap * del1, -1)
        s2, _ = snhcsh(sigmap * del2, -1)
        curv2 = sum + (yp[i] * del1 * (s1 - ss) + yp[im1] * del2 * (s2 - ss)) / (sigdel * sigmap * (1 + ss))
    else:
        curv2 = sum - del1 * del2 * (yp[i] * (del1 + dels) + yp[im1] * (del2 + dels)) / (6 * dels)

    return curv2



def bgwsrt2(buf, b, iwid, jwid, isep, jsep, smoo, tens, pc1, pc2, npix, nrast, maxdim, maxwin, xb, yb, ys, ysp, wrk, bw, index, iprint, ilog):
    ifpix = 1
    ilpix = npix
    ifrast = 1
    ilrast = nrast
    rmin = 1.0
    delr = 1.0
    zmin = 1.0
    delz = 1.0

    #print('BGWSRT Window background fitting...', file=iprint)
    #print('BGWSRT Window background fitting...', file=ilog)

    ibad = 0

    nb = (2 * iwid + 1) * (2 * jwid + 1)
    if nb > maxwin:
        #print('BGWSRT Error - window size too large for array', file=iprint)
        #print('BGWSRT Error - window size too large for array', file=ilog)
        raise ValueError('Window size too large for array')
        sys.exit()

    for j in range(1, nrast + 1):
        for i in range(1, npix + 1):
            m = (j - 1) * npix + i
            b[m - 1] = -1.0e+30

    jsep = jwid if jsep <= 0 else jsep
    isep = iwid if isep <= 0 else isep

    for j in range(ifrast, ilrast + 1):
        jw1 = max(j - jwid, 1)
        jw2 = min(j + jwid, nrast)
        zs = float(j) * delz + zmin

        istrt = 0
        iend = 0
        knot = [False] * npix
        for i in range(ifpix, ilpix + 1):
            m = (j - 1) * npix + i
            mm = j * npix - i + 1
            if buf[m - 1] > -1.0e+30:
                if istrt == 0:
                    istrt = i
            if buf[mm - 1] > -1.0e+30:
                if iend == 0:
                    iend = npix - i + 1

        line = False
        if (j - ifrast) % jsep == 0 or j == ilrast:
            niwind = (iend - istrt - 1) // isep + 2
            for in_ in range(1, niwind + 1):
                i = min(istrt + (in_ - 1) * isep, iend)
                knot[i - 1] = True
            line = True
        else:
            for i in range(istrt, iend + 1):
                m = (j - 1) * npix + i 
                if buf[m - npix - 1] < -0.5e+30 or buf[m + npix - 1] < -0.55e+30:
                    knot[i - 1] = True

        ibck = 0
        for i in range(ifpix, ilpix + 1):
            if knot[i - 1]:
                m = (j - 1) * npix + i
                if buf[m - 1] > -1.0e+30:
                    iw1 = max(i - iwid, 1)
                    iw2 = min(i + iwid, npix)
                    nb = 0
                    for jn in range(jw1, jw2 + 1):
                        for in_ in range(iw1, iw2 + 1):
                            mm = (jn - 1) * npix + in_
                            if buf[mm - 1] > -1.0e+30:
                                nb += 1
                                bw[nb - 1] = buf[mm - 1]

                    if nb > 0:
                        bw_sorted_indices = np.argsort(bw[:nb])
                        ip = 0
                        bck = 0.0
                        iw1 = min(int(pc1 * float(nb)) + 1, nb)
                        iw2 = min(int(pc2 * float(nb)) + 1, nb)
                        for iw in range(iw1 - 1, iw2):
                            ip += 1
                            bck += bw[bw_sorted_indices[iw]]

                        ibck += 1
                        yb[ibck - 1] = bck / float(ip)
                        b[m - 1] = yb[ibck - 1]
                        xb[ibck - 1] = float(i) * delr + rmin
                    else:
                        ibad += 1

        if line and ibck > 1:
            eps = np.sqrt(2.0 / float(ibck))
            s = smoo * float(ibck)
            ys, ysp, ier = curvs(ibck, xb, yb, 1.0, isw=1, s=s, eps=eps, ys=ys, ysp=ysp, sigma=tens, temp=wrk)
            if ier != 0:
                #print(f'***Error in spline fitting- FITPACK error {ier}', file=iprint)
                #print(f'***Error in spline fitting- FITPACK error {ier}', file=ilog)
                raise ValueError(f'***Error in spline fitting- FITPACK error {ier}')
                sys.exit('Fatal error')

            for i in range(istrt + 1, iend):
                m = (j - 1) * npix + i
                if buf[m - 1] > -1.0e+30:
                    rs = delr * float(i) + rmin
                    b[m - 1] = curv2(rs, ibck, xb, ys, ysp, tens)

    for i in range(ifpix, ilpix + 1):
        ibck = 0
        for j in range(ifrast, ilrast + 1):
            m = (j - 1) * npix + i
            if b[m - 1] > -1.0e+30:
                ibck += 1
                xb[ibck - 1] = float(j) * delz + zmin
                yb[ibck - 1] = b[m - 1]

        if ibck > 1:
            eps = np.sqrt(2.0 / float(ibck))
            s = smoo * float(ibck)
            ys, ysp, ier = curvs(ibck, xb, yb, 1.0, 1, s, eps, ys, ysp, tens, wrk)
            if ier != 0:
                #print(f'***Error in spline fitting- FITPACK error {ier}', file=iprint)
                #print(f'***Error in spline fitting- FITPACK error {ier}', file=ilog)
                raise ValueError(f'***Error in spline fitting- FITPACK error {ier}')
                sys.exit('Fatal error')

            for j in range(ifrast, ilrast + 1):
                m = (j - 1) * npix + i
                if buf[m - 1] > -1.0e+30:
                    zs = delz * float(j) + zmin
                    b[m - 1] = curv2(zs, ibck, xb, ys, ysp, tens)
    return b
    #print(f'Number of bad background points {ibad}', file=iprint)
    #print(f'Number of bad background points {ibad}', file=ilog)

@jit(nopython=True)
def process_window(buf, iwid, jwid, pc1, pc2, npix, nrast, i, j):
    # Calculate window boundaries
    iw1 = max(i - iwid, 0)
    iw2 = min(i + iwid + 1, npix)
    jw1 = max(j - jwid, 0)
    jw2 = min(j + jwid + 1, nrast)

    # Collect and sort window values, compute average
    max_window_size = (iw2 - iw1) * (jw2 - jw1)
    window_values = np.empty(max_window_size, dtype=np.float32)
    count = 0

    for jn in range(jw1, jw2):
        for in_ in range(iw1, iw2):
            idx = jn * npix + in_
            if buf[idx] > -1.0E+30:
                window_values[count] = buf[idx]
                count += 1

    if count > 0:
        # Only consider the non-empty part of the window
        valid_window_values = window_values[:count]
        valid_window_values.sort()
        start_idx = int(pc1 * count)
        end_idx = int(pc2 * count)
        # Compute the mean manually
        avg_bck = valid_window_values[start_idx:end_idx].sum() / (end_idx - start_idx)
        return avg_bck
    else:
        return -1.0E+30
    
def replicate_bgwsrt2(buf, b, iwid, jwid, isep, jsep, smoo, tens, pc1, pc2, npix, nrast, maxdim, maxwin, xb, yb, ys, ysp, wrk, bw, index, iprint, ilog):
    """
    Implement the background subtraction using the roving window method.

    :param buf: Flattened image data.
    :param b: Background estimate array.
    :param iwid, jwid: Window dimensions.
    :param isep, jsep: Window separations.
    :param smoo, tens: Smoothing and tension parameters.
    :param pc1, pc2: Percentile values for background.
    :param npix, nrast: Image dimensions.
    :param maxdim, maxwin: Maximum dimensions and window size.
    :param xb, yb, ys, ysp, wrk: Working arrays.
    :param bw: Background window array.
    :param index: Index array.
    :param iprint, ilog: Print and log parameters.
    :return: Updated background estimate array.
    """
    b.fill(-1.0E+30)

    # Iterate over the image using the roving window
    for j in range(nrast):
        for i in range(npix):
            b[j * npix + i] = process_window(buf, iwid, jwid, pc1, pc2, npix, nrast, i, j)

    # Fit splines row-wise
    for j in range(nrast):
        valid_indices = np.where(b[j * npix: (j + 1) * npix] > -1.0E+30)[0]
        if len(valid_indices) > 1:
            spline = UnivariateSpline(valid_indices, b[j * npix + valid_indices], s=smoo, k=tens)
            b[j * npix: (j + 1) * npix] = spline(np.arange(npix))

    # Transpose and fit splines column-wise
    b_transposed = b.reshape(nrast, npix).T.flatten()
    for i in range(npix):
        valid_indices = np.where(b_transposed[i * nrast: (i + 1) * nrast] > -1.0E+30)[0]
        if len(valid_indices) > 1:
            spline = UnivariateSpline(valid_indices, b_transposed[i * nrast + valid_indices], s=smoo, k=tens)
            b_transposed[i * nrast: (i + 1) * nrast] = spline(np.arange(nrast))
    b[:] = b_transposed.reshape(npix, nrast).T.flatten()

    return b



@jit(nopython=True, parallel=True)
def curvs(n, x, y, d, isw, s, eps, ys, ysp, sigma, temp):
    print("isw")
    print(isw)
    # Check input parameters
    if n < 2:
        ierr = 1
        return ys, ysp, ierr
    if s < 0:
        ierr = 2
        return ys, ysp, ierr
    if eps < 0 or eps > 1:
        ierr = 3
        return ys, ysp, ierr

    # Check if x-values are strictly increasing
    for i in range(n - 1):
        if x[i] >= x[i + 1]:
            ierr = 4
            return ys, ysp, ierr

    # Check if d-values are positive
    if isinstance(d, float):
        if d <= 0:
            ierr = 5
            return ys, ysp, ierr
    else:
        # Assuming 'd' is an array in this case
        for i in range(n):
            if d[i] <= 0:
                ierr = 5
                return ys, ysp, ierr
    # Decompose temp into required arrays
    temp_size = 9 * n
    td = temp[:n]
    tsd1 = temp[n : 2 * n]
    hd = temp[2 * n : 3 * n]
    hsd1 = temp[3 * n : 4 * n]
    hsd2 = temp[4 * n : 5 * n]
    rd = temp[5 * n : 6 * n]
    rsd1 = temp[6 * n : 7 * n]
    rsd2 = temp[7 * n : 8 * n]
    v = temp[8 * n : 9 * n]

    # Call curvss with the decomposed temp array components
    ys, ysp, ierr = curvss(
        n, x, y, d, isw, s, eps, ys, ysp, sigma, td, tsd1, hd, hsd1, hsd2, rd, rsd1, rsd2, v
    )

    return ys, ysp, ierr

@jit(nopython=True, parallel=True)
def sort(x, n, ind):
    for i in range(n):
        ind[i] = i
    if n == 1:
        return
    l = n // 2 + 1
    ir = n
    while True:
        if l > 1:
            l -= 1
            itmp = ind[l - 1]
            q = x[itmp]
        else:
            itmp = ind[ir - 1]
            q = x[itmp]
            ind[ir - 1] = ind[0]
            ir -= 1
            if ir == 1:
                ind[0] = itmp
                return
        i = l
        j = l + l
        while j <= ir:
            if j < ir and x[ind[j - 1]] < x[ind[j]]:
                j += 1
            if q < x[ind[j - 1]]:
                ind[i - 1] = ind[j - 1]
                i = j
                j += j
            else:
                j = ir + 1
        ind[i - 1] = itmp


@jit(nopython=True, parallel=True)
def curvss(n, x, y, d, isw, s, eps, ys, ysp, sigma, td, tsd1, hd, hsd1, hsd2, rd, rsd1, rsd2, v):
    ierr = 0

    if n < 2:
        ierr = 1
        return None, None, ierr

    if s < 0.:
        ierr = 2
        return None, None, ierr

    if eps < 0. or eps > 1.:
        ierr = 3
        return None, None, ierr

    p = 0.
    v[0] = 0.
    v[n-1] = 0.
    ysp[0] = 0.
    ysp[n-1] = 0.

    if n == 2:
        store_smoothed_values(n, y, v, p, ys, ysp)
        return ys, ysp, ierr

    rsd1[0] = 0.
    rd[0] = 0.
    rsd2[n-1] = 0.
    rdim1 = 0.
    yspim2 = 0.

    sigmap = abs(sigma) * (n-1) / (x[n-1] - x[0])

    nm1 = n-1
    nm3 = n-3
    delxi1 = 1.
    delyi1 = 0.
    dim1 = 0.

    for i in range(nm1):
        delxi = x[i+1] - x[i]
        if delxi <= 0.:
            ierr = 4
            return ys, ysp, ierr
        delyi = (y[i+1] - y[i]) / delxi
        ys[i] = delyi - delyi1
        di, tsd1[i+1] = terms(sigmap, delxi)  # terms function needs implementation
        td[i] = di + dim1
        hd[i] = -(1. / delxi + 1. / delxi1)
        hsd1[i+1] = 1. / delxi
        delxi1 = delxi
        delyi1 = delyi
        dim1 = di

    sl = s * (1. - eps)
    su = s * (1. + eps)

    if isw == 1:
        if d <= 0.:
            ierr = 5
            return ys, ysp, ierr
        sl = d * d * sl
        su = d * d * su
        hsd1p = 0.
        hdim1 = 0.
        for i in range(1, nm1):
            hdi = hd[i]
            hd[i] = hsd1[i] * hsd1[i] + hdi * hdi + hsd1[i+1] * hsd1[i+1]
            hsd2[i] = hsd1[i] * hsd1p
            hsd1p = hsd1[i]
            hsd1[i] = hsd1p * (hdi + hdim1)
            hdim1 = hdi

        for i in range(1, nm1):
            rsd2i = hsd2[i]
            rsd1i = p * tsd1[i] + hsd1[i] - rsd2i * rsd1[i-1]
            rsd2[i] = rsd2i * rdim1
            rdim1 = rd[i-1]
            rsd1[i] = rsd1i * rdim1
            rd[i] = 1. / (p * td[i] + hd[i] - rsd1i * rsd1[i] - rsd2i * rsd2[i])
            ysp[i] = ys[i] - rsd1[i] * ysp[i-1] - rsd2[i] * yspim2
            yspim2 = ysp[i-1]

        ysp[nm1] = rd[nm1] * ysp[nm1]
        if n == 3:
            ysp[1] = rd[1] * ysp[1] - rsd1[2] * ysp[2]
        else:
            for ibak in range(1, nm3+1):
                i = nm1 - ibak
                ysp[i - 1] = rd[i - 1] * ysp[i - 1] - rsd1[i] * ysp[i] - rsd2[i+1] * ysp[i+1]
        sum = 0.
        delyi1 = 0.
        for i in range(nm1 - 1):  # Python indexing is 0-based, so nm1 - 1
            delyi = (ysp[i + 1] - ysp[i]) / (x[i + 1] - x[i])
            v[i] = delyi - delyi1
            sum += v[i] * (delyi - delyi1)
            delyi1 = delyi

        # Handle the last element of v
        v[nm1 - 1] = -delyi1  # Adjusted to access the last element correctly

        # Adjust the sum calculation for the last element
        sum -= v[nm1 - 1] * delyi1

        if sum <= su:
            store_smoothed_values(n, y, v, p, ys, ysp)
            return ys, ysp, ierr

        f = 0.
        g = 0.
        wim2 = 0.
        wim1 = 0.
        for i in range(1, nm1):
            tui = tsd1[i] * ysp[i-1] + td[i] * ysp[i] + tsd1[i+1] * ysp[i+1]
            wi = tui - rsd1[i] * wim1 - rsd2[i] * wim2
            f += tui * ysp[i]
            g += wi * wi * rd[i]
            wim2 = wim1
            wim1 = wi

        h = f - p * g
        if h <= 0:
            store_smoothed_values(n, y, v, p, ys, ysp)
            return ys, ysp, ierr

        step = (sum - np.sqrt(sum * sl)) / h
        if sl != 0:
            step *= np.sqrt(sum / sl)
        p += step

    store_smoothed_values(n, y, v, p, ys, ysp)
    return ys, ysp, ierr

@jit(nopython=True, parallel=True)
def store_smoothed_values(n, y, v, p, ys, ysp):
    for i in range(n):
        ys[i] = y[i] - v[i]
        ysp[i] = p * ysp[i]



@jit(nopython=True, parallel=True)
def terms(sigma, del_step):
    if sigma != 0:
        sigdel = sigma * del_step
        sinhm, coshm = np.sinh(sigdel), np.cosh(sigdel)
        denom = sigma * sigdel * (1. + sinhm)
        diag = (coshm - sinhm) / denom
        sdiag = sinhm / denom
    else:
        diag = del_step / 3.
        sdiag = del_step / 6.

    return diag, sdiag


@jit(nopython=True, parallel=True)
def snhcsh(x, isw):
    sp13 = 0.3029390e-5
    sp12 = 0.1975135e-3
    sp11 = 0.8334261e-2
    sp10 = 0.1666665e0
    sp24 = 0.3693467e-7
    sp23 = 0.2459974e-5
    sp22 = 0.2018107e-3
    sp21 = 0.8315072e-2
    sp20 = 0.1667035e0
    sp33 = 0.6666558e-5
    sp32 = 0.6646307e-3
    sp31 = 0.4001477e-1
    sq32 = 0.2037930e-3
    sq31 = -0.6372739e-1
    sq30 = 0.6017497e1
    sp43 = 0.2311816e-4
    sp42 = 0.2729702e-3
    sp41 = 0.9868757e-1
    sq42 = 0.1776637e-3
    sq41 = -0.7549779e-1
    sq40 = 0.9110034e1
    cp4 = 0.2982628e-6
    cp3 = 0.2472673e-4
    cp2 = 0.1388967e-2
    cp1 = 0.4166665e-1
    cp0 = 0.5000000e0

    ax = abs(x)
    sinhm = coshm = 0  # Initialize to default values

    # Check for very small ax to avoid division by zero
    if ax < 1e-10:
        sinhm = ax  # For very small x, sinh(x) ≈ x
        coshm = 1.0  # For very small x, cosh(x) ≈ 1
        return sinhm, coshm
    # Corrected conditions for calculating sinhm
    if ax > 10.1:
        xs = ax * ax
        sinhm = (
            xs * (((sp43 * xs + sp42) * xs + sp41) * xs + 1.0)
            / ((sq42 * xs + sq41) * xs + sq40)
        )
    elif ax > 7.65:
        xs = ax * ax
        sinhm = (
            xs * (((sp33 * xs + sp32) * xs + sp31) * xs + 1.0)
            / ((sq32 * xs + sq31) * xs + sq30)
        )
    elif ax > 4.45:
        xs = ax * ax
        if ax > 2.3:
            sinhm = xs * (((sp24 * xs + sp23) * xs + sp22) * xs + sp21) * xs + sp20
        else:
            sinhm = xs * (((sp13 * xs + sp12) * xs + sp11) * xs + sp10)
    else:
        sinhm = np.exp(ax) / (ax + ax) - 1.0

    # Corrected conditions for calculating coshm
    if isw >= 2:
        if ax > 2.3:
            xs = ax * ax
            coshm = xs * (((cp4 * xs + cp3) * xs + cp2) * xs + cp1) * xs + cp0
        else:
            expx = np.exp(ax)
            coshm = (expx + 1.0 / expx) / 2.0 - 1.0

    if isw >= 3:
        xs = ax * ax
        if ax > 2.3:
            coshm = xs * (((cp4 * xs + cp3) * xs + cp2) * xs + cp1)
        else:
            expx = np.exp(ax)
            coshm = ((expx + 1.0 / expx - xs) / 2.0 - 1.0) / xs

    return sinhm, coshm


def snhcsh2(sinhm, coshm, x, isw):
    sp14 = 0.227581660976348e-7
    sp13 = 0.612189863171694e-5
    sp12 = 0.715314759211209e-3
    sp11 = 0.398088289992973e-1
    sq12 = 0.206382701413725e-3
    sq11 = -0.611470260009508e-1
    sq10 = 0.599999999999986e1
    sp25 = 0.129094158037272e-9
    sp24 = 0.473731823101666e-7
    sp23 = 0.849213455598455e-5
    sp22 = 0.833264803327242e-3
    sp21 = 0.425024142813226e-1
    sq22 = 0.106008515744821e-3
    sq21 = -0.449855169512505e-1
    sq20 = 0.600000000268619e1
    sp35 = 0.155193945864942e-9
    sp34 = 0.511529451668737e-7
    sp33 = 0.884775635776784e-5
    sp32 = 0.850447617691392e-3
    sp31 = 0.428888148791777e-1
    sq32 = 0.933128831061610e-4
    sq31 = -0.426677570538507e-1
    sq30 = 0.600000145086489e1
    sp45 = 0.188070632058331e-9
    sp44 = 0.545792817714192e-7
    sp43 = 0.920119535795222e-5
    sp42 = 0.866559391672985e-3
    sp41 = 0.432535234960858e-1
    sq42 = 0.824891748820670e-4
    sq41 = -0.404938841672262e-1
    sq40 = 0.600005006283834e1
    cp5 = 0.552200614584744e-9
    cp4 = 0.181666923620944e-6
    cp3 = 0.270540125846525e-4
    cp2 = 0.206270719503934e-2
    cp1 = 0.744437205569040e-1
    cq2 = 0.514609638642689e-4
    cq1 = -0.177792255528382e-1
    cq0 = 0.200000000000000e1
    zp4 = 0.664418805876835e-8
    zp3 = 0.218274535686385e-5
    zp2 = 0.324851059327161e-3
    zp1 = 0.244515150174258e-1
    zq2 = 0.616165782306621e-3
    zq1 = -0.213163639579425e0
    zq0 = 0.240000000000000e2

    ax = abs(x)
    if isw >= 0:
        if ax > 3.9:
            if ax > 6.1:
                sinhm = (expx - 1.0 / expx) / (ax + ax) - 1.0
            else:
                xs = ax * ax
                sinhm = (
                    xs
                    * (
                        ((((sp45 * xs + sp44) * xs + sp43) * xs + sp42) * xs + sp41)
                        * xs
                        + 1.0
                    )
                    / ((sq42 * xs + sq41) * xs + sq40)
                )
        else:
            if ax > 2.2:
                sinhm = (
                    xs
                    * (
                        ((((sp35 * xs + sp34) * xs + sp33) * xs + sp32) * xs + sp31)
                        * xs
                        + 1.0
                    )
                    / ((sq32 * xs + sq31) * xs + sq30)
                )
            else:
                sinhm = xs * (
                    (((sp25 * xs + sp24) * xs + sp23) * xs + sp22) * xs + sp21
                ) * xs + 1.0 / ((sq22 * xs + sq21) * xs + sq20)
    else:
        if isw >= 2:
            xs = ax * ax
            if ax > 2.2:
                coshm = ((expx + 1.0 / expx - xs) / 2.0 - 1.0) / xs
                if isw == 3:
                    sinhm = (expx - 1.0 / expx) / (ax + ax) - 1.0
            else:
                coshm = (
                    xs
                    * (
                        ((((cp5 * xs + cp4) * xs + cp3) * xs + cp2) * xs + cp1) * xs
                        + 1.0
                    )
                    / ((cq2 * xs + cq1) * xs + cq0)
                )
                if isw == 0:
                    sinhm = xs * (
                        (((sp25 * xs + sp24) * xs + sp23) * xs + sp22) * xs + sp21
                    ) * xs + 1.0 / ((sq22 * xs + sq21) * xs + sq20)
        else:
            expx = np.exp(ax)
            coshm = (expx + 1.0 / expx) / 2.0 - 1.0
            if isw == 0:
                sinhm = (expx - 1.0 / expx) / (ax + ax) - 1.0

    return sinhm, coshm


#@jit(nopython=True, parallel=True)
def blur(BUF, CBACK, SMBUF, NPIX, NRAST, PWID, RWID, XC, YC, TEMPBUF, IFLAG, DOEDGE):
    for J in range(1, NRAST + 1):
        for I in range(1, NPIX + 1):
            M = (J - 1) * NPIX + I
            IFLAG2 = 0
            TEMPBUF[M - 1] = 0.0
            if IFLAG[M - 1] == 1:
                for L in range(1, RWID + 1):
                    for K in range(1, PWID + 1):
                        SPIX = I + K - (PWID + 1) // 2
                        SRAST = J + L - (RWID + 1) // 2
                        if SPIX > 0 and SPIX <= NPIX and SRAST > 0 and SRAST <= NRAST:
                            SPOINT = (SRAST - 1) * NPIX + SPIX
                            if BUF[SPOINT - 1] > -0.5e30:
                                TEMPBUF[M - 1] += (
                                    CBACK[SPOINT - 1] * SMBUF[(L - 1) * PWID + K - 1]
                                )
                            else:
                                IFLAG2 += 1
                        else:
                            IFLAG2 += 1
                if IFLAG2 > 0:
                    TEMPBUF[M - 1] *= float(PWID * RWID) / float(PWID * RWID - IFLAG2)
                TEMPBUF[M - 1] = BUF[M - 1] - TEMPBUF[M - 1]
    return TEMPBUF


def blurlimits(BUF, NPIX, NRAST, PWID, RWID, XC, YC, IFLAG, DOEDGE):
    for J in range(1, NRAST + 1):
        for I in range(1, NPIX + 1):
            M = (J - 1) * NPIX + I
            IFLAG[M - 1] = 0
            if BUF[M - 1] > -0.5e30 and DOEDGE:
                IFLAG[M - 1] = 1
            elif BUF[M - 1] > -0.5e30 and not DOEDGE:
                for L in range(1, RWID + 1):
                    for K in range(1, PWID + 1):
                        SPIX = I + K - (PWID + 1) // 2
                        SRAST = J + L - (RWID + 1) // 2
                        if SPIX > 0 and SPIX <= NPIX and SRAST > 0 and SRAST <= NRAST:
                            SPOINT = (SRAST - 1) * NPIX + SPIX
                            if BUF[SPOINT - 1] > -0.5e30:
                                IFLAG[M - 1] = 1
                            else:
                                IFLAG[M - 1] = 2
                        else:
                            IFLAG[M - 1] = 2
                            break
    return IFLAG