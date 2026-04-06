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

import shutil
import os
import sys
import numpy as np
from musclex import __version__


def qFromCenter(point, center, scale):
    """Compute calibrated reciprocal-space coordinates from a pixel position.

    Args:
        point:  (x, y) pixel coordinates of the cursor/picked point.
        center: (cx, cy) pixel coordinates of the beam center.
        scale:  pixels per nm⁻¹  (= radius_px × d_spacing_nm  for image
                calibration, or  λ·SDD / pixel_size  for instrument params).

    Returns:
        (q_x, q_y, q_R, unit) where
            q_x  = signed distance from the meridian  [nm⁻¹]
            q_y  = signed distance from the equator   [nm⁻¹]
            q_R  = radial distance from center         [nm⁻¹]
            unit = "nm\u207b\u00b9"  (nm⁻¹)
    """
    unit = "nm\u207b\u00b9"
    try:
        x, y = point
        q_x = (x - center[0]) / scale
        q_y = (y - center[1]) / scale
        q_R = np.sqrt(q_x ** 2 + q_y ** 2)
    except Exception as e:
        print("Error calculating q from center:", e)
        return None, None, None, unit
    return q_x, q_y, q_R, unit


def inverseNmFromCenter(point, center, scale, unit="nm^-1"):
    """Deprecated wrapper — use qFromCenter instead.

    Kept for backward compatibility with any external callers.
    Returns (q_R, unit) where q_R is the radial distance in nm⁻¹.
    """
    q_x, q_y, q_R, u = qFromCenter(point, center, scale)
    if q_R is None:
        return -1, unit
    return f"{q_R:.4f}", u