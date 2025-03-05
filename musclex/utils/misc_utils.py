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


def inverseNmFromCenter(point, center, scale, unit="nm^-1"):
    """ Takes a point, center coordinates, a scale, and a unit.
    Returns the point's inverse distance from center in nanometers and the unit"""

    try:
        #Calculate inverse nm from center
        x,y = point
        distance = np.sqrt((center[0] - x) ** 2 + (center[1] - y) ** 2)
        d = distance / scale
        if (d > 0.01):
            q = 1.0/d
        else:
            q = distance
        q = f"{q:.4f}"
    except Exception as e:
        #Return error indicating values and print error to console if there is an error
        print("Error caluclating the inverse nm from center: ", e)
        return -1, unit
    else:
        #return values if there is no error.
        return q, unit