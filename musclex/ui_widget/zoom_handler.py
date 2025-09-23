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

import sys
from enum import Flag, auto

class ZoomState(Flag):
    DISABLED = auto()
    READY = auto()

class ZoomHandler:
    def __init__(self, imageAxes):
        self.imageAxes = imageAxes
        self.imageFigure = self.imageAxes.figure if self.imageAxes is not None else None
        self.imageCanvas = self.imageFigure.canvas if self.imageFigure is not None else None

        self.state = ZoomState.READY

    def handle_mouse_wheel_scroll_event(self, event):
        if event.inaxes != self.imageAxes:
            return
        base_scale = 1.2  # zoom factor
        if event.button == 'up':   # scroll up to zoom in
            scale_factor = 1 / base_scale
        elif event.button == 'down':  # scroll down to zoom out
            scale_factor = base_scale
        else:
            return

        x = event.xdata
        y = event.ydata
        xlim = self.imageAxes.get_xlim()
        ylim = self.imageAxes.get_ylim()

        new_width = (xlim[1] - xlim[0]) * scale_factor
        new_height = (ylim[1] - ylim[0]) * scale_factor

        relx = (x - xlim[0]) / (xlim[1] - xlim[0])
        rely = (y - ylim[0]) / (ylim[1] - ylim[0])

        self.imageAxes.set_xlim([x - new_width * relx,
                                 x + new_width * (1 - relx)])
        self.imageAxes.set_ylim([y - new_height * rely,
                                 y + new_height * (1 - rely)])
        self.imageCanvas.draw_idle()
