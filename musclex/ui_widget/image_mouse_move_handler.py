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
from matplotlib.backend_bases import MouseButton


class ImageMouseMoveState(Flag):
    DISABLED = auto()
    READY = auto()
    MOUSE_PRESSED = auto()
    MOUSE_DRAGGING = auto()
    MOUSE_DRAG_COMPLETED = auto()
    MOUSE_MOVING = auto()
    CLICKED = auto()

class ImageMouseMoveHandler:
    def __init__(self, imageAxes, img):
        self.imageAxes = imageAxes
        self.img = img

        self.mouse_pressed = False
        self.press_x = None
        self.press_y = None
        self.state = ImageMouseMoveState.READY

    def handle_mouse_button_press_event(self, event):
        if event.button != MouseButton.LEFT:
            return

        if event.inaxes != self.imageAxes:
            return

        x = event.xdata
        y = event.ydata

        self.press_x = x
        self.press_y = y
        self.state = ImageMouseMoveState.MOUSE_PRESSED

    def handle_mouse_move_event(self, event):
        if event.inaxes != self.imageAxes:
            return

        x = event.xdata
        y = event.ydata

        if self.state != ImageMouseMoveState.MOUSE_DRAGGING:
            if self.state == ImageMouseMoveState.MOUSE_PRESSED:
                # If moved enough, switch to dragging state.
                # Otherwise, still remain pressed state.
                dragging = self.check_dragging(x, y)

                if dragging:
                    self.state = ImageMouseMoveState.MOUSE_DRAGGING
            else:
                self.state = ImageMouseMoveState.MOUSE_MOVING

        # Move Image
        if self.state == ImageMouseMoveState.MOUSE_DRAGGING:
            # dx = x - self.last_drag_x
            # dy = y - self.last_drag_y
            dx = x - self.press_x
            dy = y - self.press_y

            xlim = self.imageAxes.get_xlim()
            ylim = self.imageAxes.get_ylim()

            left = xlim[0] - dx
            right = xlim[1] - dx
            bottom = ylim[0] - dy
            top = ylim[1] - dy

            if left < 0:
                left = 0
                right = left + (xlim[1] - xlim[0])

            if right > self.img.shape[1]:
                right = self.img.shape[1]
                left = right - (xlim[1] - xlim[0])

            if bottom < 0:
                bottom = 0
                top = bottom + (ylim[1] - ylim[0])

            if top > self.img.shape[0]:
                top = self.img.shape[0]
                bottom = top - (ylim[1] - ylim[0])

            self.imageAxes.set_xlim(left, right)
            self.imageAxes.set_ylim(bottom, top)

    def handle_mouse_button_release_event(self, event):
        if event.button != MouseButton.LEFT:
            return

        if event.inaxes != self.imageAxes:
            return

        x = event.xdata
        y = event.ydata

        if self.state == ImageMouseMoveState.MOUSE_DRAGGING:
            self.state = ImageMouseMoveState.MOUSE_DRAG_COMPLETED
        elif self.state == ImageMouseMoveState.MOUSE_PRESSED:
            self.state = ImageMouseMoveState.CLICKED

        self.press_x = None
        self.press_y = None

    def check_dragging(self, x, y):
        """
        If moved enough, switch to dragging mode.
        """
        move_threshold = 2.0  # pixels in data coords
        dx = abs(x - self.press_x)
        dy = abs(y - self.press_y)
        dragging = dx > move_threshold or dy > move_threshold
        return dragging
