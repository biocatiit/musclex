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
import cv2
from PySide6.QtCore import Qt
from PySide6.QtWidgets import (QApplication,
                               QWidget,
                               QPushButton,
                               QVBoxLayout)
import matplotlib.patches as patches

from .ui_widget import UIWidget


class ZoomInWidgetState(Flag):
    INIT = auto()

class ZoomInWidget(UIWidget):
    def __init__(self, imageAxes):
        super().__init__(imageAxes)
        self.state = ZoomInWidgetState.INIT

        self.zoomInBtn = QPushButton("Zoom In")

        self.layout = QVBoxLayout(self)
        self.layout.addWidget(self.zoomInBtn)

        self.zoomInBtn.clicked.connect(self.zoomInBtnClick)

        self.zoomInPoints = []

        self.set_ready()

    def set_ready(self):
        self.zoomInBtn.setChecked(False)

        self.remove_image_lines(labels=["Zoomin Red Dot", "zoom_region"])
        self.imageCanvas.draw_idle()

        self.zoomInPoints.clear()
        super().set_ready()

    def handle_mouse_move_event(self, event):
        if not self.is_enabled():
            return

        if event.inaxes != self.imageAxes:
            return

        x = event.xdata
        y = event.ydata

        ax = self.imageAxes

        # Remove old lines
        self.remove_image_lines(labels=["Zoomin Red Dot", "zoom_region"])
        # Draw cursor location in image using red cross lines.
        axis_size = 5

        ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r', label="Zoomin Red Dot")
        ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r', label="Zoomin Red Dot")

        if len(self.zoomInPoints) == 1:
            # Draw rectangle

            start_pt = self.zoomInPoints[0]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                        linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted',
                                        label="zoom_region"))

    def click_with_double_zoom(self, event):
        # Do nothing and wait for user
        #   to click double-zoom image.
        pass

    def handle_click_event(self, event, x, y):
        if not self.is_enabled():
            return

        self.zoomInPoints.append((x, y))

        if len(self.zoomInPoints) == 2:
            x1, y1 = self.zoomInPoints[0]
            x2, y2 = self.zoomInPoints[1]
            img_zoom = [(min(x1, x2), max(x1, x2)),
            (min(y1, y2), max(y1, y2))]
            self.resizeImage(img_zoom)
            self.set_ready()

    def resizeImage(self, img_zoom):
            self.imageAxes.set_xlim(img_zoom[0])
            self.imageAxes.set_ylim(img_zoom[1])

            # self.imageFigure.tight_layout()
            self.imageCanvas.draw_idle()

    def zoomInBtnClick(self, btnChecked):
        if btnChecked:
            self.set_enabled()
        else:
            self.set_ready()
