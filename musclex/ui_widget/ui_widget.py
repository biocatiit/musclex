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
from PySide6.QtCore import Qt
from PySide6.QtWidgets import (QApplication,
                               QWidget,
                               QPushButton,
                               QVBoxLayout)


class UIWidgetState(Flag):
    DISABLED = auto()
    READY = auto()
    RUNNING = auto()
    PAUSED = auto()

class UIWidget(QWidget):
    def __init__(self, imageAxes=None):
        super().__init__()
        self.imageAxes = imageAxes

    stateChanged = Signal(bool)

    def set_ready(self):
        self.stateChanged.emit(False)

    def set_running(self):
        self.stateChanged.emit(True)

    def remove_image_lines(self, ax=None, labels=None):
        if ax is None:
            ax = self.imageAxes

        if labels:
            for i in range(len(ax.lines)-1, -1, -1):
                if ax.lines[i].get_label() in labels:
                    ax.lines[i].remove()

            for p in ax.patches:
                if p.get_label() in labels:
                    p.remove()
        else:
            for i in range(len(ax.lines)-1, -1, -1):
                ax.lines[i].remove()

            for p in ax.patches:
                p.remove()
