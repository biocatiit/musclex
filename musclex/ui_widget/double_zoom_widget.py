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
from PySide6.QtCore import Qt, Slot, Signal
from PySide6.QtWidgets import (QApplication,
                               QWidget,
                               QCheckBox,
                               QMessageBox,
                               QVBoxLayout)

from .ui_widget import UIWidget


class DoubleZoomWidgetState(Flag):
    INIT = auto()

    MainImageClicked = auto()
    DoubleZoomImageClicked = auto()

    MainImageMouseMoving = auto()
    DoubleZoomImageMouseMoving = auto()

    MainImageMultipleClicked = auto()
    DoubleZoomImageMultipleClicked = auto()


class DoubleZoomWidget(UIWidget):
    def __init__(self,
        imageAxes=None,
        parent=None,
        dontShowMessage=False):
        super().__init__(imageAxes)

        self.state = DoubleZoomWidgetState.INIT

        self.parent = parent

        self.doubleZoomAxes = None
        self.doubleZoomCheckbox = QCheckBox("Double Zoom")
        self.layout = QVBoxLayout(self)
        self.layout.addWidget(self.doubleZoomCheckbox)

        self.doubleZoomCheckbox.checkStateChanged.connect(self.handleDoubleZoomCheckedEvent)

        # Mouse click point in main image
        self.mainImagePoint = (0, 0)
        # Mouse click point in double zoom image
        self.doubleZoomPoint = (0, 0)
        self.dontShowAgainDoubleZoomMessageResult = dontShowMessage

        self.set_ready()

    def set_state(self, state):
        self.state = state

    # def is_running(self):
    #     # disable_states = [DoubleZoomWidgetState.DISABLED,
    #     #                   DoubleZoomWidgetState.PAUSED]
    #     # is_disabled = any(disable_state in self.state
    #     #     for disable_state in disable_states)
    #     # return not is_disabled
    #     return DoubleZoomWidgetState.RUNNING in self.state

    def set_running(self):
        self.doubleZoomCheckbox.setChecked(True)
        if self.doubleZoomAxes is None:
            self.doubleZoomAxes = self.imageFigure.add_subplot(333)
            self.doubleZoomAxes.set_aspect('equal', adjustable="box")
            self.doubleZoomAxes.axes.xaxis.set_visible(False)
            self.doubleZoomAxes.axes.yaxis.set_visible(False)

        img = self.parent.quadFold.orig_img
        center = self.parent.quadFold.info['center']
        x,y = center
        x, y = int(x), int(y)
        imgCropped = img[y - 10:y + 10, x - 10:x + 10]
        if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
            self.mainImagePoint = (x, y)

            imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
            self.doubleZoomAxes.imshow(imgScaled)
            self.doubleZoomAxes.invert_yaxis()

            self.imageCanvas.draw_idle()

        super().set_running()

    def set_ready(self):
        self.doubleZoomCheckbox.setChecked(False)

        self.remove_image_lines(labels=["DoubleZoom Blue Dot"])

        if self.doubleZoomAxes is not None:
            self.imageFigure.delaxes(self.doubleZoomAxes)
            self.doubleZoomAxes = None
        self.imageCanvas.draw_idle()

        super().set_ready()


    def handleDoubleZoomCheckedEvent(self, doubleZoomCheckboxState):
        if self.parent is None or self.parent.quadFold is None or self.parent.quadFold.orig_img is None:
            return

        if doubleZoomCheckboxState == Qt.CheckState.Checked:
            self.set_running()
        elif doubleZoomCheckboxState == Qt.CheckState.Unchecked:
            self.set_ready()

    def handle_mouse_button_press_event(self, mouse_event):
        pass

    def handle_mouse_move_event(self, mouse_event):
        if not self.is_running():
            return

        if self.parent.quadFold is None or self.parent.quadFold.orig_img is None:
            return

        x = mouse_event.xdata
        y = mouse_event.ydata

        img = self.parent.quadFold.orig_img

        if mouse_event.inaxes == self.imageAxes:
            # Draw cursor location in image using blue dot.
            self.drawBlueDot(x, y, self.imageAxes)

            # Do nothing if image has been clicked.
            if DoubleZoomWidgetState.MainImageClicked in self.state:
                self.state = DoubleZoomWidgetState.MainImageMouseMoving
                self.imageCanvas.draw_idle()
                return

            # Update double zoom image.
            self.drawDoubleZoomImage(x, y, img)
            self.imageCanvas.draw_idle()

        elif mouse_event.inaxes == self.doubleZoomAxes:
            # Draw cursor location in zoom using red cross lines.
            self.drawRedDot(x, y, self.doubleZoomAxes)

            self.imageCanvas.draw_idle()

            # Do nothing if image has been clicked.
            if DoubleZoomWidgetState.DoubleZoomImageClicked in self.state:
                self.state = DoubleZoomWidgetState.DoubleZoomImageMouseMoving
                return

    def handle_mouse_button_release_event(self, mouse_event):
        if not self.is_running():
            return

        x = mouse_event.xdata
        y = mouse_event.ydata

        # print("=" * 40)
        # print(f"position in image:{x} {y}")
        # print("^" * 40)

        if mouse_event.inaxes == self.imageAxes:
            if not self.dontShowAgainDoubleZoomMessageResult:
                self.showPopup()

            self.mainImagePoint = (x, y)

            # If image has been clicked, then it is clicked multiple times.
            if DoubleZoomWidgetState.MainImageClicked in self.state:
                new_state = self.state | DoubleZoomWidgetState.MainImageMultipleClicked
            else:
                new_state = DoubleZoomWidgetState.MainImageClicked

            self.set_state(new_state)

        elif mouse_event.inaxes == self.doubleZoomAxes:
            self.doubleZoomPoint = (x, y)

            # If image has been clicked, then it is clicked multiple times.
            if DoubleZoomWidgetState.DoubleZoomImageClicked in self.state:
                new_state = self.state | DoubleZoomWidgetState.DoubleZoomImageMultipleClicked
            else:
                new_state = DoubleZoomWidgetState.DoubleZoomImageClicked

            self.set_state(new_state)

    def handle_mouse_scroll_event(self, mouse_event):
        pass

    def is_no_action_state(self, mouse_event):
        no_action_states = {
            "main_image": [
                DoubleZoomWidgetState.MainImageClicked | DoubleZoomWidgetState.MainImageMouseMoving,
            ],
            "double_zoom_image": [
                DoubleZoomWidgetState.DoubleZoomImageClicked | DoubleZoomWidgetState.DoubleZoomImageMouseMoving,
            ]
        }

        if mouse_event.inaxes == self.imageAxes:
            no_action = any(no_action_state in self.state
                for no_action_state in no_action_states["main_image"])
            return no_action
        elif mouse_event.inaxes == self.doubleZoomAxes:
            no_action = any(no_action_state in self.state
                for no_action_state in no_action_states["double_zoom_image"])
            return no_action

        return False

    def showPopup(self):
        msg = QMessageBox()
        msg.setInformativeText(
            "Please click on zoomed window on the top right")
        dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")
        msg.setStandardButtons(QMessageBox.Ok)
        msg.setWindowTitle("Double Zoom Guide")
        msg.setStyleSheet("QLabel{min-width: 500px;}")
        msg.setCheckBox(dontShowAgainDoubleZoomMessage)
        msg.exec()
        self.dontShowAgainDoubleZoomMessageResult = dontShowAgainDoubleZoomMessage.isChecked()

    def doubleZoomToOrigCoord(self):
        """
        Compute the new x and y for double zoom to orig coord
        """
        dzx, dzy = self.mainImagePoint
        x, y = self.doubleZoomPoint
        newX = dzx -10 + x / 10
        newY = dzy - 10 + y / 10

        # print("=" * 40)
        # print(f"position in zoom:{x} {y}, in image: {newX} {newY}")
        # print("^" * 40)
        # return (newX, newY)

    def drawBlueDot(self, x, y, ax):
        self.remove_image_lines(ax=ax, labels=["DoubleZoom Blue Dot"])

        # Plot a blue dot at the given coordinates
        ax.plot(x, y, 'bo', markersize=2, label="DoubleZoom Blue Dot")

    def drawRedDot(self, x, y, ax):
            axis_size = 1

            x_min, x_max = ax.get_xlim()
            y_min, y_max = ax.get_ylim()

            # Clamp values so they stay inside axis bounds
            x1 = max(x_min, min(x - axis_size, x_max))
            x2 = max(x_min, min(x + axis_size, x_max))
            y1 = max(y_min, min(y - axis_size, y_max))
            y2 = max(y_min, min(y + axis_size, y_max))

            self.remove_image_lines(ax=ax, labels=["DoubleZoom Red Dot"])

            ax.plot((x1, x2), (y1, y2), color='r', label="DoubleZoom Red Dot")
            ax.plot((x1, x2), (y2, y1), color='r', label="DoubleZoom Red Dot")

    def drawDoubleZoomImage(self, x, y, img):
        if x > 10 and x<img.shape[1]-10 and y>10 and y<img.shape[0]-10:
            ax = self.doubleZoomAxes
            imgCropped = img[int(y - 10):int(y + 10), int(x - 10):int(x + 10)]
            if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                self.doubleZoomAxes.imshow(imgScaled)
                self.doubleZoomAxes.invert_yaxis()

                if len(ax.lines) > 0:
                    for i in range(len(ax.lines)-1,-1,-1):
                        if ax.lines[i].get_label() == "Red Dot":
                            ax.lines[i].remove()

def main():
    app = QApplication(sys.argv)
    widget = DoubleZoomWidget(None, None)
    widget.show()
    app.exec()


if __name__ == '__main__':
    main()
