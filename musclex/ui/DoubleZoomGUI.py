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

import numpy as np
from PySide6.QtWidgets import QMessageBox, QCheckBox
import cv2
from musclex import __version__

class DoubleZoom:

    def __init__(self, img_fig):
        self.axes = None
        self.doubleZoomEnabled = False
        self.dontShowAgainDoubleZoomMessageResult = False
        self.mousePosHist = []
        self.doubleZoomPoint = (0, 0)
        self.mouseSensitivity = 1.0
        self.doubleZoomMode = False
        self.image_figure = img_fig

    def onClicked(self, x, y):
        if not self.dontShowAgainDoubleZoomMessageResult:
            self.showPopup() 

    def showPopup(self):
        msg = QMessageBox()
        msg.setInformativeText(
            "Please click on zoomed window on the top right")
        dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")
        msg.setStandardButtons(QMessageBox.Ok)
        msg.setWindowTitle("Double Zoom Guide")
        msg.setStyleSheet("QLabel{min-width: 500px;}")
        msg.setCheckBox(dontShowAgainDoubleZoomMessage)
        msg.exec_()
        self.dontShowAgainDoubleZoomMessageResult = dontShowAgainDoubleZoomMessage.isChecked()

    def showPopupConditional(self):
        if not self.dontShowAgainDoubleZoomMessageResult:
            self.showPopup()

    def doubleZoomToOrigCoord(self, x, y):
        print("DOUBLE ZOOM TO ORIGINAL COORD") #NICKA DEBUG
        """
        Compute the new x and y for double zoom to orig coord
        """
        M = [[1/10, 0, 0], [0, 1/10, 0],[0, 0, 1]]
        dzx, dzy = self.doubleZoomPoint
        x, y, _ = np.dot(M, [x, y, 1])
        newX = dzx -10 + x
        newY = dzy - 10 + y
        return (newX, newY)

    def doubleZoomChecked(self, img, canv, center = (0, 0), is_checked=False):
        """
        Triggered when double zoom is checked
        """
        print("DOUBLE ZOOM CHECKED FUNCTION") #NICKA DEBUG
        print("IMG: ", img.shape) #NICKA DEBUG
        print("CANV: ", canv) #NICKA DEBUG
        print("CENTER: ",center) #NICKA DEBUG
        print("IS_CHECKED: ", is_checked) #NICKA DEBUG

        if img is not None and canv is not None:
            if is_checked:

                self.mousePosHist = []  # Reset mouse tracking

                print("Double zoom checked")
                self.axes = self.image_figure.add_subplot(333)
                self.axes.axes.xaxis.set_visible(False)
                self.axes.axes.yaxis.set_visible(False)
                self.doubleZoomMode = True

                ax1 = self.axes
                x,y = center
                x, y = int(x), int(y)
                print("(FROM DOUBLEZOOM CHEKCED)Center is ", x, y) #NICKA DEBUG
                imgCropped = img[y - 10:y + 10, x - 10:x + 10]
                if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                    imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                    self.doubleZoomPoint = (x, y)
                    ax1.imshow(imgScaled)
                    ax1.invert_yaxis()
                    y, x = imgScaled.shape
                    # cy, cx = y // 2, x // 2
                    if len(ax1.lines) > 0:
                        for i in range(len(ax1.lines)-1,-1,-1):
                            ax1.lines[i].remove()
                    for i in range(len(ax1.patches)-1,-1,-1):
                        ax1.patches[i].remove()
            else:

                self.image_figure.delaxes(self.axes)
                self.doubleZoomMode = False
            canv.draw_idle()

    def setAxes(self, imageFigure):
        self.axes = imageFigure.add_subplot(333)

    def showAxes(self):
        pass

    def updateMousePosHist(self, coords):
        "Updated the list that stores the last 5 mouse positions"

        if len(self.mousePosHist) > 3:
            self.mousePosHist.pop(0)
        self.mousePosHist.append(coords)

    def subplotClicked(self):
        pass

    def drawBlueDot(self, x, y, ax):
        # Remove any existing lines or patches so they don't stack
        if len(ax.lines) > 0:
            for i in range(len(ax.lines)-1, -1, -1):
                if ax.lines[i].get_label() == "Blue Dot":
                    ax.lines[i].remove()
        if len(ax.patches) > 0:
            for i in range(len(ax.patches)-1, -1, -1):
                ax.patches[i].remove()

        # Plot a blue dot at the given coordinates
        ax.plot(x, y, 'bo', markersize=2, label="Blue Dot")

    def adjustXY(self, x, y):
        # Calculate deltas with sensitivity
        deltaX = (x - self.mousePosHist[-1][0]) * self.mouseSensitivity
        deltaY = (y - self.mousePosHist[-1][1]) * self.mouseSensitivity

        # Adjust x and y based on sensitivity
        x = int(round(self.mousePosHist[-1][0] + deltaX))
        y = int(round(self.mousePosHist[-1][1] + deltaY))

        return x, y

    def updateMousePosHist(self, coords):
        "Updated the list that stores the last 5 mouse positions"

        if len(self.mousePosHist) > 3:
            self.mousePosHist.pop(0)
        self.mousePosHist.append(coords)

    def calcMouseMovement(self):
        "Determines relatively how fast the mouse is moving around"

        mph = self.mousePosHist
        if len(mph) < 2:
            return 0

        diffs = len(self.mousePosHist) - 1
        total = 0
        for i in range(diffs):
            total += np.sqrt(((mph[i][0] - mph[i+1][0]) ** 2) + ((mph[i][1] - mph[i+1][1]) ** 2))
            
        return total / diffs

    def updateMouseSensitivity(self):
        "Updates the mouse sensitivity based on the speed of the mouse movements."
        movement = self.calcMouseMovement()

        if movement < 2:
            self.mouseSensitivity = 0.07
        elif movement < 82:
            self.mouseSensitivity = ((movement - 2) / 80) + 0.07
        else:
            self.mouseSensitivity = 1.07

    def updateAndDraw(self, x, y, ax):
        self.updateMousePosHist((x, y))
        self.updateMouseSensitivity()
        self.drawBlueDot(x, y, ax)

    def mouseClickBehavior(self, x, y):
        # If x, y is inside figure and image is clicked for first time in double zoom mode
        print(x, y)
        self.showPopupConditional()
        self.doubleZoomMode = False

    def mouseHoverBehavior(self, x, y, img, canv, is_checked):
        if is_checked and self.doubleZoomMode and x>10 and x<img.shape[1]-10 and y>10 and y<img.shape[0]-10:
            ax1 = self.axes
            imgCropped = img[int(y - 10):int(y + 10), int(x - 10):int(x + 10)]
            if len(imgCropped) != 0 or imgCropped.shape[0] != 0 or imgCropped.shape[1] != 0:
                imgScaled = cv2.resize(imgCropped.astype("float32"), (0, 0), fx=10, fy=10)
                self.doubleZoomPoint = (x,y)
                ax1.imshow(imgScaled)
                ax1.invert_yaxis()
                if len(ax1.lines) > 0:
                    for i in range(len(ax1.lines)-1,-1,-1):
                        ax1.lines[i].remove()
                for i in range(len(ax1.patches)-1,-1,-1):
                    ax1.patches[i].remove()
                canv.draw_idle()

    def updateAxesInner(self, x, y):
            axis_size = 1
            ax1 = self.axes
            if len(ax1.lines) > 0:
                for i in range(len(ax1.lines)-1,-1,-1):
                    ax1.lines[i].remove()
            ax1.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            ax1.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

    def updateAxes(self, x, y):
        if (not self.doubleZoomMode) and x < 200 and y < 200:
            self.updateAxesInner(x, y)

    def beginImgMotion(self, x, y, img_width, img_height, extent, img_axes):
        if self.mousePosHist == []:
            #print("MOUSE POS HIST IS EMPTY") #NICKA DEBUG
            self.mousePosHist.append((x, y))
            return
        
        x, y = self.adjustXY(x, y)

        #Upper bound on x and y vals to avoid errors
        if x + extent[0] > img_width:
            x = img_width - extent[0] - 1
        if y + extent[1] > img_height:
            y = img_height - extent[1] - 1

        #Lower bound on x and y to avoid errors
        """
        if x < 0:
            x = 0
        if y < 0:
            y = 0
        """

        # Update last mouse position, recalculate sensitivity and draw dot
        self.updateAndDraw(x, y, img_axes)