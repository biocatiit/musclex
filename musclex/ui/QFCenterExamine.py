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

import os
import json
import copy
import pandas as pd
import matplotlib.pyplot as plt
from musclex import __version__
from .pyqt_utils import *
from ..utils.file_manager import *
from matplotlib.colors import Normalize
import cv2

def displayImgToAxes(img, ax, min_inten, max_inten):
    ax.cla()
    ax.imshow(img, cmap='gray', norm=Normalize(vmin=min_inten, vmax=max_inten))

class QFCenterExamine(QMainWindow):
    
    def __init__(self):
        print("Checkpoint 2")

        super().__init__()
        
        self.dir_path = ""
        self.file_name = ""

        self.orig_img = None

        self.orig_center = None
        self.center = None

        self.rot_angle = 0

        self.cent_img = None
        self.rot_img = None
        self.resized_img = None

        self.initUI()
        #self.setConnections()
        self.browseFile()

    def initUI(self):
        print("Checkpoint 3")
        """
        Initialize the UI
        """
        self.setWindowTitle("QF Center Examination")
        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)

        self.centralWidget = QWidget(self)
        self.scrollArea.setWidget(self.centralWidget)

        
        self.mainHLayout = QHBoxLayout(self.centralWidget)
        self.setCentralWidget(self.scrollArea)

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 200px; }")
        self.mainHLayout.addWidget(self.tabWidget)

        self.originalImageTab = QWidget()
        self.tabWidget.addTab(self.originalImageTab, "Original Image")

        self.origImageTabLayout = QHBoxLayout(self.originalImageTab)
        self.origFigure = plt.figure()
        self.origAxes = self.origFigure.add_subplot(111)
        self.origCanvas = FigureCanvas(self.origFigure)
        self.origImageTabLayout.addWidget(self.origCanvas)

        self.origControlGrp = QGroupBox("Original Image Controls")
        self.origControlLayout = QGridLayout(self.origControlGrp)
        self.origImageTabLayout.addWidget(self.origControlGrp)

        self.minIntOrig = QDoubleSpinBox()
        self.minIntOrig.setSingleStep(5)
        self.minIntOrig.setDecimals(0)
        self.minIntOrig.setMinimum(0)
        self.minIntOrig.setMaximum(9999999999999999999999999999)
        self.origControlLayout.addWidget(QLabel("Min Intensity:"), 0, 0, 1, 2)
        self.origControlLayout.addWidget(self.minIntOrig, 0, 3, 1, 2)
        self.minIntOrig.valueChanged.connect(self.refreshOrigImg)

        self.maxIntOrig = QDoubleSpinBox()
        self.maxIntOrig.setSingleStep(5)
        self.maxIntOrig.setDecimals(0)
        self.maxIntOrig.setMinimum(0)
        self.maxIntOrig.setMaximum(9999999999999999999999999999)
        self.origControlLayout.addWidget(QLabel("Max Intensity:"), 1, 0, 1, 2)
        self.origControlLayout.addWidget(self.maxIntOrig, 1, 3, 1, 2)
        self.maxIntOrig.valueChanged.connect(self.refreshOrigImg)

        self.origCenterXBox = QDoubleSpinBox()
        self.origCenterXBox.setMinimum(0)
        self.origCenterXBox.setMaximum(9999999999999999999999999999)
        self.origControlLayout.addWidget(QLabel("Center X"), 2, 0, 1, 2)
        self.origControlLayout.addWidget(self.origCenterXBox, 2, 3, 1, 2)
        self.origCenterXBox.valueChanged.connect(self.refreshOrigImg)

        self.origCenterYBox = QDoubleSpinBox()
        self.origCenterYBox.setMinimum(0)
        self.origCenterYBox.setMaximum(9999999999999999999999999999)
        self.origControlLayout.addWidget(QLabel("Center Y"), 3, 0, 1, 2)
        self.origControlLayout.addWidget(self.origCenterYBox, 3, 3, 1, 2)
        self.origCenterYBox.valueChanged.connect(self.refreshOrigImg)

        self.centerizeButton = QPushButton('Centerize')
        self.origControlLayout.addWidget(self.centerizeButton)
        self.centerizeButton.clicked.connect(self.centerize)

        self.origFigure.canvas.mpl_connect('button_press_event', self.imageClicked)

        self.centImageTab = QWidget()
        self.tabWidget.addTab(self.centImageTab, "Centerized Image")

        self.centImageTabLayout = QHBoxLayout(self.centImageTab)
        self.centFigure = plt.figure()
        self.centAxes = self.centFigure.add_subplot(111)
        self.centCanvas = FigureCanvas(self.centFigure)
        self.centImageTabLayout.addWidget(self.centCanvas)

        self.centControlGrp = QGroupBox("Centerized Image Controls")
        self.centControlLayout = QGridLayout(self.centControlGrp)
        self.centImageTabLayout.addWidget(self.centControlGrp)

        self.rotAngSpBx = QDoubleSpinBox()
        self.rotAngSpBx.setSingleStep(5)
        self.rotAngSpBx.setDecimals(0)
        self.rotAngSpBx.setMinimum(-9999999999999999999999999999)
        self.minIntOrig.setMaximum(9999999999999999999999999999)
        self.centControlLayout.addWidget(QLabel("Rotation Angle:"), 0, 0, 1, 2)
        self.centControlLayout.addWidget(self.rotAngSpBx, 0, 3, 1, 2)
        self.rotAngSpBx.valueChanged.connect(self.refreshCentImg)

        self.rotateButton = QPushButton('Rotate Image')
        self.centControlLayout.addWidget(self.rotateButton)
        self.rotateButton.clicked.connect(self.rotateImage)

        self.rotatedImageTab = QWidget()
        self.tabWidget.addTab(self.rotatedImageTab, "Rotated Image")

        self.rotImageTabLayout = QHBoxLayout(self.rotatedImageTab)
        self.rotFigure = plt.figure()
        self.rotAxes = self.rotFigure.add_subplot(111)
        self.rotCanvas = FigureCanvas(self.rotFigure)
        self.rotImageTabLayout.addWidget(self.rotCanvas)

        self.resizedImageTab = QWidget()
        self.tabWidget.addTab(self.resizedImageTab, "Resized Image")

        self.resizImageTabLayout = QHBoxLayout(self.resizedImageTab)
        self.resizFigure = plt.figure()
        self.resizAxes = self.resizFigure.add_subplot(111)
        self.resizCanvas = FigureCanvas(self.resizFigure)
        self.resizImageTabLayout.addWidget(self.resizCanvas)

        self.show()
        self.resize(1200, 900)

    def setConnections(self):
        self.minInt.valueChanged.connect(self.refreshImage)
        self.maxInt.valueChanged.connect(self.refreshImage)

        self.nextFileButton.clicked.connect(self.nextFBClicked)
        self.prevFileButton.clicked.connect(self.prevFBClicked)

        self.processFolderButton.clicked.connect(self.makeCSV)

    def browseFile(self):
        """
        Popup input dialog and set file selection
        """
        self.newProcess = True
        file_name = getAFile()
        if file_name != "":
            self.file_name = file_name
            self.onNewFileSelected(str(file_name))
            self.centralWidget.setMinimumSize(700, 500)

    def onNewFileSelected(self, file_name):
        self.dir_path, self.imgList, self.currentFileNumber, self.fileList, self.ext = getImgFiles(str(file_name))

        self.orig_img = fabio.open(str(file_name)).data
        
        self.minIntOrig.setValue(0)
        max_inten = min(700, np.max(self.orig_img) / 40)
        self.maxIntOrig.setValue(max_inten)

        shape = self.orig_img.shape
        center = [shape[0] / 2, shape[1] / 2]
        self.origCenterXBox.setValue(center[0])
        self.origCenterYBox.setValue(center[1])
        self.orig_center = center

        self.refreshOrigImg()

    def refreshOrigImg(self):
        displayImgToAxes(self.orig_img, self.origAxes, self.minIntOrig.value(), self.maxIntOrig.value())
        self.origAxes.axvline(self.origCenterXBox.value(), color='y')
        self.origAxes.axhline(self.origCenterYBox.value(), color='y')
        self.origCanvas.draw_idle()

    def centerize(self):
        self.center = [self.origCenterXBox.value(), self.origCenterYBox.value()]
        
        print("First we set the center data attribute to be the values in the spin boxes: ", self.center)

        diff_x = abs(self.orig_center[0] - self.center[0])
        diff_y = abs(self.orig_center[1] - self.center[1])

        print("Then we figure out the difference between the center and the middle of the image in the x direction: ", diff_x)
        print("And the y direction: ", diff_y)

        orig_x, orig_y = self.orig_img.shape

        new_x, new_y = orig_x + abs(diff_x), orig_y + abs(diff_y)

        print("We create dimmensions for the centerized image that are equal to the original image directions + the differences between centers.  X: ", new_x)
        print("Y: ", new_y)

        trans_x = new_x/2 - self.center[0]
        print("We figure out how far the new image needs to be translated in the x direction to get the center to the middle: ", trans_x)
        if trans_x < 0:
            new_x += abs(trans_x)* 2
            print("Since trans_x is negative, we add 2 * trans_x: ", 2 * trans_x, " to the x dimmension of the new image to make it: ", new_x, " and set trans_x to 0.  This simulates the negative translation and lets us keep the image coordinates as only ints greater than 0.")
            trans_x = 0

        trans_y = new_y/2 - self.center[1]
        print("We figure out how far the new image needs to be translated in the y direction to get the center to the middle: ", trans_y)
        if trans_y < 0:
            new_y += abs(trans_y) * 2
            print("Since trans_y is negative, we add 2 * trans_y: ", 2 * trans_y, " to the y dimmension of the new image to make it: ", new_y, " and set trans_y to 0.  This simulates the negative translation and lets us keep the image coordinates as only ints greater than 0.")
            trans_y = 0


        dim = max(new_x, new_y)
        print("We make a variable called dim and set it to the max of the dim.s of the new image: ", dim)

        if new_x < dim:
            print("the x side was smaller than dim, so we'll add half of the difference to the x translation: ", (dim - new_x) / 2, " to make sure the image stays in the center of the x axis")
            trans_x += (dim - new_x) / 2
            print("Trans_x is now: ", trans_x)

        if new_y < dim:
            print("the y side was smaller than dim, so we'll add half of the difference to the y translation: ", (dim - new_y) / 2, " to make sure the image stays in the center of the y axis")
            trans_y += (dim - new_y) / 2
            print("Trans_y is now: ", trans_y)

        trans_mat = np.array([[1, 0, trans_x],
                             [0, 1, trans_y]], dtype=np.float32)

        new_img = np.zeros((int(dim), int(dim))).astype("float32")
        new_img[0:orig_x, 0:orig_y] = self.orig_img
        ##ADD CONDITIONS TO PAD THE IMAGE IF EITHER OF THE TRANSFORMATIONS ARE NEG- THAT WAY THE WHOLE THING FITS IN
        
        print("TRANS MAT: ", trans_mat)
        print("ORIGIMGSHAPE: ", self.orig_img.shape)

        print("Trans x: ", trans_x)
        print("Trans y: ", trans_y)

        self.cent_img = cv2.warpAffine(new_img, trans_mat, (int(dim), int(dim)))

        print("ORIGINAL IMAGE COORDINATES: ", self.orig_img.shape)
        print("ORIGINAL IMAGE CENTER: ", self.center)

        print("NEW IMAGE SHAPE: ", new_img.shape)
        print("NEW IMAGE CENTER SHOULD BE: ", dim/2, ", ",dim/2 )

        displayImgToAxes(self.cent_img, self.centAxes, self.minIntOrig.value(), self.maxIntOrig.value())

        self.centAxes.axvline(dim/2, color='y')
        self.centAxes.axhline(dim/2, color='y')

        self.centCanvas.draw_idle()

    def refreshCentImg(self):
        ax = self.centAxes

        self.rot_angle = self.rotAngSpBx.value()
        theta = np.deg2rad(self.rot_angle)
        print("ROTATION ANGLE: ", theta)

        displayImgToAxes(self.cent_img, self.centAxes, self.minIntOrig.value(), self.maxIntOrig.value())

        #Yellow Center lines
        self.centAxes.axvline(self.cent_img.shape[0]/2, color='y')
        self.centAxes.axhline(self.cent_img.shape[1]/2, color='y')

        #Blue Rotation Lines
        cx = self.cent_img.shape[0]/2
        cy = self.cent_img.shape[1]/2

        ax.axline((cx, cy), (cx + np.cos(theta), cy + np.sin(theta)), color='blue')
        print(f"LINE 1 POINTS: [({cx}, {cy}), ({cx + np.cos(theta)}, {cy + np.sin(theta)})]")
        #ax.axline((cx, cy), (cx - np.sin(theta), cy + np.cos(theta)), color='blue')
        #print(f"LINE 2 POINTS: [({cx}, {cy}), ({cx-np.sin(theta)}, {cy + np.cos(theta)})]")

        self.centCanvas.draw_idle()


    def imageClicked(self, event):
        """
        Triggered when mouse presses on image in original image tab
        """
        x = event.xdata
        y = event.ydata

        self.origCenterXBox.setValue(x)
        self.origCenterYBox.setValue(y)

        self.refreshOrigImg()

    def rotateImage(self):
        theta = self.rot_angle
        center = [self.cent_img.shape[0]/2, self.cent_img.shape[1]/2]

        #Image rotation Matrix
        M = cv2.getRotationMatrix2D(center, theta, 1.0)

        #Corners rotation matrix(for final image size determination)
        theta_r = np.deg2rad(theta) #Rot angle in radians
        R = np.array([[np.cos(theta_r), -np.sin(theta_r)],
                [np.sin(theta_r),  np.cos(theta_r)]])
        
        corners = [
            [-center[0], -center[1]],
            [-center[0], center[1]],
            [center[0], center[1]],
            [center[0], -center[1]]
        ]
        print("CORNERS: ", corners)

        diff_x = self.orig_center[0] - self.center[0]
        diff_y = self.orig_center[1] - self.center[1]

        shifted = corners - np.array([diff_x, diff_y])

        # Apply the rotation to the shifted corners
        rotated_shifted = np.dot(shifted, R.T)
        dim = max([abs(i) for i in np.ravel(rotated_shifted)])

        print("Rotation Matrix: ", M)

        self.rot_img = cv2.warpAffine(self.cent_img, M, (dim, dim))

        displayImgToAxes(self.rot_img, self.rotAxes, self.minIntOrig.value(), self.maxIntOrig.value())
        self.rotCanvas.draw_idle()