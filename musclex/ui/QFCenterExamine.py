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
import math
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

        self.master_min_int = None
        self.master_max_int = None
        self.master_center = None
        self.master_rot_angle = None

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
        self.origFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)


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

        self.centFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)

        self.rotatedImageTab = QWidget()
        self.tabWidget.addTab(self.rotatedImageTab, "Rotated Image")

        self.rotImageTabLayout = QHBoxLayout(self.rotatedImageTab)
        self.rotFigure = plt.figure()
        self.rotAxes = self.rotFigure.add_subplot(111)
        self.rotCanvas = FigureCanvas(self.rotFigure)
        self.rotImageTabLayout.addWidget(self.rotCanvas)

        self.rotFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)

        self.mastImageTab = QWidget()
        self.tabWidget.addTab(self.mastImageTab, "Master Image")

        self.mastImageTabLayout = QHBoxLayout(self.mastImageTab)
        self.mastFigure = plt.figure()
        self.mastAxes = self.mastFigure.add_subplot(111)
        self.mastCanvas = FigureCanvas(self.mastFigure)
        self.mastImageTabLayout.addWidget(self.mastCanvas)

        self.mastControlGrp = QGroupBox("Master Image Controls")
        self.mastControlLayout = QGridLayout(self.mastControlGrp)
        self.mastImageTabLayout.addWidget(self.mastControlGrp)

        self.mastMinInt = QDoubleSpinBox()
        self.mastMinInt.setSingleStep(5)
        self.mastMinInt.setDecimals(0)
        self.mastMinInt.setMinimum(0)  # Allow 0 for min intensity
        self.mastMinInt.setMaximum(9999999999999999999999999999)  # Allow large values for min intensity
        self.mastControlLayout.addWidget(QLabel("Min Intensity:"), 0, 0, 1, 2)
        self.mastControlLayout.addWidget(self.mastMinInt, 0, 3, 1, 2)
        self.mastMinInt.valueChanged.connect(self.mastMinIntChanged)
        self.clearMastMinIntBtn = QPushButton('Clear')
        self.mastControlLayout.addWidget(self.clearMastMinIntBtn, 0, 5, 1, 2)
        self.clearMastMinIntBtn.clicked.connect(self.clearMastMinInt)

        self.mastMaxInt = QDoubleSpinBox()
        self.mastMaxInt.setSingleStep(5)  # Allow larger steps for quicker testing
        self.mastMaxInt.setDecimals(0)  # Set decimals to 0 for simplicity in testing
        self.mastMaxInt.setMinimum(0)  # Allow 0 for max intensity
        self.mastMaxInt.setMaximum(9999999999999999999999999999)  # Allow large values for max intensity
        self.mastControlLayout.addWidget(QLabel("Max Intensity:"), 1, 0, 1, 2)
        self.mastControlLayout.addWidget(self.mastMaxInt, 1, 3, 1, 2)
        self.mastMaxInt.valueChanged.connect(self.mastMaxIntChanged)
        self.clearMastMaxIntBtn = QPushButton('Clear')
        self.mastControlLayout.addWidget(self.clearMastMaxIntBtn, 1, 5, 1, 2)
        self.clearMastMaxIntBtn.clicked.connect(self.clearMastMaxInt)

        self.mastCenterXBox = QDoubleSpinBox()
        self.mastCenterXBox.setMinimum(0)  # Allow 0 for center
        self.mastCenterXBox.setMaximum(9999999999999999999999999999)  # Allow large values for center
        self.mastCenterXBox.setSingleStep(1)  # Set single step to 1 for easier testing
        self.mastControlLayout.addWidget(QLabel("Center X"), 2, 0, 1, 2)
        self.mastControlLayout.addWidget(self.mastCenterXBox, 2, 3, 1, 2)
        self.mastCenterXBox.valueChanged.connect(self.mastCenterChanged)
        self.clearMastCenterBtn = QPushButton('Clear')
        self.mastControlLayout.addWidget(self.clearMastCenterBtn, 2, 5, 1, 2)
        self.clearMastCenterBtn.clicked.connect(self.clearMastCenter)

        self.mastCenterYBox = QDoubleSpinBox()
        self.mastCenterYBox.setMinimum(0)
        self.mastCenterYBox.setMaximum(9999999999999999999999999999)  # Allow large values for center
        self.mastCenterYBox.setSingleStep(1)  # Set single step to 1 for easier testing
        self.mastControlLayout.addWidget(QLabel("Center Y"), 3, 0, 1, 2)
        self.mastControlLayout.addWidget(self.mastCenterYBox, 3, 3, 1, 2)
        self.mastCenterYBox.valueChanged.connect(self.mastCenterChanged)
        
        self.mastRotationAngle = QDoubleSpinBox()
        self.mastControlLayout.addWidget(QLabel("Rotation Angle:"), 4, 0, 1, 2)
        self.mastControlLayout.addWidget(self.mastRotationAngle, 4, 3, 1, 2)
        self.mastRotationAngle.valueChanged.connect(self.mastRotAngleChanged)
        self.clearMastRotAngleBtn = QPushButton('Clear')
        self.mastControlLayout.addWidget(self.clearMastRotAngleBtn, 4, 5, 1, 2)
        self.clearMastRotAngleBtn.clicked.connect(self.clearMastRotAngle)

        self.mastRecalc = QPushButton('Recalculate')
        self.mastControlLayout.addWidget(self.mastRecalc)
        self.mastRecalc.clicked.connect(self.recalculate)

        self.mastFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)

        #STATUS BAR
        self.statusBar = QStatusBar()
        self.imgCoordOnStatusBar = QLabel()
        self.statusBar.addPermanentWidget(self.imgCoordOnStatusBar)
        self.setStatusBar(self.statusBar)

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

        self.master_center = [dim/2, dim/2]
        self.mastCenterXBox.setValue(dim/2)
        self.mastCenterYBox.setValue(dim/2)
        self.recalculate()

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


    def recalculate(self):
        print("RECALCULATE FUNCITON")
        min_int = self.minIntOrig.value() if self.master_min_int is None else self.master_min_int
        max_int = self.maxIntOrig.value() if self.master_max_int is None else self.master_max_int


        center = self.center if self.master_center is None else self.master_center
        rot_angle = self.rot_angle if self.master_rot_angle is None else self.master_rot_angle

        """ print("We start with center = ", center)
        print("and rot_angle = ", rot_angle)

        R_w = cv2.getRotationMatrix2D((center[1], center[0]), -rot_angle, 1)

        R_2x2 = np.array([[np.cos(-rot_angle), -np.sin(-rot_angle)],
                [np.sin(-rot_angle),  np.cos(-rot_angle)]])
        

        R = np.zeros((2, 3))
        R[:, :-1] = R_2x2

        print("The inverse rotation matrix is: ", R)
        print("The weird shit from getROtaitonMatrix2d (cv2) is: ", R_w)

        dx = self.center[0] - center[0]
        dy = self.center[1] - center[1]
        T = np.float32([[1, 0, dx], [0, 1, dy]])

        print("The inverse tranlation matrix is: ", T)

        M = R
        M[:, 2] = T[:, 2]

        print("The combined transformation matrix is: ", M)

        temp_c = [self.mastCenterXBox.value(), self.mastCenterXBox.value()]

        print("The center specified is: ", temp_c)

        #orig_center = np.dot(M, temp_c + [1])
        pt = np.dot(temp_c, R_w)
        print("pt: ", pt)
        orig_center = np.dot(pt[:-1], T)

        print("The specified center converted to original image coordinates is: ", orig_center)"""

        dx = center[0] - self.center[0]
        dy = center[1] - self.center[1]

        print("Center: ", center)
        print("self.center: ", self.center)

        M = cv2.getRotationMatrix2D(center, -rot_angle, 1.0)

        print("M: ", M)

        M[:, 2] += np.array((dx, dy))

        print("M with trans: ", M)

        M_inv = cv2.invertAffineTransform(M)

        print("M_inv: ", M_inv)

        ui_x = self.mastCenterXBox.value()  # For example, x-coordinate from a widget
        ui_y = self.mastCenterYBox.value()  # For example, y-coordinate from a widget
        ui_point = np.array([ui_x, ui_y, 1])  # Homogeneous coordinate

        print("ui_point: ", ui_point)

        orig_center = np.dot(M_inv, ui_point)

        print("orig_center: ", orig_center)

        cent_img = self.recalculateCenterPart(orig_center)
        rot_img = self.recalculateRotationPart(orig_center, rot_angle, cent_img)

        displayImgToAxes(rot_img, self.mastAxes, min_int, max_int)

        self.mastAxes.axvline(center[0], color='y')  # center line for x
        self.mastAxes.axhline(center[1], color='y')  # center line for y

        self.mastCanvas.draw_idle()

    def recalculateCenterPart(self, center):
        diff_x = abs(self.orig_center[0] - center[0])
        diff_y = abs(self.orig_center[1] - center[1])

        orig_x, orig_y = self.orig_img.shape

        new_x, new_y = orig_x + abs(diff_x), orig_y + abs(diff_y)

        trans_x = new_x/2 - center[0]
        if trans_x < 0:
            new_x += abs(trans_x)* 2
            trans_x = 0

        trans_y = new_y/2 - center[1]
        if trans_y < 0:
            new_y += abs(trans_y) * 2
            trans_y = 0

        dim = max(new_x, new_y)

        if new_x < dim:
            trans_x += (dim - new_x) / 2

        if new_y < dim:
            trans_y += (dim - new_y) / 2

        trans_mat = np.array([[1, 0, trans_x],
                             [0, 1, trans_y]], dtype=np.float32)

        new_img = np.zeros((int(dim), int(dim))).astype("float32")
        new_img[0:orig_x, 0:orig_y] = self.orig_img
        ##ADD CONDITIONS TO PAD THE IMAGE IF EITHER OF THE TRANSFORMATIONS ARE NEG- THAT WAY THE WHOLE THING FITS IN

        cent_img = cv2.warpAffine(new_img, trans_mat, (int(dim), int(dim)))
        
        return cent_img


    def recalculateRotationPart(self, center, rot_angle, cent_img):
        theta = rot_angle
        center_1 = [cent_img.shape[0]/2, cent_img.shape[1]/2]

        #Corners rotation matrix(for final image size determination)
        theta_r = -np.deg2rad(theta) #Negative rot angle in radians
        R = np.array([[np.cos(theta_r), -np.sin(theta_r)],
                [np.sin(theta_r),  np.cos(theta_r)]])
        
        corners = [
            [-center[0], -center[1]],
            [-center[0], self.orig_img.shape[0] - center[1]],  # bottom left
            [self.orig_img.shape[1] - center[0], -center[1]],  # top right
            [self.orig_img.shape[1] - center[0], self.orig_img.shape[0] - center[1]],  # bottom right

        ]

        # Apply the rotation to the shifted corners
        rotated_shifted = np.dot(corners, R.T)

        cent = 2 * max([abs(i) for i in np.ravel(rotated_shifted)])
        dim = math.ceil(cent)

        #Image rotation Matrix
        M = cv2.getRotationMatrix2D((int(cent/2), int(cent/2)), theta, 1.0)

        new_img = np.zeros((dim, dim), dtype="float32")

        diff_x = int(dim / 2 - center_1[0])
        diff_y = int(dim / 2 - center_1[1])

        new_img[diff_x:diff_x+int(center_1[0]*2), diff_y:diff_y+int(center_1[1]*2)] = cent_img

        rot_img = cv2.warpAffine(new_img, M, (dim, dim))

        return rot_img

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

        #Corners rotation matrix(for final image size determination)
        theta_r = -np.deg2rad(theta) #Negative rot angle in radians
        R = np.array([[np.cos(theta_r), -np.sin(theta_r)],
                [np.sin(theta_r),  np.cos(theta_r)]])
        
        corners = [
            [-self.center[0], -self.center[1]],
            [-self.center[0], self.orig_img.shape[0] - self.center[1]],  # bottom left
            [self.orig_img.shape[1] - self.center[0], -self.center[1]],  # top right
            [self.orig_img.shape[1] - self.center[0], self.orig_img.shape[0] - self.center[1]],  # bottom right

        ]
        print("CORNERS: ", corners)

        # Apply the rotation to the shifted corners
        rotated_shifted = np.dot(corners, R.T)

        print("ROTATED SHIFTED CORNERS: tl:", rotated_shifted[0], " bl:", rotated_shifted[1],
              " tr:", rotated_shifted[2], " br:", rotated_shifted[3])

        cent = 2 * max([abs(i) for i in np.ravel(rotated_shifted)])
        dim = math.ceil(cent)

        #Image rotation Matrix
        M = cv2.getRotationMatrix2D((int(cent/2), int(cent/2)), theta, 1.0)

        new_img = np.zeros((dim, dim), dtype="float32")

        diff_x = int(dim / 2 - center[0])
        diff_y = int(dim / 2 - center[1])

        print("diff_x: ", diff_x)
        print("diff_y: ", diff_y)

        print("x_range: ", diff_x, ", ", dim-diff_x)
        print("y_range: ", diff_y, ", ", dim-diff_y)

        print("Cent_img shape: ", self.cent_img.shape)

        new_img[diff_x:diff_x+int(center[0]*2), diff_y:diff_y+int(center[1]*2)] = self.cent_img

        print("DIMENSION FOR ROTATED IMAGE: ", dim)

        print("Rotation Matrix: ", M)

        self.rot_img = cv2.warpAffine(new_img, M, (dim, dim))

        displayImgToAxes(self.rot_img, self.rotAxes, self.minIntOrig.value(), self.maxIntOrig.value())
        self.rotCanvas.draw_idle()

        self.rotAxes.axvline(dim/2, color='y')
        self.rotAxes.axhline(dim/2, color='y')

        """
        corners_draw = rotated_shifted + [dim/2, dim/2]

        self.rotAxes.plot(corners_draw[0][0], corners_draw[0][1], marker='o', markersize=4, color='r')
        self.rotAxes.plot(corners_draw[1][0], corners_draw[1][1], marker='o', markersize=4, color='r')
        self.rotAxes.plot(corners_draw[2][0], corners_draw[2][1], marker='o', markersize=4, color='r')
        self.rotAxes.plot(corners_draw[3][0], corners_draw[3][1], marker='o', markersize=4, color='r')
        """

        self.master_center = [dim/2, dim/2]
        self.mastCenterXBox.setValue(dim/2)
        self.mastCenterYBox.setValue(dim/2)

        self.master_rot_angle = self.rot_angle
        self.mastRotationAngle.setValue(self.rot_angle)
        self.recalculate()

    def imageOnMotion(self, event):
        x = event.xdata
        y = event.ydata

        if x is not None and y is not None:
            self.imgCoordOnStatusBar.setText(f"X: {x:.2f}, Y: {y:.2f}")

    def clearMastMinInt(self):
        self.master_min_int = None

    def mastMinIntChanged(self):
        self.master_min_int = self.mastMinInt.value()

    def clearMastMaxInt(self):
        self.master_max_int = None

    def mastMaxIntChanged(self):
        self.master_max_int = self.mastMaxInt.value()

    def clearMastCenter(self):
        self.master_center = None
    
    def mastCenterChanged(self):
        self.master_center = [self.mastCenterXBox.value(), self.mastCenterYBox.value()]

    def clearMastRotAngle(self):
        self.master_rot_angle = None

    def mastRotAngleChanged(self):
        self.master_rot_angle = self.mastRotationAngle.value()