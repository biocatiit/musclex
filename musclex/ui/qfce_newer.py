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
import numpy as np
import fabio


def displayImgToAxes(img, ax, min_inten, max_inten):
    """Clear axes and render grayscale image with intensity bounds."""
    ax.cla()
    ax.imshow(img, cmap='gray', norm=Normalize(vmin=min_inten, vmax=max_inten))
    ax.set_xticks([])
    ax.set_yticks([])


class QFCenterExamine(QMainWindow):
    """Tab-based GUI for stepping through image centering and rotation."""

    def __init__(self):
        super().__init__()
        self.orig_img = None             # original image array
        self.center = None               # [x, y] in orig coords
        self.rot_angle = 0.0             # radians

        self.cent_img = None             # centered image
        self.rot_img = None              # rotated image

        # overrides from Master tab
        self.master_min_int = None
        self.master_max_int = None
        self.master_center = None
        self.master_rot_angle = None

        self.initUI()
        self.browseFile()

    def initUI(self):
        self.setWindowTitle("QF Center Examination")
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        central = QWidget()
        scroll.setWidget(central)
        self.setCentralWidget(scroll)
        main_h = QHBoxLayout(central)

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setStyleSheet("QTabBar::tab{height:40px;width:200px}")
        main_h.addWidget(self.tabWidget)

        # Original tab
        self.originalImageTab = QWidget()
        self.tabWidget.addTab(self.originalImageTab, "Original Image")
        orig_lo = QHBoxLayout(self.originalImageTab)
        self.origFigure = plt.figure()
        self.origAxes = self.origFigure.add_subplot(111)
        self.origCanvas = FigureCanvas(self.origFigure)
        orig_lo.addWidget(self.origCanvas)
        self.origControlGrp = QGroupBox("Original Image Controls")
        ctrl1 = QGridLayout(self.origControlGrp)
        orig_lo.addWidget(self.origControlGrp)

        def mkSpin(step=5, dec=0):
            s = QDoubleSpinBox()
            s.setSingleStep(step)
            s.setDecimals(dec)
            s.setMinimum(0)
            s.setMaximum(1e12)
            return s

        self.minIntOrig = mkSpin()
        self.maxIntOrig = mkSpin()
        ctrl1.addWidget(QLabel("Min Intensity:"), 0, 0)
        ctrl1.addWidget(self.minIntOrig, 0, 1)
        ctrl1.addWidget(QLabel("Max Intensity:"), 1, 0)
        ctrl1.addWidget(self.maxIntOrig, 1, 1)

        self.origCenterXBox = mkSpin(dec=1)
        self.origCenterYBox = mkSpin(dec=1)
        ctrl1.addWidget(QLabel("Center X:"), 2, 0)
        ctrl1.addWidget(self.origCenterXBox, 2, 1)
        ctrl1.addWidget(QLabel("Center Y:"), 3, 0)
        ctrl1.addWidget(self.origCenterYBox, 3, 1)

        self.centerizeButton = QPushButton("Centerize")
        ctrl1.addWidget(self.centerizeButton, 4, 0, 1, 2)

        # Center tab
        self.centImageTab = QWidget()
        self.tabWidget.addTab(self.centImageTab, "Centerized Image")
        cent_lo = QHBoxLayout(self.centImageTab)
        self.centFigure = plt.figure()
        self.centAxes = self.centFigure.add_subplot(111)
        self.centCanvas = FigureCanvas(self.centFigure)
        cent_lo.addWidget(self.centCanvas)
        self.centControlGrp = QGroupBox("Centerized Image Controls")
        ctrl2 = QGridLayout(self.centControlGrp)
        cent_lo.addWidget(self.centControlGrp)

        self.rotAngSpBx = mkSpin(dec=1)
        ctrl2.addWidget(QLabel("Rotation Angle (°):"), 0, 0)
        ctrl2.addWidget(self.rotAngSpBx, 0, 1)
        self.rotateButton = QPushButton("Rotate")
        ctrl2.addWidget(self.rotateButton, 1, 0, 1, 2)

        # Rotated tab
        self.rotatedImageTab = QWidget()
        self.tabWidget.addTab(self.rotatedImageTab, "Rotated Image")
        rot_lo = QHBoxLayout(self.rotatedImageTab)
        self.rotFigure = plt.figure()
        self.rotAxes = self.rotFigure.add_subplot(111)
        self.rotCanvas = FigureCanvas(self.rotFigure)
        rot_lo.addWidget(self.rotCanvas)

        # Master tab
        self.mastImageTab = QWidget()
        self.tabWidget.addTab(self.mastImageTab, "Master Image")
        mast_lo = QHBoxLayout(self.mastImageTab)
        self.mastFigure = plt.figure()
        self.mastAxes = self.mastFigure.add_subplot(111)
        self.mastCanvas = FigureCanvas(self.mastFigure)
        mast_lo.addWidget(self.mastCanvas)
        self.mastControlGrp = QGroupBox("Master Image Controls")
        ctrl3 = QGridLayout(self.mastControlGrp)
        mast_lo.addWidget(self.mastControlGrp)

        self.mastMinInt = mkSpin()
        self.clearMastMinIntBtn = QPushButton("Clear")
        ctrl3.addWidget(QLabel("Min Intensity:"), 0, 0)
        ctrl3.addWidget(self.mastMinInt, 0, 1)
        ctrl3.addWidget(self.clearMastMinIntBtn, 0, 2)

        self.mastMaxInt = mkSpin()
        self.clearMastMaxIntBtn = QPushButton("Clear")
        ctrl3.addWidget(QLabel("Max Intensity:"), 1, 0)
        ctrl3.addWidget(self.mastMaxInt, 1, 1)
        ctrl3.addWidget(self.clearMastMaxIntBtn, 1, 2)

        self.mastCenterXBox = mkSpin(dec=1)
        self.clearMastCenterBtn = QPushButton("Clear")
        ctrl3.addWidget(QLabel("Center X:"), 2, 0)
        ctrl3.addWidget(self.mastCenterXBox, 2, 1)
        ctrl3.addWidget(self.clearMastCenterBtn, 2, 2)

        self.mastCenterYBox = mkSpin(dec=1)
        ctrl3.addWidget(QLabel("Center Y:"), 3, 0)
        ctrl3.addWidget(self.mastCenterYBox, 3, 1)

        self.mastRotationAngle = mkSpin(dec=1)
        self.clearMastRotAngleBtn = QPushButton("Clear")
        ctrl3.addWidget(QLabel("Rotation Angle (°):"), 4, 0)
        ctrl3.addWidget(self.mastRotationAngle, 4, 1)
        ctrl3.addWidget(self.clearMastRotAngleBtn, 4, 2)

        self.mastRecalc = QPushButton("Recalculate")
        ctrl3.addWidget(self.mastRecalc, 5, 0, 1, 3)

        # Status bar
        self.statusBar = QStatusBar()
        self.setStatusBar(self.statusBar)
        self.coordLabel = QLabel()
        self.statusBar.addPermanentWidget(self.coordLabel)

        # Signal connections
        self.minIntOrig.valueChanged.connect(self.refreshOrigImg)
        self.maxIntOrig.valueChanged.connect(self.refreshOrigImg)
        self.origCenterXBox.valueChanged.connect(self.refreshOrigImg)
        self.origCenterYBox.valueChanged.connect(self.refreshOrigImg)
        self.centerizeButton.clicked.connect(self._centerizeOriginal)
        self.origCanvas.mpl_connect("button_press_event", self._orig_clicked)
        self.origCanvas.mpl_connect("motion_notify_event", self._orig_motion)

        self.rotAngSpBx.valueChanged.connect(self._onCentAngleChanged)
        self.rotateButton.clicked.connect(self._onCentRotate)
        self.centCanvas.mpl_connect("button_press_event", self._cent_clicked)
        self.centCanvas.mpl_connect("motion_notify_event", self._cent_motion)

        self.rotCanvas.mpl_connect("motion_notify_event", self._rot_motion)

        self.mastMinInt.valueChanged.connect(lambda v: setattr(self, "master_min_int", v))
        self.clearMastMinIntBtn.clicked.connect(lambda: setattr(self, "master_min_int", None))
        self.mastMaxInt.valueChanged.connect(lambda v: setattr(self, "master_max_int", v))
        self.clearMastMaxIntBtn.clicked.connect(lambda: setattr(self, "master_max_int", None))
        self.mastCenterXBox.valueChanged.connect(lambda v: self._set_master_center(x=v))
        self.mastCenterYBox.valueChanged.connect(lambda v: self._set_master_center(y=v))
        self.clearMastCenterBtn.clicked.connect(lambda: setattr(self, "master_center", None))
        self.mastRotationAngle.valueChanged.connect(lambda v: setattr(self, "master_rot_angle", math.radians(v)))
        self.clearMastRotAngleBtn.clicked.connect(lambda: setattr(self, "master_rot_angle", None))
        self.mastRecalc.clicked.connect(self.recalculate)
        self.mastCanvas.mpl_connect("button_press_event", self._mast_clicked)
        self.mastCanvas.mpl_connect("motion_notify_event", self._mast_motion)

        self.show()
        self.resize(1200, 900)

    def browseFile(self):
        path = getAFile()
        if path:
            self.onNewFileSelected(path)

    def onNewFileSelected(self, path):
        self.orig_img = fabio.open(path).data.astype('float32')
        h, w = self.orig_img.shape
        self.center = [w/2, h/2]
        self.rot_angle = 0.0
        self.master_min_int = self.master_max_int = None
        self.master_center = None
        self.master_rot_angle = None

        self.minIntOrig.setValue(0)
        self.maxIntOrig.setValue(min(700, np.max(self.orig_img)/40))
        self.origCenterXBox.setValue(self.center[0])
        self.origCenterYBox.setValue(self.center[1])
        self.refreshOrigImg()

        self.cent_img = self._compute_center(self.center)
        self.rotAngSpBx.setValue(0)
        self.refreshCentImg()

        self.rot_img = None
        self.rotAxes.cla(); self.rotCanvas.draw_idle()

        self.mastAxes.cla(); self.mastCanvas.draw_idle()

    def refreshOrigImg(self):
        if self.orig_img is None:
            return
        displayImgToAxes(self.orig_img, self.origAxes,
                         self.minIntOrig.value(), self.maxIntOrig.value())
        self.origAxes.axvline(self.origCenterXBox.value(), color='y')
        self.origAxes.axhline(self.origCenterYBox.value(), color='y')
        self.origCanvas.draw_idle()

    def _orig_clicked(self, ev):
        if ev.xdata is None: return
        self.origCenterXBox.setValue(ev.xdata)
        self.origCenterYBox.setValue(ev.ydata)
        self.refreshOrigImg()

    def _orig_motion(self, ev):
        if ev.xdata is None: return
        self.coordLabel.setText(f"Orig coords: X={ev.xdata:.1f}, Y={ev.ydata:.1f}")

    def _centerizeOriginal(self):
        if self.orig_img is None: return
        self.center = [self.origCenterXBox.value(), self.origCenterYBox.value()]
        h, w = self.orig_img.shape
        dx = w/2 - self.center[0]
        dy = h/2 - self.center[1]
        M = np.array([[1, 0, dx], [0, 1, dy]], dtype=np.float32)
        self.cent_img = cv2.warpAffine(self.orig_img, M, (w, h))
        self.refreshCentImg()

    def refreshCentImg(self):
        if self.cent_img is None: return
        displayImgToAxes(self.cent_img, self.centAxes,
                         self.minIntOrig.value(), self.maxIntOrig.value())
        h, w = self.cent_img.shape
        cx, cy = w/2, h/2
        self.centAxes.axvline(cx, color='y')
        self.centAxes.axhline(cy, color='y')
        theta = self.rotAngSpBx.value() * math.pi/180.0
        x2 = cx + math.cos(theta)*10
        y2 = cy + math.sin(theta)*10
        self.centAxes.axline((cx, cy), (x2, y2), color='b')
        self.centCanvas.draw_idle()

    def _onCentAngleChanged(self, _):
        self.rot_angle = math.radians(self.rotAngSpBx.value())
        self.refreshCentImg()
        self._updateRotated()

    def _onCentRotate(self):
        self.rot_angle = math.radians(self.rotAngSpBx.value())
        self._updateRotated()

    def _cent_clicked(self, ev):
        if ev.xdata is None: return
        h, w = self.cent_img.shape
        cx, cy = w/2, h/2
        dx = ev.xdata - cx
        dy = ev.ydata - cy
        ang = math.degrees(math.atan2(dy, dx)) % 360
        self.rotAngSpBx.setValue(ang)
        self._onCentRotate()

    def _cent_motion(self, ev):
        if ev.xdata is None: return
        h, w = self.cent_img.shape
        cx, cy = w/2, h/2
        dx = ev.xdata - cx
        dy = ev.ydata - cy
        ox = dx + self.center[0]
        oy = dy + self.center[1]
        self.coordLabel.setText(f"Orig coords: X={ox:.1f}, Y={oy:.1f}")

    def _updateRotated(self):
        if self.cent_img is None: return
        h, w = self.cent_img.shape
        M = cv2.getRotationMatrix2D((w/2, h/2), math.degrees(-self.rot_angle), 1.0)
        self.rot_img = cv2.warpAffine(self.cent_img, M, (w, h))
        displayImgToAxes(self.rot_img, self.rotAxes,
                         self.minIntOrig.value(), self.maxIntOrig.value())
        self.rotAxes.axvline(w/2, color='y')
        self.rotAxes.axhline(h/2, color='y')
        self.rotCanvas.draw_idle()

    def _rot_motion(self, ev):
        if ev.xdata is None: return
        h, w = self.rot_img.shape
        cx, cy = w/2, h/2
        dx = ev.xdata - cx
        dy = ev.ydata - cy
        cosA = math.cos(-self.rot_angle)
        sinA = math.sin(-self.rot_angle)
        ux = dx*cosA - dy*sinA
        uy = dx*sinA + dy*cosA
        ox = ux + self.center[0]
        oy = uy + self.center[1]
        self.coordLabel.setText(f"Orig coords: X={ox:.1f}, Y={oy:.1f}")

    def _set_master_center(self, x=None, y=None):
        if self.master_center is None:
            self.master_center = [self.center[0], self.center[1]]
        if x is not None: self.master_center[0] = x
        if y is not None: self.master_center[1] = y

    def recalculate(self):
        if self.orig_img is None: return
        min_i = self.master_min_int if self.master_min_int is not None else self.minIntOrig.value()
        max_i = self.master_max_int if self.master_max_int is not None else self.maxIntOrig.value()
        cen  = self.master_center     if self.master_center     is not None else self.center
        ang  = self.master_rot_angle  if self.master_rot_angle is not None else self.rot_angle
        # center & rotate
        h, w = self.orig_img.shape
        dx = w/2 - cen[0]
        dy = h/2 - cen[1]
        M1 = np.array([[1,0,dx],[0,1,dy]],dtype=np.float32)
        cent = cv2.warpAffine(self.orig_img, M1, (w,h))
        M2 = cv2.getRotationMatrix2D((w/2,h/2), math.degrees(-ang), 1.0)
        final = cv2.warpAffine(cent, M2, (w,h))
        displayImgToAxes(final, self.mastAxes, min_i, max_i)
        self.mastAxes.axvline(w/2, color='y')
        self.mastAxes.axhline(h/2, color='y')
        self.mastCanvas.draw_idle()

    def _mast_clicked(self, ev):
        if ev.xdata is None: return
        h, w = self.orig_img.shape
        cx, cy = w/2, h/2
        dx = ev.xdata - cx
        dy = ev.ydata - cy
        ox = dx + (self.master_center or self.center)[0]
        oy = dy + (self.master_center or self.center)[1]
        if ev.button == 1:
            self.mastCenterXBox.setValue(ox)
            self.mastCenterYBox.setValue(oy)
        elif ev.button == 3:
            ang = math.degrees(math.atan2(dy, dx)) % 360
            self.mastRotationAngle.setValue(ang)
        self.recalculate()

    def _mast_motion(self, ev):
        if ev.xdata is None: return
        h, w = self.orig_img.shape
        cx, cy = w/2, h/2
        dx = ev.xdata - cx
        dy = ev.ydata - cy
        ox = dx + (self.master_center or self.center)[0]
        oy = dy + (self.master_center or self.center)[1]
        self.coordLabel.setText(f"Orig coords: X={ox:.1f}, Y={oy:.1f}")

    # --- Geometry helpers ---
    def _compute_center(self, center):
        """Recenter orig_img so that `center` → middle, pad to square."""
        print("Compute center function")
        h, w = self.orig_img.shape
        print("h: ", h)
        print("w: ", w)
        cx, cy = center
        print("center: ", center)
        diff_x = (w/2 - cx); diff_y = (h/2 - cy)
        print("diff_x: ", diff_x)
        print("diff_y: ", diff_y)
        new_w = int(math.ceil(w + 2*diff_x)); new_h = int(math.ceil(h + 2*diff_y))
        print("new_w: ", new_w)
        print("new_h: ", new_h)
        dim = max(new_w, new_h)
        print("dim: ", dim)
        trans_x = dim/2 - cx; trans_y = dim/2 - cy
        print("Trans_x: ", trans_x) #debug
        print("Trans_y: ", trans_y) #debug
        M = np.array([[1,0,trans_x],[0,1,trans_y]],dtype=np.float32)
        canvas = np.zeros((dim,dim),dtype='float32')
        canvas[int(abs(diff_y)):int(abs(diff_y)+h), int(abs(diff_x)):int(abs(diff_x)+w)] = self.orig_img
        return cv2.warpAffine(canvas, M, (dim,dim))

    def _compute_rotation(self, center, angle, cent_img):
        """Rotate `cent_img` about its center by `angle` radians, pad to square."""
        h, w = cent_img.shape
        cx, cy = w/2, h/2
        theta = -angle
        M = cv2.getRotationMatrix2D((cx,cy), math.degrees(theta), 1.0)
        # bounding box
        corners = np.array([[0-cx,0-cy],[w-cx,0-cy],[w-cx,h-cy],[0-cx,h-cy]])
        R = np.array([[math.cos(theta), -math.sin(theta)],
                      [math.sin(theta),  math.cos(theta)]])
        rotated = corners.dot(R.T)
        max_dim = int(math.ceil(2*np.max(np.abs(rotated))))
        canvas = np.zeros((max_dim, max_dim), dtype='float32')
        dx = int(max_dim/2 - cx); dy = int(max_dim/2 - cy)
        canvas[dy:dy+h, dx:dx+w] = cent_img
        return cv2.warpAffine(canvas, M, (max_dim, max_dim))
