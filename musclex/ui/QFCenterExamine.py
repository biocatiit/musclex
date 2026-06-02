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
        self.mast_img = None             # master image

        self.cent_trans = None
        self.cent_scale = None

        self.rot_trans = None
        self.rot_scale = None
        self.rot_angle = None

        self.mast_trans = None
        self.mast_scale = None
        self.mast_angle = None

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
        if self.orig_img is None:
            return

        # 1) grab center from your UI
        cx = self.origCenterXBox.value()
        cy = self.origCenterYBox.value()

        # 2) image height (rows) and width (cols)
        h, w = self.orig_img.shape[:2]

        # 3) how far to shift so that (cx,cy) lands at (w/2,h/2)
        dx = w/2 - cx
        dy = h/2 - cy

        # 4) compute the max distance from the chosen center to each image edge
        max_dist_x = max(cx, w - cx)
        max_dist_y = max(cy, h - cy)

        # 5) pick the smallest scale that still fits both horizontally and vertically
        scale_x = (w/2) / max_dist_x
        scale_y = (h/2) / max_dist_y
        scale = min(scale_x, scale_y)

        # debug prints
        print(f"w={w}, h={h}")
        print(f"center=({cx},{cy}), max_dist_x={max_dist_x}, max_dist_y={max_dist_y}")
        print(f"scale_x={scale_x:.3f}, scale_y={scale_y:.3f} → using scale={scale:.3f}")
        print(f"dx*scale={dx*scale:.1f}, dy*scale={dy*scale:.1f}")

        # 2) now compute tx, ty so that (s·cx, s·cy) lands at (w/2, h/2)
        tx = (w / 2) - scale * cx
        ty = (h / 2) - scale * cy

        print("tx, ty: ", tx, ty) #debug

        # 6) build the affine matrix: scale then translate
        M = np.array([
            [scale, 0,     tx],
            [0,     scale, ty]
        ], dtype=np.float32)

        self.cent_trans =   (tx, ty)
        self.cent_scale = scale

        # 7) warpAffine wants dsize=(width, height)
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
        if self.orig_img is None:
            return

        # 1) dimensions and rotation in radians
        h, w = self.orig_img.shape
        angle_rad = -self.rot_angle              # assuming rot_angle is in radians
        cos_a, sin_a = math.cos(angle_rad), math.sin(angle_rad)

        # 2) the four image corners, relative to image center (0,0)
        o_h, o_w = self.orig_img.shape
        x = self.origCenterXBox.value()
        y = self.origCenterYBox.value()
        
        corners = [
            (-x, -y),
            ( o_w - x, -y),
            ( o_w - x,  o_h - y),
            (-x,  o_h - y)
        ]
        print("corners: ", corners) #debug

        # 3) rotate each corner and track max extents
        rotated = [(x * cos_a - y * sin_a, x * sin_a + y * cos_a)
                for x, y in corners]
        
        print("rotated: ", rotated) #debug

        max_x = max(abs(x) for x,y in rotated)
        max_y = max(abs(y) for x,y in rotated)

        # 3) pick scale so none of those extents exceeds half‑width/height
        scale_x = (w/2) / max_x
        scale_y = (h/2) / max_y
        s = min(scale_x, scale_y, 1.0)   # cap at 1.0 if you never want to up‑scale

        # 4) build affine to SCALE about center then translate back into frame
        #    T_center * S(s) * T_-center
        #    gives M1 = [ s, 0, (w/2) - s*(w/2) ]
        #             [ 0, s, (h/2) - s*(h/2) ]
        tx = (w/2) - s*x
        ty = (h/2) - s*y
        M1 = np.array([
            [s, 0,  tx],
            [0, s,  ty]
        ], dtype=np.float32)

        # 5) warp – this yields a scaled image, centered in a (w×h) frame
        scaled = cv2.warpAffine(self.orig_img, M1, (w, h))

        # 6) now rotate the scaled image about its center (no extra scale)
        M2 = cv2.getRotationMatrix2D((w/2, h/2), math.degrees(self.rot_angle), 1.0)
        self.rot_img = cv2.warpAffine(scaled, M2, (w, h))


        self.rot_trans = (tx, ty)
        self.rot_scale = s
        self.rot_angle = self.rot_angle
        # 7) redraw
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
        if self.orig_img is None:
            return

        # 0) dims
        h, w = self.orig_img.shape

        # Find the correct translation
        if self.mast_trans is not None:
            tx, ty = self.mast_trans
        elif self.rot_trans is not None:
            tx, ty = self.rot_trans
        elif self.cent_trans is not None:
            tx, ty = self.cent_trans
        else:
            tx, ty = 0, 0

        #Find the correct scale
        if self.mast_scale is not None:
            s = self.mast_scale
        elif self.rot_scale is not None:
            s = self.rot_scale
        elif self.cent_scale is not None:
            s = self.cent_scale
        else:
            s = 1.0

        #Find the correct rotation
        if self.mast_angle is not None:
            angle = self.mast_angle
        elif self.rot_angle is not None:
            angle = self.rot_angle
        elif self.cent_scale is not None:
            angle = self.rot_angle
        else:
            angle = 0.0


        # build M1 = S(fit_scale) about center + recenter
        M1  = np.array([[s, 0,         tx],
                        [0,         s, ty]],
                    dtype=np.float32)

        cent_img = cv2.warpAffine(self.orig_img, M1, (w, h))

        # === STAGE 2: rotate‐and‐fit ===

        # figure out how big the rotated corners go
        ang_rad = -angle
        c, s   = math.cos(ang_rad), math.sin(ang_rad)
        o_h, o_w = self.orig_img.shape
        x = self.origCenterXBox.value()
        y = self.origCenterYBox.value()
        
        corners = [
            (-x, -y),
            ( o_w - x, -y),
            ( o_w - x,  o_h - y),
            (-x,  o_h - y)
        ]

        rot_pts = [(x*c - y*s, x*s + y*c) for x,y in corners]
        max_rx = max(abs(x) for x,_ in rot_pts)
        max_ry = max(abs(y) for _,y in rot_pts)

        # compute fit‐scale so rotated image still fits
        rot_fit_scale = min((w/2)/max_rx, (h/2)/max_ry, 1.0)

        # build M2 = rotate(angle) about (w/2,h/2), scaled by rot_fit_scale
        M2 = cv2.getRotationMatrix2D(
            (w/2, h/2),
            math.degrees(angle),
            1
        )
        final = cv2.warpAffine(cent_img, M2, (w, h))

        # === DISPLAY ===

        min_i = (self.master_min_int
                if self.master_min_int is not None
                else self.minIntOrig.value())
        max_i = (self.master_max_int
                if self.master_max_int is not None
                else self.maxIntOrig.value())

        self.mast_img = final
        displayImgToAxes(final, self.mastAxes, min_i, max_i)
        self.mastAxes.axvline(w/2, color='y')
        self.mastAxes.axhline(h/2, color='y')
        self.mastCanvas.draw_idle()


    """
    1: rotate clicked point about the current center by - angle
    2. apply inverse transformation/scale matrix to get original image coords
    3. With original image coords, calculate new translation and scale.  Save these as self.mast_trans and self.mast_scale.  Use the recalculate function to apply these to the original image.
    """ 

    def _mast_clicked(self, ev):
        if ev.xdata is None or self.orig_img is None:
            return

        # 1) grab the click in display coords
        x_disp, y_disp = ev.xdata, ev.ydata
        print("Clicked: ", x_disp, y_disp) #debug
        h, w = self.mast_img.shape
        print("Master shape: ", h, w) #debug
        cx_disp, cy_disp = w/2, h/2
        print("Center(master coordinates): ", cx_disp, cy_disp) #debug
        # 2) figure out which angle & stage‐1 params were used
        angle = -self._get_angle_master()
        print("Angle: ", angle) #debug
        s1    = self._get_scale_master()
        print("Scale: ", s1) #debug
        tx1, ty1 = self._get_translation_master()
        print("Translation: ", tx1, ty1) #debug

        # === invert Stage 2 (rotation about center by +angle) ===
        # move into center‐origin
        dx = x_disp - cx_disp
        dy = y_disp - cy_disp
        print("CLicked point in centered coordinates: ", dx, dy) #debug
        cos_a = math.cos(angle)
        sin_a = math.sin(angle)
        # undo rotation
        x1 =  dx * cos_a + dy * sin_a
        y1 = -dx * sin_a + dy * cos_a
        print("Centered and inverse rotated points: ", x1, y1) #debug
        # back into the scaled frame
        x_cent = x1 + cx_disp
        y_cent = y1 + cy_disp
        print("Inverse rotated points in master coordinates: ", x_cent, y_cent) #debug

        # === invert Stage 1 (scale then translate) ===
        # (x_cent, y_cent) = s1*(x_orig, y_orig) + (tx1, ty1)

        x_orig = (x_cent - tx1) / s1
        y_orig = (y_cent - ty1) / s1

        print("original coordinates: ", x_orig, y_orig) #debug

        # update your UI controls for the new master center
        self.mastCenterXBox.setValue(x_orig)
        self.mastCenterYBox.setValue(y_orig)

        angle2 = self._get_angle_master()
        cos2, sin2 = math.cos(angle2), math.sin(angle2)
        h_o, w_o = self.orig_img.shape

        corners = [
            (-x_orig,      -y_orig),
            ( w_o - x_orig,  -y_orig),
            ( w_o - x_orig,   h_o - y_orig),
            (-x_orig,       h_o - y_orig)
        ]

        rot_pts = [
            (cx * cos2 - cy * sin2, cx * sin2 + cy * cos2)
            for cx, cy in corners
        ]

        max_rx = max(abs(px) for px, py in rot_pts)
        max_ry = max(abs(py) for px, py in rot_pts)

        # fit‐to‐frame scale (never upscale beyond 1.0)
        new_s = min((w_o/2) / max_rx, (h_o/2) / max_ry, 1.0)

        new_tx = (w_o/2) - new_s * x_orig
        new_ty = (h_o/2) - new_s * y_orig

        # store master params y6
        self.mast_scale = new_s
        self.mast_trans = (new_tx, new_ty)

        # 3) redraw
        self.recalculate()


    def _mast_motion(self, ev):
        if ev.xdata is None or self.orig_img is None:
            return

        # display coords (master image coords)
        x_disp, y_disp = ev.xdata, ev.ydata

        # original image dimensions
        h, w = self.orig_img.shape[:2]
        cx_disp, cy_disp = w/2, h/2

        # 1) invert STAGE 2: rotation + rot_scale
        # pick the angle & scale actually used
        angle = (
            self.master_rot_angle
            if self.master_rot_angle is not None
            else (self.rot_angle or 0.0)
        )
        s_rot = self.rot_scale or 1.0

        # translate into zero‐centered coords
        x0 = x_disp - cx_disp
        y0 = y_disp - cy_disp
        # undo rotation‐scale
        x1 = x0 / s_rot
        y1 = y0 / s_rot
        # undo rotation by +angle:
        cos_a = math.cos(angle)
        sin_a = math.sin(angle)
        x_cent0 =  x1 * cos_a + y1 * sin_a
        y_cent0 = -x1 * sin_a + y1 * cos_a
        # back into centered frame
        x_cent = x_cent0 + cx_disp
        y_cent = y_cent0 + cy_disp

        # 2) invert STAGE 1: centering + cent_scale
        s_cent = self.cent_scale or 1.0
        # center point used when centering
        cen_x, cen_y = (
            self.master_center
            if self.master_center is not None
            else (self.cent_trans or (w/2, h/2))
        )
        tx1 = (w/2) - s_cent * cen_x
        ty1 = (h/2) - s_cent * cen_y

        orig_x = (x_cent - tx1) / s_cent
        orig_y = (y_cent - ty1) / s_cent

        # update label: master coords and original image coords
        self.coordLabel.setText(
            f"Master coords: X={x_disp:.1f}, Y={y_disp:.1f}    "
            f"Orig coords: X={orig_x:.1f}, Y={orig_y:.1f}"
        )


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


    def _get_angle_master(self):
        """Get the angle of the master image."""
        if self.master_rot_angle is not None:
            return self.master_rot_angle
        elif self.rot_angle is not None:
            return self.rot_angle
        else:
            return 0.0
        
    def _get_scale_master(self):
        """Get the scale of the master image."""
        if self.mast_scale is not None:
            return self.mast_scale
        elif self.rot_scale is not None:
            return self.rot_scale
        elif self.cent_scale is not None:
            return self.cent_scale
        else:
            return 1.0
        
    def _get_translation_master(self):
        """Get the translation of the master image."""
        if self.mast_trans is not None:
            return self.mast_trans
        elif self.rot_trans is not None:
            return self.rot_trans
        elif self.cent_trans is not None:
            return self.cent_trans
        else:
            return 0,0