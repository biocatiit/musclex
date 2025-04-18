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
"""

import os
import math
import cv2
import numpy as np
import fabio
import matplotlib.pyplot as plt
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.colors import Normalize
from PyQt5.QtCore import Qt, QPointF, pyqtSignal
from PyQt5.QtWidgets import (
    QMainWindow, QWidget, QScrollArea, QTabWidget,
    QHBoxLayout, QGridLayout, QGroupBox, QStatusBar,
    QLabel, QPushButton, QDoubleSpinBox
)
from .pyqt_utils import *
from ..utils.file_manager import *


def displayImgToAxes(img, ax, min_inten, max_inten):
    ax.cla()
    if img is not None:
        ax.imshow(img, cmap='gray', norm=Normalize(vmin=min_inten, vmax=max_inten))
    ax.set_xticks([])
    ax.set_yticks([])


class QFCenterExamine(QMainWindow):
    coordChanged = pyqtSignal(str)

    def __init__(self):
        super().__init__()
        
        # Image state
        self.orig_img = None
        self.cent_img = None
        self.rot_img = None
        
        # Transform state
        self.center = QPointF(0, 0)
        self.angle = 0.0
        self.scale = 1.0
        
        # Master overrides
        self.master_min = None
        self.master_max = None
        self.master_center = None
        self.master_angle = None

        self.initUI()
        self.browseFile()

    def initUI(self):
        self.setWindowTitle("QF Center Examination")
        self.setupMainWidget()
        self.setupTabs()
        self.setupStatusBar()
        self.setupConnections()
        self.resize(1200, 900)

    def setupMainWidget(self):
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        central = QWidget()
        scroll.setWidget(central)
        self.setCentralWidget(scroll)
        self.main_layout = QHBoxLayout(central)
        self.tab_widget = QTabWidget()
        self.main_layout.addWidget(self.tab_widget)

    def setupTabs(self):
        # Original Image Tab
        self.orig_tab = QWidget()
        self.tab_widget.addTab(self.orig_tab, "Original")
        orig_layout = QHBoxLayout(self.orig_tab)
        
        # Original Image Plot
        self.orig_fig, self.orig_ax = plt.subplots()
        self.orig_canvas = FigureCanvas(self.orig_fig)
        orig_layout.addWidget(self.orig_canvas)
        
        # Original Controls
        orig_ctrl = QGroupBox("Original Controls")
        orig_layout.addWidget(orig_ctrl)
        g = QGridLayout(orig_ctrl)
        
        self.min_spin = self.createSpinBox()
        self.max_spin = self.createSpinBox()
        self.cx_spin = self.createSpinBox(dec=1)
        self.cy_spin = self.createSpinBox(dec=1)
        
        g.addWidget(QLabel("Min Intensity:"), 0, 0)
        g.addWidget(self.min_spin, 0, 1)
        g.addWidget(QLabel("Max Intensity:"), 1, 0)
        g.addWidget(self.max_spin, 1, 1)
        g.addWidget(QLabel("Center X:"), 2, 0)
        g.addWidget(self.cx_spin, 2, 1)
        g.addWidget(QLabel("Center Y:"), 3, 0)
        g.addWidget(self.cy_spin, 3, 1)
        g.addWidget(QPushButton("Centerize", clicked=self.centerize), 4, 0, 1, 2)

        # Centerized Tab
        self.cent_tab = QWidget()
        self.tab_widget.addTab(self.cent_tab, "Centerized")
        cent_layout = QHBoxLayout(self.cent_tab)
        
        self.cent_fig, self.cent_ax = plt.subplots()
        self.cent_canvas = FigureCanvas(self.cent_fig)
        cent_layout.addWidget(self.cent_canvas)
        
        cent_ctrl = QGroupBox("Centerized Controls")
        cent_layout.addWidget(cent_ctrl)
        g = QGridLayout(cent_ctrl)
        
        self.angle_spin = self.createSpinBox(dec=1)
        g.addWidget(QLabel("Rotation Angle:"), 0, 0)
        g.addWidget(self.angle_spin, 0, 1)
        g.addWidget(QPushButton("Rotate", clicked=self.rotate), 1, 0, 1, 2)

        # Rotated Tab
        self.rot_tab = QWidget()
        self.tab_widget.addTab(self.rot_tab, "Rotated")
        rot_layout = QHBoxLayout(self.rot_tab)
        
        self.rot_fig, self.rot_ax = plt.subplots()
        self.rot_canvas = FigureCanvas(self.rot_fig)
        rot_layout.addWidget(self.rot_canvas)

        # Master Tab
        self.master_tab = QWidget()
        self.tab_widget.addTab(self.master_tab, "Master")
        master_layout = QHBoxLayout(self.master_tab)
        
        self.master_fig, self.master_ax = plt.subplots()
        self.master_canvas = FigureCanvas(self.master_fig)
        master_layout.addWidget(self.master_canvas)
        
        master_ctrl = QGroupBox("Master Controls")
        master_layout.addWidget(master_ctrl)
        g = QGridLayout(master_ctrl)
        
        self.m_min = self.createSpinBox()
        self.m_max = self.createSpinBox()
        self.m_cx = self.createSpinBox(dec=1)
        self.m_cy = self.createSpinBox(dec=1)
        self.m_ang = self.createSpinBox(dec=1)
        
        g.addWidget(QLabel("Min Intensity:"), 0, 0)
        g.addWidget(self.m_min, 0, 1)
        g.addWidget(QPushButton("Clear", clicked=lambda: self.clearMaster('min')), 0, 2)
        
        g.addWidget(QLabel("Max Intensity:"), 1, 0)
        g.addWidget(self.m_max, 1, 1)
        g.addWidget(QPushButton("Clear", clicked=lambda: self.clearMaster('max')), 1, 2)
        
        g.addWidget(QLabel("Center X:"), 2, 0)
        g.addWidget(self.m_cx, 2, 1)
        g.addWidget(QLabel("Center Y:"), 3, 0)
        g.addWidget(self.m_cy, 3, 1)
        g.addWidget(QPushButton("Clear", clicked=lambda: self.clearMaster('center')), 3, 2)
        
        g.addWidget(QLabel("Rotation:"), 4, 0)
        g.addWidget(self.m_ang, 4, 1)
        g.addWidget(QPushButton("Clear", clicked=lambda: self.clearMaster('ang')), 4, 2)
        
        g.addWidget(QPushButton("Recalculate", clicked=self.recalculate), 5, 0, 1, 3)

    def setupStatusBar(self):
        self.status = QStatusBar()
        self.coord_label = QLabel()
        self.status.addPermanentWidget(self.coord_label)
        self.setStatusBar(self.status)

    def setupConnections(self):
        # Mouse events
        self.orig_canvas.mpl_connect('button_press_event', self.origClick)
        self.orig_canvas.mpl_connect('motion_notify_event', self.origMotion)
        self.cent_canvas.mpl_connect('button_press_event', self.centClick)
        self.cent_canvas.mpl_connect('motion_notify_event', self.centMotion)
        self.master_canvas.mpl_connect('button_press_event', self.masterClick)
        self.master_canvas.mpl_connect('motion_notify_event', self.masterMotion)

        # Spinbox changes
        self.min_spin.valueChanged.connect(self.refreshOrig)
        self.max_spin.valueChanged.connect(self.refreshOrig)
        self.cx_spin.valueChanged.connect(self.refreshOrig)
        self.cy_spin.valueChanged.connect(self.refreshOrig)
        self.angle_spin.valueChanged.connect(self.refreshCent)
        self.m_min.valueChanged.connect(lambda v: setattr(self, 'master_min', v))
        self.m_max.valueChanged.connect(lambda v: setattr(self, 'master_max', v))
        self.m_cx.valueChanged.connect(self.masterCenterChanged)
        self.m_cy.valueChanged.connect(self.masterCenterChanged)
        self.m_ang.valueChanged.connect(lambda v: setattr(self, 'master_angle', math.radians(v)))

    def createSpinBox(self, dec=0):
        s = QDoubleSpinBox()
        s.setDecimals(dec)
        s.setRange(-1e6 if dec else 0, 1e6)
        s.setSingleStep(1 if dec else 10)
        return s

    def browseFile(self):
        path = getAFile()
        if path:
            self.loadImage(path)

    def loadImage(self, path):
        try:
            self.orig_img = fabio.open(path).data
            h, w = self.orig_img.shape
            self.center = QPointF(w/2, h/2)
            self.cx_spin.setValue(w/2)
            self.cy_spin.setValue(h/2)
            self.min_spin.setValue(0)
            self.max_spin.setValue(np.percentile(self.orig_img, 99))
            self.refreshAll()
        except Exception as e:
            print(f"Error loading image: {str(e)}")

    def refreshAll(self):
        self.refreshOrig()
        self.centerize()
        self.rotate()

    # Original Tab Logic ----------------------------------------------------
    def refreshOrig(self):
        displayImgToAxes(self.orig_img, self.orig_ax, 
                        self.min_spin.value(), self.max_spin.value())
        self.orig_ax.axvline(self.cx_spin.value(), color='y')
        self.orig_ax.axhline(self.cy_spin.value(), color='y')
        self.orig_canvas.draw()

    def origClick(self, event):
        if event.xdata and event.ydata:
            self.cx_spin.setValue(event.xdata)
            self.cy_spin.setValue(event.ydata)

    def origMotion(self, event):
        if event.xdata and event.ydata:
            self.coord_label.setText(f"Original Coords: X={event.xdata:.1f}, Y={event.ydata:.1f}")

    def centerize(self):
        if self.orig_img is None:
            return
            
        # Calculate translation
        h, w = self.orig_img.shape
        tx = w/2 - self.cx_spin.value()
        ty = h/2 - self.cy_spin.value()
        
        # Create centered image
        M = np.float32([[1, 0, tx], [0, 1, ty]])
        self.cent_img = cv2.warpAffine(self.orig_img, M, (w, h))
        self.refreshCent()

    # Centerized Tab Logic --------------------------------------------------
    def refreshCent(self):
        if self.cent_img is None:
            return
            
        # Calculate rotation preview
        angle = math.radians(self.angle_spin.value())
        self._updateRotation(angle)
        
        # Draw image
        displayImgToAxes(self.cent_img, self.cent_ax,
                        self.min_spin.value(), self.max_spin.value())
        h, w = self.cent_img.shape
        self.cent_ax.axvline(w/2, color='y')
        self.cent_ax.axhline(h/2, color='y')
        
        # Draw rotation line
        cx, cy = w/2, h/2
        length = min(w, h) * 0.4
        dx = math.cos(angle) * length
        dy = math.sin(angle) * length
        self.cent_ax.plot([cx, cx+dx], [cy, cy+dy], 'b-')
        self.cent_canvas.draw()

    def _updateRotation(self, angle):
        self.angle = angle
        self._updateScale()

    def _updateScale(self):
        if self.orig_img is None:
            return
            
        h, w = self.orig_img.shape
        corners = [
            QPointF(0, 0), QPointF(w, 0),
            QPointF(w, h), QPointF(0, h)
        ]
        
        max_r = 0.0
        cos_a = math.cos(self.angle)
        sin_a = math.sin(self.angle)
        
        for pt in corners:
            dx = pt.x() - self.center.x()
            dy = pt.y() - self.center.y()
            rx = dx * cos_a - dy * sin_a
            ry = dx * sin_a + dy * cos_a
            max_r = max(max_r, math.hypot(rx, ry))
            
        self.scale = min(
            self.cent_canvas.width() / (2 * max_r),
            self.cent_canvas.height() / (2 * max_r),
            1.0
        )

    def centClick(self, event):
        if event.xdata and event.ydata:
            # Calculate angle from center
            cx = self.cent_img.shape[1]/2
            cy = self.cent_img.shape[0]/2
            dx = (event.xdata - cx) / self.scale
            dy = (event.ydata - cy) / self.scale
            angle = math.degrees(math.atan2(dy, dx))
            self.angle_spin.setValue(angle % 360)

    def centMotion(self, event):
        if event.xdata and event.ydata:
            cx = self.cent_img.shape[1]/2
            cy = self.cent_img.shape[0]/2
            dx = (event.xdata - cx) / self.scale
            dy = (event.ydata - cy) / self.scale
            self.coord_label.setText(
                f"Original Coords: X={self.center.x()+dx:.1f}, Y={self.center.y()+dy:.1f}"
            )

    # Rotation Tab Logic ----------------------------------------------------
    def rotate(self):
        if self.cent_img is None:
            return
            
        # Calculate rotation matrix
        h, w = self.cent_img.shape
        M = cv2.getRotationMatrix2D((w/2, h/2), math.degrees(self.angle), 1.0)
        self.rot_img = cv2.warpAffine(self.cent_img, M, (w, h))
        self.refreshRot()

    def refreshRot(self):
        displayImgToAxes(self.rot_img, self.rot_ax,
                        self.min_spin.value(), self.max_spin.value())
        self.rot_ax.axvline(self.rot_img.shape[1]/2, color='y')
        self.rot_ax.axhline(self.rot_img.shape[0]/2, color='y')
        self.rot_canvas.draw()

    # Master Tab Logic ------------------------------------------------------
    def masterCenterChanged(self):
        if self.master_center is None:
            self.master_center = QPointF(0, 0)
        self.master_center.setX(self.m_cx.value())
        self.master_center.setY(self.m_cy.value())

    def clearMaster(self, field):
        if field == 'min':
            self.master_min = None
            self.m_min.clear()
        elif field == 'max':
            self.master_max = None
            self.m_max.clear()
        elif field == 'center':
            self.master_center = None
            self.m_cx.clear()
            self.m_cy.clear()
        elif field == 'ang':
            self.master_angle = None
            self.m_ang.clear()

    def recalculate(self):
        # Get current parameters
        min_int = self.master_min if self.master_min is not None else self.min_spin.value()
        max_int = self.master_max if self.master_max is not None else self.max_spin.value()
        center = self.master_center if self.master_center is not None else self.center
        angle = self.master_angle if self.master_angle is not None else self.angle

        # Create transformed image
        img = self.orig_img.copy()
        
        # Apply centerization
        h, w = img.shape
        tx = w/2 - center.x()
        ty = h/2 - center.y()
        M = np.float32([[1, 0, tx], [0, 1, ty]])
        img = cv2.warpAffine(img, M, (w, h))
        
        # Apply rotation
        M = cv2.getRotationMatrix2D((w/2, h/2), math.degrees(angle), 1.0)
        img = cv2.warpAffine(img, M, (w, h))
        
        # Update display
        displayImgToAxes(img, self.master_ax, min_int, max_int)
        self.master_ax.axvline(w/2, color='y')
        self.master_ax.axhline(h/2, color='y')
        self.master_canvas.draw()

    def masterClick(self, event):
        if event.xdata and event.ydata:
            # Map click to original coordinates
            h, w = self.orig_img.shape
            cx = w/2
            cy = h/2
            
            # Calculate inverse transform
            dx = (event.xdata - cx) / self.scale
            dy = (event.ydata - cy) / self.scale
            cos_a = math.cos(-self.angle)
            sin_a = math.sin(-self.angle)
            orig_dx = dx * cos_a - dy * sin_a
            orig_dy = dx * sin_a + dy * cos_a
            
            # Update center
            new_x = self.center.x() + orig_dx
            new_y = self.center.y() + orig_dy
            self.m_cx.setValue(new_x)
            self.m_cy.setValue(new_y)

    def masterMotion(self, event):
        if event.xdata and event.ydata:
            h, w = self.orig_img.shape
            cx = w/2
            cy = h/2
            dx = (event.xdata - cx) / self.scale
            dy = (event.ydata - cy) / self.scale
            cos_a = math.cos(-self.angle)
            sin_a = math.sin(-self.angle)
            orig_x = self.center.x() + (dx * cos_a - dy * sin_a)
            orig_y = self.center.y() + (dx * sin_a + dy * cos_a)
            self.coord_label.setText(f"Original Coords: X={orig_x:.1f}, Y={orig_y:.1f}")

"""if __name__ == "__main__":
    import sys
    app = QApplication(sys.argv)
    window = QFCenterExamine()
    window.show()
    sys.exit(app.exec_())"""