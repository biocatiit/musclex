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
from datetime import datetime
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.backend_bases import MouseButton
from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.colors import LogNorm, Normalize, ListedColormap

from PySide6.QtWidgets import (
    QApplication,
    QDialog,
    QMainWindow,
    QPushButton,
    QDialogButtonBox,
    QVBoxLayout,
    QHBoxLayout,
    QGridLayout,
    QLabel,
    QCheckBox,
    QFrame,
    QScrollArea,
    QGroupBox,
    QSpinBox,
    QDoubleSpinBox,
    QLineEdit,
    QSizePolicy,
)
from PySide6.QtCore import Qt

from ..ui_widget.double_zoom_widget import (DoubleZoomWidget,
                                            DoubleZoomWidgetState)
from ..ui_widget.crop_widget import (CropWidget, CropWidgetState)
from ..ui_widget.image_mouse_move_handler import (ImageMouseMoveHandler,
                                                  ImageMouseMoveState)
from ..ui_widget.zoom_handler import ZoomHandler

def print_log(log_str):
    now = datetime.now()
    now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"{now_str}: {log_str}")


class SetCentDialog(QDialog):
    def __init__(self,
                parent,
                img,
                center,
                isLogScale,
                vmin,
                vmax
        ):
        super().__init__()
        self.setModal(True)
        self.setWindowTitle("Set Center")
        self.img = img
        self.center = center
        self.isLogScale = isLogScale
        self.vmin = vmin
        self.vmax = vmax

        self.function = None

        x, y = self.center

        self.imageFigure = plt.figure()
        self.imageAxes = self.imageFigure.add_subplot(111)
        self.imageAxes.set_aspect('equal', adjustable="box")
        self.imageCanvas = FigureCanvas(self.imageFigure)

        if isLogScale:
            self.imageAxes.imshow(
                self.img,
                cmap="gray",
                norm=LogNorm(vmin=max(1, vmin), vmax=vmax),
            )
        else:
            self.imageAxes.imshow(
                self.img,
                cmap="gray",
                norm=Normalize(vmin=vmin, vmax=vmax),
            )

        self.imageAxes.set_facecolor('black')

        self.imageAxes.set_xlim((0, self.img.shape[1]))
        self.imageAxes.set_ylim((0, self.img.shape[0]))
        self.vline = self.imageAxes.axvline(x, color='y')
        self.hline = self.imageAxes.axhline(y, color='y')

        self.xInput = QLineEdit(f"{x:.2f}")
        self.yInput = QLineEdit(f"{y:.2f}")

        self.setCenterGroup = QGroupBox("Set Center")
        self.setCenterLayout = QGridLayout(self.setCenterGroup)

        # self.xInputLayout = QHBoxLayout()
        # self.xInputLayout.addWidget(QLabel("X:"))
        # self.xInputLayout.addWidget(self.xInput)

        # self.yInputLayout = QHBoxLayout()
        # self.yInputLayout.addWidget(QLabel("Y:"))
        # self.yInputLayout.addWidget(self.yInput)

        centerLayoutRowIndex = 0
        self.setCenterLayout.addWidget(QLabel("X (Current coords): "), centerLayoutRowIndex, 0, 1, 2)
        self.setCenterLayout.addWidget(self.xInput, centerLayoutRowIndex, 2, 1, 2)
        self.setCenterLayout.addWidget(QLabel("px"), centerLayoutRowIndex, 4, 1, 1)
        centerLayoutRowIndex += 1
        self.setCenterLayout.addWidget(QLabel("Y (Current coords): "), centerLayoutRowIndex, 0, 1, 2)
        self.setCenterLayout.addWidget(self.yInput, centerLayoutRowIndex, 2, 1, 2)
        self.setCenterLayout.addWidget(QLabel("px"), centerLayoutRowIndex, 4, 1, 1)

        QBtn = QDialogButtonBox.Ok | QDialogButtonBox.Cancel

        self.buttonBox = QDialogButtonBox(QBtn, Qt.Horizontal, self)
        self.buttonBox.accepted.connect(self.accept)
        self.buttonBox.rejected.connect(self.reject)

        self.mainLayout = QVBoxLayout(self)

        self.imageLayout = QHBoxLayout()
        self.imageLayout.setContentsMargins(0, 0, 0, 0)

        self.mainLayout.addLayout(self.imageLayout)

        self.optionsLayout = QVBoxLayout()

        self.displayOptGrpBx = QGroupBox("Display Options")
        self.dispOptLayout = QGridLayout(self.displayOptGrpBx)

        self.minIntLabel = QLabel('Min Intensity')
        self.maxIntLabel = QLabel('Max Intensity')

        self.spminInt = QDoubleSpinBox()
        self.spminInt.setToolTip("Reduction in the maximal intensity shown to allow for more details in the image.")
        self.spminInt.setKeyboardTracking(False)

        self.spmaxInt = QDoubleSpinBox()
        self.spmaxInt.setToolTip("Increase in the minimal intensity shown to allow for more details in the image.")
        self.spmaxInt.setKeyboardTracking(False)

        min_val = self.img.min()
        max_val = self.img.max()
        self.spminInt.setRange(min_val, max_val)
        self.spmaxInt.setRange(min_val, max_val)

        self.spminInt.setSingleStep(max_val * .05)
        self.spmaxInt.setSingleStep(max_val * .05)

        self.spminInt.setValue(self.vmin)
        self.spmaxInt.setValue(self.vmax)

        self.spminInt.setDecimals(2)
        self.spmaxInt.setDecimals(2)

        self.logScaleIntChkBx = QCheckBox("Log scale intensity")
        self.logScaleIntChkBx.setChecked(self.isLogScale)

        self.cropWidget = CropWidget(self.imageAxes)
        self.imgZoomOutBtn = QPushButton("Full")
        self.doubleZoom = DoubleZoomWidget(self.imageAxes, parent)

        self.doubleZoomText = QLabel("In Double Zoom mode, click a point in the image. Then, click the same point in the double zoom region to mark the center location.")
        self.doubleZoomText.setWordWrap(True)

        self.dispOptLayoutRowIndex = 0
        self.dispOptLayout.addWidget(self.minIntLabel, self.dispOptLayoutRowIndex, 0, 1, 2)
        self.dispOptLayout.addWidget(self.maxIntLabel, self.dispOptLayoutRowIndex, 2, 1, 2)
        self.dispOptLayoutRowIndex += 1
        self.dispOptLayout.addWidget(self.spminInt, self.dispOptLayoutRowIndex, 0, 1, 2)
        self.dispOptLayout.addWidget(self.spmaxInt, self.dispOptLayoutRowIndex, 2, 1, 2)
        self.dispOptLayoutRowIndex += 1
        self.dispOptLayout.addWidget(self.logScaleIntChkBx, self.dispOptLayoutRowIndex, 0, 1, 2)
        self.dispOptLayoutRowIndex += 1
        self.dispOptLayout.addWidget(self.cropWidget, self.dispOptLayoutRowIndex, 0, 1, 2)
        self.dispOptLayout.addWidget(self.imgZoomOutBtn, self.dispOptLayoutRowIndex, 2, 1, 2)
        self.dispOptLayoutRowIndex += 1
        self.dispOptLayout.addWidget(self.doubleZoom, self.dispOptLayoutRowIndex, 0, 1, 4)
        self.dispOptLayoutRowIndex += 1
        self.dispOptLayout.addWidget(self.doubleZoomText, self.dispOptLayoutRowIndex, 0, 1, 4)
        self.dispOptLayoutRowIndex += 1

        self.optionsLayout.addWidget(self.displayOptGrpBx)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.setCenterGroup)
        self.optionsLayout.addStretch()

        self.scrollAreaImg = QScrollArea()
        self.scrollAreaImg.setWidgetResizable(True)
        self.imageLayout.addWidget(self.scrollAreaImg)
        # self.imageLayout.addWidget(self.imageCanvas)
        self.scrollAreaImg.setWidget(self.imageCanvas)

        self.scrollAreaImg.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        self.scrollAreaImg.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        self.imageLayout.addLayout(self.optionsLayout)
        self.mainLayout.addWidget(self.buttonBox)
        self.mainLayout.setAlignment(self.buttonBox, Qt.AlignCenter)

        self.imageCanvas.setMinimumSize(800, 600)

        self.setMinimumSize(700, 500)
        self.resize(1200, 1000 // 4 * 3)

        self.imageFigure.tight_layout()
        self.imageCanvas.draw()

        self.createConnections()

        self.imageMouseMoveHandler = ImageMouseMoveHandler(self.imageAxes, self.img)
        self.zoomHandler = ZoomHandler(self.imageAxes)

    def createConnections(self):
        self.imgZoomOutBtn.clicked.connect(self.imageZoomOut)
        self.imageFigure.canvas.mpl_connect('button_press_event', self.handle_mouse_button_press_event)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.handle_mouse_move_event)
        self.imageFigure.canvas.mpl_connect('button_release_event', self.handle_mouse_button_release_event)
        self.imageFigure.canvas.mpl_connect('scroll_event', self.handle_mouse_wheel_scroll_event)

        self.spminInt.valueChanged.connect(
            lambda vmin: self.updateImageMinMax(vmin=vmin))
        self.spmaxInt.valueChanged.connect(
            lambda vmax: self.updateImageMinMax(vmax=vmax))
        self.logScaleIntChkBx.stateChanged.connect(self.imageScaleChecked)

        # Update center immediately when losing focus or pressing enter, without closing dialog
        self.xInput.returnPressed.connect(self.updateCenterFromInput)
        self.yInput.returnPressed.connect(self.updateCenterFromInput)
        self.xInput.editingFinished.connect(self.updateCenterFromInput)
        self.yInput.editingFinished.connect(self.updateCenterFromInput)

    def keyPressEvent(self, event):
        key = event.key()
        if key in [Qt.Key_Return, Qt.Key_Enter]:
            # Prevent closing dialog with keyboard.
            return

        if key == Qt.Key_Escape:
            if self.function and self.function[0] == "im_zoomin":
                self.function = None
                self.cropWidget.set_disable()
                ax = self.imageAxes
                label = "zoom_region"
                to_remove = [p for p in ax.patches if p.get_label() == label]
                for p in to_remove:
                    p.remove()

                self.refreshCenter()
            # Prevent closing dialog with keyboard.
            return

        super().keyPressEvent(event)

    def handle_mouse_button_press_event(self, event):
        if event.button != MouseButton.LEFT:
            return

        if event.inaxes != self.imageAxes:
            return

        self.imageMouseMoveHandler.handle_mouse_button_press_event(event)

        if self.doubleZoom.is_enabled():
            self.doubleZoom.handle_mouse_button_press_event(event)

    def handle_mouse_move_event(self, event):
        x = event.xdata
        y = event.ydata
        ax = self.imageAxes

        self.imageMouseMoveHandler.handle_mouse_move_event(event)

        if self.imageMouseMoveHandler.state == ImageMouseMoveState.MOUSE_DRAGGING:
            return

        if self.doubleZoom.is_enabled():
            self.doubleZoom.handle_mouse_move_event(event)

            if self.doubleZoom.is_no_action_state(event):
                return

        if event.inaxes != self.imageAxes:
            return

        self.cropWidget.handle_mouse_move_event(event)

        self.imageCanvas.draw_idle()

    def handle_mouse_button_release_event(self, event):
        if event.button != MouseButton.LEFT:
            return

        x = event.xdata
        y = event.ydata

        self.imageMouseMoveHandler.handle_mouse_button_release_event(event)

        if self.imageMouseMoveHandler.state == ImageMouseMoveState.MOUSE_DRAG_COMPLETED:
            return

        if self.doubleZoom.is_enabled():
            self.doubleZoom.handle_mouse_button_release_event(event)

            if self.doubleZoom.is_no_action_state(event):
                return

            if event.inaxes == self.imageAxes:
                if DoubleZoomWidgetState.MainImageClicked in self.doubleZoom.state:
                    if self.cropWidget.is_enabled():
                        self.cropWidget.click_with_double_zoom(event)
                    else:
                        self.center = (x, y)
                        self.refreshCenter()
                return

            # If double-zoom image was clicked, update mouse click coordinates.
            elif DoubleZoomWidgetState.DoubleZoomImageClicked in self.doubleZoom.state:
                x, y = self.doubleZoom.doubleZoomToOrigCoord()

        if event.inaxes == self.imageAxes or (self.doubleZoom.is_enabled()
            and DoubleZoomWidgetState.DoubleZoomImageClicked in self.doubleZoom.state):
            if self.cropWidget.is_enabled():
                self.cropWidget.handle_click_event(event, x, y)

                # if len(self.function) == 3:
                #     p1 = self.function[1]
                #     p2 = self.function[2]
                #     img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                #     self.function = None
                #     self.cropWidget.set_disable()
                #     self.resizeImage(img_zoom)
                #     self.refreshCenter()
            else:
                self.center = (x, y)
                self.refreshCenter(updateText=True)

        # elif (self.doubleZoom.is_enabled()
        #     and DoubleZoomWidgetState.DoubleZoomImageClicked in self.doubleZoom.state):
        #     if self.cropWidget.is_enabled():

        #     if self.function and self.function[0] == "im_zoomin":
        #         self.function.append((x, y))
        #         if len(self.function) == 3:
        #             p1 = self.function[1]
        #             p2 = self.function[2]
        #             img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
        #             self.function = None
        #             self.cropWidget.set_disable()
        #     else:
        #         self.center = (x, y)
        #         self.refreshCenter(updateText=True)

    def handle_mouse_wheel_scroll_event(self, event):
        if event.inaxes != self.imageAxes:
            return

        self.zoomHandler.handle_mouse_wheel_scroll_event(event)

    def updateImageMinMax(self, vmin=None, vmax=None):
        if vmin is not None:
            self.vmin = vmin
        if vmax is not None:
            self.vmax = vmax

        self.redrawImage()

    def imageScaleChecked(self):
        self.isLogScale = self.logScaleIntChkBx.isChecked()
        self.redrawImage()

    def redrawImage(self):
        if self.isLogScale:
            self.imageAxes.imshow(
                self.img,
                cmap="gray",
                norm=LogNorm(vmin=max(1, self.vmin), vmax=self.vmax),
            )
        else:
            self.imageAxes.imshow(
                self.img,
                cmap="gray",
                norm=Normalize(vmin=self.vmin, vmax=self.vmax),
            )

        # self.imageFigure.tight_layout()
        self.imageCanvas.draw()


    def resizeImage(self, img_zoom):
        if img_zoom and len(img_zoom) == 2:
            ax = self.imageAxes
            ax.set_xlim(img_zoom[0])
            ax.set_ylim(img_zoom[1])

        # self.imageFigure.tight_layout()
        self.imageCanvas.draw_idle()

    def refreshCenter(self, updateText=False):
        x, y = self.center

        ax = self.imageAxes

        # Remove old lines
        self.remove_image_lines()

        # Draw new lines
        self.vline = self.imageAxes.axvline(x, color='y')
        self.hline = self.imageAxes.axhline(y, color='y')

        if updateText:
            # Update input output
            self.xInput.setText(f"{x:.2f}")
            self.yInput.setText(f"{y:.2f}")
            # self.xOutput.setText(f"{x:.2f}")
            # self.yOutput.setText(f"{y:.2f}")

        # self.imageFigure.tight_layout()
        self.imageCanvas.draw_idle()

    def updateCenterFromInput(self):
        x = float(self.xInput.text())
        y = float(self.yInput.text())
        self.center = (x, y)
        self.refreshCenter(updateText=False)

    def remove_image_lines(self, labels=None):
        ax = self.imageAxes

        for i in range(len(ax.lines)-1, -1, -1):
            if labels:
                if ax.lines[i].get_label() in labels:
                    ax.lines[i].remove()
            else:
                ax.lines[i].remove()

        for i in range(len(ax.patches)-1, -1, -1):
            ax.patches[i].remove()

    def imageZoomIn(self):
        if self.cropWidget.isChecked():
            ax = self.imageAxes
            for i in range(len(ax.lines)-1,-1,-1):
                ax.lines[i].remove()
            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            self.imageCanvas.draw_idle()
            self.function = ["im_zoomin"]
        else:
            self.function = None

    def imageZoomOut(self):
        self.cropWidget.set_disable()
        img_zoom = [(0, self.img.shape[1]), (0, self.img.shape[0])]
        self.resizeImage(img_zoom)
        self.refreshCenter()
