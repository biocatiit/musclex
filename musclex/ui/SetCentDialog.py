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

from .DoubleZoomViewer import DoubleZoom


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
        self.mouse_pressed = False
        self.dragging = False
        self.press_x = None
        self.press_y = None
        self.last_drag_x = None
        self.last_drag_y = None

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
        # self.setCenterLayout.addLayout(self.xInputLayout)
        # self.setCenterLayout.addLayout(self.yInputLayout)

        # self.setCenterLayout.addWidget(self.updateBtn)

        # # Output boxes to show actual center values
        # self.xOutput = QLineEdit(f"{x:.2f}")
        # self.yOutput = QLineEdit(f"{y:.2f}")
        # self.xOutput.setReadOnly(True)
        # self.yOutput.setReadOnly(True)

        # self.outputLayout = QHBoxLayout()
        # self.outputLayout.addWidget(QLabel("Actual X:"))
        # self.outputLayout.addWidget(self.xOutput)
        # self.outputLayout.addWidget(QLabel("Actual Y:"))
        # self.outputLayout.addWidget(self.yOutput)

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
        # self.spminInt.setSingleStep(5)

        self.spmaxInt = QDoubleSpinBox()
        self.spmaxInt.setToolTip("Increase in the minimal intensity shown to allow for more details in the image.")
        self.spmaxInt.setKeyboardTracking(False)
        # self.spmaxInt.setSingleStep(5)

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

        self.imgZoomInBtn = QPushButton("Zoom in")
        self.imgZoomInBtn.setCheckable(True)
        self.imgZoomOutBtn = QPushButton("Full")
        self.doubleZoom = QCheckBox("Double Zoom")

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
        self.dispOptLayout.addWidget(self.imgZoomInBtn, self.dispOptLayoutRowIndex, 0, 1, 2)
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

        # self.scroll_areaImg = QScrollArea()
        # self.imageLayout.addWidget(self.scroll_areaImg)

        # self.scroll_areaImg.setWidgetResizable(True)

        # self.frameOfKeys = QFrame()
        # self.frameOfKeys.setFixedWidth(500)
        # self.frameOfKeys.setLayout(self.optionsLayout)
        # self.scroll_areaImg.setWidget(self.frameOfKeys)
        # self.scroll_areaImg.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        # self.mainLayout.addLayout(self.outputLayout)
        self.mainLayout.addWidget(self.buttonBox)
        # self.mainLayout.setAlignment(Qt.AlignCenter)
        self.mainLayout.setAlignment(self.buttonBox, Qt.AlignCenter)

        self.doubleZoomGUI = DoubleZoom(self.imageFigure, dontShowMessage=True)

        # pixels
        self.imageCanvas.setMinimumSize(800, 600)
        # self.imageCanvas.setSizePolicy(
        #     QSizePolicy.Expanding, QSizePolicy.Expanding
        # )

        self.setMinimumSize(700, 500)
        self.resize(1200, 1000 // 4 * 3)

        self.imageFigure.tight_layout()
        self.imageCanvas.draw()

        self.createConnections()

    def createConnections(self):
        self.imgZoomInBtn.clicked.connect(self.imageZoomIn)
        self.imgZoomOutBtn.clicked.connect(self.imageZoomOut)
        self.imageFigure.canvas.mpl_connect('button_press_event', self.imagePressed)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)
        self.imageFigure.canvas.mpl_connect('button_release_event', self.imageReleased)
        self.imageFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)
        self.doubleZoom.stateChanged.connect(self.doubleZoomChecked)

        self.spminInt.valueChanged.connect(
            lambda vmin: self.updateImageMinMax(vmin=vmin))
        self.spmaxInt.valueChanged.connect(
            lambda vmax: self.updateImageMinMax(vmax=vmax))
        self.logScaleIntChkBx.stateChanged.connect(self.ImageScaleChecked)

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
                self.imgZoomInBtn.setChecked(False)
                ax = self.imageAxes
                label = "zoom_region"
                to_remove = [p for p in ax.patches if p.get_label() == label]
                for p in to_remove:
                    p.remove()

                self.refreshCenter()
            # Prevent closing dialog with keyboard.
            return

        super().keyPressEvent(event)

    def imagePressed(self, event):
        if event.button != MouseButton.LEFT:
            return

        x = event.xdata
        y = event.ydata

        if event.inaxes == self.imageAxes:
            self.mouse_pressed = True
            self.dragging = False
            self.press_x = x
            self.press_y = y
            self.last_drag_x = x
            self.last_drag_y = y

    def imageOnMotion(self, event):
        x = event.xdata
        y = event.ydata
        ax = self.imageAxes

        if self.doubleZoom.isChecked():
            if event.inaxes == self.doubleZoomGUI.axes:
                if not self.doubleZoomGUI.doubleZoomMode:
                    # Draw cursor location in zoom using red cross lines.
                    self.doubleZoomGUI.updateAxes(x, y)
                    self.imageCanvas.draw_idle()

            elif event.inaxes == self.imageAxes:
                if self.doubleZoomGUI.doubleZoomMode:
                    # Draw cursor location in image using blue dot.
                    self.doubleZoomGUI.beginImgMotion(x, y, self.img.shape[1], self.img.shape[0], (0, 0), self.imageAxes)

                    # Update sursor area in zoom
                    self.doubleZoomGUI.mouseHoverBehavior(
                        x,
                        y,
                        self.img,
                        self.imageCanvas,
                        self.doubleZoom.isChecked(),
                        isLogScale=self.isLogScale,
                        vmin=self.vmin,
                        vmax=self.vmax
                    )

        else:
            if event.inaxes == self.imageAxes:
                # If moved enough, switch to dragging mode
                move_threshold = 2.0  # pixels in data coords
                if self.mouse_pressed:
                    if not self.dragging:
                        dx = abs(x - self.press_x)
                        dy = abs(y - self.press_y)
                        if dx > move_threshold or dy > move_threshold:
                            self.dragging = True

                    # Perform dragging
                    if self.dragging:
                        dx = x - self.last_drag_x
                        dy = y - self.last_drag_y

                        xlim = self.imageAxes.get_xlim()
                        ylim = self.imageAxes.get_ylim()

                        left = xlim[0] - dx
                        right = xlim[1] - dx
                        bottom = ylim[0] - dy
                        top = ylim[1] - dy

                        # left = min(max(left, 0), self.img.shape[1])
                        # right = min(max(right, 0), self.img.shape[1])
                        # bottom = min(max(bottom, 0), self.img.shape[0])
                        # top = min(max(top, 0), self.img.shape[0])

                        inImage = ((0 <= left < self.img.shape[1])
                            and (0 <= right < self.img.shape[1])
                            and (0 <= bottom < self.img.shape[0])
                            and (0 <= top < self.img.shape[0]))

                        if inImage:
                            self.imageAxes.set_xlim(left, right)
                            self.imageAxes.set_ylim(bottom, top)

                        self.last_drag_x = x
                        self.last_drag_y = y

                # Remove old lines
                self.remove_image_lines(labels=["Red Dot"])
                # Draw cursor location in image using red cross lines.
                axis_size = 5

                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r', label="Red Dot")
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r', label="Red Dot")

                if self.function and len(self.function) == 2 and self.function[0] == "im_zoomin":
                    # draw rectangle

                    start_pt = self.function[1]
                    w = abs(start_pt[0] - x)
                    h = abs(start_pt[1] - y)
                    x = min(start_pt[0], x)
                    y = min(start_pt[1], y)
                    ax.add_patch(patches.Rectangle((x, y), w, h,
                                                linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted',
                                                label="zoom_region"))

                self.imageCanvas.draw_idle()

    def imageReleased(self, event):
        x = event.xdata
        y = event.ydata

        if event.button != MouseButton.LEFT:
            return

        # If moved enough, switch to dragging mode
        move_threshold = 2.0  # pixels in data coords

        if event.inaxes == self.imageAxes:
            print("imageAxes clicked!")

            if self.mouse_pressed:
                if not self.dragging:
                    dx = abs(x - self.press_x)
                    dy = abs(y - self.press_y)
                    if dx > move_threshold or dy > move_threshold:
                        self.dragging = True

                if not self.dragging:
                    # print("imageAxes clicked!")
                    if self.doubleZoom.isChecked():
                        # print(f"doubleZoomMode: {self.doubleZoomGUI.doubleZoomMode}")
                        if self.doubleZoomGUI.doubleZoomMode:
                            # set self.doubleZoomMode = False
                            self.doubleZoomGUI.mouseClickBehavior(x, y)

                            self.center = (x, y)
                            self.refreshCenter(updateText=True)

                    else:
                        if self.function and self.function[0] == "im_zoomin":
                            self.function.append((x, y))
                            if len(self.function) == 3:
                                p1 = self.function[1]
                                p2 = self.function[2]
                                img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                                self.function = None
                                self.imgZoomInBtn.setChecked(False)
                                self.resizeImage(img_zoom)
                                self.refreshCenter()
                        else:
                            self.center = (x, y)
                            self.refreshCenter(updateText=True)

        elif event.inaxes == self.doubleZoomGUI.axes:
            print("doubleZoomGUI clicked!")

            if self.doubleZoom.isChecked():
                # if not self.doubleZoomGUI.doubleZoomMode:
                x, y = self.doubleZoomGUI.doubleZoomToOrigCoord(x, y)
                self.doubleZoomGUI.doubleZoomMode = True

                self.center = (x, y)
                self.refreshCenter(updateText=True)

        # Reset state
        self.mouse_pressed = False
        self.dragging = False
        self.press_x = None
        self.press_y = None
        self.last_drag_x = None
        self.last_drag_y = None

    def imgScrolled(self, event):
        if event.inaxes != self.imageAxes:
            return
        base_scale = 1.2  # zoom factor
        if event.button == 'up':   # scroll up to zoom in
            scale_factor = 1 / base_scale
        elif event.button == 'down':  # scroll down to zoom out
            scale_factor = base_scale
        else:
            return

        xdata, ydata = event.xdata, event.ydata
        xlim = self.imageAxes.get_xlim()
        ylim = self.imageAxes.get_ylim()

        new_width = (xlim[1] - xlim[0]) * scale_factor
        new_height = (ylim[1] - ylim[0]) * scale_factor

        relx = (xdata - xlim[0]) / (xlim[1] - xlim[0])
        rely = (ydata - ylim[0]) / (ylim[1] - ylim[0])

        self.imageAxes.set_xlim([xdata - new_width * relx,
                                 xdata + new_width * (1 - relx)])
        self.imageAxes.set_ylim([ydata - new_height * rely,
                                 ydata + new_height * (1 - rely)])
        self.imageCanvas.draw_idle()

    def updateImageMinMax(self, vmin=None, vmax=None):
        if vmin is not None:
            self.vmin = vmin
        if vmax is not None:
            self.vmax = vmax

        self.redrawImage()

    def ImageScaleChecked(self):
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

    def doubleZoomChecked(self):
        """
        Triggered when double zoom is checked
        """
        self.doubleZoomGUI.doubleZoomChecked(
            img=self.img,
            canv=self.imageCanvas,
            center=self.center,
            is_checked=self.doubleZoom.isChecked(),
            isLogScale=self.isLogScale,
            vmin=self.vmin,
            vmax=self.vmax
        )

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
        if self.imgZoomInBtn.isChecked():
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
        self.imgZoomInBtn.setChecked(False)
        img_zoom = [(0, self.img.shape[1]), (0, self.img.shape[0])]
        self.resizeImage(img_zoom)
        self.refreshCenter()
