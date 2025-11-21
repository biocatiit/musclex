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

from .widgets.image_viewer_widget import ImageViewerWidget

def print_log(log_str):
    now = datetime.now()
    now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"{now_str}: {log_str}")


class SetCentDialog(QDialog):
    def __init__(self,
                parent,
                img,
                center,
                inv_transform=None,
                isLogScale=False,
                vmin=0,
                vmax=1
        ):
        super().__init__()
        self.setModal(True)
        self.setWindowTitle("Set Center")
        self.img = img
        self.center = center
        self.inv_transform = inv_transform
        self.isLogScale = isLogScale
        self.vmin = vmin
        self.vmax = vmax

        x, y = self.center

        # Create ImageViewerWidget with integrated display panel
        # (tool_manager is created internally, includes zoom_rectangle tool by default)
        self.imageViewer = ImageViewerWidget(parent=self, show_display_panel=True)
        

        
        # Quick access to axes and canvas for drawing center lines
        self.imageAxes = self.imageViewer.axes
        self.imageCanvas = self.imageViewer.canvas
        self.imageFigure = self.imageViewer.figure
        
        # Display the image with initial settings
        self.imageViewer.display_image(self.img, vmin, vmax, isLogScale)
        
        # Draw center crosshair
        self.imageAxes.axvline(x, color='y', label="Cross Center Yellow")
        self.imageAxes.axhline(y, color='y', label="Cross Center Yellow")
        self.imageCanvas.draw()

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
        self.setCenterLayout.addWidget(QLabel("X (Original coords): "), centerLayoutRowIndex, 0, 1, 2)
        self.setCenterLayout.addWidget(self.xInput, centerLayoutRowIndex, 2, 1, 2)
        self.setCenterLayout.addWidget(QLabel("px"), centerLayoutRowIndex, 4, 1, 1)
        centerLayoutRowIndex += 1
        self.setCenterLayout.addWidget(QLabel("Y (Original coords): "), centerLayoutRowIndex, 0, 1, 2)
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
        
        # Add integrated display panel from ImageViewerWidget
        self.optionsLayout.addWidget(self.imageViewer.display_panel)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.setCenterGroup)
        self.optionsLayout.addStretch()

        self.scrollAreaImg = QScrollArea()
        self.scrollAreaImg.setWidgetResizable(True)
        self.imageLayout.addWidget(self.scrollAreaImg)
        self.scrollAreaImg.setWidget(self.imageViewer.canvas)

        self.scrollAreaImg.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        self.scrollAreaImg.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        self.imageLayout.addLayout(self.optionsLayout)
        self.mainLayout.addWidget(self.buttonBox)
        self.mainLayout.setAlignment(self.buttonBox, Qt.AlignCenter)

        self.imageViewer.canvas.setMinimumSize(800, 600)

        self.setMinimumSize(700, 500)
        self.resize(1200, 1000 // 4 * 3)

        self.imageFigure.tight_layout()
        self.imageCanvas.draw()

        self.createConnections()

    def createConnections(self):
        # Connect to canvas click signal (only emitted when no tool handles it)
        self.imageViewer.canvasClicked.connect(self.handle_canvas_click)

        # Connect display panel signals to update internal state
        self.imageViewer.display_panel.intensityChanged.connect(self._on_intensity_changed)
        self.imageViewer.display_panel.logScaleChanged.connect(self._on_log_scale_changed)
        
        # Connect zoom in button from display panel
        self.imageViewer.display_panel.zoomInRequested.connect(self.imageZoomInToggle)
        
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
            # Check if zoom tool is active and deactivate it
            if self.imageViewer.tool_manager.is_tool_active('zoom_rectangle'):
                self.imageViewer.tool_manager.deactivate_tool('zoom_rectangle')
                # Uncheck button via display_panel API
                self.imageViewer.display_panel.set_zoom_in_checked(False)
            # Prevent closing dialog with keyboard.
            return

        super().keyPressEvent(event)

    def handle_canvas_click(self, event):
        """
        Handle canvas click for setting center.
        This is only called when no tool has handled the event.
        """
        if event.button != MouseButton.LEFT:
            return

        x = event.xdata
        y = event.ydata

        if event.inaxes == self.imageAxes and x is not None and y is not None:
            # Convert to original coordinates if transformation exists
            if self.inv_transform is not None:
                import numpy as np
                point = np.array([x, y, 1])
                orig_point = self.inv_transform @ point
                self.center = (orig_point[0], orig_point[1])
            else:
                self.center = (x, y)
            self.refreshCenter(updateText=True)

    def _on_intensity_changed(self, vmin, vmax):
        """Handle intensity changes from display panel"""
        self.vmin = vmin
        self.vmax = vmax
        # ImageViewerWidget will automatically update via update_display_settings()
        # which preserves zoom and overlays (like center crosshair)
    
    def _on_log_scale_changed(self, log_scale):
        """Handle log scale changes from display panel"""
        self.isLogScale = log_scale
        # ImageViewerWidget will automatically update via update_display_settings()
        # which preserves zoom and overlays (like center crosshair)

    def redrawImage(self):
        """Redraw image with current settings (called after refreshCenter)"""
        self.imageViewer.display_image(self.img, self.vmin, self.vmax, self.isLogScale)


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
        self.remove_image_lines(labels=["Cross Center Yellow"])

        # Draw new lines
        self.imageAxes.axvline(x, color='y', label="Cross Center Yellow")
        self.imageAxes.axhline(y, color='y', label="Cross Center Yellow")

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

    def imageZoomInToggle(self):
        """Toggle zoom tool on/off based on button state from DisplayOptionsPanel"""
        zoom_btn = self.imageViewer.display_panel.zoomInBtn
        if zoom_btn.isChecked():
            # Button is checked - activate the zoom tool
            self.imageViewer.tool_manager.activate_tool('zoom_rectangle')
        else:
            # Button is unchecked - deactivate the zoom tool
            self.imageViewer.tool_manager.deactivate_current_tool()