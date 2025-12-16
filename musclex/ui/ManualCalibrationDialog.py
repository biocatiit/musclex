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
from datetime import datetime
from matplotlib.backend_bases import MouseButton

from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QVBoxLayout,
    QHBoxLayout,
    QGridLayout,
    QLabel,
    QPushButton,
    QGroupBox,
    QListWidget,
    QScrollArea,
    QMessageBox,
)
from PySide6.QtCore import Qt

from .widgets.image_viewer_widget import ImageViewerWidget


def print_log(log_str):
    now_str = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"{now_str}: {log_str}")


class ManualCalibrationDialog(QDialog):
    """
    Dialog for manual calibration point selection.
    
    Users can click on the calibration ring to select points,
    which are then used to fit a circle and refine the calibration.
    """
    
    def __init__(self, parent, img, vmin=None, vmax=None):
        """
        Initialize the manual calibration dialog.
        
        Args:
            parent: Parent widget
            img: Calibration image (numpy array)
            vmin: Minimum intensity for display
            vmax: Maximum intensity for display
        """
        super().__init__(parent)
        self.setModal(True)
        self.setWindowTitle("Manual Calibration - Select Points on Ring")
        
        self.img = img
        self.selected_points = []  # List of (x, y) tuples
        self.point_artists = []  # List of matplotlib artists for point markers
        
        # Auto-calculate vmin/vmax if not provided
        if vmin is None:
            vmin = float(np.min(img))
        if vmax is None:
            vmax = float(np.max(img) * 0.2)
        
        self.vmin = vmin
        self.vmax = vmax
        
        # Create main layout
        self.mainLayout = QVBoxLayout(self)
        
        # Create horizontal layout for image and options
        self.contentLayout = QHBoxLayout()
        
        # Create ImageViewerWidget with integrated display panel
        self.imageViewer = ImageViewerWidget(parent=self, show_display_panel=True)
        
        # Quick access to components
        self.imageAxes = self.imageViewer.axes
        self.imageCanvas = self.imageViewer.canvas
        self.imageFigure = self.imageViewer.figure
        
        # Display the image
        self.imageViewer.display_image(self.img)
        
        # Set initial display settings
        self.imageViewer.display_panel.set_intensity_values(vmin, vmax)
        
        # Add image to scroll area
        self.scrollAreaImg = QScrollArea()
        self.scrollAreaImg.setWidgetResizable(True)
        self.scrollAreaImg.setWidget(self.imageViewer.canvas)
        self.scrollAreaImg.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        self.scrollAreaImg.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOn)
        
        # Create options panel on the right
        self.optionsLayout = QVBoxLayout()
        
        # Add integrated display panel
        self.optionsLayout.addWidget(self.imageViewer.display_panel)
        self.optionsLayout.addSpacing(10)
        
        # Instructions group
        self.instructionsGroup = QGroupBox("Instructions")
        instructionsLayout = QVBoxLayout(self.instructionsGroup)
        instructionsText = QLabel(
            "1. Click on the calibration ring to select points\n"
            "2. Select at least 5 points around the ring\n"
            "3. Points will be shown as red × markers\n"
            "4. Click 'Done' to fit the circle\n"
            "5. Blue circles will show refined positions"
        )
        instructionsText.setWordWrap(True)
        instructionsLayout.addWidget(instructionsText)
        self.optionsLayout.addWidget(self.instructionsGroup)
        
        # Points list group
        self.pointsGroup = QGroupBox("Selected Points")
        pointsLayout = QVBoxLayout(self.pointsGroup)
        
        self.pointsCountLabel = QLabel("Points selected: 0")
        pointsLayout.addWidget(self.pointsCountLabel)
        
        self.pointsList = QListWidget()
        self.pointsList.setMaximumHeight(200)
        pointsLayout.addWidget(self.pointsList)
        
        self.clearPointsBtn = QPushButton("Clear All Points")
        self.clearPointsBtn.clicked.connect(self.clearAllPoints)
        pointsLayout.addWidget(self.clearPointsBtn)
        
        self.optionsLayout.addWidget(self.pointsGroup)
        self.optionsLayout.addStretch()
        
        # Add to content layout
        self.contentLayout.addWidget(self.scrollAreaImg, stretch=3)
        self.contentLayout.addLayout(self.optionsLayout, stretch=1)
        
        self.mainLayout.addLayout(self.contentLayout)
        
        # Button box
        self.buttonBox = QDialogButtonBox(
            QDialogButtonBox.Ok | QDialogButtonBox.Cancel,
            Qt.Horizontal,
            self
        )
        self.buttonBox.accepted.connect(self.onAccept)
        self.buttonBox.rejected.connect(self.reject)
        
        # Change OK button text to "Done"
        self.buttonBox.button(QDialogButtonBox.Ok).setText("Done")
        
        self.mainLayout.addWidget(self.buttonBox)
        self.mainLayout.setAlignment(self.buttonBox, Qt.AlignCenter)
        
        # Set minimum size
        self.imageViewer.canvas.setMinimumSize(800, 600)
        self.setMinimumSize(1000, 700)
        self.resize(1400, 900)
        
        self.imageFigure.tight_layout()
        self.imageCanvas.draw()
        
        # Create connections
        self.createConnections()
    
    def createConnections(self):
        """Set up signal connections"""
        # Connect to canvas click signal
        self.imageViewer.canvasClicked.connect(self.handleCanvasClick)
        
        # Connect display panel signals
        self.imageViewer.display_panel.intensityChanged.connect(self._onIntensityChanged)
        self.imageViewer.display_panel.logScaleChanged.connect(self._onLogScaleChanged)
        
        # Connect zoom button
        self.imageViewer.display_panel.zoomInRequested.connect(self.imageZoomInToggle)
        
        # Connect point list selection for deletion
        self.pointsList.itemDoubleClicked.connect(self.removeSelectedPoint)
    
    def handleCanvasClick(self, event):
        """
        Handle canvas click for point selection.
        Only called when no tool has handled the event.
        """
        if event.button != MouseButton.LEFT:
            return
        
        x = event.xdata
        y = event.ydata
        
        if event.inaxes == self.imageAxes and x is not None and y is not None:
            # Add point to list
            self.addPoint(x, y)
    
    def addPoint(self, x, y):
        """Add a point to the selection"""
        point = (x, y)
        self.selected_points.append(point)
        
        # Draw point on image (red X marker)
        artist = self.imageAxes.plot(x, y, 'rx', markersize=10, markeredgewidth=2)[0]
        self.point_artists.append(artist)
        
        # Add to list widget
        self.pointsList.addItem(f"Point {len(self.selected_points)}: ({x:.2f}, {y:.2f})")
        
        # Update count
        self.updatePointsCount()
        
        # Redraw canvas
        self.imageCanvas.draw_idle()
        
        print_log(f"Added point {len(self.selected_points)}: ({x:.2f}, {y:.2f})")
    
    def removeSelectedPoint(self, item):
        """Remove a selected point (double-click on list item)"""
        index = self.pointsList.row(item)
        
        if 0 <= index < len(self.selected_points):
            # Remove from data
            self.selected_points.pop(index)
            
            # Remove artist
            if index < len(self.point_artists):
                self.point_artists[index].remove()
                self.point_artists.pop(index)
            
            # Remove from list widget
            self.pointsList.takeItem(index)
            
            # Update remaining items
            for i in range(index, self.pointsList.count()):
                pt = self.selected_points[i]
                self.pointsList.item(i).setText(f"Point {i+1}: ({pt[0]:.2f}, {pt[1]:.2f})")
            
            # Update count
            self.updatePointsCount()
            
            # Redraw
            self.imageCanvas.draw_idle()
            
            print_log(f"Removed point at index {index}")
    
    def clearAllPoints(self):
        """Clear all selected points"""
        # Remove all artists
        for artist in self.point_artists:
            artist.remove()
        
        # Clear data
        self.selected_points.clear()
        self.point_artists.clear()
        
        # Clear list widget
        self.pointsList.clear()
        
        # Update count
        self.updatePointsCount()
        
        # Redraw
        self.imageCanvas.draw_idle()
        
        print_log("Cleared all points")
    
    def updatePointsCount(self):
        """Update the points count label"""
        count = len(self.selected_points)
        self.pointsCountLabel.setText(f"Points selected: {count}")
        
        # Enable/disable done button
        if count < 5:
            self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(False)
            self.pointsCountLabel.setStyleSheet("QLabel { color: orange; }")
        else:
            self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(True)
            self.pointsCountLabel.setStyleSheet("QLabel { color: green; }")
    
    def onAccept(self):
        """Handle OK/Done button click"""
        if len(self.selected_points) < 5:
            QMessageBox.warning(
                self,
                "Insufficient Points",
                "Please select at least 5 points on the calibration ring."
            )
            return
        
        self.accept()
    
    def getSelectedPoints(self):
        """Return the list of selected points"""
        return self.selected_points
    
    def _onIntensityChanged(self, vmin, vmax):
        """Handle intensity changes from display panel"""
        self.vmin = vmin
        self.vmax = vmax
    
    def _onLogScaleChanged(self, log_scale):
        """Handle log scale changes from display panel"""
        pass  # ImageViewerWidget handles this automatically
    
    def imageZoomInToggle(self):
        """Toggle zoom tool on/off"""
        zoom_btn = self.imageViewer.display_panel.zoomInBtn
        if zoom_btn.isChecked():
            self.imageViewer.tool_manager.activate_tool('zoom_rectangle')
        else:
            self.imageViewer.tool_manager.deactivate_current_tool()
    
    def keyPressEvent(self, event):
        """Handle key press events"""
        key = event.key()
        
        # Prevent Enter/Return from closing dialog
        if key in [Qt.Key_Return, Qt.Key_Enter]:
            return
        
        # Handle Escape to deactivate zoom tool
        if key == Qt.Key_Escape:
            if self.imageViewer.tool_manager.is_tool_active('zoom_rectangle'):
                self.imageViewer.tool_manager.deactivate_tool('zoom_rectangle')
                self.imageViewer.display_panel.set_zoom_in_checked(False)
            return
        
        # Handle Delete/Backspace to remove last point
        if key in [Qt.Key_Delete, Qt.Key_Backspace]:
            if len(self.selected_points) > 0:
                # Remove last point
                self.selected_points.pop()
                if len(self.point_artists) > 0:
                    self.point_artists[-1].remove()
                    self.point_artists.pop()
                if self.pointsList.count() > 0:
                    self.pointsList.takeItem(self.pointsList.count() - 1)
                self.updatePointsCount()
                self.imageCanvas.draw_idle()
                print_log("Removed last point")
            return
        
        super().keyPressEvent(event)

