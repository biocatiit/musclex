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
import cv2
from datetime import datetime
from matplotlib.backend_bases import MouseButton
import matplotlib.patches as patches

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
    QDoubleSpinBox,
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
    
    def __init__(self, parent, img, vmin=None, vmax=None, silver_behenate=5.83803):
        """
        Initialize the manual calibration dialog.
        
        Args:
            parent: Parent widget
            img: Calibration image (numpy array)
            vmin: Minimum intensity for display
            vmax: Maximum intensity for display
            silver_behenate: Silver Behenate d-spacing value
        """
        super().__init__(parent)
        self.setModal(True)
        self.setWindowTitle("Manual Calibration - Select Points on Ring")
        
        self.img = img
        self.selected_points = []  # List of (x, y) tuples
        self.point_artists = []  # List of matplotlib artists for point markers
        self.refined_points = []  # List of refined (x, y) tuples
        self.refined_artists = []  # List of matplotlib artists for refined markers
        self.calibration_circle = None  # Matplotlib circle patch
        self.calibration_center_artist = None  # Center point marker
        self.radial_lines = []  # List of radial line artists
        
        # Calibration results
        self.center = None
        self.radius = None
        self.scale = None
        self.silver_behenate = silver_behenate
        
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
        
        # Create ImageViewerWidget with integrated display panel and double zoom
        self.imageViewer = ImageViewerWidget(parent=self, show_display_panel=True, show_double_zoom=True)
        
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
        
        # Calibration parameters group
        self.calibrationGroup = QGroupBox("Calibration Parameters")
        calibrationLayout = QGridLayout(self.calibrationGroup)
        
        calibrationLayout.addWidget(QLabel("Silver Behenate d-spacing:"), 0, 0)
        self.silverBehenateSpinBox = QDoubleSpinBox()
        self.silverBehenateSpinBox.setDecimals(5)
        self.silverBehenateSpinBox.setRange(0.0, 100.0)
        self.silverBehenateSpinBox.setValue(self.silver_behenate)
        self.silverBehenateSpinBox.setSingleStep(0.1)
        calibrationLayout.addWidget(self.silverBehenateSpinBox, 0, 1)
        
        self.optionsLayout.addWidget(self.calibrationGroup)
        
        # Points list group
        self.pointsGroup = QGroupBox("Selected Points")
        pointsLayout = QVBoxLayout(self.pointsGroup)
        
        self.pointsCountLabel = QLabel("Points selected: 0")
        pointsLayout.addWidget(self.pointsCountLabel)
        
        self.pointsList = QListWidget()
        self.pointsList.setMaximumHeight(150)
        pointsLayout.addWidget(self.pointsList)
        
        buttonsLayout = QHBoxLayout()
        self.clearPointsBtn = QPushButton("Clear All Points")
        self.clearPointsBtn.clicked.connect(self.clearAllPoints)
        buttonsLayout.addWidget(self.clearPointsBtn)
        
        self.applyBtn = QPushButton("Apply (Preview)")
        self.applyBtn.clicked.connect(self.applyCalibration)
        self.applyBtn.setEnabled(False)
        buttonsLayout.addWidget(self.applyBtn)
        
        pointsLayout.addLayout(buttonsLayout)
        
        self.optionsLayout.addWidget(self.pointsGroup)
        
        # Calibration results group
        self.resultsGroup = QGroupBox("Calibration Results")
        resultsLayout = QVBoxLayout(self.resultsGroup)
        
        self.centerLabel = QLabel("Center: Not calculated")
        self.radiusLabel = QLabel("Radius: Not calculated")
        self.scaleLabel = QLabel("Scale: Not calculated")
        
        resultsLayout.addWidget(self.centerLabel)
        resultsLayout.addWidget(self.radiusLabel)
        resultsLayout.addWidget(self.scaleLabel)
        
        self.optionsLayout.addWidget(self.resultsGroup)
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
        
        # Enable/disable buttons based on point count
        if count < 5:
            self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(False)
            self.applyBtn.setEnabled(False)
            self.pointsCountLabel.setStyleSheet("QLabel { color: orange; }")
        else:
            self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(True)
            self.applyBtn.setEnabled(True)
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
    
    def refine_point_on_ring(self, center, user_point, search_radius=30):
        """
        Refine a user-clicked point by finding the local intensity maximum along the radial direction.
        
        Args:
            center: fitted center coordinates [x, y]
            user_point: user-clicked point [x, y]
            search_radius: search range around the user point (pixels)
        
        Returns:
            refined_point: [x, y] coordinates of the intensity maximum
        """
        # Calculate radial direction from center to user point
        dx = user_point[0] - center[0]
        dy = user_point[1] - center[1]
        distance = np.sqrt(dx**2 + dy**2)
        
        if distance == 0:
            return user_point
        
        # Normalize direction vector
        dir_x = dx / distance
        dir_y = dy / distance
        
        # Sample along the radial direction
        start_dist = distance - search_radius
        end_dist = distance + search_radius
        num_samples = 2 * search_radius + 1
        
        radial_distances = np.linspace(start_dist, end_dist, num_samples)
        intensities = []
        sample_points = []
        
        for dist in radial_distances:
            # Calculate point coordinates along the radial direction
            x = center[0] + dir_x * dist
            y = center[1] + dir_y * dist
            
            # Check bounds
            if 0 <= x < self.img.shape[1] and 0 <= y < self.img.shape[0]:
                # Bilinear interpolation for sub-pixel accuracy
                x0, y0 = int(np.floor(x)), int(np.floor(y))
                x1, y1 = min(x0 + 1, self.img.shape[1] - 1), min(y0 + 1, self.img.shape[0] - 1)
                
                fx, fy = x - x0, y - y0
                
                intensity = (self.img[y0, x0] * (1 - fx) * (1 - fy) +
                            self.img[y0, x1] * fx * (1 - fy) +
                            self.img[y1, x0] * (1 - fx) * fy +
                            self.img[y1, x1] * fx * fy)
                
                intensities.append(intensity)
                sample_points.append([x, y])
            else:
                intensities.append(0)
                sample_points.append([x, y])
        
        # Find the maximum intensity point
        if len(intensities) > 0:
            max_idx = np.argmax(intensities)
            refined_point = sample_points[max_idx]
            return refined_point
        else:
            return user_point
    
    def applyCalibration(self):
        """Apply calibration and show preview"""
        if len(self.selected_points) < 5:
            QMessageBox.warning(
                self,
                "Insufficient Points",
                "Please select at least 5 points on the calibration ring."
            )
            return
        
        print_log(f"Applying calibration with {len(self.selected_points)} points")
        
        # Step 1: Initial fit using user-clicked points
        user_points_array = np.array(self.selected_points, dtype=np.float32)
        initial_center, initial_radius, _ = cv2.fitEllipse(user_points_array)
        initial_center = [initial_center[0], initial_center[1]]
        
        print_log(f"Initial fit - Center: {initial_center}, Radius: {initial_radius}")
        
        # Step 2: Refine each point by finding intensity maximum along radial direction
        self.refined_points = []
        for pt in self.selected_points:
            refined_pt = self.refine_point_on_ring(initial_center, pt, search_radius=30)
            self.refined_points.append(refined_pt)
        
        print_log(f"Refined {len(self.refined_points)} points")
        
        # Step 3: Refit circle using refined points
        refined_points_array = np.array(self.refined_points, dtype=np.float32)
        final_center, final_radius, _ = cv2.fitEllipse(refined_points_array)
        
        self.center = [final_center[0], final_center[1]]
        self.radius = (final_radius[0] + final_radius[1]) / 4.0
        self.scale = self.radius * self.silverBehenateSpinBox.value()
        
        print_log(f"Final fit - Center: {self.center}, Radius: {self.radius}, Scale: {self.scale}")
        
        # Update display
        self.displayCalibrationResults()
        
        QMessageBox.information(
            self,
            "Calibration Applied",
            f"Calibration preview updated!\n\n"
            f"Center: ({self.center[0]:.2f}, {self.center[1]:.2f})\n"
            f"Radius: {self.radius:.2f} px\n"
            f"Scale: {self.scale:.2f}\n\n"
            f"Click 'Done' to accept these results."
        )
    
    def displayCalibrationResults(self):
        """Display calibration results on the image"""
        # Clear previous calibration overlays
        self.clearCalibrationOverlays()
        
        if self.center is None or self.radius is None:
            return
        
        # Draw refined points (blue circles)
        for pt in self.refined_points:
            artist = self.imageAxes.plot(pt[0], pt[1], 'bo', markersize=8, markeredgewidth=2)[0]
            self.refined_artists.append(artist)
        
        # Draw radial lines from center to refined points (green dashed)
        for pt in self.refined_points:
            line = self.imageAxes.plot([self.center[0], pt[0]], [self.center[1], pt[1]], 
                                       'g--', linewidth=0.5, alpha=0.5)[0]
            self.radial_lines.append(line)
        
        # Draw calibration circle (red dashed)
        self.calibration_circle = patches.Circle(
            self.center, 
            self.radius, 
            linewidth=2, 
            edgecolor='r', 
            facecolor='none', 
            linestyle='dotted',
            label='Fitted Circle'
        )
        self.imageAxes.add_patch(self.calibration_circle)
        
        # Draw center point (red circle)
        self.calibration_center_artist = self.imageAxes.plot(
            self.center[0], self.center[1], 'ro', markersize=6
        )[0]
        
        # Update results labels
        self.centerLabel.setText(f"Center: ({self.center[0]:.2f}, {self.center[1]:.2f})")
        self.radiusLabel.setText(f"Radius: {self.radius:.2f} px")
        self.scaleLabel.setText(f"Scale: {self.scale:.2f}")
        
        # Redraw canvas
        self.imageCanvas.draw_idle()
        
        print_log("Calibration results displayed")
    
    def clearCalibrationOverlays(self):
        """Clear all calibration overlay elements from the image"""
        # Remove refined point artists
        for artist in self.refined_artists:
            artist.remove()
        self.refined_artists.clear()
        
        # Remove radial lines
        for line in self.radial_lines:
            line.remove()
        self.radial_lines.clear()
        
        # Remove calibration circle
        if self.calibration_circle is not None:
            self.calibration_circle.remove()
            self.calibration_circle = None
        
        # Remove center artist
        if self.calibration_center_artist is not None:
            self.calibration_center_artist.remove()
            self.calibration_center_artist = None
    
    def getCalibrationResults(self):
        """
        Get calibration results.
        
        Returns:
            dict with keys: 'selected_points', 'refined_points', 'center', 'radius', 'scale'
        """
        return {
            'selected_points': self.selected_points,
            'refined_points': self.refined_points,
            'center': self.center,
            'radius': self.radius,
            'scale': self.scale,
            'silver_behenate': self.silverBehenateSpinBox.value()
        }

