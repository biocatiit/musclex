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
import csv
import os
import cv2
from datetime import datetime
from scipy.optimize import differential_evolution
from matplotlib.backend_bases import MouseButton
import matplotlib.patches as patches

from PySide6.QtWidgets import (
    QDialog,
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
    
    def __init__(
        self,
        parent,
        img,
        vmin=None,
        vmax=None,
        silver_behenate=5.83803,
        objective_Q=1.0,
        objective_nphi=720,
        objective_alpha=0.5,
        objective_bg_k=3.0,
    ):
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
        self.applied = False  # Flag to track if Apply has been clicked
        
        # Calibration results
        self.center = None
        self.radius = None
        self.scale = None
        self.silver_behenate = silver_behenate

        # Objective-based center/radius refinement (pixel-space)
        self.objective_Q = float(objective_Q)  # band half-width (pixels)
        self.objective_nphi = int(objective_nphi)  # angular samples
        self.objective_alpha = float(objective_alpha)  # background penalty weight
        self.objective_bg_k = float(objective_bg_k)  # background offsets multiplier (k*Q)
        self.optimization_history = []  # record objective process (list of dicts)
        
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
        
        # self.optionsLayout.addWidget(self.calibrationGroup)
        
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
        
        # Button layout (Cancel - Apply - Done)
        buttonLayout = QHBoxLayout()
        buttonLayout.addStretch()
        
        # Set consistent height for all buttons
        btn_height = 28
        
        self.cancelBtn = QPushButton("Cancel")
        self.cancelBtn.setFixedHeight(btn_height)
        self.cancelBtn.clicked.connect(self.reject)
        buttonLayout.addWidget(self.cancelBtn)
        
        self.applyBtn = QPushButton("Apply (Preview)")
        self.applyBtn.setFixedHeight(btn_height)
        self.applyBtn.clicked.connect(self.applyCalibration)
        self.applyBtn.setEnabled(False)
        buttonLayout.addWidget(self.applyBtn)
        
        self.doneBtn = QPushButton("Done")
        self.doneBtn.setFixedHeight(btn_height)
        self.doneBtn.clicked.connect(self.onAccept)
        self.doneBtn.setEnabled(False)  # Disabled until Apply is clicked
        buttonLayout.addWidget(self.doneBtn)
        
        buttonLayout.addStretch()
        self.mainLayout.addLayout(buttonLayout)
        
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
            self.applyBtn.setEnabled(False)
            self.pointsCountLabel.setStyleSheet("QLabel { color: orange; }")
        else:
            self.applyBtn.setEnabled(True)
            self.pointsCountLabel.setStyleSheet("QLabel { color: green; }")
        
        # Done button is only enabled after Apply is clicked
        self.doneBtn.setEnabled(self.applied)
    
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

    # ===================== Objective-based refinement (NEW) =====================

    def _bilinear_sample(self, img: np.ndarray, x: np.ndarray, y: np.ndarray) -> np.ndarray:
        """
        Vectorized bilinear sampling.
        x, y: float arrays in image coordinates (x=col, y=row).
        Returns sampled intensities; out-of-bounds -> 0.
        """
        h, w = img.shape[:2]
        x = np.asarray(x, dtype=np.float64)
        y = np.asarray(y, dtype=np.float64)

        x0 = np.floor(x).astype(np.int64)
        y0 = np.floor(y).astype(np.int64)
        x1 = x0 + 1
        y1 = y0 + 1

        m = (x0 >= 0) & (y0 >= 0) & (x1 < w) & (y1 < h)
        out = np.zeros_like(x, dtype=np.float64)
        if not np.any(m):
            return out

        xm = x[m]
        ym = y[m]
        x0m = x0[m]
        y0m = y0[m]
        x1m = x1[m]
        y1m = y1[m]

        fx = xm - x0m
        fy = ym - y0m

        Ia = img[y0m, x0m].astype(np.float64)
        Ib = img[y0m, x1m].astype(np.float64)
        Ic = img[y1m, x0m].astype(np.float64)
        Id = img[y1m, x1m].astype(np.float64)

        out[m] = (
            Ia * (1 - fx) * (1 - fy)
            + Ib * fx * (1 - fy)
            + Ic * (1 - fx) * fy
            + Id * fx * fy
        )
        return out

    def _circle_band_objective(self, center, radius, Q: float) -> float:
        """
        Contrast objective (maximize):
            mean(signal_band) - alpha * mean(background_bands)

        This reduces bias from strong radial background gradients compared to
        using absolute intensity sum alone. Outlier angles are rejected using
        MAD (Median Absolute Deviation) filtering.
        """
        cx, cy = float(center[0]), float(center[1])
        r = float(radius)
        if not np.isfinite(cx + cy + r):
            return -np.inf
        if Q <= 0:
            return -np.inf
        # Need room for background sampling at +/- (bg_k * Q)
        if r <= max(1e-6, float(self.objective_bg_k) * Q + 1.0):
            return -np.inf

        nphi = max(90, int(self.objective_nphi))
        phi = np.linspace(0.0, 2.0 * np.pi, nphi, endpoint=False)
        c = np.cos(phi)
        s = np.sin(phi)

        offsets_signal = np.array([-1.0, -0.5, 0.0, 0.5, 1.0], dtype=np.float64) * float(Q)
        offsets_bg = (
            np.array([-float(self.objective_bg_k), +float(self.objective_bg_k)], dtype=np.float64) * float(Q)
        )

        def sample_offsets_2d(offsets: np.ndarray) -> np.ndarray:
            """Returns array of shape (n_offsets, nphi)"""
            rr = r + offsets[:, None]  # (nrad, nphi)
            x = cx + rr * c[None, :]
            y = cy + rr * s[None, :]
            samples = self._bilinear_sample(self.img, x.ravel(), y.ravel())
            return samples.reshape(len(offsets), nphi)

        sig_2d = sample_offsets_2d(offsets_signal)  # (5, nphi)
        bg_2d = sample_offsets_2d(offsets_bg)       # (2, nphi)

        if sig_2d.size == 0 or bg_2d.size == 0:
            return -np.inf

        # Compute per-angle (phi) mean across radial offsets
        sig_per_phi = np.mean(sig_2d, axis=0)  # (nphi,)
        bg_per_phi = np.mean(bg_2d, axis=0)    # (nphi,)

        # MAD-based outlier rejection: identify "good" phi angles using signal intensity
        median_sig = np.median(sig_per_phi)
        mad_sig = np.median(np.abs(sig_per_phi - median_sig))

        if mad_sig < 1e-10:
            # All values nearly identical, no outliers to reject
            good_mask = np.ones(nphi, dtype=bool)
        else:
            k = 3.0  # Number of MADs from median to allow
            good_mask = np.abs(sig_per_phi - median_sig) <= k * mad_sig

        # Apply same mask to both signal and background for consistency
        sig_filtered = sig_per_phi[good_mask]
        bg_filtered = bg_per_phi[good_mask]

        if sig_filtered.size == 0 or bg_filtered.size == 0:
            return -np.inf

        sig_mean = float(np.mean(sig_filtered))
        bg_mean = float(np.mean(bg_filtered))
        return sig_mean - float(self.objective_alpha) * bg_mean

    def _record_eval(self, phase: str, center, radius, delta: float, obj: float, accepted: bool, best_obj: float):
        self.optimization_history.append(
            {
                "phase": phase,  # "center" or "radius"
                "center": [float(center[0]), float(center[1])],
                "radius": float(radius),
                "delta": float(delta),
                "objective": float(obj),
                "accepted": bool(accepted),
                "best_objective": float(best_obj),
            }
        )

    def _refine_center(self, center, radius, Q: float, delta0=0.1, shrink=2.0, delta_min=1e-3):
        """
        Center refinement:
        - Search 8 neighbors of +/- delta (delta starts at delta0).
        - If improvement found, move to best neighbor and continue with same delta.
        - Otherwise, delta /= shrink and repeat.
        - Stop when no improvement and delta < delta_min.
        """
        best_center = [float(center[0]), float(center[1])]
        r = float(radius)
        best_obj = self._circle_band_objective(best_center, r, Q)

        delta = float(delta0)
        dirs = np.array(
            [
                [1, 0],
                [-1, 0],
                [0, 1],
                [0, -1],
                [1, 1],
                [1, -1],
                [-1, 1],
                [-1, -1],
            ],
            dtype=np.float64,
        )

        improved_any = False
        while True:
            moved = False
            # Evaluate all 8 neighbors; take best if any improves.
            cand_best_center = None
            cand_best_obj = best_obj

            for dxy in dirs:
                cand_center = [best_center[0] + float(dxy[0]) * delta, best_center[1] + float(dxy[1]) * delta]
                obj = self._circle_band_objective(cand_center, r, Q)
                accepted = obj > cand_best_obj
                if accepted:
                    cand_best_obj = obj
                    cand_best_center = cand_center
                self._record_eval("center", cand_center, r, delta, obj, accepted, cand_best_obj)

            if cand_best_center is not None:
                best_center = cand_best_center
                best_obj = cand_best_obj
                moved = True
                improved_any = True

            if moved:
                continue

            # No improvement at this delta
            if delta <= float(delta_min):
                break
            delta = delta / float(shrink)

        return best_center, best_obj, improved_any

    def _refine_radius(self, center, radius, Q: float, delta0=0.01, shrink=2.0, delta_min=1e-4):
        """
        Radius refinement:
        - Search 2 neighbors (+/- delta).
        - If improvement found, move and continue with same delta.
        - Otherwise, delta /= shrink and repeat.
        - Stop when no improvement and delta < delta_min.
        """
        c = [float(center[0]), float(center[1])]
        best_r = float(radius)
        best_obj = self._circle_band_objective(c, best_r, Q)

        delta = float(delta0)
        improved_any = False
        while True:
            moved = False
            cand_best_r = None
            cand_best_obj = best_obj

            for dr in (-delta, +delta):
                cand_r = best_r + float(dr)
                obj = self._circle_band_objective(c, cand_r, Q)
                accepted = obj > cand_best_obj
                if accepted:
                    cand_best_obj = obj
                    cand_best_r = cand_r
                self._record_eval("radius", c, cand_r, delta, obj, accepted, cand_best_obj)

            if cand_best_r is not None:
                best_r = cand_best_r
                best_obj = cand_best_obj
                moved = True
                improved_any = True

            if moved:
                continue

            if delta <= float(delta_min):
                break
            delta = delta / float(shrink)

        return best_r, best_obj, improved_any

    def _optimize_center_radius(self, center, radius, Q: float, max_cycles=25):
        """
        Global optimization using differential evolution to find optimal center and radius.
        This avoids local minima issues that depend on user-selected points.

        Parameters
        ----------
        center : list or tuple
            Initial center estimate [x, y] (used to define search bounds).
        radius : float
            Initial radius estimate (used to define search bounds).
        Q : float
            Half-width of the signal band (in pixels).
        max_cycles : int
            Not directly used; controls maxiter indirectly for compatibility.

        Returns
        -------
        opt_center : list
            Optimized center [x, y].
        opt_radius : float
            Optimized radius.
        opt_obj : float
            Best objective value found.
        """
        self.optimization_history = []

        # Record initial state
        init_obj = self._circle_band_objective(center, radius, Q)
        self._record_eval("init", list(center), radius, 0.0, init_obj, True, init_obj)

        # Define search bounds around initial estimate
        center_range = 10.0  # pixels: search +/- 10 pixels from initial center
        radius_range = 0.10  # fraction: search +/- 10% of initial radius

        bounds = [
            (center[0] - center_range, center[0] + center_range),  # center_x
            (center[1] - center_range, center[1] + center_range),  # center_y
            (radius * (1 - radius_range), radius * (1 + radius_range)),  # radius
        ]

        # Counter for recording evaluations
        self._de_eval_count = 0
        self._de_best_obj = -np.inf

        def neg_objective(params):
            """Negative objective for minimization (DE minimizes)."""
            c = [params[0], params[1]]
            r = params[2]
            obj = self._circle_band_objective(c, r, Q)

            # Record evaluation
            self._de_eval_count += 1
            accepted = obj > self._de_best_obj
            if accepted:
                self._de_best_obj = obj
            self._record_eval("DE", c, r, 0.0, obj, accepted, self._de_best_obj)

            return -obj  # Negate for minimization

        # Run differential evolution
        result = differential_evolution(
            neg_objective,
            bounds,
            maxiter=max_cycles * 10,  # More iterations for global search
            seed=42,  # Reproducibility
            polish=True,  # Local refinement at end (uses L-BFGS-B)
            updating='deferred',  # Better for non-noisy objectives
            workers=1,  # Single-threaded (GUI compatibility)
            disp=False,
        )

        opt_center = [result.x[0], result.x[1]]
        opt_radius = result.x[2]
        opt_obj = -result.fun  # Un-negate

        # Record final result
        self._record_eval("final", opt_center, opt_radius, 0.0, opt_obj, True, opt_obj)

        return opt_center, opt_radius, opt_obj

    def _get_history_export_dir(self):
        """
        Choose a directory to export the optimization history CSV.
        Prefer the calibration file directory (if available), otherwise cwd.
        """
        parent = self.parent()
        if parent is not None and hasattr(parent, "calFile"):
            cal_file = getattr(parent, "calFile", None)
            if cal_file:
                return os.path.dirname(str(cal_file))
        return os.getcwd()

    def _export_history_csv(self):
        """
        Export optimization history to a CSV file for analysis.
        """
        if not self.optimization_history:
            return None

        export_dir = self._get_history_export_dir()
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"manual_calibration_history_{timestamp}.csv"
        out_path = os.path.join(export_dir, filename)

        fieldnames = [
            "eval_index",
            "phase",
            "center_x",
            "center_y",
            "radius",
            "delta",
            "objective",
            "accepted",
            "best_objective",
        ]

        try:
            with open(out_path, "w", newline="") as f:
                writer = csv.DictWriter(f, fieldnames=fieldnames)
                writer.writeheader()
                for i, row in enumerate(self.optimization_history):
                    writer.writerow(
                        {
                            "eval_index": i,
                            "phase": row.get("phase"),
                            "center_x": row.get("center", [None, None])[0],
                            "center_y": row.get("center", [None, None])[1],
                            "radius": row.get("radius"),
                            "delta": row.get("delta"),
                            "objective": row.get("objective"),
                            "accepted": row.get("accepted"),
                            "best_objective": row.get("best_objective"),
                        }
                    )
        except OSError as exc:
            print_log(f"Failed to export optimization history: {exc}")
            return None

        return out_path

    # ===================== end objective-based refinement =====================
    
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

        # Step 4 (NEW): Objective-based refinement of center & radius in pixel-space
        opt_center, opt_radius, opt_obj = self._optimize_center_radius(
            self.center, self.radius, Q=float(self.objective_Q), max_cycles=25
        )
        self.center = opt_center
        self.radius = opt_radius

        self.scale = self.radius * self.silverBehenateSpinBox.value()

        history_path = self._export_history_csv()
        
        print_log(
            f"Final fit (after objective opt) - Center: {self.center}, Radius: {self.radius}, "
            f"Scale: {self.scale}, Obj: {opt_obj:.6g}, Evals: {len(self.optimization_history)}"
        )
        if history_path:
            print_log(f"Optimization history exported: {history_path}")
        
        # Update display
        self.displayCalibrationResults()
        
        # Enable Done button after Apply is clicked
        self.applied = True
        self.doneBtn.setEnabled(True)
        
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
            'silver_behenate': self.silverBehenateSpinBox.value(),
            'optimization_history': self.optimization_history,
            'objective_Q': self.objective_Q,
            'objective_nphi': self.objective_nphi,
            'objective_alpha': self.objective_alpha,
            'objective_bg_k': self.objective_bg_k,
        }

