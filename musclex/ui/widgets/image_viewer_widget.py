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

import matplotlib.pyplot as plt
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.colors import LogNorm, Normalize
from PySide6.QtWidgets import QWidget, QVBoxLayout
from PySide6.QtCore import Signal
from .display_options_panel import DisplayOptionsPanel
from .double_zoom_widget import DoubleZoomWidget
from ..tools.tool_manager import ToolManager
from ..tools.zoom_rectangle_tool import ZoomRectangleTool


class ImageViewerWidget(QWidget):
    """
    Reusable image viewer widget with built-in tool management.
    
    This widget provides:
    - Basic image display using matplotlib
    - Built-in ToolManager for modal interactions
    - Built-in DoubleZoomWidget for precise coordinate selection
    - Built-in non-modal features: pan (middle-click drag), scroll zoom
    - Event coordination: delegates to tool_manager first, then handles internally
    
    Architecture:
        - Self-contained: manages its own image data through _current_image
        - DoubleZoom receives image data via callback (decoupled from parent)
        - Backward compatible: can extract image from axes.images for legacy code
        - Tools access axes/canvas directly through tool_manager
        - Emits signals for external coordination (toolCompleted, etc.)
    
    Components:
        - figure, axes, canvas: Matplotlib components (accessible for advanced use)
        - tool_manager: Manages interactive tools (use tool_manager.* methods directly)
        - double_zoom: Provides zoomed view for precise clicks (access via double_zoom.*)
        - display_panel: Optional display controls
    
    Usage:
        # Basic viewer
        viewer = ImageViewerWidget()
        viewer.display_image(img, vmin, vmax, log_scale=False)
        
        # With display panel and double zoom
        viewer = ImageViewerWidget(show_display_panel=True, show_double_zoom=True)
        
        # Tool management
        viewer.tool_manager.register_tool('zoom', ZoomRectangleTool)
        viewer.tool_manager.activate_tool('zoom')
        
        # DoubleZoom (if not in display panel)
        viewer.enable_double_zoom(True)  # or use viewer.double_zoom.doubleZoomCheckbox
    
    Args:
        parent: Parent widget
        show_display_panel: Whether to create and display the DisplayOptionsPanel
        show_double_zoom: Whether to include double zoom in the display panel (requires show_display_panel=True)
    """
    
    # Signals for mouse events (raw events from matplotlib)
    mousePressed = Signal(object)    # Mouse button press event
    mouseMoved = Signal(object)      # Mouse motion event
    mouseReleased = Signal(object)   # Mouse button release event
    mouseScrolled = Signal(object)   # Mouse scroll event
    
    # Signals for specific interactions
    coordinatesChanged = Signal(float, float, float)  # x, y, value at mouse position
    canvasClicked = Signal(object)  # Emitted when canvas clicked without active tool handling it
    rightClickAt = Signal(float, float)  # Right-click at x, y
    leftClickAt = Signal(float, float)   # Left-click at x, y
    
    # Tool-related signals
    toolCompleted = Signal(str, object)  # (tool_name, result) when a tool completes
    
    # DoubleZoom signals
    preciseCoordinatesSelected = Signal(float, float)  # Precise coordinates from DoubleZoom
    
    # Display options signals
    displayOptionsChanged = Signal(dict)  # Display options (vmin, vmax, log_scale, colormap) changed
    
    def __init__(self, parent=None, show_display_panel=False, show_double_zoom=False):
        super().__init__(parent)
        
        # Matplotlib components (exposed for flexibility)
        self.figure = plt.figure()
        self.axes = self.figure.add_subplot(111)
        self.axes.set_aspect('equal', adjustable='box')
        self.canvas = FigureCanvas(self.figure)
        
        # Built-in tool manager (publicly accessible)
        self.tool_manager = ToolManager(self.axes, self.canvas)
        
        # Register built-in zoom rectangle tool
        self.tool_manager.register_tool('zoom_rectangle', 
            lambda axes, canvas: ZoomRectangleTool(axes, canvas, self._on_zoom_applied))
        
        # Built-in double zoom widget (publicly accessible, disabled by default)
        # Pass callback to get current image - decoupled from parent structure
        self.double_zoom = DoubleZoomWidget(
            self.axes, 
            parent=self,  # Parent is ImageViewerWidget for UI hierarchy
            get_image_func=self._get_current_image_data  # Callback to get current image
        )
        # DoubleZoomWidget is initialized in ready state (disabled) by default
        
        # Optional display options panel
        self.display_panel = None
        if show_display_panel:
            self.display_panel = DisplayOptionsPanel(
                self, 
                show_double_zoom=show_double_zoom,
                double_zoom_widget=self.double_zoom if show_double_zoom else None
            )
            self._connect_display_panel()
        
        # Internal state
        self._current_image = None
        self._current_colormap = 'gray'
        self._current_vmin = None
        self._current_vmax = None
        self._current_log_scale = False
        self._pan_start = None
        self._zoom_bounds = None
        self._left_button_pressed = False  # Track left button for drag panning
        
        # Connect matplotlib events (event coordination hub)
        self.canvas.mpl_connect('button_press_event', self._on_button_press)
        self.canvas.mpl_connect('motion_notify_event', self._on_motion)
        self.canvas.mpl_connect('button_release_event', self._on_button_release)
        self.canvas.mpl_connect('scroll_event', self._on_scroll)
        
        # Setup layout (minimal - just the canvas)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.canvas)
    
    def _connect_display_panel(self):
        """Connect display panel signals to internal handlers."""
        if self.display_panel:
            self.display_panel.intensityChanged.connect(self._on_intensity_changed_from_panel)
            self.display_panel.logScaleChanged.connect(self._on_log_scale_changed_from_panel)
            self.display_panel.colorMapChanged.connect(self._on_colormap_changed_from_panel)
            self.display_panel.zoomOutRequested.connect(self.reset_zoom)
    
    # ===== Internal Helper Methods =====
    
    def _get_current_image_data(self):
        """
        Get current image data for DoubleZoom and other components.
        
        This method provides backward compatibility:
        1. First checks _current_image (set by display_image())
        2. Falls back to extracting from axes.images (for legacy code)
        
        Returns:
            numpy.ndarray or None: Current image data
        """
        # Check if image was set through display_image()
        if self._current_image is not None:
            return self._current_image
        
        # Fallback: extract from axes.images (backward compatibility)
        if len(self.axes.images) > 0:
            # Get the most recent image
            img_artist = self.axes.images[-1]
            return img_artist.get_array()
        
        return None
    
    # ===== Public API for DoubleZoom =====
    
    def enable_double_zoom(self, enabled=True):
        """
        Enable or disable the DoubleZoom feature.
        
        Args:
            enabled: True to enable, False to disable
        """
        if enabled:
            self.double_zoom.set_running()
        else:
            self.double_zoom.set_ready()
    
    def is_double_zoom_enabled(self):
        """Check if DoubleZoom is currently enabled."""
        return self.double_zoom.is_enabled()
    
    def _on_intensity_changed_from_panel(self, vmin, vmax):
        """Handle intensity change from display panel."""
        self._current_vmin = vmin
        self._current_vmax = vmax
        if self._current_image is not None:
            self.update_display_settings()
            self._emit_display_options_changed()
    
    def _on_log_scale_changed_from_panel(self, log_scale):
        """Handle log scale change from display panel."""
        self._current_log_scale = log_scale
        if self._current_image is not None:
            self.update_display_settings()
            self._emit_display_options_changed()
    
    def _on_colormap_changed_from_panel(self, colormap):
        """Handle colormap change from display panel."""
        self._current_colormap = colormap
        if self._current_image is not None:
            self.update_display_settings()
            self._emit_display_options_changed()
    
    def _emit_display_options_changed(self):
        """Emit signal when display options change."""
        options = self.get_display_options()
        self.displayOptionsChanged.emit(options)
    
    def _on_zoom_applied(self, zoom_bounds):
        """
        Internal callback for zoom_rectangle tool.
        Applies the zoom and emits toolCompleted signal for external handlers.
        
        Args:
            zoom_bounds: [(x_min, x_max), (y_min, y_max)]
        """
        # Apply the zoom
        self.set_zoom_bounds(zoom_bounds[0], zoom_bounds[1])
        # Deactivate the tool (clears selection rectangle)
        self.tool_manager.deactivate_tool('zoom_rectangle')
        # Emit signal for external handlers (e.g., to update UI state, save zoom state, etc.)
        self.toolCompleted.emit('zoom_rectangle', zoom_bounds)
    
    # ===== Public API =====
    
    def update_display_settings(self):
        """
        Update only the intensity/colormap settings without clearing axes.
        Preserves zoom level and any overlays (like center crosshairs).
        """
        if self._current_image is None:
            return
        
        # Get current zoom state
        xlim = self.axes.get_xlim()
        ylim = self.axes.get_ylim()
        
        # Get all existing artists (lines, patches) to preserve them
        lines = list(self.axes.lines)
        patches = list(self.axes.patches)
        texts = list(self.axes.texts)
        
        # Clear only images
        for im in self.axes.images:
            im.remove()
        
        # Redraw image with new settings
        if self._current_log_scale:
            self.axes.imshow(self._current_image, cmap=self._current_colormap, 
                           norm=LogNorm(vmin=max(1, self._current_vmin), 
                                      vmax=self._current_vmax))
        else:
            self.axes.imshow(self._current_image, cmap=self._current_colormap,
                           norm=Normalize(vmin=self._current_vmin, 
                                        vmax=self._current_vmax))
        
        # Restore zoom state
        self.axes.set_xlim(xlim)
        self.axes.set_ylim(ylim)
        
        self.canvas.draw()
    
    def display_image(self, img, vmin=None, vmax=None, log_scale=None, colormap=None):
        """
        Display an image with specified intensity settings.
        
        Args:
            img: 2D numpy array
            vmin: Minimum intensity value (None = keep current if exists, else image min)
            vmax: Maximum intensity value (None = keep current if exists, else image max)
            log_scale: Whether to use logarithmic scale (None = keep current)
            colormap: Color map name (None = keep current)
        """
        self._current_image = img
        
        # Update vmin/vmax: prefer explicit values, then current values, finally image min/max
        if vmin is not None:
            self._current_vmin = vmin
        elif self._current_vmin is None:
            self._current_vmin = img.min()
        
        if vmax is not None:
            self._current_vmax = vmax
        elif self._current_vmax is None:
            self._current_vmax = img.max()
        
        # Update log_scale: prefer explicit value, then keep current
        if log_scale is not None:
            self._current_log_scale = log_scale
        
        # Update colormap: prefer explicit value, then keep current
        if colormap is not None:
            self._current_colormap = colormap
        
        self.axes.cla()
        
        if log_scale:
            self.axes.imshow(img, cmap=self._current_colormap, 
                           norm=LogNorm(vmin=max(1, self._current_vmin), vmax=self._current_vmax))
        else:
            self.axes.imshow(img, cmap=self._current_colormap,
                           norm=Normalize(vmin=self._current_vmin, vmax=self._current_vmax))
        
        self.axes.set_facecolor('black')
        self.axes.set_xlim(0, img.shape[1])
        self.axes.set_ylim(0, img.shape[0])
        # Invert Y-axis to match image coordinates: (0,0) at top-left
        # This ensures Y increases downward, matching image array indexing
        self.axes.invert_yaxis()
        
        # Update display panel if present
        if self.display_panel:
            self.display_panel.set_intensity_range(img.min(), img.max())
            if vmin is not None and vmax is not None:
                self.display_panel.set_intensity_values(vmin, vmax)
            if log_scale:
                self.display_panel.set_log_scale(log_scale)
            if colormap is not None:
                self.display_panel.set_color_map(colormap)
        
        self.canvas.draw()
        
        # Emit signal for external handlers
        self._emit_display_options_changed()
    
    def refresh_display(self):
        """
        Refresh the display with current settings (alias for update_display_settings).
        Preserves zoom level and any overlays.
        """
        self.update_display_settings()
    
    def get_display_options(self):
        """
        Get current display options.
        
        Returns:
            Dict with display settings: vmin, vmax, log_scale, colormap
        """
        return {
            'vmin': self._current_vmin,
            'vmax': self._current_vmax,
            'log_scale': self._current_log_scale,
            'colormap': self._current_colormap
        }
    
    def set_display_options(self, vmin=None, vmax=None, log_scale=None, colormap=None):
        """
        Set display options programmatically and update display.
        
        Args:
            vmin: Minimum intensity value
            vmax: Maximum intensity value
            log_scale: Whether to use logarithmic scale
            colormap: Color map name
        """
        changed = False
        
        if vmin is not None and vmin != self._current_vmin:
            self._current_vmin = vmin
            changed = True
        
        if vmax is not None and vmax != self._current_vmax:
            self._current_vmax = vmax
            changed = True
        
        if log_scale is not None and log_scale != self._current_log_scale:
            self._current_log_scale = log_scale
            changed = True
        
        if colormap is not None and colormap != self._current_colormap:
            self._current_colormap = colormap
            changed = True
        
        # Update display panel if present
        if self.display_panel:
            if vmin is not None or vmax is not None:
                self.display_panel.set_intensity_values(
                    vmin if vmin is not None else self._current_vmin,
                    vmax if vmax is not None else self._current_vmax
                )
            if log_scale is not None:
                self.display_panel.set_log_scale(log_scale)
            if colormap is not None:
                self.display_panel.set_color_map(colormap)
        
        # Refresh display if anything changed
        if changed and self._current_image is not None:
            self.update_display_settings()
            self._emit_display_options_changed()
    
    def reset_zoom(self):
        """Reset view to show full image."""
        if self._current_image is not None:
            self.axes.set_xlim(0, self._current_image.shape[1])
            # Set ylim in correct order for inverted Y-axis (already inverted in display_image)
            self.axes.set_ylim(self._current_image.shape[0], 0)
            self.canvas.draw()
    
    def set_zoom_bounds(self, xlim, ylim):
        """
        Set specific zoom bounds.
        
        Args:
            xlim: Tuple (xmin, xmax)
            ylim: Tuple (ymin, ymax) - Note: for inverted Y-axis, use (ymax, ymin)
        """
        self.axes.set_xlim(xlim)
        self.axes.set_ylim(ylim)
        self.canvas.draw()
    
    def get_zoom_bounds(self):
        """
        Get current zoom bounds.
        
        Returns:
            Tuple of ((xmin, xmax), (ymin, ymax))
        """
        return (self.axes.get_xlim(), self.axes.get_ylim())
    
    def redraw(self):
        """Force canvas redraw."""
        self.canvas.draw()
    
    # ===== Event Coordination (delegates to tool_manager or internal handlers) =====
    
    def _on_button_press(self, event):
        """
        Mouse button press event coordinator.
        
        Processing order:
        1. Emit signal to external handlers
        2. DoubleZoom (coordinate precision layer)
        3. ToolManager (modal interactions)
        4. Emit specific click signals (right/left click)
        5. Internal handlers (pan, etc.)
        """
        # 0. Emit raw event signal first
        self.mousePressed.emit(event)
        
        # 1. DoubleZoom handling (coordinate precision layer)
        if self.double_zoom.is_enabled():
            if self.double_zoom.handle_click(event):
                return  # DoubleZoom intercepted the event
        
        # 2. Try tool manager (modal interactions)
        if self.tool_manager and self.tool_manager.handle_press(event):
            return  # Tool handled it
        
        # 3. Emit specific click signals for business logic
        if event.inaxes == self.axes and event.xdata is not None and event.ydata is not None:
            if event.button == 3:  # Right-click
                self.rightClickAt.emit(event.xdata, event.ydata)
            elif event.button == 1:  # Left-click
                self.leftClickAt.emit(event.xdata, event.ydata)
        
        # 4. Fall back to internal handlers (basic navigation)
        self._handle_pan_start(event)
    
    def _on_motion(self, event):
        """
        Mouse motion event coordinator.
        
        Processing order:
        1. Emit signal to external handlers
        2. DoubleZoom updates
        3. ToolManager motion handling
        4. Internal handlers (pan drag)
        5. Coordinate display
        """
        # 0. Emit raw event signal
        self.mouseMoved.emit(event)
        
        # 1. DoubleZoom updates (updates zoom window as mouse moves)
        if self.double_zoom.is_enabled():
            self.double_zoom.handle_mouse_move_event(event)
        
        # 2. Try tool manager (modal interactions)
        if self.tool_manager and self.tool_manager.handle_motion(event):
            return  # Tool handled it
        
        # 3. Fall back to internal handlers (basic navigation)
        self._handle_pan_drag(event)
        
        # 4. Always display coordinates (non-blocking)
        self._display_coordinates(event)
    
    def _on_button_release(self, event):
        """
        Mouse button release event coordinator.
        
        Processing order:
        1. Emit signal to external handlers
        2. DoubleZoom precise coordinate selection (two-stage click handling)
        3. ToolManager release handling + tool completion detection
        4. Internal handlers (pan end)
        5. Emit canvasClicked if not handled
        """
        # 0. Emit raw event signal
        self.mouseReleased.emit(event)
        
        # 1. DoubleZoom handling (two-stage click: main image -> zoom window)
        # Note: handle_click already processed in _on_button_press
        if self.double_zoom.is_enabled():
            # Check if DoubleZoom is blocking (waiting for zoom window click after main image click)
            if self.double_zoom.is_blocking_other_actions():
                # First click on main image was processed - zoom frozen, waiting for second click
                # Don't emit coordinates yet, block all further processing
                return
            
            # Check if the completed click was on zoom window (second click)
            # If so, precise coordinates were already calculated in _on_button_press
            if event.inaxes == self.double_zoom.doubleZoomAxes:
                # Get the precise coordinates and emit them
                precise_x, precise_y = self.double_zoom.get_precise_coords()
                self.preciseCoordinatesSelected.emit(precise_x, precise_y)
                
                # Modify event for downstream handlers (tools, etc.)
                event.xdata = precise_x
                event.ydata = precise_y
                event.inaxes = self.axes
                # Continue to tool manager with precise coordinates
        
        # 2. Try tool manager (modal interactions)
        tool_handled = False
        if self.tool_manager and self.tool_manager.handle_release(event):
            tool_handled = True
            
            # Check if the active tool has completed
            if self.tool_manager.active_tool and hasattr(self.tool_manager.active_tool, 'completed'):
                if self.tool_manager.active_tool.completed:
                    # Tool has completed! Get result and emit signal
                    tool_name, result = self.tool_manager.deactivate_current_tool()
                    if tool_name is not None and result is not None:
                        self.toolCompleted.emit(tool_name, result)
        
        # 3. If no tool handled it, fall back to internal handlers
        if not tool_handled:
            self._handle_pan_end(event)
            # Emit signal for external handlers
            self.canvasClicked.emit(event)
    
    def _on_scroll(self, event):
        """
        Mouse scroll event handler.
        
        Processing order:
        1. Emit signal to external handlers
        2. Internal wheel zoom handling
        """
        # 0. Emit raw event signal
        self.mouseScrolled.emit(event)
        
        # 1. Handle wheel zoom (built-in feature)
        if event.inaxes != self.axes or event.xdata is None:
            return
        
        self._handle_wheel_zoom(event)
    
    # ===== Internal Handlers (built-in non-modal features) =====
    
    def _handle_pan_start(self, event):
        """
        Start pan operation.
        
        Supports:
        - Middle-click drag (always available)
        - Left-click drag (only when no tool/DoubleZoom is active)
        """
        if event.inaxes != self.axes or event.xdata is None or event.ydata is None:
            return
        
        # Middle-click: always available for pan
        if event.button == 2:
            self._pan_start = (event.xdata, event.ydata)
            return
        
        # Left-click: only when no tool or DoubleZoom is active
        if event.button == 1:
            # Check if any tool is active or DoubleZoom is enabled
            has_active_tool = self.tool_manager and self.tool_manager.has_active_tool()
            double_zoom_enabled = self.double_zoom.is_enabled()
            
            if not has_active_tool and not double_zoom_enabled:
                self._pan_start = (event.xdata, event.ydata)
                self._left_button_pressed = True
    
    def _handle_pan_drag(self, event):
        """Execute pan operation with boundary constraints."""
        if not self._pan_start or event.xdata is None or event.ydata is None:
            return
        
        if self._current_image is None:
            return
        
        dx = self._pan_start[0] - event.xdata
        dy = self._pan_start[1] - event.ydata
        
        xlim = self.axes.get_xlim()
        ylim = self.axes.get_ylim()
        
        # Calculate new limits
        new_x1 = xlim[0] + dx
        new_x2 = xlim[1] + dx
        new_y1 = ylim[0] + dy
        new_y2 = ylim[1] + dy
        
        # Get image bounds
        img_width = self._current_image.shape[1]
        img_height = self._current_image.shape[0]
        
        # Apply boundary constraints for X axis
        zoom_width = xlim[1] - xlim[0]
        if new_x1 < 0:
            new_x1 = 0
            new_x2 = zoom_width
        if new_x2 > img_width:
            new_x2 = img_width
            new_x1 = img_width - zoom_width
        
        # Apply boundary constraints for Y axis
        # Handle both normal and inverted Y-axis
        if ylim[0] > ylim[1]:  # Inverted Y-axis
            zoom_height = ylim[0] - ylim[1]
            if new_y2 < 0:
                new_y2 = 0
                new_y1 = zoom_height
            if new_y1 > img_height:
                new_y1 = img_height
                new_y2 = img_height - zoom_height
        else:  # Normal Y-axis
            zoom_height = ylim[1] - ylim[0]
            if new_y1 < 0:
                new_y1 = 0
                new_y2 = zoom_height
            if new_y2 > img_height:
                new_y2 = img_height
                new_y1 = img_height - zoom_height
        
        self.axes.set_xlim(new_x1, new_x2)
        self.axes.set_ylim(new_y1, new_y2)
        self.canvas.draw_idle()
    
    def _handle_pan_end(self, event):
        """End pan operation."""
        self._pan_start = None
        self._left_button_pressed = False
    
    def _handle_wheel_zoom(self, event):
        """Handle mouse wheel zoom (always available)."""
        if self._current_image is None or event.xdata is None or event.ydata is None:
            return
        
        direction = event.button
        x = event.xdata
        y = event.ydata
        img_shape = self._current_image.shape
        
        xlim = self.axes.get_xlim()
        ylim = self.axes.get_ylim()
        
        # Calculate current view size (using absolute difference to handle inverted axes)
        zoom_width = abs(xlim[1] - xlim[0])
        zoom_height = abs(ylim[1] - ylim[0])
        
        # Calculate clicked position percentage within current view
        # Work with actual min/max regardless of axis direction
        x_min, x_max = min(xlim), max(xlim)
        y_min, y_max = min(ylim), max(ylim)
        
        clicked_x_percentage = (x - x_min) / zoom_width if zoom_width > 0 else 0.5
        clicked_y_percentage = (y - y_min) / zoom_height if zoom_height > 0 else 0.5
        
        # Calculate step size (10% of current view)
        step_x = 0.1 * zoom_width
        step_y = 0.1 * zoom_height
        
        # Adjust step direction: zoom in = reduce view, zoom out = increase view
        if direction == 'up':  # zoom in
            step_x *= -1
            step_y *= -1
        
        # Calculate new view size with constraints
        zoom_width = min(img_shape[1], max(zoom_width + step_x, 50))
        zoom_height = min(img_shape[0], max(zoom_height + step_y, 50))
        
        # Calculate new bounds centered on mouse position
        x1 = x - clicked_x_percentage * zoom_width
        x2 = x1 + zoom_width
        y1 = y - clicked_y_percentage * zoom_height
        y2 = y1 + zoom_height
        
        # Constrain to image bounds
        if x1 < 0:
            x1 = 0
            x2 = zoom_width
        if x2 > img_shape[1]:
            x2 = img_shape[1]
            x1 = img_shape[1] - zoom_width
        
        if y1 < 0:
            y1 = 0
            y2 = zoom_height
        if y2 > img_shape[0]:
            y2 = img_shape[0]
            y1 = img_shape[0] - zoom_height
        
        # Apply new limits
        self.axes.set_xlim(x1, x2)
        # For inverted Y-axis, set limits in reverse order to maintain inversion
        if ylim[0] > ylim[1]:  # Y-axis is inverted
            self.axes.set_ylim(y2, y1)
        else:  # Normal Y-axis
            self.axes.set_ylim(y1, y2)
        
        self.canvas.draw_idle()
    
    def _display_coordinates(self, event):
        """Display mouse coordinates (emit signal for external display)."""
        if event.inaxes == self.axes and event.xdata is not None:
            x = int(round(event.xdata))
            y = int(round(event.ydata))
            
            # Get pixel value if image exists
            if (self._current_image is not None and 
                0 <= x < self._current_image.shape[1] and 
                0 <= y < self._current_image.shape[0]):
                value = self._current_image[y][x]
                self.coordinatesChanged.emit(x, y, value)
