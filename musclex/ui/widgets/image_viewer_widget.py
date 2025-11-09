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


class ImageViewerWidget(QWidget):
    """
    Reusable image viewer widget with event coordination.
    
    This widget provides:
    - Basic image display using matplotlib
    - Event coordination: delegates to ToolManager first, then handles internally
    - Built-in non-modal features: pan (middle-click drag), scroll zoom
    
    Architecture:
        - Receives all matplotlib events
        - Delegates to tool_manager (if set) for modal interactions
        - Falls back to internal handlers for basic navigation
    
    Usage:
        viewer = ImageViewerWidget()
        viewer.display_image(img, vmin, vmax, log_scale=False)
        
        # Optional: connect to tool system
        tool_manager = ToolManager(viewer.axes)
        viewer.set_tool_manager(tool_manager)
    """
    
    # Signals
    coordinatesChanged = Signal(float, float, float)  # x, y, value
    canvasClicked = Signal(object)  # Emitted when canvas clicked without active tool handling it
    
    def __init__(self, parent=None, show_display_panel=False):
        super().__init__(parent)
        
        # Matplotlib components (exposed for flexibility)
        self.figure = plt.figure()
        self.axes = self.figure.add_subplot(111)
        self.axes.set_aspect('equal', adjustable='box')
        self.canvas = FigureCanvas(self.figure)
        
        # Optional display options panel
        self.display_panel = None
        if show_display_panel:
            self.display_panel = DisplayOptionsPanel(self)
            self._connect_display_panel()
        
        # Tool manager reference (optional, set by external code)
        self.tool_manager = None
        
        # Internal state
        self._current_image = None
        self._current_colormap = 'gray'
        self._current_vmin = None
        self._current_vmax = None
        self._current_log_scale = False
        self._pan_start = None
        self._zoom_bounds = None
        
        # Connect matplotlib events (event coordination hub)
        self.canvas.mpl_connect('button_press_event', self._on_button_press)
        self.canvas.mpl_connect('motion_notify_event', self._on_motion)
        self.canvas.mpl_connect('button_release_event', self._on_button_release)
        self.canvas.mpl_connect('scroll_event', self._on_scroll)
        
        # Setup layout (minimal - just the canvas)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.canvas)
    
    def set_tool_manager(self, tool_manager):
        """
        Set the tool manager for handling modal interactions.
        
        Args:
            tool_manager: ToolManager instance that will receive events first
        """
        self.tool_manager = tool_manager
    
    def _connect_display_panel(self):
        """Connect display panel signals to internal handlers."""
        if self.display_panel:
            self.display_panel.intensityChanged.connect(self._on_intensity_changed_from_panel)
            self.display_panel.logScaleChanged.connect(self._on_log_scale_changed_from_panel)
            self.display_panel.colorMapChanged.connect(self._on_colormap_changed_from_panel)
            self.display_panel.zoomOutRequested.connect(self.reset_zoom)
    
    def _on_intensity_changed_from_panel(self, vmin, vmax):
        """Handle intensity change from display panel."""
        self._current_vmin = vmin
        self._current_vmax = vmax
        if self._current_image is not None:
            self.display_image(self._current_image, vmin, vmax, self._current_log_scale)
    
    def _on_log_scale_changed_from_panel(self, log_scale):
        """Handle log scale change from display panel."""
        self._current_log_scale = log_scale
        if self._current_image is not None:
            self.display_image(self._current_image, 
                             self._current_vmin, self._current_vmax, log_scale)
    
    def _on_colormap_changed_from_panel(self, colormap):
        """Handle colormap change from display panel."""
        self._current_colormap = colormap
        if self._current_image is not None:
            self.display_image(self._current_image,
                             self._current_vmin, self._current_vmax, 
                             self._current_log_scale)
    
    # ===== Public API =====
    
    def display_image(self, img, vmin=None, vmax=None, log_scale=False):
        """
        Display an image with specified intensity settings.
        
        Args:
            img: 2D numpy array
            vmin: Minimum intensity value
            vmax: Maximum intensity value
            log_scale: Whether to use logarithmic scale
        """
        self._current_image = img
        self._current_vmin = vmin if vmin is not None else img.min()
        self._current_vmax = vmax if vmax is not None else img.max()
        self._current_log_scale = log_scale
        
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
        self.axes.invert_yaxis()
        
        # Update display panel if present
        if self.display_panel:
            self.display_panel.set_intensity_range(img.min(), img.max())
            if vmin is not None and vmax is not None:
                self.display_panel.set_intensity_values(vmin, vmax)
        
        self.canvas.draw()
    
    def reset_zoom(self):
        """Reset view to show full image."""
        if self._current_image is not None:
            self.axes.set_xlim(0, self._current_image.shape[1])
            self.axes.set_ylim(0, self._current_image.shape[0])
            self.axes.invert_yaxis()
            self.canvas.draw()
    
    def set_zoom_bounds(self, xlim, ylim):
        """
        Set specific zoom bounds.
        
        Args:
            xlim: Tuple (xmin, xmax)
            ylim: Tuple (ymin, ymax)
        """
        self.axes.set_xlim(xlim)
        self.axes.set_ylim(ylim)
        self.axes.invert_yaxis()
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
        Priority: tool_manager → internal handlers
        """
        # 1. Try tool manager first (modal interactions)
        if self.tool_manager and self.tool_manager.handle_press(event):
            return  # Tool handled it
        
        # 2. Fall back to internal handlers (basic navigation)
        self._handle_pan_start(event)
    
    def _on_motion(self, event):
        """
        Mouse motion event coordinator.
        Priority: tool_manager → internal handlers → coordinate display
        """
        # 1. Try tool manager first (modal interactions)
        if self.tool_manager and self.tool_manager.handle_motion(event):
            return  # Tool handled it
        
        # 2. Fall back to internal handlers (basic navigation)
        self._handle_pan_drag(event)
        
        # 3. Always display coordinates (non-blocking)
        self._display_coordinates(event)
    
    def _on_button_release(self, event):
        """
        Mouse button release event coordinator.
        Priority: tool_manager → internal handlers → signal emission
        """
        # 1. Try tool manager first (modal interactions)
        tool_handled = False
        if self.tool_manager and self.tool_manager.handle_release(event):
            tool_handled = True
        
        # 2. If no tool handled it, fall back to internal handlers and emit signal
        if not tool_handled:
            self._handle_pan_end(event)
            # 3. Emit signal for external handlers (e.g., set center in SetCentDialog)
            self.canvasClicked.emit(event)
    
    def _on_scroll(self, event):
        """
        Mouse scroll event handler.
        Scroll zoom is a built-in feature, not delegated to tools.
        """
        if event.inaxes != self.axes or event.xdata is None:
            return
        
        self._handle_wheel_zoom(event)
    
    # ===== Internal Handlers (built-in non-modal features) =====
    
    def _handle_pan_start(self, event):
        """Start pan operation (middle-click drag)."""
        if event.button == 2 and event.inaxes == self.axes:  # Middle button
            self._pan_start = (event.xdata, event.ydata)
    
    def _handle_pan_drag(self, event):
        """Execute pan operation."""
        if self._pan_start and event.xdata is not None and event.ydata is not None:
            dx = self._pan_start[0] - event.xdata
            dy = self._pan_start[1] - event.ydata
            
            xlim = self.axes.get_xlim()
            ylim = self.axes.get_ylim()
            
            self.axes.set_xlim(xlim[0] + dx, xlim[1] + dx)
            self.axes.set_ylim(ylim[0] + dy, ylim[1] + dy)
            self.canvas.draw_idle()
    
    def _handle_pan_end(self, event):
        """End pan operation."""
        self._pan_start = None
    
    def _handle_wheel_zoom(self, event):
        """Handle mouse wheel zoom (always available)."""
        scale = 1.1 if event.button == 'up' else 0.9
        
        xlim = self.axes.get_xlim()
        ylim = self.axes.get_ylim()
        
        # Zoom centered on mouse position
        xdata, ydata = event.xdata, event.ydata
        
        new_width = (xlim[1] - xlim[0]) * scale
        new_height = (ylim[1] - ylim[0]) * scale
        
        relx = (xlim[1] - xdata) / (xlim[1] - xlim[0])
        rely = (ylim[1] - ydata) / (ylim[1] - ylim[0])
        
        self.axes.set_xlim([xdata - new_width * (1 - relx),
                           xdata + new_width * relx])
        self.axes.set_ylim([ydata - new_height * (1 - rely),
                           ydata + new_height * rely])
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
