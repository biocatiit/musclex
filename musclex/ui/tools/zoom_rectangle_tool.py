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

from matplotlib.widgets import RectangleSelector
from .interaction_tool import InteractionTool


class ZoomRectangleTool(InteractionTool):
    """
    Tool for selecting a rectangular zoom region using matplotlib RectangleSelector.
    
    This tool wraps matplotlib's RectangleSelector widget to provide a professional
    zoom selection experience with drag-to-select and interactive adjustment.
    
    Workflow:
        1. User activates the tool
        2. User drags to select a rectangular region
        3. When selection is released, the zoom is applied IMMEDIATELY
        4. Tool automatically deactivates itself
    
    Usage:
        tool = ZoomRectangleTool(axes, canvas, on_zoom_callback)
        tool.activate()
        # ... user drags to select region ...
        # Zoom is applied automatically when selection is released
    """
    
    def __init__(self, axes, canvas, on_zoom_callback=None):
        """
        Initialize the ZoomRectangleTool.
        
        Args:
            axes: The matplotlib axes to draw on
            canvas: The matplotlib canvas to redraw
            on_zoom_callback: Optional callback function(zoom_bounds) to call when zoom is applied
        """
        super().__init__(axes, canvas)
        self.selector = None
        self.zoom_bounds = None
        self.on_zoom_callback = on_zoom_callback
    
    def _on_activate(self):
        """Create and activate the RectangleSelector when tool is activated."""
        self.zoom_bounds = None
        
        # Create RectangleSelector
        self.selector = RectangleSelector(
            self.axes,
            self._on_select,
            useblit=True,
            button=[1],  # Left mouse button
            minspanx=5,
            minspany=5,
            spancoords='pixels',
            interactive=True,
            props=dict(
                facecolor='red',
                edgecolor='red',
                alpha=0.2,
                fill=True
            )
        )
    
    def _on_deactivate(self):
        """Clean up the RectangleSelector when tool is deactivated."""
        if self.selector is not None:
            # Set selector to inactive and hide it
            self.selector.set_active(False)
            self.selector.set_visible(False)
            # Remove the selector's artists from the axes
            self.selector = None
        self.canvas.draw_idle()
    
    def _on_select(self, eclick, erelease):
        """
        Callback when rectangle selection is completed (mouse button released).
        This is called IMMEDIATELY when user finishes dragging, so we apply zoom here.
        
        Args:
            eclick: Mouse button press event
            erelease: Mouse button release event
        """
        x1, y1 = eclick.xdata, eclick.ydata
        x2, y2 = erelease.xdata, erelease.ydata
        
        # Store zoom bounds
        self.zoom_bounds = [
            (min(x1, x2), max(x1, x2)),  # x range
            (min(y1, y2), max(y1, y2))   # y range
        ]
        
        # Immediately hide the selection rectangle to avoid leaving visual artifacts
        if self.selector is not None:
            self.selector.set_visible(False)
            self.canvas.draw_idle()
        
        # Apply zoom immediately if callback is provided
        if self.on_zoom_callback:
            self.on_zoom_callback(self.zoom_bounds)
    
    def handle_click(self, event) -> bool:
        """
        RectangleSelector handles clicks internally, so we don't need to process them.
        Just return False to indicate we're not blocking the event.
        """
        return False
    
    def handle_motion(self, event) -> bool:
        """
        RectangleSelector handles motion internally for drawing the rectangle.
        Just return False to indicate we're not blocking the event.
        """
        return False
    
    def handle_release(self, event) -> bool:
        """
        RectangleSelector handles release internally to finalize the selection.
        We return False to let other handlers process the event if needed.
        """
        return False
    
    def get_result(self):
        """
        Get the selected zoom bounds.
        
        Returns:
            List with two tuples: [(x_min, x_max), (y_min, y_max)], or None if no selection
        """
        return self.zoom_bounds

