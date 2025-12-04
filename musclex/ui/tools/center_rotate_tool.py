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
from .interaction_tool import InteractionTool


class CenterRotateTool(InteractionTool):
    """
    Tool for simultaneously setting image center and rotation angle.
    
    Workflow:
        1. User clicks on the first reflection peak on the equator
        2. User clicks on the corresponding reflection peak on the opposite side
        3. Tool calculates:
           - Center: midpoint between the two peaks
           - Angle: angle of the line connecting the peaks
        4. Returns both center and angle as a result
    
    This is particularly useful for diffraction patterns where equatorial
    reflections should be horizontal and centered.
    
    Usage:
        tool = CenterRotateTool(axes, canvas, coord_transform_func)
        tool.activate()
        # ... user clicks two points ...
        result = tool.get_result()  # Returns {'center': (x, y), 'angle': float}
    """
    
    def __init__(self, axes, canvas):
        """
        Initialize the center-rotate tool.
        
        Args:
            axes: matplotlib axes object
            canvas: matplotlib canvas object
        
        Note: This tool returns center in DISPLAY coordinates (same as ChordsCenterTool
        and PerpendicularsCenterTool). Coordinate transformation to original image
        coordinates should be done by the caller (e.g., ImageSettingsPanel).
        """
        super().__init__(axes, canvas)
        self.points = []  # Points in display coordinates
        self.axis_size = 5  # Size of cross markers
        self.completed = False
    
    def _on_activate(self):
        """Reset state and clear axes when tool is activated."""
        self.points = []
        self.completed = False
        self.clear_axes()
    
    def _on_deactivate(self):
        """Clean up when tool is deactivated."""
        self.clear_axes()
    
    def handle_click(self, event) -> bool:
        """Not used - we handle release instead."""
        return False
    
    def handle_motion(self, event) -> bool:
        """
        Draw preview line when hovering after clicking first point.
        
        Args:
            event: matplotlib mouse motion event
            
        Returns:
            True if event was handled
        """
        if not self.is_active:
            return False
        
        if event.inaxes != self.axes:
            return False
        
        # Only draw preview if we have exactly one point
        if len(self.points) == 1:
            x, y = event.xdata, event.ydata
            start_pt = self.points[0]
            
            # Remove old preview items
            self.remove_labeled_items(['preview_cross', 'preview_line'])
            
            # Draw preview cross at cursor
            self.axes.plot((x - self.axis_size, x + self.axis_size), 
                          (y - self.axis_size, y + self.axis_size), 
                          color='r', alpha=0.5, label='preview_cross')
            self.axes.plot((x - self.axis_size, x + self.axis_size), 
                          (y + self.axis_size, y - self.axis_size), 
                          color='r', alpha=0.5, label='preview_cross')
            
            # Draw preview line
            self.axes.plot((start_pt[0], x), (start_pt[1], y), 
                          color='r', linestyle='--', alpha=0.5, 
                          label='preview_line')
            
            self.canvas.draw_idle()
            return True
        
        return False
    
    def handle_release(self, event) -> bool:
        """
        Handle mouse button release - add a point.
        After 2 points, calculate center and angle.
        
        Args:
            event: matplotlib mouse button release event
            
        Returns:
            True if event was handled
        """
        if not self.is_active:
            return False
        
        if event.inaxes != self.axes:
            return False
        
        x, y = event.xdata, event.ydata
        
        # Store display coordinates (caller will convert to original if needed)
        self.points.append((x, y))
        
        # Remove preview items
        self.remove_labeled_items(['preview_cross', 'preview_line'])
        
        # Draw permanent cross marker at this point
        self._draw_cross(x, y)
        
        # If this is the second point, draw the final line
        if len(self.points) == 2:
            pt1 = self.points[0]
            pt2 = self.points[1]
            self.axes.plot((pt1[0], pt2[0]), (pt1[1], pt2[1]), 
                          color='r', linewidth=2)
            
            # Mark as completed - the GUI will auto-apply
            self.completed = True
        
        self.canvas.draw_idle()
        return True
    
    def get_result(self):
        """
        Calculate and return the center and angle.
        
        Returns center in DISPLAY coordinates (consistent with ChordsCenterTool
        and PerpendicularsCenterTool). Caller should transform if needed.
        
        Angle: Calculated in display coordinates (as an increment relative to current rotation)
        
        Returns:
            Dictionary with 'center' (tuple) and 'angle' (float), or None if incomplete
        """
        if len(self.points) < 2:
            return None
        
        # Calculate center using display coordinates
        pt1 = self.points[0]
        pt2 = self.points[1]
        
        if pt1[0] < pt2[0]:
            x1, y1 = pt1
            x2, y2 = pt2
        else:
            x1, y1 = pt2
            x2, y2 = pt1
        
        # Calculate center (midpoint in display coordinates)
        cx = (x1 + x2) / 2.0
        cy = (y1 + y2) / 2.0
        
        # Calculate angle using display coordinates (as increment relative to current rotation)
        # Note: We already have x1, y1, x2, y2 from above (in display coordinates)
        # This is because setAngle() accumulates: new_rotation = current_rotation + angle
        
        # Calculate angle from horizontal
        if abs(x2 - x1) == 0:
            # Vertical line
            angle = -90.0
        else:
            # arctan gives angle from horizontal
            angle = -180.0 * np.arctan((y1 - y2) / abs(x1 - x2)) / np.pi
        
        return {
            'center': (cx, cy),
            'angle': angle
        }
    
    def _draw_cross(self, x, y):
        """
        Draw a cross marker at the specified location.
        
        Args:
            x, y: coordinates
        """
        size = self.axis_size
        self.axes.plot((x - size, x + size), (y - size, y + size), color='r')
        self.axes.plot((x - size, x + size), (y + size, y - size), color='r')

