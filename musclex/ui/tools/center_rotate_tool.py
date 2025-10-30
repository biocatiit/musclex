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
    
    def __init__(self, axes, canvas, coord_transform_func):
        """
        Initialize the center-rotate tool.
        
        Args:
            axes: matplotlib axes object
            canvas: matplotlib canvas object
            coord_transform_func: callable that converts display coords to original image coords
                                 Should accept (x, y) and return (x_orig, y_orig)
        """
        super().__init__(axes, canvas)
        self.coord_transform_func = coord_transform_func
        self.points_orig = []  # Points in original image coordinates
        self.points_display = []  # Points in display coordinates
        self.axis_size = 5  # Size of cross markers
        self.completed = False
    
    def _on_activate(self):
        """Reset state and clear axes when tool is activated."""
        self.points_orig = []
        self.points_display = []
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
        if len(self.points_display) == 1:
            x, y = event.xdata, event.ydata
            start_pt = self.points_display[0]
            
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
        
        # Convert to original image coordinates
        x_orig, y_orig = self.coord_transform_func(x, y)
        
        # Store both coordinate systems
        self.points_orig.append((x_orig, y_orig))
        self.points_display.append((x, y))
        
        # Remove preview items
        self.remove_labeled_items(['preview_cross', 'preview_line'])
        
        # Draw permanent cross marker at this point
        self._draw_cross(x, y)
        
        # If this is the second point, draw the final line
        if len(self.points_display) == 2:
            pt1 = self.points_display[0]
            pt2 = self.points_display[1]
            self.axes.plot((pt1[0], pt2[0]), (pt1[1], pt2[1]), 
                          color='r', linewidth=2)
            
            # Mark as completed - the GUI will auto-apply
            self.completed = True
        
        self.canvas.draw_idle()
        return True
    
    def get_result(self):
        """
        Calculate and return the center and angle.
        
        Center: Calculated in original image coordinates (for precise positioning)
        Angle: Calculated in display coordinates (as an increment relative to current rotation)
        
        Returns:
            Dictionary with 'center' (tuple) and 'angle' (float), or None if incomplete
        """
        if len(self.points_orig) < 2:
            return None
        
        # Calculate center using original coordinates (for precise positioning)
        pt1_orig = self.points_orig[0]
        pt2_orig = self.points_orig[1]
        
        if pt1_orig[0] < pt2_orig[0]:
            x1, y1 = pt1_orig
            x2, y2 = pt2_orig
        else:
            x1, y1 = pt2_orig
            x2, y2 = pt1_orig
        
        # Calculate center (midpoint in original coordinates)
        cx = int(round((x1 + x2) / 2.0))
        cy = int(round((y1 + y2) / 2.0))
        
        # Calculate angle using display coordinates (as increment relative to current rotation)
        # This is because setAngle() accumulates: new_rotation = current_rotation + angle
        pt1_disp = self.points_display[0]
        pt2_disp = self.points_display[1]
        
        if pt1_disp[0] < pt2_disp[0]:
            dx1, dy1 = pt1_disp
            dx2, dy2 = pt2_disp
        else:
            dx1, dy1 = pt2_disp
            dx2, dy2 = pt1_disp
        
        # Calculate angle from horizontal (in display coordinates)
        if abs(dx2 - dx1) == 0:
            # Vertical line
            angle = -90.0
        else:
            # arctan gives angle from horizontal
            angle = -180.0 * np.arctan((dy1 - dy2) / abs(dx1 - dx2)) / np.pi
        
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

