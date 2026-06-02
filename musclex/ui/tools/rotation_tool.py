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


class RotationTool(InteractionTool):
    """
    Tool for interactively setting rotation angle.
    
    Workflow:
        1. Tool shows a line from center through cursor position
        2. The line is mirrored (shows both sides through center)
        3. User clicks to set the angle
        4. Tool calculates angle from horizontal
    
    This is useful for aligning images where a specific feature (like an
    equatorial reflection) should be horizontal.
    
    Usage:
        tool = RotationTool(axes, canvas, get_center_func)
        tool.activate()
        # ... user moves mouse and clicks ...
        angle = tool.get_result()  # Returns angle in degrees
    """
    
    def __init__(self, axes, canvas, get_center_func):
        """
        Initialize the rotation tool.
        
        Args:
            axes: matplotlib axes object
            canvas: matplotlib canvas object
            get_center_func: callable that returns current center (x, y)
        """
        super().__init__(axes, canvas)
        self.get_center_func = get_center_func
        self.angle = None
        self.completed = False
    
    def _on_activate(self):
        """Reset state and clear axes when tool is activated."""
        self.angle = None
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
        Draw preview line showing rotation angle.
        
        The line goes through the center and cursor position,
        and is mirrored on the opposite side.
        
        Args:
            event: matplotlib mouse motion event
            
        Returns:
            True if event was handled
        """
        if not self.is_active:
            return False
        
        if event.inaxes != self.axes:
            return False
        
        # Get current center
        center = self.get_center_func()
        if center is None:
            return False
        
        cx, cy = center
        x, y = event.xdata, event.ydata
        
        # Calculate mirrored point (opposite side through center)
        dx = x - cx
        dy = y - cy
        x2 = cx - dx
        y2 = cy - dy
        
        # Remove old line
        self.remove_labeled_items(['rotation_line'])
        
        # Draw new line from mirrored point through center to cursor
        self.axes.plot([x, x2], [y, y2], color='g', linewidth=2, 
                      label='rotation_line')
        
        self.canvas.draw_idle()
        return True
    
    def handle_release(self, event) -> bool:
        """
        Handle mouse button release - set the angle and mark as complete.
        
        Args:
            event: matplotlib mouse button release event
            
        Returns:
            True if event was handled
        """
        if not self.is_active:
            return False
        
        if event.inaxes != self.axes:
            return False
        
        # Get current center
        center = self.get_center_func()
        if center is None:
            return False
        
        cx, cy = center
        x, y = event.xdata, event.ydata
        
        # Calculate angle and store it
        self.angle = self._calculate_angle(cx, cy, x, y)
        
        # Mark that we've completed the interaction
        # The GUI should check this and auto-deactivate the tool
        self.completed = True
        
        return True
    
    def get_result(self):
        """
        Get the calculated rotation angle.
        
        Returns:
            Angle in degrees (float), or None if not set
        """
        return self.angle
    
    def _calculate_angle(self, cx, cy, x, y):
        """
        Calculate rotation angle from horizontal.
        
        The angle is calculated such that rotating the image by this angle
        would make the line from center to (x,y) horizontal.
        
        Args:
            cx, cy: center coordinates
            x, y: point coordinates
            
        Returns:
            Angle in degrees (negative values for clockwise rotation)
        """
        # Ensure the line goes from left to right
        if cx < x:
            x1, y1 = cx, cy
            x2, y2 = x, y
        else:
            x1, y1 = x, y
            x2, y2 = cx, cy
        
        # Calculate angle from horizontal
        dx = abs(x1 - x2)
        dy = y1 - y2
        
        if dx == 0:
            # Vertical line
            return -90.0
        else:
            # arctan gives angle from horizontal
            # Negative because matplotlib y-axis points down
            angle = -180.0 * np.arctan(dy / dx) / np.pi
            return angle

