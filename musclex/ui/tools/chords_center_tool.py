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


def get_perpendicular_line_homogeneous(p1, p2):
    """
    Calculate the perpendicular bisector of a line segment.
    
    Args:
        p1: [x1, y1] first point
        p2: [x2, y2] second point
        
    Returns:
        (slope, center) where:
            slope: slope of perpendicular bisector (float('inf') for vertical)
            center: [x, y] midpoint of the segment
    """
    x1, y1 = p1
    x2, y2 = p2
    
    # Midpoint
    cx = (x1 + x2) / 2
    cy = (y1 + y2) / 2
    center = [cx, cy]
    
    # Slope of original line
    if abs(x2 - x1) < 1e-10:
        # Original line is vertical, perpendicular is horizontal
        return 0, center
    
    original_slope = (y2 - y1) / (x2 - x1)
    
    if abs(original_slope) < 1e-10:
        # Original line is horizontal, perpendicular is vertical
        return float('inf'), center
    
    # Perpendicular slope is negative reciprocal
    perp_slope = -1 / original_slope
    
    return perp_slope, center


class ChordsCenterTool(InteractionTool):
    """
    Tool for finding image center using chord perpendicular bisectors.
    
    Workflow:
        1. User clicks 3+ points on circular features
        2. Tool draws perpendicular bisectors through chord midpoints
        3. Tool calculates center as average of all line intersections
    
    Usage:
        tool = ChordsCenterTool(axes, canvas)
        tool.activate()
        # ... user clicks points ...
        center = tool.get_result()  # Returns (x, y) tuple
    """
    
    def __init__(self, axes, canvas):
        super().__init__(axes, canvas)
        self.chord_points = []
        self.chord_lines = []  # List of (slope, intercept) tuples
        self.axis_size = 5  # Size of cross markers
    
    def _on_activate(self):
        """Reset state and clear axes when tool is activated."""
        self.chord_points = []
        self.chord_lines = []
        self.clear_axes()
    
    def _on_deactivate(self):
        """Clean up when tool is deactivated."""
        self.clear_axes()
    
    def handle_click(self, event) -> bool:
        """Not used - we handle release instead."""
        return False
    
    def handle_motion(self, event) -> bool:
        """Not used - no hover preview for this tool."""
        return False
    
    def handle_release(self, event) -> bool:
        """
        Handle mouse button release - add a chord point.
        
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
        
        # Add point
        self.chord_points.append([x, y])
        
        # Draw cross marker at click location
        self._draw_cross(x, y)
        
        # If we have 3+ points, draw perpendicular bisectors
        if len(self.chord_points) >= 3:
            self._draw_perpendiculars()
        
        self.canvas.draw_idle()
        return True
    
    def get_result(self):
        """
        Calculate and return the center point.
        
        Returns:
            (x, y) tuple of center coordinates, or None if insufficient points
        """
        if len(self.chord_points) < 3:
            return None
        
        # Make sure perpendiculars are calculated
        if not self.chord_lines:
            self._calculate_perpendiculars()
        
        # Calculate all intersection points
        centers = []
        for i, line1 in enumerate(self.chord_lines):
            for line2 in self.chord_lines[i + 1:]:
                # Skip parallel lines
                if line1[0] == line2[0]:
                    continue
                
                # Calculate intersection
                if line1[0] == float('inf'):
                    # line1 is vertical: x = line1[1]
                    xcent = line1[1]
                    ycent = line2[0] * xcent + line2[1]
                elif line2[0] == float('inf'):
                    # line2 is vertical: x = line2[1]
                    xcent = line2[1]
                    ycent = line1[0] * xcent + line1[1]
                else:
                    # Both lines are non-vertical
                    # y = m1*x + b1 = m2*x + b2
                    # (m1 - m2)*x = b2 - b1
                    xcent = (line2[1] - line1[1]) / (line1[0] - line2[0])
                    ycent = line1[0] * xcent + line1[1]
                
                centers.append([xcent, ycent])
        
        if not centers:
            return None
        
        # Return average of all intersection points
        cx = int(sum(c[0] for c in centers) / len(centers))
        cy = int(sum(c[1] for c in centers) / len(centers))
        
        return (cx, cy)
    
    def _draw_cross(self, x, y):
        """
        Draw a cross marker at the specified location.
        
        Args:
            x, y: coordinates
        """
        size = self.axis_size
        self.axes.plot((x - size, x + size), (y - size, y + size), color='r')
        self.axes.plot((x - size, x + size), (y + size, y - size), color='r')
    
    def _calculate_perpendiculars(self):
        """Calculate perpendicular bisector lines for all chord pairs."""
        self.chord_lines = []
        points = self.chord_points
        
        for i, p1 in enumerate(points):
            for p2 in points[i + 1:]:
                slope, cent = get_perpendicular_line_homogeneous(p1, p2)
                
                if slope == float('inf'):
                    # Vertical line: store as (inf, x_position)
                    self.chord_lines.append([slope, cent[0]])
                else:
                    # Non-vertical: y = mx + b, store as (m, b)
                    # b = y - mx
                    b = cent[1] - slope * cent[0]
                    self.chord_lines.append([slope, b])
    
    def _draw_perpendiculars(self):
        """Draw all perpendicular bisector lines."""
        self._calculate_perpendiculars()
        
        for slope, param in self.chord_lines:
            if slope == float('inf'):
                # Vertical line: x = param
                y_vals = np.array(self.axes.get_ylim())
                x_vals = param + np.zeros(y_vals.shape)
            else:
                # Non-vertical line: y = slope*x + param
                x_vals = np.array(self.axes.get_xlim())
                y_vals = slope * x_vals + param
            
            self.axes.plot(x_vals, y_vals, linestyle='dashed', color='b')

