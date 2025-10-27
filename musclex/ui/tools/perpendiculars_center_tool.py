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

from .interaction_tool import InteractionTool
from ...utils.image_processor import calcSlope, getIntersectionOfTwoLines


class PerpendicularsCenterTool(InteractionTool):
    """
    Tool for finding image center using perpendicular lines through peaks.
    
    Workflow:
        1. User clicks pairs of points (2 points at a time)
        2. Tool draws lines between each pair and perpendiculars through them
        3. Tool calculates intersections of vertical and horizontal lines
        4. Returns center as average of all intersections
    
    This method is useful when you have clear horizontal and vertical features
    (like diffraction peaks) that should intersect at the center.
    
    Usage:
        tool = PerpendicularsCenterTool(axes, canvas)
        tool.activate()
        # ... user clicks point pairs ...
        center = tool.get_result()  # Returns (x, y) tuple
    """
    
    def __init__(self, axes, canvas):
        super().__init__(axes, canvas)
        self.points = []  # List of all clicked points
        self.axis_size = 5  # Size of cross markers
    
    def _on_activate(self):
        """Reset state and clear axes when tool is activated."""
        self.points = []
        self.clear_axes()
    
    def _on_deactivate(self):
        """Clean up when tool is deactivated."""
        self.clear_axes()
    
    def handle_click(self, event) -> bool:
        """Not used - we handle release instead."""
        return False
    
    def handle_motion(self, event) -> bool:
        """
        Draw preview line when hovering after clicking odd number of points.
        
        Args:
            event: matplotlib mouse motion event
            
        Returns:
            True if event was handled
        """
        if not self.is_active:
            return False
        
        if event.inaxes != self.axes:
            return False
        
        # Only draw preview if we have an odd number of points
        # (waiting for the second point of a pair)
        if len(self.points) % 2 == 1:
            x, y = event.xdata, event.ydata
            start_pt = self.points[-1]
            
            # Remove old preview line
            self.remove_labeled_items(['preview_line'])
            
            # Draw preview line from last point to current cursor
            self.axes.plot([start_pt[0], x], [start_pt[1], y], 
                          color='r', linestyle='--', alpha=0.5, 
                          label='preview_line')
            self.canvas.draw_idle()
            return True
        
        return False
    
    def handle_release(self, event) -> bool:
        """
        Handle mouse button release - add a point.
        
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
        self.points.append((x, y))
        
        # Draw cross marker at click location
        self._draw_cross(x, y)
        
        # If this completes a pair, draw the line between them
        if len(self.points) % 2 == 0:
            # Remove preview line
            self.remove_labeled_items(['preview_line'])
            
            # Draw final line between the pair
            pt1 = self.points[-2]
            pt2 = self.points[-1]
            self.axes.plot([pt1[0], pt2[0]], [pt1[1], pt2[1]], 
                          color='r', linewidth=2)
        
        self.canvas.draw_idle()
        return True
    
    def get_result(self):
        """
        Calculate and return the center point.
        
        The algorithm:
        1. Group points into pairs (lines)
        2. Classify lines as vertical (steep slope) or horizontal (shallow slope)
        3. Find intersections between vertical and horizontal lines
        4. Return average of all intersections
        
        Returns:
            (x, y) tuple of center coordinates, or None if insufficient points
        """
        # Need at least 2 pairs (4 points) to find intersections
        if len(self.points) < 4:
            return None
        
        # Group points into pairs
        lines = []
        for i in range(0, len(self.points) - 1, 2):
            if i + 1 < len(self.points):
                lines.append((self.points[i], self.points[i + 1]))
        
        # Classify lines as horizontal or vertical based on slope
        horizontal_lines = []
        vertical_lines = []
        
        for line in lines:
            slope = calcSlope(line[0], line[1])
            if abs(slope) > 1:
                # Steep slope = more vertical
                vertical_lines.append(line)
            else:
                # Shallow slope = more horizontal
                horizontal_lines.append(line)
        
        # Find all intersections between vertical and horizontal lines
        intersections = []
        for v_line in vertical_lines:
            for h_line in horizontal_lines:
                try:
                    cx, cy = getIntersectionOfTwoLines(h_line, v_line)
                    intersections.append((cx, cy))
                except:
                    # Lines might be parallel or calculation failed
                    continue
        
        if not intersections:
            return None
        
        # Return average of all intersection points
        cx = int(sum(pt[0] for pt in intersections) / len(intersections))
        cy = int(sum(pt[1] for pt in intersections) / len(intersections))
        
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

