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

from abc import ABC, abstractmethod


class InteractionTool(ABC):
    """
    Base class for interactive tools that handle mouse events on matplotlib axes.
    
    Tools are pure logic handlers - they do NOT inherit from QWidget.
    They process mouse events and modify the axes/canvas accordingly.
    
    Lifecycle:
        1. Tool is created and registered with ToolManager
        2. activate() is called when user selects the tool
        3. handle_click/motion/release process mouse events
        4. get_result() returns the tool's output (e.g., calculated center point)
        5. deactivate() is called when user switches to another tool
    """
    
    def __init__(self, axes, canvas):
        """
        Initialize the tool.
        
        Args:
            axes: matplotlib axes object where the tool draws
            canvas: matplotlib canvas object to trigger redraws
        """
        self.axes = axes
        self.canvas = canvas
        self.is_active = False
    
    def activate(self):
        """
        Activate the tool.
        Called when user selects this tool (e.g., clicks a button).
        """
        self.is_active = True
        self._on_activate()
    
    def deactivate(self):
        """
        Deactivate the tool.
        Called when user switches to another tool or cancels.
        """
        self.is_active = False
        self._on_deactivate()
    
    @abstractmethod
    def _on_activate(self):
        """
        Hook for subclasses: called during activate().
        Use this to reset state, clear axes, etc.
        """
        pass
    
    @abstractmethod
    def _on_deactivate(self):
        """
        Hook for subclasses: called during deactivate().
        Use this to clean up, remove drawings, etc.
        """
        pass
    
    @abstractmethod
    def handle_click(self, event) -> bool:
        """
        Handle mouse button press event.
        
        Args:
            event: matplotlib mouse button press event
            
        Returns:
            True if event was handled, False otherwise
        """
        pass
    
    @abstractmethod
    def handle_motion(self, event) -> bool:
        """
        Handle mouse motion event.
        
        Args:
            event: matplotlib mouse motion event
            
        Returns:
            True if event was handled, False otherwise
        """
        pass
    
    @abstractmethod
    def handle_release(self, event) -> bool:
        """
        Handle mouse button release event.
        
        Args:
            event: matplotlib mouse button release event
            
        Returns:
            True if event was handled, False otherwise
        """
        pass
    
    def get_result(self):
        """
        Get the result of the tool's operation.
        
        Returns:
            Tool-specific result (e.g., center point tuple, angle float, etc.)
            or None if no valid result
        """
        return None
    
    def clear_axes(self):
        """
        Remove all lines and patches from axes.
        Utility method for subclasses.
        """
        # Remove all lines
        for line in self.axes.lines[:]:
            line.remove()
        
        # Remove all patches
        for patch in self.axes.patches[:]:
            patch.remove()
        
        # Redraw
        self.canvas.draw_idle()
    
    def remove_labeled_items(self, labels):
        """
        Remove only items with specific labels.
        
        Args:
            labels: list of label strings to remove
        """
        # Remove matching lines
        for line in self.axes.lines[:]:
            if line.get_label() in labels:
                line.remove()
        
        # Remove matching patches
        for patch in self.axes.patches[:]:
            if patch.get_label() in labels:
                patch.remove()
        
        self.canvas.draw_idle()

