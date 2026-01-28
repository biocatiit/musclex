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


class PlaceholderTool(InteractionTool):
    """
    A placeholder tool that blocks ImageViewerWidget's built-in left-click panning.
    
    This tool is used when ProjectionTracesGUI has its own active operation
    (like drawing a box) that manages events through self.function state.
    
    The tool doesn't handle any events itself - it just exists to tell
    ImageViewerWidget that an operation is in progress, preventing the
    built-in left-click pan from activating.
    """
    
    def _on_activate(self):
        """No-op: tool has no visual elements."""
        pass
    
    def _on_deactivate(self):
        """No-op: tool has no visual elements to clean up."""
        pass
    
    def handle_click(self, event) -> bool:
        """
        Don't handle click events - let them pass through to ProjectionTracesGUI.
        
        Returns False so the event propagates to external handlers.
        """
        return False
    
    def handle_motion(self, event) -> bool:
        """
        Don't handle motion events - let them pass through to ProjectionTracesGUI.
        
        Returns False so the event propagates to external handlers.
        """
        return False
    
    def handle_release(self, event) -> bool:
        """
        Don't handle release events - let them pass through to ProjectionTracesGUI.
        
        Returns False so the event propagates to external handlers.
        """
        return False
