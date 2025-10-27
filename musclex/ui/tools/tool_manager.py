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


class ToolManager:
    """
    Manages interactive tools and dispatches mouse events.
    
    Responsibilities:
        - Register tool classes
        - Activate/deactivate tools (only one active at a time)
        - Dispatch mouse events to the active tool
        - Return tool results
    
    Usage:
        manager = ToolManager(axes, canvas)
        manager.register_tool('chords', ChordsCenterTool)
        manager.activate_tool('chords')
        
        # In mouse event handlers:
        if manager.handle_click(event):
            return  # Event was handled by active tool
    """
    
    def __init__(self, axes, canvas):
        """
        Initialize the tool manager.
        
        Args:
            axes: matplotlib axes object
            canvas: matplotlib canvas object
        """
        self.axes = axes
        self.canvas = canvas
        self.tools = {}  # name -> tool instance
        self.active_tool = None
        self.active_tool_name = None
    
    def register_tool(self, name, tool_class, *args, **kwargs):
        """
        Register a tool class.
        
        Args:
            name: string identifier for the tool
            tool_class: class (not instance) of the tool
            *args, **kwargs: additional arguments passed to tool constructor
        """
        # Instantiate the tool
        self.tools[name] = tool_class(self.axes, self.canvas, *args, **kwargs)
    
    def activate_tool(self, name):
        """
        Activate a tool by name.
        
        Automatically deactivates the currently active tool.
        
        Args:
            name: string identifier of the tool to activate
            
        Returns:
            True if tool was activated, False if tool not found
        """
        if name not in self.tools:
            return False
        
        # Deactivate current tool
        if self.active_tool:
            self.active_tool.deactivate()
        
        # Activate new tool
        self.active_tool = self.tools[name]
        self.active_tool_name = name
        self.active_tool.activate()
        return True
    
    def deactivate_current_tool(self):
        """
        Deactivate the currently active tool.
        
        Returns:
            Tuple of (tool_name, result) or (None, None) if no active tool
        """
        if not self.active_tool:
            return None, None
        
        tool_name = self.active_tool_name
        result = self.active_tool.get_result()
        self.active_tool.deactivate()
        self.active_tool = None
        self.active_tool_name = None
        
        return tool_name, result
    
    def deactivate_tool(self, name):
        """
        Deactivate a specific tool by name.
        
        Args:
            name: string identifier of the tool
            
        Returns:
            The tool's result, or None if tool not active or not found
        """
        if name not in self.tools:
            return None
        
        tool = self.tools[name]
        
        # Only deactivate if it's the active tool
        if tool == self.active_tool:
            _, result = self.deactivate_current_tool()
            return result
        
        return None
    
    def get_active_tool_name(self):
        """
        Get the name of the currently active tool.
        
        Returns:
            Tool name string or None
        """
        return self.active_tool_name
    
    def is_tool_active(self, name):
        """
        Check if a specific tool is active.
        
        Args:
            name: string identifier of the tool
            
        Returns:
            True if this tool is active, False otherwise
        """
        return self.active_tool_name == name
    
    def has_active_tool(self):
        """
        Check if any tool is currently active.
        
        Returns:
            True if a tool is active, False otherwise
        """
        return self.active_tool is not None
    
    # Event dispatching methods
    
    def handle_click(self, event) -> bool:
        """
        Dispatch mouse click event to active tool.
        
        Args:
            event: matplotlib mouse button press event
            
        Returns:
            True if event was handled, False otherwise
        """
        if self.active_tool:
            return self.active_tool.handle_click(event)
        return False
    
    def handle_motion(self, event) -> bool:
        """
        Dispatch mouse motion event to active tool.
        
        Args:
            event: matplotlib mouse motion event
            
        Returns:
            True if event was handled, False otherwise
        """
        if self.active_tool:
            return self.active_tool.handle_motion(event)
        return False
    
    def handle_release(self, event) -> bool:
        """
        Dispatch mouse release event to active tool.
        
        Args:
            event: matplotlib mouse button release event
            
        Returns:
            True if event was handled, False otherwise
        """
        if self.active_tool:
            return self.active_tool.handle_release(event)
        return False

