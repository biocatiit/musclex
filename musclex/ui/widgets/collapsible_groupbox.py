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

from PySide6.QtWidgets import QGroupBox, QVBoxLayout, QWidget, QToolButton, QSizePolicy
from PySide6.QtCore import Qt, Signal


class CollapsibleGroupBox(QGroupBox):
    """
    A collapsible QGroupBox with a toggle button in the title area.
    
    This widget provides a GroupBox that can be collapsed/expanded by clicking
    a toggle button (arrow icon) displayed at the left of the title.
    
    Features:
    - Toggle button with arrow icon (▶ collapsed, ▼ expanded)
    - Instant collapse/expand (no animation for better performance)
    - Can be used as a drop-in replacement for QGroupBox
    - Maintains all standard QGroupBox features
    
    Layout structure:
    ┌─ ▼ Title ──────────┐
    │                     │
    │   Content Widget    │
    │                     │
    └─────────────────────┘
    
    When collapsed:
    ┌─ ▶ Title ──────────┐
    └─────────────────────┘
    
    Signals:
        toggled(expanded): Emitted when the box is expanded/collapsed
    
    Usage:
        # Create collapsible group box
        box = CollapsibleGroupBox("My Settings")
        
        # Add widgets using standard QGroupBox methods
        layout = QVBoxLayout()
        layout.addWidget(myWidget1)
        layout.addWidget(myWidget2)
        box.setLayout(layout)
        
        # Or use convenience method
        box.set_content_layout(layout)
        
        # Connect to collapse/expand events
        box.toggled.connect(lambda expanded: print(f"Box is {'expanded' if expanded else 'collapsed'}"))
        
        # Control programmatically
        box.set_expanded(False)  # Collapse
        box.set_expanded(True)   # Expand
    """
    
    # Signal emitted when expanded/collapsed state changes
    toggled = Signal(bool)  # expanded
    
    def __init__(self, title="", parent=None, start_expanded=True):
        """
        Initialize the collapsible group box.
        
        Args:
            title: Title text displayed in the group box header
            parent: Parent widget
            start_expanded: Initial state (True = expanded, False = collapsed)
        """
        super().__init__(title, parent)
        
        self._is_expanded = start_expanded
        
        # Keep the standard bold title style and add left padding for arrow button
        # Only pad the title text, not the whole group box
        self.setStyleSheet("QGroupBox { font-weight: bold; } QGroupBox::title { padding-left: 22px; }")
        
        # Create toggle button (will be positioned in title area)
        self.toggle_button = QToolButton()
        self.toggle_button.setCheckable(True)
        self.toggle_button.setChecked(start_expanded)
        self.toggle_button.setToolTip("Click to collapse/expand")
        self.toggle_button.setAutoRaise(True)
        self.toggle_button.setFixedSize(16, 16)
        
        # Set arrow icon based on state
        self._update_arrow()
        
        # Connect button
        self.toggle_button.clicked.connect(self._on_toggle_clicked)
        
        # Make button a child of this widget so it appears in the title
        self.toggle_button.setParent(self)
        self.toggle_button.move(3, 0)  # Position at left of title, aligned with title baseline
        
        # Content widget and layout will be set by user
        self.content_widget = None
        self.content_layout = None
    
    def setLayout(self, layout):
        """
        Override setLayout to wrap content for collapsible support.
        
        Args:
            layout: QLayout to set as content
        """
        # Create a container widget for the content
        if self.content_widget is None:
            self.content_widget = QWidget()
            self.content_widget.setParent(self)
        
        self.content_widget.setLayout(layout)
        self.content_layout = layout
        
        # Call parent to set up the basic structure
        container_layout = QVBoxLayout()
        container_layout.setContentsMargins(0, 0, 0, 0)
        container_layout.setSpacing(0)
        container_layout.addWidget(self.content_widget)
        
        super().setLayout(container_layout)
        
        # Apply initial state
        if not self._is_expanded:
            self.content_widget.setMaximumHeight(0)
            self.content_widget.setVisible(False)
            # Apply collapsed style
            self.setStyleSheet("QGroupBox { font-weight: bold; margin-top: 0px; border: 0px; padding: 0px; } QGroupBox::title { padding-left: 22px; }")
    
    def set_content_layout(self, layout):
        """
        Convenience method to set the content layout.
        Same as setLayout() but more explicit.
        
        Args:
            layout: QLayout to set as content
        """
        self.setLayout(layout)
    
    def _update_arrow(self):
        """Update the arrow icon based on expanded state."""
        if self._is_expanded:
            # Down arrow (expanded)
            self.toggle_button.setArrowType(Qt.DownArrow)
        else:
            # Right arrow (collapsed)
            self.toggle_button.setArrowType(Qt.RightArrow)
    
    def _on_toggle_clicked(self):
        """Handle toggle button click."""
        self.set_expanded(not self._is_expanded)
    
    def set_expanded(self, expanded):
        """
        Set the expanded/collapsed state.
        
        Args:
            expanded: True to expand, False to collapse
        """
        if self.content_widget is None:
            # No content yet, just store the state
            self._is_expanded = expanded
            self.toggle_button.setChecked(expanded)
            self._update_arrow()
            return
        
        if self._is_expanded == expanded:
            return  # Already in desired state
        
        self._is_expanded = expanded
        self.toggle_button.setChecked(expanded)
        self._update_arrow()
        
        # Update stylesheet to hide/show border and margins when collapsed
        if expanded:
            # Restore normal style with padding for title
            self.setStyleSheet("QGroupBox { font-weight: bold; } QGroupBox::title { padding-left: 22px; }")
            # Show content
            self.content_widget.setMaximumHeight(16777215)  # Qt max
            self.content_widget.setVisible(True)
        else:
            # Collapsed: remove margins and minimize visual space
            self.setStyleSheet("QGroupBox { font-weight: bold; margin-top: 0px; border: 0px; padding: 0px; } QGroupBox::title { padding-left: 22px; }")
            # Hide content
            self.content_widget.setMaximumHeight(0)
            self.content_widget.setVisible(False)
        
        # Emit signal
        self.toggled.emit(expanded)
    
    def is_expanded(self):
        """
        Check if the box is expanded.
        
        Returns:
            bool: True if expanded, False if collapsed
        """
        return self._is_expanded
    
    def resizeEvent(self, event):
        """Override to reposition toggle button on resize."""
        super().resizeEvent(event)
        # Keep button at left of title, aligned with title baseline
        self.toggle_button.move(3, 0)

