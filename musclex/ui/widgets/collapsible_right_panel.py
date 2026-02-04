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

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QPushButton, QScrollArea, QFrame, QSizePolicy
from PySide6.QtCore import Qt, QPropertyAnimation, QEasingCurve, Signal, QSettings
from PySide6.QtGui import QIcon


class CollapsibleRightPanel(QWidget):
    """
    Collapsible right panel with show/hide button and smooth animations.
    
    This widget provides a standard right panel that can be collapsed to save space.
    It includes:
    - Toggle button to show/hide the panel
    - Smooth animation when expanding/collapsing
    - State persistence using QSettings
    - Scrollable content area
    
    Layout structure:
    ┌──────────────────┐
    │ [<< Button]      │  ← Toggle button (always visible)
    ├──────────────────┤
    │ [Scroll Area]    │  ← Content area (collapsible)
    │   - Widget 1     │
    │   - Widget 2     │
    │   - ...          │
    └──────────────────┘
    
    Signals:
        visibilityChanged(visible): Emitted when panel visibility changes
    
    Usage:
        # Create panel
        panel = CollapsibleRightPanel(settings_key="myapp/right_panel")
        
        # Add widgets
        panel.add_widget(displayOptionsPanel)
        panel.add_widget(quadrantOptionsGroupBox)
        
        # Get container for adding more controls
        layout = panel.get_content_layout()
        layout.addWidget(my_widget)
    """
    
    # Signals
    visibilityChanged = Signal(bool)  # is_visible
    
    def __init__(self, parent=None, title="Options", settings_key=None, 
                 start_visible=True, animation_duration=200, show_toggle_internally=True):
        """
        Initialize the collapsible right panel.
        
        Args:
            parent: Parent widget
            title: Panel title (shown in button hint)
            settings_key: QSettings key for state persistence (e.g., "quadrant/right_panel")
            start_visible: Initial visibility if no saved state
            animation_duration: Animation duration in milliseconds
            show_toggle_internally: If True, toggle button is added to panel's layout.
                                   If False, button is created but not added (for external placement)
        """
        super().__init__(parent)
        
        self.title = title
        self.settings_key = settings_key
        self.animation_duration = animation_duration
        self.show_toggle_internally = show_toggle_internally
        
        # Track last "expanded" fixed width so we can collapse/expand cleanly
        # when toggle button is inside the panel layout (scheme A).
        self._expanded_fixed_width = None
        
        # Main layout
        self.main_layout = QVBoxLayout(self)
        self.main_layout.setContentsMargins(0, 0, 0, 0)
        self.main_layout.setSpacing(0)
        
        # Setup UI
        self._setup_ui()
        
        # Always start with specified visibility (default: expanded).
        # We no longer restore saved collapsed state to avoid confusing initial layouts.
        self.set_visible(start_visible, animate=False)

    def setFixedWidth(self, w: int):
        """
        Override to remember the intended expanded width.
        This enables internal-toggle collapse to temporarily shrink the panel and
        restore it when expanded again.
        """
        self._expanded_fixed_width = w
        super().setFixedWidth(w)
    
    def _setup_ui(self):
        """Create UI components."""
        # Ensure the panel stays full-height in parent layouts even when collapsed.
        # This keeps the toggle button anchored at the top rather than having the
        # entire panel widget vertically centered when its contents are hidden.
        self.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Expanding)

        # Toggle button - small and compact, fixed in top-right
        self.toggle_btn = QPushButton()
        self.toggle_btn.setCheckable(True)
        self.toggle_btn.setChecked(True)  # Start as visible
        self.toggle_btn.setToolTip("Hide panel")
        self.toggle_btn.setFixedSize(50, 25)  # Small fixed size
        self.toggle_btn.clicked.connect(self._on_toggle_clicked)
        self._update_button_text()
        
        # Content area (scrollable)
        self.scroll_area = QScrollArea()
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setFrameShape(QFrame.NoFrame)
        self.scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        self.scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAsNeeded)
        
        # Content container
        self.content_widget = QWidget()
        self.content_layout = QVBoxLayout(self.content_widget)
        self.content_layout.setContentsMargins(5, 5, 5, 5)
        self.content_layout.setSpacing(5)
        self.content_layout.addStretch()  # Push content to top
        
        self.scroll_area.setWidget(self.content_widget)
        
        # Fixed bottom area (for navigation controls, always visible)
        self.bottom_widget = QWidget()
        self.bottom_layout = QVBoxLayout(self.bottom_widget)
        self.bottom_layout.setContentsMargins(5, 5, 5, 5)
        self.bottom_layout.setSpacing(5)
        
        # Add to main layout
        # Only add toggle button if it should be shown internally
        if self.show_toggle_internally:
            # Create a horizontal layout for the button to align it to the right
            button_layout = QHBoxLayout()
            button_layout.setContentsMargins(0, 0, 0, 0)
            button_layout.addStretch()  # Push button to the right
            button_layout.addWidget(self.toggle_btn)
            self.main_layout.addLayout(button_layout)
        self.main_layout.addWidget(self.scroll_area, 1)  # Stretch factor 1 - can grow
        self.main_layout.addWidget(self.bottom_widget, 0)  # Stretch factor 0 - fixed size
        
        # Spacer widget: only visible when collapsed (internal toggle mode).
        # Pushes the toggle button to the top when content is hidden.
        self._collapsed_spacer = QWidget()
        self._collapsed_spacer.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Expanding)
        self._collapsed_spacer.setVisible(False)  # Hidden by default (panel starts expanded)
        self.main_layout.addWidget(self._collapsed_spacer, 1)
        
        # Animation for smooth collapse/expand
        self.animation = QPropertyAnimation(self.scroll_area, b"maximumHeight")
        self.animation.setDuration(self.animation_duration)
        self.animation.setEasingCurve(QEasingCurve.InOutQuad)
    
    def _update_button_text(self):
        """Update button text based on state."""
        if self.toggle_btn.isChecked():
            self.toggle_btn.setText("<<")  # Just arrow, compact
            self.toggle_btn.setToolTip(f"Hide {self.title}")
        else:
            self.toggle_btn.setText(">>")  # Just arrow, compact
            self.toggle_btn.setToolTip(f"Show {self.title}")
    
    def _on_toggle_clicked(self):
        """Handle toggle button click."""
        is_visible = self.toggle_btn.isChecked()
        self.set_visible(is_visible, animate=True)
    
    def set_visible(self, visible, animate=True):
        """
        Set panel visibility (show/hide entire panel).
        
        Args:
            visible: True to show, False to hide
            animate: Whether to use animation
        """
        # Update button state
        self.toggle_btn.setChecked(visible)
        self._update_button_text()
        
        # Visibility behavior depends on toggle placement:
        # - Internal toggle (show_toggle_internally=True): keep panel visible so the
        #   user can always re-open it; collapse only hides content areas.
        # - External floating toggle: hide/show entire panel widget.
        if self.show_toggle_internally:
            self.show()
            self.scroll_area.setVisible(visible)
            self.bottom_widget.setVisible(visible)
            # Show spacer only when collapsed to keep button at top
            self._collapsed_spacer.setVisible(not visible)
            
            # Shrink panel when collapsed so it actually frees screen space
            if visible:
                if self._expanded_fixed_width is not None:
                    super().setFixedWidth(self._expanded_fixed_width)
            else:
                # Minimal width to show the button with a little padding
                super().setFixedWidth(self.toggle_btn.width() + 10)
        else:
            if visible:
                self.show()
            else:
                self.hide()
        
        # Save state
        if self.settings_key:
            settings = QSettings()
            settings.setValue(self.settings_key, visible)
        
        # Emit signal
        self.visibilityChanged.emit(visible)
    
    def is_visible(self):
        """Check if panel content is visible."""
        return self.toggle_btn.isChecked()
    
    # ===== Public API =====
    
    def add_widget(self, widget):
        """
        Add a widget to the content area.
        
        Args:
            widget: QWidget to add
        """
        # Insert before the stretch
        self.content_layout.insertWidget(self.content_layout.count() - 1, widget)
    
    def add_widgets(self, widgets):
        """
        Add multiple widgets to the content area.
        
        Args:
            widgets: List of widgets
        """
        for widget in widgets:
            self.add_widget(widget)
    
    def add_bottom_widget(self, widget):
        """
        Add a widget to the fixed bottom area (outside scroll area).
        This area is always visible and doesn't scroll.
        
        Args:
            widget: QWidget to add to bottom area
        """
        self.bottom_layout.addWidget(widget)
    
    def get_content_layout(self):
        """
        Get the content layout for direct manipulation.
        
        Returns:
            QVBoxLayout of content area
        """
        return self.content_layout
    
    def get_bottom_layout(self):
        """
        Get the bottom layout for direct manipulation.
        
        Returns:
            QVBoxLayout of bottom fixed area
        """
        return self.bottom_layout
    
    def clear_content(self):
        """Remove all widgets from content area."""
        while self.content_layout.count() > 1:  # Keep the stretch
            child = self.content_layout.takeAt(0)
            if child.widget():
                child.widget().deleteLater()
    
    def set_scroll_policy(self, horizontal, vertical):
        """
        Set scroll bar policies.
        
        Args:
            horizontal: Qt.ScrollBarPolicy (e.g., Qt.ScrollBarAsNeeded)
            vertical: Qt.ScrollBarPolicy
        """
        self.scroll_area.setHorizontalScrollBarPolicy(horizontal)
        self.scroll_area.setVerticalScrollBarPolicy(vertical)

