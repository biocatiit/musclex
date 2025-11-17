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

from PySide6.QtWidgets import (QGroupBox, QGridLayout, QVBoxLayout, QLabel, 
                               QDoubleSpinBox, QCheckBox, QPushButton,
                               QComboBox)
from PySide6.QtCore import Signal


class DisplayOptionsPanel(QGroupBox):
    """
    Reusable display options panel for image viewing with slot system.
    
    This is a pure View component that provides common display controls:
    - Intensity range (min/max)
    - Log scale toggle
    - Persist intensities (optional)
    - Color map selector
    - Zoom controls (Zoom In button + Full button)
    - Extensible slots for custom controls (top and bottom)
    
    Layout structure:
    ┌─────────────────────────────┐
    │ [Top Slot - VBoxLayout]     │  ← Custom controls before basics
    ├─────────────────────────────┤
    │ [Basic Controls]            │  ← Fixed standard controls
    │  Min/Max Intensity          │
    │  Zoom buttons               │
    │  Log scale, Persist         │
    ├─────────────────────────────┤
    │ [Bottom Slot - VBoxLayout]  │  ← Custom controls after basics
    │  - Double Zoom (optional)   │
    └─────────────────────────────┘
    
    Signals:
        intensityChanged(vmin, vmax): Intensity values changed
        logScaleChanged(enabled): Log scale toggled
        persistChanged(enabled): Persist toggled
        colorMapChanged(colormap): Color map changed
        zoomInRequested(): Zoom In button clicked
        zoomOutRequested(): Full button clicked
    
    Usage:
        panel = DisplayOptionsPanel()
        panel.intensityChanged.connect(my_handler)
        
        # Add custom controls to slots
        panel.add_to_bottom_slot(myCheckBox)
    """
    
    # Signals
    intensityChanged = Signal(float, float)  # vmin, vmax
    logScaleChanged = Signal(bool)  # log_scale enabled
    persistChanged = Signal(bool)  # persist enabled
    colorMapChanged = Signal(str)  # colormap name
    zoomInRequested = Signal()  # Zoom In button clicked
    zoomOutRequested = Signal()  # Full button clicked
    
    def __init__(self, parent=None, show_persist=True, show_colormap=True, show_double_zoom=False, double_zoom_widget=None):
        """
        Initialize the display options panel.
        
        Args:
            parent: Parent widget
            show_persist: Whether to show "Persist intensities" checkbox
            show_colormap: Whether to show color map selector
            show_double_zoom: Whether to show double zoom widget
            double_zoom_widget: The DoubleZoomWidget instance to display (if show_double_zoom is True)
        """
        super().__init__("Display Options", parent)
        
        self._show_persist = show_persist
        self._show_colormap = show_colormap
        self._show_double_zoom = show_double_zoom
        self.double_zoom_widget = double_zoom_widget
        
        # Main layout: VBoxLayout to hold slots and basic controls
        self.main_layout = QVBoxLayout(self)
        self.main_layout.setSpacing(5)
        
        # Slot containers
        self.top_slot_layout = QVBoxLayout()
        self.top_slot_layout.setSpacing(3)
        self.bottom_slot_layout = QVBoxLayout()
        self.bottom_slot_layout.setSpacing(3)
        
        self._setup_ui()
        self._setup_connections()
    
    def _setup_ui(self):
        """Create and layout UI elements: Top Slot -> Basic Controls -> Bottom Slot"""
        # ===== Top Slot (for custom controls before basics) =====
        self.main_layout.addLayout(self.top_slot_layout)
        
        # ===== Basic Controls (fixed standard controls) =====
        basic_layout = self._create_basic_controls()
        self.main_layout.addLayout(basic_layout)
        
        # ===== Bottom Slot (for custom controls after basics) =====
        self.main_layout.addLayout(self.bottom_slot_layout)
        
        # Double zoom (if requested, automatically add to bottom slot)
        if self._show_double_zoom and self.double_zoom_widget:
            self.bottom_slot_layout.addWidget(self.double_zoom_widget)
    
    def _create_basic_controls(self):
        """Create the basic controls area (using GridLayout for compact layout)"""
        layout = QGridLayout()
        layout.setSpacing(5)
        
        # Min intensity
        self.minIntLabel = QLabel("Min Intensity:")
        self.minIntSpnBx = QDoubleSpinBox()
        self.minIntSpnBx.setRange(-1e10, 1e10)  # Allow any value for scientific data
        self.minIntSpnBx.setDecimals(2)
        self.minIntSpnBx.setSingleStep(5)
        self.minIntSpnBx.setKeyboardTracking(False)
        self.minIntSpnBx.setToolTip("Minimum intensity value to display")
        
        # Max intensity
        self.maxIntLabel = QLabel("Max Intensity:")
        self.maxIntSpnBx = QDoubleSpinBox()
        self.maxIntSpnBx.setRange(-1e10, 1e10)  # Allow any value for scientific data
        self.maxIntSpnBx.setDecimals(2)
        self.maxIntSpnBx.setSingleStep(5)
        self.maxIntSpnBx.setKeyboardTracking(False)
        self.maxIntSpnBx.setToolTip("Maximum intensity value to display")
        
        # Zoom buttons
        self.zoomInBtn = QPushButton("Zoom In")
        self.zoomInBtn.setCheckable(True)
        self.zoomOutBtn = QPushButton("Full")
        
        # Color map selector (optional)
        if self._show_colormap:
            self.colorMapLabel = QLabel("Color Map:")
            self.colorMapCombo = QComboBox()
            self.colorMapCombo.addItems(['gray', 'viridis', 'plasma', 'inferno', 
                                         'magma', 'cividis', 'hot', 'cool', 'jet'])
        
        # Log scale checkbox
        self.logScaleChkBx = QCheckBox("Log scale intensity")
        
        # Persist checkbox (optional)
        if self._show_persist:
            self.persistChkBx = QCheckBox("Persist intensities")
        
        # Layout
        row = 0
        layout.addWidget(self.minIntLabel, row, 0, 1, 2)
        layout.addWidget(self.maxIntLabel, row, 2, 1, 2)
        
        row += 1
        layout.addWidget(self.minIntSpnBx, row, 0, 1, 2)
        layout.addWidget(self.maxIntSpnBx, row, 2, 1, 2)
        
        row += 1
        layout.addWidget(self.zoomInBtn, row, 0, 1, 2)
        layout.addWidget(self.zoomOutBtn, row, 2, 1, 2)
        
        if self._show_colormap:
            row += 1
            layout.addWidget(self.colorMapLabel, row, 0, 1, 2)
            layout.addWidget(self.colorMapCombo, row, 2, 1, 2)
        
        row += 1
        layout.addWidget(self.logScaleChkBx, row, 0, 1, 2)
        
        if self._show_persist:
            layout.addWidget(self.persistChkBx, row, 2, 1, 2)
        
        return layout
    
    def _setup_connections(self):
        """Connect internal signals."""
        # Intensity changes
        self.minIntSpnBx.valueChanged.connect(self._on_intensity_changed)
        self.maxIntSpnBx.valueChanged.connect(self._on_intensity_changed)
        
        # Checkboxes
        self.logScaleChkBx.stateChanged.connect(
            lambda: self.logScaleChanged.emit(self.logScaleChkBx.isChecked())
        )
        
        if self._show_persist:
            self.persistChkBx.stateChanged.connect(
                lambda: self.persistChanged.emit(self.persistChkBx.isChecked())
            )
        
        # Color map
        if self._show_colormap:
            self.colorMapCombo.currentTextChanged.connect(
                lambda text: self.colorMapChanged.emit(text)
            )
        
        # Zoom buttons
        self.zoomInBtn.clicked.connect(self._on_zoom_in_clicked)
        self.zoomOutBtn.clicked.connect(lambda: self.zoomOutRequested.emit())
    
    def _on_intensity_changed(self):
        """Internal handler: collect values and emit signal."""
        self.intensityChanged.emit(
            self.minIntSpnBx.value(),
            self.maxIntSpnBx.value()
        )
    
    def _on_zoom_in_clicked(self):
        """Handle Zoom In button click."""
        # Emit signal for external handling
        self.zoomInRequested.emit()
    
    # ===== Public API =====
    
    def set_intensity_range(self, vmin, vmax):
        """
        Set the range of intensity values.
        
        Args:
            vmin: Minimum possible value
            vmax: Maximum possible value
        """
        self.minIntSpnBx.setRange(vmin, vmax)
        self.maxIntSpnBx.setRange(vmin, vmax)
        
        # Update labels to show range
        self.minIntLabel.setText(f"Min Intensity ({vmin}):")
        self.maxIntLabel.setText(f"Max Intensity ({vmax}):")
    
    def set_intensity_values(self, vmin, vmax):
        """
        Set the current intensity values.
        
        Args:
            vmin: Minimum intensity value
            vmax: Maximum intensity value
        """
        # Block signals to avoid triggering during programmatic changes
        self.minIntSpnBx.blockSignals(True)
        self.maxIntSpnBx.blockSignals(True)
        
        self.minIntSpnBx.setValue(vmin)
        self.maxIntSpnBx.setValue(vmax)
        
        self.minIntSpnBx.blockSignals(False)
        self.maxIntSpnBx.blockSignals(False)
    
    def set_intensity_step(self, step):
        """
        Set the step size for intensity spin boxes.
        
        Args:
            step: Step size for increment/decrement
        """
        self.minIntSpnBx.setSingleStep(step)
        self.maxIntSpnBx.setSingleStep(step)
    
    def get_intensity_values(self):
        """
        Get current intensity values.
        
        Returns:
            Tuple of (vmin, vmax)
        """
        return (self.minIntSpnBx.value(), self.maxIntSpnBx.value())
    
    def set_log_scale(self, enabled):
        """Set log scale checkbox state."""
        self.logScaleChkBx.setChecked(enabled)
    
    def is_log_scale(self):
        """Check if log scale is enabled."""
        return self.logScaleChkBx.isChecked()
    
    def set_persist(self, enabled):
        """Set persist checkbox state."""
        if self._show_persist:
            self.persistChkBx.setChecked(enabled)
    
    def is_persist_enabled(self):
        """Check if persist is enabled."""
        if self._show_persist:
            return self.persistChkBx.isChecked()
        return False
    
    def set_color_map(self, colormap):
        """Set the current color map."""
        if self._show_colormap:
            index = self.colorMapCombo.findText(colormap)
            if index >= 0:
                self.colorMapCombo.setCurrentIndex(index)
    
    def get_color_map(self):
        """Get current color map name."""
        if self._show_colormap:
            return self.colorMapCombo.currentText()
        return 'gray'
    
    def set_zoom_in_checked(self, checked):
        """Set Zoom In button checked state (for external sync)."""
        self.zoomInBtn.setChecked(checked)
    
    def is_zoom_in_checked(self):
        """Check if Zoom In button is checked."""
        return self.zoomInBtn.isChecked()
    
    def get_all_values(self):
        """
        Get all display option values.
        
        Returns:
            Dict with all settings
        """
        result = {
            'vmin': self.minIntSpnBx.value(),
            'vmax': self.maxIntSpnBx.value(),
            'log_scale': self.logScaleChkBx.isChecked(),
            'colormap': self.get_color_map(),
            'zoom_in_active': self.zoomInBtn.isChecked()
        }
        
        if self._show_persist:
            result['persist'] = self.persistChkBx.isChecked()
        
        return result
    
    # ===== Slot System API =====
    
    def add_to_top_slot(self, widget):
        """
        Add a widget to the top slot (before basic controls).
        
        Args:
            widget: QWidget to add
        """
        self.top_slot_layout.addWidget(widget)
    
    def add_to_bottom_slot(self, widget):
        """
        Add a widget to the bottom slot (after basic controls).
        
        Args:
            widget: QWidget to add
        """
        self.bottom_slot_layout.addWidget(widget)
    
    def add_custom_controls(self, widgets, position='bottom'):
        """
        Add multiple widgets to a slot.
        
        Args:
            widgets: List of widgets or single widget
            position: 'top' or 'bottom'
        """
        if not isinstance(widgets, (list, tuple)):
            widgets = [widgets]
        
        for widget in widgets:
            if position == 'top':
                self.add_to_top_slot(widget)
            else:
                self.add_to_bottom_slot(widget)
    
    def clear_top_slot(self):
        """Remove all widgets from the top slot."""
        while self.top_slot_layout.count():
            child = self.top_slot_layout.takeAt(0)
            if child.widget():
                child.widget().deleteLater()
    
    def clear_bottom_slot(self):
        """
        Remove all widgets from the bottom slot.
        Note: Preserves double_zoom if it was added.
        """
        while self.bottom_slot_layout.count():
            child = self.bottom_slot_layout.takeAt(0)
            widget = child.widget()
            if widget and widget != self.double_zoom_widget:
                widget.deleteLater()

