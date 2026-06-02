"""
Example usage of CollapsibleGroupBox.

This file demonstrates how to use the CollapsibleGroupBox widget
to replace standard QGroupBox with collapsible sections.
"""

from PySide6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout,
                               QLabel, QCheckBox, QPushButton, QSpinBox, QHBoxLayout)
from PySide6.QtCore import Qt
import sys

from collapsible_groupbox import CollapsibleGroupBox


class ExampleWindow(QMainWindow):
    """Example window showing CollapsibleGroupBox usage."""
    
    def __init__(self):
        super().__init__()
        self.setWindowTitle("CollapsibleGroupBox Example")
        self.resize(400, 600)
        
        # Central widget
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        
        # Main layout
        main_layout = QVBoxLayout(central_widget)
        
        # Example 1: Display Options (expanded by default, no animation)
        display_group = CollapsibleGroupBox("Display Options", start_expanded=True)
        display_layout = QVBoxLayout()
        display_layout.addWidget(QLabel("Min Intensity:"))
        display_layout.addWidget(QSpinBox())
        display_layout.addWidget(QLabel("Max Intensity:"))
        display_layout.addWidget(QSpinBox())
        display_layout.addWidget(QCheckBox("Log scale intensity"))
        display_layout.addWidget(QCheckBox("Persist intensities"))
        display_group.setLayout(display_layout)
        
        # Connect to collapse/expand events
        display_group.toggled.connect(
            lambda expanded: print(f"Display Options is {'expanded' if expanded else 'collapsed'}")
        )
        
        main_layout.addWidget(display_group)
        
        # Example 2: Advanced Settings (collapsed by default)
        advanced_group = CollapsibleGroupBox("Advanced Settings", start_expanded=False)
        advanced_layout = QVBoxLayout()
        advanced_layout.addWidget(QLabel("Parameter 1:"))
        advanced_layout.addWidget(QSpinBox())
        advanced_layout.addWidget(QLabel("Parameter 2:"))
        advanced_layout.addWidget(QSpinBox())
        advanced_layout.addWidget(QCheckBox("Enable feature A"))
        advanced_layout.addWidget(QCheckBox("Enable feature B"))
        advanced_group.setLayout(advanced_layout)
        
        main_layout.addWidget(advanced_group)
        
        # Example 3: Background Subtraction
        bg_group = CollapsibleGroupBox("Background Subtraction", start_expanded=True)
        bg_layout = QVBoxLayout()
        bg_layout.addWidget(QLabel("Method: Circularly-symmetric"))
        bg_layout.addWidget(QLabel("R-min:"))
        bg_layout.addWidget(QSpinBox())
        bg_layout.addWidget(QLabel("Pixel Range:"))
        
        pixel_range_layout = QHBoxLayout()
        pixel_range_layout.addWidget(QSpinBox())
        pixel_range_layout.addWidget(QLabel("-"))
        pixel_range_layout.addWidget(QSpinBox())
        bg_layout.addLayout(pixel_range_layout)
        
        bg_layout.addWidget(QPushButton("Apply"))
        bg_group.setLayout(bg_layout)
        
        main_layout.addWidget(bg_group)
        
        # Control buttons
        control_layout = QHBoxLayout()
        
        collapse_all_btn = QPushButton("Collapse All")
        collapse_all_btn.clicked.connect(lambda: self._toggle_all(False))
        control_layout.addWidget(collapse_all_btn)
        
        expand_all_btn = QPushButton("Expand All")
        expand_all_btn.clicked.connect(lambda: self._toggle_all(True))
        control_layout.addWidget(expand_all_btn)
        
        main_layout.addLayout(control_layout)
        
        # Add stretch to push everything to top
        main_layout.addStretch()
        
        # Store references to collapsible groups
        self.collapsible_groups = [display_group, advanced_group, bg_group]
    
    def _toggle_all(self, expand):
        """Toggle all collapsible groups."""
        for group in self.collapsible_groups:
            group.set_expanded(expand)


def main():
    """Run the example application."""
    app = QApplication(sys.argv)
    window = ExampleWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == '__main__':
    main()

