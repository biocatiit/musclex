"""
Center Settings Widget for Quadrant Folding

This widget provides UI controls for setting the center point of images,
including calibration, manual setting, chords, and perpendiculars methods.
"""

from PySide6.QtWidgets import (QLabel, QPushButton, QGridLayout, QDialog,
                                QVBoxLayout, QRadioButton, QDialogButtonBox, QMessageBox)
from PySide6.QtCore import Signal
from .collapsible_groupbox import CollapsibleGroupBox


class ApplyCenterDialog(QDialog):
    """Dialog for choosing how to apply center to images"""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Apply Center")
        
        layout = QVBoxLayout()
        layout.addWidget(QLabel("Apply current center to:"))
        
        # Create radio buttons for exclusive selection
        self.subsequentRadio = QRadioButton("Apply to subsequent images")
        self.previousRadio = QRadioButton("Apply to previous images")
        self.allRadio = QRadioButton("Apply to all images")
        
        # Set default selection
        self.subsequentRadio.setChecked(True)
        
        layout.addWidget(self.subsequentRadio)
        layout.addWidget(self.previousRadio)
        layout.addWidget(self.allRadio)
        
        # OK and Cancel buttons
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttonBox.accepted.connect(self.accept)
        buttonBox.rejected.connect(self.reject)
        layout.addWidget(buttonBox)
        
        self.setLayout(layout)
    
    def getSelection(self):
        """Returns 'subsequent', 'previous', or 'all'"""
        if self.subsequentRadio.isChecked():
            return 'subsequent'
        elif self.previousRadio.isChecked():
            return 'previous'
        else:
            return 'all'


class RestoreAutoCenterDialog(QDialog):
    """Dialog for choosing how to restore auto center to images"""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Restore Auto Center")
        
        layout = QVBoxLayout()
        layout.addWidget(QLabel("Restore auto center to:"))
        
        # Create radio buttons for exclusive selection
        self.currentRadio = QRadioButton("Apply to current image")
        self.subsequentRadio = QRadioButton("Apply to subsequent images")
        self.previousRadio = QRadioButton("Apply to previous images")
        self.allRadio = QRadioButton("Apply to all images")
        
        # Set default selection to current image
        self.currentRadio.setChecked(True)
        
        layout.addWidget(self.currentRadio)
        layout.addWidget(self.subsequentRadio)
        layout.addWidget(self.previousRadio)
        layout.addWidget(self.allRadio)
        
        # OK and Cancel buttons
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttonBox.accepted.connect(self.accept)
        buttonBox.rejected.connect(self.reject)
        layout.addWidget(buttonBox)
        
        self.setLayout(layout)
    
    def getSelection(self):
        """Returns 'current', 'subsequent', 'previous', or 'all'"""
        if self.currentRadio.isChecked():
            return 'current'
        elif self.subsequentRadio.isChecked():
            return 'subsequent'
        elif self.previousRadio.isChecked():
            return 'previous'
        else:
            return 'all'


class CenterSettingsWidget(CollapsibleGroupBox):
    """
    Widget for center point settings.
    
    Provides controls for:
    - Quick center and rotation setting
    - Calibration-based center
    - Manual center setting (click, chords, perpendiculars)
    - Apply/restore center to multiple images
    
    Signals:
        applyCenterRequested(str): Emitted when user wants to apply center (scope: 'all'|'subsequent'|'previous')
        restoreAutoCenterRequested(str): Emitted when user wants to restore auto center (scope)
    """
    
    # Signals for complex interactions (that require dialog selection)
    applyCenterRequested = Signal(str)  # scope
    restoreAutoCenterRequested = Signal(str)  # scope
    
    def __init__(self, parent=None):
        super().__init__("Set Center", start_expanded=True, parent=parent)
        
        # Create all UI components
        self.setCenterRotationButton = QPushButton("Quick Center and Rotation Angle")
        self.setCenterRotationButton.setCheckable(True)
        
        self.calibrationButton = QPushButton("Set Center by Calibration")
        
        self.setCentByChords = QPushButton("Set Center by Chords")
        self.setCentByChords.setCheckable(True)
        
        self.setCentByPerp = QPushButton("Set Center by Perpendiculars")
        self.setCentByPerp.setCheckable(True)
        
        self.setCentBtn = QPushButton("Set Center Manually")
        self.setCentBtn.setCheckable(False)
        
        # Labels for displaying center info
        self.centerLabel = QLabel()
        self.centerLabel.setStyleSheet("color: green")
        
        self.modeLabel = QLabel()
        self.modeLabel.setStyleSheet("color: green")
        
        # Buttons for apply/restore (handled internally with dialogs)
        self.applyCenterBtn = QPushButton("Apply Center")
        self.restoreAutoCenterBtn = QPushButton("Restore Auto Center")
        
        # Setup layout
        self._setup_layout()
        
        # Connect internal handlers
        self._connect_internal()
    
    def _setup_layout(self):
        """Setup the layout for all components"""
        layout = QGridLayout()
        
        row = 0
        layout.addWidget(self.setCenterRotationButton, row, 0, 1, 4)
        row += 1
        
        layout.addWidget(self.calibrationButton, row, 0, 1, 2)
        layout.addWidget(self.setCentBtn, row, 2, 1, 2)
        row += 1
        
        layout.addWidget(self.setCentByChords, row, 0, 1, 2)
        layout.addWidget(self.setCentByPerp, row, 2, 1, 2)
        row += 1
        
        layout.addWidget(self.centerLabel, row, 0, 1, 4)
        row += 1
        
        layout.addWidget(self.modeLabel, row, 0, 1, 4)
        row += 1
        
        layout.addWidget(self.applyCenterBtn, row, 0, 1, 2)
        layout.addWidget(self.restoreAutoCenterBtn, row, 2, 1, 2)
        
        self.setLayout(layout)
    
    def _connect_internal(self):
        """Connect buttons that need internal dialog handling"""
        self.applyCenterBtn.clicked.connect(self._on_apply_center_clicked)
        self.restoreAutoCenterBtn.clicked.connect(self._on_restore_center_clicked)
    
    def _on_apply_center_clicked(self):
        """Handle Apply Center button - show dialog and emit signal"""
        dialog = ApplyCenterDialog(self)
        if dialog.exec() == QDialog.Accepted:
            scope = dialog.getSelection()
            self.applyCenterRequested.emit(scope)
    
    def _on_restore_center_clicked(self):
        """Handle Restore Auto Center button - show dialog and emit signal"""
        dialog = RestoreAutoCenterDialog(self)
        if dialog.exec() == QDialog.Accepted:
            scope = dialog.getSelection()
            self.restoreAutoCenterRequested.emit(scope)
    
    # Public methods for updating display
    
    def update_current_center(self, center):
        """Update the displayed center coordinates"""
        self.centerLabel.setText(
            f"Center (Current coords): x={center[0]:.2f}, y={center[1]:.2f} px"
        )
    
    def update_mode_display(self, auto_count, total_count):
        """Update the mode statistics display"""
        self.modeLabel.setText(
            f"{auto_count}/{total_count} images have auto center settings"
        )
    
    def update_mode_indicator(self, is_manual):
        """Update the group title to show current image mode"""
        mode = "Manual" if is_manual else "Auto"
        self.setTitle(f"Set Center  (Current Image Mode:{mode})")

