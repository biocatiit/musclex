"""
Rotation Settings Widget for Quadrant Folding

This widget provides UI controls for setting the rotation angle of images,
including interactive rotation, manual angle entry, and auto orientation configuration.
"""

from PySide6.QtWidgets import (QLabel, QPushButton, QGridLayout, QDialog,
                                QVBoxLayout, QRadioButton, QDialogButtonBox, 
                                QComboBox, QCheckBox, QHBoxLayout, QMessageBox)
from PySide6.QtCore import Signal
from .collapsible_groupbox import CollapsibleGroupBox


class ApplyRotationDialog(QDialog):
    """Dialog for choosing how to apply rotation to images"""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Apply Rotation")
        
        layout = QVBoxLayout()
        layout.addWidget(QLabel("Apply current rotation to:"))
        
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


class RestoreAutoRotationDialog(QDialog):
    """Dialog for choosing how to restore auto rotation to images"""
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Restore Auto Rotation")
        
        layout = QVBoxLayout()
        layout.addWidget(QLabel("Restore auto rotation to:"))
        
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


class AutoOrientationDialog(QDialog):
    """Dialog for configuring automatic orientation settings"""
    def __init__(self, parent=None, current_orientation_model=None, mode_orientation_enabled=False):
        super().__init__(parent)
        self.setWindowTitle("Auto Orientation Settings")
        
        layout = QVBoxLayout()
        
        # Orientation Finding
        orientationLayout = QHBoxLayout()
        orientationLayout.addWidget(QLabel("Orientation Finding:"))
        self.orientationCmbBx = QComboBox()
        self.orientationCmbBx.addItem("Max Intensity")
        self.orientationCmbBx.addItem("GMM")
        self.orientationCmbBx.addItem("Herman Factor (Half Pi)")
        self.orientationCmbBx.addItem("Herman Factor (Pi)")
        if current_orientation_model is not None:
            self.orientationCmbBx.setCurrentIndex(current_orientation_model)
        orientationLayout.addWidget(self.orientationCmbBx)
        layout.addLayout(orientationLayout)
        
        layout.addSpacing(10)
        
        # Mode Orientation
        self.modeAngleChkBx = QCheckBox("Mode Orientation")
        self.modeAngleChkBx.setChecked(mode_orientation_enabled)
        self.modeAngleChkBx.setToolTip("Use the most common orientation angle from all images in the folder")
        layout.addWidget(self.modeAngleChkBx)
        
        layout.addSpacing(20)
        
        # OK and Cancel buttons
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttonBox.accepted.connect(self.accept)
        buttonBox.rejected.connect(self.reject)
        layout.addWidget(buttonBox)
        
        self.setLayout(layout)
        self.setMinimumWidth(350)
    
    def getOrientationModel(self):
        """Returns the selected orientation model index"""
        return self.orientationCmbBx.currentIndex()
    
    def getModeOrientationEnabled(self):
        """Returns whether mode orientation is enabled"""
        return self.modeAngleChkBx.isChecked()


class RotationSettingsWidget(CollapsibleGroupBox):
    """
    Widget for rotation angle settings.
    
    Provides controls for:
    - Auto orientation configuration
    - Interactive rotation setting
    - Manual rotation angle entry
    - Apply/restore rotation to multiple images
    
    Signals:
        autoOrientationRequested(int, bool): Emitted when auto orientation configured (model, mode_enabled)
        applyRotationRequested(str): Emitted when user wants to apply rotation (scope)
        restoreAutoRotationRequested(str): Emitted when user wants to restore auto rotation (scope)
    """
    
    # Signals for complex interactions
    autoOrientationRequested = Signal(int, bool)  # orientation_model, mode_enabled
    applyRotationRequested = Signal(str)  # scope
    restoreAutoRotationRequested = Signal(str)  # scope
    
    def __init__(self, parent=None, orientation_model=None, mode_orientation_enabled=False):
        super().__init__("Set Rotation Angle", start_expanded=True, parent=parent)
        
        self._orientation_model = orientation_model
        self._mode_orientation_enabled = mode_orientation_enabled
        
        # Create all UI components
        self.setAutoOrientationBtn = QPushButton("Set Auto Orientation")
        self.setAutoOrientationBtn.setCheckable(False)
        
        self.setRotationButton = QPushButton("Set Angle Interactively")
        self.setRotationButton.setCheckable(True)
        
        self.setAngleBtn = QPushButton("Set Angle Manually")
        self.setAngleBtn.setCheckable(False)
        
        # Labels for displaying rotation info
        self.rotationLabel = QLabel()
        self.rotationLabel.setStyleSheet("color: green")
        
        self.modeLabel = QLabel()
        self.modeLabel.setStyleSheet("color: green")
        
        # Buttons for apply/restore (handled internally with dialogs)
        self.applyRotationBtn = QPushButton("Apply Rotation")
        self.restoreAutoRotationBtn = QPushButton("Restore Auto Rotation")
        
        # Setup layout
        self._setup_layout()
        
        # Connect internal handlers
        self._connect_internal()
    
    def _setup_layout(self):
        """Setup the layout for all components"""
        layout = QGridLayout()
        
        row = 0
        layout.addWidget(self.setAutoOrientationBtn, row, 0, 1, 4)
        row += 1
        
        layout.addWidget(self.setRotationButton, row, 0, 1, 2)
        layout.addWidget(self.setAngleBtn, row, 2, 1, 2)
        row += 1
        
        layout.addWidget(self.rotationLabel, row, 0, 1, 4)
        row += 1
        
        layout.addWidget(self.modeLabel, row, 0, 1, 4)
        row += 1
        
        layout.addWidget(self.applyRotationBtn, row, 0, 1, 2)
        layout.addWidget(self.restoreAutoRotationBtn, row, 2, 1, 2)
        
        self.setLayout(layout)
    
    def _connect_internal(self):
        """Connect buttons that need internal dialog handling"""
        self.setAutoOrientationBtn.clicked.connect(self._on_auto_orientation_clicked)
        self.applyRotationBtn.clicked.connect(self._on_apply_rotation_clicked)
        self.restoreAutoRotationBtn.clicked.connect(self._on_restore_rotation_clicked)
    
    def _on_auto_orientation_clicked(self):
        """Handle Set Auto Orientation button - show dialog and emit signal"""
        dialog = AutoOrientationDialog(self, 
                                       current_orientation_model=self._orientation_model,
                                       mode_orientation_enabled=self._mode_orientation_enabled)
        
        if dialog.exec() == QDialog.Accepted:
            orientation_model = dialog.getOrientationModel()
            mode_enabled = dialog.getModeOrientationEnabled()
            
            # Update internal state
            self._orientation_model = orientation_model
            self._mode_orientation_enabled = mode_enabled
            
            # Emit signal
            self.autoOrientationRequested.emit(orientation_model, mode_enabled)
    
    def _on_apply_rotation_clicked(self):
        """Handle Apply Rotation button - show dialog and emit signal"""
        dialog = ApplyRotationDialog(self)
        if dialog.exec() == QDialog.Accepted:
            scope = dialog.getSelection()
            self.applyRotationRequested.emit(scope)
    
    def _on_restore_rotation_clicked(self):
        """Handle Restore Auto Rotation button - show dialog and emit signal"""
        dialog = RestoreAutoRotationDialog(self)
        if dialog.exec() == QDialog.Accepted:
            scope = dialog.getSelection()
            self.restoreAutoRotationRequested.emit(scope)
    
    # Public methods for updating display
    
    def update_rotation_display(self, angle):
        """Update the displayed rotation angle"""
        self.rotationLabel.setText(
            f"Rotation Angle (Original Coords): {angle % 360:.2f} °"
        )
    
    def update_mode_display(self, auto_count, total_count):
        """Update the mode statistics display"""
        self.modeLabel.setText(
            f"{auto_count}/{total_count} images have auto rotation settings"
        )
    
    def update_mode_indicator(self, is_manual):
        """Update the group title to show current image mode"""
        mode = "Manual" if is_manual else "Auto"
        self.setTitle(f"Set Rotation Angle  (Current Image Mode:{mode})")
    
    def set_orientation_model(self, model):
        """Update the orientation model"""
        self._orientation_model = model
    
    def set_mode_orientation_enabled(self, enabled):
        """Update the mode orientation enabled state"""
        self._mode_orientation_enabled = enabled

