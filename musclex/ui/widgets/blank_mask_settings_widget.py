"""
Empty Cell Image and Mask Settings Widget

This widget provides UI controls for configuring empty cell image (blank image)
and mask settings, including enabling/disabling their application.
"""

from pathlib import Path
from PySide6.QtWidgets import QPushButton, QCheckBox, QGridLayout
from .collapsible_groupbox import CollapsibleGroupBox


class BlankMaskSettingsWidget(CollapsibleGroupBox):
    """
    Widget for empty cell image and mask settings.
    Self-contained: can check settings directory and update its own state.
    
    Provides controls for:
    - Setting empty cell image (blank image)
    - Setting mask
    - Enabling/disabling empty cell image application
    - Enabling/disabling mask application
    
    All UI components (buttons and checkboxes) are public and can be connected
    directly in the parent GUI.
    """
    
    def __init__(self, parent=None):
        super().__init__("Apply Empty Cell Image and Mask", start_expanded=True, parent=parent)
        
        # Create UI components (all public for direct access)
        self.blankSettingButton = QPushButton("Set Empty Cell Image")
        self.maskSettingButton = QPushButton("Set Mask")
        
        self.applyBlankImageChkBx = QCheckBox("Apply Empty Cell Image")
        self.applyBlankImageChkBx.setEnabled(False)  # Disabled until settings exist
        
        self.applyMaskChkBx = QCheckBox("Apply Mask")
        self.applyMaskChkBx.setEnabled(False)  # Disabled until settings exist
        
        # Setup layout
        self._setup_layout()
    
    def _setup_layout(self):
        """Setup the layout for all components"""
        layout = QGridLayout()
        layout.addWidget(self.blankSettingButton, 0, 0, 1, 2)
        layout.addWidget(self.maskSettingButton, 0, 2, 1, 2)
        layout.addWidget(self.applyBlankImageChkBx, 1, 0, 1, 2)
        layout.addWidget(self.applyMaskChkBx, 1, 2, 1, 2)
        self.setLayout(layout)
    
    # ===== Public API: For external use =====
    
    def update_from_directory(self, settings_dir_path):
        """
        Check settings directory and update checkbox states automatically.
        This is the main method for other modules to use.
        
        Args:
            settings_dir_path: Path to the settings directory (Path or str)
        
        Example:
            widget.update_from_directory(Path("/path/to/settings"))
        """
        if not settings_dir_path:
            return
        
        settings_dir = Path(settings_dir_path)
        
        # Check settings status
        status = self.check_settings_status(settings_dir)
        
        # Update UI
        self.update_checkbox_states(
            blank_exists=status['blank_exists'],
            blank_enabled=status['blank_enabled'],
            mask_exists=status['mask_exists'],
            mask_enabled=status['mask_enabled']
        )
    
    @staticmethod
    def check_settings_status(settings_dir):
        """
        Check the status of empty cell image and mask settings.
        Static method - can be called without widget instance.
        
        Args:
            settings_dir: Path to the settings directory (Path or str)
            
        Returns:
            dict with keys:
                - blank_exists: bool - Whether empty cell image settings file exists
                - blank_enabled: bool - Whether empty cell image is enabled (not disabled by flag)
                - mask_exists: bool - Whether mask file exists
                - mask_enabled: bool - Whether mask is enabled (not disabled by flag)
        """
        settings_dir = Path(settings_dir)
        
        # Check empty cell image settings
        blank_config_path = settings_dir / "blank_image_settings.json"
        blank_exists = blank_config_path.exists()
        blank_disabled_flag = settings_dir / ".blank_image_disabled"
        blank_enabled = not blank_disabled_flag.exists()
        
        # Check mask settings
        mask_file_path = settings_dir / "mask.tif"
        mask_exists = mask_file_path.exists()
        mask_disabled_flag = settings_dir / ".mask_disabled"
        mask_enabled = not mask_disabled_flag.exists()
        
        return {
            'blank_exists': blank_exists,
            'blank_enabled': blank_enabled,
            'mask_exists': mask_exists,
            'mask_enabled': mask_enabled
        }
    
    def update_checkbox_states(self, blank_exists, blank_enabled, mask_exists, mask_enabled):
        """
        Update checkbox states (UI only).
        Usually called internally by update_from_directory().
        
        Args:
            blank_exists: Whether empty cell image settings exist
            blank_enabled: Whether empty cell image is enabled
            mask_exists: Whether mask settings exist
            mask_enabled: Whether mask is enabled
        """
        # Block signals to avoid triggering handlers during programmatic update
        self.applyBlankImageChkBx.blockSignals(True)
        self.applyMaskChkBx.blockSignals(True)
        
        # Update empty cell image checkbox
        if blank_exists:
            self.applyBlankImageChkBx.setEnabled(True)
            self.applyBlankImageChkBx.setChecked(blank_enabled)
        else:
            self.applyBlankImageChkBx.setEnabled(False)
            self.applyBlankImageChkBx.setChecked(False)
        
        # Update mask checkbox
        if mask_exists:
            self.applyMaskChkBx.setEnabled(True)
            self.applyMaskChkBx.setChecked(mask_enabled)
        else:
            self.applyMaskChkBx.setEnabled(False)
            self.applyMaskChkBx.setChecked(False)
        
        # Re-enable signals
        self.applyBlankImageChkBx.blockSignals(False)
        self.applyMaskChkBx.blockSignals(False)

