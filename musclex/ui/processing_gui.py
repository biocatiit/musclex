"""
ProcessingGUI - Base class for image processing GUIs.

Provides common functionality for image processing applications:
- Center detection and manual setting
- Rotation angle calculation and adjustment
- Blank image and mask handling
- Interactive tools (chords, perpendiculars, rotation)

Copyright 1999 Illinois Institute of Technology
"""

from abc import abstractmethod
from PySide6.QtWidgets import QDialog, QMessageBox
from .base_gui import BaseGUI
from .widgets.center_settings_widget import CenterSettingsWidget
from .widgets.rotation_settings_widget import RotationSettingsWidget
from .widgets.blank_mask_settings_widget import BlankMaskSettingsWidget
from .tools.chords_center_tool import ChordsCenterTool
from .tools.perpendiculars_center_tool import PerpendicularsCenterTool


class ProcessingGUI(BaseGUI):
    """
    Abstract base class for image processing GUIs.
    
    Extends BaseGUI with common processing functionality:
    - Center detection and setting (calibration, manual, chords, perpendiculars)
    - Rotation angle calculation and adjustment
    - Blank image and mask handling
    - Settings persistence
    
    Automatically provides standard processing widgets (center, rotation, blank/mask)
    via template method pattern in _create_tabs().
    
    Used by: QuadrantFoldingGUI, ProjectionTracesGUI, EquatorialGUI
    Not used by: XRayViewerGUI (viewer only, inherits BaseGUI directly)
    
    Subclasses must implement:
        - _add_custom_widgets() - Add GUI-specific widgets to right panel
        - _get_image_processor() - Return the processor object
        - _apply_center_from_tool(center, tool_name) - Apply center result
        - calibrationClicked() - Handle calibration button
        - setCentBtnClicked() - Handle manual center button
    """
    
    def __init__(self):
        # Initialize processing-specific attributes
        self.checkableButtons = []  # For exclusive button behavior
        self.dir_path = None
        self.calSettings = None
        
        super().__init__()  # Calls initUI()
    
    def _additional_setup(self):
        """Hook: Register processing tools after UI is created"""
        super()._additional_setup()
        self._register_processing_tools()
    
    # ========== Tab Creation Template ==========
    
    def _create_tabs(self):
        """
        Template method: Create tabs with standard processing setup.
        
        Standard flow:
        1. Create standard image tab (from BaseGUI)
        2. Add display options (subclass hook: _add_display_options)
        3. Add standard processing widgets (center, rotation, blank/mask)
        4. Add custom widgets (subclass hook: _add_custom_widgets)
        5. Add navigation controls to bottom (subclass hook: _add_navigation_controls)
        
        Subclasses typically only need to implement:
        - _get_image_tab_title() - Optional, return custom title
        - _add_display_options() - Optional, add GUI-specific display checkboxes
        - _add_custom_widgets() - Required, add GUI-specific settings/tabs
        - _add_standard_processing_widgets() - Optional override to customize
        - _add_navigation_controls() - Optional override for custom navigation
        """
        # 1. Create standard image tab
        self._create_standard_image_tab(tab_title=self._get_image_tab_title())
        
        # 2. Add display options (subclass-specific)
        if hasattr(self, '_add_display_options'):
            self._add_display_options()
        
        # 3. Add standard processing widgets (automatic)
        self._add_standard_processing_widgets()
        
        # 4. Add custom widgets (subclass hook)
        self._add_custom_widgets()
        
        # 5. Add navigation controls to bottom (subclass hook)
        self._add_navigation_controls()
    
    def _get_image_tab_title(self) -> str:
        """
        Hook: Return the title for the image tab.
        
        Default: "Image"
        Subclasses can override to customize.
        
        Example:
            return "Original Image"  # QuadrantFoldingGUI
        """
        return "Image"
    
    def _add_standard_processing_widgets(self):
        """
        Add standard processing widgets to right panel.
        
        By default, adds:
        - Center settings (calibration, chords, perpendiculars, manual)
        - Rotation settings
        - Blank/Mask settings
        
        Subclasses can override to customize. For example:
        
        Example (skip blank/mask if GUI has its own):
            def _add_standard_processing_widgets(self):
                self._create_center_rotation_settings()
                # Skip blank/mask - we have custom blank settings
        
        Example (no rotation needed):
            def _add_standard_processing_widgets(self):
                self._create_center_settings()
                self._create_blank_mask_settings()
        """
        self._create_center_rotation_settings()
        self._create_blank_mask_settings()
    
    @abstractmethod
    def _add_custom_widgets(self):
        """
        Hook: Add GUI-specific widgets to right panel.
        
        Called after standard processing widgets are added.
        This is where you add your custom settings groups and tabs.
        
        Example (QuadrantFoldingGUI):
            def _add_custom_widgets(self):
                self._create_processing_settings()
                self._create_result_processing_settings()
                self._create_result_tab()
        
        Example (ProjectionTracesGUI):
            def _add_custom_widgets(self):
                self._create_pattern_settings()
                self._create_box_settings()
                self._create_peaks_settings()
                self._create_blank_settings()  # PT-specific blank settings
        """
        pass
    
    def _add_navigation_controls(self):
        """
        Hook: Add navigation controls to bottom of right panel.
        
        Default: adds standard navigation controls (self.navControls).
        Subclasses can override to add custom navigation widget.
        
        Example (override with custom navigation):
            def _add_navigation_controls(self):
                self._create_navigation()  # Create custom bottomWidget
                self.right_panel.add_bottom_widget(self.bottomWidget)
        """
        self.right_panel.add_bottom_widget(self.navControls)
    
    # ========== Tool Registration ==========
    
    def _register_processing_tools(self):
        """
        Register interactive tools with image viewer's tool manager.
        
        Subclasses can override to add more tools or customize.
        """
        # Register common center detection tools
        self.image_viewer.tool_manager.register_tool('chords', ChordsCenterTool)
        self.image_viewer.tool_manager.register_tool('perpendiculars', PerpendicularsCenterTool)
    
    # ========== Widget Creation Methods ==========
    
    def _create_center_settings(self):
        """
        Create and add center settings widget to right panel.
        
        Call this in subclass's _create_tabs() after creating image tab.
        """
        self.centerSettings = CenterSettingsWidget(parent=self)
        
        # Connect simple button signals
        self.centerSettings.calibrationButton.clicked.connect(self.calibrationClicked)
        self.centerSettings.setCenterRotationButton.clicked.connect(self._on_center_rotation_clicked)
        self.centerSettings.setCentByChords.clicked.connect(self._on_chords_clicked)
        self.centerSettings.setCentByPerp.clicked.connect(self._on_perp_clicked)
        self.centerSettings.setCentBtn.clicked.connect(self.setCentBtnClicked)
        
        # Add to checkable buttons for exclusive behavior
        self.checkableButtons.extend([
            self.centerSettings.setCenterRotationButton,
            self.centerSettings.setCentByChords,
            self.centerSettings.setCentByPerp,
            self.centerSettings.setCentBtn
        ])
        
        # Add to right panel
        self.right_panel.add_widget(self.centerSettings)
    
    def _create_rotation_settings(self, orientation_model=None, mode_orientation_enabled=False):
        """
        Create and add rotation settings widget to right panel.
        
        Args:
            orientation_model: Optional orientation model for analysis
            mode_orientation_enabled: Whether mode-based orientation is enabled
        
        Call this in subclass's _create_tabs() if rotation settings are needed.
        """
        self.rotationSettings = RotationSettingsWidget(
            parent=self,
            orientation_model=orientation_model,
            mode_orientation_enabled=mode_orientation_enabled
        )
        
        # Add to checkable buttons
        self.checkableButtons.extend([
            self.rotationSettings.setRotationButton,
            self.rotationSettings.setAngleBtn
        ])
        
        # Add to right panel
        self.right_panel.add_widget(self.rotationSettings)
    
    def _create_center_rotation_settings(self, orientation_model=None, mode_orientation_enabled=False):
        """
        Create and add both center and rotation settings widgets.
        
        This is a convenience method that calls both _create_center_settings()
        and _create_rotation_settings() in sequence.
        
        Args:
            orientation_model: Optional orientation model for rotation analysis
            mode_orientation_enabled: Whether mode-based orientation is enabled
        
        Call this in subclass's _create_tabs() for apps that need both widgets.
        """
        self._create_center_settings()
        self._create_rotation_settings(orientation_model, mode_orientation_enabled)
    
    def _create_blank_mask_settings(self):
        """
        Create and add blank/mask settings widget to right panel.
        
        Call this in subclass's _create_tabs() if blank/mask settings are needed.
        """
        self.blankMaskSettings = BlankMaskSettingsWidget(parent=self)
        self.right_panel.add_widget(self.blankMaskSettings)
    
    # ========== Center Tool Handlers ==========
    
    def _on_chords_clicked(self):
        """Handle chords center tool activation/deactivation"""
        if not self._get_image_processor():
            return
        
        if self.centerSettings.setCentByChords.isChecked():
            # Activate the chords center tool
            self.image_viewer.tool_manager.activate_tool('chords')
        else:
            # Deactivate the tool and get the result
            result = self.image_viewer.tool_manager.deactivate_tool('chords')
            
            if result:
                print("Chords center found:", result)
                # Let subclass handle the result
                self._apply_center_from_tool(result, "Chords")
            else:
                print("Chords center calculation failed (need 3+ points)")
    
    def _on_perp_clicked(self):
        """Handle perpendiculars center tool activation/deactivation"""
        if not self._get_image_processor():
            return
        
        if self.centerSettings.setCentByPerp.isChecked():
            # Activate the perpendiculars center tool
            self.image_viewer.tool_manager.activate_tool('perpendiculars')
        else:
            # Deactivate the tool and get the result
            result = self.image_viewer.tool_manager.deactivate_tool('perpendiculars')
            
            if result:
                print("Perpendiculars center found:", result)
                # Let subclass handle the result
                self._apply_center_from_tool(result, "Perpendicular")
            else:
                print("Perpendiculars center calculation failed (need at least 2 line pairs)")
    
    def _on_center_rotation_clicked(self):
        """
        Handle quick center and rotation tool.
        
        Default implementation does nothing. Subclasses should override if needed.
        """
        pass
    
    # ========== Abstract Methods (Subclasses Must Implement) ==========
    
    @abstractmethod
    def _get_image_processor(self):
        """
        Return the image processor object (quadFold, projProc, bioImg, etc).
        
        Returns:
            The processor object, or None if not ready.
        
        Example:
            return self.quadFold
        """
        pass
    
    @abstractmethod
    def _apply_center_from_tool(self, center, tool_name):
        """
        Apply center result from an interactive tool and reprocess image.
        
        Args:
            center: tuple (x, y) center coordinates
            tool_name: str name of the tool that produced the result
        
        Example (QF):
            x, y = center
            orig_x, orig_y = self.getOrigCoordsCenter(x, y)
            self.setCenter((orig_x, orig_y), tool_name)
            self.processImage()
        
        Example (PT):
            self.centerx = int(center[0])
            self.centery = int(center[1])
            self.projProc.info['centerx'] = self.centerx
            self.projProc.info['centery'] = self.centery
            self.center_func = 'manual'
            self.processImage()
        """
        pass
    
    @abstractmethod
    def calibrationClicked(self):
        """
        Handle calibration button click.
        
        Should launch calibration settings dialog and apply results.
        
        Example:
            dlg = CalibrationSettings(self.dir_path)
            if dlg.exec() == QDialog.Accepted:
                self.calSettings = dlg.getValues()
                # Apply calibration...
        """
        pass
    
    @abstractmethod
    def setCentBtnClicked(self):
        """
        Handle manual center setting button click.
        
        Should launch a dialog for manual center input.
        
        Example:
            dlg = SetCentDialog(self, image, current_center)
            if dlg.exec() == QDialog.Accepted:
                new_center = dlg.center
                # Apply center...
        """
        pass

