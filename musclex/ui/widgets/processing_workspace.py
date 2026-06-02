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

import json
from pathlib import Path
from typing import Optional, Tuple
from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout, QDialog
from PySide6.QtCore import Signal

from ...utils.image_data import ImageData
from ...utils.settings_manager import SettingsManager
from ...utils.directory_context import DirectoryContext
from .image_navigator_widget import ImageNavigatorWidget
from .collapsible_right_panel import CollapsibleRightPanel
from .center_settings_widget import CenterSettingsWidget
from .rotation_settings_widget import RotationSettingsWidget
from .blank_mask_settings_widget import BlankMaskSettingsWidget


class ProcessingWorkspace(QWidget):
    """
    Complete workspace for image processing modules (QF, PT, EQ).
    
    This workspace combines image viewing, navigation, file management, and settings
    in one unified interface, providing everything needed for image processing workflows.
    
    Components:
    - ImageNavigatorWidget: Image display, file navigation, and file management
    - Settings Widgets: Center, rotation, blank/mask configuration
    - Settings Management: Persistent storage of manual settings to JSON
    - Interactive Tools: Center finding, rotation adjustment via image viewer
    
    Architecture:
    - Self-contained: Creates and owns all internal components
    - Integrated: ImageNavigatorWidget provides viewer + file manager
    - Flexible: Exposes components for advanced customization
    
    Public Interface:
        load_from_file(filepath): Load a file/directory into workspace
        update_display(image_data): Update settings display for current image
        get_manual_settings(filename): Get manual center/rotation settings
        save_settings(): Save all settings to JSON files
    
    Signals:
        imageChanged(img, filename, dir_path): New image loaded from navigator
        needsReprocess: Settings changed, need to reprocess current image
        statusTextRequested(str): Request parent GUI to update status bar
    
    Usage:
        # Create complete workspace
        workspace = ProcessingWorkspace(settings_dir="/path/to/dir")
        
        # Connect to processing pipeline
        workspace.imageChanged.connect(self.process_and_display)
        workspace.needsReprocess.connect(self.reprocess)
        
        # Note: Navigation controls are automatically added to right_panel bottom
        # by ImageNavigatorWidget. Display panel is also added automatically.
        
        # Add GUI-specific settings to right panel
        workspace.right_panel.add_widget(my_custom_settings)
        
        # Add display options to display panel
        workspace.navigator.image_viewer.display_panel.add_to_top_slot(my_options)
        
        # Load images
        workspace.load_from_file("/path/to/image.tif")
        
        # Access components if needed
        workspace.navigator.image_viewer.display_image(processed_img)
        workspace.navigator.file_manager.names  # File list
    """
    
    # Signals
    imageDataReady = Signal(object)  # ImageData instance - main signal for GUIs
    scanComplete = Signal()  # Forwarded from navigator
    scanProgressChanged = Signal(int, int)  # Forwarded from navigator
    needsReprocess = Signal()  # Settings changed, need to reprocess
    statusTextRequested = Signal(str)  # Request GUI to update status bar text
    batchSettingsChanged = Signal()  # Emitted after batch apply/restore of center or rotation
    outputDirChanged = Signal(str)  # Emitted when output directory is changed via menu
    
    def __init__(self, settings_dir: str, coord_transform_func=None, get_display_center_func=None):
        """
        Initialize the ProcessingWorkspace.
        
        Args:
            settings_dir: Directory path where settings will be stored
            coord_transform_func: Optional function to transform coordinates from
                                 displayed (transformed) image to original image.
                                 Signature: (x, y) -> (orig_x, orig_y)
                                 This is needed when tools operate on transformed images.
            get_display_center_func: Optional function to get the center in display
                                    (transformed) coordinates.
                                    Signature: () -> (x, y) or None
                                    For QF: returns transformed center (image center after transform)
                                    For PT: not needed (display coords = original coords)
        
        Note:
            ImageNavigatorWidget and all other components are created internally.
            No need to pass image_viewer or file_manager - they're managed by this workspace.
        """
        super().__init__()
        
        # Configuration
        self._settings_dir = settings_dir
        self._coord_transform_func = coord_transform_func
        self._get_display_center_func = get_display_center_func
        self._current_filename = None
        self._current_image_data = None
        
        # Read/write directory context (set on folder load)
        self.dir_context: DirectoryContext = DirectoryContext.colocated(settings_dir) if settings_dir else None
        
        # Orientation model and mode rotation state
        self._orientation_model = 0  # Default: Max Intensity
        self._mode_rotation = None  # Cached mode rotation value
        
        # Centralized settings I/O
        self.settings_manager = SettingsManager(settings_dir)
        
        # Track first image in folder for notification
        self._first_image_in_folder = True
        
        # PT-specific: Quadrant Folded checkbox (created on demand)
        self.qf_checkbox = None
        
        # Create ImageNavigatorWidget (internal, owned by workspace)
        self.navigator = ImageNavigatorWidget(
            auto_display=False,  # Processor mode: manual display control
            show_display_panel=True,
            show_double_zoom=True
        )
        
        # Internal references (for backward compatibility with existing code)
        self._image_viewer = self.navigator.image_viewer
        self._file_manager = self.navigator.file_manager
        
        # Create collapsible right panel for settings
        self.right_panel = self.navigator.right_panel
        # Create settings widgets
        self._setup_components()
        
        # Setup UI layout (horizontal: navigator + settings)
        self._setup_ui()
        
        # Connect all internal signals
        self._connect_signals()
    
    # Backward-compatible properties – external code (QuadrantFoldingGUI,
    # AddIntensitiesSingleExp, etc.) may still read these directly.

    @property
    def _center_settings(self) -> dict:
        return self.settings_manager.center_settings

    @property
    def _rotation_settings(self) -> dict:
        return self.settings_manager.rotation_settings

    def _setup_components(self):
        """Create settings widgets (image_viewer is external)."""
        # Create these unattached so modules that do not add them to a layout
        # do not get stray group-box titles painted at the workspace origin.
        self._center_widget = CenterSettingsWidget(parent=None)
        self._rotation_widget = RotationSettingsWidget(parent=None)
        self._blank_mask_widget = BlankMaskSettingsWidget(parent=None)
        
        # Register tools to the external image viewer's tool manager
        self._register_tools()
    
    def _setup_ui(self):
        """
        Setup UI layout - ImageNavigatorWidget includes right panel.
        
        Layout:
        ┌────────────────────────────────────────────────────┐
        │  ProcessingWorkspace                               │
        │ ┌────────────────────────────────────────────────┐ │
        │ │ ImageNavigatorWidget                           │ │
        │ │ ┌──────────────────┬─────────────────────────┐ │ │
        │ │ │  [Image Display] │ CollapsibleRightPanel   │ │ │
        │ │ │                  │ ┌─────────────────────┐ │ │ │
        │ │ │                  │ │ Display Panel       │ │ │ │
        │ │ │                  │ │ Nav Controls        │ │ │ │
        │ │ │                  │ │ [GUI adds widgets]  │ │ │ │
        │ │ │                  │ └─────────────────────┘ │ │ │
        │ │ └──────────────────┴─────────────────────────┘ │ │
        │ └────────────────────────────────────────────────┘ │
        └────────────────────────────────────────────────────┘
        
        Note: Right panel is created by ImageNavigatorWidget and includes
              display panel and nav controls automatically. GUI adds its
              own settings widgets as needed.
        """
        # Main layout: horizontal
        main_layout = QHBoxLayout(self)
        main_layout.setContentsMargins(0, 0, 0, 0)
        main_layout.setSpacing(0)
        
        # Left side: ImageNavigatorWidget (takes most space)
        main_layout.addWidget(self.navigator, 1)
        

    
    def _register_tools(self):
        """Register interactive tools to the ToolManager."""
        from ..tools.chords_center_tool import ChordsCenterTool
        from ..tools.perpendiculars_center_tool import PerpendicularsCenterTool
        from ..tools.rotation_tool import RotationTool
        from ..tools.center_rotate_tool import CenterRotateTool
        
        tool_mgr = self._image_viewer.tool_manager
        
        # Register center finding tools
        tool_mgr.register_tool('chords', ChordsCenterTool)
        tool_mgr.register_tool('perpendiculars', PerpendicularsCenterTool)
        
        # Register rotation tool (needs current center as positional arg)
        tool_mgr.register_tool('rotation', RotationTool, self._get_current_center)
        
        # Register combined center+rotation tool
        # Note: Returns center in display coordinates (consistent with other center tools)
        tool_mgr.register_tool('center_rotate', CenterRotateTool)
    
    def _connect_signals(self):
        """Connect all internal signals."""
        # Navigator signals
        self.navigator.fileLoaded.connect(self.on_file_loaded)
        self.navigator.imageChanged.connect(self.on_image_changed) 
        
        # Forward scan signals
        self.navigator.scanComplete.connect(self.scanComplete.emit)
        self.navigator.scanProgressChanged.connect(self.scanProgressChanged.emit)
        
        # Tool buttons -> Activate/deactivate tools
        self._center_widget.setCentByChords.clicked.connect(
            lambda checked: self._on_chords_button_clicked(checked)
        )
        self._center_widget.setCentByPerp.clicked.connect(
            lambda checked: self._on_perp_button_clicked(checked)
        )
        self._center_widget.setCenterRotationButton.clicked.connect(
            lambda checked: self._on_center_rotation_button_clicked(checked)
        )
        self._center_widget.setCentBtn.clicked.connect(
            self._on_set_center_manually_clicked
        )
        self._center_widget.calibrationButton.clicked.connect(
            self._on_calibration_button_clicked
        )
        self._rotation_widget.setRotationButton.clicked.connect(
            lambda checked: self._on_rotation_button_clicked(checked)
        )
        self._rotation_widget.setAngleBtn.clicked.connect(
            self._on_set_angle_manually_clicked
        )
        
        # Tool completed -> Handle result
        self._image_viewer.toolCompleted.connect(self._on_tool_completed)
        
        # Apply/Restore buttons
        self._center_widget.applyCenterRequested.connect(self._on_apply_center)
        self._center_widget.restoreAutoCenterRequested.connect(self._on_restore_auto_center)
        self._rotation_widget.applyRotationRequested.connect(self._on_apply_rotation)
        self._rotation_widget.restoreAutoRotationRequested.connect(self._on_restore_auto_rotation)
        
        # Auto orientation
        self._rotation_widget.autoOrientationRequested.connect(self.handle_auto_orientation_request)
        
        # Blank/Mask settings buttons
        self._blank_mask_widget.blankSettingButton.clicked.connect(self._on_blank_setting_clicked)
        self._blank_mask_widget.maskSettingButton.clicked.connect(self._on_mask_setting_clicked)
        
        # Blank/Mask checkboxes
        self._blank_mask_widget.applyBlankImageChkBx.stateChanged.connect(
            self._on_blank_checkbox_changed
        )
        self._blank_mask_widget.applyMaskChkBx.stateChanged.connect(
            self._on_mask_checkbox_changed
        )
    
    # ==================== Tool Button Handlers ====================
    
    def _on_chords_button_clicked(self, checked):
        """Handle Chords button click."""
        if checked:
            self._image_viewer.tool_manager.activate_tool('chords')
        else:
            # User manually unchecked - handle the result if present
            # (If tool auto-completed, it was already handled by toolCompleted signal
            #  and deactivate_tool will return None)
            result = self._image_viewer.tool_manager.deactivate_tool('chords')
            if result:
                self._handle_center_result(result, 'chords')
    
    def _on_perp_button_clicked(self, checked):
        """Handle Perpendiculars button click."""
        if checked:
            self._image_viewer.tool_manager.activate_tool('perpendiculars')
        else:
            # User manually unchecked - handle the result if present
            result = self._image_viewer.tool_manager.deactivate_tool('perpendiculars')
            if result:
                self._handle_center_result(result, 'perpendiculars')
    
    def _on_rotation_button_clicked(self, checked):
        """Handle Rotation button click."""
        if checked:
            self._image_viewer.tool_manager.activate_tool('rotation')
        else:
            # User manually unchecked - handle the result if present
            result = self._image_viewer.tool_manager.deactivate_tool('rotation')
            if result:
                self._handle_rotation_result(result, 'rotation_tool')
    
    def _on_center_rotation_button_clicked(self, checked):
        """
        Handle Center+Rotation button click.
        
        This activates a tool that allows setting both center and rotation
        in one interaction by clicking on two corresponding reflection peaks.
        """
        if checked:
            # Request GUI to update status bar
            self.statusTextRequested.emit(
                "Click on 2 corresponding reflection peaks along the equator (click to set)"
            )
            self._image_viewer.tool_manager.activate_tool('center_rotate')
        else:
            # User manually unchecked - handle the result if present
            result = self._image_viewer.tool_manager.deactivate_tool('center_rotate')
            if result:
                self._handle_center_rotate_result(result)
            # Request GUI to reset status bar
            self.statusTextRequested.emit("")
    
    
    def _on_calibration_button_clicked(self):
        """
        Handle calibration button click.
        
        Opens calibration settings dialog and triggers reprocessing if settings changed.
        """
        if self._current_image_data is None:
            print("Warning: No image loaded, cannot open calibration settings")
            return
        
        # Show calibration dialog (force=True to always show)
        success = self.show_calibration_dialog(self._current_image_data, force=True)
        
        if success:
            # Trigger reprocessing via signal
            # ImageData fingerprint change will automatically invalidate dependent caches
            self.needsReprocess.emit()
    
    def _on_set_center_manually_clicked(self):
        """
        Handle 'Set Center Manually' button click.
        
        Opens SetCentDialog for user to manually input center coordinates.
        NOTE: Uses ORIGINAL image (from file_manager), not transformed image,
              because center is defined in original image coordinates.
        """
        if not self._current_image_data or not self._file_manager:
            return
        
        center = self._current_image_data.center
        if not center:
            return
        
        # Use original image from file_manager (NOT transformed image from viewer)
        # Center is defined in original image coordinates
        img = self._file_manager.current_image.copy()
        
        # Get display settings from image_viewer
        display_settings = self._image_viewer.get_display_options()
        
        # Import SetCentDialog
        from ..SetCentDialog import SetCentDialog
        
        # Show dialog
        dialog = SetCentDialog(
            self,
            img,
            center,
            isLogScale=display_settings['log_scale'],
            vmin=display_settings['vmin'],
            vmax=display_settings['vmax']
        )
        
        if dialog.exec():
            # User accepted - get the new center
            new_center = dialog.center
            
            # Save using public method
            self.set_center_from_source(
                self._current_filename,
                new_center,
                "SetCentDialog"
            )
            
            # Emit needsReprocess
            self.needsReprocess.emit()
            
            print(f"Center manually set to {new_center} from SetCentDialog")
    
    def _on_set_angle_manually_clicked(self):
        """
        Handle 'Set Angle Manually' button click.
        
        Opens SetAngleDialog for user to manually adjust rotation angle.
        """
        if not self._current_image_data or not self._image_viewer:
            return
        
        # Get current displayed image from image_viewer (transformed by processor)
        img = self._image_viewer.get_current_image_data()
        if img is None:
            return
        
        rotation = self._current_image_data.rotation or 0.0
        
        # For transformed images, center is at image center
        h, w = img.shape
        display_center = (w // 2, h // 2)
        
        # Get display settings from image_viewer
        display_settings = self._image_viewer.get_display_options()
        
        # Import SetAngleDialog
        from ..SetAngleDialog import SetAngleDialog
        
        # Show dialog with transformed image and center
        dialog = SetAngleDialog(
            self,
            img,
            display_center,
            rotation,
            isLogScale=display_settings['log_scale'],
            vmin=display_settings['vmin'],
            vmax=display_settings['vmax']
        )
        
        if dialog.exec():
            # User accepted - get the angle increment
            angle_increment = dialog.get_angle()
            
            # Save using public method
            self.set_rotation_from_source(
                self._current_filename,
                angle_increment,
                "SetAngleDialog"
            )
            
            # Emit needsReprocess
            self.needsReprocess.emit()
            
            print(f"Rotation manually set with increment {angle_increment:.2f}° from SetAngleDialog")
    
    # ==================== Tool Completion Handlers ====================
    
    def _on_tool_completed(self, tool_name, result):
        """
        Handle tool completion (from toolCompleted signal).
        
        This is the central handler for all interactive tool results.
        """
        if tool_name in ['chords', 'perpendiculars']:
            self._handle_center_result(result, tool_name)
        elif tool_name == 'rotation':
            self._handle_rotation_result(result, tool_name)
        elif tool_name == 'center_rotate':
            # CenterRotateTool: sets both center and rotation together
            # Result is {'center': (x, y), 'angle': angle}
            self._handle_center_rotate_result(result)
    
    def _handle_center_result(self, center: Tuple[float, float], source: str):
        """
        Handle center result from tools.
        
        Complete workflow:
        1. Transform coordinates (if needed)
        2. Save to settings
        3. Update ImageData
        4. Update UI
        5. Notify GUI (emit needsReprocess)
        """
        if not self._current_filename:
            return
        
        # 1. Transform coordinates from displayed image to original image
        if self._coord_transform_func:
            center = self._coord_transform_func(center[0], center[1])
            print(f"  Transformed center to original coordinates: {center}")
        
        # 2. Save to settings (now in original coordinates)
        self.settings_manager.set_center(self._current_filename, center, source)
        self.settings_manager.save_center()
        self._after_center_save()
        
        # 3. Update ImageData
        if self._current_image_data:
            self._current_image_data.update_manual_center(center)
        
        # 3. Update UI
        # NOTE: update_current_center removed - GUI will update after processImage() with transformed coords
        self._center_widget.update_mode_indicator(is_manual=True)
        
        # Uncheck button
        self._center_widget.setCentByChords.setChecked(False)
        self._center_widget.setCentByPerp.setChecked(False)
        
        # 4. Notify GUI
        self.needsReprocess.emit()
        
        print(f"Center set to {center} from {source}")
    
    def _apply_rotation_increment(self, angle: float, source: str, uncheck_button=None):
        """
        Apply a rotation angle increment and update all related state.
        
        Args:
            angle: Rotation increment in degrees (relative to current rotation)
            source: Source identifier ('rotation_tool', 'center_rotate', etc.)
            uncheck_button: Optional button to uncheck after applying
        
        Returns:
            The new absolute rotation angle
        """
        if not self._current_filename:
            return None
        
        # Get current rotation from ImageData
        current_rotation = 0.0
        if self._current_image_data:
            current_rotation = self._current_image_data.rotation or 0.0
        
        # Calculate new absolute rotation (user's angle is relative to displayed image)
        new_rotation = current_rotation + angle
        
        # 1. Save to settings
        self.settings_manager.set_rotation(self._current_filename, new_rotation, source)
        self.settings_manager.save_rotation()
        self._after_rotation_save()
        
        # 2. Update ImageData
        if self._current_image_data:
            self._current_image_data.update_manual_rotation(new_rotation)
        
        # 3. Update UI
        self._rotation_widget.update_rotation_display(new_rotation)
        self._rotation_widget.update_mode_indicator(is_manual=True)
        
        # Uncheck button if specified
        if uncheck_button:
            uncheck_button.setChecked(False)
        
        # 4. Notify GUI
        self.needsReprocess.emit()
        
        print(f"Rotation set to {new_rotation}° from {source}")
        return new_rotation
    
    def _handle_rotation_result(self, angle: float, source: str):
        """
        Handle rotation result from tools.
        
        Similar workflow as center handling.
        """
        self._apply_rotation_increment(
            angle, 
            source, 
            uncheck_button=self._rotation_widget.setRotationButton
        )
    
    def _handle_center_rotate_result(self, result: dict):
        """
        Handle center_rotate tool result (sets both center and rotation).
        
        This tool allows setting center and rotation together in one interaction.
        Result format: {'center': (x, y), 'angle': angle}
        
        Note: CenterRotateTool returns center in DISPLAY coordinates (consistent
        with ChordsCenterTool and PerpendicularsCenterTool), so we transform here.
        """
        if not self._current_filename:
            return
        
        center = tuple(result['center'])  # In display coordinates
        angle = result['angle']
        
        # Transform coordinates from displayed image to original image
        if self._coord_transform_func:
            center = self._coord_transform_func(center[0], center[1])
            print(f"  Transformed center to original coordinates: {center}")
        
        # 1. Save center settings
        self.settings_manager.set_center(self._current_filename, center, 'center_rotate')
        self.settings_manager.save_center()
        self._after_center_save()
        
        # 2. Update center in ImageData
        if self._current_image_data:
            self._current_image_data.update_manual_center(center)
        
        # 3. Update center UI
        # NOTE: update_current_center removed - GUI will update after processImage() with transformed coords
        self._center_widget.update_mode_indicator(is_manual=True)
        
        # Uncheck the center_rotate button
        if hasattr(self._center_widget, 'setCenterRotationButton'):
            self._center_widget.setCenterRotationButton.setChecked(False)
        
        # 4. Apply rotation increment (handles save, ImageData update, UI update, and needsReprocess signal)
        new_rotation = self._apply_rotation_increment(angle, 'center_rotate')
        
        print(f"Center & Rotation set to {center}, {new_rotation}° from center_rotate tool")
    
    # ==================== Apply/Restore Handlers ====================
    
    def _on_apply_center(self, scope: str):
        """Handle Apply Center to batch."""
        from PySide6.QtWidgets import QMessageBox
        
        if not self._current_image_data:
            QMessageBox.warning(self.window(), "No Center", 
                "No center available to apply.")
            return
        
        center = self._current_image_data.center
        if not center:
            QMessageBox.warning(self.window(), "No Center", 
                "No center available to apply.")
            return
        
        self.apply_center_to_batch(center, scope)
        
        QMessageBox.information(self.window(), "Center Applied", 
            f"Center {center} applied to {scope} images.")
    
    def _on_restore_auto_center(self, scope: str):
        """Handle Restore Auto Center."""
        from PySide6.QtWidgets import QMessageBox
        
        self.restore_auto_center_for_batch(scope)
        
        QMessageBox.information(self.window(), "Auto Center Restored", 
            f"Auto center restored for {scope} images.")
    
    def _on_apply_rotation(self, scope: str):
        """Handle Apply Rotation to batch."""
        from PySide6.QtWidgets import QMessageBox
        
        if not self._current_image_data:
            QMessageBox.warning(self.window(), "No Rotation", 
                "No rotation available to apply.")
            return
        
        rotation = self._current_image_data.rotation
        if rotation is None:
            QMessageBox.warning(self.window(), "No Rotation", 
                "No rotation available to apply.")
            return
        
        self.apply_rotation_to_batch(rotation, scope)
        
        QMessageBox.information(self.window(), "Rotation Applied", 
            f"Rotation {rotation:.2f}° applied to {scope} images.")
    
    def _on_restore_auto_rotation(self, scope: str):
        """Handle Restore Auto Rotation."""
        from PySide6.QtWidgets import QMessageBox
        
        self.restore_auto_rotation_for_batch(scope)
        
        QMessageBox.information(self.window(), "Auto Rotation Restored", 
            f"Auto rotation restored for {scope} images.")
    
    # ==================== Blank/Mask Handlers ====================
    
    def _on_blank_changed(self, apply_blank: bool):
        """Handle blank checkbox change."""
        # Blank/mask changes are handled by ImageData automatically
        # Just notify GUI to reprocess
        self.needsReprocess.emit()
    
    def _on_mask_changed(self, apply_mask: bool):
        """Handle mask checkbox change."""
        # Same as blank
        self.needsReprocess.emit()
    
    # ==================== Public Interface ====================
    
    def update_display(self, image_data: ImageData):
        """
        Update widgets to show settings for the current image.
        
        Note: This does NOT display the image itself (that's done by the external
        ImageViewerWidget). This only updates the settings widgets to reflect the
        current image's settings.
        
        Args:
            image_data: ImageData object containing current image and settings
        """
        self._current_filename = image_data.img_name
        self._current_image_data = image_data
        
        # Update widgets to show current settings
        self._update_widgets_display()
    
    def set_center_from_source(self, filename: str, center: Optional[tuple], source: str):
        """
        Public method for GUI to set center programmatically (e.g., from calibration dialog).
        
        Args:
            filename: Name of the file to set center for
            center: (x, y) center coordinates, or None to remove manual center
            source: Description of where this center came from (e.g., "calibration", "SetCentDialog")
        """
        if center is None:
            self.settings_manager.clear_center(filename)
        else:
            self.settings_manager.set_center(filename, center, source)
        self.save_settings()
        
        # Update ImageData if it's the current image
        if self._current_image_data and self._current_filename == filename:
            self._current_image_data.update_manual_center(center)
            self.update_display(self._current_image_data)
    
    def set_rotation_from_source(self, filename: str, rotation_increment: Optional[float], source: str):
        """
        Public method for GUI to set rotation programmatically (e.g., from SetAngleDialog).
        
        Args:
            filename: Name of the file to set rotation for
            rotation_increment: Rotation angle increment in degrees (relative to current rotation),
                               or None to remove manual rotation
            source: Description of where this rotation came from (e.g., "SetAngleDialog", "ModeAngle")
        
        Note: rotation_increment is treated as a delta to add to the current rotation,
              because rotations are performed on the already-transformed (displayed) image.
        """
        if rotation_increment is None:
            # Remove manual rotation setting
            self.settings_manager.clear_rotation(filename)
        else:
            # Get current rotation from ImageData
            current_rotation = 0.0
            if self._current_image_data and self._current_filename == filename:
                current_rotation = self._current_image_data.rotation or 0.0
            
            # Calculate new absolute rotation
            new_rotation = current_rotation + rotation_increment
            self.settings_manager.set_rotation(filename, new_rotation, source)
        
        self.save_settings()
        
        # Update ImageData if it's the current image
        if self._current_image_data and self._current_filename == filename:
            if rotation_increment is None:
                self._current_image_data.update_manual_rotation(None)
            else:
                new_rotation = self.settings_manager.get_rotation(filename)
                self._current_image_data.update_manual_rotation(new_rotation)
            self.update_display(self._current_image_data)
    
    def get_manual_settings(self, filename: str) -> Tuple[Optional[Tuple[float, float]], Optional[float]]:
        """
        Get manual center and rotation settings for a file.
        
        Args:
            filename: Image filename
        
        Returns:
            Tuple of (manual_center, manual_rotation)
            Both can be None if not manually set
        """
        center = self.settings_manager.get_center(filename)
        rotation = self.settings_manager.get_rotation(filename)
        return center, rotation
    
    def save_settings(self):
        """Save all settings to JSON files."""
        self.settings_manager.save_center()
        self._after_center_save()
        self.settings_manager.save_rotation()
        self._after_rotation_save()
    
    def _update_mode_statistics_internal(self):
        """Internal method to update mode statistics using file_manager."""
        if not self._file_manager or not self._file_manager.names:
            return
        
        total_files = len(self._file_manager.names)
        auto_center_count = total_files - len(self._center_settings)
        auto_rotation_count = total_files - len(self._rotation_settings)
        
        self._center_widget.update_mode_display(auto_center_count, total_files)
        self._rotation_widget.update_mode_display(auto_rotation_count, total_files)
    
    def update_mode_statistics(self, total_files: int = None):
        """
        Update mode display statistics for both center and rotation widgets.
        
        Args:
            total_files: Total number of files (optional, will use file_manager if not provided)
        """
        if total_files is None:
            self._update_mode_statistics_internal()
        else:
            auto_center_count = total_files - len(self._center_settings)
            auto_rotation_count = total_files - len(self._rotation_settings)
            
            self._center_widget.update_mode_display(auto_center_count, total_files)
            self._rotation_widget.update_mode_display(auto_rotation_count, total_files)
    
    def set_orientation_model(self, orientation_model: int):
        """
        Update the orientation model used for auto-rotation calculation.
        
        This will:
        1. Update the widget display
        2. Clear current manual rotation (if exists) to trigger recalculation
        3. Emit needsReprocess signal
        
        Args:
            orientation_model: The orientation model index
        """
        # Update widget display
        self._rotation_widget.set_orientation_model(orientation_model)
        
        # Clear manual rotation to force recalculation with new model
        if self._current_filename and self._current_image_data:
            # Clear from ImageData
            self._current_image_data.update_manual_rotation(None)
            
            # Remove from settings
            self.settings_manager.clear_rotation(self._current_filename)
            self.settings_manager.save_rotation()
            self._after_rotation_save()
            
            # Update UI
            self._rotation_widget.update_mode_indicator(is_manual=False)
            
            # Trigger reprocess
            self.needsReprocess.emit()
            
            print(f"Orientation model changed, rotation will be recalculated")
    
    def calculate_mode_rotation(self):
        """
        Calculate the mode (most common) rotation angle across all images.
        
        Uses FileManager to load all images and ImageData to get rotation for each.
        
        Returns:
            The most common rotation angle, or None if calculation fails
        """
        if not self._file_manager or not self._file_manager.names:
            print("Warning: No files available for mode rotation calculation")
            return None
        
        print("Calculating mode of rotation angles for all images in directory")
        angles = []
        
        for idx, filename in enumerate(self._file_manager.names):
            # Load image using FileManager
            img = self._file_manager.get_image_by_index(idx)
            
            if img is None:
                print(f"Error loading {filename}")
                return None
            
            # Create ImageData (auto-calculates rotation if needed)
            from ...utils.image_data import ImageData
            img_data = ImageData(
                img=img,
                img_path=self._settings_dir,
                img_name=filename,
                settings_manager=self.settings_manager,
            )
            
            # Get rotation from ImageData (triggers auto-calculation via lazy loading)
            rotation = img_data.rotation
            
            print(f'  Getting angle for {filename}: {rotation}')
            
            if rotation is None:
                print(f"Warning: Could not calculate rotation for {filename}")
                return None
            
            angles.append(rotation)
        
        # Calculate mode (most common angle)
        mode_rotation = max(set(angles), key=angles.count)
        print(f"Mode rotation calculated: {mode_rotation}°")
        return mode_rotation
    
    def set_mode_orientation_enabled(self, enabled: bool, mode_rotation=None):
        """
        Enable or disable mode orientation feature.
        
        Args:
            enabled: Whether to enable mode orientation
            mode_rotation: Pre-calculated mode rotation value (optional)
                          If None and enabled=True, will calculate automatically
        
        Returns:
            The mode rotation value if enabled, None otherwise
        """
        if enabled:
            # Calculate mode rotation if not provided
            if mode_rotation is None:
                mode_rotation = self.calculate_mode_rotation()
            
            if mode_rotation is not None:
                # Update widget display
                self._rotation_widget.set_mode_orientation_enabled(True)
                print(f"Mode orientation enabled: {mode_rotation}°")
            else:
                print("Failed to calculate mode rotation")
                return None
        else:
            # Disable mode orientation
            self._rotation_widget.set_mode_orientation_enabled(False)
            mode_rotation = None
            print("Mode orientation disabled")
        
        return mode_rotation
    
    def handle_auto_orientation_request(self, orientation_model: int, mode_enabled: bool):
        """
        Handle auto orientation request from widget.
        
        This combines orientation model update and mode orientation enable/disable.
        Panel manages the state internally.
        
        Args:
            orientation_model: The orientation model index
            mode_enabled: Whether to enable mode orientation
        
        Returns:
            The mode rotation value if enabled, None otherwise
        """
        # Store orientation model
        self._orientation_model = orientation_model
        
        # Update orientation model
        self.set_orientation_model(orientation_model)
        
        # Update mode orientation (with cached value)
        self._mode_rotation = self.set_mode_orientation_enabled(
            mode_enabled, 
            self._mode_rotation if hasattr(self, '_mode_rotation') else None
        )
        
        return self._mode_rotation
    
    # ==================== Batch Operations ====================
    
    def apply_center_to_batch(self, center: Tuple[float, float], scope: str):
        """
        Apply manual center to multiple images based on scope.
        
        Args:
            center: Center coordinates to apply
            scope: One of 'all', 'subsequent', 'previous'
        """
        if not self._file_manager:
            print("Warning: FileManager not available for batch operations")
            return
        
        file_list = self._file_manager.names
        current_idx = self._file_manager.current
        
        # Calculate indices based on scope
        if scope == 'all':
            indices = range(len(file_list))
        elif scope == 'subsequent':
            indices = range(current_idx, len(file_list))
        elif scope == 'previous':
            indices = range(0, current_idx + 1)
        else:
            print(f"Warning: Unknown scope '{scope}'")
            return
        
        # Apply manual center to selected images
        for idx in indices:
            filename = file_list[idx]
            self.settings_manager.set_center(filename, center, 'propagated')
        
        # Save settings
        self.settings_manager.save_center()
        self._after_center_save()
        
        # Update statistics display
        self.update_mode_statistics(len(file_list))
        
        # If current image is in scope, trigger reprocess
        if current_idx in indices:
            if self._current_image_data:
                self._current_image_data.update_manual_center(center)
            self._center_widget.update_mode_indicator(is_manual=True)
            self.needsReprocess.emit()
        
        self.batchSettingsChanged.emit()
    
    def restore_auto_center_for_batch(self, scope: str):
        """
        Restore auto center mode for multiple images based on scope.
        
        Args:
            scope: One of 'current', 'all', 'subsequent', 'previous'
        """
        if not self._file_manager:
            print("Warning: FileManager not available for batch operations")
            return
        
        file_list = self._file_manager.names
        current_idx = self._file_manager.current
        
        # Calculate indices based on scope
        if scope == 'current':
            indices = [current_idx]
        elif scope == 'all':
            indices = range(len(file_list))
        elif scope == 'subsequent':
            indices = range(current_idx, len(file_list))
        elif scope == 'previous':
            indices = range(0, current_idx + 1)
        else:
            print(f"Warning: Unknown scope '{scope}'")
            return
        
        # Remove manual settings for selected images
        for idx in indices:
            self.settings_manager.clear_center(file_list[idx])
        
        # If current image is in scope, update ImageData and UI
        if current_idx in indices:
            if self._current_image_data:
                self._current_image_data.update_manual_center(None)
            self._center_widget.update_mode_indicator(is_manual=False)
            # Notify GUI to reprocess
            self.needsReprocess.emit()
        
        # Save settings
        self.settings_manager.save_center()
        self._after_center_save()
        
        # Update statistics display
        self.update_mode_statistics(len(file_list))
        
        self.batchSettingsChanged.emit()
    
    def apply_rotation_to_batch(self, rotation: float, scope: str):
        """
        Apply manual rotation to multiple images based on scope.
        
        Args:
            rotation: Rotation angle to apply
            scope: One of 'all', 'subsequent', 'previous'
        """
        if not self._file_manager:
            print("Warning: FileManager not available for batch operations")
            return
        
        file_list = self._file_manager.names
        current_idx = self._file_manager.current
        
        # Calculate indices based on scope
        if scope == 'all':
            indices = range(len(file_list))
        elif scope == 'subsequent':
            indices = range(current_idx, len(file_list))
        elif scope == 'previous':
            indices = range(0, current_idx + 1)
        else:
            print(f"Warning: Unknown scope '{scope}'")
            return
        
        # Apply manual rotation to selected images
        for idx in indices:
            self.settings_manager.set_rotation(file_list[idx], rotation, 'propagated')
        
        # Save settings
        self.settings_manager.save_rotation()
        self._after_rotation_save()
        
        # Update statistics display
        self.update_mode_statistics(len(file_list))
        
        # If current image is in scope, trigger reprocess
        if current_idx in indices:
            if self._current_image_data:
                self._current_image_data.update_manual_rotation(rotation)
            self._rotation_widget.update_mode_indicator(is_manual=True)
            self.needsReprocess.emit()
        
        self.batchSettingsChanged.emit()
    
    def restore_auto_rotation_for_batch(self, scope: str):
        """
        Restore auto rotation mode for multiple images based on scope.
        
        Args:
            scope: One of 'current', 'all', 'subsequent', 'previous'
        """
        if not self._file_manager:
            print("Warning: FileManager not available for batch operations")
            return
        
        file_list = self._file_manager.names
        current_idx = self._file_manager.current
        
        # Calculate indices based on scope
        if scope == 'current':
            indices = [current_idx]
        elif scope == 'all':
            indices = range(len(file_list))
        elif scope == 'subsequent':
            indices = range(current_idx, len(file_list))
        elif scope == 'previous':
            indices = range(0, current_idx + 1)
        else:
            print(f"Warning: Unknown scope '{scope}'")
            return
        
        # Remove manual settings for selected images
        for idx in indices:
            self.settings_manager.clear_rotation(file_list[idx])
        
        # If current image is in scope, update ImageData and UI
        if current_idx in indices:
            if self._current_image_data:
                self._current_image_data.update_manual_rotation(None)
            self._rotation_widget.update_mode_indicator(is_manual=False)
            # Notify GUI to reprocess
            self.needsReprocess.emit()
        
        # Save settings
        self.settings_manager.save_rotation()
        self._after_rotation_save()
        
        # Update statistics display
        self.update_mode_statistics(len(file_list))
        
        self.batchSettingsChanged.emit()
    
    def set_settings_dir(self, new_settings_dir: str):
        """
        Update the settings directory and reload settings.
        
        This should be called when the GUI changes to a new data directory.
        
        Args:
            new_settings_dir: New directory path for settings
        """
        if new_settings_dir != self._settings_dir:
            self._settings_dir = new_settings_dir
            self.settings_manager.switch_dir(new_settings_dir)
    
    # ==================== Internal Helper Methods ====================
    
    def _get_current_center(self) -> Optional[Tuple[float, float]]:
        """
        Get current center in DISPLAY coordinates (for RotationTool).
        
        For QF: Returns transformed center (after image transformation)
        For PT: Returns original center (no transformation needed)
        
        Returns:
            Current center in display coordinates, or None
        """
        # If GUI provides a display center function, use it (needed for QF)
        if self._get_display_center_func:
            return self._get_display_center_func()
        
        # Default: use ImageData center (works for PT where no transform needed)
        if self._current_image_data:
            return self._current_image_data.center
        return None
    
    def _update_widgets_display(self):
        """Update widgets to display current settings."""
        if not self._current_filename:
            return
        
        # Update center widget
        # NOTE: update_current_center removed - GUI will update with transformed coords after processImage()
        # Only update mode indicator here
        if self._current_filename in self._center_settings:
            self._center_widget.update_mode_indicator(is_manual=True)
        else:
            self._center_widget.update_mode_indicator(is_manual=False)
        
        # Update rotation widget
        if self._current_filename in self._rotation_settings:
            rotation = self._rotation_settings[self._current_filename]['rotation']
            self._rotation_widget.update_rotation_display(rotation)
            self._rotation_widget.update_mode_indicator(is_manual=True)
        else:
            # Auto mode
            if self._current_image_data:
                auto_rotation = self._current_image_data.rotation
                if auto_rotation is not None:
                    self._rotation_widget.update_rotation_display(auto_rotation)
            self._rotation_widget.update_mode_indicator(is_manual=False)
    
    # ==================== Settings Persistence Hooks ====================

    def _after_center_save(self):
        """Update mode statistics after center settings change."""
        if self._file_manager and self._file_manager.names:
            self._update_mode_statistics_internal()

    def _after_rotation_save(self):
        """Update mode statistics after rotation settings change."""
        if self._file_manager and self._file_manager.names:
            self._update_mode_statistics_internal()
    
    # ==================== Public API for Blank/Mask Settings ====================
    
    def get_blank_mask_config(self):
        """
        Get current blank/mask configuration from settings directory.
        
        Returns:
            dict: {'apply_blank': bool, 'apply_mask': bool, 'blank_weight': float}
        """
        sm = self.settings_manager
        return {
            'apply_blank': sm.blank_enabled,
            'apply_mask': sm.mask_enabled,
            'blank_weight': sm.blank_weight,
        }
    
    def update_blank_mask_states(self):
        """
        Update the state of blank/mask checkboxes based on settings directory.
        
        Called automatically when folder changes (in on_file_loaded).
        No need for GUI to call this manually.
        """
        if not self._settings_dir:
            return
        
        self._blank_mask_widget.update_from_directory(self.settings_manager.settings_path)
    
    # ==================== Public API for Inpainting ====================

    @property
    def inpaint_enabled(self) -> bool:
        """Whether inpainting is enabled (set by GUI via inpaint checkbox)."""
        return getattr(self, '_inpaint_enabled', False)

    @inpaint_enabled.setter
    def inpaint_enabled(self, value: bool):
        self._inpaint_enabled = value

    # ==================== Public API for Quadrant Folded (PT-specific) ====================
    
    def create_qf_checkbox(self):
        """
        Create Quadrant Folded checkbox for ProjectionTraces.
        This is PT-specific and optional - only called by ProjectionTracesGUI.
        
        The checkbox is created once and reused. Workspace handles all state
        management internally - GUI only needs the widget for layout placement.
        
        Returns:
            QCheckBox: The quadrant folded checkbox widget for GUI to place in layout
        
        Example:
            # In ProjectionTracesGUI
            qf_checkbox = self.workspace.create_qf_checkbox()
            self.propLayout.addWidget(qf_checkbox, 0, 0, 1, 2)
        """
        if self.qf_checkbox is not None:
            return self.qf_checkbox
        
        from PySide6.QtWidgets import QCheckBox
        
        self.qf_checkbox = QCheckBox("Quadrant Folded?")
        self.qf_checkbox.setChecked(False)  # Default: not quadrant folded
        self.qf_checkbox.setToolTip(
            "Mark the loaded image as already quadrant-folded.\n"
            "When enabled, ProjectionTraces skips its own folding step and treats the image "
            "as a pre-folded pattern.")
        
        # Connect to internal handler - workspace manages all logic
        self.qf_checkbox.stateChanged.connect(self._on_qf_changed)
        
        print("Created Quadrant Folded checkbox for ProjectionTraces")
        
        return self.qf_checkbox
    
    def _on_qf_changed(self):
        """
        Internal: Handle quadrant folded checkbox state change.
        
        Updates ImageData's quadrant_folded state and triggers reprocess.
        GUI doesn't need to know about this - it happens automatically.
        """
        if self._current_image_data is None:
            return
        
        is_checked = self.qf_checkbox.isChecked()
        self._current_image_data.quadrant_folded = is_checked
        
        print(f"Quadrant folded state changed to: {is_checked}")
        
        # Notify GUI to reprocess image
        self.needsReprocess.emit()
    
    def _sync_qf_from_image_data(self, image_data):
        """
        Internal: Sync checkbox state from ImageData.
        
        Called automatically when image changes. ImageData auto-detects
        quadrant folded state from filename/metadata, this syncs it to UI.
        
        Args:
            image_data: ImageData object with quadrant_folded state
        """
        if self.qf_checkbox is None:
            return  # Checkbox not created (not PT), skip
        
        quadrant_folded = image_data.is_quadrant_folded
        
        # Temporarily disconnect signal to avoid triggering reprocess
        try:
            self.qf_checkbox.stateChanged.disconnect(self._on_qf_changed)
        except RuntimeError:
            pass  # Signal not connected, that's fine
        
        self.qf_checkbox.setChecked(quadrant_folded)
        
        # Reconnect signal
        self.qf_checkbox.stateChanged.connect(self._on_qf_changed)
        
        print(f"Synced quadrant folded state from ImageData: {quadrant_folded}")
    
    def _on_blank_setting_clicked(self):
        """Handle blank image settings button click."""
        if not self._file_manager or self._file_manager.current_image is None:
            return
        
        image = self._file_manager.current_image.copy()
        settings_dir_path = self.settings_manager.settings_path
        
        try:
            settings_dir_path.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            print("Exception occurred:", e)
            import traceback
            traceback.print_exc()
            return
        
        # Get display options from viewer
        display_opts = self._image_viewer.get_display_options()
        
        # Import and show dialog
        from ..ImageBlankDialog import ImageBlankDialog
        dialog = ImageBlankDialog(
            image_data=image,
            settings_dir_path=settings_dir_path,
            vmin=display_opts['vmin'],
            vmax=display_opts['vmax']
        )
        
        dialog_code = dialog.exec()
        
        if dialog_code == QDialog.Accepted:
            self._blank_mask_widget.update_from_directory(settings_dir_path)
            if self._current_image_data:
                self._current_image_data.apply_blank = self._blank_mask_widget.applyBlankImageChkBx.isChecked()
                self._current_image_data.invalidate_blank_mask_cache()
            self.needsReprocess.emit()
        else:
            self._blank_mask_widget.update_from_directory(settings_dir_path)
    
    def _on_blank_checkbox_changed(self, state):
        """
        Handle blank checkbox change.
        Creates/removes .blank_image_disabled flag file and updates ImageData.
        """
        if not self._settings_dir:
            return
        
        is_checked = (state == 2)
        self.settings_manager.set_blank_enabled(is_checked)
        print(f"{'Enabled' if is_checked else 'Disabled'} blank image application")
        
        if self._current_image_data:
            self._current_image_data.apply_blank = is_checked
            self._current_image_data.reset_preprocessing()
            print(f"Updated ImageData.apply_blank = {is_checked}")
        
        self.needsReprocess.emit()
    
    def _on_mask_checkbox_changed(self, state):
        """
        Handle mask checkbox change.
        Creates/removes .mask_disabled flag file and updates ImageData.
        """
        if not self._settings_dir:
            return
        
        is_checked = (state == 2)
        self.settings_manager.set_mask_enabled(is_checked)
        print(f"{'Enabled' if is_checked else 'Disabled'} mask application")
        
        if self._current_image_data:
            self._current_image_data.apply_mask = is_checked
            self._current_image_data.reset_preprocessing()
            print(f"Updated ImageData.apply_mask = {is_checked}")
        
        self.needsReprocess.emit()
    
    def _on_mask_setting_clicked(self):
        """Handle mask settings button click."""
        if not self._file_manager or self._file_manager.current_image is None:
            return
        
        image = self._file_manager.current_image.copy()
        settings_dir_path = self.settings_manager.settings_path
        
        try:
            settings_dir_path.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            print("Exception occurred:", e)
            import traceback
            traceback.print_exc()
            return
        
        # Get display options from viewer
        display_opts = self._image_viewer.get_display_options()
        
        # Import and show dialog
        from ..ImageMaskDialog import ImageMaskDialog
        dialog = ImageMaskDialog(
            image_data=image,
            settings_dir_path=settings_dir_path,
            vmin=display_opts['vmin'],
            vmax=display_opts['vmax']
        )
        
        dialog_code = dialog.exec()
        
        if dialog_code == QDialog.Accepted:
            # Update checkbox states (signals are blocked inside, so stateChanged won't fire)
            self._blank_mask_widget.update_from_directory(settings_dir_path)
            # Sync apply_mask and invalidate cache, since signals were blocked during update
            if self._current_image_data:
                self._current_image_data.apply_mask = self._blank_mask_widget.applyMaskChkBx.isChecked()
                self._current_image_data.invalidate_blank_mask_cache()
            self.needsReprocess.emit()
        else:
            # Still update checkbox states in case settings were deleted
            self._blank_mask_widget.update_from_directory(settings_dir_path)

    
    # ===== Public Convenience Methods =====
    
    def on_file_loaded(self, dir_path: str):
        """
        Called when a new file/folder is loaded (BEFORE first image loads).
        
        Resolves the output directory (via association store or user dialog),
        updates settings directory, and reloads persistent settings.
        This is the right place for folder-level initialization.
        
        Args:
            dir_path: Directory path of the loaded file/folder
        """
        # Resolve output directory for this input
        from .output_dir_dialog import resolve_output_directory
        ctx = resolve_output_directory(dir_path, parent=self)
        if ctx is None:
            # User cancelled — keep previous context unchanged
            return
        self.dir_context = ctx
        
        # Let FileManager know the output dir for scan cache fallback
        self.navigator.file_manager.output_dir = ctx.output_dir
        
        # Settings live under the output directory
        self.set_settings_dir(ctx.output_dir)
        
        # Update blank/mask checkbox states based on new directory
        self.update_blank_mask_states()
        
        # Mark that next image will be the first in this folder
        self._first_image_in_folder = True

    def change_output_directory(self):
        """Let the user pick a new output directory for the current input.

        Opens the OutputDirDialog pre-filled with the current output path.
        On success, updates dir_context, association store, settings dir,
        and emits outputDirChanged so GUIs can reset CSV managers etc.
        """
        from .output_dir_dialog import OutputDirDialog, _persist_association
        from PySide6.QtWidgets import QDialog, QMessageBox

        if not self.dir_context:
            QMessageBox.information(
                self, "No folder loaded",
                "Please load a folder before changing the output directory.")
            return

        input_dir = self.dir_context.input_dir
        dlg = OutputDirDialog(input_dir, self.dir_context.output_dir, parent=self)
        if dlg.exec() != QDialog.Accepted or dlg.chosen_output is None:
            return

        from ...utils.directory_context import DirectoryContext
        new_output = dlg.chosen_output
        _persist_association(input_dir, new_output)
        self.dir_context = DirectoryContext(input_dir=input_dir, output_dir=new_output)
        self.navigator.file_manager.output_dir = new_output
        self.set_settings_dir(new_output)
        self.update_blank_mask_states()
        self.outputDirChanged.emit(new_output)

    def on_image_changed(self, img, filename: str, dir_path: str):
        """
        Called when a new image is loaded.
        
        This is the main processing pipeline entry point:
        1. Creates ImageData with workspace settings
        2. Syncs PT-specific quadrant folded state to checkbox
        3. Shows center and rotation status notification on first image load
        4. Emits imageDataReady signal for GUI to process
        
        Args:
            img: Image array (numpy ndarray)
            filename: Name of the image file
            dir_path: Directory path
        """
        # Create ImageData with workspace settings (center, rotation, blank, mask)
        image_data = self.create_image_data(img, filename)
        
        # Sync quadrant folded state (PT-specific, no-op if checkbox doesn't exist)
        self._sync_qf_from_image_data(image_data)
        
        # IMPORTANT (UI responsiveness):
        # - Showing the first-image notification and immediately emitting imageDataReady can cause
        #   the popup to appear slowly or briefly render black, because downstream slots often do
        #   heavy synchronous processing (e.g., processImage()) on the UI thread.
        # - To ensure Qt gets a chance to paint the popup, we schedule the notification for the
        #   next event loop turn, and delay the imageDataReady emission slightly.
        from PySide6.QtCore import QTimer

        # Show settings status notification on first image in folder (blocking)
        # This must complete BEFORE emitting imageDataReady to avoid rendering issues
        if self._first_image_in_folder:
            self._first_image_in_folder = False
            self._show_first_image_settings_notification(filename)

        # Emit high-level signal with ImageData (delayed to allow UI paint)
        # GUIs should listen to this instead of imageChanged
        QTimer.singleShot(
            30,
            lambda d=image_data: self.imageDataReady.emit(d),
        )
    
    def _show_first_image_settings_notification(self, filename: str):
        """
        Show auto-dismissing notification about center and rotation status when first image is loaded.
        
        Checks if current image has manual center/rotation settings and shows popup with:
        - Center status: manual (with coordinates and source) or auto-detected
        - Rotation status: manual (with angle and source) or auto-detected
        
        The notification automatically disappears after 4 seconds.
        
        Args:
            filename: Name of the current image file
        """
        from PySide6.QtWidgets import QMessageBox
        from PySide6.QtCore import QTimer, Qt
        
        # Check center status
        has_manual_center = filename in self._center_settings
        if has_manual_center:
            center_data = self._center_settings[filename]
            center = center_data.get('center', None)
            center_source = center_data.get('source', 'unknown')
            if center:
                center_text = f"Using previously set manual center: ({center[0]:.1f}, {center[1]:.1f}) [{center_source}]"
            else:
                center_text = f"Using previously set manual center [{center_source}]"
        else:
            center_text = "No manual center set, will use auto-detected center"
        
        # Check rotation status
        has_manual_rotation = filename in self._rotation_settings
        if has_manual_rotation:
            rotation_data = self._rotation_settings[filename]
            rotation = rotation_data.get('rotation', None)
            rotation_source = rotation_data.get('source', 'unknown')
            if rotation is not None:
                rotation_text = f"Using previously set manual rotation: {rotation:.2f}° [{rotation_source}]"
            else:
                rotation_text = f"Using previously set manual rotation [{rotation_source}]"
        else:
            rotation_text = "No manual rotation set, will use auto-detected rotation"
        
        # Determine title and icon
        if has_manual_center or has_manual_rotation:
            title = "Settings Loaded"
        else:
            title = "Auto-Detection Mode"
        
        # Build message - just concatenate the two lines
        message = f"{center_text}\n{rotation_text}"
        
        # Use top-level window as parent for proper display.
        # self.window() returns self when the workspace has no Qt parent (it is
        # used as a component bag and its sub-widgets are reparented elsewhere).
        # In that case fall back to the currently active window so the popup is
        # centred on screen rather than placed at (0, 0).
        parent = self.window()
        if parent is self:
            from PySide6.QtWidgets import QApplication
            active = QApplication.activeWindow()
            if active is not None:
                parent = active
        
        # Create modal message box with OK button
        msg_box = QMessageBox(parent)
        msg_box.setIcon(QMessageBox.Information)
        msg_box.setText(message)
        msg_box.setStandardButtons(QMessageBox.Ok)
        msg_box.setWindowModality(Qt.ApplicationModal)
        
        # Get OK button and set initial text with countdown
        ok_button = msg_box.button(QMessageBox.Ok)
        
        # Store reference to prevent garbage collection
        self._notification_box = msg_box
        self._notification_countdown = 7  # seconds
        
        # Update button text with countdown
        def update_countdown():
            if hasattr(self, '_notification_box') and self._notification_box:
                if hasattr(self, '_notification_countdown') and self._notification_countdown > 0:
                    ok_button.setText(f"OK ({self._notification_countdown}s)")
                    self._notification_countdown -= 1
                    # Schedule next update in 1 second
                    QTimer.singleShot(1000, update_countdown)
                else:
                    # Time's up, close the dialog
                    self._notification_box.close()
                    self._notification_box.deleteLater()
                    self._notification_box = None
        
        # Start countdown before exec (timer will still run)
        update_countdown()
        
        # Show the message box modally (blocks until closed)
        msg_box.exec()
    
    def create_image_data(self, img, filename):
        """
        Create ImageData for the current image with settings from workspace.
        
        This encapsulates the logic of pulling center/rotation/blank/mask settings
        from the workspace and creating a properly configured ImageData instance.
        
        Args:
            img: Image array (numpy ndarray)
            filename: Name of the image file
            
        Returns:
            ImageData: Configured ImageData instance
        """
        from ...utils.image_data import ImageData
        
        return ImageData.from_settings_panel(
            img, self._settings_dir, filename, self
        )
    
    def load_from_file(self, filepath: str, start_background_scan: bool = True):
        """
        Load a file or directory into the workspace.
        
        This is a convenience method that delegates to the internal navigator.
        
        Args:
            filepath: Full path to image file or directory
            start_background_scan: Whether to start background scan for HDF5 expansion
        
        Example:
            workspace = ProcessingWorkspace(settings_dir="/path")
            workspace.load_from_file("/path/to/image.tif")
        """
        self.navigator.load_from_file(filepath, start_background_scan)
    
    # ===== Calibration Dialog Management =====
    
    def _load_calibration_cache(self):
        """
        Load calibration cache from settings directory.
        
        Returns:
            dict or None: Cache dictionary with 'path', 'settings', 'version' if exists,
                         None otherwise
        """
        return self.settings_manager.load_calibration_cache()
    
    def show_calibration_dialog(self, image_data, force=False):
        """
        Show calibration settings dialog for current image.
        
        This method manages the CalibrationSettings dialog, which allows users to:
        - Set detector parameters
        - Define calibration center
        - Configure beam properties
        
        The dialog is only shown if:
        - force=True, OR
        - Cached calibration settings exist
        
        Args:
            image_data: ImageData instance for current image
            force: If True, show dialog even if no cached settings exist
        
        Returns:
            bool: True if calibration was set and accepted, False otherwise
        """
        from ...CalibrationSettings import CalibrationSettings
        
        # Check for cached settings (without creating dialog)
        cache = self._load_calibration_cache()
        
        # Decide whether to show dialog
        if cache is None and not force:
            return False  # No cache and not forced, don't show
        
        cal_dialog = CalibrationSettings(
            str(self._settings_dir),
            center=image_data.center,
            quadrant_folded=image_data.is_quadrant_folded,
            initial_settings=cache,
            settings_manager=self.settings_manager,
        )
        
        cal_dialog.recalculate = False
        
        # Show dialog
        result = cal_dialog.exec_()
        if result == 1:  # Dialog accepted
            cal_settings = cal_dialog.getValues()
            
            if cal_settings is not None:
                if 'center' in cal_settings:
                    # Update center using workspace method
                    self.set_center_from_source(
                        image_data.img_name,
                        cal_settings['center'],
                        "calibration"
                    )
                else:
                    # Clear calibration center if unchecked
                    self.set_center_from_source(
                        image_data.img_name,
                        None,
                        "calibration_cleared"
                    )
                
                return True
        
        return False
    
    
    @property
    def current_image(self):
        """
        Get current image array from navigator.
        
        Returns:
            numpy.ndarray or None: Current image data
        """
        return self.navigator.get_current_image()
    
    @property
    def file_list(self):
        """Get list of file names from file_manager."""
        return self._file_manager.names if self._file_manager else []
    
    @property
    def image_viewer(self):
        """
        Get the ImageViewerWidget for direct access.
        
        Use this to manually display processed images:
            workspace.image_viewer.display_image(processed_img)
        """
        return self._image_viewer
    
    @property
    def file_manager(self):
        """
        Get the FileManager for batch operations.
        
        Use this for batch processing:
            for idx in range(len(workspace.file_manager.names)):
                img = workspace.file_manager.get_image_by_index(idx)
                process(img)
        """
        return self._file_manager
    
    @property
    def calibration_settings(self):
        """
        Get calibration settings from cache file.
        
        Returns the 'settings' dict from calibration.info, which contains:
        - type: "img" or "cont"
        - For "img" type: silverB, radius
        - For "cont" type: lambda, sdd, pixel_size, scale
        - center: [x, y] (optional)
        - detector: detector name (optional)
        
        Returns:
            dict or None: Calibration settings, or None if not available
        """
        cache = self._load_calibration_cache()
        if cache is not None and 'settings' in cache:
            return cache['settings']
        return None