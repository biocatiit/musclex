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
from PySide6.QtWidgets import QWidget, QVBoxLayout, QDialog
from PySide6.QtCore import Signal

from ...utils.image_data import ImageData
from .center_settings_widget import CenterSettingsWidget
from .rotation_settings_widget import RotationSettingsWidget
from .blank_mask_settings_widget import BlankMaskSettingsWidget


class ImageSettingsPanel(QWidget):
    """
    Image Settings Panel - Settings management component for image processing.
    
    This component manages:
    - Settings widgets (Center, Rotation, Blank/Mask)
    - Settings data management (persisted to JSON)
    - Interactive tools integration (works with external ImageViewerWidget)
    - Complete tool completion handling workflow
    
    Note: This panel works with an external ImageViewerWidget (typically provided
    by BaseGUI). It registers tools to that viewer and manages settings, but does
    not create its own image viewer.
    
    Public Interface:
        update_display(image_data): Update widgets to show current settings
        get_manual_settings(filename): Get manual center/rotation for a file
        save_settings(): Save all settings to JSON files
    
    Signals:
        needsReprocess: Emitted when any setting changes requiring reprocessing
    
    Usage:
        # GUI creates image_viewer (via BaseGUI)
        self._create_standard_image_tab()
        
        # Create panel with reference to image_viewer
        panel = ImageSettingsPanel(
            settings_dir=self.filePath,
            image_viewer=self.image_viewer
        )
        
        # Add to right panel
        self.right_panel.add_widget(panel)
        
        # Connect signal
        panel.needsReprocess.connect(self.processImage)
    """
    
    # Signals
    needsReprocess = Signal()
    statusTextRequested = Signal(str)  # Request GUI to update status bar text
    
    def __init__(self, settings_dir: str, image_viewer, coord_transform_func=None,
                 file_manager=None):
        """
        Initialize the Image Settings Panel.
        
        Args:
            settings_dir: Directory path where settings will be stored
            image_viewer: External ImageViewerWidget (from BaseGUI)
            coord_transform_func: Optional function to transform coordinates from
                                 displayed (transformed) image to original image.
                                 Signature: (x, y) -> (orig_x, orig_y)
                                 This is needed when tools operate on transformed images.
            file_manager: Optional FileManager instance for batch operations.
                         Provides access to file list (names) and current index (current).
        """
        super().__init__()
        
        self._settings_dir = settings_dir
        self._image_viewer = image_viewer  # External viewer (not owned by this panel)
        self._coord_transform_func = coord_transform_func  # Coordinate transformation callback
        self._file_manager = file_manager  # FileManager reference for batch operations
        self._current_filename = None
        self._current_image_data = None
        
        # Orientation model and mode rotation state
        self._orientation_model = 0  # Default: Max Intensity
        self._mode_rotation = None  # Cached mode rotation value
        
        # Settings data (persisted to JSON)
        self._center_settings = {}     # {"filename": {"center": [x,y], "source": "..."}}
        self._rotation_settings = {}   # {"filename": {"rotation": angle, "source": "..."}}
        
        # Create internal components
        self._setup_components()
        
        # Setup UI layout (vertical layout with settings widgets)
        self._setup_ui()
        
        # Connect all internal signals
        self._connect_signals()
        
        # Load persisted settings
        self._load_settings()
    
    def _setup_components(self):
        """Create settings widgets (image_viewer is external)."""
        # Settings widgets
        self._center_widget = CenterSettingsWidget(parent=self)
        self._rotation_widget = RotationSettingsWidget(parent=self)
        self._blank_mask_widget = BlankMaskSettingsWidget(parent=self)
        
        # Register tools to the external image viewer's tool manager
        self._register_tools()
    
    def _setup_ui(self):
        """Setup UI layout - vertical layout with settings widgets."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        
        # Add settings widgets vertically
        layout.addWidget(self._center_widget)
        layout.addWidget(self._rotation_widget)
        layout.addWidget(self._blank_mask_widget)
        
        # Add stretch to push widgets to top
        layout.addStretch()
    
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
        self._center_settings[self._current_filename] = {
            'center': list(center),
            'source': source
        }
        self._save_center_settings()
        
        # 2. Update ImageData
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
        self._rotation_settings[self._current_filename] = {
            'rotation': new_rotation,
            'source': source
        }
        self._save_rotation_settings()
        
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
        self._center_settings[self._current_filename] = {
            'center': list(center),
            'source': 'center_rotate'
        }
        self._save_center_settings()
        
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
        if not self._current_image_data:
            print("Warning: No current image data for apply center")
            return
        
        center = self._current_image_data.center
        if not center:
            print("Warning: No center available to apply")
            return
        
        self.apply_center_to_batch(center, scope)
        print(f"Applied center {center} to {scope} images")
    
    def _on_restore_auto_center(self, scope: str):
        """Handle Restore Auto Center."""
        self.restore_auto_center_for_batch(scope)
        print(f"Restored auto center for {scope} images")
    
    def _on_apply_rotation(self, scope: str):
        """Handle Apply Rotation to batch."""
        if not self._current_image_data:
            print("Warning: No current image data for apply rotation")
            return
        
        rotation = self._current_image_data.rotation
        if rotation is None:
            print("Warning: No rotation available to apply")
            return
        
        self.apply_rotation_to_batch(rotation, scope)
        print(f"Applied rotation {rotation:.2f}° to {scope} images")
    
    def _on_restore_auto_rotation(self, scope: str):
        """Handle Restore Auto Rotation."""
        self.restore_auto_rotation_for_batch(scope)
        print(f"Restored auto rotation for {scope} images")
    
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
            # Remove manual center setting
            if filename in self._center_settings:
                del self._center_settings[filename]
        else:
            # Set manual center
            self._center_settings[filename] = {
                'center': list(center),
                'source': source
            }
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
            if filename in self._rotation_settings:
                del self._rotation_settings[filename]
        else:
            # Get current rotation from ImageData
            current_rotation = 0.0
            if self._current_image_data and self._current_filename == filename:
                current_rotation = self._current_image_data.rotation or 0.0
            
            # Calculate new absolute rotation
            new_rotation = current_rotation + rotation_increment
            
            # Set manual rotation
            self._rotation_settings[filename] = {
                'rotation': new_rotation,
                'source': source
            }
        
        self.save_settings()
        
        # Update ImageData if it's the current image
        if self._current_image_data and self._current_filename == filename:
            if rotation_increment is None:
                self._current_image_data.update_manual_rotation(None)
            else:
                new_rotation = self._rotation_settings[filename]['rotation']
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
        # Get manual center
        center = None
        if filename in self._center_settings:
            center_data = self._center_settings[filename]
            if 'center' in center_data:
                center = tuple(center_data['center'])
        
        # Get manual rotation
        rotation = None
        if filename in self._rotation_settings:
            rotation_data = self._rotation_settings[filename]
            if 'rotation' in rotation_data:
                rotation = rotation_data['rotation']
        
        return center, rotation
    
    def save_settings(self):
        """Save all settings to JSON files."""
        self._save_center_settings()
        self._save_rotation_settings()
    
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
            if self._current_filename in self._rotation_settings:
                del self._rotation_settings[self._current_filename]
                self._save_rotation_settings()
            
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
                img_name=filename
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
            self._center_settings[filename] = {
                'center': list(center),
                'source': 'propagated'
            }
        
        # Save settings
        self._save_center_settings()
        
        # Update statistics display
        self.update_mode_statistics(len(file_list))
    
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
            filename = file_list[idx]
            if filename in self._center_settings:
                del self._center_settings[filename]
        
        # If current image is in scope, update ImageData and UI
        if current_idx in indices:
            if self._current_image_data:
                self._current_image_data.update_manual_center(None)
            self._center_widget.update_mode_indicator(is_manual=False)
            # Notify GUI to reprocess
            self.needsReprocess.emit()
        
        # Save settings
        self._save_center_settings()
        
        # Update statistics display
        self.update_mode_statistics(len(file_list))
    
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
            filename = file_list[idx]
            self._rotation_settings[filename] = {
                'rotation': rotation,
                'source': 'propagated'
            }
        
        # Save settings
        self._save_rotation_settings()
        
        # Update statistics display
        self.update_mode_statistics(len(file_list))
    
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
            filename = file_list[idx]
            if filename in self._rotation_settings:
                del self._rotation_settings[filename]
        
        # If current image is in scope, update ImageData and UI
        if current_idx in indices:
            if self._current_image_data:
                self._current_image_data.update_manual_rotation(None)
            self._rotation_widget.update_mode_indicator(is_manual=False)
            # Notify GUI to reprocess
            self.needsReprocess.emit()
        
        # Save settings
        self._save_rotation_settings()
        
        # Update statistics display
        self.update_mode_statistics(len(file_list))
    
    def set_settings_dir(self, new_settings_dir: str):
        """
        Update the settings directory and reload settings.
        
        This should be called when the GUI changes to a new data directory.
        
        Args:
            new_settings_dir: New directory path for settings
        """
        if new_settings_dir != self._settings_dir:
            # Save current settings before switching
            if self._settings_dir:
                self.save_settings()
            
            # Update directory
            self._settings_dir = new_settings_dir
            
            # Reload settings from new directory
            self._load_settings()
            
            print(f"Settings directory updated to: {new_settings_dir}")
    
    # ==================== Internal Helper Methods ====================
    
    def _get_current_center(self) -> Optional[Tuple[float, float]]:
        """
        Get current center (for RotationTool).
        
        Returns:
            Current center or None
        """
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
    
    # ==================== Settings Persistence ====================
    
    def _load_settings(self):
        """Load all settings from JSON files."""
        self._load_center_settings()
        self._load_rotation_settings()
    
    def _load_center_settings(self):
        """Load center settings from JSON."""
        settings_path = Path(self._settings_dir) / "settings" / "center_settings.json"
        if settings_path.exists():
            try:
                with open(settings_path, 'r') as f:
                    self._center_settings = json.load(f)
                print(f"Loaded center settings for {len(self._center_settings)} images")
            except Exception as e:
                print(f"Error loading center settings: {e}")
                self._center_settings = {}
        else:
            print("No center settings file found")
    
    def _save_center_settings(self):
        """Save center settings to JSON and update mode statistics."""
        if not self._settings_dir:
            return
        
        settings_dir = Path(self._settings_dir) / "settings"
        settings_dir.mkdir(exist_ok=True)
        
        settings_path = settings_dir / "center_settings.json"
        try:
            with open(settings_path, 'w') as f:
                json.dump(self._center_settings, f, indent=2)
            print(f"Saved center settings for {len(self._center_settings)} images")
            
            # Auto-update mode statistics after saving
            if self._file_manager and self._file_manager.names:
                self._update_mode_statistics_internal()
        except Exception as e:
            print(f"Error saving center settings: {e}")
    
    def _load_rotation_settings(self):
        """Load rotation settings from JSON."""
        settings_path = Path(self._settings_dir) / "settings" / "rotation_settings.json"
        if settings_path.exists():
            try:
                with open(settings_path, 'r') as f:
                    self._rotation_settings = json.load(f)
                print(f"Loaded rotation settings for {len(self._rotation_settings)} images")
            except Exception as e:
                print(f"Error loading rotation settings: {e}")
                self._rotation_settings = {}
        else:
            print("No rotation settings file found")
    
    def _save_rotation_settings(self):
        """Save rotation settings to JSON and update mode statistics."""
        if not self._settings_dir:
            return
        
        settings_dir = Path(self._settings_dir) / "settings"
        settings_dir.mkdir(exist_ok=True)
        
        settings_path = settings_dir / "rotation_settings.json"
        try:
            with open(settings_path, 'w') as f:
                json.dump(self._rotation_settings, f, indent=2)
            print(f"Saved rotation settings for {len(self._rotation_settings)} images")
            
            # Auto-update mode statistics after saving
            if self._file_manager and self._file_manager.names:
                self._update_mode_statistics_internal()
        except Exception as e:
            print(f"Error saving rotation settings: {e}")
    
    # ==================== Public API for Blank/Mask Settings ====================
    
    def get_blank_mask_config(self):
        """
        Get current blank/mask configuration from settings directory.
        
        Returns:
            dict: {'apply_blank': bool, 'apply_mask': bool, 'blank_weight': float}
        """
        if not self._settings_dir:
            return {'apply_blank': False, 'apply_mask': False, 'blank_weight': 1.0}
        
        settings_dir = Path(self._settings_dir) / "settings"
        
        # Check blank image status
        blank_config_path = settings_dir / "blank_image_settings.json"
        blank_disabled_flag = settings_dir / ".blank_image_disabled"
        apply_blank = blank_config_path.exists() and not blank_disabled_flag.exists()
        
        # Check mask status
        mask_file_path = settings_dir / "mask.tif"
        mask_disabled_flag = settings_dir / ".mask_disabled"
        apply_mask = mask_file_path.exists() and not mask_disabled_flag.exists()
        
        # Get blank weight from config
        blank_weight = 1.0
        if apply_blank and blank_config_path.exists():
            try:
                import json
                with open(blank_config_path) as f:
                    config = json.load(f)
                    blank_weight = config.get('weight', 1.0)
            except Exception as e:
                print(f"Error reading blank weight: {e}")
        
        return {
            'apply_blank': apply_blank,
            'apply_mask': apply_mask,
            'blank_weight': blank_weight
        }
    
    def update_blank_mask_states(self):
        """
        Update the state of blank/mask checkboxes based on settings directory.
        Public method that can be called by GUI when folder changes.
        """
        if not self._settings_dir:
            return
        
        settings_dir = Path(self._settings_dir) / "settings"
        self._blank_mask_widget.update_from_directory(settings_dir)
    
    def _on_blank_setting_clicked(self):
        """Handle blank image settings button click."""
        if not self._file_manager or self._file_manager.current_image is None:
            return
        
        image = self._file_manager.current_image.copy()
        settings_dir_path = Path(self._settings_dir) / "settings"
        
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
            # Update checkbox states
            self._blank_mask_widget.update_from_directory(settings_dir_path)
            # Note: Cache clearing not needed - fingerprint will auto-detect changes
            self.needsReprocess.emit()
        else:
            # Still update checkbox states in case settings were deleted
            self._blank_mask_widget.update_from_directory(settings_dir_path)
    
    def _on_blank_checkbox_changed(self, state):
        """
        Handle blank checkbox change.
        Creates/removes .blank_image_disabled flag file and updates ImageData.
        """
        if not self._settings_dir:
            return
        
        settings_dir = Path(self._settings_dir) / "settings"
        blank_disabled_flag = settings_dir / ".blank_image_disabled"
        
        # Qt.CheckState.Checked = 2, Qt.CheckState.Unchecked = 0
        is_checked = (state == 2)
        
        if is_checked:
            # Remove the disabled flag if it exists
            if blank_disabled_flag.exists():
                blank_disabled_flag.unlink()
                print("Enabled blank image application")
        else:
            # Create the disabled flag
            settings_dir.mkdir(exist_ok=True)
            blank_disabled_flag.touch()
            print("Disabled blank image application")
        
        # Update ImageData's apply_blank flag
        if self._current_image_data:
            self._current_image_data.apply_blank = is_checked
            # Clear cached images so they'll be regenerated
            self._current_image_data._processed_img = None
            self._current_image_data._preprocessing_applied = False
            print(f"Updated ImageData.apply_blank = {is_checked}")
        
        # QuadrantFolder.updateInfo() will get fresh image from ImageData on next process()
        self.needsReprocess.emit()
    
    def _on_mask_checkbox_changed(self, state):
        """
        Handle mask checkbox change.
        Creates/removes .mask_disabled flag file and updates ImageData.
        """
        if not self._settings_dir:
            return
        
        settings_dir = Path(self._settings_dir) / "settings"
        mask_disabled_flag = settings_dir / ".mask_disabled"
        
        # Qt.CheckState.Checked = 2, Qt.CheckState.Unchecked = 0
        is_checked = (state == 2)
        
        if is_checked:
            # Remove the disabled flag if it exists
            if mask_disabled_flag.exists():
                mask_disabled_flag.unlink()
                print("Enabled mask application")
        else:
            # Create the disabled flag
            settings_dir.mkdir(exist_ok=True)
            mask_disabled_flag.touch()
            print("Disabled mask application")
        
        # Update ImageData's apply_mask flag
        if self._current_image_data:
            self._current_image_data.apply_mask = is_checked
            # Clear cached images so they'll be regenerated
            self._current_image_data._processed_img = None
            self._current_image_data._preprocessing_applied = False
            print(f"Updated ImageData.apply_mask = {is_checked}")
        
        # QuadrantFolder.updateInfo() will get fresh image from ImageData on next process()
        self.needsReprocess.emit()
    
    def _on_mask_setting_clicked(self):
        """Handle mask settings button click."""
        if not self._file_manager or self._file_manager.current_image is None:
            return
        
        image = self._file_manager.current_image.copy()
        settings_dir_path = Path(self._settings_dir) / "settings"
        
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
            # Update checkbox states
            self._blank_mask_widget.update_from_directory(settings_dir_path)
            # Note: Cache clearing not needed - fingerprint will auto-detect changes
            self.needsReprocess.emit()
        else:
            # Still update checkbox states in case settings were deleted
            self._blank_mask_widget.update_from_directory(settings_dir_path)

