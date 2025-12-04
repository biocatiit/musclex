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
from PySide6.QtWidgets import QWidget, QVBoxLayout
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
    
    # Single signal - GUI only needs to know when to reprocess
    needsReprocess = Signal()
    
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
        
        tool_mgr = self._image_viewer.tool_manager
        
        # Register center finding tools
        tool_mgr.register_tool('chords', ChordsCenterTool)
        tool_mgr.register_tool('perpendiculars', PerpendicularsCenterTool)
        
        # Register rotation tool (needs current center as positional arg)
        tool_mgr.register_tool('rotation', RotationTool, self._get_current_center)
    
    def _connect_signals(self):
        """Connect all internal signals."""
        # Tool buttons -> Activate/deactivate tools
        self._center_widget.setCentByChords.clicked.connect(
            lambda checked: self._on_chords_button_clicked(checked)
        )
        self._center_widget.setCentByPerp.clicked.connect(
            lambda checked: self._on_perp_button_clicked(checked)
        )
        self._rotation_widget.setRotationButton.clicked.connect(
            lambda checked: self._on_rotation_button_clicked(checked)
        )
        
        # Tool completed -> Handle result
        self._image_viewer.toolCompleted.connect(self._on_tool_completed)
        
        # Apply/Restore buttons
        self._center_widget.applyCenterRequested.connect(self._on_apply_center)
        self._center_widget.restoreAutoCenterRequested.connect(self._on_restore_auto_center)
        self._rotation_widget.applyRotationRequested.connect(self._on_apply_rotation)
        self._rotation_widget.restoreAutoRotationRequested.connect(self._on_restore_auto_rotation)
        
        # Blank/Mask checkboxes -> Reprocess
        # TODO: Connect blank/mask signals when they are available
        # self._blank_mask_widget.blankChanged.connect(self._on_blank_changed)
        # self._blank_mask_widget.maskChanged.connect(self._on_mask_changed)
    
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
        self._center_widget.update_current_center(center)
        self._center_widget.update_mode_indicator(is_manual=True)
        
        # Uncheck button
        self._center_widget.setCentByChords.setChecked(False)
        self._center_widget.setCentByPerp.setChecked(False)
        
        # 4. Notify GUI
        self.needsReprocess.emit()
        
        print(f"Center set to {center} from {source}")
    
    def _handle_rotation_result(self, angle: float, source: str):
        """
        Handle rotation result from tools.
        
        Similar workflow as center handling.
        """
        if not self._current_filename:
            return
        
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
        
        # Uncheck button
        self._rotation_widget.setRotationButton.setChecked(False)
        
        # 4. Notify GUI
        self.needsReprocess.emit()
        
        print(f"Rotation set to {new_rotation}° from {source}")
    
    def _handle_center_rotate_result(self, result: dict):
        """
        Handle center_rotate tool result (sets both center and rotation).
        
        This tool allows setting center and rotation together in one interaction.
        Result format: {'center': (x, y), 'angle': angle}
        """
        if not self._current_filename:
            return
        
        center = tuple(result['center'])
        angle = result['angle']
        
        # Transform coordinates from displayed image to original image
        if self._coord_transform_func:
            center = self._coord_transform_func(center[0], center[1])
            print(f"  Transformed center to original coordinates: {center}")
        
        # Get current rotation from ImageData
        current_rotation = 0.0
        if self._current_image_data:
            current_rotation = self._current_image_data.rotation or 0.0
        
        # Calculate new absolute rotation
        new_rotation = current_rotation + angle
        
        # 1. Save both settings
        self._center_settings[self._current_filename] = {
            'center': list(center),
            'source': 'center_rotate'
        }
        self._rotation_settings[self._current_filename] = {
            'rotation': new_rotation,
            'source': 'center_rotate'
        }
        self._save_center_settings()
        self._save_rotation_settings()
        
        # 2. Update ImageData
        if self._current_image_data:
            self._current_image_data.update_manual_center(center)
            self._current_image_data.update_manual_rotation(new_rotation)
        
        # 3. Update UI
        self._center_widget.update_current_center(center)
        self._center_widget.update_mode_indicator(is_manual=True)
        self._rotation_widget.update_rotation_display(new_rotation)
        self._rotation_widget.update_mode_indicator(is_manual=True)
        
        # Uncheck the center_rotate button (if it exists in centerSettings)
        if hasattr(self._center_widget, 'setCenterRotationButton'):
            self._center_widget.setCenterRotationButton.setChecked(False)
        
        # 4. Notify GUI
        self.needsReprocess.emit()
        
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
    
    def update_mode_statistics(self, total_files: int):
        """
        Update mode display statistics for both center and rotation widgets.
        
        Args:
            total_files: Total number of files in the current directory
        """
        auto_center_count = total_files - len(self._center_settings)
        auto_rotation_count = total_files - len(self._rotation_settings)
        
        self._center_widget.update_mode_display(auto_center_count, total_files)
        self._rotation_widget.update_mode_display(auto_rotation_count, total_files)
    
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
        if self._current_filename in self._center_settings:
            center = tuple(self._center_settings[self._current_filename]['center'])
            self._center_widget.update_current_center(center)
            self._center_widget.update_mode_indicator(is_manual=True)
        else:
            # Auto mode - show auto-calculated center if available
            if self._current_image_data:
                auto_center = self._current_image_data.center
                self._center_widget.update_current_center(auto_center)
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
        """Save center settings to JSON."""
        if not self._settings_dir:
            return
        
        settings_dir = Path(self._settings_dir) / "settings"
        settings_dir.mkdir(exist_ok=True)
        
        settings_path = settings_dir / "center_settings.json"
        try:
            with open(settings_path, 'w') as f:
                json.dump(self._center_settings, f, indent=2)
            print(f"Saved center settings for {len(self._center_settings)} images")
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
        """Save rotation settings to JSON."""
        if not self._settings_dir:
            return
        
        settings_dir = Path(self._settings_dir) / "settings"
        settings_dir.mkdir(exist_ok=True)
        
        settings_path = settings_dir / "rotation_settings.json"
        try:
            with open(settings_path, 'w') as f:
                json.dump(self._rotation_settings, f, indent=2)
            print(f"Saved rotation settings for {len(self._rotation_settings)} images")
        except Exception as e:
            print(f"Error saving rotation settings: {e}")

