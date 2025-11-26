# ProcessingGUI Usage Guide

## Overview

`ProcessingGUI` is a middle-layer base class for image processing GUIs. It extends `BaseGUI` with common processing functionality used by QuadrantFoldingGUI, ProjectionTracesGUI, and EquatorialGUI.

## Inheritance Hierarchy

```
BaseGUI (UI structure)
  ↓
ProcessingGUI (adds: center, rotation, blank/mask)
  ↓
QuadrantFoldingGUI, ProjectionTracesGUI, EquatorialGUI

XRayViewerGUI (directly inherits BaseGUI - no processing needed)
```

## What ProcessingGUI Provides

### 1. Center Detection Tools

- **Chords Center Tool**: Interactive tool for finding center by drawing chords
- **Perpendiculars Center Tool**: Interactive tool for finding center by perpendicular lines
- **Calibration Settings**: Launch calibration dialog
- **Manual Center Setting**: Set center manually

### 2. Rotation Tools

- **Rotation Angle Tool**: Interactive tool for setting rotation angle
- **Automatic Rotation**: Calculate rotation automatically

### 3. Blank/Mask Settings

- **Blank Image**: Background subtraction
- **Mask Image**: Image masking

### 4. Settings Persistence

- Save/load center settings per image
- Save/load rotation settings per image

## How to Use

### 1. Inherit from ProcessingGUI

```python
from .processing_gui import ProcessingGUI

class MyProcessingGUI(ProcessingGUI):
    def __init__(self):
        super().__init__()
        # Your initialization...
        self.initUI()
        self.setConnections()
```

### 2. Add Processing Widgets in _create_tabs()

```python
def _create_tabs(self):
    # Create standard image tab
    self._create_standard_image_tab(tab_title="Image")
    
    # Add center settings widget (from ProcessingGUI)
    self._create_center_settings()
    
    # Optionally add rotation and blank/mask settings
    self._create_rotation_settings()  # Optional
    self._create_blank_mask_settings()  # Optional
    
    # Add your specific widgets
    self._create_my_specific_widgets()
```

### 3. Implement Required Abstract Methods

ProcessingGUI requires you to implement these methods:

#### `_get_image_processor()`

Return your image processor object.

```python
def _get_image_processor(self):
    """Return the image processor"""
    return self.quadFold  # or self.projProc, self.bioImg, etc.
```

#### `_apply_center_from_tool(center, tool_name)`

Apply center result from interactive tools.

```python
def _apply_center_from_tool(self, center, tool_name):
    """Apply center result from tool"""
    # Extract coordinates
    x, y = center
    
    # Apply to your processor (implementation varies)
    self.my_processor.center = (x, y)
    
    # Reprocess image
    self.processImage()
```

#### `calibrationClicked()`

Handle calibration button click.

```python
def calibrationClicked(self):
    """Handle calibration settings"""
    from .CalibrationSettings import CalibrationSettings
    
    dlg = CalibrationSettings(self.dir_path)
    if dlg.exec() == QDialog.Accepted:
        self.calSettings = dlg.getValues()
        # Apply calibration...
```

#### `setCentBtnClicked()`

Handle manual center setting button.

```python
def setCentBtnClicked(self):
    """Handle manual center setting"""
    from .SetCentDialog import SetCentDialog
    
    dlg = SetCentDialog(self, image, current_center)
    if dlg.exec() == QDialog.Accepted:
        new_center = dlg.center
        # Apply center...
```

### 4. Call super()._additional_setup()

If you override `_additional_setup()`, make sure to call the parent:

```python
def _additional_setup(self):
    """Additional setup"""
    # Call parent to register processing tools
    super()._additional_setup()
    
    # Your additional setup...
    self.my_specific_initialization()
```

### 5. (Optional) Override Tool Registration

If you need to register additional tools:

```python
def _register_processing_tools(self):
    """Register tools (override to add more)"""
    # Call parent to register common tools
    super()._register_processing_tools()
    
    # Register your specific tools
    from .tools.my_custom_tool import MyCustomTool
    self.image_viewer.tool_manager.register_tool('my_tool', MyCustomTool)
```

## Complete Example

```python
from .processing_gui import ProcessingGUI
from PySide6.QtWidgets import QDialog

class MyProcessingGUI(ProcessingGUI):
    def __init__(self):
        super().__init__()
        self.my_processor = None
        self.initUI()
        self.setConnections()
    
    # ===== Required BaseGUI methods =====
    
    def _setup_window(self):
        from musclex import __version__
        self.setWindowTitle(f"My Processing GUI v.{__version__}")
    
    def _create_tabs(self):
        # Create standard image tab
        self._create_standard_image_tab(tab_title="Image")
        
        # Add processing widgets (from ProcessingGUI)
        self._create_center_settings()
        self._create_rotation_settings()
        self._create_blank_mask_settings()
        
        # Add your specific widgets
        self._create_my_widgets()
    
    def _create_menu_bar(self):
        menubar = self.menuBar()
        file_menu = menubar.addMenu('&File')
        # Add menu items...
    
    # ===== Required ProcessingGUI methods =====
    
    def _get_image_processor(self):
        """Return the image processor"""
        return self.my_processor
    
    def _apply_center_from_tool(self, center, tool_name):
        """Apply center from interactive tool"""
        x, y = center
        self.my_processor.center = (x, y)
        self.processImage()
    
    def calibrationClicked(self):
        """Handle calibration"""
        from .CalibrationSettings import CalibrationSettings
        dlg = CalibrationSettings(self.dir_path)
        if dlg.exec() == QDialog.Accepted:
            self.calSettings = dlg.getValues()
            # Apply...
    
    def setCentBtnClicked(self):
        """Handle manual center"""
        from .SetCentDialog import SetCentDialog
        dlg = SetCentDialog(self, self.image, self.center)
        if dlg.exec() == QDialog.Accepted:
            self.center = dlg.center
            # Apply...
    
    # ===== Your specific methods =====
    
    def _create_my_widgets(self):
        """Create application-specific widgets"""
        # Your widgets...
        pass
    
    def processImage(self):
        """Process the current image"""
        # Your processing logic...
        pass
```

## What You Get Automatically

When you inherit from `ProcessingGUI`, you automatically get:

### Attributes

- `self.centerSettings` - CenterSettingsWidget instance
- `self.rotationSettings` - RotationSettingsWidget instance (if created)
- `self.blankMaskSettings` - BlankMaskSettingsWidget instance (if created)
- `self.checkableButtons` - List for exclusive button behavior
- `self.calSettings` - Calibration settings dictionary

### Methods

- `_create_center_settings()` - Create center settings widget
- `_create_rotation_settings()` - Create rotation settings widget
- `_create_blank_mask_settings()` - Create blank/mask settings widget
- `_register_processing_tools()` - Register interactive tools
- `_on_chords_clicked()` - Handle chords tool activation
- `_on_perp_clicked()` - Handle perpendiculars tool activation

### Interactive Tools

Tools are automatically registered with `image_viewer.tool_manager`:

- `'chords'` - ChordsCenterTool
- `'perpendiculars'` - PerpendicularsCenterTool

Additional tools can be registered by overriding `_register_processing_tools()`.

## Differences from BaseGUI

| Feature | BaseGUI | ProcessingGUI |
|---------|---------|---------------|
| **Purpose** | UI structure | Image processing functionality |
| **Image Tab** | Standard 3-panel layout | Same + processing widgets |
| **Center Tools** | ❌ | ✅ (chords, perpendiculars, calibration) |
| **Rotation Tools** | ❌ | ✅ (manual, automatic) |
| **Blank/Mask** | ❌ | ✅ |
| **Tool Manager** | ✅ (built-in to image_viewer) | ✅ (registers processing tools) |
| **Used By** | XRayViewerGUI | QF, PT, Equator |

## Best Practices

1. **Always call super()**: When overriding methods, call `super().method_name()` first
2. **Use provided widgets**: Don't recreate center/rotation widgets, use the provided ones
3. **Implement all abstract methods**: ProcessingGUI requires specific methods
4. **Don't duplicate tool registration**: Tools are registered automatically in `_additional_setup()`
5. **Use `_apply_center_from_tool()`**: This is your hook to apply tool results to your specific processor

## See Also

- [BASE_GUI_USAGE.md](BASE_GUI_USAGE.md) - BaseGUI documentation
- [processing_gui.py](processing_gui.py) - ProcessingGUI source code
- [QuadrantFoldingGUI.py](QuadrantFoldingGUI.py) - Example usage
- [ProjectionTracesGUI.py](ProjectionTracesGUI.py) - Example usage

