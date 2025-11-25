# BaseGUI Usage Guide

## Overview

`BaseGUI` is a base class for all MuscleX GUI applications that provides a consistent initialization flow and common UI components.

## Benefits

- ✅ **Eliminates code duplication** - Common UI setup code written once
- ✅ **Consistent structure** - All GUIs follow the same initialization pattern
- ✅ **Easy to maintain** - Changes to common behavior only need to be made in one place
- ✅ **Flexible** - Subclasses can customize specific parts while reusing common code
- ✅ **Standard Image Tab** - Pre-built image tab with modern layout for all GUIs

## Initialization Flow

When you call `initUI()`, `BaseGUI` executes this sequence:

1. `_setup_window()` - Set window title and properties
2. `_setup_main_layout()` - Create scroll area and tab widget
3. `_create_tabs()` - Create application-specific tabs
4. `_create_status_bars()` - Create status bars
5. `_create_menu_bar()` - Create menu bar
6. `_additional_setup()` - Hook for extra initialization
7. `_finalize_ui()` - Show window and set size
8. `_position_floating_widgets()` - Position floating widgets (called automatically after window is rendered)

## How to Use

### 1. Inherit from BaseGUI

```python
from .base_gui import BaseGUI

class MyGUI(BaseGUI):  # Instead of QMainWindow
    def __init__(self):
        super().__init__()
        # ... your instance variables ...
        self.initUI()  # Calls BaseGUI's template method
```

### 2. Implement Required Methods

You **must** implement these three methods:

#### `_setup_window()`
Set your window title and any window-specific properties.

```python
def _setup_window(self):
    from musclex import __version__
    self.setWindowTitle(f"My Application v.{__version__}")
```

#### `_create_tabs()`
Create all tabs for your GUI.

```python
def _create_tabs(self):
    self._create_image_tab()
    self._create_result_tab()
```

#### `_create_menu_bar()`
Create your menu structure.

```python
def _create_menu_bar(self):
    menubar = self.menuBar()
    
    # File menu
    file_menu = menubar.addMenu('&File')
    save_action = QAction('Save Settings', self)
    save_action.triggered.connect(self.saveSettings)
    file_menu.addAction(save_action)
    
    # Help menu
    help_menu = menubar.addMenu('&Help')
    about_action = QAction('About', self)
    about_action.triggered.connect(self.showAbout)
    help_menu.addAction(about_action)
```

### 3. Optional Customization

You **can** override these methods if needed:

#### `_tabs_closable()` 
Return `True` if tabs should have close buttons.

```python
def _tabs_closable(self):
    return True  # Default is False
```

#### `_tab_stylesheet()`
Customize tab appearance.

```python
def _tab_stylesheet(self):
    return "QTabBar::tab { height: 20px; width: 150px; }"
```

#### `_additional_setup()`
Perform extra initialization after UI is created.

```python
def _additional_setup(self):
    self._register_tools()
    self._initialize_patches()
```

#### `_position_floating_widgets()`
Position floating widgets (like toggle buttons) after window is fully rendered.

This is called automatically via `QTimer.singleShot(0)` after `show()` to ensure widget geometry is calculated correctly.

```python
def _position_floating_widgets(self):
    self._position_toggle_button()  # Position your floating buttons
```

**Why is this needed?** When the window first shows, widget geometries may not be calculated yet. This hook ensures floating widgets are positioned correctly after the window is fully rendered.

#### `_create_status_bars()`
Create custom status bars (rarely needed).

```python
def _create_status_bars(self):
    # Custom status bar implementation
    pass
```

## Complete Example

```python
from .base_gui import BaseGUI
from PySide6.QtGui import QAction

class MyNewGUI(BaseGUI):
    def __init__(self):
        super().__init__()
        
        # Initialize your instance variables
        self.data = None
        self.settings = {}
        
        # Call initUI (executes template method)
        self.initUI()
        
        # Post-UI initialization
        self.setConnections()
    
    # ===== Required methods =====
    
    def _setup_window(self):
        from musclex import __version__
        self.setWindowTitle(f"My New GUI v.{__version__}")
    
    def _create_tabs(self):
        self._create_main_tab()
        self._create_results_tab()
    
    def _create_menu_bar(self):
        menubar = self.menuBar()
        
        file_menu = menubar.addMenu('&File')
        open_action = QAction('Open...', self)
        open_action.triggered.connect(self.open_file)
        file_menu.addAction(open_action)
        
        help_menu = menubar.addMenu('&Help')
        about_action = QAction('About', self)
        about_action.triggered.connect(self.show_about)
        help_menu.addAction(about_action)
    
    # ===== Optional customization =====
    
    def _additional_setup(self):
        # Custom initialization
        self.load_default_settings()
    
    # ===== Your GUI-specific methods =====
    
    def _create_main_tab(self):
        # Create your main tab
        pass
    
    def _create_results_tab(self):
        # Create your results tab
        pass
    
    def open_file(self):
        # Your file opening logic
        pass
    
    def show_about(self):
        # Your about dialog
        pass
```

## What BaseGUI Provides

### Attributes Created by BaseGUI

After `initUI()` completes, your GUI will have these attributes:

- `self.scrollArea` - Main scroll area
- `self.centralWidget` - Central widget
- `self.mainVLayout` - Main vertical layout
- `self.tabWidget` - Tab widget for organizing content
- `self.statusBar` - Main status bar
- `self.progressBar` - Progress bar in status bar
- `self.statusReport` - Status label
- `self.imgDetailOnStatusBar` - Image detail label
- `self.imgCoordOnStatusBar` - Coordinate label
- `self.imgPathOnStatusBar` - Path label
- `self.lowerStatusBar` - Lower status bar
- `self.left_status` - Left status label

### Default Behavior

- Window size: 1200x900 (can override `_finalize_ui()`)
- Tabs: Not closable (can override `_tabs_closable()`)
- Tab style: 40px height, 200px width (can override `_tab_stylesheet()`)
- Status bars: Standard two-bar layout (can override `_create_status_bars()`)

## Migration Guide

### Before (using QMainWindow)

```python
class MyGUI(QMainWindow):
    def __init__(self):
        super().__init__()
        self.initUI()
    
    def initUI(self):
        self.setWindowTitle("My GUI")
        
        # Setup scroll area
        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.centralWidget = QWidget(self)
        self.scrollArea.setWidget(self.centralWidget)
        self.mainVLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.scrollArea)
        
        # Setup tabs
        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 200px; }")
        self.mainVLayout.addWidget(self.tabWidget)
        
        # Create tabs
        self._create_main_tab()
        
        # Create status bars
        self.statusBar = QStatusBar()
        # ... (lots of status bar setup)
        
        # Create menu
        self._create_menu_bar()
        
        self.resize(1200, 900)
        self.show()
```

### After (using BaseGUI)

```python
class MyGUI(BaseGUI):
    def __init__(self):
        super().__init__()
        self.initUI()
    
    def _setup_window(self):
        self.setWindowTitle("My GUI")
    
    def _create_tabs(self):
        self._create_main_tab()
    
    def _create_menu_bar(self):
        # Just the menu creation code
        pass
```

Much cleaner! All the boilerplate is handled by `BaseGUI`.

## Currently Refactored GUIs

- ✅ `QuadrantFoldingGUI` - Refactored to use BaseGUI

## TODO

- [ ] Refactor `ProjectionTracesGUI` to use BaseGUI
- [ ] Refactor `XRayViewerGUI` to use BaseGUI

## Standard Image Tab

### Overview

`BaseGUI` provides `_create_standard_image_tab()`, a method that creates a standardized image tab with the modern QuadrantFolding layout:

```
[Left Panel: Select Buttons] [Center: ImageViewerWidget] [Right: CollapsibleRightPanel]
```

This **drastically reduces boilerplate** and ensures **consistent UI** across all MuscleX GUIs.

### Basic Usage

```python
def _create_tabs(self):
    # Create standard image tab with default settings
    # Note: display_panel is automatically added to right_panel
    self._create_standard_image_tab(tab_title="Image")
    
    # Add your GUI-specific display options
    self._add_display_options()
    
    # Add your GUI-specific settings to right panel
    # (display_panel is already there, just add your custom settings)
    self._create_my_settings()
    
    # Add navigation controls to bottom of right panel
    self.right_panel.add_bottom_widget(self.navControls)
```

### What It Creates

After calling `_create_standard_image_tab()`, you have access to:

**Tab Structure:**
- `self.imageTab` - The tab widget
- `self.imageTabLayout` - The horizontal layout (left | center | right)

**Left Panel (optional):**
- `self.leftWidget` - Container widget
- `self.selectImageButton` - Standard "Select Image" button
- `self.selectFolder` - Standard "Select Folder" button
- `self.bgWd` - Background widget (for compatibility)

**Center Panel (ImageViewerWidget):**
- `self.image_viewer` - The ImageViewerWidget instance
- `self.imageAxes`, `self.imageCanvas`, `self.imageFigure` - (backward compatibility)

**Display Controls (if `show_display_panel=True`):**
- `self.spminInt`, `self.spmaxInt` - Min/max intensity spinboxes
- `self.logScaleIntChkBx` - Log scale checkbox
- `self.persistIntensity` - Persist intensity checkbox
- `self.imgZoomInB`, `self.imgZoomOutB` - Zoom buttons
- Access via `self.image_viewer.display_panel`

**Right Panel:**
- `self.right_panel` - CollapsibleRightPanel instance
- Display panel is **automatically added** to right_panel (if `show_display_panel=True`)
- `self.navControls` - Navigation controls (created but not added - you add it)

### Parameters

```python
_create_standard_image_tab(
    tab_title: str = "Image",              # Tab title
    show_left_select_buttons: bool = True, # Show standard left panel
    show_display_panel: bool = True,       # Show display controls
    show_double_zoom: bool = True          # Show double zoom feature
)
```

### Customizing the Image Tab

#### 1. Add Display Options

Add custom widgets to the display panel:

```python
def _add_display_options(self):
    """Add GUI-specific display options"""
    self.myCheckbox = QCheckBox("My Option")
    self.image_viewer.display_panel.add_to_top_slot(self.myCheckbox)
```

#### 2. Add Settings to Right Panel

Add your settings groups to the right panel:

```python
def _create_my_settings(self):
    """Add GUI-specific settings"""
    # Display panel is automatically added to right panel
    # Just add your custom settings groups
    self.right_panel.add_widget(self.my_settings_group)
```

#### 3. Replace Left Panel

Some GUIs need custom left panels:

```python
def _create_tabs(self):
    # Create standard tab but without left buttons
    self._create_standard_image_tab(
        tab_title="Image",
        show_left_select_buttons=False
    )
    
    # Add your custom left panel
    self._create_custom_left_panel()
```

### Complete Example (QuadrantFoldingGUI)

```python
class QuadrantFoldingGUI(BaseGUI):
    def _create_tabs(self):
        # 1. Create standard image tab
        self._create_standard_image_tab(tab_title="Original Image")
        
        # 2. Add quadrant-specific display options
        self._add_display_options()
        
        # 3. Add quadrant-specific settings
        self._create_quadrant_settings()
        
        # 4. Add navigation controls to bottom
        self.right_panel.add_bottom_widget(self.navControls)
        
        # 5. Create result tab
        self._create_result_tab()
    
    def _add_display_options(self):
        """Add quadrant-specific display options"""
        self.showSeparator = QCheckBox("Show Quadrant Separator")
        self.showSeparator.setChecked(True)
        
        self.cropFoldedImageChkBx = QCheckBox("Save Cropped Image")
        self.cropFoldedImageChkBx.setChecked(False)
        
        # Add to display panel
        self.image_viewer.display_panel.add_to_top_slot(self.showSeparator)
        self.image_viewer.display_panel.add_to_top_slot(self.cropFoldedImageChkBx)
    
    def _create_quadrant_settings(self):
        """Add quadrant-specific settings to right panel"""
        self._create_processing_settings()
        self._create_center_rotation_settings()
        self._create_blank_mask_settings()
        self._create_result_processing_settings()
    
    def _position_floating_widgets(self):
        """Position floating toggle buttons after window is fully rendered"""
        self._position_toggle_button()
```

**Before:** ~250 lines of boilerplate  
**After:** ~15 lines + your specific content

### Benefits

✅ **Eliminates ~200+ lines of boilerplate per GUI**  
✅ **Consistent modern layout across all GUIs**  
✅ **Easy to customize** - just add your specific widgets  
✅ **Backward compatible** - exposes all legacy attributes  
✅ **Flexible** - can disable features or replace panels

## Notes

- BaseGUI uses the Template Method design pattern
- All existing functionality is preserved
- No behavior changes, just code organization
- Subclasses can still override any method if needed

