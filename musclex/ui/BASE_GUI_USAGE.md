# BaseGUI Usage Guide

## Overview

`BaseGUI` is a base class for all MuscleX GUI applications that provides a consistent initialization flow and common UI components.

## Benefits

- ✅ **Eliminates code duplication** - Common UI setup code written once
- ✅ **Consistent structure** - All GUIs follow the same initialization pattern
- ✅ **Easy to maintain** - Changes to common behavior only need to be made in one place
- ✅ **Flexible** - Subclasses can customize specific parts while reusing common code

## Initialization Flow

When you call `initUI()`, `BaseGUI` executes this sequence:

1. `_setup_window()` - Set window title and properties
2. `_setup_main_layout()` - Create scroll area and tab widget
3. `_create_tabs()` - Create application-specific tabs
4. `_create_status_bars()` - Create status bars
5. `_create_menu_bar()` - Create menu bar
6. `_additional_setup()` - Hook for extra initialization
7. `_finalize_ui()` - Show window and set size

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

## Notes

- BaseGUI uses the Template Method design pattern
- All existing functionality is preserved
- No behavior changes, just code organization
- Subclasses can still override any method if needed

