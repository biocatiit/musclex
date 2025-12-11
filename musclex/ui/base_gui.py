"""
Base GUI class for MuscleX image applications.

Provides common initialization flow and UI components for all MuscleX GUI applications.
Uses Template Method pattern to allow subclasses to customize specific steps.

This is a minimal framework class. For image processing GUIs, use ProcessingWorkspace.
For image viewing GUIs, use ImageNavigatorWidget.

Copyright 1999 Illinois Institute of Technology
"""

from PySide6.QtWidgets import (
    QMainWindow, QScrollArea, QWidget, QVBoxLayout, QHBoxLayout, QTabWidget,
    QStatusBar, QProgressBar, QLabel
)
from PySide6.QtCore import QTimer


class BaseGUI(QMainWindow):
    """
    Base class for all MuscleX GUI applications.
    
    This class defines the standard initialization sequence for GUI applications.
    It provides only the framework - subclasses use ProcessingWorkspace or 
    ImageNavigatorWidget for actual image handling.
    
    Initialization Flow:
        0. __init__()               - Initialize core components
        1. _setup_window()          - Set window title and properties
        2. _setup_main_layout()     - Create scroll area and tab widget
        3. _create_tabs()           - Create application-specific tabs
        4. _create_status_bars()    - Create status bars
        5. _create_menu_bar()       - Create menu bar
        6. _additional_setup()      - Hook for extra initialization
        7. _finalize_ui()           - Final window setup
    
    Subclasses must implement:
        - _setup_window()
        - _create_tabs()
        - _create_menu_bar()
    
    Subclasses may override:
        - _tabs_closable()
        - _tab_stylesheet()
        - _additional_setup()
        - _create_status_bars() (if custom status bars needed)
    """
    
    def __init__(self):
        """
        Initialize BaseGUI.
        
        Subclasses should call super().__init__() first in their __init__.
        """
        super().__init__()
    
    def initUI(self):
        """
        Initialize the user interface.
        
        This is the main template method that defines the initialization sequence.
        Subclasses should NOT override this method, but instead override the
        individual step methods below.
        """
        # 1. Window setup
        self._setup_window()
        
        # 2. Main layout (scroll area + tab widget)
        self._setup_main_layout()
        
        # 3. Create tabs (abstract - subclass defines what tabs to create)
        self._create_tabs()
        
        # 4. Status bars
        self._create_status_bars()
        
        # 5. Menu bar
        self._create_menu_bar()
        
        # 6. Additional setup (hook for subclasses)
        self._additional_setup()
        
        # 7. Finalize
        self._finalize_ui()
    
    # ===== Template steps with default implementations =====
    
    def _setup_window(self):
        """
        Step 1: Set window title and basic properties.
        
        Subclasses must implement this to set their specific window title.
        
        Example:
            def _setup_window(self):
                from musclex import __version__
                self.setWindowTitle(f"My Application v.{__version__}")
        """
        raise NotImplementedError("Subclasses must implement _setup_window()")
    
    def _setup_main_layout(self):
        """
        Step 2: Create the main scroll area and tab widget structure.
        
        This creates a standard layout with:
        - QScrollArea as central widget
        - QTabWidget for organizing content in tabs
        
        Subclasses can override if they need a different main layout,
        but most applications should use this default.
        """
        # Create scroll area
        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        
        # Create central widget
        self.centralWidget = QWidget(self)
        self.scrollArea.setWidget(self.centralWidget)
        self.mainVLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.scrollArea)
        
        # Create tab widget
        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(self._tabs_closable())
        self.tabWidget.setStyleSheet(self._tab_stylesheet())
        
        # Add tab widget to main layout
        self.mainVLayout.addWidget(self.tabWidget)
    
    def _create_tabs(self):
        """
        Step 3: Create all tabs for this GUI.
        
        Subclasses must implement this to create their specific tabs.
        Each GUI creates different tabs and uses appropriate components:
        - Use ProcessingWorkspace for image processing modules (QF, PT, EQ)
        - Use ImageNavigatorWidget for image viewing modules (XV)
        
        Example with ProcessingWorkspace:
            def _create_tabs(self):
                self.imageTab = QWidget()
                self.imageTabLayout = QHBoxLayout(self.imageTab)
                self.tabWidget.addTab(self.imageTab, "Image")
                
                self.workspace = ProcessingWorkspace(settings_dir=self.filePath)
                self.imageTabLayout.addWidget(self.workspace, 1)
                
                # Add custom settings
                self.workspace.right_panel.add_widget(my_settings)
        
        Example with ImageNavigatorWidget:
            def _create_tabs(self):
                self.imageTab = QWidget()
                self.imageTabLayout = QVBoxLayout(self.imageTab)
                self.tabWidget.addTab(self.imageTab, "Image")
                
                self.navigator = ImageNavigatorWidget(auto_display=True)
                self.imageTabLayout.addWidget(self.navigator, 1)
        """
        raise NotImplementedError("Subclasses must implement _create_tabs()")
    
    def _create_status_bars(self):
        """
        Step 4: Create status bars.
        
        Creates standard status bars with:
        - Main status bar (top) with progress bar and status info
        - Lower status bar (bottom) for additional information
        
        Subclasses can override if they need custom status bars.
        """
        # Main status bar
        self.statusBar = QStatusBar()
        
        # Progress bar
        self.progressBar = QProgressBar()
        self.progressBar.setMaximum(100)
        self.progressBar.setMinimum(0)
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        
        # Status labels
        self.statusReport = QLabel()
        self.imgDetailOnStatusBar = QLabel()
        self.imgCoordOnStatusBar = QLabel()
        self.imgPathOnStatusBar = QLabel()
        
        # Add widgets to status bar
        self.statusBar.addPermanentWidget(self.statusReport)
        self.statusBar.addPermanentWidget(self.imgCoordOnStatusBar)
        self.statusBar.addPermanentWidget(self.imgDetailOnStatusBar)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.statusBar.addWidget(QLabel("    "))
        self.statusBar.addWidget(self.imgPathOnStatusBar)
        
        # Lower status bar
        self.lowerStatusBar = QStatusBar()
        self.left_status = QLabel()
        self.lowerStatusBar.addWidget(self.left_status)
        
        # Add status bars to main layout
        self.mainVLayout.addWidget(self.statusBar)
        self.mainVLayout.addWidget(self.lowerStatusBar)
    
    def _create_menu_bar(self):
        """
        Step 5: Create menu bar.
        
        Subclasses must implement this to create their specific menu structure.
        Each GUI has different menu items.
        
        Example:
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
        """
        raise NotImplementedError("Subclasses must implement _create_menu_bar()")
    
    def _finalize_ui(self):
        """
        Step 7: Final UI setup.
        
        Sets default window size and shows the window.
        Subclasses can override to change default size or add final touches.
        """
        self.resize(1200, 900)
        self.show()
        
        # Position floating widgets after window is fully rendered
        # Use QTimer to ensure geometry is calculated correctly
        QTimer.singleShot(0, self._position_floating_widgets)
    
    # ===== Hook methods - subclasses CAN override =====
    
    def _tabs_closable(self) -> bool:
        """
        Hook: Are tabs closable?
        
        Returns:
            bool: True if tabs should have close buttons, False otherwise.
                  Default is False.
        
        Override this in subclasses if tabs should be closable:
            def _tabs_closable(self):
                return True
        """
        return False
    
    def _tab_stylesheet(self) -> str:
        """
        Hook: Tab widget stylesheet.
        
        Returns:
            str: CSS stylesheet for tab widget.
                 Default sets tab size to 40px height, 200px width.
        
        Override this in subclasses for different tab styling:
            def _tab_stylesheet(self):
                return "QTabBar::tab { height: 20px; width: 150px; }"
        """
        return "QTabBar::tab { height: 40px; width: 200px; }"
    
    def _additional_setup(self):
        """
        Step 6: Hook for additional setup.
        
        Override this in subclasses that need extra initialization after
        the main UI is created but before finalization.
        
        Examples:
            - Register tools
            - Initialize patches
            - Set up connections
            - Load settings
        
        Default implementation connects to workspace/navigator scan signals
        if they exist.
        
        Example:
            def _additional_setup(self):
                super()._additional_setup()  # Keep default connections
                self._initialize_patches()
                self._register_tools()
        """
        # Auto-connect to workspace/navigator scan signals if available
        self._setup_scan_monitoring()
    
    def _position_floating_widgets(self):
        """
        Hook: Position floating widgets (like toggle buttons) after window is rendered.
        
        This is called via QTimer.singleShot(0) after show() to ensure
        widget geometry is calculated correctly.
        
        Override this in subclasses that have floating widgets:
            def _position_floating_widgets(self):
                self._position_toggle_button()
        
        Default implementation does nothing.
        """
        pass
    
    # ===== Background Scan Monitoring =====
    
    def _setup_scan_monitoring(self):
        """
        Setup automatic monitoring of background directory scans.
        
        If the GUI uses ProcessingWorkspace or ImageNavigatorWidget,
        automatically connects to their scan signals to display progress
        in the status bar.
        
        This provides default handling that most GUIs want:
        - Show progress bar during HDF5 file scanning
        - Hide progress bar when scan completes
        - Update status bar when scan completes
        """
        # Check if workspace exists (ProcessingWorkspace)
        if hasattr(self, 'workspace') and hasattr(self.workspace, 'navigator'):
            navigator = self.workspace.navigator
            navigator.scanProgressChanged.connect(self._on_scan_progress)
            navigator.scanComplete.connect(self._on_scan_complete)
        # Check if navigator exists directly (for pure viewer GUIs)
        elif hasattr(self, 'navigator'):
            self.navigator.scanProgressChanged.connect(self._on_scan_progress)
            self.navigator.scanComplete.connect(self._on_scan_complete)
    
    def _on_scan_progress(self, h5_done: int, h5_total: int):
        """
        Handle scan progress update.
        
        Default implementation shows a progress bar with HDF5 file count.
        Override this if you need custom progress display.
        
        Args:
            h5_done: Number of HDF5 files processed
            h5_total: Total number of HDF5 files to process
        """
        if not hasattr(self, 'progressBar'):
            return
        
        if not self.progressBar.isVisible():
            self.progressBar.setVisible(True)
        
        self.progressBar.setRange(0, h5_total)
        self.progressBar.setValue(h5_done)
        self.progressBar.setFormat(f"Processing HDF5 files: {h5_done}/{h5_total}")
    
    def _on_scan_complete(self):
        """
        Handle scan completion.
        
        Default implementation hides progress bar and updates status bar.
        Override this if you need custom completion handling.
        
        Subclasses can call super()._on_scan_complete() and add their own logic.
        """
        # Hide progress bar
        if hasattr(self, 'progressBar'):
            self.progressBar.setVisible(False)
            self.progressBar.setFormat("%p%")  # Reset to default format
        
        # Update status bar if method exists
        if hasattr(self, 'resetStatusbar'):
            self.resetStatusbar()
