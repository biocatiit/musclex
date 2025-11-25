"""
Base GUI class for MuscleX image applications.

Provides common initialization flow and UI components for all MuscleX GUI applications.
Uses Template Method pattern to allow subclasses to customize specific steps.

Copyright 1999 Illinois Institute of Technology
"""

from PySide6.QtWidgets import (
    QMainWindow, QScrollArea, QWidget, QVBoxLayout, QTabWidget,
    QStatusBar, QProgressBar, QLabel
)
from PySide6.QtGui import QAction
from PySide6.QtCore import Qt


class BaseGUI(QMainWindow):
    """
    Base class for all MuscleX GUI applications.
    
    This class defines the standard initialization sequence for GUI applications
    that work with images. Subclasses override specific methods to customize
    behavior while maintaining a consistent structure.
    
    Initialization Flow:
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
        Each GUI creates different tabs:
        - QuadrantFolding: image tab + result tab
        - ProjectionTraces: image tab (+ dynamic box tabs)
        - XRayViewer: image tab + graph tab
        
        Example:
            def _create_tabs(self):
                self._create_image_tab()
                self._create_result_tab()
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
        
        Default implementation does nothing.
        
        Example:
            def _additional_setup(self):
                self._initialize_patches()
                self._register_tools()
        """
        pass

