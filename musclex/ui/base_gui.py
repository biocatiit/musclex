"""
Base GUI class for MuscleX image applications.

Provides common initialization flow and UI components for all MuscleX GUI applications.
Uses Template Method pattern to allow subclasses to customize specific steps.

Copyright 1999 Illinois Institute of Technology
"""

from PySide6.QtWidgets import (
    QMainWindow, QScrollArea, QWidget, QVBoxLayout, QHBoxLayout, QTabWidget,
    QStatusBar, QProgressBar, QLabel, QPushButton
)
from PySide6.QtGui import QAction
from PySide6.QtCore import Qt, QTimer


class BaseGUI(QMainWindow):
    """
    Base class for all MuscleX GUI applications.
    
    This class defines the standard initialization sequence for GUI applications
    that work with images. Subclasses override specific methods to customize
    behavior while maintaining a consistent structure.
    
    Initialization Flow:
        0. __init__()               - Initialize FileManager and core components
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
        - _on_file_manager_changed() - Handle image changes from navigation
    
    Subclasses may override:
        - _tabs_closable()
        - _tab_stylesheet()
        - _additional_setup()
        - _create_status_bars() (if custom status bars needed)
    """
    
    def __init__(self):
        """
        Initialize BaseGUI with core components.
        
        Initializes FileManager early so it's available for:
        - ImageSettingsPanel (needs file_manager for batch operations)
        - Navigation controls
        - Image loading
        
        Subclasses should call super().__init__() first in their __init__.
        """
        super().__init__()
        
        # Initialize FileManager early (before UI creation)
        from ..utils.file_manager import FileManager
        self.file_manager = FileManager()
        
        # Background directory scan support
        self._scan_timer = QTimer(self)
        self._scan_timer.setInterval(250)
        self._scan_timer.timeout.connect(self._check_scan_done)
        self._provisionalCount = False
    
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
        
        Default implementation does nothing.
        
        Example:
            def _additional_setup(self):
                self._initialize_patches()
                self._register_tools()
        """
        pass
    
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
    
    # ===== Standard Image Tab Creation =====
    
    def _create_standard_image_tab(
        self,
        tab_title: str = "Image",
        show_left_select_buttons: bool = True,
        show_display_panel: bool = True,
        show_double_zoom: bool = True
    ):
        """
        Create a standardized image tab with the QuadrantFolding layout.
        
        This is the standard layout for all MuscleX GUIs:
        [Left: Select Buttons] [Center: ImageViewerWidget] [Right: CollapsibleRightPanel]
        
        After calling this, GUIs can:
        - Add custom display options: self.image_viewer.display_panel.add_to_top_slot(widget)
        - Add settings to right panel: self.right_panel.add_widget(widget)
        - Add bottom widgets: self.right_panel.add_bottom_widget(widget)
        - Replace left panel if needed (e.g., ProjectionTraces)
        
        Args:
            tab_title: Title for the tab (default: "Image")
            show_left_select_buttons: Show standard left panel with select buttons
            show_display_panel: Show ImageViewerWidget's display panel
            show_double_zoom: Show double zoom feature
        
        Sets up these attributes:
            - self.imageTab
            - self.imageTabLayout
            - self.image_viewer
            - self.imageAxes, self.imageCanvas, self.imageFigure (backward compat)
            - self.right_panel
            - self.navControls
            - self.spminInt, self.spmaxInt, etc. (if show_display_panel=True)
        
        Example:
            def _create_tabs(self):
                # Create standard image tab
                self._create_standard_image_tab(tab_title="Original Image")
                
                # Add GUI-specific settings
                self.right_panel.add_widget(self.my_settings_widget)
                self.right_panel.add_bottom_widget(self.navControls)
        """
        from .widgets.image_viewer_widget import ImageViewerWidget
        from .widgets.collapsible_right_panel import CollapsibleRightPanel
        from .widgets.navigation_controls import NavigationControls
        from PySide6.QtCore import Qt
        
        # Create tab
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.tabWidget.addTab(self.imageTab, tab_title)
        
        # ===== Left Panel: Select Buttons =====
        if show_left_select_buttons:
            self._create_standard_left_panel()
        
        # ===== Center: ImageViewerWidget =====
        self.image_viewer = ImageViewerWidget(
            parent=self,
            show_display_panel=show_display_panel,
            show_double_zoom=show_double_zoom
        )
        
        # Backward compatibility: expose axes, canvas, figure
        self.imageAxes = self.image_viewer.axes
        self.imageCanvas = self.image_viewer.canvas
        self.imageFigure = self.image_viewer.figure
        self.imageCanvas.setHidden(True)
        
        # Expose display panel controls (if display panel is shown)
        if show_display_panel:
            self.spminInt = self.image_viewer.display_panel.minIntSpnBx
            self.spmaxInt = self.image_viewer.display_panel.maxIntSpnBx
            self.logScaleIntChkBx = self.image_viewer.display_panel.logScaleChkBx
            self.persistIntensity = self.image_viewer.display_panel.persistChkBx
            self.imgZoomInB = self.image_viewer.display_panel.zoomInBtn
            self.imgZoomOutB = self.image_viewer.display_panel.zoomOutBtn
            self.minIntLabel = self.image_viewer.display_panel.minIntLabel
            self.maxIntLabel = self.image_viewer.display_panel.maxIntLabel
        
        self.imageTabLayout.addWidget(self.image_viewer, 1)  # Stretch to fill
        
        # ===== Right Panel: CollapsibleRightPanel =====
        self.right_panel = CollapsibleRightPanel(
            parent=self,
            title="Options",
            settings_key=f"{self.__class__.__name__.lower()}/right_panel",
            start_visible=True,
            show_toggle_internally=False
        )
        self.right_panel.setFixedWidth(500)
        self.right_panel.setContentsMargins(0, 35, 0, 0)
        
        # Add display panel to right panel (if display panel is shown)
        if show_display_panel:
            self.right_panel.add_widget(self.image_viewer.display_panel)
        
        self.imageTabLayout.addWidget(self.right_panel, 0)  # No stretch
        
        # Setup floating toggle button
        self.right_panel.toggle_btn.setParent(self.imageTab)
        self.right_panel.toggle_btn.raise_()
        self.right_panel.toggle_btn.show()
        
        # Create navigation controls (GUIs will add this to right_panel bottom)
        self.navControls = NavigationControls(
            process_folder_text="Process Current Folder",
            process_h5_text="Process Current H5 File"
        )
    
    def _create_standard_left_panel(self):
        """
        Create standard left panel with select buttons.
        
        This is the default left panel used by most GUIs.
        Some GUIs (like ProjectionTraces) may replace this with custom content.
        """
        from PySide6.QtCore import Qt
        
        self.verImgLayout = QVBoxLayout()
        self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)
        
        self.leftWidget = QWidget()
        self.leftWidget.setLayout(self.verImgLayout)
        
        self.selectImageButton = QPushButton('Click Here to Select an Image...')
        self.selectImageButton.setFixedHeight(100)
        self.selectImageButton.setFixedWidth(300)
        
        self.selectFolder = QPushButton('Click Here to Select a Folder...')
        self.selectFolder.setFixedHeight(100)
        self.selectFolder.setFixedWidth(300)
        
        self.bgWd = QWidget()  # Background widget (for compatibility)
        
        self.verImgLayout.addWidget(self.selectImageButton)
        self.verImgLayout.addWidget(self.selectFolder)
        
        self.imageTabLayout.addWidget(self.leftWidget, 0)  # No stretch
    
    # ===== FileManager Integration =====
    
    def _connect_standard_navigation(self):
        """
        Connect NavigationControls to FileManager for standard navigation behavior.
        
        This provides automatic navigation through images using FileManager.
        Call this method after creating self.navControls in your _create_tabs().
        
        Connected buttons:
            - prevButton/nextButton: Navigate frames within current file
            - prevFileButton/nextFileButton: Navigate between files (H5)
        
        Example usage in subclass:
            def _create_tabs(self):
                self._create_standard_image_tab()
                # ... add widgets to right_panel ...
                self.right_panel.add_bottom_widget(self.navControls)
                self._connect_standard_navigation()  # Connect after creating navControls
        """
        if not hasattr(self, 'navControls'):
            print("Warning: navControls not found, cannot connect navigation")
            return
        
        # Frame navigation
        self.navControls.prevButton.clicked.connect(self._navigate_prev)
        self.navControls.nextButton.clicked.connect(self._navigate_next)
        
        # File navigation (H5)
        self.navControls.prevFileButton.clicked.connect(self._navigate_prev_file)
        self.navControls.nextFileButton.clicked.connect(self._navigate_next_file)
    
    def _navigate_next(self):
        """Navigate to next image/frame using FileManager."""
        if self.file_manager.next():
            self._on_file_manager_changed()
    
    def _navigate_prev(self):
        """Navigate to previous image/frame using FileManager."""
        if self.file_manager.prev():
            self._on_file_manager_changed()
    
    def _navigate_next_file(self):
        """Navigate to next file (H5 navigation)."""
        if self.file_manager.next_file():
            self._on_file_manager_changed()
    
    def _navigate_prev_file(self):
        """Navigate to previous file (H5 navigation)."""
        if self.file_manager.prev_file():
            self._on_file_manager_changed()
    
    def _on_file_manager_changed(self):
        """
        Hook method called when FileManager navigates to a new image.
        
        This is called after:
        - next() / prev() navigation
        - next_file() / prev_file() navigation
        - set_from_file() (initial load)
        
        Subclasses MUST implement this to:
        1. Load the current image from file_manager.current_image
        2. Get filename from file_manager.current_image_name (or names[current])
        3. Create ImageData object
        4. Process the image
        5. Update UI
        
        Example implementation:
            def _on_file_manager_changed(self):
                # 1. Get image from FileManager
                img = self.file_manager.current_image
                filename = self.file_manager.current_image_name
                
                # 2. Create ImageData
                self.current_image_data = self._create_image_data(
                    img, self.file_manager.dir_path, filename
                )
                
                # 3. Process
                self.processImage()
                
                # 4. Update UI
                self.image_settings_panel.update_display(self.current_image_data)
                self.navControls.setNavMode(self.file_manager.current_file_type)
        
        Raises:
            NotImplementedError: If subclass doesn't implement this method
        """
        raise NotImplementedError(
            f"{self.__class__.__name__} must implement _on_file_manager_changed() "
            "to handle image loading and processing when FileManager navigates to a new image."
        )
    
    def browseFile(self):
        """
        Standard file browsing using FileManager.
        
        Opens a file dialog and loads the selected file into FileManager.
        Subclasses can override this if they need custom file selection behavior,
        but most should use this default implementation.
        """
        from .pyqt_utils import getAFile
        
        file_name = getAFile()
        if file_name and file_name != "":
            self.file_manager.set_from_file(file_name)
            self._on_file_manager_changed()
    
    # ===== Background Directory Scan Support =====
    
    def _start_background_scan(self):
        """
        Start background scan timer to monitor FileManager's async directory scan.
        
        FileManager scans directories asynchronously to avoid blocking the UI,
        especially when processing HDF5 files which need to be opened to count frames.
        
        This timer periodically checks if the scan is complete and updates the UI.
        Call this after file_manager.set_from_file() if you want to show progress.
        """
        self._provisionalCount = True
        self._scan_timer.start()
    
    def _check_scan_done(self):
        """
        Check if background directory scan is complete.
        
        This is called periodically by the scan timer. When the scan completes:
        1. Updates the image layer with full HDF5 frame expansion
        2. Hides the progress bar
        3. Calls _on_scan_complete() hook for subclass-specific updates
        """
        if not self.file_manager:
            return
        
        # Show HDF5 processing progress (if applicable)
        h5_done, h5_total = self.file_manager.get_h5_progress()
        if h5_total > 0 and hasattr(self, 'progressBar'):
            if not self.progressBar.isVisible():
                self.progressBar.setVisible(True)
                self.progressBar.setRange(0, h5_total)
            self.progressBar.setValue(h5_done)
            self.progressBar.setFormat(f"Processing HDF5 files: {h5_done}/{h5_total}")
        
        # Check if scan is complete
        if not self.file_manager.is_scan_done():
            return
        
        # Scan complete - hide progress bar
        if hasattr(self, 'progressBar'):
            self.progressBar.setVisible(False)
            self.progressBar.setFormat("%p%")  # Reset format to default
        
        self._provisionalCount = False
        self._scan_timer.stop()
        
        # Call hook for subclass-specific updates
        self._on_scan_complete()
    
    def _on_scan_complete(self):
        """
        Hook method called when background directory scan completes.
        
        Subclasses can override this to perform actions after scan completion:
        - Update status bar with final count
        - Update mode statistics in ImageSettingsPanel
        - Refresh UI elements that depend on full file list
        
        Default implementation does nothing.
        
        Example:
            def _on_scan_complete(self):
                self.resetStatusbar()
                if hasattr(self, 'image_settings_panel'):
                    self.image_settings_panel.update_mode_statistics(
                        len(self.file_manager.names)
                    )
        """
        pass

