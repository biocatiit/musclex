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

from PySide6.QtWidgets import QWidget, QVBoxLayout, QHBoxLayout
from PySide6.QtCore import Signal, QTimer

from musclex.ui.widgets.collapsible_right_panel import CollapsibleRightPanel

from .image_viewer_widget import ImageViewerWidget
from .navigation_controls import NavigationControls
from ...utils.file_manager import FileManager


class ImageNavigatorWidget(QWidget):
    """
    Composite widget combining image viewing with file navigation.
    
    This widget integrates:
    - ImageViewerWidget: Image display with tools and interactions
    - FileManager: File/folder management and image loading
    - NavigationControls: UI controls for navigation
    - CollapsibleRightPanel: Settings panel with display options and navigation
    
    The widget handles all internal connections between these components,
    providing a unified interface for image browsing and navigation.
    
    Use Cases:
    - XRayViewer: Pure image viewer with navigation (no processing)
    - ImageProcessorPanel: Will embed this for PT/QF/EQ (with processing)
    
    Signals:
        imageChanged(img, filename, dir_path): New image loaded from FileManager
        navigationError(str): Error during navigation (e.g., failed to load image)
        fileManagerReady(): FileManager has loaded a directory
        scanComplete(): Background directory scan complete (HDF5 expansion done)
        scanProgressChanged(done, total): HDF5 file scan progress
    
    Public Interface:
        load_from_file(filepath): Load a file/folder into FileManager
        navigate_next(): Navigate to next frame/image
        navigate_prev(): Navigate to previous frame/image
        navigate_next_file(): Navigate to next file (H5)
        navigate_prev_file(): Navigate to previous file (H5)
        get_current_image(): Get currently displayed image array
        
    Public Attributes (for advanced use):
        image_viewer: ImageViewerWidget instance
        file_manager: FileManager instance
        nav_controls: NavigationControls instance
        right_panel: CollapsibleRightPanel instance
    
    Example:
        # Basic usage
        navigator = ImageNavigatorWidget()
        navigator.imageChanged.connect(my_process_function)
        navigator.load_from_file("/path/to/image.tif")
        
        # With display panel visible in right panel
        navigator = ImageNavigatorWidget(
            show_display_panel=True,
            show_double_zoom=True
        )
        
        # Advanced: Access internal components
        navigator.image_viewer.tool_manager.activate_tool('zoom')
        total_files = len(navigator.file_manager.names)
        navigator.right_panel.add_widget(my_custom_widget)
    """
    
    # Signals
    fileLoaded = Signal(str)  # (dir_path) - New file/folder loaded (BEFORE first image loads)
    fileManagerReady = Signal()  # FileManager initialized with directory
    imageChanged = Signal(object, str, str)  # (image_array, filename, dir_path)
    navigationError = Signal(str)  # Error message
    scanComplete = Signal()  # Background directory scan complete
    scanProgressChanged = Signal(int, int)  # (h5_done, h5_total) - HDF5 scan progress
    filePathTextReady = Signal(str)  # Formatted file path status text
    
    def __init__(
        self, 
        parent=None,
        show_display_panel=False,
        show_double_zoom=False,
        auto_display=True,
        navigation_process_folder_text="Process Current Folder",
        navigation_process_h5_text="Process Current H5 File"
    ):
        """
        Initialize ImageNavigatorWidget.
        
        Args:
            parent: Parent widget
            show_display_panel: Show DisplayOptionsPanel in ImageViewerWidget
            show_double_zoom: Show DoubleZoom in display panel
            auto_display: If True, automatically display images when loaded.
                         If False, only emit imageChanged signal without displaying.
                         Set to False for processor modules that need to process
                         images before displaying. Set to True for simple viewers.
            navigation_process_folder_text: Text for process folder button
            navigation_process_h5_text: Text for process H5 button
        
        Note:
            NavigationControls (self.nav_controls) is automatically added to 
            the right_panel bottom area. Display panel (if exists) is added
            to right_panel top area.
        """
        super().__init__(parent)
        
        # Display control
        self._auto_display = auto_display
        
        # Create components
        self.image_viewer = ImageViewerWidget(
            parent=self,
            show_display_panel=show_display_panel,
            show_double_zoom=show_double_zoom
        )
        
        self.file_manager = FileManager()
        
        # Always create nav_controls, but don't add to layout (GUI controls position)
        self.nav_controls = NavigationControls(
            process_folder_text=navigation_process_folder_text,
            process_h5_text=navigation_process_h5_text,
            parent=self
        )
        
        # Create collapsible right panel
        self.right_panel = CollapsibleRightPanel(
            parent=self,
            settings_key="image_navigator/right_panel",
            start_visible=True,
            show_toggle_internally=False
        )
        self.right_panel.setFixedWidth(500)
        
        # Add display panel to right panel if it exists
        if self.image_viewer.display_panel:
            self.right_panel.add_widget(self.image_viewer.display_panel)
        
        # Add navigation controls to right panel bottom
        self.right_panel.add_bottom_widget(self.nav_controls)

        # Setup floating toggle button for right panel
        self.right_panel.toggle_btn.setParent(self)
        self.right_panel.toggle_btn.raise_()
        self.right_panel.toggle_btn.show()

        # Create select buttons panel if requested
        self.select_panel = None
        self.select_image_btn = None
        self.select_folder_btn = None

        self._create_select_panel()
        
        # Background scan support
        self._scan_timer = QTimer(self)
        self._scan_timer.setInterval(250)
        self._scan_timer.timeout.connect(self._check_scan_progress)
        
        # Setup UI
        self._setup_ui()
        
        # Connect internal signals
        self._connect_signals()
    
    def _create_select_panel(self):
        """Create left panel with select image/folder buttons."""
        from PySide6.QtWidgets import QPushButton
        from PySide6.QtCore import Qt
        
        self.select_panel = QWidget()
        select_layout = QVBoxLayout(self.select_panel)
        select_layout.setContentsMargins(0, 0, 0, 0)
        select_layout.setAlignment(Qt.AlignCenter)
        
        self.select_image_btn = QPushButton('Click Here to Select an Image...')
        self.select_image_btn.setFixedHeight(100)
        self.select_image_btn.setFixedWidth(300)
        self.select_image_btn.clicked.connect(self.browse_file)
        
        self.select_folder_btn = QPushButton('Click Here to Select a Folder...')
        self.select_folder_btn.setFixedHeight(100)
        self.select_folder_btn.setFixedWidth(300)
        # NOTE: browse_folder() removed - button kept for future implementation
        
        select_layout.addWidget(self.select_image_btn)
        select_layout.addWidget(self.select_folder_btn)
        
        # Initially hide image viewer, show buttons
        self.image_viewer.setHidden(True)
    
    def _setup_ui(self):
        """
        Setup the widget layout.
        
        Layout: [select_panel (optional)] - [image_viewer] - [right_panel]
        
        Note: NavigationControls are added to right_panel bottom by default.
        """
        layout = QHBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        
        # Add select panel if it exists
        if self.select_panel:
            layout.addWidget(self.select_panel, 0)
        
        # Add image viewer (takes all space)
        layout.addWidget(self.image_viewer, 1)
        
        # Add right panel
        layout.addWidget(self.right_panel, 0)
    
    def _connect_signals(self):
        """Connect internal component signals."""
        # Navigation button connections
        self.nav_controls.nextButton.clicked.connect(self.navigate_next)
        self.nav_controls.prevButton.clicked.connect(self.navigate_prev)
        self.nav_controls.nextFileButton.clicked.connect(self.navigate_next_file)
        self.nav_controls.prevFileButton.clicked.connect(self.navigate_prev_file)
        self.nav_controls.filenameLineEdit.editingFinished.connect(self._on_filename_changed)
    
    # ===== Public API =====
    
    def load_from_file(self, filepath: str, start_background_scan: bool = True):
        """
        Load a file or directory into the FileManager.
        
        Args:
            filepath: Full path to image file or directory
            start_background_scan: Whether to start background scan for HDF5 expansion
        
        Emits:
            fileLoaded: Immediately after folder/file is loaded (before image loads)
            fileManagerReady: After initial load
            imageChanged: After first image is loaded
        """
        try:
            # Load file into FileManager
            self.file_manager.set_from_file(filepath)
            
            # Update navigation mode
            self.nav_controls.setNavMode(self.file_manager.current_file_type)
            
            # Hide select buttons, show image viewer
            if self.select_panel:
                self.select_panel.setHidden(True)
                self.image_viewer.setHidden(False)
            
            # Emit fileLoaded signal FIRST (for folder-level initialization like csvManager)
            # This happens BEFORE loading the first image
            if self.file_manager.dir_path:
                self.fileLoaded.emit(self.file_manager.dir_path)
            
            # Emit ready signal
            self.fileManagerReady.emit()
            
            # Load and display current image
            self._load_current_image()
            
            # Start background scan if requested
            if start_background_scan and self.file_manager.dir_path:
                self.file_manager.start_async_scan(self.file_manager.dir_path)
                self._scan_timer.start()
                
        except Exception as e:
            error_msg = f"Failed to load file: {str(e)}"
            self.navigationError.emit(error_msg)
            print(f"ImageNavigatorWidget: {error_msg}")
    
    def browse_file(self):
        """
        Open file dialog and load selected file.
        
        Opens a file selection dialog for the user to choose an image file.
        Supports TIF, TIFF, H5, HDF5, and other common image formats.
        
        Emits:
            fileManagerReady: After file is loaded into FileManager
            imageChanged: After image is displayed
            navigationError: If user cancels or error occurs
        
        Returns:
            bool: True if file was loaded, False if cancelled or error
        """
        from PySide6.QtWidgets import QFileDialog
        
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "Select Image File",
            "",
            "Image Files (*.tif *.tiff *.h5 *.hdf5 *.png *.jpg *.jpeg);;All Files (*)"
        )
        
        if file_path:
            self.load_from_file(file_path)
            return True
        else:
            self.navigationError.emit("File selection cancelled")
            return False
    
    # NOTE: browse_folder() removed - not currently used and needs proper implementation
    # When needed, should handle folder paths differently from file paths in load_from_file()
    
    def navigate_next(self):
        """Navigate to next image/frame."""
        self.file_manager.next_frame()
        self._load_current_image()
    
    def navigate_prev(self):
        """Navigate to previous image/frame."""
        self.file_manager.prev_frame()
        self._load_current_image()
    
    def navigate_next_file(self):
        """Navigate to next file (for H5 navigation)."""
        self.file_manager.next_file()
        self._load_current_image()
    
    def navigate_prev_file(self):
        """Navigate to previous file (for H5 navigation)."""
        self.file_manager.prev_file()
        self._load_current_image()
    
    def switch_to_image_by_name(self, filename: str):
        """
        Switch to a specific image by filename.
        
        Args:
            filename: Name of the file to switch to (without directory path)
        
        Returns:
            bool: True if successful, False if file not found
        """
        if filename not in self.file_manager.names:
            return False
        
        self.file_manager.switch_image_by_name(filename)
        self._load_current_image()
        return True
    
    def switch_to_image_by_index(self, index: int):
        """
        Switch to a specific image by index.
        
        Args:
            index: Index in file_manager.names
        
        Returns:
            bool: True if successful, False if index out of range
        """
        if index < 0 or index >= len(self.file_manager.names):
            return False
        
        self.file_manager.switch_image_by_index(index)
        self._load_current_image()
        return True
    
    def get_current_image(self):
        """
        Get the currently displayed image array.
        
        Returns:
            numpy.ndarray or None: Current image array
        """
        return self.image_viewer.get_current_image_data()
    
    def refresh_current_image(self):
        """
        Reload and display the current image from FileManager.
        
        Useful when external code modifies file_manager state.
        """
        self._load_current_image()
    
    def set_auto_display(self, enabled: bool):
        """
        Enable or disable automatic image display.
        
        Args:
            enabled: If True, images are automatically displayed when loaded.
                    If False, only imageChanged signal is emitted.
        
        Example:
            # Start with auto display off (for processor)
            navigator = ImageNavigatorWidget(auto_display=False)
            
            # Later enable it for quick preview
            navigator.set_auto_display(True)
        """
        self._auto_display = enabled
    
    def get_auto_display(self) -> bool:
        """
        Get current auto_display setting.
        
        Returns:
            bool: True if auto display is enabled, False otherwise
        """
        return self._auto_display
    
    # ===== Internal Methods =====
    
    def _load_current_image(self):
        """
        Load current image from FileManager and optionally display it.
        
        Internal method that handles the image loading and display logic.
        
        Behavior:
        - If auto_display=True: Automatically displays the image in image_viewer
        - If auto_display=False: Only emits signal, allows external code to control display
        
        Always emits imageChanged signal regardless of auto_display setting.
        """
        try:
            # Get image from FileManager
            img = self.file_manager.current_image
            filename = self.file_manager.current_image_name
            dir_path = self.file_manager.dir_path
            
            if img is None:
                self.navigationError.emit(f"Failed to load image: {filename}")
                return
            
            # Display image only if auto_display is enabled
            if self._auto_display:
                self.image_viewer.display_image(img)
            
            # Update navigation controls
            self.nav_controls.filenameLineEdit.setText(filename)
            self.nav_controls.setNavMode(self.file_manager.current_file_type)
            
            # Emit signal for external processing (always)
            self.imageChanged.emit(img, filename, dir_path)
            
            # Emit formatted file path text for statusBar
            self._emit_file_path_text(filename, dir_path)
            
        except Exception as e:
            error_msg = f"Error loading image: {str(e)}"
            self.navigationError.emit(error_msg)
            print(f"ImageNavigatorWidget: {error_msg}")
    
    def _on_filename_changed(self):
        """Handle filename line edit changes (user typed a filename)."""
        filename = self.nav_controls.filenameLineEdit.text().strip()
        if filename and filename in self.file_manager.names:
            self.switch_to_image_by_name(filename)
        else:
            # Reset to current filename if invalid
            self.nav_controls.filenameLineEdit.setText(
                self.file_manager.current_image_name
            )
    
    def _emit_file_path_text(self, filename: str, dir_path: str):
        """
        Prepare formatted file path text and emit signal.
        
        Args:
            filename: Current filename
            dir_path: Directory path
        
        Emits:
            filePathTextReady: Formatted text for statusBar display
        """
        from os.path import join
        
        current_idx = self.file_manager.current + 1
        total = len(self.file_manager.names)
        
        # Add '*' if still scanning HDF5 files
        scanning = not self.file_manager.is_scan_done()
        total_str = f"{total}{'*' if scanning else ''}"
        
        # Full path
        full_path = join(dir_path, filename)
        
        # Format text
        text = f"Current File ({current_idx}/{total_str}) : {full_path}"
        
        # Emit signal
        self.filePathTextReady.emit(text)
    
    def _check_scan_progress(self):
        """
        Check background directory scan progress.
        
        Called periodically by timer. Emits scanProgressChanged for HDF5 progress
        and scanComplete when done.
        """
        if not self.file_manager:
            return
        
        # Emit HDF5 processing progress
        h5_done, h5_total = self.file_manager.get_h5_progress()
        if h5_total > 0:
            self.scanProgressChanged.emit(h5_done, h5_total)
        
        # Check if scan is complete
        if self.file_manager.is_scan_done():
            self._scan_timer.stop()
            self.scanComplete.emit()
            
            # Update file path text to remove scanning indicator (*)
            if self.file_manager.current_image_name and self.file_manager.dir_path:
                self._emit_file_path_text(
                    self.file_manager.current_image_name,
                    self.file_manager.dir_path
                )
    
    # ===== Property Accessors (for convenience) =====
    
    @property
    def current_filename(self) -> str:
        """Get current filename (convenience accessor)."""
        return getattr(self.file_manager, 'current_image_name', '')
    
    @property
    def current_dir_path(self) -> str:
        """Get current directory path (convenience accessor)."""
        return getattr(self.file_manager, 'dir_path', '')
    
    @property
    def current_index(self) -> int:
        """Get current image index (convenience accessor)."""
        return getattr(self.file_manager, 'current', 0)
    
    @property
    def total_images(self) -> int:
        """Get total number of images (convenience accessor)."""
        names = getattr(self.file_manager, 'names', None)
        return len(names) if names else 0
    
    @property
    def is_h5_mode(self) -> bool:
        """Check if current file is H5 (convenience accessor)."""
        file_type = getattr(self.file_manager, 'current_file_type', None)
        return file_type == 'h5'
