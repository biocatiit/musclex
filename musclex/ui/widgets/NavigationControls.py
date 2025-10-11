from PySide6.QtWidgets import QWidget, QPushButton, QGridLayout, QLineEdit


class NavigationControls(QWidget):
    """
    Reusable navigation/control widget used across multiple GUIs.

    Provides:
    - processFolderButton: main action (e.g., Play / Process Current Folder)
    - processH5Button: optional secondary action (e.g., Process All H5 Files)
    - prevButton / nextButton: frame navigation
    - prevFileButton / nextFileButton: file navigation (H5)
    - filenameLineEdit: editable file name box

    Layout is a compact 2-column grid to fit sidebars.
    """

    def __init__(
        self,
        process_folder_text="Play",
        process_h5_text=None,
        checkable_process_folder=True,
        checkable_process_h5=True,
        process_button_style="QPushButton { color: #ededed; background-color: #af6207}",
        parent=None,
    ):
        super().__init__(parent)

        # Controls
        self.processFolderButton = QPushButton(process_folder_text)
        self.processFolderButton.setCheckable(checkable_process_folder)
        self.processFolderButton.setStyleSheet(process_button_style)

        self.processH5Button = QPushButton(process_h5_text)
        self.processH5Button.setCheckable(checkable_process_h5)
        self.processH5Button.setStyleSheet(process_button_style)

        self.prevButton = QPushButton("<")
        self.nextButton = QPushButton(">")
        self.prevFileButton = QPushButton("<<<")
        self.nextFileButton = QPushButton(">>>")
        self.filenameLineEdit = QLineEdit()

        # Helpful tooltips (can be overridden by parent GUIs)
        self.nextButton.setToolTip('Next Frame')
        self.prevButton.setToolTip('Previous Frame')
        self.nextFileButton.setToolTip('Next H5 File in this Folder')
        self.prevFileButton.setToolTip('Previous H5 File in this Folder')

        # Layout
        layout = QGridLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        row = 0
        layout.addWidget(self.processFolderButton, row, 0, 1, 2)
        row += 1
        layout.addWidget(self.processH5Button, row, 0, 1, 2)
        row += 1

        layout.addWidget(self.prevButton, row, 0, 1, 1)
        layout.addWidget(self.nextButton, row, 1, 1, 1)
        row += 1
        layout.addWidget(self.prevFileButton, row, 0, 1, 1)
        layout.addWidget(self.nextFileButton, row, 1, 1, 1)
        row += 1
        layout.addWidget(self.filenameLineEdit, row, 0, 1, 2)

        # Default: hide file-level navigation until a GUI enables H5 mode
        self.prevFileButton.hide()
        self.nextFileButton.hide()
    
    def setNavMode(self, mode):
        """
        Set the navigation mode based on the mode parameter.
        """
        if mode == 'h5':
            self.nextFileButton.show()
            self.prevFileButton.show()
            self.processH5Button.show()
        else:
            self.nextFileButton.hide()
            self.prevFileButton.hide()
            self.processH5Button.hide()