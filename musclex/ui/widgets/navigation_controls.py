from PySide6.QtWidgets import (
    QWidget,
    QPushButton,
    QGridLayout,
    QLineEdit,
    QCheckBox,
)


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
        self.default_process_folder_text = process_folder_text

        # Controls
        self.processFolderButton = QPushButton(process_folder_text)
        self.processFolderButton.setCheckable(checkable_process_folder)
        self.processFolderButton.setStyleSheet(process_button_style)

        self.processH5Button = QPushButton(process_h5_text)
        self.processH5Button.setCheckable(checkable_process_h5)
        self.processH5Button.setStyleSheet(process_button_style)

        # # Batch processing options (initially hidden, can be shown by parent GUIs)
        # self.batchScopeWidget = QWidget()
        # self.batchScopeLayout = QGridLayout(self.batchScopeWidget)
        # self.batchScopeLayout.setContentsMargins(0, 0, 0, 0)

        self.select_batch_folder_button = QPushButton("Choose Batch Folders")
        self.select_batch_folder_button.setToolTip(
            "Choose one or more folders to process with the current settings."
        )
        self.select_batch_folder_button.hide()
        # self.batchScopeLayout.addWidget(self.select_batch_folder_button, 0, 0, 1, 3)

        # self.process_sibling_folder_checkbox = QCheckBox("Sibling folders")
        # self.process_sibling_folder_checkbox.setToolTip(
        #     "Also process folders next to the current folder."
        # )
        # self.process_recursively_checkbox = QCheckBox("Recursive")
        # self.process_recursively_checkbox.setToolTip(
        #     "Also process image folders inside the selected folder scope."
        # )

        # self.cache_checkbox = QCheckBox("Cache Settings")
        # self.cache_checkbox.setToolTip(
        #     "Save qfsettings.json automatically when processing the folder."
        # )

        # self.batchScopeLayout.addWidget(self.process_sibling_folder_checkbox, 0, 0)
        # self.batchScopeLayout.addWidget(self.process_recursively_checkbox, 0, 1)
        # self.batchScopeLayout.addWidget(self.cache_checkbox, 0, 2)
        # self.batchScopeLayout.setColumnStretch(0, 1)
        # self.batchScopeLayout.setColumnStretch(1, 1)
        # self.batchScopeLayout.setColumnStretch(2, 1)
        # self.batchScopeWidget.hide()

        self.prevButton = QPushButton("<")
        self.nextButton = QPushButton(">")
        self.prevFileButton = QPushButton("<<<")
        self.nextFileButton = QPushButton(">>>")
        self.filenameLineEdit = QLineEdit()

        # Helpful tooltips (can be overridden by parent GUIs)
        self.nextButton.setToolTip("Go to the next image / frame in the folder")
        self.prevButton.setToolTip("Go to the previous image / frame in the folder")
        self.nextFileButton.setToolTip("Jump to the next H5 file in this folder")
        self.prevFileButton.setToolTip("Jump to the previous H5 file in this folder")
        self.processFolderButton.setToolTip(
            "Process every image in the current folder using the current settings"
        )
        self.processH5Button.setToolTip("Process all H5 files in the current folder")

        # Layout
        layout = QGridLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)

        row = 0
        layout.addWidget(self.select_batch_folder_button, row, 0, 1, 2)
        row += 1
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
        if mode == "h5":
            self.nextFileButton.show()
            self.prevFileButton.show()
            self.processH5Button.show()
        else:
            self.nextFileButton.hide()
            self.prevFileButton.hide()
            self.processH5Button.hide()

    def set_select_batch_folder_visible(self, visible):
        """
        Show or hide the "Choose Batch Folders" button.
        """
        self.select_batch_folder_button.setVisible(visible)

    def reset_process_folder_text(self):
        """
        Reset the process folder button text to the default.
        """
        self.processFolderButton.setText(self.default_process_folder_text)

    def reset_batch_folder_button_text(self):
        """
        Reset the batch folder button text to the default.
        """
        self.select_batch_folder_button.setText("Choose Batch Folders")
