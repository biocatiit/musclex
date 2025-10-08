from PySide6.QtWidgets import QWidget, QPushButton, QGridLayout, QLineEdit


class NavigationControls(QWidget):
    """
    Reusable navigation/control widget used across multiple GUIs.

    Provides:
    - primaryButton: main action (e.g., Play / Process Current Folder)
    - secondaryButton: optional secondary action (e.g., Process All H5 Files)
    - prevButton / nextButton: frame navigation
    - prevFileButton / nextFileButton: file navigation (H5)
    - filenameLineEdit: editable file name box

    Layout is a compact 2-column grid to fit sidebars.
    """

    def __init__(
        self,
        primary_text="Play",
        secondary_text=None,
        checkable_primary=True,
        checkable_secondary=True,
        parent=None,
    ):
        super().__init__(parent)

        # Controls
        self.primaryButton = QPushButton(primary_text)
        self.primaryButton.setCheckable(checkable_primary)

        self.secondaryButton = None
        if secondary_text is not None:
            self.secondaryButton = QPushButton(secondary_text)
            self.secondaryButton.setCheckable(checkable_secondary)

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
        layout.addWidget(self.primaryButton, row, 0, 1, 2)
        row += 1
        if self.secondaryButton is not None:
            layout.addWidget(self.secondaryButton, row, 0, 1, 2)
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

    # Convenience helpers for parent widgets
    def setPrimaryText(self, text):
        self.primaryButton.setText(text)

    def setSecondaryVisible(self, visible):
        if self.secondaryButton is not None:
            self.secondaryButton.setVisible(visible)


