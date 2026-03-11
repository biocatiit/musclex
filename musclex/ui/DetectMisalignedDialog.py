import os
from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTableWidget, QTableWidgetItem,
    QPushButton, QHeaderView, QAbstractItemView, QLabel, QProgressBar,
    QSizePolicy
)
from PySide6.QtCore import Qt
from PySide6.QtGui import QColor, QBrush


class DetectMisalignedDialog(QDialog):
    """
    Dialog that lists all images from the file manager in a table.
    Columns: Frame, Original Center, Center Mode Distance,
             Rotation, Rotation Mode Distance, Image Difference.
    Rows for misaligned images are highlighted in red when detection data
    is provided.
    """

    # Column indices
    COL_FRAME = 0
    COL_CENTER = 1
    COL_CENTER_DIST = 2
    COL_ROTATION = 3
    COL_ROTATION_DIST = 4
    COL_IMAGE_DIFF = 5

    HEADERS = [
        "Frame",
        "Original Center",
        "Center Mode Distance",
        "Rotation",
        "Rotation Mode",
        "Image Difference",
    ]

    def __init__(self, img_list, dir_path="", misaligned_names=None,
                 items_data=None, parent=None):
        """
        Parameters
        ----------
        img_list : list[str]
            All image file names (base names for tiff, full names for hdf5).
        dir_path : str
            Directory path (used for display only, not loading).
        misaligned_names : set[str] | None
            Names of images flagged as misaligned; highlighted in red.
        items_data : list[dict] | None
            Per-image metric dicts with keys:
            'name', 'center', 'center_dist', 'angle', 'angle_dist', 'image_diff'
            If None, only the Frame column is populated.
        parent : QWidget | None
        """
        super().__init__(parent)
        self.setWindowTitle("Detect Misaligned Images")
        self.img_list = img_list or []
        self.dir_path = dir_path
        self.misaligned_names = set(misaligned_names) if misaligned_names else set()
        self.items_data = items_data  # None until detection is run

        self._build_ui()
        self.resize(900, 600)

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        root = QVBoxLayout(self)
        root.setContentsMargins(8, 8, 8, 8)
        root.setSpacing(6)

        # Status label
        self.statusLabel = QLabel("")
        self.statusLabel.setVisible(False)
        root.addWidget(self.statusLabel)

        # Progress bar (hidden until needed)
        self.progressBar = QProgressBar()
        self.progressBar.setVisible(False)
        root.addWidget(self.progressBar)

        # Table
        self.table = QTableWidget()
        self.table.setColumnCount(len(self.HEADERS))
        self.table.setHorizontalHeaderLabels(self.HEADERS)
        self.table.setSelectionBehavior(QAbstractItemView.SelectRows)
        self.table.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self.table.setAlternatingRowColors(True)
        self.table.verticalHeader().setDefaultSectionSize(22)
        self.table.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)

        header = self.table.horizontalHeader()
        header.setSectionResizeMode(self.COL_FRAME, QHeaderView.Stretch)
        for col in range(1, len(self.HEADERS)):
            header.setSectionResizeMode(col, QHeaderView.ResizeToContents)

        root.addWidget(self.table)

        # Buttons
        btn_layout = QHBoxLayout()
        self.closeButton = QPushButton("Close")
        btn_layout.addStretch()
        btn_layout.addWidget(self.closeButton)
        root.addLayout(btn_layout)

        self.closeButton.clicked.connect(self.accept)

        # Populate with whatever data is already available
        self.populate()

    # ------------------------------------------------------------------
    # Data population
    # ------------------------------------------------------------------

    def populate(self):
        """Fill the table from img_list / items_data."""
        self.table.setRowCount(0)

        if self.items_data:
            self._populate_from_items_data()
        else:
            self._populate_names_only()

    def _populate_names_only(self):
        """Populate only the Frame column from img_list."""
        self.table.setRowCount(len(self.img_list))
        for row, name in enumerate(self.img_list):
            item = QTableWidgetItem(os.path.basename(name))
            item.setToolTip(name)
            self.table.setItem(row, self.COL_FRAME, item)
            self._apply_misaligned_highlight(row, name)

    def _populate_from_items_data(self):
        """Populate all columns from items_data dicts."""
        self.table.setRowCount(len(self.items_data))
        for row, d in enumerate(self.items_data):
            name = d.get("name", "")
            base = os.path.basename(name)

            def cell(text):
                return QTableWidgetItem(str(text))

            frame_item = QTableWidgetItem(base)
            frame_item.setToolTip(name)
            self.table.setItem(row, self.COL_FRAME, frame_item)

            cx, cy = d.get("center", (None, None))
            center_text = f"({cx:.1f}, {cy:.1f})" if cx is not None else ""
            self.table.setItem(row, self.COL_CENTER, cell(center_text))

            cd = d.get("center_dist")
            self.table.setItem(row, self.COL_CENTER_DIST,
                               cell(f"{cd:.4f}" if cd is not None else ""))

            ang = d.get("angle")
            self.table.setItem(row, self.COL_ROTATION,
                               cell(f"{ang:.4f}" if ang is not None else ""))

            ad = d.get("angle_dist")
            self.table.setItem(row, self.COL_ROTATION_DIST,
                               cell(f"{ad:.4f}" if ad is not None else ""))

            img_diff = d.get("image_diff")
            self.table.setItem(row, self.COL_IMAGE_DIFF,
                               cell(f"{img_diff:.4f}" if img_diff is not None else ""))

            self._apply_misaligned_highlight(row, name)

    def _apply_misaligned_highlight(self, row, name):
        """Colour the whole row red if the image is in misaligned_names."""
        if not self.misaligned_names:
            return
        base = os.path.basename(name)
        if name in self.misaligned_names or base in self.misaligned_names:
            highlight = QBrush(QColor(255, 120, 120))
            for col in range(self.table.columnCount()):
                item = self.table.item(row, col)
                if item is None:
                    item = QTableWidgetItem("")
                    self.table.setItem(row, col, item)
                item.setBackground(highlight)

    # ------------------------------------------------------------------
    # Public helpers for updating the dialog after detection
    # ------------------------------------------------------------------

    def setMisalignedNames(self, misaligned_names):
        self.misaligned_names = set(misaligned_names)
        self.populate()

    def setItemsData(self, items_data):
        self.items_data = items_data
        self.populate()

    def setStatus(self, text):
        self.statusLabel.setText(text)
        self.statusLabel.setVisible(bool(text))
