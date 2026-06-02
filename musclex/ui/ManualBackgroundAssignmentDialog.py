"""
Manual assignment dialog for background configurations.
"""

from .pyqt_utils import *


class ManualBackgroundAssignmentDialog(QDialog):
    """Dialog for assigning saved background configurations to images."""

    def __init__(self, image_names, configuration_names, current_assignments=None, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Manual Background Configuration Assignment")
        self.resize(780, 600)

        self.image_names = list(image_names or [])
        self.configuration_names = list(configuration_names or [])
        self.current_assignments = dict(current_assignments or {})

        self._build_ui()
        self._populate_table()

    def _build_ui(self):
        layout = QVBoxLayout(self)

        info = QLabel(
            "Select a background configuration for each image. "
            "Leave as 'Auto / Unassigned' to use automatic selection or default behavior."
        )
        info.setWordWrap(True)
        layout.addWidget(info)

        controls = QHBoxLayout()
        controls.addWidget(QLabel("Set all rows to:"))
        self.bulkCombo = QComboBox()
        self.bulkCombo.addItem("Auto / Unassigned")
        for name in self.configuration_names:
            self.bulkCombo.addItem(name)
        controls.addWidget(self.bulkCombo)

        self.applyAllButton = QPushButton("Apply to All")
        self.applyAllButton.clicked.connect(self._apply_bulk_selection)
        controls.addWidget(self.applyAllButton)
        controls.addStretch(1)
        layout.addLayout(controls)

        self.table = QTableWidget(0, 2)
        self.table.setHorizontalHeaderLabels(["Image", "Assigned Configuration"])
        self.table.verticalHeader().setVisible(False)
        self.table.setAlternatingRowColors(True)
        self.table.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self.table.setSelectionBehavior(QAbstractItemView.SelectRows)
        self.table.setSelectionMode(QAbstractItemView.SingleSelection)
        self.table.horizontalHeader().setSectionResizeMode(0, QHeaderView.Stretch)
        self.table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeToContents)
        layout.addWidget(self.table)

        self.buttonBox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        self.buttonBox.accepted.connect(self.accept)
        self.buttonBox.rejected.connect(self.reject)
        layout.addWidget(self.buttonBox)

    def _populate_table(self):
        self.table.setRowCount(len(self.image_names))

        for row, img_name in enumerate(self.image_names):
            item = QTableWidgetItem(str(img_name))
            item.setFlags(item.flags() & ~Qt.ItemIsEditable)
            self.table.setItem(row, 0, item)

            combo = QComboBox()
            combo.addItem("Auto / Unassigned")
            for name in self.configuration_names:
                combo.addItem(name)

            assigned_name = self.current_assignments.get(img_name, "")
            idx = combo.findText(assigned_name)
            combo.setCurrentIndex(idx if idx >= 0 else 0)
            self.table.setCellWidget(row, 1, combo)

    def _apply_bulk_selection(self):
        bulk_name = self.bulkCombo.currentText()
        for row in range(self.table.rowCount()):
            combo = self.table.cellWidget(row, 1)
            if combo is None:
                continue
            idx = combo.findText(bulk_name)
            combo.setCurrentIndex(idx if idx >= 0 else 0)

    def get_assignments(self):
        """Return mapping: image_name -> configuration_name (only assigned rows)."""
        assignments = {}
        for row in range(self.table.rowCount()):
            name_item = self.table.item(row, 0)
            combo = self.table.cellWidget(row, 1)
            if name_item is None or combo is None:
                continue

            img_name = name_item.text()
            cfg_name = combo.currentText()
            if cfg_name and cfg_name != "Auto / Unassigned":
                assignments[img_name] = cfg_name
        return assignments
