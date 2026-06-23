from pathlib import Path

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QFileDialog,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QTreeWidget,
    QTreeWidgetItem,
    QVBoxLayout,
)


class BatchFolderSelectionDialog(QDialog):
    def __init__(self, parent=None, start_dir=""):
        super().__init__(parent)

        self.setWindowTitle("Choose Batch Folders")
        self.resize(760, 560)

        self.start_dir = Path(start_dir or Path.home()).resolve()
        self._updating_checks = False

        layout = QVBoxLayout(self)

        self.summaryLabel = QLabel("Select folders to process.")
        layout.addWidget(self.summaryLabel)

        buttonLayout = QHBoxLayout()

        self.chooseRootButton = QPushButton("Choose Root Folder")
        self.chooseRootButton.clicked.connect(self.choose_root_folder)
        buttonLayout.addWidget(self.chooseRootButton)

        self.clearButton = QPushButton("Clear")
        self.clearButton.clicked.connect(self.clear_checks)
        buttonLayout.addWidget(self.clearButton)

        layout.addLayout(buttonLayout)

        self.tree = QTreeWidget()
        self.tree.setHeaderLabels(["Folder"])
        self.tree.setColumnCount(1)
        self.tree.itemExpanded.connect(self._on_item_expanded)
        self.tree.itemChanged.connect(self._on_item_changed)
        layout.addWidget(self.tree)

        buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)

        self.set_root_folder(self.start_dir)

    def choose_root_folder(self):
        folder = QFileDialog.getExistingDirectory(
            self,
            "Choose Parent Folder",
            str(self.start_dir),
        )
        if folder:
            self.set_root_folder(Path(folder).resolve())

    def set_root_folder(self, folder):
        self.start_dir = Path(folder).resolve()
        self.tree.clear()

        root_item = self._make_item(self.start_dir)
        self.tree.addTopLevelItem(root_item)
        self._populate_one_level(root_item)
        root_item.setExpanded(True)

        self._update_summary()

    def selected_folders(self):
        folders = []

        def walk(item):
            for i in range(item.childCount()):
                child = item.child(i)
                raw_path = child.data(0, Qt.UserRole)

                if raw_path and child.checkState(0) == Qt.Checked:
                    folders.append(Path(raw_path))

                walk(child)

        walk(self.tree.invisibleRootItem())
        return sorted(set(folders))

    def clear_checks(self):
        self._updating_checks = True
        try:
            root = self.tree.invisibleRootItem()
            self._set_checks_recursive(root, Qt.Unchecked)
        finally:
            self._updating_checks = False

        self._update_summary()

    def _make_item(self, path):
        item = QTreeWidgetItem([path.name or str(path)])
        item.setData(0, Qt.UserRole, str(path))
        item.setFlags(item.flags() | Qt.ItemIsUserCheckable)
        item.setCheckState(0, Qt.Unchecked)
        item.setToolTip(0, str(path))
        return item

    def _child_dirs(self, path):
        try:
            return sorted(
                [p for p in Path(path).iterdir() if p.is_dir()],
                key=lambda p: p.name.lower(),
            )
        except Exception:
            return []

    def _populate_one_level(self, item):
        if item.data(0, Qt.UserRole + 1) == "loaded":
            return

        path = Path(item.data(0, Qt.UserRole))
        item.takeChildren()

        for child_path in self._child_dirs(path):
            child = self._make_item(child_path)
            item.addChild(child)

            if self._child_dirs(child_path):
                placeholder = QTreeWidgetItem(["Loading..."])
                placeholder.setData(0, Qt.UserRole, "")
                child.addChild(placeholder)

        item.setData(0, Qt.UserRole + 1, "loaded")

    def _on_item_expanded(self, item):
        self._populate_one_level(item)

    def _on_item_changed(self, item, column):
        if self._updating_checks or column != 0:
            return

        self._update_summary()

    def _set_checks_recursive(self, item, state):
        for i in range(item.childCount()):
            child = item.child(i)
            if child.data(0, Qt.UserRole):
                child.setCheckState(0, state)
            self._set_checks_recursive(child, state)

    def _update_summary(self):
        count = len(self.selected_folders())
        self.summaryLabel.setText(f"{count} folder(s) selected.")
