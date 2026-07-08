from pathlib import Path

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QFileDialog,
    QHBoxLayout,
    QLabel,
    QListWidget,
    QListWidgetItem,
    QPushButton,
    QStyle,
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

        selectionLayout = QHBoxLayout()

        self.tree = QTreeWidget()
        self.tree.setHeaderLabels(["Folder"])
        self.tree.setColumnCount(1)
        self.tree.itemExpanded.connect(self._on_item_expanded)
        self.tree.itemChanged.connect(self._on_item_changed)
        selectionLayout.addWidget(self.tree, 3)

        selectedLayout = QVBoxLayout()
        selectedLayout.addWidget(QLabel("Selected order"))

        self.selectedList = QListWidget()
        selectedLayout.addWidget(self.selectedList, 1)

        moveLayout = QHBoxLayout()
        self.moveUpButton = QPushButton()
        self.moveUpButton.setIcon(
            self.style().standardIcon(QStyle.StandardPixmap.SP_ArrowUp)
        )
        self.moveUpButton.setToolTip("Move selected folder up")
        self.moveUpButton.clicked.connect(self.move_selected_folder_up)
        moveLayout.addWidget(self.moveUpButton)

        self.moveDownButton = QPushButton()
        self.moveDownButton.setIcon(
            self.style().standardIcon(QStyle.StandardPixmap.SP_ArrowDown)
        )
        self.moveDownButton.setToolTip("Move selected folder down")
        self.moveDownButton.clicked.connect(self.move_selected_folder_down)
        moveLayout.addWidget(self.moveDownButton)

        selectedLayout.addLayout(moveLayout)
        selectionLayout.addLayout(selectedLayout, 2)
        layout.addLayout(selectionLayout)

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
        self.selectedList.clear()

        root_item = self._make_item(self.start_dir)
        self.tree.addTopLevelItem(root_item)
        self._populate_one_level(root_item)
        root_item.setExpanded(True)

        self._update_summary()

    def selected_folders(self):
        folders = []
        seen = set()

        for row in range(self.selectedList.count()):
            raw_path = self.selectedList.item(row).data(Qt.UserRole)
            if not raw_path or raw_path in seen:
                continue
            folders.append(Path(raw_path))
            seen.add(raw_path)

        return folders

    def clear_checks(self):
        self._updating_checks = True
        try:
            root = self.tree.invisibleRootItem()
            self._set_checks_recursive(root, Qt.Unchecked)
        finally:
            self._updating_checks = False

        self.selectedList.clear()
        self._update_summary()

    def move_selected_folder_up(self):
        self._move_selected_folder(-1)

    def move_selected_folder_down(self):
        self._move_selected_folder(1)

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

        raw_path = item.data(0, Qt.UserRole)
        if raw_path:
            if item.checkState(0) == Qt.Checked:
                self._add_selected_folder(raw_path)
            else:
                self._remove_selected_folder(raw_path)

        self._update_summary()

    def _set_checks_recursive(self, item, state):
        for i in range(item.childCount()):
            child = item.child(i)
            if child.data(0, Qt.UserRole):
                child.setCheckState(0, state)
            self._set_checks_recursive(child, state)

    def _add_selected_folder(self, raw_path):
        raw_path = str(raw_path)
        if self._selected_list_row(raw_path) is not None:
            return

        path = Path(raw_path)
        selected_item = QListWidgetItem(path.name or str(path))
        selected_item.setData(Qt.UserRole, raw_path)
        selected_item.setToolTip(raw_path)
        self.selectedList.addItem(selected_item)
        self.selectedList.setCurrentRow(self.selectedList.count() - 1)

    def _remove_selected_folder(self, raw_path):
        row = self._selected_list_row(raw_path)
        if row is not None:
            self.selectedList.takeItem(row)

    def _selected_list_row(self, raw_path):
        raw_path = str(raw_path)
        for row in range(self.selectedList.count()):
            if self.selectedList.item(row).data(Qt.UserRole) == raw_path:
                return row
        return None

    def _move_selected_folder(self, offset):
        row = self.selectedList.currentRow()
        if row < 0:
            return

        target = row + offset
        if target < 0 or target >= self.selectedList.count():
            return

        item = self.selectedList.takeItem(row)
        self.selectedList.insertItem(target, item)
        self.selectedList.setCurrentRow(target)
        self._update_summary()

    def _update_summary(self):
        count = len(self.selected_folders())
        self.summaryLabel.setText(f"{count} folder(s) selected.")
