"""
OutputDirDialog: Qt dialog for confirming or selecting the output directory.

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

import os
from typing import Optional

from PySide6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel,
    QLineEdit, QPushButton, QFileDialog, QMessageBox,
)

from ...utils.association_store import AssociationStore
from ...utils.directory_context import DirectoryContext


def _is_writable(path: str) -> bool:
    """Return True if *path* exists and is writable, or can be created."""
    if os.path.isdir(path):
        return os.access(path, os.W_OK)
    parent = os.path.dirname(path)
    return os.path.isdir(parent) and os.access(parent, os.W_OK)


class OutputDirDialog(QDialog):
    """Modal dialog asking the user to confirm or change the output directory."""

    def __init__(self, input_dir: Optional[str], suggested_output: str,
                 parent=None, info_text: str = ""):
        super().__init__(parent)
        self.setWindowTitle("Select Output Directory")
        self.setMinimumWidth(520)

        layout = QVBoxLayout(self)

        if input_dir is not None:
            layout.addWidget(QLabel(f"<b>Input directory:</b>  {input_dir}"))

        if info_text:
            layout.addWidget(QLabel(info_text))

        # Output directory (editable)
        layout.addWidget(QLabel("<b>Output directory:</b>"))
        row = QHBoxLayout()
        self._path_edit = QLineEdit(suggested_output)
        self._path_edit.setMinimumWidth(360)
        row.addWidget(self._path_edit)

        browse_btn = QPushButton("Browse...")
        browse_btn.clicked.connect(self._browse)
        row.addWidget(browse_btn)
        layout.addLayout(row)

        # OK / Cancel
        btn_row = QHBoxLayout()
        btn_row.addStretch()
        ok_btn = QPushButton("OK")
        ok_btn.setDefault(True)
        ok_btn.clicked.connect(self._on_ok)
        btn_row.addWidget(ok_btn)
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        btn_row.addWidget(cancel_btn)
        layout.addLayout(btn_row)

        self.chosen_output: Optional[str] = None

    def _browse(self):
        path = QFileDialog.getExistingDirectory(
            self, "Select Output Directory", self._path_edit.text()
        )
        if path:
            self._path_edit.setText(path)

    def _on_ok(self):
        path = self._path_edit.text().strip()
        if not path:
            QMessageBox.warning(self, "Empty path",
                                "Please specify an output directory.")
            return

        # Try creating if it doesn't exist
        if not os.path.isdir(path):
            try:
                os.makedirs(path, exist_ok=True)
            except OSError as exc:
                QMessageBox.warning(
                    self, "Cannot create directory",
                    f"Could not create output directory:\n{path}\n\n{exc}"
                )
                return

        if not _is_writable(path):
            QMessageBox.warning(
                self, "Not writable",
                f"The selected directory is not writable:\n{path}\n\n"
                "Please choose a different directory."
            )
            return

        self.chosen_output = os.path.realpath(path)
        self.accept()


# ---- convenience helper used by GUI integration points ----

_store = AssociationStore()


def resolve_output_directory(input_dir: str,
                             parent=None) -> Optional[DirectoryContext]:
    """Look up or ask the user for an output directory.

    Returns a :class:`DirectoryContext`, or ``None`` if the user cancels.

    If a stored association exists and is writable, it is used silently.
    If no association exists and the input directory is writable, it is
    used directly (co-located) without showing a dialog.  The dialog
    only appears when the input directory is not writable or a stored
    association has become invalid.
    """
    stored = _store.lookup(input_dir)

    if stored is not None:
        if _is_writable(stored):
            return DirectoryContext(input_dir=input_dir, output_dir=stored)
        info = (f"The previously associated output directory no longer "
                f"exists or is not writable:\n{stored}")
    else:
        if _is_writable(input_dir):
            return DirectoryContext.colocated(input_dir)
        info = f"The input directory is not writable:\n{input_dir}"

    suggested = stored if stored and os.path.isdir(stored) else ""

    dlg = OutputDirDialog(input_dir, suggested, parent=parent, info_text=info)
    if dlg.exec() != QDialog.Accepted or dlg.chosen_output is None:
        return None

    output_dir = dlg.chosen_output
    _store.save(input_dir, output_dir)
    return DirectoryContext(input_dir=input_dir, output_dir=output_dir)


def resolve_output_directory_headless(input_dir: str,
                                      output_dir: Optional[str] = None
                                      ) -> Optional[DirectoryContext]:
    """Non-interactive variant for headless / CLI mode.

    If *output_dir* is given explicitly it is used directly.
    Otherwise the :class:`AssociationStore` is consulted.
    Falls back to co-located when the input directory is writable.
    Returns ``None`` only if no writable output can be determined.
    """
    if output_dir:
        out = os.path.realpath(output_dir)
        if not os.path.isdir(out):
            try:
                os.makedirs(out, exist_ok=True)
            except OSError as exc:
                print(f"Error: cannot create output directory {out}: {exc}")
                return None
        if not _is_writable(out):
            print(f"Error: output directory is not writable: {out}")
            return None
        _store.save(input_dir, out)
        return DirectoryContext(input_dir=input_dir, output_dir=out)

    stored = _store.lookup(input_dir)
    if stored and _is_writable(stored):
        return DirectoryContext(input_dir=input_dir, output_dir=stored)

    if _is_writable(input_dir):
        return DirectoryContext.colocated(input_dir)

    print(f"Error: input directory is read-only and no output directory "
          f"is configured: {input_dir}")
    print("Use --output-dir to specify a writable output directory.")
    return None
