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

import os
import numpy as np
import pandas as pd

from musclex import __version__
from .pyqt_utils import (
    QMainWindow, QWidget, QVBoxLayout, QLabel, QStatusBar, QMessageBox,
    QApplication, QAction,
)
from ..utils.file_manager import fullPath
from .widgets import ProcessingWorkspace


class TotalDiffractionIntensity(QMainWindow):
    """
    Total Diffraction Intensity GUI.

    Built on top of :class:`ProcessingWorkspace`. The workspace owns image
    display, navigation, output-directory handling and blank/mask configuration
    (Set Blank / Set Mask / Apply Blank / Apply Mask). This class only adds
    the TDI-specific batch summary: for every image in the current folder
    (or HDF5 file), compute the total and average intensity over valid pixels
    and write ``summary.csv`` under ``<output_dir>/tdi_results/``.
    """

    def __init__(self):
        super().__init__()

        self.dir_path = ""
        self.stop_process = False

        self._build_ui()
        self._build_menu_bar()
        self._connect_signals()
        self.resize(1200, 800)
        self.show()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        self.setWindowTitle("Muscle X Total Diffraction Intensity v." + __version__)

        central = QWidget(self)
        self.setCentralWidget(central)
        layout = QVBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # ProcessingWorkspace handles: image display, navigation controls,
        # blank/mask settings, output directory dialog, settings persistence.
        # TDI does not need center / rotation / quadrant-folded settings, so
        # only the blank/mask widget is added to the right panel.
        self.workspace = ProcessingWorkspace(settings_dir="")
        layout.addWidget(self.workspace, 1)

        # Backwards-friendly aliases for clarity in the rest of the file.
        self.navigator = self.workspace.navigator
        self.image_viewer = self.workspace.navigator.image_viewer
        self.file_manager = self.workspace.file_manager
        self.navControls = self.workspace.navigator.nav_controls

        # Only blank/mask is relevant for total intensity.
        self.workspace.right_panel.add_widget(self.workspace._blank_mask_widget)

        # Status bar
        self.statusBarWidget = QStatusBar()
        self.imgDetailOnStatusBar = QLabel(
            "  Please select an image or a folder to process"
        )
        self.statusBarWidget.addWidget(self.imgDetailOnStatusBar)
        self.setStatusBar(self.statusBarWidget)

    def _build_menu_bar(self):
        selectImageAction = QAction('Select an Image...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.setToolTip(
            "Open an image (or HDF5) file for total intensity analysis (Ctrl+I)"
        )
        selectImageAction.triggered.connect(self.navigator.browse_file)

        changeOutputDirAction = QAction('Change Output Directory...', self)
        changeOutputDirAction.setToolTip(
            "Choose a different folder to write tdi_results/summary.csv"
        )
        changeOutputDirAction.triggered.connect(self.workspace.change_output_directory)

        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addSeparator()
        fileMenu.addAction(changeOutputDirAction)

        aboutAct = QAction('About', self)
        aboutAct.triggered.connect(self._show_about)
        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)

    def _connect_signals(self):
        self.workspace.navigator.fileLoaded.connect(self._on_folder_loaded)
        self.workspace.imageDataReady.connect(self._on_image_data_ready)
        self.workspace.needsReprocess.connect(self._on_needs_reprocess)

        self.navControls.processFolderButton.clicked.connect(
            self._on_process_folder_toggled)
        self.navControls.processH5Button.clicked.connect(
            self._on_process_h5_toggled)

    # ------------------------------------------------------------------
    # Workspace event hooks
    # ------------------------------------------------------------------

    def _on_folder_loaded(self, dir_path: str):
        """Folder/file was just loaded (before first image)."""
        self.dir_path = dir_path
        self._update_process_button_text()

    def _on_image_data_ready(self, image_data):
        """Single image is ready — refresh display + status bar."""
        if image_data is None:
            return

        self.workspace.update_display(image_data)
        try:
            img = image_data.get_working_image()
        except Exception:
            img = image_data.get_raw_image()
        self.image_viewer.display_image(img)

        idx = self.navigator.current_index + 1
        total = len(self.file_manager.names) if self.file_manager.names else 0
        self.imgDetailOnStatusBar.setText(
            f"  Current File ({idx}/{total}) : {image_data.img_name}"
        )

    def _on_needs_reprocess(self):
        """Settings (blank/mask) changed — reapply preprocessing and redisplay."""
        image_data = self.workspace._current_image_data
        if image_data is None:
            return
        image_data.reset_preprocessing()
        self._on_image_data_ready(image_data)

    # ------------------------------------------------------------------
    # Process Folder / H5 (PT-style Stop pattern)
    # ------------------------------------------------------------------

    def _update_process_button_text(self):
        if self.workspace.navigator.is_h5_mode:
            self.navControls.processFolderButton.setText("Process Current H5 File")
        else:
            self.navControls.processFolderButton.setText("Process Current Folder")
        self.navControls.processH5Button.setText("Process All H5 Files")

    def _on_process_folder_toggled(self):
        btn = self.navControls.processFolderButton
        if btn.isChecked():
            btn.setText("Stop")
            idxs = range(len(self.file_manager.names))
            title = ("Process Current H5 File"
                     if self.workspace.navigator.is_h5_mode
                     else "Process Current Folder")
            self._run_batch(idxs, title)
        else:
            self.stop_process = True

    def _on_process_h5_toggled(self):
        btn = self.navControls.processH5Button
        if btn.isChecked():
            btn.setText("Stop")
            start, end = self.file_manager.get_current_h5_range()
            if start is None or end is None:
                btn.setChecked(False)
                self._update_process_button_text()
                return
            self._run_batch(range(start, end + 1), "Process Current H5 File")
        else:
            self.stop_process = True

    def _run_batch(self, img_ids, title: str):
        """Compute total/avg intensities for ``img_ids`` and write summary.csv."""
        img_ids = list(img_ids)
        if not img_ids:
            self._reset_batch_buttons()
            return

        config = self.workspace.get_blank_mask_config()
        info = (
            f"{len(img_ids)} image(s) will be processed using current "
            f"blank/mask settings:\n"
            f"  - Apply Blank : {bool(config.get('apply_blank'))}\n"
            f"  - Apply Mask  : {bool(config.get('apply_mask'))}\n\n"
            "Are you sure you want to continue?"
        )
        msg = QMessageBox(self)
        msg.setWindowTitle(title)
        msg.setIcon(QMessageBox.Warning)
        msg.setText(title)
        msg.setInformativeText(info)
        msg.setStandardButtons(QMessageBox.Yes | QMessageBox.Cancel)
        if msg.exec_() != QMessageBox.Yes:
            self._reset_batch_buttons()
            return

        # Resolve output directory
        out_dir = (self.workspace.dir_context.output_dir
                   if self.workspace.dir_context else self.dir_path)
        result_dir = fullPath(out_dir, "tdi_results")
        if not os.path.exists(result_dir):
            os.makedirs(result_dir, exist_ok=True)
        csv_path = fullPath(result_dir, "summary.csv")

        self.stop_process = False
        rows = []
        names = self.file_manager.names

        try:
            for i_loop, idx in enumerate(img_ids):
                if self.stop_process:
                    break
                if idx < 0 or idx >= len(names):
                    continue
                name = names[idx]
                self.imgDetailOnStatusBar.setText(
                    f"  Processing ({i_loop + 1}/{len(img_ids)}) : {name}"
                )
                # Allow Stop button to remain responsive
                if i_loop % 5 == 0:
                    QApplication.processEvents()

                img = self.file_manager.get_image_by_index(idx)
                if img is None:
                    print(f"TDI: failed to load image '{name}', skipping.")
                    continue

                try:
                    total, n_valid = self._measure_image(img, name)
                except Exception as e:
                    print(f"TDI: error measuring '{name}': {e}. Skipping.")
                    continue

                rows.append({
                    "ImageName": name,
                    "TotalIntensity": total,
                    "AvgIntensity": (total / n_valid) if n_valid else 0.0,
                    "ValidPixels": n_valid,
                })

            df = pd.DataFrame(rows)
            df.to_csv(
                csv_path,
                index=False,
                columns=["ImageName", "TotalIntensity",
                         "AvgIntensity", "ValidPixels"],
            )

            stopped = self.stop_process
            self.imgDetailOnStatusBar.setText(
                f"  {'Stopped' if stopped else 'Done'}: wrote {len(rows)} row(s) to {csv_path}"
            )
        finally:
            self._reset_batch_buttons()

    def _measure_image(self, img, name: str):
        """
        Compute (total_intensity, valid_pixel_count) for ``img`` using
        ImageData's standard preprocessing (so blank/mask Apply checkboxes
        are honoured exactly the same as in QF/PT). Pixels marked invalid
        by mask application are excluded from both numerator and denominator.
        """
        image_data = self.workspace.create_image_data(img, name)
        working = image_data.get_working_image()
        invalid = image_data.invalid_pixel_threshold

        valid_mask = working != invalid
        n_valid = int(np.count_nonzero(valid_mask))
        total = float(working[valid_mask].sum()) if n_valid else 0.0
        return total, n_valid

    def _reset_batch_buttons(self):
        self.navControls.processFolderButton.blockSignals(True)
        self.navControls.processH5Button.blockSignals(True)
        try:
            self.navControls.processFolderButton.setChecked(False)
            self.navControls.processH5Button.setChecked(False)
        finally:
            self.navControls.processFolderButton.blockSignals(False)
            self.navControls.processH5Button.blockSignals(False)
        self._update_process_button_text()

    # ------------------------------------------------------------------
    # Misc
    # ------------------------------------------------------------------

    def _show_about(self):
        QMessageBox.about(
            self,
            "About",
            "Muscle X – Total Diffraction Intensity v." + __version__ +
            "\n\nComputes per-image total and average intensities over the "
            "valid (non-masked) pixels and writes them to "
            "tdi_results/summary.csv.",
        )
