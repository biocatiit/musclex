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
from matplotlib.patches import Circle

from musclex import __version__
from .pyqt_utils import (
    QMainWindow, QWidget, QVBoxLayout, QGridLayout, QLabel, QStatusBar,
    QMessageBox, QApplication, QAction,
    QGroupBox, QCheckBox, QSpinBox, QPushButton,
)
from ..utils.file_manager import fullPath
from .widgets import ProcessingWorkspace
from .tools.radius_tool import RadiusTool


# Tags used to mark overlay artists drawn on the image_viewer axes so we can
# remove/redraw them without disturbing other artists (matplotlib image, etc.).
_RMIN_LABEL = 'tdi_rmin_overlay'
_RMAX_LABEL = 'tdi_rmax_overlay'


class TotalDiffractionIntensity(QMainWindow):
    """
    Total Diffraction Intensity GUI.

    Built on top of :class:`ProcessingWorkspace`. The workspace owns image
    display, navigation, output-directory handling, manual center / rotation
    and blank/mask configuration. TDI adds:

    * a Radial Range panel (R-min / R-max) that confines the intensity
      computation to an annulus around the (auto or manual) beam center;
    * per-image total + average intensity batch processing that writes
      ``summary.csv`` under ``<output_dir>/tdi_results/``.
    """

    def __init__(self):
        super().__init__()

        self.dir_path = ""
        self.stop_process = False

        # Radial range state (in pixels, integer)
        self.r_min_enabled = False
        self.r_max_enabled = False
        self.r_min_value = 1
        self.r_max_value = 1000

        self._build_ui()
        self._build_menu_bar()
        self._connect_signals()
        self._register_tools()
        self.resize(1200, 800)
        self.show()

    # ------------------------------------------------------------------
    # UI construction
    # ------------------------------------------------------------------

    def _build_ui(self):
        self.setWindowTitle(
            "Muscle X Total Diffraction Intensity v." + __version__
        )

        central = QWidget(self)
        self.setCentralWidget(central)
        layout = QVBoxLayout(central)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)

        # ProcessingWorkspace handles: image display, navigation controls,
        # output-directory dialog and the full Set Center / Set Rotation /
        # Set Blank / Set Mask machinery (with settings persistence).
        self.workspace = ProcessingWorkspace(settings_dir="")
        layout.addWidget(self.workspace, 1)

        # Friendly aliases used throughout this file.
        self.navigator = self.workspace.navigator
        self.image_viewer = self.workspace.navigator.image_viewer
        self.file_manager = self.workspace.file_manager
        self.navControls = self.workspace.navigator.nav_controls

        # Right panel: center → rotation → blank/mask → radial range.
        self.workspace.right_panel.add_widget(self.workspace._center_widget)
        self.workspace.right_panel.add_widget(self.workspace._rotation_widget)
        self.workspace.right_panel.add_widget(self.workspace._blank_mask_widget)
        self.workspace.right_panel.add_widget(self._build_radial_group())

        # Status bar
        self.statusBarWidget = QStatusBar()
        self.imgDetailOnStatusBar = QLabel(
            "  Please select an image or a folder to process"
        )
        self.statusBarWidget.addWidget(self.imgDetailOnStatusBar)
        self.setStatusBar(self.statusBarWidget)

    def _build_radial_group(self) -> QGroupBox:
        """Construct the R-min / R-max settings group placed in right panel."""
        grp = QGroupBox("Radial Range")
        grid = QGridLayout(grp)

        self.rMinChkBx = QCheckBox("Use R-min")
        self.rMinChkBx.setToolTip(
            "Exclude pixels inside this radius (pixels) from the sum.")
        self.rMinSpnBx = QSpinBox()
        self.rMinSpnBx.setRange(0, 100000)
        self.rMinSpnBx.setValue(self.r_min_value)
        self.rMinSpnBx.setKeyboardTracking(False)
        self.rMinSpnBx.setEnabled(False)
        self.setRminBtn = QPushButton("Set R-min")
        self.setRminBtn.setCheckable(True)
        self.setRminBtn.setToolTip(
            "Click on the image to set R-min as distance (in pixels) "
            "from the current center.")

        self.rMaxChkBx = QCheckBox("Use R-max")
        self.rMaxChkBx.setToolTip(
            "Exclude pixels outside this radius (pixels) from the sum.")
        self.rMaxSpnBx = QSpinBox()
        self.rMaxSpnBx.setRange(1, 100000)
        self.rMaxSpnBx.setValue(self.r_max_value)
        self.rMaxSpnBx.setKeyboardTracking(False)
        self.rMaxSpnBx.setEnabled(False)
        self.setRmaxBtn = QPushButton("Set R-max")
        self.setRmaxBtn.setCheckable(True)
        self.setRmaxBtn.setToolTip(
            "Click on the image to set R-max as distance (in pixels) "
            "from the current center.")

        self.showRingsChkBx = QCheckBox("Show R-min / R-max overlays")
        self.showRingsChkBx.setChecked(True)
        self.showRingsChkBx.setToolTip(
            "Draw dotted circles at R-min (red) and R-max (orange) on the image.")

        grid.addWidget(self.rMinChkBx, 0, 0)
        grid.addWidget(self.rMinSpnBx, 0, 1)
        grid.addWidget(self.setRminBtn, 0, 2)
        grid.addWidget(self.rMaxChkBx, 1, 0)
        grid.addWidget(self.rMaxSpnBx, 1, 1)
        grid.addWidget(self.setRmaxBtn, 1, 2)
        grid.addWidget(self.showRingsChkBx, 2, 0, 1, 3)
        return grp

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
        changeOutputDirAction.triggered.connect(
            self.workspace.change_output_directory)

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
        # Workspace ↔ TDI hooks
        self.workspace.navigator.fileLoaded.connect(self._on_folder_loaded)
        self.workspace.imageDataReady.connect(self._on_image_data_ready)
        self.workspace.needsReprocess.connect(self._on_needs_reprocess)
        # Picked up after RadiusTool completes (workspace already listens
        # to the same signal; it silently ignores unknown tool names).
        self.image_viewer.toolCompleted.connect(self._on_tool_completed)

        # Batch processing
        self.navControls.processFolderButton.clicked.connect(
            self._on_process_folder_toggled)
        self.navControls.processH5Button.clicked.connect(
            self._on_process_h5_toggled)

        # Radial range
        self.rMinChkBx.toggled.connect(self._on_rmin_chk_toggled)
        self.rMaxChkBx.toggled.connect(self._on_rmax_chk_toggled)
        self.rMinSpnBx.valueChanged.connect(self._on_rmin_value_changed)
        self.rMaxSpnBx.valueChanged.connect(self._on_rmax_value_changed)
        self.showRingsChkBx.toggled.connect(lambda _c: self._refresh_overlay())
        self.setRminBtn.toggled.connect(
            lambda c: self._toggle_radius_tool('rmin', c))
        self.setRmaxBtn.toggled.connect(
            lambda c: self._toggle_radius_tool('rmax', c))

    def _register_tools(self):
        """Register R-min / R-max picking tools on the image viewer."""
        tm = self.image_viewer.tool_manager
        tm.register_tool('rmin', RadiusTool, self._get_current_center,
                         color='red')
        tm.register_tool('rmax', RadiusTool, self._get_current_center,
                         color='orange')

    # ------------------------------------------------------------------
    # Workspace event hooks
    # ------------------------------------------------------------------

    def _on_folder_loaded(self, dir_path: str):
        """Folder/file was just loaded (before first image)."""
        self.dir_path = dir_path
        self._update_process_button_text()

    def _on_image_data_ready(self, image_data):
        """Single image is ready — refresh display + status bar + overlays."""
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

        self._refresh_overlay()

    def _on_needs_reprocess(self):
        """Settings changed (mask/blank/center/rotation) — redraw."""
        image_data = self.workspace._current_image_data
        if image_data is None:
            return
        image_data.reset_preprocessing()
        self._on_image_data_ready(image_data)

    def _on_tool_completed(self, tool_name, result):
        """Handle RadiusTool completion (workspace handles its own tools)."""
        if result is None:
            return
        if tool_name == 'rmin':
            self._set_radius_value('rmin', int(result), auto_enable=True)
            self.setRminBtn.setChecked(False)
        elif tool_name == 'rmax':
            self._set_radius_value('rmax', int(result), auto_enable=True)
            self.setRmaxBtn.setChecked(False)

    # ------------------------------------------------------------------
    # Radial range handlers
    # ------------------------------------------------------------------

    def _on_rmin_chk_toggled(self, checked: bool):
        self.r_min_enabled = checked
        self.rMinSpnBx.setEnabled(checked)
        self._refresh_overlay()

    def _on_rmax_chk_toggled(self, checked: bool):
        self.r_max_enabled = checked
        self.rMaxSpnBx.setEnabled(checked)
        self._refresh_overlay()

    def _on_rmin_value_changed(self, value: int):
        self.r_min_value = int(value)
        self._refresh_overlay()

    def _on_rmax_value_changed(self, value: int):
        self.r_max_value = int(value)
        self._refresh_overlay()

    def _set_radius_value(self, which: str, value: int, auto_enable: bool):
        """Update spinbox + state for ``which`` ∈ {'rmin','rmax'}."""
        if which == 'rmin':
            self.rMinSpnBx.blockSignals(True)
            try:
                self.rMinSpnBx.setValue(value)
            finally:
                self.rMinSpnBx.blockSignals(False)
            self.r_min_value = value
            if auto_enable and not self.r_min_enabled:
                self.rMinChkBx.setChecked(True)  # fires _on_rmin_chk_toggled
        else:
            self.rMaxSpnBx.blockSignals(True)
            try:
                self.rMaxSpnBx.setValue(value)
            finally:
                self.rMaxSpnBx.blockSignals(False)
            self.r_max_value = value
            if auto_enable and not self.r_max_enabled:
                self.rMaxChkBx.setChecked(True)
        self._refresh_overlay()

    def _toggle_radius_tool(self, name: str, checked: bool):
        """Activate / deactivate the RadiusTool tied to a Set R-* button."""
        tm = self.image_viewer.tool_manager
        if checked:
            # Mutual exclusion: untick the other radius button visually
            other_btn = (self.setRmaxBtn if name == 'rmin' else self.setRminBtn)
            if other_btn.isChecked():
                other_btn.blockSignals(True)
                other_btn.setChecked(False)
                other_btn.blockSignals(False)

            if self._get_current_center() is None:
                # No image loaded — bounce the button back and warn.
                btn = self.setRminBtn if name == 'rmin' else self.setRmaxBtn
                btn.blockSignals(True)
                btn.setChecked(False)
                btn.blockSignals(False)
                QMessageBox.information(
                    self, "No image loaded",
                    "Please load an image before setting R-min/R-max by clicking.")
                return

            tm.activate_tool(name)
            self.imgDetailOnStatusBar.setText(
                f"  Click on the image to set {name.upper()} "
                "(distance from center, in pixels)."
            )
        else:
            result = tm.deactivate_tool(name)
            if result is not None:
                self._set_radius_value(name, int(result), auto_enable=True)
            self.imgDetailOnStatusBar.setText("")

    def _get_current_center(self):
        """Return the current image's center, or ``None`` if no image."""
        image_data = self.workspace._current_image_data
        if image_data is None:
            return None
        try:
            return image_data.center
        except Exception:
            return None

    def _refresh_overlay(self):
        """Redraw the R-min / R-max overlay circles on the image axes."""
        ax = self.image_viewer.axes
        # Remove our old overlays only — leave the image (and any artists
        # belonging to other components) untouched.
        for patch in list(ax.patches):
            if patch.get_label() in (_RMIN_LABEL, _RMAX_LABEL):
                patch.remove()

        center = self._get_current_center()
        if center is None or not self.showRingsChkBx.isChecked():
            self.image_viewer.canvas.draw_idle()
            return

        cx, cy = center
        if self.r_min_enabled:
            ax.add_patch(Circle(
                (cx, cy), self.r_min_value,
                fill=False, ec='red', ls=':', lw=1.5, label=_RMIN_LABEL,
            ))
        if self.r_max_enabled:
            ax.add_patch(Circle(
                (cx, cy), self.r_max_value,
                fill=False, ec='orange', ls=':', lw=1.5, label=_RMAX_LABEL,
            ))
        self.image_viewer.canvas.draw_idle()

    # ------------------------------------------------------------------
    # Process Folder / H5 (PT-style Stop pattern)
    # ------------------------------------------------------------------

    def _update_process_button_text(self):
        if self.workspace.navigator.is_h5_mode:
            self.navControls.processFolderButton.setText(
                "Process Current H5 File")
        else:
            self.navControls.processFolderButton.setText(
                "Process Current Folder")
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
        rmin_text = str(self.r_min_value) if self.r_min_enabled else "off"
        rmax_text = str(self.r_max_value) if self.r_max_enabled else "off"
        info = (
            f"{len(img_ids)} image(s) will be processed using current "
            f"settings:\n"
            f"  - Apply Blank : {bool(config.get('apply_blank'))}\n"
            f"  - Apply Mask  : {bool(config.get('apply_mask'))}\n"
            f"  - R-min       : {rmin_text}\n"
            f"  - R-max       : {rmax_text}\n\n"
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
                    "Rmin": self.r_min_value if self.r_min_enabled else "",
                    "Rmax": self.r_max_value if self.r_max_enabled else "",
                    "TotalIntensity": total,
                    "AvgIntensity": (total / n_valid) if n_valid else 0.0,
                    "ValidPixels": n_valid,
                })

            df = pd.DataFrame(rows)
            df.to_csv(
                csv_path,
                index=False,
                columns=["ImageName", "Rmin", "Rmax",
                         "TotalIntensity", "AvgIntensity", "ValidPixels"],
            )

            stopped = self.stop_process
            self.imgDetailOnStatusBar.setText(
                f"  {'Stopped' if stopped else 'Done'}: "
                f"wrote {len(rows)} row(s) to {csv_path}"
            )
        finally:
            self._reset_batch_buttons()

    # ------------------------------------------------------------------
    # Measurement
    # ------------------------------------------------------------------

    def _annular_mask(self, shape, center, r_min=None, r_max=None):
        """Boolean array: True for pixels with r_min ≤ dist ≤ r_max."""
        h, w = shape
        cx, cy = center
        ys, xs = np.ogrid[:h, :w]
        r2 = (xs - cx) ** 2 + (ys - cy) ** 2
        mask = np.ones(shape, dtype=bool)
        if r_min is not None:
            mask &= r2 >= float(r_min) ** 2
        if r_max is not None:
            mask &= r2 <= float(r_max) ** 2
        return mask

    def _measure_image(self, img, name: str):
        """
        Compute (total_intensity, valid_pixel_count) for ``img`` using
        ImageData's standard preprocessing (so blank/mask Apply checkboxes
        are honoured exactly the same as in QF/PT) and the current
        R-min/R-max annular restriction.
        """
        image_data = self.workspace.create_image_data(img, name)
        working = image_data.get_working_image()
        invalid = image_data.invalid_pixel_threshold

        valid = working != invalid

        rmin = self.r_min_value if self.r_min_enabled else None
        rmax = self.r_max_value if self.r_max_enabled else None
        if rmin is not None or rmax is not None:
            try:
                center = image_data.center
            except Exception:
                center = None
            if center is not None:
                valid &= self._annular_mask(working.shape, center, rmin, rmax)

        n_valid = int(np.count_nonzero(valid))
        total = float(working[valid].sum()) if n_valid else 0.0
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
            "valid (non-masked) pixels inside the configured R-min/R-max "
            "annulus and writes them to tdi_results/summary.csv.",
        )
