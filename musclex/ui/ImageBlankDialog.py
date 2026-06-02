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

import sys
import traceback
import json
import os
from pathlib import Path
import numpy as np

from PySide6.QtWidgets import (
    QApplication,
    QDialog,
    QLabel,
    QPushButton,
    QGroupBox,
    QCheckBox,
    QRadioButton,
    QButtonGroup,
    QDoubleSpinBox,
    QComboBox,
    QDialogButtonBox,
    QGridLayout,
    QHBoxLayout,
    QWidget,
    QVBoxLayout,
    QScrollArea,
)

from PySide6.QtCore import Qt

import fabio

from .pyqt_utils import getAFile
from .widgets.image_viewer_widget import ImageViewerWidget


class ImageBlankDialog(QDialog):
    def __init__(self, image_data, settings_dir_path, vmin, vmax):
        """
        Initialize Empty Cell Subtraction Dialog.

        Args:
            image_data: numpy array of the image (not a file path!)
            settings_dir_path: directory where blank config will be saved
            vmin: minimum intensity for display
            vmax: maximum intensity for display
        """
        super().__init__()
        self.setModal(True)
        self.setWindowTitle("Empty Cell Subraction")
        self.settings_dir_path = Path(settings_dir_path)
        self.vmin = vmin
        self.vmax = vmax

        # Store image data directly (no file I/O needed)
        self.imageData = np.asarray(image_data).astype("float32")

        self.blank_config_file_path = (
            self.settings_dir_path / "blank_image_settings.json"
        )

        blank_image_info = self.read_blank_image_info(self.blank_config_file_path)

        if (
            (blank_image_info is not None)
            and (blank_image_info.get("blank_image") is not None)
            and blank_image_info["blank_image"].shape == self.imageData.shape
        ):
            self.blank_image_info = blank_image_info
        else:
            self.blank_image_info = None

        # Image viewer (replaces the previous QLabel + manual QPixmap rendering).
        # Provides built-in pan, wheel zoom, zoom-rectangle tool, and a
        # DisplayOptionsPanel for intensity / log scale / colormap controls.
        self.imageViewer = ImageViewerWidget(parent=self, show_display_panel=True)
        self.imageViewer.canvas.setMinimumSize(800, 600)
        # Seed initial intensity into the viewer (and its display_panel) before
        # the first display_image() call so the panel reflects the caller's
        # vmin/vmax instead of auto-scaling to img.min()/img.max().
        self.imageViewer.set_display_options(vmin=self.vmin, vmax=self.vmax)

        self.statusBar = QLabel("Current View: No Display")
        self.statusBar.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)

        self.imageWidget = QWidget()
        self.imageLayout = QVBoxLayout(self.imageWidget)
        self.imageLayout.setContentsMargins(0, 0, 0, 0)
        self.imageLayout.addWidget(self.imageViewer)
        self.imageLayout.addWidget(self.statusBar)

        # Empty Cell Image Selection Group (Top)
        self.blankSelectionGroup = QGroupBox("Empty Cell Image Selection")
        self.selectBlankBtn = QPushButton("Select Empty Cell Image")
        self.blankStatusText = QLabel()
        self.blankWeightLabel = QLabel("Empty Cell Image Scale: ")
        self.blankWeightText = QDoubleSpinBox()
        self.blankWeightText.setKeyboardTracking(False)
        self.blankWeightText.setRange(0, 1000)
        self.blankWeightText.setValue(1.00)
        self.blankWeightText.setSingleStep(0.01)

        self.blankSelectionLayout = QGridLayout(self.blankSelectionGroup)
        row = 0
        self.blankSelectionLayout.addWidget(self.selectBlankBtn, row, 0, 1, 2)
        self.blankSelectionLayout.addWidget(self.blankStatusText, row, 2, 1, 2)
        row += 1
        self.blankSelectionLayout.addWidget(self.blankWeightLabel, row, 0, 1, 2)
        self.blankSelectionLayout.addWidget(self.blankWeightText, row, 2, 1, 2)

        # Compare Group - 3 exclusive radio buttons that pick which image to show.
        # Renamed from "Display Options" so it does not collide with the
        # ImageViewerWidget's built-in DisplayOptionsPanel.
        self.compareGroup = QGroupBox("Compare")
        self.displayButtonGroup = QButtonGroup()

        self.differenceImageRadio = QRadioButton(
            "Difference Image (Original - Empty Cell)"
        )
        self.originalImageRadio = QRadioButton("Original Image")
        self.emptyCellImageRadio = QRadioButton("Empty Cell Image")

        self.displayButtonGroup.addButton(self.differenceImageRadio, 0)
        self.displayButtonGroup.addButton(self.originalImageRadio, 1)
        self.displayButtonGroup.addButton(self.emptyCellImageRadio, 2)

        # Default to showing difference image if blank is available, otherwise original
        if self.blank_image_info is not None:
            self.differenceImageRadio.setChecked(True)
        else:
            self.originalImageRadio.setChecked(True)
            self.differenceImageRadio.setEnabled(False)
            self.emptyCellImageRadio.setEnabled(False)

        self.compareLayout = QVBoxLayout(self.compareGroup)
        self.compareLayout.addWidget(self.differenceImageRadio)
        self.compareLayout.addWidget(self.originalImageRadio)
        self.compareLayout.addWidget(self.emptyCellImageRadio)

        self.updateBlankWidgets()

        self.dialogButtons = QDialogButtonBox(
            QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self
        )
        okButton = self.dialogButtons.button(QDialogButtonBox.Ok)
        okButton.setText("Save")

        self.dialogButtons.accepted.connect(self.okClicked)
        self.dialogButtons.rejected.connect(self.reject)

        self.settingsWidget = QWidget()
        self.settingsLayout = QVBoxLayout(self.settingsWidget)
        # DisplayOptionsPanel sits at the top of the right column.
        self.settingsLayout.addWidget(self.imageViewer.display_panel)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.blankSelectionGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.compareGroup)
        self.settingsLayout.addStretch()

        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.scrollArea.setWidget(self.settingsWidget)

        self.mainLayout = QVBoxLayout(self)
        self.contentLayout = QHBoxLayout()
        self.contentLayout.addWidget(self.imageWidget)
        self.contentLayout.addWidget(self.scrollArea)

        self.mainLayout.addLayout(self.contentLayout)
        self.mainLayout.addWidget(self.dialogButtons, alignment=Qt.AlignHCenter)

        self.refreshImage()

        self.setConnections()

        # Automatically resize dialog to fit all widgets
        self.adjustSize()

    def setConnections(self):
        self.selectBlankBtn.clicked.connect(self.readBlankImage)
        self.blankWeightText.valueChanged.connect(self.updateBlankWeight)
        self.displayButtonGroup.buttonClicked.connect(self.onDisplayOptionChanged)

    def onDisplayOptionChanged(self):
        self.refreshImage()

    def okClicked(self):
        self.saveBlankConfig()
        self.accept()

    def saveBlankConfig(self):
        # Save config if blank image is available
        if self.blank_image_info is not None:
            blank_config = {
                "file_path": str(self.blank_image_info["file_path"]),
                "weight": self.blank_image_info["weight"],
            }

            self.blank_config_file_path.parent.mkdir(parents=True, exist_ok=True)

            with open(self.blank_config_file_path, "w") as file_stream:
                json.dump(blank_config, file_stream, indent=4)
        else:
            # Remove config file if no blank image
            self.blank_config_file_path.unlink(missing_ok=True)

    def updateBlankWeight(self, blank_image_weight):
        if self.blank_image_info is not None:
            self.blank_image_info["weight"] = blank_image_weight
            self.refreshImage()

    def updateBlankWidgets(self):
        if self.blank_image_info is not None:
            self.blankStatusText.setText("✓ Empty cell image loaded")
            self.blankStatusText.setStyleSheet("color: green;")
            self.blankWeightLabel.setEnabled(True)
            self.blankWeightText.setEnabled(True)
            self.blankWeightText.setValue(self.blank_image_info.get("weight", 1.0))

            # Enable display options that require blank image
            self.differenceImageRadio.setEnabled(True)
            self.emptyCellImageRadio.setEnabled(True)

            # Set default to difference image
            self.differenceImageRadio.setChecked(True)
        else:
            self.blankStatusText.setText("No empty cell image loaded")
            self.blankStatusText.setStyleSheet("color: red;")
            self.blankWeightLabel.setEnabled(False)
            self.blankWeightText.setEnabled(False)

            # Disable display options that require blank image
            self.differenceImageRadio.setEnabled(False)
            self.emptyCellImageRadio.setEnabled(False)

            # Set default to original image
            self.originalImageRadio.setChecked(True)

    def refreshImage(self):
        # The viewer keeps current vmin/vmax/log_scale/colormap and the current
        # zoom across display_image() calls, so simply selecting a new array
        # here will not reset the user's display parameters.
        if self.imageData is None:
            self.statusBar.setText("Current View: No Display")
            return

        if self.differenceImageRadio.isChecked():
            if self.blank_image_info is None:
                self.statusBar.setText("Current View: No blank image available")
                return
            blank_image_weight = self.blank_image_info["weight"]
            imageData = (
                self.imageData
                - self.blank_image_info["blank_image"] * blank_image_weight
            )
            status = f"Current View: Difference Image (scale: {blank_image_weight:.2f})"
        elif self.originalImageRadio.isChecked():
            imageData = self.imageData
            status = "Current View: Original Image"
        elif self.emptyCellImageRadio.isChecked():
            if self.blank_image_info is None:
                self.statusBar.setText("Current View: No blank image available")
                return
            blank_image_weight = self.blank_image_info["weight"]
            imageData = self.blank_image_info["blank_image"] * blank_image_weight
            status = f"Current View: Empty Cell Image (scale: {blank_image_weight:.2f})"
        else:
            self.statusBar.setText("Current View: No Display")
            return

        self.imageViewer.display_image(imageData)
        self.statusBar.setText(status)

    def readBlankImage(self):
        blank_image_file_path = getAFile(path=str(self.settings_dir_path.parent))
        blank_image_file_path = Path(blank_image_file_path)

        if (not blank_image_file_path) or not blank_image_file_path.exists():
            return

        blank_image = self.read_image_data(blank_image_file_path)
        blank_image_weight = 1.0

        blank_image_info = {
            "file_path": str(blank_image_file_path),
            "blank_image": blank_image,
            "weight": blank_image_weight,
        }

        self.blank_image_info = blank_image_info

        self.updateBlankWidgets()

        self.refreshImage()

    def read_image_data(self, file_path):
        if not file_path.exists():
            return None

        image_data = fabio.open(file_path).data
        return image_data

    def read_blank_image_info(self, blank_config_file_path):
        if not blank_config_file_path.exists():
            return None

        with open(blank_config_file_path, "r") as file_stream:
            blank_config = json.load(file_stream)

        if not isinstance(blank_config, dict):
            return None

        blank_image_file_path = blank_config.get("file_path")

        if not blank_image_file_path:
            return None

        blank_image_file_path = Path(blank_image_file_path)

        if not blank_image_file_path.exists():
            return None

        blank_image = self.read_image_data(blank_image_file_path)
        blank_image_weight = blank_config.get("weight", 1.0)

        blank_image_info = {
            "file_path": str(blank_image_file_path),
            "blank_image": blank_image,
            "weight": blank_image_weight,
        }

        return blank_image_info
