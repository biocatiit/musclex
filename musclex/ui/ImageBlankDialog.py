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
    QDoubleSpinBox,
    QComboBox,
    QDialogButtonBox,
    QGridLayout,
    QHBoxLayout,
    QWidget,
    QVBoxLayout,
    QScrollArea,
)

from PySide6.QtGui import QImage, QPixmap, QFont
from PySide6.QtCore import Qt

import fabio

from .pyqt_utils import getAFile


class ImageBlankDialog(QDialog):
    def __init__(self,
                image_file_path,
                vmin,
                vmax
        ):
        super().__init__()
        self.setModal(True)
        self.setWindowTitle("Empty Cell Subraction")
        self.image_file_path = Path(image_file_path)
        self.vmin = vmin
        self.vmax = vmax

        self.blank_settings_file_path = self.image_file_path.parent / f"blank_image_settings.json"
        self.imageData = self.read_image_data(self.image_file_path)

        blank_image_info = self.read_blank_image_info(self.blank_settings_file_path)

        if ((blank_image_info is not None)
            and (blank_image_info.get("blank_image") is not None)
            and blank_image_info["blank_image"].shape == self.imageData.shape):
            self.blank_image_info = blank_image_info
        else:
            self.blank_image_info = None

        # Show image.
        self.imageLabel = QLabel()
        # Set fixed (width, height)
        self.imageLabel.setMinimumSize(800, 600)
        # Optional: Set a border to visualize the area if you like
        # self.imageLabel.setStyleSheet("border: 1px solid black;")
        self.imageLabel.setAlignment(Qt.AlignCenter)  # Center-align the image

        self.statusBar = QLabel(f"Current View: No Display")
        self.statusBar.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)

        self.imageWidget = QWidget()
        self.imageLayout = QVBoxLayout(self.imageWidget)
        self.imageLayout.addWidget(self.imageLabel)
        self.imageLayout.addWidget(self.statusBar)

        self.displayGroup = QGroupBox("Display Options")

        self.showImageCheckBox = QCheckBox("Show Image")

        if self.imageData is not None:
            self.showImageCheckBox.setCheckState(Qt.CheckState.Checked)
        else:
            self.showImageCheckBox.setEnabled(False)

        self.showImageCheckBox.setToolTip("Uncheck to show empty cell only")

        self.displayLayout = QVBoxLayout(self.displayGroup)
        self.displayLayout.addWidget(self.showImageCheckBox)

        self.applyBlankGroup = QGroupBox("Empty Cell Image Options")
        self.applyBlankCheckBox = QCheckBox("Apply Empty Cell Image")
        self.applyBlankText = QLabel()

        self.selectBlankBtn = QPushButton("Select Empty Cell Image")
        self.blankWeightLabel = QLabel("Empty Cell Image Scale: ")
        self.blankWeightText = QDoubleSpinBox()
        self.blankWeightText.setKeyboardTracking(False)
        self.blankWeightText.setRange(0, 1000)
        self.blankWeightText.setValue(1.00)
        self.blankWeightText.setSingleStep(0.01)

        self.updateBlankWidgets()

        self.applyBlankLayout = QGridLayout(self.applyBlankGroup)
        settingsRowIndex = 0
        self.applyBlankLayout.addWidget(self.applyBlankCheckBox, settingsRowIndex, 0, 1, 2)
        self.applyBlankLayout.addWidget(self.applyBlankText, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        self.applyBlankLayout.addWidget(self.selectBlankBtn, settingsRowIndex, 0, 1, 4)
        settingsRowIndex += 1
        self.applyBlankLayout.addWidget(self.blankWeightLabel, settingsRowIndex, 0, 1, 2)
        self.applyBlankLayout.addWidget(self.blankWeightText, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1

        self.dialogButtons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        okButton = self.dialogButtons.button(QDialogButtonBox.Ok)
        okButton.setText("Save")

        self.dialogButtons.accepted.connect(self.okClicked)
        self.dialogButtons.rejected.connect(self.reject)

        self.buttonLayout = QGridLayout()

        settingsRowIndex = 0
        self.buttonLayout.addWidget(self.dialogButtons, settingsRowIndex, 0, 1, 4)
        settingsRowIndex += 1

        self.buttonWidget = QWidget()
        self.buttonWidget.setLayout(self.buttonLayout)

        self.settingsWidget = QWidget()
        self.settingsLayout = QVBoxLayout(self.settingsWidget)
        self.settingsLayout.addWidget(self.displayGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.applyBlankGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.buttonWidget)
        self.settingsLayout.addStretch()

        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.scrollArea.setWidget(self.settingsWidget)

        self.mainLayout = QHBoxLayout(self)
        self.mainLayout.addWidget(self.imageWidget)
        self.mainLayout.addWidget(self.scrollArea)

        if self.imageData is not None:
            scaledPixmap = self.createDisplayImage(self.imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)

        self.refreshImage()

        self.setConnections()

    def setConnections(self):
        self.showImageCheckBox.checkStateChanged.connect(self.enableShowImage)
        self.applyBlankCheckBox.checkStateChanged.connect(self.enableBlankSubtraction)
        self.selectBlankBtn.clicked.connect(self.readBlankImage)
        self.blankWeightText.valueChanged.connect(self.updateBlankWeight)

    def enableShowImage(self, state):
        self.refreshImage()

    def okClicked(self):
        pass

    def enableBlankSubtraction(self, state):
        if state == Qt.CheckState.Checked:
            self.blankWeightLabel.setEnabled(True)
            self.blankWeightText.setEnabled(True)

        if state == Qt.CheckState.Unchecked:
            self.blankWeightLabel.setEnabled(False)
            self.blankWeightText.setEnabled(False)

        self.refreshImage()

    def updateBlankWeight(self, blank_image_weight):
        self.blank_image_info["weight"] = blank_image_weight

        blank_settings = {
            "file_path": str(self.blank_image_info["file_path"]),
            "weight": blank_image_weight,
        }

        with open(self.blank_settings_file_path, "w") as file_stream:
            json.dump(blank_settings, file_stream, indent=4)

        self.refreshImage()

    def updateBlankWidgets(self):
        if self.blank_image_info is not None:
            self.applyBlankCheckBox.setChecked(True)
            self.applyBlankCheckBox.setEnabled(True)
            self.applyBlankText.setText("Empty cell image is available.")
            self.applyBlankText.setStyleSheet("color: green;")
            self.blankWeightLabel.setEnabled(True)
        else:
            self.applyBlankCheckBox.setChecked(False)
            self.applyBlankCheckBox.setEnabled(False)
            self.applyBlankText.setText("No empty cell image found. Ignore this message if expected.")
            self.applyBlankText.setStyleSheet("color: red;")
            self.blankWeightLabel.setEnabled(False)

    def refreshImage(self):
        isShowImage = (self.showImageCheckBox.isEnabled()
            and self.showImageCheckBox.isChecked())

        isApplyBlank = (self.applyBlankCheckBox.isEnabled()
            and self.applyBlankCheckBox.isChecked())

        # If user does not select showing image:
        if not isShowImage:
            # If nothing is selected, Show nothing.
            if not isApplyBlank:
                self.imageLabel.clear()
                self.statusBar.setText(f"Current View: No Display")
                return

            # Only show empty cell.
            blank_image_weight = self.blank_image_info["weight"]
            imageData = self.blank_image_info["blank_image"] * blank_image_weight
            scaledPixmap = self.createDisplayImage(imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)
            self.statusBar.setText(f"Current View: Empty Cell (with scale: {blank_image_weight:.2f})")
            return

        # User selected showing image:
        imageData = self.imageData.copy()

        # Only show image.
        if not isApplyBlank:
            scaledPixmap = self.createDisplayImage(imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)
            self.statusBar.setText(f"Current View: Original Image")
            return

        # Show image + subtract blank.
        blank_image_weight = self.blank_image_info["weight"]
        imageData = imageData - self.blank_image_info["blank_image"] * blank_image_weight

        scaledPixmap = self.createDisplayImage(imageData,
            self.vmin,
            self.vmax,
            self.imageLabel.width(),
            self.imageLabel.height())
        self.imageLabel.setPixmap(scaledPixmap)
        self.statusBar.setText(f"Current View: Original Image + Empty Cell subtraction (with scale: {blank_image_weight:.2f})")

    def readBlankImage(self):
        blank_image_file_path = getAFile(path=str(self.image_file_path.parent))
        blank_image_file_path = Path(blank_image_file_path)

        if (not blank_image_file_path) or not blank_image_file_path.exists():
            return

        blank_image = self.read_image_data(blank_image_file_path)
        blank_image_weight = 1.0

        blank_settings = {
            "file_path": str(blank_image_file_path),
            "weight": blank_image_weight,
        }

        with open(self.blank_settings_file_path, "w") as file_stream:
            json.dump(blank_settings, file_stream, indent=4)

        blank_image_info = {
            "file_path": str(blank_image_file_path),
            "blank_image": blank_image,
            "weight": blank_image_weight
        }

        self.blank_image_info = blank_image_info

        self.updateBlankWidgets()

        self.refreshImage()

    def read_image_data(self, file_path):
        if not file_path.exists():
            return None

        image_data = fabio.open(file_path).data
        return image_data

    def read_blank_image_info(self, blank_settings_file_path):
        if not blank_settings_file_path.exists():
            return None

        with open(blank_settings_file_path, "r") as file_stream:
            blank_settings = json.load(file_stream)

        if not isinstance(blank_settings, dict):
            return None

        blank_image_file_path = blank_settings.get("file_path")

        if not blank_image_file_path:
            return None

        blank_image_file_path = Path(blank_image_file_path)

        if not blank_image_file_path.exists():
            return None

        blank_image = self.read_image_data(blank_image_file_path)
        blank_image_weight = blank_settings.get("weight", 1.0)

        blank_image_info = {
            "file_path": str(blank_image_file_path),
            "blank_image": blank_image,
            "weight": blank_image_weight
        }

        return blank_image_info

    def createDisplayImage(self,
        imageArray,
        minInt,
        maxInt,
        displayImageWidth,
        displayImageHeight):
        imageArray = imageArray.copy()
        # Flip the image vertically (up-down) so it matches the display
        # in the main window, where the y-axis is defined bottom-to-top
        # using ax.set_ylim.
        flippedImageArray = np.flipud(imageArray)

        # Normalize the flipped image to the 0-255 range for display
        if np.max(flippedImageArray) == np.min(flippedImageArray):
            normFlippedImageArray = np.full(flippedImageArray.shape, 128, dtype=np.uint8)
        else:
            # If minInt == maxInt, just use the images min and max (perhaps need to change since we are performing operations)
            if minInt == maxInt:
                normFlippedImageArray = 255 * (flippedImageArray - np.min(flippedImageArray)) / (np.max(flippedImageArray) - np.min(flippedImageArray))
            else:
                normFlippedImageArray = 255 * (np.array(flippedImageArray) - minInt) / (maxInt - minInt)
            normFlippedImageArray = normFlippedImageArray.astype(np.uint8)  # Convert to 8-bit

        # Create a QImage from the 8-bit array
        qImg = QImage(normFlippedImageArray.data, normFlippedImageArray.shape[1], normFlippedImageArray.shape[0], normFlippedImageArray.strides[0], QImage.Format_Grayscale8)

        # Convert QImage to QPixmap
        pixmap = QPixmap.fromImage(qImg)

        # Scale the pixmap to fit within the display area while maintaining the aspect ratio
        scaledPixmap = pixmap.scaled(
            displayImageWidth, displayImageHeight,
            Qt.KeepAspectRatio, Qt.SmoothTransformation)

        return scaledPixmap
