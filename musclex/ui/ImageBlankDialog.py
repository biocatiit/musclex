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

        self.blank_config_file_path = self.image_file_path.parent / f"blank_image_settings.json"
        self.imageData = self.read_image_data(self.image_file_path)

        blank_image_info = self.read_blank_image_info(self.blank_config_file_path)

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

        # Display Options Group (Bottom) - 3 exclusive radio buttons
        self.displayGroup = QGroupBox("Display Options")
        self.displayButtonGroup = QButtonGroup()
        
        self.differenceImageRadio = QRadioButton("Difference Image (Original - Empty Cell)")
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

        self.displayLayout = QVBoxLayout(self.displayGroup)
        self.displayLayout.addWidget(self.differenceImageRadio)
        self.displayLayout.addWidget(self.originalImageRadio)
        self.displayLayout.addWidget(self.emptyCellImageRadio)

        self.updateBlankWidgets()

        self.dialogButtons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        okButton = self.dialogButtons.button(QDialogButtonBox.Ok)
        okButton.setText("Save")

        self.dialogButtons.accepted.connect(self.okClicked)
        self.dialogButtons.rejected.connect(self.reject)

        self.settingsWidget = QWidget()
        self.settingsLayout = QVBoxLayout(self.settingsWidget)
        self.settingsLayout.addWidget(self.blankSelectionGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.displayGroup)
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

        if self.imageData is not None:
            scaledPixmap = self.createDisplayImage(self.imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)

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
        if self.imageData is None:
            self.imageLabel.clear()
            self.statusBar.setText("Current View: No Display")
            return

        # Determine which radio button is selected
        if self.differenceImageRadio.isChecked():
            # Show difference image (Original - Empty Cell)
            if self.blank_image_info is None:
                self.imageLabel.clear()
                self.statusBar.setText("Current View: No blank image available")
                return
            
            blank_image_weight = self.blank_image_info["weight"]
            imageData = self.imageData.copy() - self.blank_image_info["blank_image"] * blank_image_weight
            
            scaledPixmap = self.createDisplayImage(imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)
            self.statusBar.setText(f"Current View: Difference Image (scale: {blank_image_weight:.2f})")
            
        elif self.originalImageRadio.isChecked():
            # Show original image
            imageData = self.imageData.copy()
            
            scaledPixmap = self.createDisplayImage(imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)
            self.statusBar.setText("Current View: Original Image")
            
        elif self.emptyCellImageRadio.isChecked():
            # Show empty cell image
            if self.blank_image_info is None:
                self.imageLabel.clear()
                self.statusBar.setText("Current View: No blank image available")
                return
            
            blank_image_weight = self.blank_image_info["weight"]
            imageData = self.blank_image_info["blank_image"] * blank_image_weight
            
            scaledPixmap = self.createDisplayImage(imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)
            self.statusBar.setText(f"Current View: Empty Cell Image (scale: {blank_image_weight:.2f})")
            
        else:
            # Fallback - should not happen
            self.imageLabel.clear()
            self.statusBar.setText("Current View: No Display")

    def readBlankImage(self):
        blank_image_file_path = getAFile(path=str(self.image_file_path.parent))
        blank_image_file_path = Path(blank_image_file_path)

        if (not blank_image_file_path) or not blank_image_file_path.exists():
            return

        blank_image = self.read_image_data(blank_image_file_path)
        blank_image_weight = 1.0

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
            # Clip values to [0, 255] range before converting to uint8 to avoid underflow/overflow
            normFlippedImageArray = np.clip(normFlippedImageArray, 0, 255).astype(np.uint8)

        # Create a QImage from the 8-bit array
        qImg = QImage(normFlippedImageArray.data, normFlippedImageArray.shape[1], normFlippedImageArray.shape[0], normFlippedImageArray.strides[0], QImage.Format_Grayscale8)

        # Convert QImage to QPixmap
        pixmap = QPixmap.fromImage(qImg)

        # Scale the pixmap to fit within the display area while maintaining the aspect ratio
        scaledPixmap = pixmap.scaled(
            displayImageWidth, displayImageHeight,
            Qt.KeepAspectRatio, Qt.SmoothTransformation)

        return scaledPixmap
