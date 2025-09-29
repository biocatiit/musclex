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
import cv2
import threading

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


class ImageMaskDialog(QDialog):
    def __init__(self,
                image_file_path,
                vmin,
                vmax
        ):
        super().__init__()
        self.setModal(True)
        self.setWindowTitle("Set Image Mask")
        self.image_file_path = Path(image_file_path)
        self.vmin = vmin
        self.vmax = vmax

        # The drawn mask file name should be <originalFileNoExt>-mask.edf
        self.drawn_mask_file_path = self.image_file_path.parent / f"{self.image_file_path.stem}-mask.edf"
        self.blank_settings_file_path = self.image_file_path.parent / f"blank_image_settings.json"

        self.imageData = self.read_image_data(self.image_file_path)
        drawnMaskData = self.read_image_data(self.drawn_mask_file_path)

        if (drawnMaskData is not None) and drawnMaskData.shape == self.imageData.shape:
            self.drawnMaskData = 1 - drawnMaskData
        else:
            self.drawnMaskData = None

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

        self.displayDescGroup = QGroupBox("Current View")
        self.displayDescLayout = QVBoxLayout(self.displayDescGroup)
        self.displayDescLabel = QLabel()
        self.displayDescLayout.addWidget(self.displayDescLabel)

        self.displayGroup = QGroupBox("Display Options")

        self.showImageCheckBox = QCheckBox("Show Image")

        if self.imageData is not None:
            self.showImageCheckBox.setCheckState(Qt.CheckState.Checked)
        else:
            self.showImageCheckBox.setEnabled(False)

        self.showImageCheckBox.setToolTip("Uncheck to show mask only")

        self.displayLayout = QVBoxLayout(self.displayGroup)
        self.displayLayout.addWidget(self.showImageCheckBox)

        self.applyBlankGroup = QGroupBox("Empty Cell Image Options")
        self.applyBlankCheckBox = QCheckBox("Apply Empty Cell Image")
        self.applyBlankText = QLabel()

        # self.selectBlankBtn = QPushButton("Select Empty Cell Image")
        self.blankWeightLabel = QLabel("Empty Cell Image Scale: ")
        # self.blankWeightText = QDoubleSpinBox()
        # self.blankWeightText.setKeyboardTracking(False)
        # self.blankWeightText.setRange(0, 1000)
        # self.blankWeightText.setValue(1.00)
        # self.blankWeightText.setSingleStep(0.01)

        self.updateBlankWidgets()

        self.applyBlankLayout = QGridLayout(self.applyBlankGroup)
        settingsRowIndex = 0
        self.applyBlankLayout.addWidget(self.applyBlankCheckBox, settingsRowIndex, 0, 1, 2)
        self.applyBlankLayout.addWidget(self.applyBlankText, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        # self.applyBlankLayout.addWidget(self.selectBlankBtn, settingsRowIndex, 0, 1, 4)
        # settingsRowIndex += 1
        self.applyBlankLayout.addWidget(self.blankWeightLabel, settingsRowIndex, 0, 1, 4)
        # self.applyBlankLayout.addWidget(self.blankWeightLabel, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1

        self.applyMaskGroup = QGroupBox("Mask Options")
        self.applyMaskGroup.setToolTip(
            "The selected mask options will be saved to a file and"
            + " applied to the image when clicking the Save button.")

        self.applyDrawnMaskCheckBox = QCheckBox("Apply Drawn Mask")
        self.applyDrawnMaskText = QLabel()

        self.applyLowMaskCheckBox = QCheckBox("Apply Low Mask Threshold")
        self.applyHighMaskCheckBox= QCheckBox("Apply High Mask Threshold")

        self.applyMaskLayout = QGridLayout(self.applyMaskGroup)
        settingsRowIndex = 0
        self.applyMaskLayout.addWidget(self.applyDrawnMaskCheckBox, settingsRowIndex, 0, 1, 2)
        self.applyMaskLayout.addWidget(self.applyDrawnMaskText, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        self.applyMaskLayout.addWidget(self.applyLowMaskCheckBox, settingsRowIndex, 0, 1, 2)
        settingsRowIndex += 1
        self.applyMaskLayout.addWidget(self.applyHighMaskCheckBox, settingsRowIndex, 0, 1, 2)
        settingsRowIndex += 1

        self.drawMaskGroup = QGroupBox("Drawn Mask with pyFAI")
        self.drawMaskLayout = QVBoxLayout(self.drawMaskGroup)

        if self.applyBlankCheckBox.isChecked():
            draw_mask_text = "Draw Mask on Empty Cell Subtracted Image"
        else:
            draw_mask_text = "Draw Mask on Original Image"

        self.drawMaskBtn = QPushButton(draw_mask_text)
        self.drawMaskLayout.addWidget(self.drawMaskBtn)

        self.maskLowThresGroup = QGroupBox("Low Mask Threshold Settings")
        self.maskLowThresLayout = QGridLayout(self.maskLowThresGroup)

        self.maskLowThreshChkbx = QCheckBox("Low Mask Threshold")

        self.maskLowThresh = QDoubleSpinBox()
        self.maskLowThresh.setMinimum(-50)
        self.maskLowThresh.setMaximum(10000)
        self.maskLowThresh.setValue(-1)
        self.maskLowThresh.setSingleStep(0.01)
        self.maskLowThresh.setEnabled(False)
        self.maskLowThresh.valueChanged.connect(self.maskLowThresholdChanged)

        self.lowMaskDilationChkbx = QCheckBox("Enable Mask Dilation")
        self.lowMaskDilationChkbx.setEnabled(False)

        self.lowDilComboBox = QComboBox()
        self.lowDilComboBox.addItem("3x3 Kernel")
        self.lowDilComboBox.addItem("5x5 Kernel")
        self.lowDilComboBox.addItem("7x7 Kernel")
        self.lowDilComboBox.setCurrentIndex(0)
        self.lowDilComboBox.setEnabled(False)
        self.lowDilComboBox.currentIndexChanged.connect(self.lowDilComboBoxChanged)

        settingsRowIndex = 0
        self.maskLowThresLayout.addWidget(self.maskLowThreshChkbx, settingsRowIndex, 0, 1, 2)
        self.maskLowThresLayout.addWidget(self.maskLowThresh, settingsRowIndex, 3, 1, 2)
        settingsRowIndex += 1
        self.maskLowThresLayout.addWidget(self.lowMaskDilationChkbx, settingsRowIndex, 0, 1, 2)
        self.maskLowThresLayout.addWidget(self.lowDilComboBox, settingsRowIndex, 3, 1, 1)
        settingsRowIndex += 1

        self.maskHighThresGroup = QGroupBox("High Mask Threshold Settings")
        self.maskHighThresLayout = QGridLayout(self.maskHighThresGroup)

        self.maskHighThreshChkbx = QCheckBox("High Mask Threshold")

        self.maskHighThresh = QDoubleSpinBox()
        self.maskHighThresh.setMinimum(0)
        self.maskHighThresh.setMaximum(self.imageData.max() + 1.0)
        self.maskHighThresh.setValue(self.imageData.max() + 1.0)
        self.maskHighThresh.setSingleStep(0.01)
        self.maskHighThresh.setEnabled(False)

        self.maskHighThresh.valueChanged.connect(self.maskHighThresholdChanged)

        #Enable or disable dilation for upper bound mask
        self.highMaskDilationChkbx = QCheckBox("Enable Mask Dilation")
        self.highMaskDilationChkbx.setEnabled(False)

        #Choose the kernel size for dilation (upper bound threshold)
        self.highDilComboBox = QComboBox()
        self.highDilComboBox.addItem("3x3 Kernel")
        self.highDilComboBox.addItem("5x5 Kernel")
        self.highDilComboBox.addItem("7x7 Kernel")
        self.highDilComboBox.setCurrentIndex(0)
        self.highDilComboBox.setEnabled(False)
        self.highDilComboBox.currentIndexChanged.connect(self.highDilComboBoxChanged)

        settingsRowIndex = 0
        self.maskHighThresLayout.addWidget(self.maskHighThreshChkbx, settingsRowIndex, 0, 1, 2)
        self.maskHighThresLayout.addWidget(self.maskHighThresh, settingsRowIndex, 3, 1, 2)
        settingsRowIndex += 1
        self.maskHighThresLayout.addWidget(self.highMaskDilationChkbx, settingsRowIndex, 0, 1, 2)
        self.maskHighThresLayout.addWidget(self.highDilComboBox, settingsRowIndex, 3, 1, 1)
        settingsRowIndex += 1

        self.greenLabel = QLabel("Green: Low Mask Threshold")
        self.greenLabel.setStyleSheet("color: green")
        self.blueLabel = QLabel("Blue: High Mask Threshold")
        self.blueLabel.setStyleSheet("color: blue")
        self.redLabel = QLabel("Red: Drawn Mask")
        self.redLabel.setStyleSheet("color: red")
        self.purpleLabel = QLabel("Purple: Rmin / Rmax mask")
        self.purpleLabel.setStyleSheet("color: purple")

        self.negativeValuesLabel = QLabel("Negative Values Detected in Image: ")
        font = QFont()
        font.setBold(True)
        self.negativeValuesLabel.setFont(font)
        self.negativeValuesLabel.setVisible(False)

        # self.clampNegativeValuesChkbx = QCheckBox("Clamp Negative Values to 0")
        # self.clampNegativeValuesChkbx.setToolTip("Sets all negative values after subtraction to 0")
        # self.clampNegativeValuesChkbx.setEnabled(False)

        self.dialogButtons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        okButton = self.dialogButtons.button(QDialogButtonBox.Ok)
        okButton.setText("Save")

        self.dialogButtons.accepted.connect(self.okClicked)
        self.dialogButtons.rejected.connect(self.reject)

        self.buttonLayout = QGridLayout()

        settingsRowIndex = 0
        self.buttonLayout.addWidget(self.greenLabel, settingsRowIndex, 0, 1, 2)
        self.buttonLayout.addWidget(self.blueLabel, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        self.buttonLayout.addWidget(self.redLabel, settingsRowIndex, 0, 1, 2)
        self.buttonLayout.addWidget(self.purpleLabel, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        self.buttonLayout.addWidget(self.negativeValuesLabel, settingsRowIndex, 0, 1, 4)
        settingsRowIndex += 1
        # self.buttonLayout.addWidget(self.clampNegativeValuesChkbx, settingsRowIndex, 0, 1, 2)
        # settingsRowIndex += 1
        self.buttonLayout.addWidget(self.dialogButtons, settingsRowIndex, 1, 1, 2)
        settingsRowIndex += 1

        self.buttonWidget = QWidget()
        self.buttonWidget.setLayout(self.buttonLayout)

        self.settingsWidget = QWidget()
        self.settingsLayout = QVBoxLayout(self.settingsWidget)
        self.settingsLayout.addWidget(self.displayDescGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.displayGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.applyBlankGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.applyMaskGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.drawMaskGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.maskLowThresGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.maskHighThresGroup)
        self.settingsLayout.addSpacing(10)
        self.settingsLayout.addWidget(self.buttonWidget)

        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.scrollArea.setWidget(self.settingsWidget)

        self.mainLayout = QHBoxLayout(self)
        self.mainLayout.addWidget(self.imageLabel)
        self.mainLayout.addWidget(self.scrollArea)

        if self.imageData is not None:
            scaledPixmap = self.createDisplayImage(self.imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)

        self.updateDrawnMaskWidgets()
        self.refreshImage()

        self.setConnections()

    def setConnections(self):
        self.showImageCheckBox.checkStateChanged.connect(self.enableShowImage)
        self.applyBlankCheckBox.checkStateChanged.connect(self.enableBlankSubtraction)
        # self.selectBlankBtn.clicked.connect(self.readBlankImage)
        # self.blankWeightText.valueChanged.connect(self.updateBlankWeight)
        self.drawMaskBtn.clicked.connect(self.drawMask)
        self.applyDrawnMaskCheckBox.checkStateChanged.connect(self.applyDrawnMask)
        self.applyLowMaskCheckBox.checkStateChanged.connect(self.enableLowMaskThresh)
        self.maskLowThreshChkbx.checkStateChanged.connect(self.enableLowMaskThresh)

        self.applyHighMaskCheckBox.checkStateChanged.connect(self.enableHighMaskThresh)
        self.maskHighThreshChkbx.checkStateChanged.connect(self.enableHighMaskThresh)

        self.lowMaskDilationChkbx.checkStateChanged.connect(self.enableLowMaskDilation)

        self.highMaskDilationChkbx.checkStateChanged.connect(self.enableHighMaskDilation)

    def enableShowImage(self, state):
        self.refreshImage()

    def enableBlankSubtraction(self, state):
        if state == Qt.CheckState.Checked:
            draw_mask_text = "Draw Mask on Empty Cell Subtracted Image"
            self.drawMaskBtn.setText(draw_mask_text)
            self.blankWeightLabel.setEnabled(True)

        if state == Qt.CheckState.Unchecked:
            draw_mask_text = "Draw Mask on Original Image"
            self.drawMaskBtn.setText(draw_mask_text)
            self.blankWeightLabel.setEnabled(False)

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
            self.applyBlankText.setText("Empty cell image not found!")
            self.applyBlankText.setStyleSheet("color: red;")
            self.blankWeightLabel.setEnabled(False)

    def updateDrawnMaskWidgets(self):
        if self.drawnMaskData is not None:
            self.applyDrawnMaskCheckBox.setChecked(True)
            self.applyDrawnMaskCheckBox.setEnabled(True)
            self.applyDrawnMaskText.setText("Drawn mask image is available.")
            self.applyDrawnMaskText.setStyleSheet("color: green;")
        else:
            self.applyDrawnMaskCheckBox.setChecked(False)
            self.applyDrawnMaskCheckBox.setEnabled(False)
            self.applyDrawnMaskText.setText("Drawn mask image not found!")
            self.applyDrawnMaskText.setStyleSheet("color: red;")

    def applyDrawnMask(self, state):
        self.refreshImage()

    def enableLowMaskThresh(self, state):
        self.applyLowMaskCheckBox.setCheckState(state)
        self.maskLowThreshChkbx.setCheckState(state)

        if state == Qt.CheckState.Checked:
            self.maskLowThresh.setEnabled(True)
            self.lowMaskDilationChkbx.setEnabled(True)

            if self.lowMaskDilationChkbx.isChecked():
                self.lowDilComboBox.setEnabled(True)

        if state == Qt.CheckState.Unchecked:
            self.maskLowThresh.setEnabled(False)
            self.lowMaskDilationChkbx.setEnabled(False)
            self.lowDilComboBox.setEnabled(False)

        self.refreshImage()

    def maskLowThresholdChanged(self):
        self.refreshImage()

    def enableLowMaskDilation(self, state):
        if state == Qt.CheckState.Checked:
            self.lowDilComboBox.setEnabled(True)

        if state == Qt.CheckState.Unchecked:
            self.lowDilComboBox.setEnabled(False)

        self.refreshImage()

    def lowDilComboBoxChanged(self):
        self.refreshImage()

    def enableHighMaskThresh(self, state):
        self.applyHighMaskCheckBox.setCheckState(state)
        self.maskHighThreshChkbx.setCheckState(state)

        if state == Qt.CheckState.Checked:
            self.maskHighThresh.setEnabled(True)
            self.highMaskDilationChkbx.setEnabled(True)

            if self.highMaskDilationChkbx.isChecked():
                self.highDilComboBox.setEnabled(True)

        if state == Qt.CheckState.Unchecked:
            self.maskHighThresh.setEnabled(False)
            self.highMaskDilationChkbx.setEnabled(False)
            self.highDilComboBox.setEnabled(False)

        self.refreshImage()

    def maskHighThresholdChanged(self):
        self.refreshImage()

    def enableHighMaskDilation(self, state):
        if state == Qt.CheckState.Checked:
            self.highDilComboBox.setEnabled(True)

        if state == Qt.CheckState.Unchecked:
            self.highDilComboBox.setEnabled(False)

        self.refreshImage()

    def highDilComboBoxChanged(self):
        self.refreshImage()

    def okClicked(self):
        pass

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

    def drawMask(self):
        if self.image_file_path.exists():
            # Assuming pyFAI-drawmask can be called directly from the command line
            thread = threading.Thread(target=self._run_drawmask_command_and_refresh)
            thread.start()
        else:
            print("No input file to draw on.")

    def _run_drawmask_command_and_refresh(self):
        try:
            # Run the command
            self._run_drawmask_command()
        except Exception as e:
            print("Exception occurred:", e)
            tb_str = traceback.format_exc()
            print(f"Full traceback: {tb_str}\n")


        drawnMaskData = self.read_image_data(self.drawn_mask_file_path)

        if np.array_equal(drawnMaskData, self.drawnMaskData):
            return

        if (drawnMaskData is not None) and drawnMaskData.shape == self.imageData.shape:
            self.drawnMaskData = 1 - drawnMaskData
        else:
            self.drawnMaskData = None

        self.updateDrawnMaskWidgets()
        self.refreshImage()

    def _run_drawmask_command(self):
        """
        Run the pyFAI-drawmask command in a way that:
        1) Applies intensity limits to produce a 'bounded' TIFF,
        2) Calls pyFAI-drawmask on that TIFF,
        3) Renames the resulting mask file so it matches the old naming convention: <original>.tif -> <original>-mask.edf
        4) Removes the temporary bounded TIFF.
        """

        # Write a bounded TIFF, so we don't overwrite original
        bounded_file_path = self.image_file_path.parent / f"{self.image_file_path.stem}_bounded.tif"

        fabio.tifimage.tifimage(data=self.imageData).write(bounded_file_path)

        # Run pyFAI-drawmask on the bounded TIFF
        command = f'pyFAI-drawmask "{bounded_file_path}"'

        # pyFAI will produce something like <bounded_file_nameNoExt>-mask.edf
        # e.g. "..._bounded-mask.edf"
        generated_mask_file_path = bounded_file_path.parent / f"{bounded_file_path.stem}-mask.edf"

        ret_val = os.system(command)

        # Rename or move the temp mask file to final maskPath
        if os.path.exists(generated_mask_file_path):
            os.rename(generated_mask_file_path, self.drawn_mask_file_path)

        # Cleanup the bounded TIFF
        os.remove(bounded_file_path)

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

    def read_image_data(self, file_path):
        if not file_path.exists():
            return None

        image_data = fabio.open(file_path).data
        return image_data

    def refreshImage(self):
        isShowImage = (self.showImageCheckBox.isEnabled()
            and self.showImageCheckBox.isChecked())

        isApplyBlank = (self.applyBlankCheckBox.isEnabled()
            and self.applyBlankCheckBox.isChecked())

        isApplyDrawnMask = (self.applyDrawnMaskCheckBox.isEnabled()
            and self.applyDrawnMaskCheckBox.isChecked())

        isApplyLowMask = (self.applyLowMaskCheckBox.isEnabled()
            and self.applyLowMaskCheckBox.isChecked())

        isApplyHighMask = (self.applyHighMaskCheckBox.isEnabled()
            and self.applyHighMaskCheckBox.isChecked())

        isApplyMask = isApplyDrawnMask or isApplyLowMask or isApplyHighMask

        # If user does not select showing image:
        if not isShowImage:
            # If nothing is selected, Show nothing.
            if not (isApplyBlank or isApplyMask):
                self.imageLabel.clear()
                self.displayDescLabel.setText("No Display")
                return

            # Only show empty cell.
            if isApplyBlank and (not isApplyMask):
                blank_image_weight = self.blank_image_info["weight"]
                imageData = self.blank_image_info["blank_image"] * blank_image_weight
                scaledPixmap = self.createDisplayImage(imageData,
                    self.vmin,
                    self.vmax,
                    self.imageLabel.width(),
                    self.imageLabel.height())
                self.imageLabel.setPixmap(scaledPixmap)
                self.displayDescLabel.setText(f"Empty Cell (with scale: {blank_image_weight:.2f})")
                return

            # Only show mask:
            if (not isApplyBlank) and isApplyMask:
                imageData = self.imageData.copy()
                drawnMaskData, lowMask, highMask = self.getMasks(imageData)
                masks = [mask for mask in
                    [drawnMaskData, lowMask, highMask]
                    if mask is not None]

                # If no mask, then show nothing.
                if not masks:
                    self.imageLabel.clear()
                    return

                maskData = np.prod(np.stack(masks), axis=0)
                scaledPixmap = self.createDisplayImage(maskData,
                    self.vmin,
                    self.vmax,
                    self.imageLabel.width(),
                    self.imageLabel.height())

                self.imageLabel.setPixmap(scaledPixmap)
                self.displayDescLabel.setText(f"Mask")
                return

            # Show empty cell + apply mask.
            blank_image_weight = self.blank_image_info["weight"]
            imageData = self.blank_image_info["blank_image"] * blank_image_weight
            drawnMaskData, lowMask, highMask = self.getMasks(imageData)

            scaledPixmap = self.createDisplayImageWithMasks(
                imageData,
                self.vmin,
                self.vmax,
                lowMask,
                highMask,
                drawnMaskData,
                self.imageLabel.width(),
                self.imageLabel.height())

            self.imageLabel.setPixmap(scaledPixmap)
            self.displayDescLabel.setText(f"Empty Cell (with scale: {blank_image_weight:.2f}) + Apply Mask")
            return

        # User selected showing image:
        imageData = self.imageData.copy()

        # Only show image.
        if not (isApplyBlank or isApplyMask):
            scaledPixmap = self.createDisplayImage(imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)
            self.displayDescLabel.setText(f"Original Image")
            return

        # Show image + subtract blank.
        if isApplyBlank and (not isApplyMask):
            blank_image_weight = self.blank_image_info["weight"]
            imageData = imageData - self.blank_image_info["blank_image"] * blank_image_weight

            scaledPixmap = self.createDisplayImage(imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)
            self.displayDescLabel.setText(f"Original Image + Empty Cell subtraction (with scale: {blank_image_weight:.2f})")
            return

        # Show image + apply mask.
        if (not isApplyBlank) and isApplyMask:
            drawnMaskData, lowMask, highMask = self.getMasks(
                imageData)

            scaledPixmap = self.createDisplayImageWithMasks(
                imageData,
                self.vmin,
                self.vmax,
                lowMask,
                highMask,
                drawnMaskData,
                self.imageLabel.width(),
                self.imageLabel.height())

            self.imageLabel.setPixmap(scaledPixmap)
            self.displayDescLabel.setText(f"Original Image + Apply Mask")
            return

        # Show image + subtract empty cell + apply mask.
        blank_image_weight = self.blank_image_info["weight"]
        imageData = imageData - self.blank_image_info["blank_image"] * blank_image_weight
        drawnMaskData, lowMask, highMask = self.getMasks(
            imageData)

        scaledPixmap = self.createDisplayImageWithMasks(
            imageData,
            self.vmin,
            self.vmax,
            lowMask,
            highMask,
            drawnMaskData,
            self.imageLabel.width(),
            self.imageLabel.height())

        self.imageLabel.setPixmap(scaledPixmap)
        self.displayDescLabel.setText(f"Original Image + Empty Cell subtraction (with scale: {blank_image_weight:.2f}) + Apply Mask")

    def getMasks(self, imageData):
        drawnMaskData = None

        if (self.applyDrawnMaskCheckBox.isEnabled()
            and self.applyDrawnMaskCheckBox.isChecked()):
            drawnMaskData = self.drawnMaskData

        lowMask = None

        if (self.applyLowMaskCheckBox.isEnabled()
            and self.applyLowMaskCheckBox.isChecked()):
            lowMask = (imageData > self.maskLowThresh.value()).astype(np.uint8)

            if (self.lowMaskDilationChkbx.isEnabled()
                and self.lowMaskDilationChkbx.isChecked()):
                low_kernel = self.getKernelSizes(self.lowDilComboBox)
                lowMask = self.dilateMask(lowMask, low_kernel)

        highMask = None
        if (self.applyHighMaskCheckBox.isEnabled()
            and self.applyHighMaskCheckBox.isChecked()):
            highMask = (imageData < self.maskHighThresh.value()).astype(np.uint8)

            if self.highMaskDilationChkbx.isChecked():
                high_kernel = self.getKernelSizes(self.highDilComboBox)
                highMask = self.dilateMask(highMask, high_kernel)

        return drawnMaskData, lowMask, highMask

    def dilateMask(self, mask, kernel_size):
        kernel = np.ones((kernel_size, kernel_size), dtype=np.uint8)
        dilated_mask = cv2.erode(mask, kernel, iterations=1)
        return dilated_mask

    def createDisplayImageWithMasks(self, imageArray, minInt, maxInt,
                              lowMask, highMask, drawnMask, displayImageWidth,
                              displayImageHeight,
                              rMask=None):
        """
        Displays 'imageArray' in grayscale and overlays:
          - lowMask (green)
          - highMask (blue)
          - drawnMask (red)
        Each mask is assumed to be the same shape as imageArray,
        and contains 0s and 1s.
        Returns a QPixmap of size 500x500 (maintaining aspect ratio).
        """

        if imageArray is None:
            print("Empty image")
            return

        # Flip the image vertically (up-down) so it matches the display
        # in the main window, where the y-axis is defined bottom-to-top
        # using ax.set_ylim.
        flippedImageArray = np.flipud(imageArray)

        # 2) Normalize to 0-255 for display
        if np.max(flippedImageArray) == np.min(flippedImageArray):
            normFlippedImageArray = np.full(flippedImageArray.shape, 128, dtype=np.uint8)
        else:
            if minInt == maxInt:
                # Use the array min/max
                normFlippedImageArray = 255 * (
                    flippedImageArray - np.min(flippedImageArray)
                ) / (np.max(flippedImageArray) - np.min(flippedImageArray))
            else:
                normFlippedImageArray = 255 * (
                    flippedImageArray - minInt
                ) / (maxInt - minInt)

            normFlippedImageArray = normFlippedImageArray.astype(np.uint8)

        # 3) Convert grayscale to 3-channel RGB
        #    shape: (height, width) -> (height, width, 3)
        height, width = normFlippedImageArray.shape
        colorImageArray = np.dstack([
            normFlippedImageArray,
            normFlippedImageArray,
            normFlippedImageArray
        ])

        # 4) Overlay masks:
        #    Assign mask color wherever mask == 1
        #    Note: The order below determines overwrite precedence
        #          if a pixel is in multiple masks
        # Red mask (drawnMask)


        # Red mask (drawnMask)
        if drawnMask is not None:
            drawnMask = np.flipud(np.asarray(drawnMask))
            colorImageArray[drawnMask == 0] = [255, 0, 0]
        # Green mask (lowMask)
        if lowMask is not None:
            lowMask = np.flipud(np.asarray(lowMask))
            colorImageArray[lowMask == 0]   = [0, 255, 0]
        # Blue mask (highMask)
        if highMask is not None:
            highMask = np.flipud(np.asarray(highMask))
            colorImageArray[highMask == 0] = [0, 0, 255]
        # Purple Mask (Rmin/Rmax)
        if rMask is not None:
            rMask = np.flipud(np.asarray(rMask))
            colorImageArray[rMask == 0] = [255, 0, 255]

        # 5) Convert the color image (RGB) to QImage
        #    Format_RGB888 expects the data in 24-bit RGB
        qImg = QImage(colorImageArray.data,
                      width,
                      height,
                      colorImageArray.strides[0],
                      QImage.Format_RGB888)

        # 6) Convert QImage to QPixmap
        pixmap = QPixmap.fromImage(qImg)

        # 7) Scale the pixmap to fit within the display area while maintaining the aspect ratio
        scaledPixmap = pixmap.scaled(
            displayImageWidth, displayImageHeight,
            Qt.KeepAspectRatio, Qt.SmoothTransformation)

        return scaledPixmap

    def getKernelSizes(self, dilComboBox):
        kernel_size = None

        if dilComboBox.currentText() == "3x3 Kernel":
            kernel_size = 3
        elif dilComboBox.currentText() == "5x5 Kernel":
            kernel_size = 5
        elif dilComboBox.currentText() == "7x7 Kernel":
            kernel_size = 7

        return kernel_size
