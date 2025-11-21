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
import shutil

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
                image_data,
                settings_dir_path,
                vmin,
                vmax
        ):
        """
        Initialize Image Mask Dialog.
        
        Args:
            image_data: numpy array of the image (not a file path!)
            settings_dir_path: directory where mask files will be saved
            vmin: minimum intensity for display
            vmax: maximum intensity for display
        """
        super().__init__()
        self.setModal(True)
        self.setWindowTitle("Set Image Mask")
        self.settings_dir_path = Path(settings_dir_path)
        self.vmin = vmin
        self.vmax = vmax

        # Store image data directly (no file I/O needed)
        self.imageData = np.asarray(image_data).astype("float32")
        
        # Mask files will be saved in settings directory
        self.drawn_mask_file_path = self.settings_dir_path / "drawn-mask.edf"
        self.mask_config_file_path = self.settings_dir_path / "mask_config.json"

        drawnMaskData = self.read_image_data(self.drawn_mask_file_path)

        if (drawnMaskData is not None) and drawnMaskData.shape == self.imageData.shape:
            # pyFAI saves mask as 0=keep, 1=mask, so we invert it
            # Also ensure it's uint8 (0 or 1 only)
            self.drawnMaskData = (1 - drawnMaskData).astype(np.uint8)
        else:
            self.drawnMaskData = None

        # If mask config file exists, load mask config.
        mask_config = self.readMaskConfig()

        if mask_config is not None:
            mask_low_thresh = mask_config.get("mask_low_thresh")
            mask_low_kernel_size = mask_config.get("mask_low_kernel_size")
            mask_high_thresh = mask_config.get("mask_high_thresh")
            mask_high_kernel_size = mask_config.get("mask_high_kernel_size")
            
            # Save original config values before they get modified for widget defaults
            # These will be used to determine which checkboxes to auto-check
            config_has_low_thresh = mask_low_thresh is not None and isinstance(mask_low_thresh, (int, float))
            config_has_low_kernel = mask_low_kernel_size is not None
            config_has_high_thresh = mask_high_thresh is not None and isinstance(mask_high_thresh, (int, float))
            config_has_high_kernel = mask_high_kernel_size is not None
        else:
            mask_low_thresh = None
            mask_low_kernel_size = None
            mask_high_thresh = None
            mask_high_kernel_size = None
            config_has_low_thresh = False
            config_has_low_kernel = False
            config_has_high_thresh = False
            config_has_high_kernel = False

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

        # Single unified Mask Options group
        self.applyMaskGroup = QGroupBox("Mask Options")
        self.applyMaskGroup.setToolTip(
            "The selected mask options will be saved to a file and"
            + " applied to the image when clicking the Save button.")

        self.applyMaskLayout = QGridLayout(self.applyMaskGroup)
        
        
        # Drawn Mask section
        settingsRowIndex = 0
        self.applyDrawnMaskCheckBox = QCheckBox("Drawn Mask")
        self.applyMaskLayout.addWidget(self.applyDrawnMaskCheckBox, settingsRowIndex, 0, 1, 4)
        settingsRowIndex += 1
        
        self.applyDrawnMaskText = QLabel()
        self.applyMaskLayout.addWidget(self.applyDrawnMaskText, settingsRowIndex, 0, 1, 4)
        settingsRowIndex += 1
        
        self.drawMaskBtn = QPushButton("Draw Mask")
        self.applyMaskLayout.addWidget(self.drawMaskBtn, settingsRowIndex, 0, 1, 4)
        settingsRowIndex += 1
        
        # Add spacing
        self.applyMaskLayout.setRowMinimumHeight(settingsRowIndex, 15)
        settingsRowIndex += 1
        
        # Low Mask Threshold section
        self.maskLowThreshChkbx = QCheckBox("Low Mask Threshold")
        self.applyMaskLayout.addWidget(self.maskLowThreshChkbx, settingsRowIndex, 0, 1, 2)
        
        self.maskLowThresh = QDoubleSpinBox()
        self.maskLowThresh.setMinimum(-50)
        self.maskLowThresh.setMaximum(10000)
        if not isinstance(mask_low_thresh, float):
            mask_low_thresh = -0.01
        self.maskLowThresh.setValue(mask_low_thresh)
        self.maskLowThresh.setSingleStep(0.01)
        self.maskLowThresh.setEnabled(False)
        self.maskLowThresh.valueChanged.connect(self.maskLowThresholdChanged)
        self.applyMaskLayout.addWidget(self.maskLowThresh, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        
        self.lowMaskDilationChkbx = QCheckBox("Enable Mask Dilation")
        self.lowMaskDilationChkbx.setEnabled(False)
        self.applyMaskLayout.addWidget(self.lowMaskDilationChkbx, settingsRowIndex, 0, 1, 2)
        
        self.lowDilComboBox = QComboBox()
        self.lowDilComboBox.addItem("3x3 Kernel")
        self.lowDilComboBox.addItem("5x5 Kernel")
        self.lowDilComboBox.addItem("7x7 Kernel")

        lowDilComboBoxIndex = 0
        if mask_low_kernel_size == 3:
            lowDilComboBoxIndex = 0
        elif mask_low_kernel_size == 5:
            lowDilComboBoxIndex = 1
        elif mask_low_kernel_size == 7:
            lowDilComboBoxIndex = 2
        self.lowDilComboBox.setCurrentIndex(lowDilComboBoxIndex)
        self.lowDilComboBox.setEnabled(False)
        self.lowDilComboBox.currentIndexChanged.connect(self.lowDilComboBoxChanged)
        self.applyMaskLayout.addWidget(self.lowDilComboBox, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        
        # Add spacing
        self.applyMaskLayout.setRowMinimumHeight(settingsRowIndex, 15)
        settingsRowIndex += 1
        
        # High Mask Threshold section
        self.maskHighThreshChkbx = QCheckBox("High Mask Threshold")
        self.applyMaskLayout.addWidget(self.maskHighThreshChkbx, settingsRowIndex, 0, 1, 2)
        
        self.maskHighThresh = QDoubleSpinBox()
        self.maskHighThresh.setMinimum(0)
        self.maskHighThresh.setMaximum(float('inf'))
        if not isinstance(mask_high_thresh, float):
            mask_high_thresh = 64000.0
        self.maskHighThresh.setValue(mask_high_thresh)
        self.maskHighThresh.setSingleStep(0.01)
        self.maskHighThresh.setEnabled(False)
        self.maskHighThresh.valueChanged.connect(self.maskHighThresholdChanged)
        self.applyMaskLayout.addWidget(self.maskHighThresh, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        
        # Enable or disable dilation for upper bound mask
        self.highMaskDilationChkbx = QCheckBox("Enable Mask Dilation")
        self.highMaskDilationChkbx.setEnabled(False)
        self.applyMaskLayout.addWidget(self.highMaskDilationChkbx, settingsRowIndex, 0, 1, 2)
        
        # Choose the kernel size for dilation (upper bound threshold)
        self.highDilComboBox = QComboBox()
        self.highDilComboBox.addItem("3x3 Kernel")
        self.highDilComboBox.addItem("5x5 Kernel")
        self.highDilComboBox.addItem("7x7 Kernel")

        highDilComboBoxIndex = 0
        if mask_high_kernel_size == 3:
            highDilComboBoxIndex = 0
        elif mask_high_kernel_size == 5:
            highDilComboBoxIndex = 1
        elif mask_high_kernel_size == 7:
            highDilComboBoxIndex = 2
        self.highDilComboBox.setCurrentIndex(highDilComboBoxIndex)
        self.highDilComboBox.setEnabled(False)
        self.highDilComboBox.currentIndexChanged.connect(self.highDilComboBoxChanged)
        self.applyMaskLayout.addWidget(self.highDilComboBox, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        
        # Add spacing before color labels
        self.applyMaskLayout.setRowMinimumHeight(settingsRowIndex, 15)
        settingsRowIndex += 1

        # Color legend labels inside the Mask Options group
        self.greenLabel = QLabel("Green: Low Mask Threshold")
        self.greenLabel.setStyleSheet("color: green")
        self.applyMaskLayout.addWidget(self.greenLabel, settingsRowIndex, 0, 1, 2)
        
        self.blueLabel = QLabel("Blue: High Mask Threshold")
        self.blueLabel.setStyleSheet("color: blue")
        self.applyMaskLayout.addWidget(self.blueLabel, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        
        self.redLabel = QLabel("Red: Drawn Mask")
        self.redLabel.setStyleSheet("color: red")
        self.applyMaskLayout.addWidget(self.redLabel, settingsRowIndex, 0, 1, 2)
        
        self.purpleLabel = QLabel("Purple: Rmin / Rmax mask")
        self.purpleLabel.setStyleSheet("color: purple")
        self.applyMaskLayout.addWidget(self.purpleLabel, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1

        self.negativeValuesLabel = QLabel("Negative Values Detected in Image: ")
        font = QFont()
        font.setBold(True)
        self.negativeValuesLabel.setFont(font)
        self.negativeValuesLabel.setVisible(False)

        self.dialogButtons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        okButton = self.dialogButtons.button(QDialogButtonBox.Ok)
        okButton.setText("Save")

        self.dialogButtons.accepted.connect(self.okClicked)
        self.dialogButtons.rejected.connect(self.reject)

        # Settings widget with only the applyMaskGroup
        self.settingsWidget = QWidget()
        self.settingsLayout = QVBoxLayout(self.settingsWidget)
        self.settingsLayout.addWidget(self.applyMaskGroup)
        self.settingsLayout.addStretch()

        self.scrollArea = QScrollArea()
        self.scrollArea.setWidgetResizable(True)
        self.scrollArea.setWidget(self.settingsWidget)
        # Set minimum width to fit the longest text without truncation
        self.scrollArea.setMinimumWidth(700)

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

        self.updateDrawnMaskWidgets()
        
        # Auto-check checkboxes based on loaded configuration
        # Note: Drawn Mask is auto-checked in updateDrawnMaskWidgets() if file exists
        
        # Auto-check Low Mask if threshold was saved in config
        if config_has_low_thresh:
            self.maskLowThreshChkbx.setChecked(True)
            self.maskLowThresh.setEnabled(True)
            self.lowMaskDilationChkbx.setEnabled(True)
            
            # Auto-check Low Mask Dilation if kernel size was saved
            if config_has_low_kernel:
                self.lowMaskDilationChkbx.setChecked(True)
                self.lowDilComboBox.setEnabled(True)
        
        # Auto-check High Mask if threshold was saved in config
        if config_has_high_thresh:
            self.maskHighThreshChkbx.setChecked(True)
            self.maskHighThresh.setEnabled(True)
            self.highMaskDilationChkbx.setEnabled(True)
            
            # Auto-check High Mask Dilation if kernel size was saved
            if config_has_high_kernel:
                self.highMaskDilationChkbx.setChecked(True)
                self.highDilComboBox.setEnabled(True)
        
        self.refreshImage()

        self.setConnections()
        
        # Automatically resize dialog to fit all widgets at their natural size
        self.adjustSize()

    def setConnections(self):
        self.drawMaskBtn.clicked.connect(self.drawMask)
        self.applyDrawnMaskCheckBox.checkStateChanged.connect(self.applyDrawnMask)
        self.maskLowThreshChkbx.checkStateChanged.connect(self.enableLowMaskThresh)

        self.maskHighThreshChkbx.checkStateChanged.connect(self.enableHighMaskThresh)

        self.lowMaskDilationChkbx.checkStateChanged.connect(self.enableLowMaskDilation)

        self.highMaskDilationChkbx.checkStateChanged.connect(self.enableHighMaskDilation)

    def updateDrawnMaskWidgets(self):
        """
        Update the drawn mask widgets based on file availability.
        If drawn mask file exists, enable and auto-check the checkbox.
        If file doesn't exist, disable and uncheck the checkbox.
        """
        if self.drawnMaskData is not None:
            # File exists: enable and auto-check
            self.applyDrawnMaskCheckBox.setEnabled(True)
            self.applyDrawnMaskCheckBox.setChecked(True)
            self.applyDrawnMaskText.setText("Drawn mask image is available.")
            self.applyDrawnMaskText.setStyleSheet("color: green;")
        else:
            # File doesn't exist: disable and uncheck
            self.applyDrawnMaskCheckBox.setEnabled(False)
            self.applyDrawnMaskCheckBox.setChecked(False)
            self.applyDrawnMaskText.setText("No drawn mask image found. Ignore this message if expected.")
            self.applyDrawnMaskText.setStyleSheet("color: red;")

    def applyDrawnMask(self, state):
        self.refreshImage()

    def enableLowMaskThresh(self, state):
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
        self.saveMaskConfig()
        self.accept()

    def saveMaskConfig(self):
        isApplyDrawnMask = (self.applyDrawnMaskCheckBox.isEnabled()
            and self.applyDrawnMaskCheckBox.isChecked())

        isApplyLowMask = (self.maskLowThreshChkbx.isEnabled()
            and self.maskLowThreshChkbx.isChecked())

        isApplyHighMask = (self.maskHighThreshChkbx.isEnabled()
            and self.maskHighThreshChkbx.isChecked())

        isApplyMask = isApplyDrawnMask or isApplyLowMask or isApplyHighMask

        if not isApplyMask:
            # Remove all mask-related files if no mask is applied
            self.mask_config_file_path.unlink(missing_ok=True)
            mask_output_path = self.settings_dir_path / "mask.tif"
            mask_output_path.unlink(missing_ok=True)
            self.drawn_mask_file_path.unlink(missing_ok=True)
            print("🗑️  Removed mask configuration, mask.tif, and drawn mask file")
            return
        
        # Handle drawn mask: delete file if user unchecked it
        if not isApplyDrawnMask and self.drawn_mask_file_path.exists():
            self.drawn_mask_file_path.unlink(missing_ok=True)
            print(f"🗑️  Deleted drawn mask file: {self.drawn_mask_file_path}")

        # Save mask config to file.
        mask_config = {}

        # mask config example:
        # {
        #     "mask_low_thresh": 1.0,
        #     "mask_low_kernel_size": 1.0,
        #     "mask_high_thresh": 1.0,
        #     "mask_high_kernel_size": 1.0,
        # }
        # Note: drawn_mask_file_path is NOT saved in config.
        # Drawn mask presence is determined solely by file existence.

        if isApplyLowMask:
            mask_low_thresh = self.maskLowThresh.value()
            mask_config["mask_low_thresh"] = mask_low_thresh

            if (self.lowMaskDilationChkbx.isEnabled()
                and self.lowMaskDilationChkbx.isChecked()):
                mask_low_kernel_size = self.getKernelSizes(self.lowDilComboBox)
                mask_config["mask_low_kernel_size"] = mask_low_kernel_size

        if isApplyHighMask:
            mask_high_thresh = self.maskHighThresh.value()
            mask_config["mask_high_thresh"] = mask_high_thresh

            if (self.highMaskDilationChkbx.isEnabled()
                and self.highMaskDilationChkbx.isChecked()):
                mask_high_kernel_size = self.getKernelSizes(self.highDilComboBox)
                mask_config["mask_high_kernel_size"] = mask_high_kernel_size

        self.mask_config_file_path.parent.mkdir(parents=True, exist_ok=True)

        with open(self.mask_config_file_path, "w") as file_stream:
            json.dump(mask_config, file_stream, indent=4)
        
        # Generate and save the final combined mask.tif file
        imageData = self.imageData.copy()
        drawnMaskData, lowMask, highMask = self.getMasks(imageData)
        
        masks = [m for m in [drawnMaskData, lowMask, highMask] if m is not None]
        if masks:
            # Ensure all masks are uint8 before combining
            masks = [m.astype(np.uint8) if m.dtype != np.uint8 else m for m in masks]
            final_mask = np.prod(np.stack(masks), axis=0).astype(np.uint8)
            mask_output_path = self.settings_dir_path / "mask.tif"
            fabio.tifimage.tifimage(data=final_mask).write(mask_output_path)
            print(f"✅ Saved final mask to: {mask_output_path}")

    def readMaskConfig(self):
        # mask config example:
        # {
        #     "mask_low_thresh": 1.0,
        #     "mask_low_kernel_size": 1.0,
        #     "mask_high_thresh": 1.0,
        #     "mask_high_kernel_size": 1.0,
        # }
        # Note: drawn_mask_file_path is NOT in config anymore.

        if not self.mask_config_file_path.exists():
            return None

        with open(self.mask_config_file_path, "r") as file_stream:
            mask_config = json.load(file_stream)

        if not isinstance(mask_config, dict):
            return None

        return mask_config

    def drawMask(self):
        if self.imageData is not None:
            # Run pyFAI-drawmask in a separate thread
            thread = threading.Thread(target=self._run_drawmask_command_and_refresh)
            thread.start()
        else:
            print("No image data available for drawing mask.")

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
            # pyFAI saves mask as 0=keep, 1=mask, so we invert it
            # Also ensure it's uint8 (0 or 1 only)
            self.drawnMaskData = (1 - drawnMaskData).astype(np.uint8)
        else:
            self.drawnMaskData = None

        # Update widgets and auto-check if mask is available
        self.updateDrawnMaskWidgets()
        self.refreshImage()

    def _run_drawmask_command(self):
        """
        Run the pyFAI-drawmask command:
        1) Create a temporary TIFF file for pyFAI to read (command-line tool needs a file)
        2) Call pyFAI-drawmask on that TIFF
        3) Move the resulting mask to the final location
        4) Clean up temporary files
        """

        # Create temporary file for pyFAI-drawmask (it's a command-line tool, needs a file path)
        temp_input_path = self.settings_dir_path / "temp_for_drawmask.tif"
        fabio.tifimage.tifimage(data=self.imageData).write(temp_input_path)

        # Run pyFAI-drawmask on the temporary file
        command = f'pyFAI-drawmask "{temp_input_path}"'

        # pyFAI will produce: temp_for_drawmask-mask.edf
        generated_mask_path = self.settings_dir_path / "temp_for_drawmask-mask.edf"

        ret_val = os.system(command)

        # Move the generated mask to final location
        if generated_mask_path.exists():
            if self.drawn_mask_file_path.exists():
                self.drawn_mask_file_path.unlink()
            generated_mask_path.rename(self.drawn_mask_file_path)

        # Clean up temporary input file
        temp_input_path.unlink(missing_ok=True)

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

    def read_image_data(self, file_path):
        if not file_path.exists():
            return None

        image_data = fabio.open(file_path).data
        return image_data

    def refreshImage(self):
        # Image is always shown
        isApplyDrawnMask = (self.applyDrawnMaskCheckBox.isEnabled()
            and self.applyDrawnMaskCheckBox.isChecked())

        isApplyLowMask = (self.maskLowThreshChkbx.isEnabled()
            and self.maskLowThreshChkbx.isChecked())

        isApplyHighMask = (self.maskHighThreshChkbx.isEnabled()
            and self.maskHighThreshChkbx.isChecked())

        isApplyMask = isApplyDrawnMask or isApplyLowMask or isApplyHighMask

        imageData = self.imageData.copy()

        # Only show image (no mask overlay)
        if not isApplyMask:
            scaledPixmap = self.createDisplayImage(imageData,
                self.vmin,
                self.vmax,
                self.imageLabel.width(),
                self.imageLabel.height())
            self.imageLabel.setPixmap(scaledPixmap)
            self.statusBar.setText(f"Current View: Original Image")
            return

        # Show image + apply mask overlay
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
        self.statusBar.setText(f"Current View: Original Image + Mask Overlay")

    def getMasks(self, imageData):
        drawnMaskData = None

        if (self.applyDrawnMaskCheckBox.isEnabled()
            and self.applyDrawnMaskCheckBox.isChecked()):
            drawnMaskData = self.drawnMaskData

        lowMask = None

        if (self.maskLowThreshChkbx.isEnabled()
            and self.maskLowThreshChkbx.isChecked()):
            lowMask = (imageData > self.maskLowThresh.value()).astype(np.uint8)

            if (self.lowMaskDilationChkbx.isEnabled()
                and self.lowMaskDilationChkbx.isChecked()):
                mask_low_kernel_size = self.getKernelSizes(self.lowDilComboBox)
                lowMask = self.dilateMask(lowMask, mask_low_kernel_size)

        highMask = None
        if (self.maskHighThreshChkbx.isEnabled()
            and self.maskHighThreshChkbx.isChecked()):
            highMask = (imageData < self.maskHighThresh.value()).astype(np.uint8)

            if (self.highMaskDilationChkbx.isEnabled()
                and self.highMaskDilationChkbx.isChecked()):
                mask_high_kernel_size = self.getKernelSizes(self.highDilComboBox)
                highMask = self.dilateMask(highMask, mask_high_kernel_size)

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

            # Clip values to [0, 255] range before converting to uint8 to avoid underflow/overflow
            normFlippedImageArray = np.clip(normFlippedImageArray, 0, 255).astype(np.uint8)

        # 3) Convert grayscale to 3-channel RGB
        #    shape: (height, width) -> (height, width, 3)
        height, width = normFlippedImageArray.shape
        colorImageArray = np.dstack([
            normFlippedImageArray,
            normFlippedImageArray,
            normFlippedImageArray
        ])


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
