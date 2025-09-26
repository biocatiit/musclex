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


class ImageMaskDialog(QDialog):
    def __init__(self,
                dir_path,
                img,
                vmin,
                vmax
        ):
        super().__init__()
        self.setModal(True)
        self.setWindowTitle("Set Image Mask")
        self.dir_path = dir_path
        self.imageData = img
        self.vmin = vmin
        self.vmax = vmax

        self.imageLabel = QLabel()
        # Set fixed (width, height)
        self.imageLabel.setMinimumSize(800, 600)
        # Optional: Set a border to visualize the area if you like
        self.imageLabel.setStyleSheet("border: 1px solid black;")
        self.imageLabel.setAlignment(Qt.AlignCenter)  # Center-align the image


        self.showImageCheckBox = QCheckBox("Show Image")
        self.showImageCheckBox.setToolTip("Uncheck to show mask only")

        self.displayGroup = QGroupBox("Display Options")
        self.displayLayout = QVBoxLayout(self.displayGroup)
        self.displayLayout.addWidget(self.showImageCheckBox)

        self.noMaskCheckBox = QCheckBox("No Mask")
        self.applyDrawnMaskCheckBox = QCheckBox("Apply Drawn Mask")
        self.applyLowMaskCheckBox = QCheckBox("Apply Low Mask Threshold")
        self.applyHighMaskCheckBox= QCheckBox("Apply High Mask Threshold")

        self.applyMaskGroup = QGroupBox("Mask Options")
        self.applyMaskGroup.setToolTip(
            "The selected mask options will be saved to a file and"
            + " applied to the image when clicking the Save button.")
        self.applyMaskLayout = QVBoxLayout(self.applyMaskGroup)
        self.applyMaskLayout.addWidget(self.noMaskCheckBox)
        self.applyMaskLayout.addWidget(self.applyDrawnMaskCheckBox)
        self.applyMaskLayout.addWidget(self.applyLowMaskCheckBox)
        self.applyMaskLayout.addWidget(self.applyHighMaskCheckBox)

        self.drawMaskGroup = QGroupBox("Drawn Mask")
        self.drawMaskLayout = QVBoxLayout(self.drawMaskGroup)
        self.drawMaskBtn = QPushButton("Draw Mask with pyFAI")
        self.drawMaskLayout.addWidget(self.drawMaskBtn)

        self.maskLowThresGroup = QGroupBox("Low Mask Threshold Settings")
        self.maskLowThresLayout = QGridLayout(self.maskLowThresGroup)

        self.maskLowThresChkbx = QCheckBox("Low Mask Threshold")
        self.maskLowThresChkbx.stateChanged.connect(self.enableLowMaskThres)

        self.maskLowThresh = QDoubleSpinBox()
        self.maskLowThresh.setMinimum(-50)
        self.maskLowThresh.setMaximum(10000)
        self.maskLowThresh.setValue(-1)
        self.maskLowThresh.setSingleStep(0.01)
        self.maskLowThresh.valueChanged.connect(self.maskLowThresholdChanged)

        self.lowMaskDilationChkbx = QCheckBox("Enable Mask Dilation")
        self.lowMaskDilationChkbx.stateChanged.connect(self.enableLowMaskDilation)

        self.lowDilComboBox = QComboBox()
        self.lowDilComboBox.addItem("3x3 Kernel")
        self.lowDilComboBox.addItem("5x5 Kernel")
        self.lowDilComboBox.addItem("7x7 Kernel")

        self.lowDilComboBox.setCurrentIndex(0)
        self.lowDilComboBox.currentIndexChanged.connect(self.lowDilComboBoxChanged)

        settingsRowIndex = 0
        self.maskLowThresLayout.addWidget(self.maskLowThresChkbx, settingsRowIndex, 0, 1, 2)
        self.maskLowThresLayout.addWidget(self.maskLowThresChkbx, settingsRowIndex, 0, 1, 2)
        self.maskLowThresLayout.addWidget(self.maskLowThresh, settingsRowIndex, 3, 1, 2)
        settingsRowIndex += 1
        self.maskLowThresLayout.addWidget(self.lowMaskDilationChkbx, settingsRowIndex, 0, 1, 2)
        self.maskLowThresLayout.addWidget(self.lowDilComboBox, settingsRowIndex, 3, 1, 1)
        settingsRowIndex += 1

        self.maskHighThresGroup = QGroupBox("High Mask Threshold Settings")
        self.maskHighThresLayout = QGridLayout(self.maskHighThresGroup)

        self.maskHighThreshChkbx = QCheckBox("High Mask Threshold")
        self.maskHighThreshChkbx.stateChanged.connect(self.enableHighMaskThres)

        self.maskHighThresh = QDoubleSpinBox()
        self.maskHighThresh.setMinimum(0)
        self.maskHighThresh.setMaximum(self.imageData.max() + 1.0)
        self.maskHighThresh.setValue(self.imageData.max() + 1.0)
        self.maskHighThresh.setSingleStep(0.01)

        self.maskHighThresh.valueChanged.connect(self.maskHighThresholdChanged)

        #Enable or disable dilation for upper bound mask
        self.highMaskDilationChkbx = QCheckBox("Enable Mask Dilation")
        self.highMaskDilationChkbx.stateChanged.connect(self.enableHighMaskDilation)

        #Choose the kernel size for dilation (upper bound threshold)
        self.highDilComboBox = QComboBox()
        self.highDilComboBox.addItem("3x3 Kernel")
        self.highDilComboBox.addItem("5x5 Kernel")
        self.highDilComboBox.addItem("7x7 Kernel")
        self.highDilComboBox.setCurrentIndex(0)
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

        self.clampNegativeValuesChkbx = QCheckBox("Clamp Negative Values to 0")
        self.clampNegativeValuesChkbx.setToolTip("Sets all negative values after subtraction to 0")
        self.clampNegativeValuesChkbx.setEnabled(False)

        self.dialogButtons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        okButton = self.dialogButtons.button(QDialogButtonBox.Ok)
        okButton.setText("Save")

        self.dialogButtons.accepted.connect(self.okClicked)
        self.dialogButtons.rejected.connect(self.reject)

        self.buttonLayout = QGridLayout()

        settingsRowIndex = 0
        # self.buttonLayout.addWidget(self.selectBlankImg, 0, 0, 1, 2)
        # self.buttonLayout.addWidget(self.showBlankImageChkbx, 0, 3, 1, 2)
        # self.buttonLayout.addWidget(self.drawMaskBtn, settingsRowIndex, 0, 1, 4)
        # settingsRowIndex += 1
        # self.buttonLayout.addWidget(self.maskLowThresChkbx, settingsRowIndex, 0, 1, 2)
        # self.buttonLayout.addWidget(self.maskLowThresh, settingsRowIndex, 3, 1, 2)
        # settingsRowIndex += 1
        # self.buttonLayout.addWidget(self.lowMaskDilationChkbx, settingsRowIndex, 0, 1, 2)
        # self.buttonLayout.addWidget(self.lowDilComboBox, settingsRowIndex, 3, 1, 1)
        # settingsRowIndex += 1

        # self.buttonLayout.addWidget(self.maskHighThreshChkbx, settingsRowIndex, 0, 1, 2)
        # self.buttonLayout.addWidget(self.maskHighThresh, settingsRowIndex, 3, 1, 2)
        # settingsRowIndex += 1
        # self.buttonLayout.addWidget(self.highMaskDilationChkbx, settingsRowIndex, 0, 1, 2)
        # self.buttonLayout.addWidget(self.highDilComboBox, settingsRowIndex, 3, 1, 1)
        # settingsRowIndex += 1

        self.buttonLayout.addWidget(self.greenLabel, settingsRowIndex, 0, 1, 2)
        self.buttonLayout.addWidget(self.blueLabel, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        self.buttonLayout.addWidget(self.redLabel, settingsRowIndex, 0, 1, 2)
        self.buttonLayout.addWidget(self.purpleLabel, settingsRowIndex, 2, 1, 2)
        settingsRowIndex += 1
        self.buttonLayout.addWidget(self.negativeValuesLabel, settingsRowIndex, 0, 1, 4)
        settingsRowIndex += 1
        self.buttonLayout.addWidget(self.clampNegativeValuesChkbx, settingsRowIndex, 0, 1, 2)
        settingsRowIndex += 1
        self.buttonLayout.addWidget(self.dialogButtons, settingsRowIndex, 1, 1, 2)
        settingsRowIndex += 1

        self.buttonWidget = QWidget()
        self.buttonWidget.setLayout(self.buttonLayout)

        self.settingsWidget = QWidget()
        self.settingsLayout = QVBoxLayout(self.settingsWidget)
        self.settingsLayout.addWidget(self.displayGroup)
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

        scaledPixmap = self.displayImage(self.imageData,
            self.vmin,
            self.vmax,
            self.imageLabel.width(),
            self.imageLabel.height())
        self.imageLabel.setPixmap(scaledPixmap)

    def enableLowMaskThres(self):
        pass

    def maskLowThresholdChanged(self):
        pass

    def enableLowMaskDilation(self):
        pass

    def lowDilComboBoxChanged(self):
        pass

    def enableHighMaskThres(self):
        pass

    def maskHighThresholdChanged(self):
        pass

    def enableHighMaskDilation(self):
        pass

    def highDilComboBoxChanged(self):
        pass

    def okClicked(self):
        pass

    def displayImage(self,
        imageArray,
        minInt,
        maxInt,
        displayImageWidth,
        displayImageHeight):
        # Flip the image vertically (up-down) so it matches the display
        # in the main window, where the y-axis is defined bottom-to-top
        # using ax.set_ylim.
        flippedImageArray = np.flipud(imageArray)

        # Normalize the flipped image to the 0-255 range for display
        if np.max(flippedImageArray) == np.min(flippedImageArray):
            normFlippedImageArray = np.full(flippedImageArray.shape, 128, dtype=np.uint8)
        else:
            # If i manually set minInt and maxInt to -1, just use the images min and max (perhaps need to change since we are performing operations)
            if minInt == -1 and maxInt == -1:
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
