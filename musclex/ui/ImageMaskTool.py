import sys
import os
from os.path import join
from PySide6.QtWidgets import QDialogButtonBox, QDoubleSpinBox, QSlider, QGridLayout, QCheckBox, QDialog, QPushButton, QLabel, QSpinBox, QStatusBar, QVBoxLayout, QFileDialog, QWidget
from PySide6.QtGui import QPixmap, QImage
from PySide6.QtCore import Qt
import numpy as np
import subprocess
import glob
import json
from PIL import Image
import fabio
from ..utils.file_manager import createFolder
from .pyqt_utils import *
import threading
import cv2
from scipy.ndimage import rotate

try:
    from ..utils.file_manager import fullPath, createFolder, getBlankImageAndMask, getMaskOnly, ifHdfReadConvertless
    from ..utils.histogram_processor import *
    from ..utils.image_processor import *
except: # for coverage
    from utils.file_manager import fullPath, createFolder, getBlankImageAndMask, getMaskOnly, ifHdfReadConvertless
    from utils.histogram_processor import *
    from utils.image_processor import *


def read_edf_to_numpy(file_path):
    # Load the EDF file
    edf_image = fabio.open(file_path)
    # Convert the data to a NumPy array
    np_array = edf_image.data
    print("Opening image:",np_array.shape)#######
    return np_array


def displayImage(imageArray, minInt, maxInt, rot=0):

    if imageArray is None:
      print("Empty image")
      return

    # Flip the image horizontally
    flippedImageArray = rotate(np.flipud(imageArray), -rot, reshape=False) #flip the image vertically to match what is displayed on the main GUI
    #flippedImageArray = np.ascontiguousarray(np.rot90(imageArray, k=-1))
    #flippedImageArray = imageArray
    
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

    # Scale the pixmap to fit within the 500x500 area while maintaining the aspect ratio
    scaledPixmap = pixmap.scaled(500, 500, Qt.KeepAspectRatio, Qt.SmoothTransformation)

    return scaledPixmap


def displayImageWithMasks(imageArray, minInt, maxInt, 
                          lowMask, highMask, drawnMask, 
                          rot=0):
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
    
    # 1) Flip the image
    flippedImageArray = np.flipud(imageArray)
    
    # 2) Normalize to 0-255 for display
    if np.max(flippedImageArray) == np.min(flippedImageArray):
        normFlippedImageArray = np.full(flippedImageArray.shape, 128, dtype=np.uint8)
    else:
        if minInt == -1 and maxInt == -1:
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

    # 5) Convert the color image (RGB) to QImage
    #    Format_RGB888 expects the data in 24-bit RGB
    qImg = QImage(colorImageArray.data, 
                  width, 
                  height, 
                  colorImageArray.strides[0], 
                  QImage.Format_RGB888)

    # 6) Convert QImage to QPixmap
    pixmap = QPixmap.fromImage(qImg)

    # 7) Scale the pixmap to fit within 500x500 (maintain aspect ratio)
    scaledPixmap = pixmap.scaled(500, 500, Qt.KeepAspectRatio, Qt.SmoothTransformation)

    return scaledPixmap


class ImageMaskerWindow(QDialog):
    def __init__(self, dir_path, imagePath, minInt, maxInt, max_val, orig_size, trans_mat=None, rot_angle = 0, isHDF5=False):
        super().__init__()

        print("IMAGEMASKERWINDOW CONSTRUCTOR") #NICKA DEBUG
        print("dir_path: ", dir_path) #NICKA DEBUG
        print("imagePath: ", imagePath) #NICKA DEBUG
        print("minInt: ", minInt) #NICKA DEBUG
        print("maxInt: ", maxInt)   #NICKA DEBUG
        print("max_val: ", max_val) #NICKA DEBUG
        print("trans mat: ", trans_mat) #NICKA DEBUG
        print("orig_size: ", orig_size) #NICKA DEBUG
        print("rot_angle: ", rot_angle) #NICKA DEBUG
        print("isHDF5: ", isHDF5) #NICKA DEBUG

        self.dir_path = dir_path
        self.imagePath = imagePath
        self.minInt = minInt # Min Intensity from AISE User Input
        self.maxInt = maxInt # Max Intensity from AISE user Input
        self.blankImagePath = None
        self.blankImageData = None
        self.isHDF5 = isHDF5
        self.imageData = None  # Attribute to store the loaded image data
        self.maskData = None  # Stores the mask data (computed + drawn mask data)
        self.maskedImage = None # Attribute to store the masked image data (original image + mask data) usually for displaying
        self.lowThreshMaskData = None  # Attribute to store the mask created by the lower bound threshold
        self.dilatedLowThreshMaskData = None  # Attribute to store the mask created by the lower bound threshold after dilation
        self.highThreshMaskData = None  # Attribute to store the mask created by the upper bound threshold
        self.dilatedHighThreshMaskData = None # Attribute to store the mask created by the upper bound threshold after dilation
        self.computedMaskData = None  # Attribute to store the computed mask data
        self.drawnMaskData = None  # Attribute to store the drawn mask data
        self.subtractedImage = None # Attribute to store the subtracted image data
        self.drawnMask = False
        self.computedMask = False
        self.max_val = max_val # Maximum intensity value in the image
        self.rot_angle = rot_angle # Rotation angle for the image
        self.trans_mat = trans_mat # Transformation matrix for the image
        self.orig_size = orig_size # Original size of the image before rotation

        self.maskHighThreshVal = None
        self.maskLowThreshVal = None

        self.initUI()
        self.loadImage(self.imagePath)

    def initUI(self):
        self.setWindowTitle('Mask and Empty Cell Specification')

        self.layout = QVBoxLayout()

        self.imageLabel = QLabel()
        self.imageLabel.setFixedSize(500, 500)  # Set the fixed size
        # Optional: Set a border to visualize the area if you like
        self.imageLabel.setStyleSheet("border: 1px solid black;")
        self.imageLabel.setAlignment(Qt.AlignCenter)  # Center-align the image

        self.buttonWidget = QWidget()
        self.buttonLayout = QGridLayout()
        self.buttonWidget.setLayout(self.buttonLayout)

        self.scrollArea = QScrollArea(self)
        self.scrollArea.setWidgetResizable(True)
        self.scrollArea.setWidget(self.buttonWidget)

        self.selectBlankImg = QPushButton("Select Empty Cell Image(s)")
        self.selectBlankImg.clicked.connect(self.browseImage)
        self.drawMaskBtn = QPushButton("Draw Mask")
        self.showBlankImageChkbx = QCheckBox("Show Empty Cell Image")
        self.showBlankImageChkbx.setEnabled(False)
        self.showBlankImageChkbx.stateChanged.connect(self.showBlankImage)

        self.maskLowThresChkbx = QCheckBox("Low Mask Threshold")
        self.maskLowThresChkbx.stateChanged.connect(self.enableLowMaskThres)
        
        self.maskLowThresh = QDoubleSpinBox()
        self.maskLowThresh.setMinimum(-50)
        self.maskLowThresh.setMaximum(10000)
        self.maskLowThresh.setValue(-1)
        self.maskLowThresh.setSingleStep(0.01)
        #self.maskThresh.setKeyboardTracking(False)
        
        self.maskLowThresh.valueChanged.connect(self.maskLowThresholdChanged)
        
        self.maskLowThresh.setEnabled(False)

        #Enable or disable dilation for lower bound mask
        self.lowMaskDilationChkbx = QCheckBox("Enable Mask Dilation")
        self.lowMaskDilationChkbx.setEnabled(False)
        self.lowMaskDilationChkbx.setVisible(False)
        self.lowMaskDilationChkbx.stateChanged.connect(self.enableLowMaskDilation)

        #Choose the kernel size for dilation (lower bound threshold)
        self.lowDilComboBox = QComboBox()
        self.lowDilComboBox.addItem("3x3 Kernel")
        self.lowDilComboBox.addItem("5x5 Kernel")
        self.lowDilComboBox.addItem("7x7 Kernel")
        self.lowDilComboBox.addItem("9x9 Kernel")
        self.lowDilComboBox.addItem("11x11 Kernel")
        self.lowDilComboBox.addItem("13x13 Kernel")

        self.lowDilComboBox.setCurrentIndex(0)
        self.lowDilComboBox.setVisible(False)
        self.lowDilComboBox.currentIndexChanged.connect(self.lowDilComboBoxChanged)

        self.maskHighThreshChkbx = QCheckBox("High Mask Threshold")
        self.maskHighThreshChkbx.stateChanged.connect(self.enableHighMaskThres)
        
        self.maskHighThresh = QDoubleSpinBox()
        self.maskHighThresh.setMinimum(0)
        self.maskHighThresh.setMaximum(self.max_val + 1.0)
        self.maskHighThresh.setValue(self.max_val + 1.0)
        self.maskHighThresh.setSingleStep(0.01)
        #self.maskThresh.setKeyboardTracking(False)
        
        self.maskHighThresh.valueChanged.connect(self.maskHighThresholdChanged)
    
        self.maskHighThresh.setEnabled(False)

        #Enable or disable dilation for upper bound mask
        self.highMaskDilationChkbx = QCheckBox("Enable Mask Dilation")
        self.highMaskDilationChkbx.setEnabled(False)
        self.highMaskDilationChkbx.setVisible(False)
        self.highMaskDilationChkbx.stateChanged.connect(self.enableHighMaskDilation)

        #Choose the kernel size for dilation (upper bound threshold)
        self.highDilComboBox = QComboBox()
        self.highDilComboBox.addItem("3x3 Kernel")
        self.highDilComboBox.addItem("5x5 Kernel")
        self.highDilComboBox.addItem("7x7 Kernel")
        self.highDilComboBox.addItem("9x9 Kernel")
        self.highDilComboBox.addItem("11x11 Kernel")
        self.highDilComboBox.addItem("13x13 Kernel")
        self.highDilComboBox.setCurrentIndex(0)
        self.highDilComboBox.setVisible(False)
        self.highDilComboBox.currentIndexChanged.connect(self.highDilComboBoxChanged)

        self.showMaskComboBox = QComboBox()
        self.showMaskComboBox.setEnabled(False)
        self.showMaskComboBox.addItem("Show Image With Mask")
        self.showMaskComboBox.addItem("Show Image Only")
        self.showMaskComboBox.addItem("Show Mask Only")
        self.showMaskComboBox.setCurrentIndex(0)
        self.showMaskComboBox.currentIndexChanged.connect(self.onShowMaskClicked)

        self.greenLabel = QLabel("Green: Low Mask Threshold")
        self.greenLabel.setStyleSheet("color: green")
        self.greenLabel.setVisible(False)
        self.blueLabel = QLabel("Blue: High Mask Threshold")
        self.blueLabel.setStyleSheet("color: blue")
        self.blueLabel.setVisible(False)
        self.redLabel = QLabel("Red: Drawn Mask")
        self.redLabel.setStyleSheet("color: red")
        self.redLabel.setVisible(False)
        
        self.subtractBlankChkbx = QCheckBox("Subtract Empty Cell Image")
        self.subtractBlankChkbx.setEnabled(False)
        self.subtractBlankChkbx.stateChanged.connect(self.enableSubtractSlider)
        
        self.negativeValuesLabel = QLabel("Negative Values Detected in Image: ")
        font = QFont()
        font.setBold(True)
        self.negativeValuesLabel.setFont(font)
        self.negativeValuesLabel.setVisible(False)
        
        self.clampNegativeValuesChkbx = QCheckBox("Clamp Negative Values to 0")
        self.clampNegativeValuesChkbx.setToolTip("Sets all negative values after subtraction to 0")
        self.clampNegativeValuesChkbx.setEnabled(False)
        
        # self.subtractSlider = QSlider(Qt.Horizontal, self)
        # self.subtractSlider.setRange(0, 200)
        # self.subtractSlider.setSingleStep(1)
        # self.subtractSlider.setValue(100)
        # self.subtractSlider.setEnabled(False)
        
        self.subtractSliderText = QDoubleSpinBox()
        self.subtractSliderText.setKeyboardTracking(False)
        self.subtractSliderText.setRange(0, 2)
        self.subtractSliderText.setValue(1.00)
        self.subtractSliderText.setSingleStep(0.01)
        self.subtractSliderText.setEnabled(False)
        
        # self.subtractSlider.valueChanged.connect(self.update_spinbox)
        self.subtractSliderText.valueChanged.connect(self.subtractBlankImage)

        self.bottons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        okButton = self.bottons.button(QDialogButtonBox.Ok)
        if okButton:
            okButton.setText("Save")
            
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)

        ### Status Bar ###
        self.statusBar = QStatusBar()
        self.pixel_detail = QLabel()
        self.statusBar.addWidget(self.pixel_detail)

        self.buttonLayout.addWidget(self.selectBlankImg, 0, 0, 1, 2)
        self.buttonLayout.addWidget(self.showBlankImageChkbx, 0, 3, 1, 2)
        self.buttonLayout.addWidget(self.drawMaskBtn, 1, 0, 1, 2)

        self.buttonLayout.addWidget(self.maskLowThresChkbx, 2, 0, 1, 2)
        self.buttonLayout.addWidget(self.maskLowThresh, 2, 3, 1, 2)

        self.buttonLayout.addWidget(self.lowMaskDilationChkbx, 3, 1, 1, 2)
        self.buttonLayout.addWidget(self.lowDilComboBox, 3, 3, 1, 1)

        self.buttonLayout.addWidget(self.maskHighThreshChkbx, 4, 0, 1, 2)
        self.buttonLayout.addWidget(self.maskHighThresh, 4, 3, 1, 2)

        self.buttonLayout.addWidget(self.highMaskDilationChkbx, 5, 1, 1, 2)
        self.buttonLayout.addWidget(self.highDilComboBox, 5, 3, 1, 1)

        self.buttonLayout.addWidget(self.showMaskComboBox, 6, 0, 1, 2)
        self.buttonLayout.addWidget(self.greenLabel, 6, 2, 1, 2)
        self.buttonLayout.addWidget(self.blueLabel, 7, 0, 1, 2)
        self.buttonLayout.addWidget(self.redLabel, 7, 2, 1, 2)

        self.buttonLayout.addWidget(self.subtractBlankChkbx, 8, 0, 1, 2)
        # self.buttonLayout.addWidget(self.subtractSlider, 4, 3, 1, 2)
        self.buttonLayout.addWidget(self.subtractSliderText, 8, 3, 1, 2)
        self.buttonLayout.addWidget(self.negativeValuesLabel, 9, 0, 1, 4)
        self.buttonLayout.addWidget(self.clampNegativeValuesChkbx, 10, 0, 1, 2)
        self.buttonLayout.addWidget(self.bottons, 11, 1, 1, 2)

        self.layout.addWidget(self.imageLabel)
        self.layout.addWidget(self.scrollArea)
        self.setLayout(self.layout)

        self.drawMaskBtn.clicked.connect(self.drawMask)
        
    def keyPressEvent(self, event):
        if event.key() == Qt.Key_Enter or event.key() == Qt.Key_Return:
            return
        super().keyPressEvent(event)
        
    def browseImage(self):
        """
        Browse a blank image
        """
        img_list = getFiles(path=self.dir_path)
        if img_list:
            raw_filepath = r"{}".format(img_list[0])
            self.blankImagePath = raw_filepath
            
            if os.path.exists(self.blankImagePath):
                image = fabio.open(self.blankImagePath)
                self.blankImageData = image.data

            self.showBlankImageChkbx.setEnabled(True)
            self.subtractBlankChkbx.setEnabled(True)
    
    def showBlankImage(self):
        if self.showBlankImageChkbx.isChecked():
            self.loadImage(self.blankImagePath)
        else:
            if self.maskedImage is not None:
                scaledPixmap=displayImage(self.maskedImage.data, self.minInt, self.maxInt, 0.0)
                self.imageLabel.setPixmap(scaledPixmap)
            else:
                self.loadImage(self.imagePath)
        
    # def update_slider(self):
    #     self.subtractSlider.blockSignals(True)
    #     value_changed = self.subtractSliderText.value() * 100
    #     self.subtractSlider.setValue(int(value_changed))
    #     self.subtractSlider.blockSignals(False)
    
    # def update_spinbox(self):
    #     self.subtractSliderText.blockSignals(True)
    #     value_changed = self.subtractSlider.value() / 100
    #     self.subtractBlankImage()
    #     self.subtractSliderText.setValue(value_changed)
    #     self.subtractSliderText.blockSignals(False)

    def showLabels(self):
        self.greenLabel.setVisible(True)
        self.blueLabel.setVisible(True)
        self.redLabel.setVisible(True)

    def hideLabels(self):     
        self.greenLabel.setVisible(False)
        self.blueLabel.setVisible(False)
        self.redLabel.setVisible(False)
        
    def enableLowMaskThres(self):
        if self.maskLowThresChkbx.isChecked():
            self.lowMaskDilationChkbx.setVisible(True)
            self.lowMaskDilationChkbx.setEnabled(True)
            self.maskLowThresChkbx.setEnabled(True)
            self.computedMask = True
            self.maskLowThresh.setEnabled(True)
            self.showMaskComboBox.setEnabled(True)
            self.showLabels()
            self.maskLowThresholdChanged()
        else:
            self.lowMaskDilationChkbx.setVisible(False)
            self.lowDilComboBox.setVisible(False)
            self.maskLowThresh.setEnabled(False)
            self.dilatedLowThreshMaskData = None
            if self.highThreshMaskData is not None and self.maskHighThreshChkbx.isChecked():
                self.computedMaskData = self.highThreshMaskData
            else:
                self.computedMask = False
                self.computedMaskData = None
                if self.drawnMaskData is None:
                    self.hideLabels()
                    self.showMaskComboBox.setEnabled(False)
            self.refreshMask()

    def enableHighMaskThres(self):
        if self.maskHighThreshChkbx.isChecked():
            self.highMaskDilationChkbx.setVisible(True)
            self.highMaskDilationChkbx.setEnabled(True)
            self.maskHighThresh.setEnabled(True)
            self.computedMask = True
            self.maskHighThresh.setEnabled(True)
            self.showMaskComboBox.setEnabled(True)  
            self.showLabels()
            self.maskHighThresholdChanged()
        else:
            self.highMaskDilationChkbx.setVisible(False)
            self.highDilComboBox.setVisible(False)
            self.maskHighThresh.setEnabled(False)
            self.dilatedHighThreshMaskData = None
            if self.lowThreshMaskData is not None and self.maskLowThresChkbx.isChecked():
                self.computedMaskData = self.lowThreshMaskData
            else:
                self.computedMask = False
                self.computedMaskData = None
                if self.drawnMaskData is None:
                    self.hideLabels()
                    self.showMaskComboBox.setEnabled(False)
            self.refreshMask()


    def getKernelSizes(self):
        kernel_size_low, kernel_size_high = None, None

        if self.lowDilComboBox.currentText() == "3x3 Kernel":
            kernel_size_low = 3
        elif self.lowDilComboBox.currentText() == "5x5 Kernel":
            kernel_size_low = 5
        elif self.lowDilComboBox.currentText() == "7x7 Kernel":
            kernel_size_low = 7
        elif self.lowDilComboBox.currentText() == "9x9 Kernel":
            kernel_size_low = 9
        elif self.lowDilComboBox.currentText() == "11x11 Kernel":
            kernel_size_low = 11
        elif self.lowDilComboBox.currentText() == "13x13 Kernel":
            kernel_size_low = 13

        if self.highDilComboBox.currentText() == "3x3 Kernel":
            kernel_size_high = 3
        elif self.highDilComboBox.currentText() == "5x5 Kernel":
            kernel_size_high = 5
        elif self.highDilComboBox.currentText() == "7x7 Kernel":
            kernel_size_high = 7
        elif self.highDilComboBox.currentText() == "9x9 Kernel":
            kernel_size_high = 9
        elif self.highDilComboBox.currentText() == "11x11 Kernel":
            kernel_size_high = 11
        elif self.highDilComboBox.currentText() == "13x13 Kernel":
            kernel_size_high = 13

        return kernel_size_low, kernel_size_high

    def dilateMask(self, mask, kernel_size):
        kernel = np.ones((kernel_size, kernel_size), dtype=np.uint8)
        dilated_mask = cv2.erode(mask, kernel, iterations=1)
        return dilated_mask

    def computeThreshMaskData(self):
        low_kernel, high_kernel = self.getKernelSizes()
        originalImageArray = self.imageData

        if self.maskLowThresChkbx.isChecked():
            if self.lowMaskDilationChkbx.isChecked():
                self.lowThreshMaskData = np.where(originalImageArray <= self.maskLowThreshVal, 0, 1).astype(np.uint8)
                low_mask = self.dilateMask(self.lowThreshMaskData, low_kernel)
            else:
                self.lowThreshMaskData = np.where(originalImageArray <= self.maskLowThreshVal, 0, 1).astype(np.uint8)
                low_mask = self.lowThreshMaskData
        else:
            low_mask = np.ones_like(originalImageArray)

        if self.maskHighThreshChkbx.isChecked():
            if self.highMaskDilationChkbx.isChecked():
                self.highThreshMaskData = np.where(originalImageArray >= self.maskHighThreshVal, 0, 1).astype(np.uint8)
                high_mask = self.dilateMask(self.highThreshMaskData, high_kernel)
            else:
                self.highThreshMaskData = np.where(originalImageArray >= self.maskHighThreshVal, 0, 1).astype(np.uint8)
                high_mask = self.highThreshMaskData
        else:
            high_mask = np.ones_like(originalImageArray)

        self.computedMaskData = np.multiply(low_mask, high_mask)
        self.dilatedLowThreshMaskData = low_mask
        self.dilatedHighThreshMaskData = high_mask

    def enableLowMaskDilation(self):
        if self.lowMaskDilationChkbx.isChecked():
            self.lowDilComboBox.setVisible(True)
        else:
            self.lowDilComboBox.setVisible(False)
        self.computeThreshMaskData()
        self.refreshMask()

    def enableHighMaskDilation(self):
        if self.highMaskDilationChkbx.isChecked():
            self.highDilComboBox.setVisible(True)
        else: 
            self.highDilComboBox.setVisible(False)
        self.computeThreshMaskData()
        self.refreshMask()

    def lowDilComboBoxChanged(self):
        self.computeThreshMaskData()
        self.refreshMask()

    def highDilComboBoxChanged(self):
        self.computeThreshMaskData()
        self.refreshMask()
            
    def enableSubtractSlider(self):
        
        if self.subtractBlankChkbx.isChecked():
            self.subtractBlankImage()
            # self.subtractSlider.setEnabled(True)
            self.subtractSliderText.setEnabled(True)
            self.clampNegativeValuesChkbx.setEnabled(True) 
        else:
            # self.subtractSlider.setEnabled(False)
            self.subtractSliderText.setEnabled(False)
            self.clampNegativeValuesChkbx.setEnabled(False)
            min_value = 0
            # reload non-subtracted image
            if self.maskedImage is not None:
                min_value = np.min(self.maskedImage)
                scaledPixmap=displayImage(self.maskedImage, self.minInt, self.maxInt, 0.0)
                self.imageLabel.setPixmap(scaledPixmap)
            else:
                min_value = np.min(self.imageData)
                scaledPixmap=displayImage(self.imageData, self.minInt, self.maxInt, 0.0)
                self.imageLabel.setPixmap(scaledPixmap)    
            if min_value < 0:
                self.negativeValuesLabel.setText("Negative Values in Image (Min: {}) ".format(min_value))
                self.negativeValuesLabel.setVisible(True)

    def refreshMask(self):
        if self.showMaskComboBox.currentText() == "Show Mask Only":
            self.showMask()
            if self.showMaskComboBox.isEnabled():
                self.hideLabels()
        elif self.showMaskComboBox.currentText() == "Show Image Only":
            self.computeCombinedMask()
            self.applyMask()
            if self.showMaskComboBox.isEnabled():
                self.hideLabels()
        elif self.showMaskComboBox.currentText() == "Show Image With Mask":
            self.computeCombinedMask()
            self.applyMask()
            scaledPixMap = displayImageWithMasks(self.imageData, self.minInt, self.maxInt, self.dilatedLowThreshMaskData, self.dilatedHighThreshMaskData, self.drawnMaskData)
            self.imageLabel.setPixmap(scaledPixMap)
            if self.showMaskComboBox.isEnabled():
                self.showLabels()

    def onShowMaskClicked(self):
        self.refreshMask()

    def loadMask(self, filePath):
        raw_filepath = r"{}".format(filePath)
        print(raw_filepath)
        if os.path.exists(raw_filepath):
            # Use fabio to open the image file and store its datas
            mask = fabio.open(raw_filepath)
            self.maskData = mask.data
        else:
            print("File does not exist.")
            
    def maskLowThresholdChanged(self):
        value = self.maskLowThresh.value()
        self.maskLowThreshVal = value
        self.computeThreshMaskData()
        self.refreshMask()

    def maskHighThresholdChanged(self):
        value = self.maskHighThresh.value()
        self.maskHighThreshVal = value
        self.computeThreshMaskData()
        self.refreshMask()


    def loadImage(self, filePath):
        # Use fabio to open the image file and store its data
        raw_filepath = r"{}".format(filePath)
        image = fabio.open(raw_filepath)
        self.imageData = image.data
        scaledPixmap=displayImage(self.imageData, self.minInt, self.maxInt, 0.0)
        self.imageLabel.setPixmap(scaledPixmap)


    def drawMask(self):
        if self.dir_path:
            # Assuming pyFAI-drawmask can be called directly from the command line
            thread = threading.Thread(target=self._run_drawmask_command_and_refresh)
            thread.start()
        else:
            print("No input file to draw on.")
            
    def _run_drawmask_command_and_refresh(self):
        try:
            # Run the command
            self._run_drawmask_command()
        finally:   # Ensure refreshMask is called in the main thread
            self.loadMask(self.maskPath)

            if os.path.exists(self.maskPath):
                # Use fabio to open the drawn mask image file and store its data in memory
                mask = fabio.open(self.maskPath)
                self.drawnMaskData = 1 - mask.data
            else:
                print("File does not exist.")

            self.showMaskComboBox.setEnabled(True)

            self.refreshMask()

    def _run_drawmask_command(self):
        """
        Run the pyFAI-drawmask command in a way that:
        1) Applies intensity limits to produce a 'bounded' TIFF,
        2) Calls pyFAI-drawmask on that TIFF,
        3) Renames the resulting mask file so it matches the old naming convention: <original>.tif -> <original>-mask.edf
        4) Removes the temporary bounded TIFF.
        """

        rotation_angle = self.rot_angle

        # Check if self.imageData is present


        if self.showBlankImageChkbx.isChecked():
            # The old code for blanks just used pyFAI-drawmask "<blankImagePath>"
            # and final mask = <blankNoExt>-mask.edf
            command = f'pyFAI-drawmask "{self.blankImagePath}"'


            base_blank = self.blankImagePath.rsplit('.', 1)[0]
            self.maskPath = base_blank + '-mask.edf'

            ret_val = os.system(command)

        else:
            if self.isHDF5:
                fabio_img = fabio.open(self.imagePath)
                data = fabio_img.data.astype(np.int32)

                # Convert special value, then apply min/max
                data[data == 4294967295] = -1
                data[data < np.int32(self.minInt)] = np.int32(self.minInt)
                data[data > np.int32(self.maxInt)] = np.int32(self.maxInt)

                # Write the "bounded" version to a temporary file
                bounded_file_name = self.imagePath.rsplit('.', 1)[0] + '_bounded.tif'

                tif_img = fabio.pilatusimage.pilatusimage(data=data, header=fabio_img.getheader())
                tif_img.write(bounded_file_name)

                # Run pyFAI-drawmask on the bounded TIFF
                command = f'pyFAI-drawmask "{bounded_file_name}"'

                # pyFAI will produce something like <bounded_file_nameNoExt>-mask.edf
                base_bounded = bounded_file_name.rsplit('.', 1)[0]  # e.g. "..._bounded"
                temp_mask_path = base_bounded + '-mask.edf'         # e.g. "..._bounded-mask.edf"

                ret_val = os.system(command)

                # The final mask name should be <originalFileNoExt>.h5-mask.edf or something
                # but based on your old code, let's do:
                self.maskPath = self.imagePath.rsplit('.', 1)[0] + '.h5-mask.edf'

                # Rename or move the temp mask file to final maskPath
                if os.path.exists(temp_mask_path):
                    os.rename(temp_mask_path, self.maskPath)

                # Cleanup the bounded TIFF
                os.remove(bounded_file_name)

            else:
                # Standard (non-HDF5) image case
                fabio_img = fabio.open(self.imagePath)
                data = fabio_img.data.astype(np.int32)

                # Bound data by minInt, maxInt
                data[data < np.int32(self.minInt)] = np.int32(self.minInt)
                data[data > np.int32(self.maxInt)] = np.int32(self.maxInt)

                # Write a bounded TIFF, so we don't overwrite original
                bounded_file_name = self.imagePath.rsplit('.', 1)[0] + '_bounded.tif'

                fabio.tifimage.tifimage(data=data).write(bounded_file_name)

                # Run pyFAI-drawmask on the bounded TIFF
                command = f'pyFAI-drawmask "{bounded_file_name}"'

                # pyFAI output file name
                base_bounded = bounded_file_name.rsplit('.', 1)[0]
                temp_mask_path = base_bounded + '-mask.edf'

                ret_val = os.system(command)


                # The final mask name in your old code was <originalFileNoExt>-mask.edf
                self.maskPath = self.imagePath.rsplit('.', 1)[0] + '-mask.edf'

                # Rename/move the temp mask file to the final name
                if os.path.exists(temp_mask_path):
                    os.rename(temp_mask_path, self.maskPath)

                # Remove the bounded TIFF
                os.remove(bounded_file_name)

            
    def computeCombinedMask(self):
        # Check if self.computedMaskData is None and initialize it to ones if so
        if self.computedMaskData is None:
            print("here!")
            # Initialize computedMaskData to an array of ones with the same shape as maskData
            self.computedMaskData = np.ones_like(self.maskData)

        # Ensure self.maskData is also not None before proceeding
        if self.drawnMaskData is not None:
            # Multiply self.maskData and self.computedMaskData element-wise
            self.maskData = np.multiply(self.drawnMaskData, self.computedMaskData)
        else:
            self.maskData = self.computedMaskData

    def showMask(self):
        self.computeCombinedMask()
        scaledPixmap=displayImage(self.maskData, -1, -1, 0.0)
        self.imageLabel.setPixmap(scaledPixmap)

    def applyMask(self):
        
        if self.dir_path:

            maskArray=self.maskData
            originalImageArray = self.imageData

            # Check if the original image is grayscale or color
            if len(originalImageArray.shape) == 3:  # Color image
                # Apply the mask to each color channel
                maskedImageArray = np.zeros_like(originalImageArray)
                for i in range(3):  # Assuming RGB channels
                    maskedImageArray[:,:,i] = originalImageArray[:,:,i] * maskArray 
            else:  # Grayscale image
                maskedImageArray = originalImageArray * maskArray 

            # Convert the result to QImage for display
            if maskedImageArray.ndim == 3:  # Color image
                height, width, channels = maskedImageArray.shape
                bytesPerLine = 3 * width
            else:  # Grayscale image
                height, width = maskedImageArray.shape
                bytesPerLine = width

            self.maskedImage = maskedImageArray

            scaledPixmap=displayImage(maskedImageArray.data, self.minInt, self.maxInt, 0.0)
            self.imageLabel.setPixmap(scaledPixmap)

            # Compute the number of pixels to mask out (where the value is 0)
            masked_out_pixels = np.sum(maskArray == 1)
            print(f"Number of pixels to mask out: {masked_out_pixels}")
            non_masked_pixels = np.sum(maskArray == 0)

            # Compute the total intensity of the masked image
            total_intensity = np.sum(maskedImageArray)
            print(f"Total intensity of the masked image: {total_intensity}")

            # Prevent division by zero
            if non_masked_pixels > 0:
                average_intensity = total_intensity / non_masked_pixels
                print(f"Average intensity of the masked image: {average_intensity}")
            else:
                print("No non-masked pixels found. Cannot compute average intensity.")
                
    def okClicked(self):
        print("OK CLICKED FUNCTION") #NICKA DEBUG
        if self.maskData is None and self.subtractBlankChkbx.isChecked() == False and self.computedMaskData is None and self.drawnMaskData is None:
            errMsg = QMessageBox()
            errMsg.setText('No mask data or blank image was provided. Please draw a mask or select a blank image.')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.setFixedWidth(300)
            errMsg.exec_()
        else:
            path = join(self.dir_path, 'settings')
            createFolder(path)
            if self.maskData is not None:
                self.computeCombinedMask() # to ensure mask data is updated

                if self.rot_angle is None:
                    self.rot_angle = 0

                print("mask data shape: ", self.maskData.shape) #NICKA DEBUG

                rot_mat = cv2.getRotationMatrix2D((self.maskData.shape[0]//2, self.maskData.shape[1]//2), -self.rot_angle, 1.0)

                print("ROTATION MATRIX: ", rot_mat) #NICKA DEBUG

                width = self.maskData.shape[1]
                height = self.maskData.shape[0]

                print("WIDTH: ", width) #NICKA DEBUG
                print("HEIGHT: ", height) #NICKA DEBUG

                rotated_mask = cv2.warpAffine(self.maskData.astype(np.uint8), rot_mat, (width, height))

                print("ROTATED MASK SHAPE: ", rotated_mask.shape) #NICKA DEBUG

                if self.trans_mat is not None:
                    print("trans_mat IF statement:  Trans mat: ", self.trans_mat) #NICKA DEBUG
                    inv_trans_mat = self.trans_mat.copy()
                else:
                    print("trans mat ELSE statement: Trans mat: ", self.trans_mat) #NICKA DEBUG
                    self.trans_mat = np.float32([[1,0,0],[0,1,0]]) #transformation by 0,0 i.e. no transformation
                    inv_trans_mat = self.trans_mat.copy()

                inv_trans_mat[0, 2] = -self.trans_mat[0, 2]
                inv_trans_mat[1, 2] = -self.trans_mat[1, 2]

                print("rotated_mask shape: ", rotated_mask.shape) #NICKA DEBUG
                print("inv_trans_mat: ", inv_trans_mat) #NICKA DEBUG

                (h, w) = rotated_mask.shape

                translated_mask = cv2.warpAffine(rotated_mask, inv_trans_mat, (w,h))
    
                print("TRANSLATED MASK SHAPE: ", translated_mask.shape) #NICKA DEBUG

                #fabio.tifimage.tifimage(data=translated_mask).write(join(path,'big_mask.tif'))
                try:
                    cropped_mask = translated_mask[0:self.orig_size[0], 0:self.orig_size[1]]
                    print("CROPPED MASK SIZE: ",cropped_mask.shape) #NICKA DEBUG
                    fabio.tifimage.tifimage(data=cropped_mask).write(join(path,'mask.tif'))
                    print("mask file saved")  
                except:
                    print("Error saving mask file")
            if self.subtractBlankChkbx.isChecked():
                dictionary = {
                    'subtractBlank': True, 
                    'weight': self.subtractSliderText.value(),
                    'path': self.blankImagePath,
                    'clampNegativeValues': self.clampNegativeValuesChkbx.isChecked()
                    }
                path = join(path, 'blank_image_settings.json')
                with open(path, 'w') as f:
                    json.dump(dictionary, f)
                print("blank image settings saved")
            self.accept()
            

    def subtractBlankImage(self):
        weight = self.subtractSliderText.value()
        if self.maskedImage is not None:
            self.subtractedImage = self.maskedImage - weight * self.blankImageData
        else:
            self.subtractedImage = self.imageData - weight * self.blankImageData
        
        if np.any(self.subtractedImage < 0):
            min_value = np.min(self.subtractedImage)
            self.negativeValuesLabel.setText("Negative Values in Image (Min: {}) ".format(min_value))
            self.negativeValuesLabel.setVisible(True)
        else:
            self.negativeValuesLabel.setVisible(False)
        self.subtractedImage[self.subtractedImage < 0] = 0 # Set negative values to 0
        scaledPixmap=displayImage(self.subtractedImage, self.minInt, self.maxInt, 0.0)
        self.imageLabel.setPixmap(scaledPixmap)