import sys
import os
from os.path import join
from PyQt5.QtWidgets import QDialogButtonBox, QDoubleSpinBox, QSlider, QGridLayout, QCheckBox, QDialog, QPushButton, QLabel, QSpinBox, QStatusBar, QVBoxLayout, QFileDialog, QWidget
from PyQt5.QtGui import QPixmap, QImage
from PyQt5.QtCore import Qt
import numpy as np
import subprocess
import glob
import json
from PIL import Image
import fabio
from ..utils.file_manager import createFolder
from .pyqt_utils import *

def read_edf_to_numpy(file_path):
    # Load the EDF file
    edf_image = fabio.open(file_path)
    # Convert the data to a NumPy array
    np_array = edf_image.data
    print("Opening image:",np_array.shape)#######
    return np_array


def displayImage(imageArray, minInt, maxInt):

    if imageArray is None:
      print("Empty image")
      return

    # Flip the image horizontally
    # flippedImageArray = np.flipud(imageArray)
    flippedImageArray = imageArray
    
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


class ImageMaskerWindow(QDialog):
    def __init__(self, dir_path, imagePath, minInt, maxInt):
        super().__init__()
        self.dir_path = dir_path
        self.imagePath = imagePath
        self.minInt = minInt # Min Intensity from AISE User Input
        self.maxInt = maxInt # Max Intensity from AISE user Input
        self.blankImagePath = None
        self.blankImageData = None
        self.imageData = None  # Attribute to store the loaded image data
        self.maskData = None  # Stores the mask data (computed + drawn mask data)
        self.maskedImage = None # Attribute to store the masked image data (original image + mask data) usually for displaying
        self.computedMaskData = None  # Attribute to store the computed mask data
        self.drawnMaskData = None  # Attribute to store the drawn mask data
        self.subtractedImage = None # Attribute to store the subtracted image data
        self.drawnMask = False
        self.computedMask = False
        self.initUI()
        self.loadImage(self.imagePath)

    def initUI(self):
        self.setWindowTitle('Mask and Empty Cell Specification')

        self.layout = QVBoxLayout()

        self.buttonWidget = QWidget()
        self.buttonLayout = QGridLayout()
        self.buttonWidget.setLayout(self.buttonLayout)

        self.imageLabel = QLabel()
        self.imageLabel.setFixedSize(500, 500)  # Set the fixed size
        # Optional: Set a border to visualize the area if you like
        self.imageLabel.setStyleSheet("border: 1px solid black;")
        self.imageLabel.setAlignment(Qt.AlignCenter)  # Center-align the image


        self.selectBlankImg = QPushButton("Select Empty Cell Image(s)")
        self.selectBlankImg.clicked.connect(self.browseImage)
        self.drawMaskBtn = QPushButton("Draw Mask")
        self.showBlankImageChkbx = QCheckBox("Show Empty Cell Image")
        self.showBlankImageChkbx.setEnabled(False)
        self.showBlankImageChkbx.stateChanged.connect(self.showBlankImage)
        self.maskThresChkbx = QCheckBox("Mask Threshold")
        self.maskThresChkbx.stateChanged.connect(self.enableMaskThres)
        
        self.maskThresh = QDoubleSpinBox()
        self.maskThresh.setMinimum(-50)
        self.maskThresh.setValue(-1)
        self.maskThresh.setSingleStep(0.01)
        #self.maskThresh.setKeyboardTracking(False)
        
        self.maskThresh.valueChanged.connect(self.maskThresholdChanged)
        
        self.maskThresh.setEnabled(False)
        
        self.showMaskChkBx = QCheckBox("Show Mask")
        self.showMaskChkBx.setEnabled(False)
        self.showMaskChkBx.stateChanged.connect(self.onShowMaskClicked)
        
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
        self.buttonLayout.addWidget(self.maskThresChkbx, 2, 0, 1, 2)
        self.buttonLayout.addWidget(self.maskThresh, 2, 3, 1, 2)
        self.buttonLayout.addWidget(self.showMaskChkBx, 3, 0, 1, 2)
        self.buttonLayout.addWidget(self.subtractBlankChkbx, 4, 0, 1, 2)
        # self.buttonLayout.addWidget(self.subtractSlider, 4, 3, 1, 2)
        self.buttonLayout.addWidget(self.subtractSliderText, 4, 3, 1, 2)
        self.buttonLayout.addWidget(self.negativeValuesLabel, 5, 0, 1, 4)
        self.buttonLayout.addWidget(self.clampNegativeValuesChkbx, 6, 0, 1, 2)
        self.buttonLayout.addWidget(self.bottons, 7, 1, 1, 2)

        self.layout.addWidget(self.imageLabel)
        self.layout.addWidget(self.buttonWidget)
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
                scaledPixmap=displayImage(self.maskedImage.data, self.minInt, self.maxInt)
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

        
    def enableMaskThres(self):
        if self.maskThresChkbx.isChecked():
            self.computedMask = True
            self.maskThresh.setEnabled(True)
            self.showMaskChkBx.setEnabled(True)  
            self.maskThresholdChanged()
        else:
            self.maskThresh.setEnabled(False)
            self.computedMask = False
            self.computedMaskData = None
            if self.drawnMaskData is not None:
                self.maskData = self.drawnMaskData
                self.applyMask()
            else:
                self.loadImage(self.imagePath)
            
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
                scaledPixmap=displayImage(self.maskedImage, self.minInt, self.maxInt)
                self.imageLabel.setPixmap(scaledPixmap)
            else:
                min_value = np.min(self.imageData)
                scaledPixmap=displayImage(self.imageData, self.minInt, self.maxInt)
                self.imageLabel.setPixmap(scaledPixmap)    
            if min_value < 0:
                self.negativeValuesLabel.setText("Negative Values in Image (Min: {}) ".format(min_value))
                self.negativeValuesLabel.setVisible(True)
            
    def onShowMaskClicked(self):
        if self.showMaskChkBx.isChecked():
            self.showMask()
        else:
            self.applyMask()

    def loadMask(self, filePath):
        raw_filepath = r"{}".format(filePath)
        print(raw_filepath)
        if os.path.exists(raw_filepath):
            # Use fabio to open the image file and store its datas
            mask = fabio.open(raw_filepath)
            self.maskData = mask.data
        else:
            print("File does not exist.")
            
    def maskThresholdChanged(self):
        value = self.maskThresh.value()
        originalImageArray = self.imageData
        self.computedMaskData = np.where(originalImageArray <= value, 0, 1).astype(np.uint8)
        # scaledPixmap=displayImage(self.computedMaskData, -1, -1)
        # self.imageLabel.setPixmap(scaledPixmap)

    def loadImage(self, filePath):
        # Use fabio to open the image file and store its data
        raw_filepath = r"{}".format(filePath)
        image = fabio.open(raw_filepath)
        self.imageData = image.data
        scaledPixmap=displayImage(self.imageData, self.minInt, self.maxInt)
        self.imageLabel.setPixmap(scaledPixmap)

    def drawMask(self):
        if self.dir_path:
            # Assuming pyFAI-drawmask can be called directly from the command line
            if self.showBlankImageChkbx.isChecked():
                command = f'pyFAI-drawmask "{self.blankImagePath}"'
                self.maskPath = self.blankImagePath.rsplit('.', 1)[0] + '-mask.edf'
            else:
                command = f'pyFAI-drawmask "{self.imagePath}"'
                self.maskPath = self.imagePath.rsplit('.', 1)[0] + '-mask.edf'
            subprocess.run(command, shell=True)

            # draw_dialog = MaskImageWidget(self.selected, self.maskData)
            # result = draw_dialog.exec_()
            
            # Assuming the mask file follows a naming convention like originalFileName-mask.edf
            self.loadMask(self.maskPath)   
            if self.maskData is not None:
                
                thresholdValue = np.max(self.maskData)  # This works for images where the mask is full white for mask-out regions
                self.maskData = np.where(self.maskData < thresholdValue, 1, 0)
                self.drawnMaskData = self.maskData
                
                self.maskThresh.setEnabled(True)
                self.showMaskChkBx.setEnabled(True)
                self.drawnMask = True
                self.applyMask()
                # scaledPixmap=displayImage(self.maskData)
                # self.imageLabel.setPixmap(scaledPixmap)
                
                os.remove(self.maskPath) # remove the mask file generated as we save a file in the settings folder
            
        else:
            print("No input file to draw on.")
        
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
        scaledPixmap=displayImage(self.maskData, -1, -1)
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
            scaledPixmap=displayImage(maskedImageArray.data, self.minInt, self.maxInt)
            self.imageLabel.setPixmap(scaledPixmap)

            # Compute the number of pixels to mask out (where the value is 0)
            masked_out_pixels = np.sum(maskArray == 0)
            print(f"Number of pixels to mask out: {masked_out_pixels}")
            non_masked_pixels = np.sum(maskArray == 1)

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
                fabio.tifimage.tifimage(data=self.maskData).write(join(path,'mask.tif'))
                print("mask file saved")  
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
        scaledPixmap=displayImage(self.subtractedImage, self.minInt, self.maxInt)
        self.imageLabel.setPixmap(scaledPixmap)