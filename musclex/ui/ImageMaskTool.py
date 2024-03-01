import sys
import os
from os.path import join
from PyQt5.QtWidgets import QDialogButtonBox, QDoubleSpinBox, QSlider, QGridLayout, QCheckBox, QDialog, QPushButton, QLabel, QSpinBox, QStatusBar, QVBoxLayout, QFileDialog, QWidget
from PyQt5.QtGui import QPixmap, QImage
from PyQt5.QtCore import Qt
import numpy as np
import subprocess
import glob
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


def displayImage(imageArray):

    if imageArray is None:
      print("Empty image")
      return

    # Flip the image horizontally
    flippedImageArray = np.flipud(imageArray)

    # Normalize the flipped image to the 0-255 range for display
    normFlippedImageArray = 255 * (flippedImageArray - np.min(flippedImageArray)) / (np.max(flippedImageArray) - np.min(flippedImageArray))
    normFlippedImageArray = normFlippedImageArray.astype(np.uint8)  # Convert to 8-bit

    # Create a QImage from the 8-bit array
    qImg = QImage(normFlippedImageArray.data, normFlippedImageArray.shape[1], normFlippedImageArray.shape[0], normFlippedImageArray.strides[0], QImage.Format_Grayscale8)

    # Convert QImage to QPixmap
    pixmap = QPixmap.fromImage(qImg)

    # Scale the pixmap to fit within the 500x500 area while maintaining the aspect ratio
    scaledPixmap = pixmap.scaled(500, 500, Qt.KeepAspectRatio, Qt.SmoothTransformation)

    return scaledPixmap


class ImageMaskerWindow(QDialog):
    def __init__(self, dir_path, firstImage):
        super().__init__()
        self.dir_path = dir_path
        self.firstImage = self.dir_path + '/' + firstImage
        self.blankImagePath = None
        self.imageData = None  # Attribute to store the loaded image data
        self.maskData = None  # Attribute to store the loaded mask data
        self.maskedImage = None
        self.computedMaskData = None  # Attribute to store the computed mask data
        self.doSubtractBlankImage = False
        self.doSubtractBlankImageWeight = None
        self.initUI()

    def initUI(self):
        self.setWindowTitle('Image Mask Application')

        self.layout = QVBoxLayout()

        self.buttonWidget = QWidget()
        self.buttonLayout = QGridLayout()
        self.buttonWidget.setLayout(self.buttonLayout)

        self.imageLabel = QLabel()
        self.imageLabel.setFixedSize(500, 500)  # Set the fixed size
        # Optional: Set a border to visualize the area if you like
        self.imageLabel.setStyleSheet("border: 1px solid black;")
        self.imageLabel.setAlignment(Qt.AlignCenter)  # Center-align the image


        self.selectBlankImg = QPushButton("Select Blank Image(s)")
        self.selectBlankImg.clicked.connect(self.browseImage)
        self.drawMaskBtn = QPushButton("Draw Mask")
        self.showBlankImageChkbx = QCheckBox("Show Blank Image")
        self.showBlankImageChkbx.setEnabled(False)
        self.showBlankImageChkbx.stateChanged.connect(self.showBlankImage)
        self.maskThresChkbx = QCheckBox("Mask Threshold")
        self.maskThresChkbx.stateChanged.connect(self.enableMaskThres)
        
        self.maskThresh = QDoubleSpinBox()
        self.maskThresh.setRange(0, 1)
        self.maskThresh.setValue(1)
        self.maskThresh.setSingleStep(0.01)
        #self.maskThresh.setKeyboardTracking(False)
        
        self.maskThresh.valueChanged.connect(self.maskThresholdChanged)
        
        self.maskThresh.setEnabled(False)
        
        
        self.showMaskChkBx = QCheckBox("Show Mask")
        self.showMaskChkBx.setEnabled(False)
        self.showMaskChkBx.stateChanged.connect(self.onShowMaskClicked)
        
        
        self.subtractBlankChkbx = QCheckBox("Subtract Blank Image")
        self.subtractBlankChkbx.setEnabled(False)
        self.subtractBlankChkbx.stateChanged.connect(self.enableSubtractSlider)
        
        self.subtractSlider = QSlider(Qt.Horizontal, self)
        self.subtractSlider.setRange(0, 100)
        self.subtractSlider.setSingleStep(1)
        self.subtractSlider.setValue(100)
        self.subtractSliderText = QDoubleSpinBox()
        self.subtractSliderText.setKeyboardTracking(False)
        self.subtractSliderText.setRange(0, 1)
        self.subtractSliderText.setValue(1.00)
        self.subtractSliderText.setSingleStep(0.01)
        self.subtractSlider.setEnabled(False)
        self.subtractSliderText.setEnabled(False)
        
        self.subtractSlider.valueChanged.connect(self.update_spinbox)
        self.subtractSliderText.valueChanged.connect(self.update_slider)

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
        self.buttonLayout.addWidget(self.subtractSlider, 4, 3, 1, 2)
        self.buttonLayout.addWidget(self.subtractSliderText, 5, 3, 1, 2)
        self.buttonLayout.addWidget(self.bottons, 6, 1, 1, 2)

        self.layout.addWidget(self.imageLabel)
        self.layout.addWidget(self.buttonWidget)
        self.setLayout(self.layout)

        self.drawMaskBtn.clicked.connect(self.drawMask)
        self.loadImage(self.firstImage)
        
    def browseImage(self):
        """
        Browse a blank image
        """
        img_list = getFiles(path=self.dir_path)
        if not img_list:
            self.blankImagePath = img_list[0]
            self.showBlankImageChkbx.setEnabled(True)
            self.subtractBlankChkbx.setEnabled(True)  
        
    def showBlankImage(self):
        if self.showBlankImageChkbx.isChecked():
            self.loadImage(self.blankImagePath)
        else:
            if self.maskedImage is not None:
                scaledPixmap=displayImage(self.maskedImage.data)
                self.imageLabel.setPixmap(scaledPixmap)
            else:
                self.loadImage(self.firstImage)
        
    def update_slider(self):
        self.subtractSlider.blockSignals(True)
        value_changed = self.subtractSliderText.value() * 100
        self.subtractSlider.setValue(int(value_changed))
        self.subtractSlider.blockSignals(False)
    def update_spinbox(self):
        self.subtractSliderText.blockSignals(True)
        value_changed = self.subtractSlider.value() / 100
        self.subtractMaskedImage()
        self.subtractSliderText.setValue(value_changed)
        self.subtractSliderText.blockSignals(False)
        
    def enableMaskThres(self):
        if self.maskThresChkbx.isChecked():
            self.maskThresh.setEnabled(True)
            self.maskThresholdChanged()
        else:
            self.maskThresh.setEnabled(False)
            self.computedMaskData = None
            self.maskData = self.drawnMaskData
            self.applyMask()
            
    def enableSubtractSlider(self):
        if self.subtractBlankChkbx.isChecked():
            self.doSubtractBlankImage = True
            self.subtractSlider.setEnabled(True)
            self.subtractSliderText.setEnabled(True)
        else:
            self.doSubtractBlankImage = False
            self.subtractSlider.setEnabled(False)
            self.subtractSliderText.setEnabled(False)
            
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
        self.computedMaskData = np.where(originalImageArray <= value, 0, 1)
        # self.showMask()

    def loadImage(self, filePath):
        # Use fabio to open the image file and store its data
        image = fabio.open(filePath)
        self.imageData = image.data
        scaledPixmap=displayImage(self.imageData)
        self.imageLabel.setPixmap(scaledPixmap)


    def drawMask(self):
        
        if self.dir_path:
            # Assuming pyFAI-drawmask can be called directly from the command line
            command = f'pyFAI-drawmask "{self.firstImage}"'
            subprocess.run(command, shell=True)

            # draw_dialog = MaskImageWidget(self.selected, self.maskData)
            # result = draw_dialog.exec_()
            
            # Assuming the mask file follows a naming convention like originalFileName-mask.edf
            self.maskPath = self.firstImage.rsplit('.', 1)[0] + '-mask.edf'
            self.loadMask(self.maskPath)   
            if self.maskData is not None:
                
                thresholdValue = np.max(self.maskData)  # This works for images where the mask is full white for mask-out regions
                self.maskData = np.where(self.maskData < thresholdValue, 1, 0)
                self.drawnMaskData = self.maskData
                
                self.maskThresh.setEnabled(True)
                self.showMaskChkBx.setEnabled(True)
                self.applyMask()
                # scaledPixmap=displayImage(self.maskData)
                # self.imageLabel.setPixmap(scaledPixmap)
                
                os.remove(self.maskPath) # remove the mask file generated as we save a file in the settings folder
            
        else:
            print("No input file to draw on.")
        

    def computeCombinedMask(self):
        # Check if self.computedMaskData is None and initialize it to ones if so
        if self.computedMaskData is None:
            # Initialize computedMaskData to an array of ones with the same shape as maskData
            self.computedMaskData = np.ones_like(self.maskData)

        # Ensure self.maskData is also not None before proceeding
        if self.maskData is not None:
            # Multiply self.maskData and self.computedMaskData element-wise
            self.maskData = np.multiply(self.maskData, self.computedMaskData)
        else:
            self.maskData = self.computedMaskData

    def showMask(self):
        self.computeCombinedMask()
        scaledPixmap=displayImage(self.maskData)
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
            scaledPixmap=displayImage(maskedImageArray.data)
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
        if self.maskData is not None:
            path = join(self.dir_path, 'settings')
            createFolder(path)
            fabio.tifimage.tifimage(data=self.maskData).write(join(path,'mask.tif'))
            if self.doSubtractBlankImage:
                self.doSubtractBlankImageWeight = self.subtractSliderText.value()
            self.accept()
        else:
            self.reject()

    def subtractMaskedImage(self):
        weight = self.subtractSliderText.value()
        self.maskData = self.maskData * weight
        self.applyMask()
        
        

