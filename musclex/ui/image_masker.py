import sys
import os
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QLabel, QVBoxLayout, QFileDialog
from PyQt5.QtGui import QPixmap, QImage
from PyQt5.QtCore import Qt
import numpy as np
import subprocess
from PIL import Image
import fabio


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



class ImageMaskerWindow(QWidget):
    def __init__(self):
        super().__init__()
        self.initUI()
        self.imageData = None  # Attribute to store the loaded image data
        self.maskData = None  # Attribute to store the loaded mask data
        self.computedMaskData = None  # Attribute to store the computed mask data

    def initUI(self):
        self.setWindowTitle('Image Mask Application')
        layout = QVBoxLayout()

        # Create an image display label with a fixed size of 500x500
        self.imageLabel = QLabel()
        self.imageLabel.setFixedSize(500, 500)  # Set the fixed size
        # Optional: Set a border to visualize the area if you like
        self.imageLabel.setStyleSheet("border: 1px solid black;")
        self.imageLabel.setAlignment(Qt.AlignCenter)  # Center-align the image
        layout.addWidget(self.imageLabel)

        self.openFileBtn = QPushButton('Open File')
        self.openFileBtn.clicked.connect(self.openFileDialog)
        layout.addWidget(self.openFileBtn)

        self.drawMaskBtn = QPushButton('Draw Mask')
        self.drawMaskBtn.clicked.connect(self.drawMask)
        self.drawMaskBtn.setEnabled(False)  # Disable until an image is loaded
        layout.addWidget(self.drawMaskBtn)

        self.computeMaskBtn = QPushButton('Compute Mask')
        self.computeMaskBtn.clicked.connect(self.computeMask)
        self.computeMaskBtn.setEnabled(False)  # Disable until an image is loaded
        layout.addWidget(self.computeMaskBtn)

        self.showMaskBtn = QPushButton('Show Combined Mask')
        self.showMaskBtn.clicked.connect(self.showMask)
        self.showMaskBtn.setEnabled(False)  # Disabled until an image is loaded
        layout.addWidget(self.showMaskBtn)

        self.applyMaskBtn = QPushButton('Apply Mask')
        self.applyMaskBtn.clicked.connect(self.applyMask)
        self.applyMaskBtn.setEnabled(False)  # Disable until mask is drawn
        layout.addWidget(self.applyMaskBtn)

        self.setLayout(layout)
        self.filePath = ''

    def openFileDialog(self):
        options = QFileDialog.Options()
        fileName, _ = QFileDialog.getOpenFileName(self, "Open Image File", "", "Image Files (*.png *.jpg *.jpeg *.bmp *.tif *.tiff);;All Files (*)", options=options)
        if fileName:
            self.filePath = fileName
            self.loadImage(fileName)            
            displayImage(self.imageData)
            self.drawMaskBtn.setEnabled(True)
            self.computeMaskBtn.setEnabled(True)


    def loadMask(self, filePath):
        if os.path.exists(filePath):
            # Use fabio to open the image file and store its data
            mask = fabio.open(filePath)
            self.maskData = mask.data
        else:
            print("File does not exist.")


    def loadImage(self, filePath):
        # Use fabio to open the image file and store its data
        image = fabio.open(filePath)
        self.imageData = image.data
        scaledPixmap=displayImage(self.imageData)
        self.imageLabel.setPixmap(scaledPixmap)


    def drawMask(self):
        if self.filePath:
            # Assuming pyFAI-drawmask can be called directly from the command line
            command = f'pyFAI-drawmask {self.filePath}'
            subprocess.run(command, shell=True)

            # Assuming the mask file follows a naming convention like originalFileName-mask.edf
            maskPath = self.filePath.rsplit('.', 1)[0] + '-mask.edf'
            self.loadMask(maskPath)   
            if self.maskData is not None:
                thresholdValue = np.max(self.maskData)  # This works for images where the mask is full white for mask-out regions
                self.maskData = np.where(self.maskData < thresholdValue, 1, 0)
                scaledPixmap=displayImage(self.maskData)
                self.imageLabel.setPixmap(scaledPixmap)
                self.showMaskBtn.setEnabled(True)
                self.applyMaskBtn.setEnabled(True)
        else:
            print("No input file to draw on.")


    def computeMask(self):
        if self.imageData is not None:
            # Define a mask for all values that are smaller or equal to 0
            self.computedMaskData = np.where(self.imageData <= 0, 0, 1)
            scaledPixmap=displayImage(self.computedMaskData)
            self.imageLabel.setPixmap(scaledPixmap)
            self.showMaskBtn.setEnabled(True)
        else:
            print("No image data to compute mask from.")


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
        self.applyMaskBtn.setEnabled(True)



    def applyMask(self):
        if self.filePath:
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




if __name__ == '__main__':
    app = QApplication(sys.argv)
    mainWindow = MainWindow()
    mainWindow.show()
    sys.exit(app.exec_())

