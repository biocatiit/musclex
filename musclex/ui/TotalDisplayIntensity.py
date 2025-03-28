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

import os
import json
import copy
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import fabio

from musclex import __version__
from .pyqt_utils import *
from ..utils.file_manager import *
from matplotlib.colors import Normalize
from .ImageMaskTool import ImageMaskerWindow


class TotalDisplayIntensity(QMainWindow):

    def __init__(self):
        super().__init__()
        
        self.dir_path = ""
        self.file_name = ""
        self.img = None
        self.imageMaskingTool = None
        self.mask = None
        self.masked_image = None
        self.total_intens_calcs = {}

        self.h5List = []
        self.h5Mode = False  # flag to indicate HDF5 mode

        self.initUI()
        self.setConnections()

    def initUI(self):
        """
        Initialize the UI
        """
        self.setWindowTitle("Muscle X Total Display Intensity v." + __version__)
        self.centralWidget = QWidget(self)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        ## display browse file and folder buttons when program started
        self.browseFileButton = QPushButton("Select an Image...")
        self.browseFileButton.clicked.connect(self.browseFile)
        self.browseFileButton.setFixedHeight(60)
        self.drawMaskButton = QPushButton("Mask The Image")
        self.drawMaskButton.clicked.connect(self.maskButtonClicked)
        self.drawMaskButton.setFixedHeight(60)

        self.canvImgGroup = QGroupBox("Image")
        self.canvImgGroup.setCheckable(False)
        self.canvImgLayout = QHBoxLayout(self.canvImgGroup)
        
        self.imgFigure = plt.figure()
        self.imgCanvas = FigureCanvas(self.imgFigure)
        
        self.intenGrp = QGroupBox("Set Intensities")
        self.intenGrp.setCheckable(False)
        self.intenLayout = QVBoxLayout(self.intenGrp)

        self.maxIntLabel = QLabel("Max intensity : ")
        self.maxInt = QDoubleSpinBox()
        self.maxInt.setSingleStep(5)
        self.maxInt.setDecimals(0)
        self.maxInt.setMaximum(999999999999999)

        self.minIntLabel = QLabel("Min intensity : ")
        self.minInt = QDoubleSpinBox()
        self.minInt.setSingleStep(5)
        self.minInt.setDecimals(0)
        self.minInt.setMaximum(999999999999999)

        self.intenLayout.addWidget(self.maxIntLabel)
        self.intenLayout.addWidget(self.maxInt)
        self.intenLayout.addWidget(self.minIntLabel)
        self.intenLayout.addWidget(self.minInt)

        self.buttonGrp = QGroupBox("Navigate Directory")
        self.buttonGrp.setCheckable(False)

        self.buttonLayout = QHBoxLayout(self.buttonGrp)

        self.nextFileButton = QPushButton(">>>")
        self.prevFileButton = QPushButton("<<<")

        self.buttonLayout.addWidget(self.prevFileButton)
        self.buttonLayout.addWidget(self.nextFileButton)

        self.intenLayout.addWidget(self.buttonGrp)

        self.processFolderButton = QPushButton("Process Folder")
        self.intenLayout.addWidget(self.processFolderButton)

        self.canvImgLayout.addWidget(self.imgCanvas)
        self.canvImgLayout.addWidget(self.intenGrp)

        self.mainLayout.addWidget(self.browseFileButton)
        self.mainLayout.addWidget(self.drawMaskButton)
        self.mainLayout.addWidget(self.canvImgGroup)

        self.statusBar = QStatusBar()
        self.imgDetailOnStatusBar = QLabel("  Please select an image or a folder to process")
        self.statusBar.addWidget(self.imgDetailOnStatusBar)
        self.setStatusBar(self.statusBar)

        # Menubar
        selectImageAction = QAction('Select an Image...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.triggered.connect(self.browseFile)
        selectFolderAction = QAction('Select a Folder...', self)
        selectFolderAction.setShortcut('Ctrl+F')
        selectFolderAction.triggered.connect(self.browseFile)
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)
        fileMenu.addAction(selectFolderAction)
        aboutAct = QAction('About', self)
        aboutAct.triggered.connect(self.showAbout)
        helpMenu = menubar.addMenu('&Help')
        helpMenu.addAction(aboutAct)

        self.show()
        self.resize(400,150)

    def setConnections(self):
        self.minInt.valueChanged.connect(self.refreshImage)
        self.maxInt.valueChanged.connect(self.refreshImage)

        self.nextFileButton.clicked.connect(self.nextFBClicked)
        self.prevFileButton.clicked.connect(self.prevFBClicked)

        self.processFolderButton.clicked.connect(self.makeCSV)

    def browseFile(self):
        """
        Popup input dialog and set file selection
        """
        print("Browse File")
        self.newProcess = True
        file_name = getAFile()
        print("Get a file returns:", file_name)

        if file_name != "":
            self.file_name = file_name
            self.onNewFileSelected(str(file_name))
            self.centralWidget.setMinimumSize(700, 500)

    def onNewFileSelected(self, file_name):
        # Retrieve directory and file list info
        self.dir_path, self.imgList, self.currentFileNumber, self.fileList, self.ext = getImgFiles(str(file_name))
        # Clear any previous HDF5 list and set mode
        self.h5List = []
        self.setH5Mode(str(file_name))
        # If in HDF5 mode, override imgList with h5List
        if self.h5Mode:
            self.imgList = self.h5List

        # Clear the figure before adding a new subplot
        self.imgFigure.clf()
        self.ax = self.imgFigure.add_subplot(111)
        print("Opening file:", file_name)
        self.img = fabio.open(str(file_name)).data

        max_inten = min(700, np.max(self.img) / 40)
        self.maxInt.setValue(max_inten)

        self.refreshImage()

    def showAbout(self):
        print("SHOW ABOUT CLICKED")

    def maskButtonClicked(self):
        self.imageMaskingTool = ImageMaskerWindow(
            self.dir_path, 
            self.file_name, 
            self.minInt.value(), 
            self.maxInt.value(), 
            max_val=np.max(np.ravel(self.img)), 
            orig_size=self.img.shape,
            trans_mat=None,                                                    
            rot_angle=None, 
            isHDF5=self.ext.lower() in ['h5', 'hdf5']
        )
        
        if self.imageMaskingTool is not None and self.imageMaskingTool.exec_():
            settings_path = join(join(self.dir_path, 'settings'))
            blank_json = join(settings_path, 'blank_image_settings.json')
            if os.path.exists(blank_json):
                with open(blank_json, 'r') as f:
                    info = json.load(f)
                    if 'path' in info:
                        img = fabio.open(info['path']).data
                        fabio.tifimage.tifimage(data=img).write(join(settings_path, 'blank.tif'))
            else:
                mask_tif = join(settings_path, 'mask.tif')
                if os.path.exists(mask_tif):
                    os.rename(mask_tif, join(settings_path, 'maskonly.tif'))

        self.buildMask()
            
    def refreshImage(self):
        if self.img is not None:
            self.imgDetailOnStatusBar.setText("Current File (" + str(self.currentFileNumber) + "/" + str(len(self.imgList)) + ") : " + self.file_name)

            if self.mask is None:
                self.buildMask()
            self.applyMask()

            self.ax.cla()
            self.masked_image = np.array(self.masked_image, dtype=np.float32)
            self.ax.imshow(self.masked_image, cmap='gray', norm=Normalize(vmin=self.minInt.value(), vmax=self.maxInt.value()))
            self.ax.invert_yaxis()
            self.imgCanvas.draw()

    def buildMask(self):
        self.mask = np.ones_like(self.img)

        blank, mask = getBlankImageAndMask(self.dir_path)
        maskOnly = getMaskOnly(self.dir_path)

        for m in [blank, mask, maskOnly]:
            if m is not None:
                self.mask = self.mask * m

        result_path = join(self.dir_path, 'tdi_results')
        if not exists(result_path):
            os.makedirs(result_path)
        fabio.tifimage.tifimage(data=self.mask).write(join(result_path, 'tdi_mask.tif'))

    def applyMask(self):
        if self.mask is None:
            self.masked_image = copy.copy(self.img)
        else:
            self.masked_image = self.mask * copy.copy(self.img)

    def nextFBClicked(self):
        print("Next button clicked")
        if self.h5Mode:
            if len(self.h5List) > 1:
                self.h5index = (self.h5index + 1) % len(self.h5List)
                next_file = os.path.join(self.dir_path, self.h5List[self.h5index])
                print("Switching to HDF5 file:", next_file)
                self.onNewFileSelected(next_file)
        else:  
            if len(self.imgList) > 0:
                self.currentFileNumber = (self.currentFileNumber + 1) % len(self.imgList)
                self.file_name = join(self.dir_path, self.imgList[self.currentFileNumber])
                print("Switching to image file:", self.file_name)
                self.img = fabio.open(str(self.file_name)).data
                self.maxInt.setValue(np.max(self.img) * 0.1)
                self.refreshImage()

    def prevFBClicked(self):
        print("Previous button clicked")
        if self.h5Mode:
            if len(self.h5List) > 1:
                self.h5index = (self.h5index - 1) % len(self.h5List)
                prev_file = os.path.join(self.dir_path, self.h5List[self.h5index])
                print("Switching to HDF5 file:", prev_file)
                self.onNewFileSelected(prev_file)
        else:
            if len(self.imgList) > 0:
                self.currentFileNumber = (self.currentFileNumber - 1) % len(self.imgList)
                self.file_name = join(self.dir_path, self.imgList[self.currentFileNumber])
                print("Switching to image file:", self.file_name)
                self.img = fabio.open(str(self.file_name)).data
                self.maxInt.setValue(np.max(self.img))
                self.refreshImage()

    def makeCSV(self):
        result_path = fullPath(self.dir_path, "tdi_results")
        if not exists(result_path):
            os.makedirs(result_path)

        csv_name = fullPath(result_path, 'summary.csv')
        colnames = ['ImageName', 'MaskFileName', 'TotalIntensity', 'AvgIntensity']

        rows = []

        for img in self.imgList:
            self.file_name = join(self.dir_path, img)
            if not os.path.exists(self.file_name):
                print(f"File {self.file_name} not found. Skipping.")
                continue

            self.img = fabio.open(str(self.file_name)).data

            # If the shape of image is different than the shape of mask, skip.
            try:
                self.applyMask()
            except Exception as e:
                print(f"Shape of {img} does not match the mask shape. Skipping. Exception: {e}")
                continue

            total_intensity = np.sum(np.ravel(self.masked_image))
            unmasked_pixels = self.masked_image.size - np.sum(np.ravel(1 - self.mask))

            new_row = {"ImageName": img, "MaskFileName": 'tdi_mask.tif', 
                       "TotalIntensity": total_intensity, 
                       "AvgIntensity": total_intensity / unmasked_pixels}
            
            rows.append(new_row)

        df = pd.DataFrame(rows)
        df.to_csv(csv_name, index=False, columns=colnames)

    def setH5Mode(self, file_name):
        """
        Sets the HDF5 list of files and displays the right set of buttons depending on the file selected.
        """
        if self.ext.lower() in ['.h5', '.hdf5']:
            self.h5Mode = True
            for file in os.listdir(self.dir_path):
                if file.endswith(".h5") or file.endswith(".hdf5"):
                    self.h5List.append(file)
            try:
                self.h5index = self.h5List.index(os.path.split(file_name)[1])
            except ValueError:
                self.h5index = 0
        else:
            self.h5Mode = False
