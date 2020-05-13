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

import matplotlib.pyplot as plt
import fabio
from .pyqt_utils import *
import numpy as np
from os.path import join, exists
from ..utils.file_manager import getStyleSheet, createFolder
from ..utils.image_processor import get8bitImage, getBGR, getMaskThreshold, averageImages

try:
    import PyMca.MaskImageWidget as PyMcaMaskImageWidget
except ImportError:
    import PyMca5.PyMca.MaskImageWidget as PyMcaMaskImageWidget


class MaskImageWidget(QDialog):
    """
    Dialog for mask image settings
    """
    def __init__(self, image, mask):
        super(MaskImageWidget, self).__init__(None)
        self.image = image
        self.mask = mask
        self.initUI()

    def initUI(self):
        self.mainLayout = QVBoxLayout(self)
        self.maskImageWidget = PyMcaMaskImageWidget.MaskImageWidget()
        self.maskImageWidget.setImageData(self.image)
        if self.mask is not None:
            self.maskImageWidget.setSelectionMask(np.array(self.mask))
        self.maskImageWidget._hFlipIconSignal()
        self.bottons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)
        self.mainLayout.addWidget(self.maskImageWidget)
        self.mainLayout.addWidget(self.bottons)


    def okClicked(self):
        self.mask = self.maskImageWidget.getSelectionMask()
        self.accept()

class BlankImageSettings(QDialog):
    """
    Dialog for blank image settings
    """
    def __init__(self, dir_path):
        super(BlankImageSettings, self).__init__(None)
        self.setWindowTitle("Blank Image and Mask")
        self.dir_path = dir_path
        self.additional_mask = None
        self.selected = None
        self.mask = None
        self.loadCurrentSettings()
        self.initUI()
        self.setConnections()
        self.updateImage()

    def loadCurrentSettings(self):
        path = join(self.dir_path, 'settings')
        blank = join(path, 'blank.tif')
        mask = join(path, 'mask.tif')
        if exists(blank):
            self.selected = fabio.open(blank).data
            if exists(mask):
                self.mask = fabio.open(mask).data

    def initUI(self):
        # self.setStyleSheet(getStyleSheet())
        self.mainLayout = QGridLayout(self)
        self.imageFigure = plt.figure()
        self.imageCanvas = FigureCanvas(self.imageFigure)
        self.imageAxes = self.imageFigure.add_subplot(111)

        self.selectImage = QPushButton("Select Blank Image(s)")
        self.drawMask = QPushButton("Draw Additional Mask")
        self.drawMaskOnly = QPushButton("Draw Mask using Base image")
        self.drawMask.setEnabled(self.selected is not None)
        self.maskThres = QSpinBox()
        self.maskThres.setRange(-10, 10)
        self.maskThres.setKeyboardTracking(False)

        self.bottons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)

        #
        ### Status Bar ###
        #
        self.statusBar = QStatusBar()
        self.pixel_detail = QLabel()
        self.statusBar.addWidget(self.pixel_detail)
        # self.statusBar.addPermanentWidget(self.pixel_detail)

        self.mainLayout.addWidget(self.imageCanvas, 0, 0, 1, 4)
        self.mainLayout.addWidget(self.selectImage, 1, 0, 1, 1)
        self.mainLayout.addWidget(self.drawMaskOnly, 2, 0, 1, 1)
        self.mainLayout.addWidget(self.drawMask, 1, 1, 1, 1)
        self.mainLayout.addWidget(QLabel("Mask Threshold:"), 1, 2, 1, 1, Qt.AlignRight)
        self.mainLayout.addWidget(self.maskThres, 1, 3, 1, 1)
        self.mainLayout.addWidget(self.bottons, 2, 0, 1, 4,  Qt.AlignCenter)
        self.mainLayout.addWidget(self.statusBar, 3, 0, 1, 4, Qt.AlignCenter)

    def setConnections(self):
        """
        Set widget handlers
        """
        self.selectImage.clicked.connect(self.browseImage)
        self.drawMaskOnly.clicked.connect(self.drawMaskOnlyImage)
        self.drawMask.clicked.connect(self.launchDrawMask)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.onMotion)
        self.maskThres.valueChanged.connect(self.generateMask)
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)
        
    def drawMaskOnlyImage(self):
        """
        Select a base image on which a mask is drawn
        The mask drawn is saved in settings folder as 'maskonly.tif'
        """
        mask_imgpath = getFiles(path=self.dir_path)[0]
        print("mask image path " + str(mask_imgpath))
        self.selected = fabio.open(mask_imgpath).data
        self.drawMask.setEnabled(False)
        
        draw_dialog = MaskImageWidget(self.selected, self.mask)
        result = draw_dialog.exec_()
        if result == 1:
            self.additional_mask = draw_dialog.mask
            mask = np.zeros(self.selected.shape)
            thres = self.maskThres.value()
            mask[self.selected <= thres] = 1
            if self.additional_mask is not None:
                mask[self.additional_mask > 0] = 1
        
                path = join(self.dir_path, 'settings')
                createFolder(path)
                fabio.tifimage.tifimage(data=mask).write(join(path,'maskonly.tif'))
                self.accept()
        

    def browseImage(self):
        """
        Browse a blank image
        """
        img_list = getFiles(path=self.dir_path)
        if len(img_list) > 0:
            self.selected = averageImages(img_list)
            self.drawMask.setEnabled(True)
            self.maskThres.setRange(self.selected.min()-1, self.selected.max())
            if self.selected.shape == (1043, 981):
                img_type = "PILATUS"
            else:
                img_type = "NORMAL"
            self.maskThres.setValue(getMaskThreshold(self.selected, img_type))
            self.generateMask()

    def launchDrawMask(self):
        """
        Launch Draw Mask dialog and get mask from it
        """
        if self.selected is not None:
            draw_dialog = MaskImageWidget(self.selected, self.mask)
            result = draw_dialog.exec_()
            if result == 1:
                self.additional_mask = draw_dialog.mask
                self.generateMask()


    def okClicked(self):
        """
        Save blank image and mask to tif files
        """
        if self.selected is not None:
            path = join(self.dir_path, 'settings')
            createFolder(path)
            fabio.tifimage.tifimage(data=self.mask).write(join(path,'mask.tif'))
            if self.mask is not None:
                fabio.tifimage.tifimage(data=self.selected).write(join(path, 'blank.tif'))
            self.accept()
        else:
            self.reject()

    def generateMask(self):
        """
        Generate Mask from Mask Threshold and additional mask from drawing
        """
        if self.selected is not None:
            mask = np.zeros(self.selected.shape)
            thres = self.maskThres.value()
            mask[self.selected <= thres] = 1
            if self.additional_mask is not None:
                mask[self.additional_mask > 0] = 1
            self.mask = mask
            self.updateImage()

    def updateImage(self):
        """
        Update Image and plot all mask
        :return:
        """
        if self.selected is not None:
            img = get8bitImage(self.selected)
            img = getBGR(img)
            if self.mask is not None:
                img[self.mask == 1] = [255, 165, 0]
            self.imageAxes.cla()
            self.imageAxes.imshow(img)
            self.imageCanvas.draw()


    def onMotion(self, e):
        x = e.xdata
        y = e.ydata
        if x is None or y is None or self.selected is None:
            self.pixel_detail.setText("")
            return

        int_x = int(round(x))
        int_y = int(round(y))
        self.pixel_detail.setText('x='+str(int_x)+', y='+str(int_y)+', val='+str(self.selected[int_y,int_x]))


