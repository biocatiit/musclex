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

__author__ = 'Miguel Menendez, Jiranun.J'

from .pyqt_utils import *
import sys
from os.path import isfile, abspath
import os
import re
import argparse
from tifffile import imsave
import collections
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
import musclex
from .CPImageWindow import CPImageWindow
from .CPBatchWindow2 import CPBatchWindow

class AddIntensities(QMainWindow):
    resizeCompleted = pyqtSignal()

    def __init__(self):
        QWidget.__init__(self)
        self.widgetList = []
        self.initUI()

    def initUI(self):
        # self.setStyleSheet(getStyleSheet())
        self.setWindowTitle("Muscle X Add Intensities v." + musclex.__version__)
        self.centralWidget = QWidget(self)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        ## display browse file and folder buttons when program started
        self.browseFolderButton = QPushButton("Select a Folder...")
        self.browseFolderButton.clicked.connect(self.browseFolder)
        self.browseFolderButton.setFixedHeight(60)
        self.mainLayout.addWidget(self.browseFolderButton)

        # Menubar
        selectFolderAction = QAction('Select a Folder...', self)
        selectFolderAction.setShortcut('Ctrl+F')
        selectFolderAction.triggered.connect(self.browseFolder)
        menubar = self.menuBar()
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectFolderAction)

        self.show()
        self.resize(400,150)

    def removeWidget(self, win):
        if win in self.widgetList:
            idx = self.widgetList.index(win)
            del self.widgetList[idx]

    def onNewFileSelected(self, fullfilepath):
        filePath, fileName = os.path.split(fullfilepath)
        new_image_window = CPImageWindow(self, str(fileName), str(filePath))
        self.widgetList.append(new_image_window)

    def browseFile(self):
        file_name = getAFile()
        QApplication.processEvents()
        if file_name != "":
            self.onNewFileSelected(str(file_name))

    def resizeImage(self, img, res_size):
        print("Size mismatched, resizing image")
        if img.shape == res_size:
            return img
        h,b = img.shape
        resH, resB = res_size
        dH = resH - h
        dB = resB - b
        extraH = dH//2
        extraB = dB//2
        res_img = np.zeros((res_size))
        res_img[extraH:extraH+h, extraB:extraB+b] = img
        return res_img

    def addIntensities(self, numberToFilesMap, dir_path):
        createFolder(fullPath(dir_path, "ai_results"))
        for key in numberToFilesMap.keys():
            sum_img = 0
            for filename in numberToFilesMap[key]:
                img = fabio.open(filename).data
                if type(sum_img) != int and img.shape[0]>sum_img.shape[0]:
                    sum_img = self.resizeImage(sum_img, img.shape)
                elif type(sum_img) != int:
                    img = self.resizeImage(img, sum_img.shape)
                sum_img += img
            result_file = os.path.join(dir_path, 'ai_results/res_' + str(key) + '.tif')
            imsave(result_file, sum_img)
            print('Saved ', result_file)
            print('Resulting image shape ', sum_img.shape)

    def browseFolder(self):
        dir_path = QFileDialog.getExistingDirectory(self, "Select a Folder")
        if dir_path != "":
            numberToFilesMap = collections.defaultdict(list)
            for root, dirs, files in os.walk(dir_path):
                for filename in files:
                    if 'ai_results' not in root:
                        number = int(re.sub(r'[^0-9]', '', filename))
                        numberToFilesMap[number].append(os.path.join(root, filename))
            print(numberToFilesMap)
            self.addIntensities(numberToFilesMap, dir_path)
            msg = QMessageBox()
            msg.setInformativeText(
                "Completed Adding intensities, results saved in folder ai_results")
            msg.setStandardButtons(QMessageBox.Ok)
            msg.setWindowTitle("Finished Adding Intensities")
            msg.setStyleSheet("QLabel{min-width: 500px;}")
            msg.exec_()

if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument('-i', help='image file')
    parser.add_argument('-f', help="handle all images in the folder")
    # parser.add_argument('-hdf', help="hdf file for the folder")
    args = parser.parse_args()

    if args.i:
        full_path = abspath(args.i)
        if isfile(full_path):
            filepath, filename = os.path.split(full_path)
            app = QApplication(sys.argv)
            myapp = CPImageWindow(mainWin=None, image_name=filename, dir_path=filepath)
            sys.exit(app.exec_())
        else:
            print("ERROR: " + str(full_path) + " does not exist. Please select another image.")
    elif args.f:
        full_path = abspath(args.f)
        if exists(full_path) and not isfile(full_path):
            app = QApplication(sys.argv)
            myapp = CPImageWindow(mainWin=None, image_name="", dir_path=full_path, process_folder=True)
            sys.exit(app.exec_())
        else:
            print("ERROR: " + str(full_path)+ " is not a folder.")
    else:
        app = QApplication(sys.argv)
        myapp = ScanningDiffractionGUI()
        sys.exit(app.exec_())
