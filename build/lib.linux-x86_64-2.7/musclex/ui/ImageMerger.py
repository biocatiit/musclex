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

__author__ = 'Jiranun.J'

from .pyqt_utils import *
from os.path import exists, join
from threading import Thread
from musclex.utils.file_manager import getFilesAndHdf, createFolder
import fabio
from musclex.utils.image_processor import averageImages, rotateImage, getRotationAngle, getCenter
import musclex

class ImageMergerGUI(QMainWindow):
    """
    A class for GUI of Image Merger
    """

    def __init__(self):
        """
        Initial window
        """
        QWidget.__init__(self)
        self.img_list = []
        self.img_grps = []
        self.initUI()
        self.setConnections()

    def initUI(self):
        """
        initial all widgets
        """
        self.setWindowTitle("Muscle X Image Merger v." + musclex.__version__)
        self.centralWid = QWidget(self)
        self.setCentralWidget(self.centralWid)
        self.mainLayout = QGridLayout(self.centralWid)

        self.in_directory = QLineEdit()
        self.in_directory.setEnabled(False)
        self.select_in_folder = QPushButton("Browse")
        self.out_directory = QLineEdit()
        self.select_out_folder = QPushButton("Browse")

        self.frame_number = QSpinBox()
        self.frame_number.setRange(1, 30000)
        self.frame_number.setValue(3)

        self.detailGrp = QGroupBox("Logs")
        self.detailLayout = QVBoxLayout(self.detailGrp)
        self.detail = QPlainTextEdit()
        self.detail.setReadOnly(True)
        self.progressbar = QProgressBar()
        self.progressbar.setHidden(True)
        self.detailLayout.addWidget(self.detail)
        self.detailLayout.addWidget(self.progressbar)

        self.start_button = QPushButton("Start")

        self.rotateChkBx = QCheckBox("Rotate and Center Images?")
        self.rotateChkBx.setChecked(False)

        self.mainLayout.addWidget(QLabel("Input Directory : "), 0, 0, 1, 1)
        self.mainLayout.addWidget(self.in_directory, 0, 1, 1, 1)
        self.mainLayout.addWidget(self.select_in_folder, 0, 2, 1, 1)

        self.mainLayout.addWidget(QLabel("Output Directory : "), 1, 0, 1, 1)
        self.mainLayout.addWidget(self.out_directory, 1, 1, 1, 1)
        self.mainLayout.addWidget(self.select_out_folder, 1, 2, 1, 1)

        self.mainLayout.addWidget(QLabel("Number of frames to average : "), 2, 0, 1, 2, Qt.AlignRight)
        self.mainLayout.addWidget(self.frame_number, 2, 2, 1, 1)
        self.mainLayout.addWidget(self.detailGrp, 3, 0, 1, 3)
        self.mainLayout.addWidget(self.start_button, 4, 0, 1, 3, Qt.AlignCenter)

        self.mainLayout.addWidget(self.rotateChkBx, 2, 0, 1, 3)

        self.mainLayout.columnStretch(1)
        self.mainLayout.rowStretch(3)
        self.resize(800, 300)
        self.show()

    def setConnections(self):
        """
        Set handler for all widgets
        """
        self.select_in_folder.clicked.connect(self.browse_input)
        self.select_out_folder.clicked.connect(self.browse_output)
        self.frame_number.valueChanged.connect(self.updateImageGroups)
        self.start_button.clicked.connect(self.start_clicked)

    def browse_input(self):
        """
        Handle when Browse for input folder is clicked
        :return:
        """
        path = getAFolder()
        if len(path) > 0:
            self.in_directory.setText(path)
            self.out_directory.setText(join(path, 'merged_results'))
            self.preprocessfolder()

    def browse_output(self):
        """
        Handle when Browse for output folder is clicked
        :return:
        """
        path = getAFolder()
        if len(path) > 0:
            self.out_directory.setText(path)

    def preprocessfolder(self):
        """
        Get all image names in the selected folder
        """
        imgs, _ = getFilesAndHdf(str(self.in_directory.text()))
        self.img_list = sorted(imgs)
        self.updateImageGroups()


    def updateImageGroups(self):
        """
        Group images and update details
        """
        self.img_grps = self.splitImages()
        grps = self.img_grps
        self.detail.clear()
        detail = "Available Groups : \n"
        if len(grps) >= 1:
            for i in range(len(grps)):
                detail += "Group "+ str(i+1)+ " : " + str(grps[i][0]) + " ... " + str(grps[i][-1]) + '\n'

        self.detail.insertPlainText(detail)
        self.detail.moveCursor(QTextCursor.End)


    def splitImages(self):
        """
        Split images into groups
        :return: list of group
        """
        imgs = self.img_list
        frames = self.frame_number.value()
        grps = []
        for i in range(0, len(imgs), frames):
            grps.append(imgs[i:i + frames])

        return grps


    def start_clicked(self):
        """
        handle when Start is clicked
        :return:
        """
        if len(self.img_grps) > 0:
            if self.start_button.text() == 'Start':
                createFolder(str(self.out_directory.text()))
                self.processFolder()
        else:
            errMsg = QMessageBox()
            errMsg.setText('Number of images : 0')
            errMsg.setInformativeText('Please select another folder')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Abort)
            errMsg.exec_()

    def processFolder(self):
        """
        merging images
        :return:
        """
        input = str(self.in_directory.text())
        output = str(self.out_directory.text())
        self.progressbar.setHidden(False)
        self.progressbar.setRange(0, len(self.img_grps))
        self.detail.insertPlainText("\n\n--------------- Start ----------------\n\n")

        for i, imgs in enumerate(self.img_grps):
            if len(imgs) > 0:
                self.progressbar.setValue(i)
                first = imgs[0]
                last = imgs[-1]
                f_ind1 = first.rfind('_')
                f_ind2 = first.rfind('.')
                l_ind1 = last.rfind('_')
                l_ind2 = last.rfind('.')

                if f_ind1 == -1 or f_ind2 == -1 or l_ind1 == -1 or l_ind2 == -1 or first[:f_ind1] != last[:l_ind1]:
                    filename = "Group"+str(i+1)+'.tif'
                else:
                    filename = first[:f_ind1] + "_"
                    filename += first[f_ind1 + 1:f_ind2]
                    filename += "_"
                    filename += last[l_ind1 + 1:l_ind2]
                    filename += '.tif'

                # Update details
                details = "- Merging Group "+ str(i+1)+" : \n"
                for j in range(len(imgs)):
                    details += str(j+1)+". "+imgs[j]+"\n"
                details += "To : " + filename +'\n\n'

                self.detail.insertPlainText(details)
                self.detail.moveCursor(QTextCursor.End)
                QApplication.processEvents()
                full_imgs = list(map(lambda f: join(input, f), imgs))
                # forimg in full_imgs, rotate and center them
                avg = averageImages(full_imgs, rotate=self.rotateChkBx.isChecked())
                fabio.tifimage.tifimage(data=avg).write(join(output, filename))
            else:
                break

        self.detail.insertPlainText("Done. All result images have been saved to "+output)
        self.progressbar.setHidden(True)
