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
from PyQt4 import QtCore, QtGui
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
import os, sys
import pandas as pd
import musclex

class LayerLineProcessorGUI(QtGui.QMainWindow):
    def __init__(self):
        QtGui.QWidget.__init__(self)
        self.setWindowTitle("Layer Line Processor v." + musclex.__version__)
        self.current_file = 0
        self.imgList = []
        self.layerProc = None
        self.checkableButtons = []
        self.initUI()
        # self.setConnections()


    def initUI(self):
        self.centralWidget = QtGui.QWidget(self)
        self.mainLayout = QtGui.QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        self.tabWidget = QtGui.QTabWidget()
        self.tabWidget.setTabPosition(QtGui.QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 20px; width: 200px; }")

        self.imageTab = QtGui.QWidget()
        self.imageTabLayer = QtGui.QHBoxLayout(self.imageTab)
        self.displayImgFigure = plt.figure(facecolor='#606060')
        self.imageVLayout = QtGui.QVBoxLayout()
        self.displayImgCanvas = FigureCanvas(self.displayImgFigure)
        self.imageVLayout.addWidget(self.displayImgCanvas)


        self.imageLeftFrame = QtGui.QFrame()
        self.imageLeftFrame.setFixedWidth(200)
        self.leftFrameLayout = QtGui.QVBoxLayout(self.imageLeftFrame)

        self.browseImageButton = QtGui.QPushButton("Browse")
        self.selectBoxButton = QtGui.QPushButton("Select Layer Line Box")
        self.selectBoxButton.setCheckable(True)
        self.checkableButtons.append(self.selectBoxButton)
        self.selectPeaksButton = QtGui.QPushButton("Select Peak Locations")
        self.selectPeaksButton.setCheckable(True)
        self.checkableButtons.append(self.selectPeaksButton)

        self.leftFrameLayout.addWidget(QtGui.QLabel("1. Select an image"))
        self.leftFrameLayout.addWidget(self.browseImageButton)
        self.leftFrameLayout.addWidget(QtGui.QLabel("2. Select layer line box"))
        self.leftFrameLayout.addWidget(self.selectBoxButton)
        self.leftFrameLayout.addWidget(QtGui.QLabel("3. Select Peaks"))
        self.leftFrameLayout.addWidget(self.selectPeaksButton)

        self.imageTabLayer.addWidget(self.imageLeftFrame)
        self.imageTabLayer.addWidget(self.displayImgCanvas)

        self.show()
        self.resize(700, 50)

    def setConnections(self):
        self.browseFileButton.clicked.connect(self.browseFile)

    def browseFile(self, cancel_to_close = False):
        file_name = str(QtGui.QFileDialog.getOpenFileName(self, 'Open File', '', 'Images (*.tif)', None))
        if file_name != "":
            self.onImageSelect(file_name)
        elif cancel_to_close:
            sys.exit()

    def processFile(self):
        self.data = None
        cols = None
        reading = "Please wait. Input file is being read ."
        self.generateButton.setEnabled(False)
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
        for i, row in enumerate(open(self.current_file)):
            if (i / 100) % 3 == 0:
                self.statusText.setText(reading)
            elif (i / 100) % 3 == 1:
                self.statusText.setText(reading + " .")
            else:
                self.statusText.setText(reading + " . .")
            QtGui.QApplication.processEvents()

            if "Sample" in row and "Stim" in row:
                r = row.rstrip("\n")
                r = r.rstrip("\r")
                cols = r.split("\t")
                self.data = pd.DataFrame(columns=cols)
                continue

            if self.data is None:
                continue

            r = row.rstrip("\n")
            r = r.rstrip("\r")
            line = r.split("\t")
            line = list(map(float, line))[:len(cols)]
            d = dict(zip(cols, line))
            self.data = self.data.append(d, ignore_index=True)

        # print self.data.head().to_string()
        QtGui.QApplication.restoreOverrideCursor()
        self.generateButton.setEnabled(True)
        self.statusText.setText("Please select columns, adjust the average frequency, and click Generate")
        QtGui.QApplication.processEvents()

    def updateUI(self):
        if len(self.colChkBxs) > 0:
            for c in self.colChkBxs:
                c.deleteLater()
                del c

        self.colChkBxs = []
        self.freqSpnBx.setRange(1, self.data.shape[0] - 1)

        if self.data is not None:
            cols = list(self.data.columns)
            cols.remove("Sample")
            for i, col_name in enumerate(cols):
                r = i / 3
                c = (i % 3)
                col_cb = QtGui.QCheckBox(col_name)
                self.colChkBxs.append(col_cb)
                self.columnGrid.addWidget(col_cb, r, c, 1, 1)
                self.columnGrid.setAlignment(col_cb, QtCore.Qt.AlignCenter)
        self.resize(700, 50)

    def generateFile(self):
        cols = list(self.data.columns[:])
        cols.remove("Sample")
        selected_cols = []
        for i, c in enumerate(self.colChkBxs):
            if c.isChecked():
                selected_cols.append(cols[i])
        if len(selected_cols) == 0:
            errMsg = QtGui.QMessageBox()
            errMsg.setText('No column selected')
            errMsg.setInformativeText("Please select at least 1 column.\n\n")
            errMsg.setStandardButtons(QtGui.QMessageBox.Ok)
            errMsg.setIcon(QtGui.QMessageBox.Warning)
            errMsg.exec_()
            return
        genData = self.data.groupby(self.data.index / self.freqSpnBx.value()).mean()
        genData = genData[selected_cols]
        dir_path, _ = os.path.split(str(self.inputField.text()))
        output = str(QtGui.QFileDialog.getSaveFileName(self, "Save an output file", dir_path,
                                                       "CSV (*.csv);; Excel (*.xlsx);; HTML (*.html)"))
        if output != "":
            _, ext = os.path.splitext(output)
            if ext == ".xlsx":
                genData.to_excel(output, index=False)
            elif ext == ".html":
                genData.to_html(output, index=False)
            else:
                genData.to_csv(output, index=False)
            self.statusText.setText(output + " has been saved.")
