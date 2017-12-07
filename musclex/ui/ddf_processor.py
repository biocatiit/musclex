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
from .pyqt_utils import *
import os, sys
import pandas as pd
import musclex

class DDFWindow(QMainWindow):
    def __init__(self):
        QWidget.__init__(self)
        self.setWindowTitle("Muscle X DDF-Processor v." + musclex.__version__)
        self.current_file = ""
        self.data = None
        self.colChkBxs = []
        self.initUI()
        self.setConnections()

    def initUI(self):
        self.centralWidget = QWidget(self)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        self.inputField = QLineEdit()
        self.inputField.setEnabled(False)

        self.browseFileButton = QPushButton("Browse")

        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Expanding)
        separator.setLineWidth(1)

        separator2 = QFrame()
        separator2.setFrameShape(QFrame.HLine)
        separator2.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Expanding)
        separator2.setLineWidth(1)

        self.columnGrid = QGridLayout()

        self.freqLayout = QHBoxLayout()
        self.freqSpnBx = QSpinBox()
        self.freqSpnBx.setValue(1)
        # self.freqSpnBx.setSuffix(" point(s)")
        self.freqLayout.addWidget(QLabel("3. Average every : "))
        self.freqLayout.addWidget(self.freqSpnBx)
        self.freqLayout.addWidget(QLabel("point(s)"))


        self.mainGrid = QGridLayout()
        self.mainGrid.addWidget(QLabel("1. Input file : "), 0, 0, 1, 1)
        self.mainGrid.addWidget(self.inputField, 0, 1, 1, 1)
        self.mainGrid.addWidget(self.browseFileButton, 0, 2, 1, 1)
        self.mainGrid.addWidget(separator, 1, 0, 1, 3)
        self.mainGrid.addWidget(QLabel("2. Column Selection"), 2, 0, 1, 3, alignment = Qt.AlignCenter)
        self.mainGrid.addLayout(self.columnGrid, 3, 0, 1, 3)
        self.mainGrid.addWidget(separator2, 4, 0, 1, 3)
        self.mainGrid.addLayout(self.freqLayout, 5, 0, 1, 3, alignment = Qt.AlignCenter)
        # self.mainGrid.addWidget(QLabel("Average every : "), 3, 1, 1, 1, alignment = Qt.AlignRight)
        # self.mainGrid.addWidget(self.freqSpnBx, 3, 2, 1, 1)
        self.columnGrid.rowMinimumHeight(50)
        self.generateButton = QPushButton("Generate")
        self.generateButton.setFixedWidth(150)
        self.generateButton.setEnabled(False)
        
        ### Status Bar ###
        self.statusBar = QStatusBar()
        self.statusText = QLabel("Please browse a data file")
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusBar.addWidget(self.statusText)
        self.statusBar.addPermanentWidget(self.progressBar)

        self.mainLayout.addLayout(self.mainGrid)
        self.mainLayout.addStretch()
        self.mainLayout.addWidget(self.generateButton)
        self.mainLayout.addWidget(self.statusBar)
        self.mainLayout.setAlignment(self.generateButton, Qt.AlignCenter)

        self.show()
        self.resize(700, 50)

    def setConnections(self):
        self.browseFileButton.clicked.connect(self.browseFile)
        self.generateButton.clicked.connect(self.generateFile)

    def browseFile(self):
        file_name = getAFile('')
        if file_name != "":
            _, ext = os.path.splitext(str(file_name))
            if ext == ".txt" or ext == ".ddf":
                self.current_file = file_name
                self.inputField.setText(file_name)
                self.processFile()
                self.updateUI()
            else:
                errMsg = QMessageBox()
                errMsg.setText('Invalid Input')
                errMsg.setInformativeText("Please select a .txt or .ddf file\n\n")
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                self.browseFile()

    def processFile(self):
        self.data = None
        cols = None
        reading = "Please wait. Input file is being read ."
        self.generateButton.setEnabled(False)
        QApplication.setOverrideCursor(Qt.WaitCursor)
        for i, row in enumerate(open(self.current_file)):
            if (i/100)%3 == 0:
                self.statusText.setText(reading)
            elif (i/100)%3 == 1:
                self.statusText.setText(reading + " .")
            else:
                self.statusText.setText(reading + " . .")
            QApplication.processEvents()

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
            line =  list(map(float, line))[:len(cols)]
            d = dict(zip(cols,line))
            self.data = self.data.append(d, ignore_index=True)

        # print self.data.head().to_string()
        QApplication.restoreOverrideCursor()
        self.generateButton.setEnabled(True)
        self.statusText.setText("Please select columns, adjust the average frequency, and click Generate")
        QApplication.processEvents()

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
            for i,col_name in enumerate(cols):
                r = i/3
                c = (i%3)
                col_cb = QCheckBox(col_name)
                self.colChkBxs.append(col_cb)
                self.columnGrid.addWidget(col_cb, r, c , 1, 1)
                self.columnGrid.setAlignment(col_cb, Qt.AlignCenter)
        self.resize(700, 50)

    def generateFile(self):
        cols = list(self.data.columns[:])
        cols.remove("Sample")
        selected_cols = []
        for i, c in enumerate(self.colChkBxs):
            if c.isChecked():
                selected_cols.append(cols[i])
        if len(selected_cols) == 0 :
            errMsg = QMessageBox()
            errMsg.setText('No column selected')
            errMsg.setInformativeText("Please select at least 1 column.\n\n")
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
            return
        genData = self.data.groupby(self.data.index / self.freqSpnBx.value()).mean()
        genData = genData[selected_cols]
        dir_path, _ = os.path.split(str(self.inputField.text()))
        output = getSaveFile(dir_path, "CSV (*.csv);; Excel (*.xlsx);; HTML (*.html)")

        if output != "":
            _, ext = os.path.splitext(output)
            if ext == ".xlsx":
                genData.to_excel(output, index=False)
            elif ext == ".html":
                genData.to_html(output, index=False)
            else:
                genData.to_csv(output, index=False)
            self.statusText.setText(output+" has been saved.")

if __name__ == "__main__":
    app = QApplication(sys.argv)
    myapp = DDFWindow()
    sys.exit(app.exec_())