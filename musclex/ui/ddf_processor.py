__author__ = 'Jiranun.J'
VERSION = '1.0'

from PyQt4 import QtGui, QtCore
import os, sys
import pandas as pd

class DDFWindow(QtGui.QMainWindow):
    def __init__(self):
        QtGui.QWidget.__init__(self)
        self.setWindowTitle("DDF-Processor v." + VERSION)
        self.current_file = ""
        self.data = None
        self.colChkBxs = []
        self.initUI()
        self.setConnections()

    def initUI(self):
        self.centralWidget = QtGui.QWidget(self)
        self.mainLayout = QtGui.QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        self.inputField = QtGui.QLineEdit()
        self.inputField.setEnabled(False)

        self.browseFileButton = QtGui.QPushButton("Browse")

        separator = QtGui.QFrame()
        separator.setFrameShape(QtGui.QFrame.HLine)
        separator.setSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        separator.setLineWidth(1)

        separator2 = QtGui.QFrame()
        separator2.setFrameShape(QtGui.QFrame.HLine)
        separator2.setSizePolicy(QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        separator2.setLineWidth(1)

        self.columnGrid = QtGui.QGridLayout()

        self.freqLayout = QtGui.QHBoxLayout()
        self.freqSpnBx = QtGui.QSpinBox()
        self.freqSpnBx.setValue(1)
        # self.freqSpnBx.setSuffix(" point(s)")
        self.freqLayout.addWidget(QtGui.QLabel("3. Average every : "))
        self.freqLayout.addWidget(self.freqSpnBx)
        self.freqLayout.addWidget(QtGui.QLabel("point(s)"))


        self.mainGrid = QtGui.QGridLayout()
        self.mainGrid.addWidget(QtGui.QLabel("1. Input file : "), 0, 0, 1, 1)
        self.mainGrid.addWidget(self.inputField, 0, 1, 1, 1)
        self.mainGrid.addWidget(self.browseFileButton, 0, 2, 1, 1)
        self.mainGrid.addWidget(separator, 1, 0, 1, 3)
        self.mainGrid.addWidget(QtGui.QLabel("2. Column Selection"), 2, 0, 1, 3, alignment = QtCore.Qt.AlignCenter)
        self.mainGrid.addLayout(self.columnGrid, 3, 0, 1, 3)
        self.mainGrid.addWidget(separator2, 4, 0, 1, 3)
        self.mainGrid.addLayout(self.freqLayout, 5, 0, 1, 3, alignment = QtCore.Qt.AlignCenter)
        # self.mainGrid.addWidget(QtGui.QLabel("Average every : "), 3, 1, 1, 1, alignment = QtCore.Qt.AlignRight)
        # self.mainGrid.addWidget(self.freqSpnBx, 3, 2, 1, 1)
        self.columnGrid.rowMinimumHeight(50)
        self.generateButton = QtGui.QPushButton("Generate")
        self.generateButton.setFixedWidth(150)
        self.generateButton.setEnabled(False)
        
        ### Status Bar ###
        self.statusBar = QtGui.QStatusBar()
        self.statusText = QtGui.QLabel("Please browse a data file")
        self.progressBar = QtGui.QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusBar.addWidget(self.statusText)
        self.statusBar.addPermanentWidget(self.progressBar)

        self.mainLayout.addLayout(self.mainGrid)
        self.mainLayout.addStretch()
        self.mainLayout.addWidget(self.generateButton)
        self.mainLayout.addWidget(self.statusBar)
        self.mainLayout.setAlignment(self.generateButton, QtCore.Qt.AlignCenter)

        self.show()
        self.resize(700, 50)

    def setConnections(self):
        self.browseFileButton.clicked.connect(self.browseFile)
        self.generateButton.clicked.connect(self.generateFile)

    def browseFile(self):
        file_name = str(QtGui.QFileDialog.getOpenFileName(self, "Select a File"))
        if file_name != "":
            _, ext = os.path.splitext(str(file_name))
            if ext == ".txt" or ext == ".ddf":
                self.current_file = file_name
                self.inputField.setText(file_name)
                self.processFile()
                self.updateUI()
            else:
                errMsg = QtGui.QMessageBox()
                errMsg.setText('Invalid Input')
                errMsg.setInformativeText("Please select a .txt or .ddf file\n\n")
                errMsg.setStandardButtons(QtGui.QMessageBox.Ok)
                errMsg.setIcon(QtGui.QMessageBox.Warning)
                errMsg.exec_()
                self.browseFile()

    def processFile(self):
        self.data = None
        cols = None
        reading = "Please wait. Input file is being read ."
        self.generateButton.setEnabled(False)
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
        for i, row in enumerate(open(self.current_file)):
            if (i/100)%3 == 0:
                self.statusText.setText(reading)
            elif (i/100)%3 == 1:
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
            line =  list(map(float, line))[:len(cols)]
            d = dict(zip(cols,line))
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
            for i,col_name in enumerate(cols):
                r = i/3
                c = (i%3)
                col_cb = QtGui.QCheckBox(col_name)
                self.colChkBxs.append(col_cb)
                self.columnGrid.addWidget(col_cb, r, c , 1, 1)
                self.columnGrid.setAlignment(col_cb, QtCore.Qt.AlignCenter)
        self.resize(700, 50)

    def generateFile(self):
        cols = list(self.data.columns[:])
        cols.remove("Sample")
        selected_cols = []
        for i, c in enumerate(self.colChkBxs):
            if c.isChecked():
                selected_cols.append(cols[i])
        if len(selected_cols) == 0 :
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
        output = str(QtGui.QFileDialog.getSaveFileName(self, "Save an output file", dir_path, "CSV (*.csv);; Excel (*.xlsx);; HTML (*.html)"))
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
    app = QtGui.QApplication(sys.argv)
    myapp = DDFWindow()
    sys.exit(app.exec_())