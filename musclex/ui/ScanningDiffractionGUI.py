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

from musclex import __version__
from .pyqt_utils import *
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
from .DIImageWindow import DIImageWindow
from .DIBatchWindow import DIBatchWindow

class ScanningDiffractionGUI(QMainWindow):
    """
    A class for window displaying all information of a selected image.
    """
    resizeCompleted = Signal()
    def __init__(self):
        super().__init__()
        self.widgetList = []
        self.initUI()

    def initUI(self):
        """
        Initialize the UI
        """
        self.setWindowTitle("Muscle X Scanning Diffraction v." + __version__)
        self.centralWidget = QWidget(self)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        ## display browse file and folder buttons when program started
        self.browseFileButton = QPushButton("Select an Image...")
        self.browseFileButton.clicked.connect(self.browseFile)
        self.browseFileButton.setFixedHeight(60)
        self.browseFolderButton = QPushButton("Select a Folder...")
        self.browseFolderButton.clicked.connect(self.browseFolder)
        self.browseFolderButton.setFixedHeight(60)
        self.mainLayout.addWidget(self.browseFileButton)
        self.mainLayout.addWidget(self.browseFolderButton)

        # Menubar
        selectImageAction = QAction('Select an Image...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.triggered.connect(self.browseFile)
        selectFolderAction = QAction('Select a Folder...', self)
        selectFolderAction.setShortcut('Ctrl+F')
        selectFolderAction.triggered.connect(self.browseFolder)
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

    def removeWidget(self, win):
        """
        Remove a widget from the current window.
        :param win: the widget to remove
        """
        if win in self.widgetList:
            idx = self.widgetList.index(win)
            del self.widgetList[idx]

    def onNewFileSelected(self, fullfilepath):
        """
        Triggered when a new file is selected.
        :param fullfilepath: the path of the selected file
        """
        filePath, fileName = os.path.split(fullfilepath)
        new_image_window = DIImageWindow(self, str(fileName), str(filePath))
        self.widgetList.append(new_image_window)

    def browseFile(self):
        """
        Open a window to browse files.
        """
        file_name = getAFile()
        QApplication.processEvents()
        if file_name != "":
            self.onNewFileSelected(str(file_name))

    def browseFolder(self):
        """
        Open a window to browse folders.
        """
        dir_path = QFileDialog.getExistingDirectory(self, "Select a Folder")
        if dir_path != "":
            new_batch_window = DIBatchWindow(self, str(dir_path))
            self.widgetList.append(new_batch_window)

    def showAbout(self):
        """
        Display About Dialog
        """
        msgBox = QMessageBox()
        msgBox.setWindowTitle("About")
        msgBox.setTextFormat(Qt.RichText)
        msgBox.setText("<br><br><br>" +
                       "Scanning Diffraction is running under" +
                       "<h2>Muscle X v" +
                       __version__ +
                       "</h2><br><br>" +
                       "&copy;2023 BioCAT <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://www.bio.aps.anl.gov/") +
                       "Documentation : <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://musclex.readthedocs.io/en/latest/") +
                       "GitHub : <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://github.com/biocatiit/musclex") +
                       "Send Feedback or Issues : <br>" +
                       "<a href='{0}'>{0}</a><br><br>".format("https://github.com/biocatiit/musclex/issues"))
        msgBox.setStandardButtons(QMessageBox.Ok)
        msgBox.exec_()
