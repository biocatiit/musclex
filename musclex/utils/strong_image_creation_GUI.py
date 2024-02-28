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


import sys
import os
from PyQt5.QtWidgets import (
    QApplication,
    QWidget,
    QVBoxLayout,
    QPushButton,
    QLabel,
    QLineEdit,
    QFileDialog,
    QMessageBox,
)
from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QHBoxLayout


from utils.strong_image_headless import (
    create_strong_image,
)


class App(QWidget):
    def __init__(self):
        super().__init__()
        self.title = "Create Strong Image"
        self.initUI()

    def initUI(self):
        self.setWindowTitle(self.title)
        layout = QVBoxLayout()

        # Folder path input
        self.folderLabel = QLabel("Folder Path:")
        layout.addWidget(self.folderLabel)
        self.folderInput = QLineEdit(self)
        layout.addWidget(self.folderInput)
        self.folderButton = QPushButton("Browse Folder", self)
        self.folderButton.clicked.connect(self.openFolderDialog)
        layout.addWidget(self.folderButton)

        # Image selection input
        self.imagesLabel = QLabel("Select Start and End Images, holding Ctrl to select multiple images:")
        layout.addWidget(self.imagesLabel)
        self.imagesButton = QPushButton("Browse Images", self)
        self.imagesButton.clicked.connect(self.openImagesDialog)
        layout.addWidget(self.imagesButton)
        
        # Center the "or" label
        orLayout = QHBoxLayout()
        self.orLabel = QLabel("or")
        orLayout.addWidget(self.orLabel, 0, Qt.AlignHCenter)
        layout.addLayout(orLayout)
        
        # Indicate manual input for start and end image names:
        self.manualSelectionLabel = QLabel("Manually Input Start and End Image Names:")
        layout.addWidget(self.manualSelectionLabel)

        # Display selected start and end image names
        self.startImageLabel = QLabel("Start Image Name:")
        self.startImageInput = QLineEdit(self)
        layout.addWidget(self.startImageLabel)
        layout.addWidget(self.startImageInput)

        self.endImageLabel = QLabel("End Image Name:")
        self.endImageInput = QLineEdit(self)
        layout.addWidget(self.endImageLabel)
        layout.addWidget(self.endImageInput)

        # Strings to exclude input
        self.excludeLabel = QLabel("Strings in filenames to Exclude (comma-separated):")
        layout.addWidget(self.excludeLabel)
        self.excludeInput = QLineEdit(self)
        layout.addWidget(self.excludeInput)

        # Submit button
        self.submitButton = QPushButton("Create Strong Image", self)
        self.submitButton.clicked.connect(self.onSubmit)
        layout.addWidget(self.submitButton)

        self.setLayout(layout)

    def openFolderDialog(self):
        options = QFileDialog.Options()
        folder = QFileDialog.getExistingDirectory(
            self, "Select Directory", options=options
        )
        if folder:
            self.folderInput.setText(folder)

    def openImagesDialog(self):
        options = QFileDialog.Options()
        files, _ = QFileDialog.getOpenFileNames(
            self, "Select Images", "", "Images (*.tif *.tiff *.h5)", options=options
        )
        if files:
            # Assuming files are returned sorted, but you can sort them based on your criteria
            start_image = os.path.basename(files[0])
            end_image = os.path.basename(files[-1])
            self.startImageInput.setText(start_image)
            self.endImageInput.setText(end_image)

    def onSubmit(self):
        folder_path = self.folderInput.text()
        start_image_name = self.startImageInput.text()
        end_image_name = self.endImageInput.text()
        str_to_exclude = (
            [x.strip() for x in self.excludeInput.text().split(",")]
            if self.excludeInput.text()
            else []
        )

        try:
            # Assuming create_strong_image is correctly implemented elsewhere
            create_strong_image(
                folder_path, start_image_name, end_image_name, str_to_exclude
            )
            QMessageBox.information(
                self, "Success", "The strong image has been successfully created."
            )
        except Exception as e:
            QMessageBox.critical(self, "Error", f"An error occurred: {str(e)}")


if __name__ == "__main__":
    app = QApplication(sys.argv)
    ex = App()
    ex.show()
    sys.exit(app.exec_())
