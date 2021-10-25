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

import matplotlib
matplotlib.use('qt5agg')
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from PyQt5 import QtGui
from PyQt5.QtCore import *
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *


from ..utils.file_manager import input_types

print("Qt version:", QT_VERSION_STR)

img_filter = 'Pattern ('
for t in input_types:
    img_filter += ' *.' + str(t)
img_filter += ')'

def getAFile(filter=None, path = '', add_txt = False):
    if filter is None:
        filter = img_filter
    if add_txt and filter != '':
        filter += ';;Failed cases (*.txt)'
    file_name = QFileDialog.getOpenFileName(None, 'Select a file', path, filter, None)
    if isinstance(file_name, tuple):
        file_name = file_name[0]
    return str(file_name)

def getFiles(path='', filter=None):
    if filter is None:
        filter = img_filter
    fileList = QFileDialog.getOpenFileNames(None, "Select frame(s) to average", path, filter)
    if isinstance(fileList, tuple):
        fileList = fileList[0]
    return list(map(str, fileList))

def getAFolder():
    dir_path = QFileDialog.getExistingDirectory(None, "Select a Folder")
    return str(dir_path)

def getSaveFile(path='', filter='Images (*.png);;SVG (*.svg)'):
    file_name = QFileDialog.getSaveFileName(None, "Save a file", path, filter)
    if isinstance(file_name, tuple):
        file_name = file_name[0]
    return str(file_name)
def file_save(self):
    name = QtGui.QFileDialog.getSaveFileName(self, 'Save File')
    file = open(name,'w')
    text = self.textEdit.toPlainText()
    file.write(text)
    file.close()