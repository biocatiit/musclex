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

from PyQt5 import QtGui
from PyQt5.QtCore import *
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
import matplotlib
matplotlib.use('Qt5Agg')
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
try:
    from ..utils.file_manager import input_types
except: # for coverage
    from utils.file_manager import input_types

print("Qt version:", QT_VERSION_STR)

img_filter = 'Pattern ('
for t in input_types:
    img_filter += ' *.' + str(t)
img_filter += ')'

def getAFile(filtr=None, path='', add_txt=False):
    """
    Open a file finder and return the name of the file selected
    """
    if filtr is None:
        filtr = img_filter
    if add_txt and filtr != '':
        filtr += ';;Failed cases (*.txt)'
    file_name = QFileDialog.getOpenFileName(None, 'Select a file', path, filtr, None)
    if isinstance(file_name, tuple):
        file_name = file_name[0]
    return str(file_name)

def getFiles(path='', filtr=None):
    """
    Give the list of the files in a folder
    """
    if filtr is None:
        filtr = img_filter
    fileList = QFileDialog.getOpenFileNames(None, "Select frame(s) to average", path, filtr)
    if isinstance(fileList, tuple):
        fileList = fileList[0]
    return list(map(str, fileList))

def getAFolder():
    """
    Open a folder finder and return the name of the folder selected
    """
    dir_path = QFileDialog.getExistingDirectory(None, "Select a Folder")
    return str(dir_path)

def getSaveFile(path='', filtr='Images (*.png);;SVG (*.svg)'):
    """
    Open a file finder and let the user save the file where he wants
    and return the file path
    """
    file_name = QFileDialog.getSaveFileName(None, "Save a file", path, filtr)
    if isinstance(file_name, tuple):
        file_name = file_name[0]
    return str(file_name)
