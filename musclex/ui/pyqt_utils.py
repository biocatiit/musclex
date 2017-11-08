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

try:
    from PyQt4.QtCore import *
    from PyQt4.QtGui import *
    from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
except ImportError:
    try:
        from PyQt5.QtCore import *
        from PyQt5.QtWidgets import *
        from PyQt5.QtGui import *
        from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
    except ImportError:
        print "Please install PyQt4 or PyQt5"


def getAFile(filter='Images (*.tif)', path = ''):
    file_name = QFileDialog.getOpenFileName(None, 'Select a file', path, filter, None)
    if isinstance(file_name, tuple):
        file_name = file_name[0]
    return str(file_name)

def getFiles(path='', filter='Images (*.tif)'):
    fileList = QFileDialog.getOpenFileNames(None, "Select frame(s) to average", path, filter)
    if isinstance(fileList, tuple):
        fileList = fileList[0]
    return map(str, fileList)

def getAFolder():
    dir_path = QFileDialog.getExistingDirectory(None, "Select a Folder")
    return str(dir_path)
