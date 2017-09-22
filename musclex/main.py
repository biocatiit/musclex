#!/usr/bin/python

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

from musclex.ui.BMStartWindow import BMStartWindow
from musclex.ui.QuadrantFoldingGUI import QuadrantFoldingGUI
from musclex.ui.diffraction_centroids import DiffractionCentroidStartWindow
from musclex.ui.circular_projection_v2 import CircularProjectionGUI
from musclex.ui.ddf_processor import DDFWindow
import sys
from PyQt4 import QtGui

def main(arguments=None):
    if arguments is None:
        arguments = sys.argv
    if len(arguments) == 1:
        arguments = [arguments[0], 'bm']
    if len(arguments) == 2:
        prog = arguments[1]
        if prog == 'bm':
            app = QtGui.QApplication(sys.argv)
            myapp = BMStartWindow()
            sys.exit(app.exec_())
        elif prog == 'qf':
            app = QtGui.QApplication(sys.argv)
            myapp = QuadrantFoldingGUI()
            sys.exit(app.exec_())
        elif prog == 'cp':
            app = QtGui.QApplication(sys.argv)
            myapp = CircularProjectionGUI()
            sys.exit(app.exec_())
        elif prog == 'dc':
            app = QtGui.QApplication(sys.argv)
            myapp = DiffractionCentroidStartWindow()
            sys.exit(app.exec_())
        elif prog == 'ddf':
            app = QtGui.QApplication(sys.argv)
            myapp = DDFWindow()
            sys.exit(app.exec_())
        else:
            print "Please specify correct program shortcut i.e. bm, qf, cp"
    else:
        print "please specify correct program shortcut i.e. bm, qf, cp"

if __name__ == "__main__":
    main(sys.argv)
