#!/usr/bin/python
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
        arguments = [arguments[0], 'qf']
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
