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
import sys
from musclex import __version__
from musclex.ui.pyqt_utils import *


def main(arguments=None):
    if arguments is None:
        arguments = sys.argv

    run = True
    if len(arguments) == 2:
        prog = arguments[1]
        if prog == 'bm':
            app = QApplication(sys.argv)
            from musclex.ui.BMStartWindow import BMStartWindow
            myapp = BMStartWindow()
            sys.exit(app.exec_())
        elif prog == 'qf':
            app = QApplication(sys.argv)
            from musclex.ui.QuadrantFoldingGUI import QuadrantFoldingGUI
            myapp = QuadrantFoldingGUI()
            sys.exit(app.exec_())
        elif prog == 'cp':
            app = QApplication(sys.argv)
            from musclex.ui.circular_projection_v2 import CircularProjectionGUI
            myapp = CircularProjectionGUI()
            sys.exit(app.exec_())
        elif prog == 'dc':
            from musclex.ui.diffraction_centroids import DiffractionCentroidStartWindow
            app = QApplication(sys.argv)
            myapp = DiffractionCentroidStartWindow()
            sys.exit(app.exec_())
        elif prog == 'ddf':
            from musclex.ui.ddf_processor import DDFWindow
            app = QApplication(sys.argv)
            myapp = DDFWindow()
            sys.exit(app.exec_())
        elif prog == 'pt':
            from musclex.ui.ProjectionTracesGUI import ProjectionTracesGUI
            app = QApplication(sys.argv)
            myapp = ProjectionTracesGUI()
            sys.exit(app.exec_())
        else:
            run = False
    else:
        run = False

    if not run:
        print "\nYou're using Muscle X version", __version__
        print "\nPlease specify the program shortcut that you want to run"
        print ""
        print "  $ musclex [--program]"
        print ""
        print "          bm - Bio Muscle"
        print "          qf - Quadrant Folding"
        print "          pt - Projection Traces"
        print "          cp - Circular Projection"
        print "          dc - Diffraction Centroids"
        print "          ddf - DDF Processor"
        print ""
        print "For example,"
        print "\t$ musclex bm"
        print
        print "More details : https://www.github.com/biocatiit/musclex/wiki"
        print "Submit Feedback or issues : https://www.github.com/biocatiit/musclex/issues\n\n"

if __name__ == "__main__":
    main(sys.argv)
