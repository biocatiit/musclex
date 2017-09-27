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
import matplotlib.pyplot as plt
from PyQt4 import QtCore, QtGui
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
import matplotlib.patches as patches

class LayerLineTab(QtGui.QWidget):
    """
    Fitting Tabs : left or right
    Display fitting graph and providing options
    """
    def __init__(self, parent, num):
        QtGui.QWidget.__init__(self)
        self.parent = parent
        self.num = num
        self.function = None
        self.syncUI = False
        self.need_update = True
        self.initUI()
        self.setAllToolTips()
        self.setConnections()

    def initUI(self):
        """
        Initial all GUIs including : 4 plots and result table
        """
        self.setContentsMargins(0, 0, 0, 0)
        self.tabLayout = QtGui.QHBoxLayout(self)
        self.graphFigure = plt.figure(facecolor='#606060')
        self.graphCanvas = FigureCanvas(self.graphFigure)
        self.tabLayout.addWidget(self.graphCanvas)

    def setAllToolTips(self):
        """
        Set Tooltips for widgets
        """

    def setConnections(self):
        """
        Set connection for interactive widgets
        """

    def clearFlags(self):
        self.need_update = True
        self.function = None

    def updateUI(self):
        if self.parent.layerProc is None or not self.need_update:
            return
        info = self.parent.layerProc.info
        hist = info['hists'][self.num]
        ax = self.graphFigure.add_subplot(111)
        ax.cla()
        ax.plot(hist, color='g')
        self.graphFigure.tight_layout()
        self.graphCanvas.draw()
        self.need_update = False