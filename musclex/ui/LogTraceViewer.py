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

from decimal import Decimal
import matplotlib.patches as patches
import matplotlib.pyplot as plt
from ..utils.file_manager import *
from ..utils.image_processor import *
from .pyqt_utils import *

class LogTraceViewer(QMainWindow):
    """
    A class for window displaying all information of a selected image.
    This window contains 2 tabs : image, and graph
    """
    def __init__(self, parent, currentFileNumber):
        """
        Initial window
        """
        super().__init__()
        self.currentFileNumber = currentFileNumber
        self.filePath = "" # current directory
        self.parentSig = parent.currSaved
        self.numberOfFiles = 0
        self.img_zoom = None # zoom location of original image (x,y range)
        self.default_img_zoom = None # default zoom calculated after processing image
        self.graph_zoom = None # zoom location of result image (x,y range)
        self.function = None # current active function
        self.uiUpdating = False # update ui status flag (prevent recursive)
        self.plot_min = None
        self.length_out = []
        self.length_in = []
        self.force = []

        self.initUI() # initial all GUI
        self.setConnections() # set triggered function for widgets
        self.setFixedHeight(400)
        self.setMinimumWidth(1000)

    def initUI(self):
        """
        Open a file finder and return the name of the file selected
        """
        self.setWindowTitle("Log Trace")

        self.centralWidget = QWidget(self)
        self.setCentralWidget(self.centralWidget)
        self.mainHLayout = QHBoxLayout(self.centralWidget)

        ##### Image Tab #####
        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QHBoxLayout(self.imageTab)
        self.mainHLayout.addWidget(self.imageTab)

        self.verImgLayout = QVBoxLayout()
        self.verImgLayout.setContentsMargins(0, 0, 0, 0)
        self.verImgLayout.setAlignment(Qt.AlignCenter)

        self.imageFigure = plt.figure()
        self.imageAxes = self.imageFigure.add_subplot(111)
        self.imageCanvas = FigureCanvas(self.imageFigure)
        self.imageCanvas.setHidden(True)

        self.imageTabLayout.addLayout(self.verImgLayout)
        self.imageTabLayout.addWidget(self.imageCanvas)

        self.selectLogButton = QPushButton('Select a Log Trace...')
        self.selectLogButton.setFixedHeight(100)
        self.selectLogButton.setFixedWidth(300)
        self.verImgLayout.addWidget(self.selectLogButton)

        #### Status bar #####
        self.statusBar = QStatusBar()
        self.statusReport = QLabel()
        self.imgDetailOnStatusBar = QLabel()
        self.imgCoordOnStatusBar = QLabel()
        self.imgPathOnStatusBar = QLabel()
        self.imgPathOnStatusBar.setText("  Please select an image or a folder to process")
        self.statusBar.addPermanentWidget(self.statusReport)
        self.statusBar.addPermanentWidget(self.imgCoordOnStatusBar)
        self.statusBar.addPermanentWidget(self.imgDetailOnStatusBar)
        self.statusBar.addWidget(QLabel("    "))
        self.statusBar.addWidget(self.imgPathOnStatusBar)
        self.setStatusBar(self.statusBar)

        #### Menu Bar #####
        selectImageAction = QAction('Select a log file...', self)
        selectImageAction.setShortcut('Ctrl+I')
        selectImageAction.triggered.connect(self.browseFile)

        menubar = self.menuBar()
        # menubar.setNativeMenuBar(False)
        fileMenu = menubar.addMenu('&File')
        fileMenu.addAction(selectImageAction)

        self.show()

    def setConnections(self):
        """
        Set all triggered functions for widgets
        """
        self.selectLogButton.clicked.connect(self.browseFile)
        self.parentSig.connect(self.showLine)
        self.imageFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.imageFigure.canvas.mpl_connect('motion_notify_event', self.plotOnMotion)
        self.imageFigure.canvas.mpl_connect('button_release_event', self.plotReleased)
        self.imageFigure.canvas.mpl_connect('scroll_event', self.plotScrolled)

    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard
        """
        key = event.key()

        if key == Qt.Key_Right:
            self.nextClicked()
        elif key == Qt.Key_Left:
            self.prevClicked()
        elif key == Qt.Key_Escape:
            self.refreshAllTabs()

    def showLine(self, signal):
        self.currentFileNumber = signal
        self.imageAxes.lines[-1].remove()
        self.imageAxes.axvline(x=signal, color='r')
        self.imageCanvas.draw()
        self.resetStatusbar()

    def plotClicked(self, event):
        """
        Triggered when mouse presses on the graph in fitting tab
        """
        x = event.xdata
        y = event.ydata
        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.imgDetailOnStatusBar.setText("")
            ax = self.imageAxes
            bounds = ax.get_window_extent().get_points() ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[1] - ylim[0]) / (bounds[0][1] - bounds[1][1]) ### todo
            cy = ylim[0] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])

        # Provide different behavior depending on current active function
        if self.function is None:
            self.function = ["g_move", (x, y)]
        else:
            func = self.function
            if func[0] == "g_zoomin":
                func.append((x,y))
                if len(func) == 3:
                    # Set new zoom location nad update graph
                    p1 = func[1]
                    p2 = func[2]
                    self.graph_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.refreshAllTabs()

    def plotOnMotion(self, event):
        """
        Triggered when mouse hovers on the graph in fitting tab
        """
        x = event.xdata
        y = event.ydata
        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.imgDetailOnStatusBar.setText("")
            ax = self.imageAxes
            bounds = ax.get_window_extent().get_points() ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[1] - ylim[0]) / (bounds[0][1] - bounds[1][1]) ### todo
            cy = ylim[0] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])
        else:
            # Display plot information if the cursor is on the graph
            if round(x) < len(self.force):
                l_in = '%.4E' % Decimal(str(self.length_in[round(x)]))
                l_out = '%.4E' % Decimal(str(self.length_out[round(x)]))
                force = '%.4E' % Decimal(str(self.force[round(x)]))
                self.imgDetailOnStatusBar.setText("L_in=" + l_in + ', L_out=' + l_out + ', F=' + force + ', t=' + str(round(x) + 1))

        if self.function is None or len(self.function) < 2:
            return

        func = self.function
        if func[0] == "g_zoomin":
            # Draw rectangle
            ax = self.imageAxes

            for i in range(len(ax.patches)-1,-1,-1):
                ax.patches[i].remove()
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.imageCanvas.draw_idle()
        elif func[0] == "g_move":
            # Change zoom location (x, y ranges) in order to go round plot by dragging
            if self.graph_zoom is not None:
                hist = self.force
                ax = self.imageAxes
                move = (func[1][0] - x, func[1][1] - y)
                self.graph_zoom = getNewZoom(self.graph_zoom, move, len(hist), max(hist)*1.1, self.plot_min)
                ax.set_xlim(self.graph_zoom[0])
                ax.set_ylim(self.graph_zoom[1])
                self.imageCanvas.draw_idle()

    def plotReleased(self, event):
        """
        Triggered when mouse releases from graph
        """
        if self.function is not None and self.function[0] == "g_move":
            self.function = None

    def leavePlot(self, event):
        """
        Clear plot information when mouse leaves the graph
        """
        self.imgDetailOnStatusBar.setText("")

    def plotScrolled(self, event):
        """
        This function is called when a mouse scrolled on the graph in fitting tab. This will affect zoom-in and zoom-out
        """
        if event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        max_size = (max(self.force) * 1.1, len(self.force))
        ax = self.imageAxes

        if self.graph_zoom is None:
            self.graph_zoom = [ax.get_xlim(), ax.get_ylim()]

        zoom_height = self.graph_zoom[1][1] - self.graph_zoom[1][0]
        zoom_width = self.graph_zoom[0][1] - self.graph_zoom[0][0]
        clicked_x_percentage = 1. * (x - self.graph_zoom[0][0]) / zoom_width
        clicked_y_percentage = 1. * (y - self.graph_zoom[1][0]) / zoom_height
        step_x = .1 * zoom_width
        step_y = .1 * zoom_height
        if direction == 'up' : # zoom in
            step_x *= -1
            step_y *= -1
        zoom_width = min(max_size[1], max(zoom_width + step_x, 5))
        zoom_height = min(max_size[0], max(zoom_height + step_y, 20))

        x1 = x - clicked_x_percentage * zoom_width
        x2 = x1 + zoom_width
        y1 = y - clicked_y_percentage * zoom_height
        y2 = y1 + zoom_height

        if x1 < 0:
            x1 = 0
            x2 = zoom_width
        if y1 < self.plot_min:
            y1 = self.plot_min
            y2 = self.plot_min + zoom_height
        if x2 > max_size[1]:
            x2 = max_size[1]
            x1 = max_size[1] - zoom_width
        if y2 > max_size[0]:
            y2 = max_size[0]
            y1 = max_size[0] - zoom_height

        # Set new x,y ranges for graph
        self.graph_zoom = [(x1,x2), (y1,y2)]
        ax.set_xlim(self.graph_zoom[0])
        ax.set_ylim(self.graph_zoom[1])
        self.imageCanvas.draw_idle()    

    def closeEvent(self, ev):
        """
        Close the event
        """
        self.close()

    def refreshAllTabs(self):
        """
        Set all tab update status to be not update, and Refresh (Redraw) all tab
        """
        self.function = None
        self.resetStatusbar()

    def resetStatusbar(self):
        """
        Reset the status bar
        """
        self.imgPathOnStatusBar.setText(
            'Current File (' + str(self.currentFileNumber + 1) + '/' + str(len(self.force)) + ') : ' + self.filePath)

    def onNewFileSelected(self, newFile):
        """
        Preprocess folder of the file and process current image
        :param newFile: full name of selected file
        """
        self.selectLogButton.setHidden(True)
        self.imageCanvas.setHidden(False)
        ax = self.imageAxes
        ax.cla()
        with open(newFile) as f:
            f = f.readlines()
        self.length_out = []
        self.length_in = []
        self.force = []
        for line in f:
            if line[0] != '#' and len(line) >= 10:
                linelist = line.split('\t')
                if len(linelist) == 10:
                    self.length_out.append(float(linelist[7]))
                    self.length_in.append(float(linelist[8]))
                    self.force.append(float(linelist[9]))
                else:
                    errMsg = QMessageBox()
                    errMsg.setText('Trace file is invalid')
                    msg = 'Please select a file formatted correctly\n\n'
                    errMsg.setInformativeText(msg)
                    errMsg.setStandardButtons(QMessageBox.Ok)
                    errMsg.setIcon(QMessageBox.Warning)
                    errMsg.setFixedWidth(300)
                    if errMsg.exec_() == QMessageBox.Ok:
                        self.close()
        ax.plot(self.length_in, color='g', label='Length In')
        ax.plot(self.length_out, color='g', linestyle='dashed', label='Length Out')
        ax.set_xlabel("Time", fontsize=14)
        ax.set_ylabel("Length", color="green", fontsize=14)
        ax.legend()
        ax2=ax.twinx()
        ax2.plot(self.force, color='b', label='Force')
        ax2.set_ylabel("Force", color="blue",fontsize=14)
        self.imageAxes.axvline(x=self.currentFileNumber, color='r')
        self.imageFigure.tight_layout()
        self.imageCanvas.draw()
        self.refreshAllTabs()

    def browseFile(self):
        """
        Popup input dialog and set file selection
        """
        file_name = getAFile(filtr='')
        if file_name != "":
            self.filePath = file_name
            self.onNewFileSelected(str(file_name))