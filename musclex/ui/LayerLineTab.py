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
import numpy as np
from ..biocat_modules.LayerLineProcessor import layerlineModel, layerlineModelBackground

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
        self.checkableButtons = []
        self.initUI()
        self.setAllToolTips()
        self.setConnections()

    def initUI(self):
        """
        Initial all GUIs including : 4 plots and result table
        """
        self.setContentsMargins(0, 0, 0, 0)
        self.tabLayout = QtGui.QHBoxLayout(self)
        self.graphFigure1 = plt.figure(facecolor='#606060')
        self.graphCanvas1 = FigureCanvas(self.graphFigure1)

        self.graphFigure2 = plt.figure(facecolor='#606060')
        self.graphCanvas2 = FigureCanvas(self.graphFigure2)

        self.optionsFrame = QtGui.QFrame()
        self.optionsFrame.setFixedWidth(350)
        self.optionsLayout = QtGui.QVBoxLayout(self.optionsFrame)

        self.displayOptionsGroup = QtGui.QGroupBox("Display Options")
        self.dispOptLayout = QtGui.QGridLayout(self.displayOptionsGroup)

        self.histChkBx = QtGui.QCheckBox("Histogram")
        self.histChkBx.setChecked(True)
        self.fitmodelChkBx = QtGui.QCheckBox("Fitting Model")
        self.fitmodelChkBx.setChecked(True)
        self.bgChkBx = QtGui.QCheckBox("Fitting Background")
        self.bgChkBx.setChecked(True)
        self.peaksChkBx = QtGui.QCheckBox("Model Peaks")
        self.peaksChkBx.setChecked(True)
        self.centerChkBx = QtGui.QCheckBox("Center")
        self.centerChkBx.setChecked(True)
        self.subHistChkBx = QtGui.QCheckBox("Subtracted Histogram")
        self.subHistChkBx.setChecked(True)
        self.baselineChkBx = QtGui.QCheckBox("Baselines")
        self.baselineChkBx.setChecked(True)
        self.centroidChkBx = QtGui.QCheckBox("Centroids")
        self.centroidChkBx.setChecked(True)
        self.dispOptLayout.addWidget(self.histChkBx, 0, 0, 1, 1)
        self.dispOptLayout.addWidget(self.fitmodelChkBx, 1, 0, 1, 1)
        self.dispOptLayout.addWidget(self.bgChkBx, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.peaksChkBx, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.centerChkBx, 4, 0, 1, 1)
        self.dispOptLayout.addWidget(self.subHistChkBx, 5, 0, 1, 1)
        self.dispOptLayout.addWidget(self.baselineChkBx, 6, 0, 1, 1)
        self.dispOptLayout.addWidget(self.centroidChkBx, 7, 0, 1, 1)

        self.settingGroup = QtGui.QGroupBox("Setting")
        self.settingLayout = QtGui.QVBoxLayout(self.settingGroup)
        self.peaksButton = QtGui.QPushButton("Select Peaks")
        self.peaksButton.setCheckable(True)
        self.checkableButtons.append(self.peaksButton)
        self.settingLayout.addWidget(self.peaksButton)

        self.results_text = QtGui.QLabel()
        
        self.resultTable = QtGui.QTableWidget()
        self.resultTable.setColumnCount(3)
        self.resultTable.setHorizontalHeaderLabels(["Baseline", "Centroid", "Width"])
        self.resultTable.horizontalHeader().setStretchLastSection(True)
        self.resultTable.setColumnWidth(0, 100)
        self.resultTable.setColumnWidth(1, 100)
        self.resultTable.setColumnWidth(2, 100)
        self.resultTable.setFixedHeight(100)
        self.pnButtons = QtGui.QHBoxLayout()
        self.prevButton = QtGui.QPushButton("<<<")
        self.nextButton = QtGui.QPushButton(">>>")
        self.pnButtons.addWidget(self.prevButton)
        self.pnButtons.addWidget(self.nextButton)

        self.optionsLayout.addWidget(self.displayOptionsGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.results_text)
        self.optionsLayout.addWidget(self.resultTable)
        self.optionsLayout.addStretch()
        self.optionsLayout.addLayout(self.pnButtons)

        self.graphLayout = QtGui.QVBoxLayout()
        self.graphLayout.addWidget(self.graphCanvas1)
        self.graphLayout.addWidget(self.graphCanvas2)
        self.tabLayout.addLayout(self.graphLayout)
        self.tabLayout.addWidget(self.optionsFrame)

    def setAllToolTips(self):
        """
        Set Tooltips for widgets
        """

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.histChkBx.stateChanged.connect(self.resetUI)
        self.fitmodelChkBx.stateChanged.connect(self.resetUI)
        self.bgChkBx.stateChanged.connect(self.resetUI)
        self.peaksChkBx.stateChanged.connect(self.resetUI)
        self.centerChkBx.stateChanged.connect(self.resetUI)
        self.subHistChkBx.stateChanged.connect(self.resetUI)
        self.baselineChkBx.stateChanged.connect(self.resetUI)
        self.centroidChkBx.stateChanged.connect(self.resetUI)

        self.peaksButton.clicked.connect(self.addPeaks)

        self.resultTable.itemChanged.connect(self.handleItemChanged)

        self.prevButton.clicked.connect(self.parent.prevClicked)
        self.nextButton.clicked.connect(self.parent.nextClicked)

        self.graphFigure1.canvas.mpl_connect('button_press_event', self.graphClicked)
        self.graphFigure2.canvas.mpl_connect('button_press_event', self.graphClicked)

    def handleItemChanged(self, item):
        """
        Trigger when a item in table is changed
        :param item:
        :return:
        """
        if self.parent.layerProc is not None and item.column() == 0 and not self.syncUI:
            self.parent.layerProc.setBaseline(self.num, item.row(), item.text())
            self.parent.processImage()

    def graphClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if self.parent.layerProc is None or self.function is None or len(self.function) < 2:
            return

        x = event.xdata

        func = self.function

        if func[0] == 'peaks':
            peaks = func[1]
            box = self.parent.layerlineboxes[self.num]
            centerx = self.parent.layerProc.orig_img.shape[1] / 2 - box[0][0]
            distance = int(round(abs(centerx - x)))
            peaks.append(distance)

            ax = self.graphFigure1.add_subplot(111)
            ax.axvline(centerx + distance, color='#ff630a', linewidth=5)
            ax.axvline(centerx - distance, color='#ff630a', linewidth=5)

            ax = self.graphFigure2.add_subplot(111)
            ax.axvline(centerx + distance, color='#ff630a', linewidth=5)
            ax.axvline(centerx - distance, color='#ff630a', linewidth=5)

            self.graphCanvas1.draw_idle()
            self.graphCanvas2.draw_idle()

    def addPeaks(self):
        """
        Trigger when "Select Peaks" is pressed
        :return:
        """
        if self.peaksButton.isChecked():
            self.peaksButton.setText("Done")
            self.function = ['peaks', []]
        else:
            self.peaksButton.setText("Select Peaks")
            peaks = self.function[1]
            self.parent.addPeakstoBox(self.num, peaks)
            self.function = None

    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard
        """
        key = event.key()

        if key == QtCore.Qt.Key_Escape:
            self.resetUI()

        self.parent.keyPressEvent(event)

    def clearFlags(self):
        """
        clear all flags
        :return:
        """
        self.need_update = True
        self.function = None
        self.peaksButton.setText("Select Peaks")
        for b in self.checkableButtons:
            self.syncUI = True
            b.setChecked(False)
            self.syncUI = False

    def resetUI(self):
        """
        clear flags and update UI
        :return:
        """
        self.clearFlags()
        self.updateUI()

    def updateUI(self):
        """
        Draw plots and display results in text
        :return:
        """
        if self.parent.layerProc is None or not self.need_update:
            return

        self.syncUI = True

        # Update graphs
        info = self.parent.layerProc.info
        num = self.num
        hist = info['hists'][num]
        fit_results = info['fit_results']
        subtracted_hists = info['subtracted_hists']
        all_baselines = info['baselines']
        all_centroids = info['centroids']
        all_widths = info['widths']

        ax = self.graphFigure1.add_subplot(111)
        ax.cla()

        ax2 = self.graphFigure2.add_subplot(111)
        ax2.cla()

        if self.histChkBx.isChecked():
            ax.plot(hist, color='g')

        if fit_results.has_key(num):
            xs = np.arange(0, len(hist))
            model = info['fit_results'][num]

            if self.fitmodelChkBx.isChecked():
                ax.plot(layerlineModel(xs, **model), color = 'b')
                ax2.plot(layerlineModel(xs, **model)-layerlineModelBackground(xs, **model), color='m')

            if self.bgChkBx.isChecked():
                ax.plot(layerlineModelBackground(xs, **model), color='m')

            if self.subHistChkBx.isChecked() and subtracted_hists.has_key(num):
                ax2.plot(subtracted_hists[num], color='k')

            if self.centerChkBx.isChecked():
                ax.axvline(model['centerX'], color='y', alpha=0.7)
                ax2.axvline(model['centerX'], color='y', alpha=0.7)

            if self.peaksChkBx.isChecked():
                i = 0
                while 'p_' + str(i) in model:
                    p = model['p_' + str(i)]
                    i += 1
                    ax.axvline(model['centerX'] - p, color='k', alpha=0.7)
                    ax.axvline(model['centerX'] + p, color='k', alpha=0.7)

            if all_centroids.has_key(num) and all_baselines.has_key(num) and (self.centroidChkBx.isChecked() or self.baselineChkBx.isChecked()):
                centroids = all_centroids[num]
                baselines = all_baselines[num]
                widths = all_widths[num]
                centerX = model['centerX']
                i = 0
                while 'p_' + str(i) in model:
                    c = centroids[i]
                    b = baselines[i]
                    w = widths[i]
                    if self.centroidChkBx.isChecked():
                        ax2.axvline(centerX + c, color='#ff4732')
                        ax2.axvline(centerX - c, color='#ff4732')
                    if self.baselineChkBx.isChecked():
                        ax2.plot(((centerX + c) - w, (centerX + c) + w), (b, b), color='y')
                        ax2.plot(((centerX - c) - w, (centerX - c) + w), (b, b), color='y')

                    i += 1


            # # Update Results Text
            # text = "<h1>Results</h1>"
            # text += "<b>center X</b> :"+str(params['centerX'])
            # text += "<br/><br/><b>Distance (Pixels)</b> :" + str(params['p'])
            # if info.has_key('distances') and info['distances'][self.num] is not None:
            #     text += "<br/><br/><b>Distance (nm)</b> :" + str(info['distances'][self.num])
            # text += "<br/><br/><b>Amplitude</b> :" + str(params['amplitude'])
            # text += "<br/><br/><b>Sigma</b> :" + str(params['sigma'])
            # self.results_text.setText(text)

        ax.set_xlim((0, len(hist)))
        ax2.set_xlim((0, len(hist)))
        self.graphFigure1.tight_layout()
        self.graphCanvas1.draw()
        self.graphFigure2.tight_layout()
        self.graphCanvas2.draw()


        # Update Table
        if all_centroids.has_key(num) and all_baselines.has_key(num):
            centroids = all_centroids[num]
            baselines = all_baselines[num]
            widths = all_widths[num]
            self.resultTable.setRowCount(len(centroids))
            for i in range(len(centroids)):
                centroid = centroids[i]
                baseline = baselines[i]
                width = widths[i]

                item = QtGui.QTableWidgetItem(str(baseline))
                self.resultTable.setItem(i, 0, item)

                item = QtGui.QTableWidgetItem(str(centroid))
                item.setFlags(QtCore.Qt.ItemIsEnabled)
                self.resultTable.setItem(i, 1, item)

                item = QtGui.QTableWidgetItem(str(width))
                item.setFlags(QtCore.Qt.ItemIsEnabled)
                self.resultTable.setItem(i, 2, item)

        self.need_update = False
        self.syncUI = False
