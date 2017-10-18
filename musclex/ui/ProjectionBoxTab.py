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
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
import matplotlib.patches as patches
import numpy as np
from ..modules.ProjectionProcessor import layerlineModel, layerlineModelBackground, layerlineBackground, meridianBackground, meridianGauss

class ProjectionBoxTab(QtGui.QWidget):
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
        self.graphFigure1 = plt.figure()
        self.graphCanvas1 = FigureCanvas(self.graphFigure1)

        self.graphFigure2 = plt.figure(facecolor='#606060')
        self.graphCanvas2 = FigureCanvas(self.graphFigure2)

        self.optionsFrame = QtGui.QFrame()
        self.optionsFrame.setFixedWidth(350)
        self.optionsLayout = QtGui.QVBoxLayout(self.optionsFrame)

        self.displayOptionsGroup = QtGui.QGroupBox("Display Options")
        self.dispOptLayout = QtGui.QGridLayout(self.displayOptionsGroup)

        self.histChkBx = QtGui.QCheckBox("Original Projection")
        self.histChkBx.setChecked(True)
        self.fitmodelChkBx = QtGui.QCheckBox("Fitting Model")
        self.fitmodelChkBx.setChecked(False)
        self.bgChkBx = QtGui.QCheckBox("Fitting Background")
        self.bgChkBx.setChecked(True)
        self.peaksChkBx = QtGui.QCheckBox("Model Peaks")
        self.peaksChkBx.setChecked(False)
        self.centerChkBx = QtGui.QCheckBox("Center")
        self.centerChkBx.setChecked(False)
        self.subHistChkBx = QtGui.QCheckBox("Subtracted Projection")
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

        self.resultTable1 = QtGui.QTableWidget()
        self.resultTable1.setColumnCount(3)
        self.resultTable1.setHorizontalHeaderLabels(["Distance", "Sigma", "Area"])
        self.resultTable1.horizontalHeader().setStretchLastSection(True)
        self.resultTable1.setColumnWidth(0, 100)
        self.resultTable1.setColumnWidth(1, 100)
        self.resultTable1.setColumnWidth(2, 100)
        self.resultTable1.setFixedHeight(100)

        self.resultTable2 = QtGui.QTableWidget()
        self.resultTable2.setColumnCount(3)
        self.resultTable2.setHorizontalHeaderLabels(["Baseline", "Centroid", "Width"])
        self.resultTable2.horizontalHeader().setStretchLastSection(True)
        self.resultTable2.setColumnWidth(0, 100)
        self.resultTable2.setColumnWidth(1, 100)
        self.resultTable2.setColumnWidth(2, 100)
        self.resultTable2.setFixedHeight(100)
        self.pnButtons = QtGui.QHBoxLayout()
        self.prevButton = QtGui.QPushButton("<<<")
        self.nextButton = QtGui.QPushButton(">>>")
        self.pnButtons.addWidget(self.prevButton)
        self.pnButtons.addWidget(self.nextButton)

        self.optionsLayout.addWidget(self.displayOptionsGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(QtGui.QLabel("<h3>Fitting Results</h3>"))
        self.optionsLayout.addWidget(self.resultTable1)
        self.optionsLayout.addWidget(QtGui.QLabel("<h3>Other Results</h3>"))
        self.optionsLayout.addWidget(self.resultTable2)
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

        self.resultTable2.itemChanged.connect(self.handleItemChanged)

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
        if self.parent.projProc is not None and item.column() == 0 and not self.syncUI:
            self.parent.projProc.setBaseline(self.num, item.row(), item.text())
            self.parent.processImage()

    def graphClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if self.parent.projProc is None or self.function is None or len(self.function) < 2:
            return

        x = event.xdata
        y = event.ydata

        func = self.function

        if func[0] == 'peaks':
            peaks = func[1]
            box = self.parent.allboxes[self.num]
            type = self.parent.boxtypes[self.num]
            if type == 'h':
                center = self.parent.projProc.orig_img.shape[1] / 2 - 0.5 - box[0][0]
            else:
                center = self.parent.projProc.orig_img.shape[0] / 2 - 0.5 - box[1][0]

            distance = int(round(abs(center - x)))
            peaks.append(distance)

            ax = self.graphFigure1.add_subplot(111)
            ax.axvline(center + distance, color='#ff630a', linewidth=2)
            ax.axvline(center - distance, color='#ff630a', linewidth=2)

            ax = self.graphFigure2.add_subplot(111)
            ax.axvline(center + distance, color='#ff630a', linewidth=2)
            ax.axvline(center - distance, color='#ff630a', linewidth=2)

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
        if self.parent.projProc is None or not self.need_update:
            return

        self.syncUI = True

        # Update graphs
        info = self.parent.projProc.info
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
            ax.plot(hist, color='k')

        if fit_results.has_key(num):
            xs = np.arange(0, len(hist))
            model = info['fit_results'][num]

            if self.subHistChkBx.isChecked() and subtracted_hists.has_key(num):
                ax2.plot(subtracted_hists[num], color='k')

            if self.fitmodelChkBx.isChecked():
                model_hist = layerlineModel(xs, **model)
                subtracted_model = layerlineModel(xs, **model)-layerlineModelBackground(xs, **model)
                ax.plot(model_hist, color='g')
                ax2.plot(subtracted_model, color='g')

            if self.bgChkBx.isChecked():
                background = layerlineBackground(xs, **model)
                meridian_bg = meridianBackground(xs, **model) + layerlineBackground(xs, **model)
                meridian = layerlineModelBackground(xs, **model)
                ax.fill_between(xs, meridian, meridian_bg, facecolor='r', alpha=0.3)
                ax.fill_between(xs, meridian_bg, background, facecolor='y', alpha=0.3)
                ax.fill_between(xs, 0, background, facecolor='b', alpha=0.3)

            if self.centerChkBx.isChecked():
                ax.axvline(model['centerX'], color='c', alpha=0.7)
                ax2.axvline(model['centerX'], color='c', alpha=0.7)

            if self.peaksChkBx.isChecked():
                i = 0
                while 'p_' + str(i) in model:
                    p = model['p_' + str(i)]
                    i += 1
                    ax.axvline(model['centerX'] - p, color='r', alpha=0.7)
                    ax.axvline(model['centerX'] + p, color='r', alpha=0.7)

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
            nPeaks = len(centroids)
            fit_result = fit_results[num]
            self.resultTable1.setRowCount(nPeaks)
            self.resultTable2.setRowCount(nPeaks)

            for i in range(nPeaks):
                center = fit_result['p_'+str(i)]
                sigma = fit_result['sigma'+str(i)]
                area = fit_result['amplitude' + str(i)]

                item = QtGui.QTableWidgetItem(str(center))
                item.setFlags(QtCore.Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 0, item)

                item = QtGui.QTableWidgetItem(str(sigma))
                item.setFlags(QtCore.Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 1, item)

                item = QtGui.QTableWidgetItem(str(area))
                item.setFlags(QtCore.Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 2, item)

                centroid = centroids[i]
                baseline = baselines[i]
                width = widths[i]

                item = QtGui.QTableWidgetItem(str(baseline))
                self.resultTable2.setItem(i, 0, item)

                item = QtGui.QTableWidgetItem(str(centroid))
                item.setFlags(QtCore.Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 1, item)

                item = QtGui.QTableWidgetItem(str(width))
                item.setFlags(QtCore.Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 2, item)

        self.need_update = False
        self.syncUI = False