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
from .pyqt_utils import *
import matplotlib.patches as patches
import numpy as np
from ..modules.ProjectionProcessor import layerlineModel, layerlineModelBackground, layerlineBackground, meridianBackground, meridianGauss
from ..utils.image_processor import getNewZoom

class ProjectionBoxTab(QWidget):
    """
    Fitting Tabs : left or right
    Display fitting graph and providing options
    """
    def __init__(self, parent, name):
        QWidget.__init__(self)
        self.parent = parent
        self.name = name
        self.function = None
        self.syncUI = False
        self.need_update = True
        self.checkableButtons = []
        self.graphMaxBound = None
        self.zoom1 = None
        self.zoom2 = None
        self.zoomRect = None
        self.centerX = None
        self.initUI()
        self.setAllToolTips()
        self.setConnections()

    def getCenterX(self):
        """
        Get center X
        :return: center X
        """
        if self.parent.projProc is None:
            return 0

        info = self.parent.projProc.info
        name = self.name

        box = info['boxes'][name]

        if info['types'][name] == 'h':
            start_x = box[0][0]
            if self.parent.centerx is None:
                self.centerX = self.parent.projProc.orig_img.shape[1] / 2. - 0.5 - start_x
            else:
                self.centerX = self.parent.centerx - start_x
        elif info['types'][name] == 'oriented':
            start_x = box[0][0]
            self.centerX = box[6][0] - start_x
        else:
            if info['types'][name] == 'v':
                start_y = box[1][0]
            else:
                start_y = box[0][1]
            if self.parent.centery is None:
                self.centerX = self.parent.projProc.orig_img.shape[0] / 2. - 0.5 - start_y
            else:
                self.centerX = self.parent.centery - start_y

        return self.centerX

    def initUI(self):
        """
        Initial all GUIs including : 4 plots and result table
        """
        self.setContentsMargins(0, 0, 0, 0)
        self.tabLayout = QHBoxLayout(self)
        self.graphFigure1 = plt.figure()
        self.graphAxes1 = self.graphFigure1.add_subplot(111)
        self.graphCanvas1 = FigureCanvas(self.graphFigure1)

        self.graphFigure2 = plt.figure()
        self.graphAxes2 = self.graphFigure2.add_subplot(111)
        self.graphCanvas2 = FigureCanvas(self.graphFigure2)

        self.optionsFrame = QFrame()
        self.optionsFrame.setFixedWidth(350)
        self.optionsLayout = QVBoxLayout(self.optionsFrame)

        self.displayOptionsGroup = QGroupBox("Display Options")
        self.dispOptLayout = QGridLayout(self.displayOptionsGroup)

        self.histChkBx = QCheckBox("Original Projection")
        self.histChkBx.setChecked(True)
        self.hullRangeChkBx = QCheckBox('Hull Range')
        self.hullRangeChkBx.setEnabled(self.parent.bgsubs[self.name] == 1)
        self.hullRangeChkBx.setChecked(self.parent.bgsubs[self.name]==1)
        self.fitmodelChkBx = QCheckBox("Fitting Model")
        self.fitmodelChkBx.setChecked(True)
        self.bgChkBx = QCheckBox("Background")
        self.bgChkBx.setChecked(True)
        self.maxPeaksChkBx = QCheckBox("Max Peaks")
        self.maxPeaksChkBx.setChecked(False)
        self.peaksChkBx = QCheckBox("Model Peaks")
        self.peaksChkBx.setChecked(True)
        self.centerChkBx = QCheckBox("Center")
        self.centerChkBx.setChecked(False)
        self.subHistChkBx = QCheckBox("Subtracted Projection")
        self.subHistChkBx.setChecked(True)
        self.baselineChkBx = QCheckBox("Baselines")
        self.baselineChkBx.setChecked(False)
        self.centroidChkBx = QCheckBox("Centroids")
        self.centroidChkBx.setChecked(False)

        self.zoomInButton = QPushButton("Zoom in")
        self.zoomInButton.setCheckable(True)
        self.zoomOutButton = QPushButton("Zoom out")
        self.fullButton = QPushButton("Full")
        self.checkableButtons.append(self.zoomInButton)
        self.dispOptLayout.addWidget(self.histChkBx, 0, 0, 1, 1)
        self.dispOptLayout.addWidget(self.fitmodelChkBx, 0, 1, 1, 1)
        self.dispOptLayout.addWidget(self.hullRangeChkBx, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.bgChkBx, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxPeaksChkBx, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.peaksChkBx, 3, 1, 1, 1)
        self.dispOptLayout.addWidget(self.centerChkBx, 4, 0, 1, 1)
        self.dispOptLayout.addWidget(self.centroidChkBx, 4, 1, 1, 1)
        self.dispOptLayout.addWidget(self.baselineChkBx, 5, 0, 1, 1)
        self.dispOptLayout.addWidget(self.subHistChkBx, 5, 1, 1, 1)
        self.dispOptLayout.addWidget(self.zoomInButton, 6, 0, 1, 1)
        self.dispOptLayout.addWidget(self.zoomOutButton, 6, 1, 1, 1)
        self.dispOptLayout.addWidget(self.fullButton, 7, 0, 1, 2)

        self.settingGroup = QGroupBox("Setting")
        self.settingLayout = QGridLayout(self.settingGroup)
        self.peaksButton = QPushButton("Select Peaks")
        self.peaksButton.setCheckable(True)
        self.checkableButtons.append(self.peaksButton)
        self.hullRangeButton = QPushButton("Set Manual Convex Hull Range")
        self.hullRangeButton.setCheckable(True)
        self.hullRangeButton.setHidden(self.parent.bgsubs[self.name]!=1)
        box = self.parent.allboxes[self.name]
        width = int(np.ceil(abs(box[0][0]-box[0][1])/2.))

        self.startHull = QSpinBox()
        self.startHull.setRange(0, width)
        self.startHull.setKeyboardTracking(False)
        self.startHull.setHidden(self.parent.bgsubs[self.name] != 1)
        self.endHull = QSpinBox()
        self.endHull.setRange(0, width)
        self.endHull.setKeyboardTracking(False)
        self.endHull.setHidden(self.parent.bgsubs[self.name] != 1)
        self.checkableButtons.append(self.hullRangeButton)
        self.settingLayout.addWidget(self.peaksButton, 0, 0, 1, 2)
        self.settingLayout.addWidget(self.hullRangeButton, 1, 0, 1, 2)
        if self.parent.bgsubs[self.name]== 1:
            self.settingLayout.addWidget(QLabel("Start"), 2, 0, 1, 1, Qt.AlignCenter)
            self.settingLayout.addWidget(QLabel("End"), 2, 1, 1, 1, Qt.AlignCenter)
        self.settingLayout.addWidget(self.startHull, 3, 0, 1, 1)
        self.settingLayout.addWidget(self.endHull, 3, 1, 1, 1)

        self.results_text = QLabel()

        self.resultTable1 = QTableWidget()
        self.resultTable1.setColumnCount(4)
        self.resultTable1.setHorizontalHeaderLabels(["Max Peak", "Gauss Center", "Gauss Sigma", "Gauss Area"])
        self.resultTable1.horizontalHeader().setStretchLastSection(True)
        self.resultTable1.setColumnWidth(0, 75)
        self.resultTable1.setColumnWidth(1, 75)
        self.resultTable1.setColumnWidth(2, 75)
        self.resultTable1.setColumnWidth(3, 75)
        self.resultTable1.setFixedHeight(100)

        self.resultTable2 = QTableWidget()
        self.resultTable2.setColumnCount(3)
        self.resultTable2.setHorizontalHeaderLabels(["Baseline", "Centroid", "Width"])
        self.resultTable2.horizontalHeader().setStretchLastSection(True)
        self.resultTable2.setColumnWidth(0, 100)
        self.resultTable2.setColumnWidth(1, 100)
        self.resultTable2.setColumnWidth(2, 100)
        self.resultTable2.setFixedHeight(100)
        self.pnButtons = QHBoxLayout()
        self.prevButton = QPushButton("<<<")
        self.nextButton = QPushButton(">>>")
        self.pnButtons.addWidget(self.prevButton)
        self.pnButtons.addWidget(self.nextButton)

        self.optionsLayout.addWidget(self.displayOptionsGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(self.settingGroup)
        self.optionsLayout.addSpacing(10)
        self.optionsLayout.addWidget(QLabel("<h3>Fitting Results</h3>"))
        self.optionsLayout.addWidget(self.resultTable1)
        self.optionsLayout.addWidget(QLabel("<h3>Other Results</h3>"))
        self.optionsLayout.addWidget(self.resultTable2)
        self.optionsLayout.addStretch()
        self.optionsLayout.addLayout(self.pnButtons)

        self.graphLayout = QVBoxLayout()
        self.graphLayout.addWidget(self.graphCanvas1)
        self.graphLayout.addWidget(self.graphCanvas2)
        self.tabLayout.addLayout(self.graphLayout)
        self.tabLayout.addWidget(self.optionsFrame)

    def setAllToolTips(self):
        """
        Set Tooltips for widgets
        """
        pass

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.histChkBx.stateChanged.connect(self.resetUI)
        self.fitmodelChkBx.stateChanged.connect(self.resetUI)
        self.hullRangeChkBx.stateChanged.connect(self.resetUI)
        self.bgChkBx.stateChanged.connect(self.resetUI)
        self.maxPeaksChkBx.stateChanged.connect(self.resetUI)
        self.peaksChkBx.stateChanged.connect(self.resetUI)
        self.centerChkBx.stateChanged.connect(self.resetUI)
        self.subHistChkBx.stateChanged.connect(self.resetUI)
        self.baselineChkBx.stateChanged.connect(self.resetUI)
        self.centroidChkBx.stateChanged.connect(self.resetUI)

        self.zoomInButton.clicked.connect(self.zoomInclicked)
        self.zoomOutButton.clicked.connect(self.zoomOutclicked)
        self.fullButton.clicked.connect(self.fullClicked)

        self.peaksButton.clicked.connect(self.addPeaks)
        self.hullRangeButton.clicked.connect(self.setManualHullRange)
        self.startHull.valueChanged.connect(self.hullRangeChanged)
        self.endHull.valueChanged.connect(self.hullRangeChanged)

        self.resultTable2.itemChanged.connect(self.handleItemChanged)

        self.prevButton.clicked.connect(self.parent.prevClicked)
        self.nextButton.clicked.connect(self.parent.nextClicked)

        self.graphFigure1.canvas.mpl_connect('button_press_event', self.graphClicked)
        self.graphFigure2.canvas.mpl_connect('button_press_event', self.graphClicked2)
        self.graphFigure1.canvas.mpl_connect('motion_notify_event', self.graphOnMotion1)
        self.graphFigure2.canvas.mpl_connect('motion_notify_event', self.graphOnMotion2)
        self.graphFigure1.canvas.mpl_connect('button_release_event', self.graphReleased)
        self.graphFigure2.canvas.mpl_connect('button_release_event', self.graphReleased)
        self.graphFigure1.canvas.mpl_connect('figure_leave_event', self.graphReleased)
        self.graphFigure2.canvas.mpl_connect('figure_leave_event', self.graphReleased)


    def hullRangeChanged(self):
        """
        Trigger when convex hull range is changed
        """
        if self.parent.projProc is not None and not self.syncUI:
            self.parent.hull_ranges[self.name] = (self.startHull.value(), self.endHull.value())
            self.parent.projProc.removeInfo(self.name, 'hists2')
            self.parent.processImage()

    def handleItemChanged(self, item):
        """
        Trigger when a item in table is changed
        :param item:
        :return:
        """
        if self.parent.projProc is not None and item.column() == 0 and not self.syncUI:
            self.parent.projProc.setBaseline(self.name, item.row(), item.text())
            self.parent.processImage()

    def zoomInclicked(self):
        """
        Trigger when zoom in button is pressed
        """
        if self.zoomInButton.isChecked():
            self.function = ['zoom']
        else:
            self.function = None
            self.resetUI()

    def zoomOutclicked(self):
        """
        Trigger when zoom out clicked
        :return:
        """
        if self.graphMaxBound is not None:
            xlim = self.graphMaxBound[0]
            ylim = self.graphMaxBound[1]

            def increaseLimit(fig, canvas, ax, xlim, ylim):
                old_xlim = ax.get_xlim()
                old_ylim = ax.get_ylim()
                xscale = abs(old_xlim[0]-old_xlim[1])*0.1
                yscale = abs(old_ylim[0]-old_ylim[1])*0.1
                new_x1 = max(old_xlim[0] - xscale, xlim[0])
                new_x2 = min(old_xlim[1] + xscale, xlim[1])
                new_y1 = max(old_ylim[0] - yscale, ylim[0])
                new_y2 = min(old_ylim[1] + yscale, ylim[1])
                zoom = [(new_x1, new_x2), (new_y1,new_y2)]
                ax.set_xlim(zoom[0])
                ax.set_ylim(zoom[1])
                canvas.draw_idle()
                return zoom

            self.zoom1 = increaseLimit(self.graphFigure1, self.graphCanvas1, self.graphAxes1, xlim, ylim)
            self.zoom2 = increaseLimit(self.graphFigure2, self.graphCanvas2, self.graphAxes2, xlim, ylim)

    def fullClicked(self):
        """
        Triggered when Full button is pressed
        """
        self.zoom1 = None
        self.zoom2 = None
        self.resetUI()

    def setZoomIn(self, point1, point2):
        """
        Set Zoom in for plot n. If n of point1 and point2 are not the same, just refresh UI.
        :param point1: (n, (x1, y1))
        :param point2: (n, (x2, y2))
        :return:
        """
        if point1[0] == point2[0]:
            # Set zoom area for a plot
            pt1 = point1[1]
            pt2 = point2[1]
            x1 = min(pt1[0], pt2[0])
            x2 = max(pt1[0], pt2[0])
            y1 = min(pt1[1], pt2[1])
            y2 = max(pt1[1], pt2[1])
            zoom = [(x1, x2), (y1, y2)]
            if point1[0] == 1:
                self.zoom1 = zoom
            else:
                self.zoom2 = zoom
        self.resetUI()

    def graphReleased(self, event):
        """
        Triggered when mouse is released from plot
        :return:
        """
        if self.function is not None and 'move' in self.function[0]:
            self.function = None
            self.parent.pixel_detail.setText("")

    def graphClicked2(self, event):
        """
        Triggered when the second plot is clicked
        """
        x = event.xdata
        y = event.ydata

        if self.parent.projProc is None or x is None or y is None:
            return

        func = self.function

        if func is None:
            self.function = ['move2', (x, y)]
        elif func[0] == 'zoom':
            # select zoom in area for the second plot
            func.append((2, (x, y)))
            if len(func) == 3:
                self.setZoomIn(func[1], func[2])
        else:
            # call event handler for first plot
            self.graphClicked(event)

    def graphClicked(self, event):
        """
        Triggered when the first plot is clicked
        """
        x = event.xdata
        y = event.ydata
        if self.parent.projProc is None or x is None or y is None:
            return

        if self.function is None:
            self.function = ['move1', (x,y)]
            return

        func = self.function
        box = self.parent.allboxes[self.name]
        type = self.parent.boxtypes[self.name]
        center = self.getCenterX()

        distance = int(round(abs(center - x)))

        if func[0] == 'peaks':
            peaks = func[1]
            peaks.append(distance)

            ax = self.graphAxes1
            ax.axvline(center + distance, color='#ff630a', linewidth=2)
            ax.axvline(center - distance, color='#ff630a', linewidth=2)

            ax = self.graphAxes2
            ax.axvline(center + distance, color='#ff630a', linewidth=2)
            ax.axvline(center - distance, color='#ff630a', linewidth=2)

            self.graphCanvas1.draw_idle()
            self.graphCanvas2.draw_idle()

        elif func[0] == 'hull':
            hull_range = func[1]
            hull_range.append(distance)

            ax = self.graphAxes1
            ax.axvline(center + distance, color='k', linewidth=2)
            ax.axvline(center - distance, color='k', linewidth=2)

            ax = self.graphAxes2
            ax.axvline(center + distance, color='k', linewidth=2)
            ax.axvline(center - distance, color='k', linewidth=2)

            self.graphCanvas1.draw_idle()
            self.graphCanvas2.draw_idle()

            if len(hull_range) == 2:
                hull_range = tuple(sorted(hull_range))
                self.parent.hull_ranges[self.name] = hull_range
                self.parent.projProc.removeInfo(self.name, 'hists2')
                self.parent.processImage()
        elif func[0] == 'zoom':
            # select zoom in area for the second plot
            func.append((1, (x, y)))
            if len(func) == 3:
                self.setZoomIn(func[1], func[2])

    def drawRectangle(self, fig, canvas, ax, point1, point2):
        """
        Draw a rectangle
        :param fig:
        :param canvas:
        :param point1:
        :param point2:
        :return:
        """
        if self.zoomRect is not None and self.zoomRect in ax.patches:
            ax.patches.remove(self.zoomRect)

        x1 = min(point1[0], point2[0])
        x2 = max(point1[0], point2[0])
        y1 = min(point1[1], point2[1])
        y2 = max(point1[1], point2[1])
        w = abs(x1-x2)
        h = abs(y1-y2)

        self.zoomRect = patches.Rectangle((x1, y1), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted')
        ax.add_patch(self.zoomRect)
        canvas.draw_idle()

    def graphOnMotion1(self, event):
        """
        Trigger when mouse hovers the first plot
        """
        x = event.xdata
        y = event.ydata
        if x is not None and y is not None:
            centerX = self.getCenterX() # this should be the center in the box?
            distance = x - centerX
            hist = self.parent.projProc.info['hists'][self.name]
            self.parent.pixel_detail.setText("Distance = " + str(round(distance, 3))+", Intensity = "+str(hist[int(round(x))]))
            if self.function is not None:
                if self.function[0] == 'move1' and self.graphMaxBound is not None and self.zoom1 is not None:
                    # change zoom-in location to move around plot
                    func = self.function
                    ax = self.graphAxes1
                    move = (func[1][0] - x, func[1][1] - y)
                    self.zoom1 = getNewZoom(self.zoom1, move, self.graphMaxBound[0][1], self.graphMaxBound[1][1], self.graphMaxBound[1][0])
                    ax.set_xlim(self.zoom1[0])
                    ax.set_ylim(self.zoom1[1])
                    self.graphCanvas1.draw_idle()
                elif self.function[0] == 'zoom' and len(self.function) == 2 and self.function[1][0] == 1:
                    # Draw rectangle
                    start_pt = self.function[1][1]
                    self.drawRectangle(self.graphFigure1, self.graphCanvas1, self.graphAxes1, start_pt, (x, y))



    def graphOnMotion2(self, event):
        """
        Trigger when mouse hovers the first plot
        """
        x = event.xdata
        y = event.ydata
        if x is not None and y is not None:
            centerX = self.getCenterX()
            distance = x - centerX
            all_hists =  self.parent.projProc.info['subtracted_hists']
            if self.name in all_hists:
                hist = all_hists[self.name]
                self.parent.pixel_detail.setText(
                    "Distance = " + str(round(distance, 3)) + ", Intensity = " + str(hist[int(round(x))]))
            else:
                self.parent.pixel_detail.setText("Distance = " + str(round(distance, 3)))
            if self.function is not None:
                if self.function[0] == 'move2' and self.graphMaxBound is not None and self.zoom2 is not None:
                    # change zoom-in location to move around plot
                    func = self.function
                    ax = self.graphAxes2
                    move = (func[1][0] - x, func[1][1] - y)
                    self.zoom2 = getNewZoom(self.zoom2, move, self.graphMaxBound[0][1], self.graphMaxBound[1][1], self.graphMaxBound[1][0])
                    ax.set_xlim(self.zoom2[0])
                    ax.set_ylim(self.zoom2[1])
                    self.graphCanvas2.draw_idle()
                elif self.function[0] == 'zoom' and len(self.function) == 2 and self.function[1][0] == 2:
                    # Draw rectangle
                    start_pt = self.function[1][1]
                    self.drawRectangle(self.graphFigure2, self.graphCanvas2, self.graphAxes2, start_pt, (x,y))

    def setManualHullRange(self):
        """
        Trigger when "Set Manual Convex hull ranges" is pressed
        :return:
        """
        if self.hullRangeButton.isChecked():
            self.function = ['hull', []]
        else:
            self.resetUI()

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
            self.function = None
            self.parent.addPeakstoBox(self.name, peaks)

    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard
        """
        key = event.key()

        if key == Qt.Key_Escape:
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
        name = self.name
        if not name in info['box_names']:
            return

        hist = info['hists'][name]
        fit_results = info['fit_results']
        subtracted_hists = info['subtracted_hists']
        all_baselines = info['baselines']
        all_centroids = info['centroids']
        all_widths = info['widths']
        all_peaks = info['moved_peaks']
        bgsubs = info['bgsubs']
        hull_ranges = info['hull_ranges']

        ax = self.graphAxes1
        ax.cla()

        ax2 = self.graphAxes2
        ax2.cla()

        if self.histChkBx.isChecked():
            ax.plot(hist, color='k')

        if name in fit_results:
            xs = np.arange(0, len(hist))
            model = info['fit_results'][name]
            convex_hull = hist - info['hists2'][name]

            if self.subHistChkBx.isChecked() and name in subtracted_hists:
                ax2.plot(subtracted_hists[name], color='k')

            if self.fitmodelChkBx.isChecked():
                model_hist = layerlineModel(xs, **model)
                subtracted_model = model_hist-layerlineModelBackground(xs, **model)

                if bgsubs[name] == 1:
                    # Add convexhull background
                    model_hist = model_hist + convex_hull

                ax.plot(model_hist, color='g')
                if bgsubs[name] == 2: # if no background subtraction, use the fit from the original model
                    ax2.plot(model_hist, color='g')
                else:
                    ax2.plot(subtracted_model, color='g')

            if self.bgChkBx.isChecked():
                if bgsubs[name] == 1:
                    # Add convex hull background
                    ax.fill_between(xs, min(convex_hull), convex_hull, facecolor='b', alpha=0.3)
                else:
                    # Add 3 Gaussians
                    background = layerlineBackground(xs, **model)
                    meridian_bg = meridianBackground(xs, **model) + layerlineBackground(xs, **model)
                    meridian = layerlineModelBackground(xs, **model)
                    ax.fill_between(xs, meridian, meridian_bg, facecolor='r', alpha=0.3)
                    ax.fill_between(xs, meridian_bg, background, facecolor='y', alpha=0.3)
                    ax.fill_between(xs, 0, background, facecolor='b', alpha=0.3)

            if self.centerChkBx.isChecked():
                # Add center line
                ax.axvline(model['centerX'], color='c', alpha=0.7)
                ax2.axvline(model['centerX'], color='c', alpha=0.7)

            if self.peaksChkBx.isChecked():
                # display model peaks
                i = 0
                while 'p_' + str(i) in model:
                    p = model['p_' + str(i)]
                    i += 1
                    ax.axvline(model['centerX'] - p, color='r', alpha=0.7)
                    ax.axvline(model['centerX'] + p, color='r', alpha=0.7)

            if self.maxPeaksChkBx.isChecked():
                peaks = all_peaks[name]
                for p in peaks:
                    d = p - model['centerX']
                    ax2.axvline(model['centerX'] - d, color='b', alpha=0.7)
                    ax2.axvline(model['centerX'] + d, color='b', alpha=0.7)
            # max intensity locations
            # if name in all_peaks:
            #     peaks = all_peaks[name]
            #     for p in peaks:
            #         ax2.axvline(p, color='r', alpha=0.7)

            if name in all_centroids and name in all_baselines and (self.centroidChkBx.isChecked() or self.baselineChkBx.isChecked()):
                # Add baselines and centroids
                centroids = all_centroids[name]
                baselines = all_baselines[name]
                widths = all_widths[name]
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

        if self.hullRangeChkBx.isChecked() and bgsubs[name] == 1 and name in hull_ranges:
            # Color area OUTSIDE convex hull range
            centerX = self.getCenterX()

            ax.axvspan(centerX - hull_ranges[name][0], centerX + hull_ranges[name][0], alpha=0.5, color='k')
            ax.axvspan(0, centerX - hull_ranges[name][1], alpha=0.5, color='k')
            ax.axvspan(centerX + hull_ranges[name][1], len(hist), alpha=0.5, color='k')


            # Update spin box
            self.startHull.setValue(hull_ranges[name][0])
            self.endHull.setValue(hull_ranges[name][1])


        if self.zoom1 is not None:
            ax.set_xlim(self.zoom1[0])
            ax.set_ylim(self.zoom1[1])
        else:
            ax.set_xlim((0, len(hist)))

        if self.zoom2 is not None:
            ax2.set_xlim(self.zoom2[0])
            ax2.set_ylim(self.zoom2[1])
        else:
            ax2.set_xlim((0, len(hist)))

        if self.graphMaxBound is None:
            self.graphMaxBound = [ax.get_xlim(), ax.get_ylim()]

        self.graphFigure1.tight_layout()
        self.graphCanvas1.draw()
        self.graphFigure2.tight_layout()
        self.graphCanvas2.draw()


        # Update Table
        if name in all_centroids and name in all_baselines:
            centroids = all_centroids[name]
            baselines = all_baselines[name]
            widths = all_widths[name]
            nPeaks = len(centroids)
            fit_result = fit_results[name]
            peaks = all_peaks[name] - fit_result['centerX']
            self.resultTable1.setRowCount(nPeaks)
            self.resultTable2.setRowCount(nPeaks)

            for i in range(nPeaks):
                center = fit_result['p_'+str(i)]
                sigma = fit_result['sigma'+str(i)]
                area = fit_result['amplitude' + str(i)]

                item = QTableWidgetItem(str(peaks[i]))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 0, item)

                item = QTableWidgetItem(str(center))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 1, item)

                item = QTableWidgetItem(str(sigma))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 2, item)

                item = QTableWidgetItem(str(area))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable1.setItem(i, 3, item)

                centroid = centroids[i]
                baseline = baselines[i]
                width = widths[i]

                item = QTableWidgetItem(str(baseline))
                self.resultTable2.setItem(i, 0, item)

                item = QTableWidgetItem(str(centroid))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 1, item)

                item = QTableWidgetItem(str(width))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable2.setItem(i, 2, item)

        self.need_update = False
        self.syncUI = False
