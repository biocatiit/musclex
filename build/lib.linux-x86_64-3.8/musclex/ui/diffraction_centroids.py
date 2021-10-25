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

__author__ = 'Jiranun.J'

from .pyqt_utils import *
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.colors import LogNorm, Normalize
import sys
import shutil
import pickle
import traceback
from ..utils.file_manager import *
from ..utils.image_processor import *
from ..modules.DiffractionCentroids import DiffractionCentroids
from ..csv_manager import DC_CSVManager
import musclex

class OffMeridianTab(QWidget):
    """
    A class for off-meridian tab containnig 4 plots and result table
    """
    def __init__(self, mainwin, off_mer):
        """
        Initial tab
        :param mainwin: main window
        :param off_mer: off-meridian settings (dict)
        """
        QWidget.__init__(self)
        self.mainwin = mainwin
        self.off_mer = off_mer
        self.fixed_hull_range = None
        self.updated = False
        self.function = None
        self.updateUIonly = False
        self.quadrants = ["top_left", "top_right", "bottom_left", "bottom_right"]
        self.tableHeader = ["name", "centroid", "baseline", "intensity", "center"]
        self.checkableButtons = []
        self.function = None
        self.initUI()
        self.setConnections()

    def initUI(self):
        """
        Initial all GUIs including : 4 plots and result table
        """
        self.setContentsMargins(0, 0, 0, 0)
        self.mainLayout = QGridLayout(self)

        self.allFiguresCanvas = {}
        for q in self.quadrants:
            fig = plt.figure()
            ax = fig.add_subplot(111)
            canvas = FigureCanvas(fig)
            self.allFiguresCanvas[q] = (fig, canvas, ax)

        self.settingGrpBx = QGroupBox("Settings")
        self.settingLayout = QGridLayout(self.settingGrpBx)
        self.setSEButton = QPushButton("Set Convex hull range")
        self.setSEButton.setCheckable(True)
        self.checkableButtons.append(self.setSEButton)
        self.settingLayout.addWidget(self.setSEButton, 0, 0, 1, 1, Qt.AlignTop)

        self.resultTable = QTableWidget()
        self.resultTable.setColumnCount(len(self.tableHeader))
        self.resultTable.setHorizontalHeaderLabels(self.tableHeader)
        self.resultTable.horizontalHeader().setStretchLastSection(True)
        self.resultTable.setColumnWidth(0, 110)
        self.resultTable.setColumnWidth(1, 70)
        self.resultTable.setColumnWidth(2, 70)
        self.resultTable.setColumnWidth(3, 70)
        self.resultTable.setColumnWidth(4, 70)
        self.mainLayout.addWidget(self.allFiguresCanvas["top_left"][1], 0, 0, 1, 1)
        self.mainLayout.addWidget(self.allFiguresCanvas["top_right"][1], 0, 1, 1, 1)
        self.mainLayout.addWidget(self.allFiguresCanvas["bottom_left"][1], 1, 0, 1, 1)
        self.mainLayout.addWidget(self.allFiguresCanvas["bottom_right"][1], 1, 1, 1, 1)
        self.mainLayout.addWidget(self.resultTable, 0, 2, 1, 1)
        self.mainLayout.addWidget(self.settingGrpBx, 1, 2, 1, 1)
        
        self.mainLayout.setColumnStretch(0, 1)
        self.mainLayout.setColumnStretch(1, 1)
        self.mainLayout.setColumnStretch(2, 1)

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.resultTable.itemChanged.connect(self.handleItemChanged)
        self.setSEButton.clicked.connect(self.startSESelection)

        for side in self.allFiguresCanvas.keys():
            fig = self.allFiguresCanvas[side][0]
            fig.canvas.mpl_connect("motion_notify_event", self.plotOnMotion)
            fig.canvas.mpl_connect("button_press_event", self.plotClicked)

    def startSESelection(self):
        """
        Triggered when start and end lines clicked
        """
        difCent = self.mainwin.getCurrentDifCent()
        if difCent is None or 'off_mer_hists' not in difCent.info.keys():
            return

        if self.setSEButton.isChecked():
            self.function = ["se"]

            for q in self.quadrants:
                hist = difCent.info['off_mer_hists']['hists'][q]
                left = int(round(0.5 * difCent.init_off_mer["s59"]))
                right = min(len(hist), int(round(1.5 * difCent.init_off_mer["e51"])))
                ax = self.allFiguresCanvas[q][2]
                ax.cla()
                title = q.replace("_", " ")
                ax.set_title(title)
                ax.plot(hist)
                ax.set_xlim((left, right))
                ax.set_ylim((0, max(hist[left:right])*1.1))
                canvas = self.allFiguresCanvas[q][1]
                canvas.draw()
        else:
            self.function = None


    def plotClicked(self, event):
        """
        Triggered when mouse clicked on the graph
        """
        x = event.xdata
        y = event.ydata

        difCent = self.mainwin.getCurrentDifCent()

        if self.function is None or difCent is None or 'off_mer_hists' not in difCent.info.keys() or len(self.function) < 1:
            return

        if self.function[0] == 'se':
            self.function.append(int(round(x)))
            if len(self.function) == 3:
                start = min(self.function[1:3])
                end = max(self.function[1:3])
                self.fixed_hull_range = (start, end)
                difCent.removeInfo('off_mer_rmin_rmax')
                self.function = None
                self.mainwin.processImage()
        
        
    def plotOnMotion(self, event):
        """
        Triggered when mouse hovers on the graph
        """
        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.mainwin.pixel_detail.setText("")
            return
        else:
            self.mainwin.pixel_detail.setText("x=" + str(np.round(x,2)) + ', y=' + str(np.round(y,2)))

        difCent = self.mainwin.getCurrentDifCent()

        if self.function is None or difCent is None or 'off_mer_hists' not in difCent.info.keys():
            return

        if self.function[0] == 'se':
            for q in self.quadrants:
                ax = self.allFiguresCanvas[q][2]
                hist = ax.lines[0:len(self.function)]
                del ax.lines
                ax.lines = hist
                ax.axvline(x, color = 'r')
                canvas = self.allFiguresCanvas[q][1]
                canvas.draw()
        
    
    def handleItemChanged(self, item):
        """
        Trigger when a baseline value in table is changed
        :param item: table item
        """
        if item.column() == self.tableHeader.index("baseline") and not self.updateUIonly:
            difCent = self.mainwin.getCurrentDifCent()
            peak = self.resultTable.item(item.row(), 0).text()
            peak = peak.split(" ")
            quadrant = str(peak[0])
            ind = 0
            if str(peak[1]) == "51":
                ind = 1
            difCent.setOffMerBaseline(quadrant, ind, item.text())
            self.mainwin.processImage()

    def updateUI(self):
        """
        Draw all plots and update result table
        """
        if self.updated:
            return
        self.updateUIonly = True
        difCent = self.mainwin.getCurrentDifCent()
        info = difCent.info
        hists = info["off_mer_hists"]["hists"]
        hulls = info["off_mer_hists"]["hulls"]
        all_peaks = info["off_mer_peaks"]
        all_baselines = info["off_mer_baselines"]
        peak_infos = info["off_mer_peak_info"]

        self.resultTable.setRowCount(8)
        row = 0

        for q in self.quadrants:
            fig = self.allFiguresCanvas[q][0]
            canvas = self.allFiguresCanvas[q][1]
            hist = hists[q]
            hull = hulls[q]
            peaks = all_peaks[q]
            baselines = all_baselines[q]
            peak_info = peak_infos[q]
            centroids = peak_info["centroids"]
            intensity = peak_info["areas"]
            widths = peak_info["widths"]

            ax = self.allFiguresCanvas[q][2]
            ax.cla()
            title = q.replace("_", " ")
            ax.set_title(title)
            ax.plot(hull, color = "g")
            # ax.plot(hist, color="k")

            names = ["59", "51"]

            for i, b in enumerate(baselines):
                width = widths[i]
                cent = centroids[i]
                area = intensity[i]
                p = peaks[i]
                n = names[i]
                name = q + " " + n

                ax.plot((cent - width, cent + width), (b, b), color='m')  ## baseline
                ax.plot((cent, cent), (0, hull[peaks[i]] + 10), color='b') ## centroid
                ax.plot(p, hull[p], 'ro') ## peak
                ax.text(p + 2, hull[p], n, fontsize=13) ## peak name

                ### Update table ###
                item = QTableWidgetItem(name)
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable.setItem(row, self.tableHeader.index('name'), item)

                item = QTableWidgetItem(str(cent))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable.setItem(row, self.tableHeader.index('centroid'), item)

                item = QTableWidgetItem(str(b))
                self.resultTable.setItem(row, self.tableHeader.index('baseline'), item)

                item = QTableWidgetItem(str(area))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable.setItem(row, self.tableHeader.index('intensity'), item)

                item = QTableWidgetItem(str(p))
                item.setFlags(Qt.ItemIsEnabled)
                self.resultTable.setItem(row, self.tableHeader.index('center'), item)
                row += 1

            ax.set_xlim((min(peaks)*0.8, max(peaks)*1.2))
            ax.set_ylim((0, max(hull[peaks]) * 1.1))

            fig.tight_layout()
            canvas.draw()

        self.setSEButton.setText('Set Convex hull range')
        self.updateUIonly = False
        self.updated = True

    def clearSettings(self):
        """
        Set update status to not updated, and clear active function
        """
        self.updated = False
        self.function = None
        for b in self.checkableButtons:
            b.setChecked(False)
        self.setSEButton.setText('Set Convex hull range')

class DiffractionTab(QWidget):
    """
    A class for diffraction tab containnig a plot, display options, settings and result table
    """
    tableHeader = ["reject", "name", "centroid", "baseline", "intensity"]
    def __init__(self, mainwin, dif_side, fix_ranges = []):
        """
        Initial tab
        :param mainwin: main window
        :param dif_side : "top" or "bottom"
        :param fix_ranges: fixed peak ranges
        """
        QWidget.__init__(self)
        self.mainwin = mainwin
        self.fixRanges = fix_ranges
        self.dif_side = dif_side
        self.fixed_se = None
        self.updated = False # update status of tab
        self.function = None # current active function
        self.updateUIonly = False # param for UI sync
        self.zoom = None # zoom locations (x,y ranges) for plot
        self.initUI()
        self.setConnections()

    def initUI(self):
        """
        Initial all layouts and widgets including plot, result table, display options, and settings
        """
        self.setContentsMargins(0, 0, 0, 0)

        #### Plot ####
        self.difTabLayout = QGridLayout(self)
        self.difFigure = plt.figure()
        self.difAxes = self.difFigure.add_subplot(111)
        self.difCanvas = FigureCanvas(self.difFigure)

        #### Result Table ####
        self.resultTable = QTableWidget()
        self.resultTable.setColumnCount(len(DiffractionTab.tableHeader))
        self.resultTable.setHorizontalHeaderLabels(DiffractionTab.tableHeader)
        self.resultTable.setEditTriggers(QAbstractItemView.AllEditTriggers)
        self.resultTable.setFixedWidth(400)
        self.resultTable.horizontalHeader().setStretchLastSection(True)

        self.resultTable.setColumnWidth(0, 40)
        self.resultTable.setColumnWidth(1, 50)
        self.resultTable.setColumnWidth(2, 80)
        self.resultTable.setColumnWidth(3, 80)
        self.resultTable.setColumnWidth(4, 80)

        #### Display Options ####
        self.orignalChkBx = QCheckBox("Display Original Histogram")
        self.orignalChkBx.setChecked(False)
        self.seChkBx = QCheckBox("Start-End Convexhull Points")
        self.seChkBx.setChecked(False)
        self.frHistChkBx = QCheckBox("Display histogram in fixed ranges only")
        self.frHistChkBx.setChecked(True)
        self.peakChkBx = QCheckBox("Display Peaks")
        self.peakChkBx.setChecked(True)
        self.centroidChkBx = QCheckBox("Display Centroids")
        self.centroidChkBx.setChecked(True)
        self.baselineChkBx = QCheckBox("Display Baselines")
        self.baselineChkBx.setChecked(True)
        self.displayOptGrp = QGroupBox("Display Options")
        self.displayOptLayout = QGridLayout()
        self.zoomInB = QPushButton("Zoom in")
        self.zoomInB.setCheckable(True)
        self.zoomInB.setFixedSize(100, 40)
        self.zoomOutB = QPushButton("Full")
        self.zoomOutB.setFixedSize(100, 40)
        self.displayOptGrp.setLayout(self.displayOptLayout)
        self.displayOptLayout.addWidget(self.zoomInB, 0, 0, 2, 1)
        self.displayOptLayout.addWidget(self.zoomOutB, 2, 0, 2, 1)
        self.displayOptLayout.addWidget(self.orignalChkBx, 0, 1, 1, 1)
        self.displayOptLayout.addWidget(self.seChkBx, 1, 1, 1, 1)
        self.displayOptLayout.addWidget(self.frHistChkBx, 2, 1, 1, 1)
        self.displayOptLayout.addWidget(self.peakChkBx, 0, 2, 1, 1)
        self.displayOptLayout.addWidget(self.centroidChkBx, 1, 2, 1, 1)
        self.displayOptLayout.addWidget(self.baselineChkBx, 2, 2, 1, 1)
        self.displayOptLayout.setAlignment(self.zoomInB, Qt.AlignCenter)
        self.displayOptLayout.setAlignment(self.zoomOutB, Qt.AlignCenter)

        ### Settings ####
        self.manualPeakSelect = QPushButton("Start Manual Peak Selection")
        self.manualPeakSelect.setFixedSize(250, 40)
        self.manualPeakSelect.setCheckable(True)
        self.manualPeakSelect.setEnabled(len(self.fixRanges) == 0)
        self.manualSESelect = QPushButton("Select Start-End Convexhull Points")
        self.manualSESelect.setFixedSize(250, 40)
        self.manualSESelect.setCheckable(True)
        self.checkableButtons = [self.zoomInB, self.zoomOutB, self.manualPeakSelect, self.manualSESelect]

        self.calSettingsGrp = QGroupBox("Settings")
        self.calSettingsLayout = QGridLayout()
        # self.calSettingsLayout.addWidget(self.manualPeakSelect, 0, 0, 1, 1)
        self.calSettingsLayout.addWidget(self.manualSESelect, 0, 0, 1, 1)
        self.calSettingsGrp.setLayout(self.calSettingsLayout)

        self.difTabLayout.addWidget(self.difCanvas, 0, 0, 1, 5)
        self.difTabLayout.addWidget(self.resultTable, 0, 5, 1, 1)
        self.difTabLayout.addWidget(self.displayOptGrp, 1, 0, 1, 1)
        self.difTabLayout.addWidget(self.calSettingsGrp, 1, 1, 1, 1)
        self.difTabLayout.setRowStretch(0, 4)
        self.difTabLayout.setRowStretch(1, 1)
        self.difTabLayout.setColumnStretch(0, 1)
        self.difTabLayout.setColumnStretch(1, 1)
        self.difTabLayout.setColumnStretch(2, 1)
        self.difTabLayout.setColumnStretch(3, 1)
        self.difTabLayout.setColumnStretch(4, 2)
        self.difTabLayout.setColumnStretch(5, 5)

    def setConnections(self):
        """
        Set connections for interactive widgets
        """
        self.orignalChkBx.stateChanged.connect(self.difSettingChanged)
        self.seChkBx.stateChanged.connect(self.difSettingChanged)
        self.frHistChkBx.stateChanged.connect(self.difSettingChanged)
        self.peakChkBx.stateChanged.connect(self.difSettingChanged)
        self.centroidChkBx.stateChanged.connect(self.difSettingChanged)
        self.baselineChkBx.stateChanged.connect(self.difSettingChanged)
        self.manualPeakSelect.clicked.connect(self.manualPeakSelectClicked)
        self.manualSESelect.clicked.connect(self.manualSESelectClicked)
        self.zoomInB.clicked.connect(self.zoomInClicked)
        self.zoomOutB.clicked.connect(self.zoomOutClicked)
        self.difFigure.canvas.mpl_connect("button_press_event", self.plotClicked)
        self.difFigure.canvas.mpl_connect("motion_notify_event", self.plotOnMotion)
        self.difFigure.canvas.mpl_connect("button_release_event", self.plotReleased)
        self.difFigure.canvas.mpl_connect("scroll_event", self.plotScrolled)
        self.resultTable.itemClicked.connect(self.handleItemClicked)
        self.resultTable.itemChanged.connect(self.handleItemChanged)

    def handleItemChanged(self, item):
        """
        Trigger when a item in table is changed
        :param item:
        :return:
        """
        if item.column() == DiffractionTab.tableHeader.index("baseline") and not self.updateUIonly:
            self.mainwin.setBaseline(self.dif_side, item.row(), item.text())

    def handleItemClicked(self, item):
        """
        Trigger when a item in table is clicked
        :param item:
        :return:
        """
        if item.column() == DiffractionTab.tableHeader.index("reject"):
            # If a peak is rejected
            peak_name = self.resultTable.item(item.row(), DiffractionTab.tableHeader.index("name")).text()
            # print peak, item.checkState()==2
            difCent = self.mainwin.getCurrentDifCent()
            reject_status = difCent.info["reject"][self.dif_side]
            if item.checkState()==Qt.Checked and peak_name not in reject_status:
                reject_status.append(peak_name)
                difCent.cacheInfo()
                self.mainwin.writeData()
            elif item.checkState()==Qt.Unchecked and peak_name in reject_status:
                reject_status.remove(peak_name)
                difCent.cacheInfo()
                self.mainwin.writeData()

    def keyPressEvent(self, event):
        # Trigger when a key on keyboard is pressed
        self.mainwin.keyPressEvent(event)

    def manualSESelectClicked(self):
        """
        Set active function for setting start and end points for convex hull
        """
        if self.manualSESelect.isChecked():
            self.manualSESelect.setText("Select Start Point")
            self.function = ["se", []]
            ax = self.difAxes
            ax.cla()
            difCent = self.mainwin.getCurrentDifCent()
            hist = difCent.info[self.dif_side+"_hist"]
            ax.plot(hist, color = "b")
            self.difCanvas.draw_idle()
        else:
            self.manualSESelect.setText("Select Start-End Convexhull Points")
            self.updated = False
            self.function = None
            self.manualPeakSelect.setChecked(False)
            self.updateUI()

    def manualPeakSelectClicked(self):
        """
        Set active function for peak selections
        """
        if self.manualPeakSelect.isChecked():
            self.manualPeakSelect.setText("Stop Manual Peak Selection")
            self.function = ["peaks", []]
            ax = self.difAxes
            ax.cla()
            difCent = self.mainwin.getCurrentDifCent()
            hist = difCent.info[self.dif_side + "_hull"]
            ax.plot(hist, color="g")
            if self.zoom is not None and len(self.zoom) == 2:
                ax.set_xlim(self.zoom[0])
                ax.set_ylim(self.zoom[1])
            self.difCanvas.draw_idle()
        else:
            self.manualPeakSelect.setText("Start Manual Peak Selection")
            difCent = self.mainwin.getCurrentDifCent()
            difCent.info["pre_" + self.dif_side + "_peaks"] = self.function[1]
            difCent.removeInfo(self.dif_side + "_peaks")
            self.manualPeakSelect.setChecked(False)
            difCent.info["reject"][self.dif_side] = []
            self.mainwin.processImage()

    def clearSettings(self):
        """
        Clear update status and current actuve function
        """
        self.mainwin.redrawPlots()
        self.updated = False
        self.function = None
        for b in self.checkableButtons:
            b.setChecked(False)

    def zoomInClicked(self):
        """
        Set active function for zoom in area selection.
        """
        difCent = self.mainwin.getCurrentDifCent()
        if difCent is None:
            return

        if self.zoomInB.isChecked():
            self.function = ["zoomin"]
        else:
            self.function = None

    def zoomOutClicked(self):
        # Clear zoom locations and redraw everything
        self.updated = False
        self.mainwin.redrawPlots()
        self.zoom = None
        self.updateUI()

    def plotClicked(self, event):
        """
        Triggered when mouse presses on the graph
        """
        difCent = self.mainwin.getCurrentDifCent()
        if difCent is None:
            return

        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            ax = self.difAxes
            bounds = ax.get_window_extent().get_points()  ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[1] - ylim[0]) / (bounds[0][1] - bounds[1][1])  ### todo
            cy = ylim[0] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])


        if self.function is None:
            self.function = ["move", (x,y)]
        else:
            func = self.function
            if func[0] == "zoomin":
                # Set zoom in location
                func.append((x,y))
                if len(func) == 3:
                    p1 = func[1]
                    p2 = func[2]
                    self.zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.updated = False
                    self.mainwin.redrawPlots()
                    self.zoomInB.setChecked(False)
                    self.updateUI()
            elif func[0] == "peaks":
                # Set peak locations
                hist = difCent.info[self.dif_side+"_hull"]
                selected_peaks = func[1]
                x = int(round(x))
                selected_peaks.append(x)
                ax = self.difAxes
                ax.axvline(x, color = "r")
                ax.text(x, hist[x]+15, "peak#"+str(len(selected_peaks)), fontsize=15)
                self.difCanvas.draw_idle()
            elif func[0] == "se":
                # Set start or end points foro convex hull
                selected_peaks = func[1]
                x = int(round(x))
                selected_peaks.append(x)
                if len(selected_peaks) == 1:
                    ax = self.difAxes
                    ax.plot((x, x), (0, ax.get_ylim()[1]), color="r")
                    ax.text(x, 0, "start", fontsize=15, horizontalalignment = "right")
                    self.difCanvas.draw_idle()
                    self.manualSESelect.setText("Select End Point")
                else:
                    difCent = self.mainwin.getCurrentDifCent()
                    self.fixed_se = (selected_peaks[0], selected_peaks[1])
                    difCent.removeInfo(self.dif_side + "_se")
                    difCent.removeInfo(self.dif_side + "_hist")
                    difCent.removeInfo(self.dif_side + "_hull")
                    self.manualSESelect.setText("Select Start-End Convexhull Points")
                    self.manualSESelect.setChecked(False)
                    self.mainwin.processImage()

    def plotOnMotion(self, event):
        """
        Triggered when mouse hovers on the graph
        """
        difCent = self.mainwin.getCurrentDifCent()
        if difCent is None:
            return

        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.mainwin.pixel_detail.setText("")
            ax = self.difAxes
            bounds = ax.get_window_extent().get_points()  ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[1] - ylim[0]) / (bounds[0][1] - bounds[1][1])  ### todo
            cy = ylim[0] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])
        else:
            self.mainwin.pixel_detail.setText("x=" + str(np.round(x,2)) + ', y=' + str(np.round(y,2)))

        if self.function is None or len(self.function) < 2:
            return

        func = self.function
        hist = difCent.info[self.dif_side+"_hist"]

        if func[0] == "zoomin":
            # draw rectangle
            ax = self.difAxes
            if len(ax.patches) > 0:
                ax.patches.pop(0)
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x,y), w, h,
                                           linewidth=1, edgecolor="r", facecolor="none", linestyle="dotted"))
            self.difCanvas.draw_idle()
        elif func[0] == "move":
            # change zoom location (plot limit)
            if self.zoom is not None:
                ax = self.difAxes
                move = (func[1][0] - x, func[1][1] - y)
                self.zoom = getNewZoom(self.zoom, move, len(hist), max(hist)*1.1)
                ax.set_xlim(self.zoom[0])
                ax.set_ylim(self.zoom[1])
                self.difCanvas.draw_idle()

    def plotReleased(self, event):
        """
        Trigger when mouse releases from graph
        """
        if self.function is not None and self.function[0] == "move":
            self.function = None

    def plotScrolled(self, event):
        """
        This function is called when a mouse scrolled on the graph in fitting tab. This will affect zoom-in and zoom-out
        """
        difCent = self.mainwin.getCurrentDifCent()
        if difCent is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata

        if self.max_size is None:
            hist = difCent.info[self.dif_side + '_hist']
            self.max_size = (max(hist), len(hist))

        if self.zoom is None:
            self.zoom = [(0, self.max_size[1]), (0, self.max_size[0])]

        zoom_height = self.zoom[1][1] - self.zoom[1][0]
        zoom_width = self.zoom[0][1] - self.zoom[0][0]

        clicked_x_percentage = 1. * (x - self.zoom[0][0]) / zoom_width
        clicked_y_percentage = 1. * (y - self.zoom[1][0]) / zoom_height

        step_x = .1 * zoom_width
        step_y = .1 * zoom_height
        if direction == 'up':  # zoom in
            step_x *= -1
            step_y *= -1
        zoom_width = min(self.max_size[1], max(zoom_width + step_x, 5))
        zoom_height = min(self.max_size[0], max(zoom_height + step_y, 20))

        x1 = x - clicked_x_percentage * zoom_width
        x2 = x1 + zoom_width
        y1 = y - clicked_y_percentage * zoom_height
        y2 = y1 + zoom_height

        if x1 < 0:
            x1 = 0
            x2 = zoom_width

        if y1 < 0:
            y1 = 0
            y2 = zoom_height

        if x2 > self.max_size[1]:
            x2 = self.max_size[1]
            x1 = self.max_size[1] - zoom_width

        if y2 > self.max_size[0]:
            y2 = self.max_size[0]
            y1 = self.max_size[0] - zoom_height

        # Set new x,y ranges for graph
        self.zoom = [(x1, x2), (y1, y2)]
        ax = self.difAxes
        ax.set_xlim(self.zoom[0])
        ax.set_ylim(self.zoom[1])
        self.difCanvas.draw_idle()

    def difSettingChanged(self):
        """
        Redraw plot
        """
        if self.updateUIonly:
            return
        self.updated = False
        self.mainwin.redrawPlots()
        self.updateUI()

    def drawPlot(self, figure, canvas, ax, info):
        """
        Draw graph to input figure and canvas from current display settings
        :param figure: matplotlib figure
        :param canvas: matplotlib canvas
        :param info: DiffractionCentroids info
        :return: -
        """
        orig_hist = info[self.dif_side + '_hist']
        hull_hist = info[self.dif_side + '_hull']
        centroids = info[self.dif_side + '_centroids']
        peaks = info[self.dif_side + '_peaks']
        baselines = info[self.dif_side + '_baselines']
        widths = info[self.dif_side + '_widths']
        names = info[self.dif_side + '_names']
        x_range = None
        ax.cla()

        if self.frHistChkBx.isChecked():
            # Draw histogram only in fixed ranges
            if len(self.fixRanges) > 0:
                new_hist = np.zeros(len(hull_hist))
                min_x = len(hull_hist)
                max_x = 0
                for fr in self.fixRanges:
                    r = fr[1]
                    new_hist[r[0]: r[1]] = hull_hist[r[0]: r[1]]
                    min_x = min(min_x, r[0])
                    max_x = max(max_x, r[1])
                x_range = (min_x*.8, max_x*1.2)
                hull_hist = new_hist
            elif len(peaks) > 0:
                x_range = (min(peaks)*.8, max(peaks)*1.2)

        # Draw background subtracted histogram
        ax.plot(hull_hist, color='g')
        max_height = max(hull_hist) * 1.05

        if self.orignalChkBx.isChecked():
            # Draw original histogram
            ax.plot(orig_hist, color='k')
            max_height = max(orig_hist) * 1.05

        if self.seChkBx.isChecked():
            # Draw start and end points for convex hull
            ax.plot((info[self.dif_side + '_se'][0], info[self.dif_side + '_se'][0]), (0, max_height), color='k')
            ax.plot((info[self.dif_side + '_se'][1], info[self.dif_side + '_se'][1]), (0, max_height), color='k')
            ax.text(info[self.dif_side + '_se'][0], 0, "start", fontsize=15, horizontalalignment='right')
            ax.text(info[self.dif_side + '_se'][1], 0, "end", fontsize=15, horizontalalignment='left')

        if self.baselineChkBx.isChecked() or self.centroidChkBx.isChecked():
            for i, b in enumerate(baselines):
                width = widths[i]
                cent = centroids[i]
                if self.baselineChkBx.isChecked():
                    # Draw baseline
                    ax.plot((cent - width, cent + width), (b, b), color='m')
                if self.centroidChkBx.isChecked():
                    # Draw centroid
                    ax.plot((cent, cent), (0, hull_hist[peaks[i]] + 10), color='b')

        if self.peakChkBx.isChecked():
            # Draw peak lines
            for i in range(len(peaks)):
                p = peaks[i]
                n = names[i]
                ax.plot(p, hull_hist[p], 'ro')
                ax.text(p+2, hull_hist[p], n, fontsize=13)

        # Set display limit
        if self.zoom is not None and len(self.zoom) == 2:
            ax.set_xlim(self.zoom[0])
            ax.set_ylim(self.zoom[1])
        else:
            ax.set_ylim((0, max_height))
            if x_range is not None:
                ax.set_xlim(x_range)

        self.zoom = [ax.get_xlim(), ax.get_ylim()]
        self.max_size = (max_height, len(hull_hist))
        figure.tight_layout()
        canvas.draw()

    def updateUI(self):
        """
        Update plot and table
        """
        difCent = self.mainwin.getCurrentDifCent()
        if self.updated or difCent is None:
            return
        self.updateUIonly = True
        info = difCent.info
        centroids = info[self.dif_side+'_centroids']
        # peaks = info[self.dif_side+'_peaks']
        baselines = info[self.dif_side+'_baselines']
        areas = info[self.dif_side + '_areas']
        reject = info["reject"][self.dif_side]
        names = info[self.dif_side + '_names']

        # Draw plot
        self.drawPlot(self.difFigure, self.difCanvas, self.difAxes, info)

        # Update table
        self.resultTable.setRowCount(len(centroids))
        for i in range(len(centroids)):
            item = QTableWidgetItem(str(names[i]))
            item.setFlags(Qt.ItemIsEnabled)
            self.resultTable.setItem(i, DiffractionTab.tableHeader.index('name'), item)

            item = QTableWidgetItem(str(centroids[i]))
            item.setFlags(Qt.ItemIsEnabled)
            self.resultTable.setItem(i, DiffractionTab.tableHeader.index('centroid'), item)

            # item.setFlags(Qt.ItemIsSelectable |  Qt.ItemIsEnabled | Qt.ItemIsEditable)
            self.resultTable.setItem(i, DiffractionTab.tableHeader.index('baseline'), QTableWidgetItem(str(baselines[i])))

            item = QTableWidgetItem(str(areas[i]))
            item.setFlags(Qt.ItemIsEnabled)
            self.resultTable.setItem(i, DiffractionTab.tableHeader.index('intensity'), item)

            chkBoxItem = QTableWidgetItem()
            chkBoxItem.setFlags(Qt.ItemIsUserCheckable | Qt.ItemIsEnabled)
            if names[i] in reject:
                chkBoxItem.setCheckState(Qt.Checked)
            else:
                chkBoxItem.setCheckState(Qt.Unchecked)
            self.resultTable.setItem(i, DiffractionTab.tableHeader.index('reject'), chkBoxItem)

        self.updateUIonly = False
        self.updated = True

class DiffractionCentroidProcessWindow(QMainWindow):
    """
    A class for Diffraction Centroids process window
    """
    def __init__(self, mainwin, dir_path, groupList, settings):
        """
        Initial window with
        :param mainwin: ptr to main window object
        :param dir_path: directory
        :param groupList: list of list of images
        :param settings: settings from main window
        """
        QWidget.__init__(self)
        self.mainWindow = mainwin # ptr to main window
        self.dir_path = dir_path # current directory path
        self.groupList = groupList # list of list of images
        self.currentGroup = 0 # current active group
        self.difCent = None # DiffractionCentroids object
        self.updateUIonly = False # updating state
        self.updated = False # updated state for image
        self.plots_updated = False # updated state for plots
        self.function = None # current active function
        self.img_zoom = None # current zoom area
        self.fixRanges = [] # fixed meridian peak ranges
        self.firstImg = True # boolean if the window has processed an image before
        self.rotatedImg = None # rotated avarage image of DiffractionCentroids
        self.off_mer = None # off-meridian settings
        self.orientationModel = None

        if 'fix_ranges' in settings:
            self.fixRanges = settings['fix_ranges']

        if 'start_group' in settings:
            self.currentGroup = settings['start_group']

        if 'off_meridian' in settings:
            self.off_mer = settings['off_meridian']

        self.csvManager = DC_CSVManager(dir_path, settings['group'], self.fixRanges) # create CSV manager
        self.initUI(settings['group'] > 0)
        QApplication.processEvents()
        self.setConnections()
        self.onImageChanged()

    def getCurrentDifCent(self):
        # get current DiffractionCentroids object
        return self.difCent

    def initUI(self, pnEnable = False):
        """
        Initial all UI in image tab and create other tabs
        :param pnEnable:
        :return:
        """
        self.setWindowTitle("Muscle X Diffraction Centroids v."+musclex.__version__)
        self.centralWidget = QWidget(self)
        self.mainLayout = QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        ### Tab ###
        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 300px; }")
        self.mainLayout.addWidget(self.tabWidget)

        ## Image Tab ##
        self.initUIImageTab(pnEnable)

        ## Top Diffraction Tab ##
        self.topDifTab = DiffractionTab(self, 'top', self.fixRanges)
        self.tabWidget.addTab(self.topDifTab, "Top Diffraction")

        ## Bottom Diffraction ##
        self.bottomDifTab = DiffractionTab(self, 'bottom', self.fixRanges)
        self.tabWidget.addTab(self.bottomDifTab, "Bottom Diffraction")

        ## Off-meridian Tab ##
        if self.off_mer is not None:
            self.offMerTab = OffMeridianTab(self, self.off_mer)
            self.tabWidget.addTab(self.offMerTab, "Off-Meridian")

        # Status bar
        self.statusBar = QStatusBar()
        self.left_status = QLabel("Path : "+self.dir_path)
        self.right_status = QLabel()
        self.pixel_detail = QLabel()
        self.progressBar = QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusBar.addWidget(self.left_status)
        self.statusBar.addPermanentWidget(self.pixel_detail)
        self.statusBar.addPermanentWidget(self.right_status)
        self.statusBar.addPermanentWidget(self.progressBar)
        self.mainLayout.addWidget(self.statusBar)

        self.show()
        self.resize(1000, 750)

    def initUIImageTab(self, pnEnable = True):
        """
        Initial layouts and widgets for image tab
        :param pnEnable: enable previous and next buttons
        :return: -
        """

        self.imageTab = QWidget()
        self.imageTab.setContentsMargins(0, 0, 0, 0)
        self.imageTabLayout = QGridLayout(self.imageTab)
        self.displayImgFigure = plt.figure()
        self.displayImgAxes = self.displayImgFigure.add_subplot(111)
        self.displayImgCanvas = FigureCanvas(self.displayImgFigure)

        self.plotsLayout = QVBoxLayout()
        self.topDifFigure = plt.figure()
        self.topDifAxes = self.topDifFigure.add_subplot(111)
        self.topDifCanvas = FigureCanvas(self.topDifFigure)
        self.bottomDifFigure = plt.figure()
        self.bottomDifAxes = self.bottomDifFigure.add_subplot(111)
        self.bottomDifCanvas = FigureCanvas(self.bottomDifFigure)
        self.plotsLayout.addWidget(self.topDifCanvas)
        self.plotsLayout.addWidget(self.bottomDifCanvas)

        ### image names group ###
        self.imageOptionsFrame = QFrame()
        self.imageOptionsFrame.setFixedWidth(300)
        self.imageNameGrp = QGroupBox('Images')
        self.imageNames = QLabel()
        self.imnLayout = QVBoxLayout()
        self.imnLayout.addWidget(self.imageNames)
        self.imageNameGrp.setLayout(self.imnLayout)

        ### Display options ###
        self.areaChkBx = QCheckBox("Meridian Area")
        self.areaChkBx.setChecked(True)
        self.offMerChkBx = QCheckBox("Off-meridian Areas")
        self.offMerChkBx.setChecked(self.off_mer is not None)
        self.offMerChkBx.setEnabled(self.off_mer is not None)
        self.graphChkBx = QCheckBox("Graphs")
        self.graphChkBx.setChecked(True)
        self.intensityGrp = QGroupBox()
        self.intensityGrp.setTitle("Image Intensity")
        self.intensityGrp.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Maximum)
        self.intensityLayout = QGridLayout()
        self.intensityGrp.setLayout(self.intensityLayout)
        self.maxInt = QDoubleSpinBox()
        self.maxInt.setValue(1.)
        self.maxInt.setKeyboardTracking(False)
        self.maxIntLabel = QLabel("Max intensity")
        self.minInt = QDoubleSpinBox()
        self.minInt.setValue(0.)
        self.minInt.setKeyboardTracking(False)
        self.minIntLabel = QLabel("Min intensity")
        self.logScaleIntChkBx = QCheckBox("Log scale intensity")
        self.intensityLayout.addWidget(self.minIntLabel, 0, 0)
        self.intensityLayout.addWidget(self.minInt, 0, 1)
        self.intensityLayout.addWidget(self.maxIntLabel, 1, 0)
        self.intensityLayout.addWidget(self.maxInt, 1, 1)
        self.intensityLayout.addWidget(self.logScaleIntChkBx, 2, 0, 1, 2)

        self.zoomLayout = QHBoxLayout()
        self.zoomInB = QPushButton("Zoom in\nImage")
        self.zoomInB.setCheckable(True)
        self.zoomInB.setFixedSize(120, 50)
        self.zoomOutB = QPushButton("Full Image")
        self.zoomOutB.setFixedSize(120, 50)
        self.zoomLayout.addWidget(self.zoomInB)
        self.zoomLayout.addWidget(self.zoomOutB)

        self.displayOptionGrp = QGroupBox()
        self.displayOptionGrp.setTitle('Display Options')
        self.displayOptionsLayout = QVBoxLayout()
        self.displayOptionsLayout.addWidget(self.areaChkBx)
        self.displayOptionsLayout.addWidget(self.offMerChkBx)
        self.displayOptionsLayout.addWidget(self.graphChkBx)
        self.displayOptionsLayout.addSpacing(5)
        self.displayOptionsLayout.addWidget(self.intensityGrp)
        self.displayOptionsLayout.addLayout(self.zoomLayout)
        self.displayOptionGrp.setLayout(self.displayOptionsLayout)


        ### Image processing ###
        self.calSettingsGrp = QGroupBox()
        self.calSettingsGrp.setTitle('Calculation Settings')
        self.calSetttingsLayout = QGridLayout()
        self.setCenterAngleB = QPushButton('Set Center and Rotation Angle')
        self.setCenterAngleB.setCheckable(True)
        self.setAngleB = QPushButton('Set Rotation Angle')
        self.setAngleB.setCheckable(True)
        self.selectIntArea = QPushButton('Set Meridian Area')
        self.selectIntArea.setCheckable(True)
        self.setX1X2 = QPushButton('Set Left\nOff-Meridian Area')
        self.setX1X2.setCheckable(True)
        self.setX1X2.setEnabled(self.off_mer is not None)
        self.setX3X4 = QPushButton('Set Right\nOff-Meridian Area')
        self.setX3X4.setCheckable(True)
        self.setX3X4.setEnabled(self.off_mer is not None)
        self.orientationCmbBx = QComboBox()
        self.orientationCmbBx.addItem("Max Intensity")
        self.orientationCmbBx.addItem("GMM")
        self.orientationCmbBx.addItem("Herman Factor (Half Pi)")
        self.orientationCmbBx.addItem("Herman Factor (Pi)")
        self.rotation90ChkBx = QCheckBox("Rotate 90")
        self.forceRot90ChkBx = QCheckBox("Persist Rotation")
        self.calSettingsGrp.setLayout(self.calSetttingsLayout)
        self.calSetttingsLayout.addWidget(self.setCenterAngleB, 0, 0, 1, 2)
        self.calSetttingsLayout.addWidget(self.setAngleB, 1, 0, 1, 2)
        self.calSetttingsLayout.addWidget(self.selectIntArea, 2, 0, 1, 2)
        self.calSetttingsLayout.addWidget(self.setX1X2, 3, 0, 1, 1)
        self.calSetttingsLayout.addWidget(self.setX3X4, 3, 1, 1, 1)
        self.calSetttingsLayout.addWidget(QLabel("Orientation Finding: "), 4, 0, 1, 2)
        self.calSetttingsLayout.addWidget(self.orientationCmbBx, 5, 0, 1, 2)
        self.calSetttingsLayout.addWidget(self.rotation90ChkBx, 6, 0, 1, 1)
        self.calSetttingsLayout.addWidget(self.forceRot90ChkBx, 6, 1, 1, 1)

        self.checkableButtons = [self.zoomInB, self.zoomOutB, self.setCenterAngleB, self.setAngleB, self.selectIntArea, self.setX3X4, self.setX1X2]

        ### Process Folder Button
        self.processFolderButton = QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet("QPushButton { color: #ededed; background-color: #af6207}")
        self.processFolderButton.setCheckable(True)
        ### Previous & Next buttons
        self.pnButtons = QGridLayout()
        self.prevButton = QPushButton('<')
        self.prevButton.clearFocus()
        self.prevButton.setEnabled(pnEnable)
        self.nextButton = QPushButton('>')
        self.nextButton.setEnabled(pnEnable)
        self.pnButtons.addWidget(self.processFolderButton, 0, 0, 1, 2)
        self.pnButtons.addWidget(self.prevButton, 1, 0, 1, 1)
        self.pnButtons.addWidget(self.nextButton, 1, 1, 1, 1)

        ### Display image ###
        self.imageOptionsLayout = QVBoxLayout()
        self.imageOptionsLayout.setAlignment(Qt.AlignTop)
        self.imageOptionsLayout.addWidget(self.imageNameGrp)
        self.imageOptionsLayout.addWidget(self.displayOptionGrp)
        self.imageOptionsLayout.addWidget(self.calSettingsGrp)
        self.imageOptionsLayout.addStretch()
        self.imageOptionsLayout.addLayout(self.pnButtons)
        self.imageOptionsFrame.setLayout(self.imageOptionsLayout)

        self.imageTabLayout.addWidget(self.displayImgCanvas, 0, 0, 1, 1)
        self.imageTabLayout.addLayout(self.plotsLayout, 0, 1, 1, 1)
        self.imageTabLayout.addWidget(self.imageOptionsFrame, 0, 2, 1, 1)
        self.imageTabLayout.setColumnStretch(0, 3)
        self.imageTabLayout.setColumnStretch(1, 2)
        self.imageTabLayout.setColumnStretch(2, 1)
        self.tabWidget.addTab(self.imageTab, "Image")

    def setConnections(self):
        # Set Connections for interactive widgets
        self.tabWidget.currentChanged.connect(self.updateUI)

        ### image tab
        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imageClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imageOnMotion)
        self.displayImgFigure.canvas.mpl_connect('button_release_event', self.imageReleased)
        self.displayImgFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)
        self.areaChkBx.stateChanged.connect(self.imageSettingChanged)
        self.offMerChkBx.stateChanged.connect(self.imageSettingChanged)
        self.graphChkBx.stateChanged.connect(self.hidePlots)
        self.maxInt.valueChanged.connect(self.imageSettingChanged)
        self.minInt.valueChanged.connect(self.imageSettingChanged)
        self.logScaleIntChkBx.stateChanged.connect(self.imageSettingChanged)
        self.orientationCmbBx.currentIndexChanged.connect(self.orientationModelChanged)
        self.rotation90ChkBx.stateChanged.connect(self.rotation90Checked)
        self.forceRot90ChkBx.stateChanged.connect(self.forceRot90Checked)
        self.prevButton.clicked.connect(self.prevClicked)
        self.nextButton.clicked.connect(self.nextClicked)
        #self.processFolderButton.clicked.connect(self.processCurrentFolder)
        self.processFolderButton.toggled.connect(self.batchProcBtnToggled)
        self.selectIntArea.clicked.connect(self.selectIntAreaClicked)
        self.setCenterAngleB.clicked.connect(self.setCenterAngleClicked)
        self.setAngleB.clicked.connect(self.setAngleClicked)
        self.setX1X2.clicked.connect(self.setX1X2Clicked)
        self.setX3X4.clicked.connect(self.setX3X4Clicked)
        self.zoomInB.clicked.connect(self.imgZoomInClicked)
        self.zoomOutB.clicked.connect(self.imgZoomOutClicked)

    def setBaseline(self, side, peak_ind, new_baseline):
        ## New baseline is sent from diffraction tab and then send it to DifftactionCentroids object
        if self.difCent is not None and not self.updateUIonly:
            self.difCent.setBaseline(side, peak_ind, new_baseline)
            self.processImage()

    def hidePlots(self):
        # Hide all plots in the first tab (image)
        self.topDifCanvas.setHidden(not self.graphChkBx.isChecked())
        self.bottomDifCanvas.setHidden(not self.graphChkBx.isChecked())
        if self.graphChkBx.isChecked():
            self.imageTabLayout.setColumnStretch(1, 2)
        else:
            self.imageTabLayout.setColumnStretch(1, 0)

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
            self.refreshUI()

    def clearFunction(self):
        """
        Clear all active function and updated status
        """
        self.function = None
        self.updated = False
        self.mainwin.redrawPlots()
        self.updateUI()

    def imgZoomOutClicked(self):
        """
        Trigger when "Full" clicked
        :return:
        """
        if self.difCent is None:
            return
        self.img_zoom = None
        self.function = None
        self.updated = False
        self.updateUI()

    def imgZoomInClicked(self):
        """
        Trigger when "Zoom in" clicked. Set active function to trigger zoom in setting
        :return: -
        """
        if self.difCent is None:
            return
        if self.zoomInB.isChecked():
            self.function = ["zoomin"]
        else:
            self.function = None
            self.updated = False
            self.updateUI()
    
    def setCenterAngleClicked(self):
        """
        Set active function to trigger rotation angle setting
        :return: -
        """
        if self.difCent is None:
            return
        if self.setCenterAngleB.isChecked():
            ax = self.displayImgAxes
            while len(ax.lines) > 0:
                ax.lines.pop(len(ax.lines) - 1)
            self.displayImgCanvas.draw_idle()
            self.function = ["center_angle"]
        else:
            self.function = None
            self.updated = False
            self.updateUI()
    
    def setAngleClicked(self):
        """
        Set active function to trigger rotation angle setting
        :return: -
        """
        if self.difCent is None:
            return

        if self.setAngleB.isChecked():
            ax = self.displayImgAxes
            while len(ax.lines) > 0:
                ax.lines.pop(len(ax.lines) - 1)
            self.displayImgCanvas.draw_idle()
            self.function = ["angle"]
        else:
            self.function = None
            self.updated = False
            self.updateUI()
            
    def setX1X2Clicked(self):
        """
        Set active function to trigger x1,x2 setting
        :return:
        """
        if self.difCent is None:
            return
        if self.setX1X2.isChecked():
            ax = self.displayImgAxes
            while len(ax.lines) > 0:
                ax.lines.pop(len(ax.lines) - 1)
            self.displayImgCanvas.draw_idle()
            self.function = ["x1x2"]
        else:
            self.function = None
            self.updated = False
            self.updateUI()

    def setX3X4Clicked(self):
        """
        Set active function to trigger x3,x4 setting
        :return:
        """
        if self.difCent is None:
            return
        if self.setX3X4.isChecked():
            ax = self.displayImgAxes
            while len(ax.lines) > 0:
                ax.lines.pop(len(ax.lines) - 1)
            self.displayImgCanvas.draw_idle()
            self.function = ["x3x4"]
        else:
            self.function = None
            self.updated = False
            self.updateUI()

    def selectIntAreaClicked(self):
        """
        Set active function to trigget integrated area (meridian) setting
        :return:
        """
        if self.difCent is None:
            return

        if self.selectIntArea.isChecked():
            ax = self.displayImgAxes
            while len(ax.lines) > 0:
                ax.lines.pop(len(ax.lines) - 1)
            self.displayImgCanvas.draw_idle()
            self.function = ["int_area"]
        else:
            self.function = None
            self.updated = False
            self.updateUI()

    def imageClicked(self, event):
        """
        Trigger when mouse presses on image in image tab
        """
        if self.difCent is None:
            return
        x = event.xdata
        y = event.ydata

        if self.function is None:
            self.function = ["move", (x,y)]
        else:
            func = self.function
            if func[0] == "int_area":
                # Set inegrated area (meridian)
                func.append(x)
                ax = self.displayImgAxes
                ax.axvline(x, color='r')
                self.displayImgCanvas.draw_idle()
                if len(func) == 3:
                    self.difCent.info['int_area'] = (int(round(func[1])), int(round(func[2])))
                    center = self.difCent.info["center"]
                    self.difCent.info["center"] = [int(round((func[1]+func[2])/2.)), center[1]]
                    self.difCent.removeInfo('top_se')
                    self.difCent.removeInfo('bottom_se')
                    self.function = None
                    self.selectIntArea.setChecked(False)
                    self.processImage()
            elif func[0] == "x1x2":
                # Set x1 or x2 (off-meridian left ranges)
                func.append(x)
                ax = self.displayImgAxes
                ax.axvline(x, color='g')
                self.displayImgCanvas.draw_idle()
                if len(func) == 3:
                    self.difCent.info['x1'] = max(int(round(func[1])), int(round(func[2])))
                    self.difCent.info['x2'] = min(int(round(func[1])), int(round(func[2])))
                    self.difCent.removeInfo('off_mer_rmin_rmax')
                    self.function = None
                    self.setX1X2.setChecked(False)
                    self.processImage()
            elif func[0] == "x3x4":
                # Set x3 or x4 (off-meridian right ranges)
                func.append(x)
                ax = self.displayImgAxes
                ax.axvline(x, color='r')
                self.displayImgCanvas.draw_idle()
                if len(func) == 3:
                    self.difCent.info['x4'] = max(int(round(func[1])), int(round(func[2])))
                    self.difCent.info['x3'] = min(int(round(func[1])), int(round(func[2])))
                    self.difCent.removeInfo('off_mer_rmin_rmax')
                    self.function = None
                    self.setX1X2.setChecked(False)
                    self.processImage()
            elif func[0] == "angle":
                # set rotation angle
                center = self.difCent.info['center']

                if center[0] < x:
                    x1 = center[0]
                    y1 = center[1]
                    x2 = x
                    y2 = y
                else:
                    x1 = x
                    y1 = y
                    x2 = center[0]
                    y2 = center[1]

                if abs(x2 - x1) == 0:
                    new_angle = -90
                else:
                    new_angle = -180. * np.arctan((y1 - y2) / abs(x1 - x2)) / np.pi
                new_angle += 90.

                self.difCent.info['rotationAngle'] = self.difCent.info['rotationAngle'] + new_angle
                self.difCent.removeInfo('rmin')
                self.setAngleB.setChecked(False)
                self.function = None
                self.processImage()
            elif func[0] == "center_angle":
                # set new center and rotation angle
                ax = self.displayImgAxes
                axis_size = 5
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                self.displayImgCanvas.draw_idle()
                func.append((x,y))
                if len(func) == 3:
                    self.setCursor(Qt.ArrowCursor)

                    if func[1][0] < func[2][0]:
                        x1, y1 = func[1]
                        x2, y2 = func[2]
                    else:
                        x1, y1 = func[2]
                        x2, y2 = func[1]

                    if abs(x2 - x1) == 0:
                        new_angle = -90
                    else:
                        new_angle = -180.*np.arctan((y1 - y2) / abs(x1 - x2))/np.pi
                    new_angle += 90.

                    cx = int(round((x1 + x2) / 2.))
                    cy = int(round((y1 + y2) / 2.))
                    M = cv2.getRotationMatrix2D(tuple(self.difCent.info['center']), self.difCent.info['rotationAngle'], 1)
                    invM = cv2.invertAffineTransform(M)
                    homo_coords = [cx, cy, 1.]
                    new_center = np.dot(invM, homo_coords)
                    self.difCent.info['center'] = (int(round(new_center[0])), int(round(new_center[1])))
                    self.difCent.info['rotationAngle'] = self.difCent.info['rotationAngle'] + new_angle
                    self.difCent.removeInfo('rmin')
                    self.setCenterAngleB.setChecked(False)
                    self.processImage()

            elif func[0] == "zoomin":
                # Set zoom in area
                func.append((x,y))
                if len(func) == 3:
                    p1 = func[1]
                    p2 = func[2]
                    self.img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.updated = False
                    self.zoomInB.setChecked(False)
                    self.refreshUI()

    def imageOnMotion(self, event):
        """
        Trigger when mouse hovers on image
        """
        if self.difCent is None or event.xdata is None or event.ydata is None or self.rotatedImg is None:
            self.pixel_detail.setText("")
            return

        x = int(round(event.xdata))
        y = int(round(event.ydata))

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < self.rotatedImg.shape[1] and y < self.rotatedImg.shape[0]:
                self.pixel_detail.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(self.rotatedImg[y][x]))

        ax = self.displayImgAxes

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.pixel_detail.setText("")
            bounds = ax.get_window_extent().get_points()  ## return [[x1,y1],[x2,y2]]
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            mx = (xlim[1] - xlim[0]) / (bounds[1][0] - bounds[0][0])
            cx = xlim[0] - bounds[0][0] * mx
            my = (ylim[0] - ylim[1]) / (bounds[0][1] - bounds[1][1])  ### todo
            cy = ylim[1] - bounds[1][1] * my
            x = event.x * mx + cx
            y = event.y * my + cy
            x = max(x, 0)
            x = min(x, xlim[1])
            y = max(y, 0)
            y = min(y, ylim[0])
            x = int(round(x))
            y = int(round(y))

        func = self.function

        if func is None:
            return

        limit = max(self.difCent.avgImg.shape)

        if func[0] == "int_area":
            # Draw verical lines
            if len(ax.lines) > len(func) - 1:
                line = ax.lines[:len(func)-1]
                del ax.lines
                ax.lines = line
            ax.axvline(x, color='r')
            self.displayImgCanvas.draw_idle()
        elif func[0] == "x1x2":
            # Draw vertical lines
            if len(ax.lines) > len(func) - 1:
                line = ax.lines[:len(func) - 1]
                del ax.lines
                ax.lines = line
            ax.axvline(x, color='g')
            self.displayImgCanvas.draw_idle()
        elif func[0] == "x3x4":
            # Draw vertical lines
            if len(ax.lines) > len(func) - 1:
                line = ax.lines[:len(func) - 1]
                del ax.lines
                ax.lines = line
            ax.axvline(x, color='r')
            self.displayImgCanvas.draw_idle()
        elif func[0] == "angle":
            # draw line as angle
            center = self.difCent.info["center"]
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            del ax.lines
            ax.lines = []
            ax.plot([x,x2],[y,y2], color = "g")
            self.displayImgCanvas.draw_idle()
        elif func[0] == "center_angle":
            axis_size = 5
            if len(func) == 1:
                # draw X
                if len(ax.lines) > 0:
                    del ax.lines
                    ax.lines = []
                # while len(ax.lines) > 0:
                    # ax.lines.pop(len(ax.lines) - 1)
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                # draw X and a line between points
                start_pt = func[1]
                if len(ax.lines) > 2:
                    first_cross = ax.lines[:2]
                    del ax.lines
                    ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')

            self.displayImgCanvas.draw_idle()

        elif func[0] == "zoomin" and len(func) > 1:
            # Draw rectangle
            if len(ax.patches) > 0:
                ax.patches.pop(0)
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            rx = min(start_pt[0], x)
            ry = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((rx, ry), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.displayImgCanvas.draw_idle()
        elif func[0] == "move" and len(func) > 1:
            # change zoom area
            if self.img_zoom is not None:
                move = (func[1][0] - event.xdata, func[1][1] - event.ydata)
                self.img_zoom = getNewZoom(self.img_zoom, move, limit, limit)
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
                self.displayImgCanvas.draw_idle()

    def imageReleased(self, event):
        """
        Trigger when mouse is released from image
        :param event:
        :return:
        """
        if self.function is not None and self.function[0] == "move":
            self.function = None

    def imgScrolled(self, event):
        """
        Trigger when mouse is scrolled on image. This affect zoom area of image
        :param event:
        :return:
        """
        if self.difCent is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img =  self.difCent.getRotatedImage()
        img_size = img.shape
        if self.img_zoom is None:
            self.img_zoom = [(0, img_size[1]), (0, img_size[0])]

        zoom_height = self.img_zoom[1][1] - self.img_zoom[1][0]
        zoom_width = self.img_zoom[0][1] - self.img_zoom[0][0]

        clicked_x_percentage = 1. * (x - self.img_zoom[0][0]) / zoom_width
        clicked_y_percentage = 1. * (y - self.img_zoom[1][0]) / zoom_height

        step_x = .1 * zoom_width
        step_y = .1 * zoom_height
        if direction == 'up':  # zoom in
            step_x *= -1
            step_y *= -1
        zoom_width = min(img_size[1], max(zoom_width + step_x, 50))
        zoom_height = min(img_size[0], max(zoom_height + step_y, 50))

        x1 = x - clicked_x_percentage * zoom_width
        x2 = x1 + zoom_width
        y1 = y - clicked_y_percentage * zoom_height
        y2 = y1 + zoom_height

        if x1 < 0:
            x1 = 0
            x2 = zoom_width

        if y1 < 0:
            y1 = 0
            y2 = zoom_height

        if x2 > img_size[1]:
            x2 = img_size[1]
            x1 = img_size[1] - zoom_width

        if y2 > img_size[0]:
            y2 = img_size[0]
            y1 = img_size[0] - zoom_height

        self.img_zoom = [(x1, x2), (y1, y2)]
        ax = self.displayImgAxes
        ax.set_xlim(self.img_zoom[0])
        ax.set_ylim(self.img_zoom[1])
        self.displayImgCanvas.draw_idle()

    def prevClicked(self):
        # Go to the previous image
        if self.prevButton.isEnabled():
            self.currentGroup = (self.currentGroup - 1) % len(self.groupList)
            self.onImageChanged()

    def nextClicked(self):
        # Go to the next image
        if self.nextButton.isEnabled():
            self.currentGroup = (self.currentGroup + 1) % len(self.groupList)
            self.onImageChanged()

    def batchProcBtnToggled(self):
        if self.processFolderButton.isChecked():
            if not self.progressBar.isVisible():
                self.processFolderButton.setText("Stop")
                self.processCurrentFolder()
        else:
            self.stop_process = True

    def processCurrentFolder(self):
        # Process current folder
        self.progressBar.setMaximum(len(self.groupList))
        self.progressBar.setMinimum(0)
        self.progressBar.setVisible(True)

        self.stop_process = False
        for i in range(len(self.groupList)):
            if self.stop_process:
                break
            self.nextClicked()
            self.progressBar.setValue(i)
            QApplication.processEvents()

        self.progressBar.setVisible(False)
        self.processFolderButton.setChecked(False)
        self.processFolderButton.setText("Process Current Folder")

    def imageSettingChanged(self):
        # update image
        if self.updateUIonly:
            return
        self.updated = False
        self.updateUI()

    def orientationModelChanged(self):
        self.orientationModel = self.orientationCmbBx.currentIndex()
        if self.difCent is None:
            return
        self.difCent.removeInfo('rotationAngle')
        self.processImage()

    def rotation90Checked(self):
        self.difCent.removeInfo('rmin')
        self.processImage()

    def forceRot90Checked(self):
        if self.forceRot90ChkBx.isChecked():
            self.rotation90ChkBx.setChecked(True)
            self.rotation90ChkBx.setEnabled(False)
        else:
            self.rotation90ChkBx.setEnabled(True)

    def onImageChanged(self):
        # Create a new DiffractionCentroids object and process it
        imgList = self.groupList[self.currentGroup]
        self.difCent = DiffractionCentroids(self.dir_path, imgList, self.currentGroup, self.fixRanges, self.off_mer)
        img = self.difCent.avgImg
        self.right_status.setText(str(img.shape[0])+"x"+str(img.shape[1])+" "+str(img.dtype))
        self.initMinMaxIntensities(self.difCent.avgImg)
        if self.rotation90ChkBx.isEnabled():
            self.rotation90ChkBx.setChecked('90rotation' in self.difCent.info and self.difCent.info['90rotation'])
        self.processImage()

    def getFlags(self):
        """
        Get current settings for process()
        :return:
        """
        flags = {}
        flags['orientation_model'] = self.orientationModel
        flags['90rotation'] = self.rotation90ChkBx.isChecked()
        if self.bottomDifTab.fixed_se is not None:
            flags['bottom_fixed_se'] = self.bottomDifTab.fixed_se
        if self.topDifTab.fixed_se is not None:
            flags['top_fixed_se'] = self.topDifTab.fixed_se
        if self.off_mer is not None and self.offMerTab.fixed_hull_range is not None:
            flags['fixed_offmer_hull_range'] = self.offMerTab.fixed_hull_range
        return flags

    def processImage(self):
        # Process current DiffractionCentroids object, refresh UI, and write data to csv file
        if self.difCent is None:
            return
        QApplication.setOverrideCursor(Qt.WaitCursor)
        QApplication.processEvents()
        flags = self.getFlags()
        try:
            self.difCent.process(flags)
        except:
            QApplication.restoreOverrideCursor()
            errMsg = QMessageBox()
            errMsg.setText('Unexpected error')
            msg = 'Please report the problem with error message below and the input image\n\n'
            msg += "Error : "+str(sys.exc_info()[0]) +'\n\n'+str(traceback.format_exc())
            errMsg.setInformativeText(msg)
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.setFixedWidth(300)
            errMsg.exec_()
            raise

        self.updateParams()
        self.writeData()
        self.refreshUI()
        QApplication.restoreOverrideCursor()
        QApplication.processEvents()

    def writeData(self):
        # Write new data to csv file
        self.csvManager.writeNewData(self.difCent.info)

    def updateParams(self):
        info = self.difCent.info
        if 'orientation_model' in info:
            self.orientationModel = info['orientation_model']

    def refreshUI(self):
        # Refresh all tabs
        self.updated = False
        self.function = None
        self.setCursor(Qt.ArrowCursor)
        self.topDifTab.clearSettings()
        self.bottomDifTab.clearSettings()
        if self.off_mer is not None:
            self.offMerTab.clearSettings()
        for b in self.checkableButtons:
            b.setChecked(False)
        self.updateUI()

    def updateUI(self):
        # Update UI on current tab
        if self.difCent is None:
            return
        self.updateUIonly = True
        selected_tab = self.tabWidget.currentIndex()
        if selected_tab == 0:
            self.updateImageTab()
        if selected_tab == 1:
            self.topDifTab.updateUI()
        if selected_tab == 2:
            self.bottomDifTab.updateUI()
        if selected_tab == 3:
            self.offMerTab.updateUI()
        self.updateUIonly = False

    def initMinMaxIntensities(self, img):
        # initial min max intensity ranges, value, and step
        self.updateUIonly = True
        self.maxInt.setMinimum(img.min())
        self.maxInt.setMaximum(img.max())
        self.minInt.setMinimum(img.min())
        self.minInt.setMaximum(img.max())
        step = (img.max()-img.min())*0.05
        self.maxInt.setSingleStep(step)
        self.minInt.setSingleStep(step)
        if self.firstImg:
            self.minInt.setValue(img.min())
            self.maxInt.setValue(img.max()*.05)
            self.firstImg = False
        self.minIntLabel.setText("Min intensity ("+str(img.min())+")")
        self.maxIntLabel.setText("Max intensity (" + str(img.max()) + ")")
        self.updateUIonly = False

    def updateImageTab(self):
        """
        Update image tab, draw image, and update widgets
        """
        if not self.updated:
            img = copy.copy(self.difCent.getRotatedImage())
            self.rotatedImg = copy.copy(self.difCent.getRotatedImage())
            #img = getBGR(get8bitImage(img, min=self.minInt.value(), max=self.maxInt.value()))
            ax = self.displayImgAxes
            ax.cla()
            # cv2.circle(img, tuple(self.difCent.info['center']), 2, (255,255,0), thickness = 2)
            if self.logScaleIntChkBx.isChecked():
                ax.imshow(img, cmap='gray', norm=LogNorm(vmin=max(1, self.minInt.value()), vmax=self.maxInt.value()))
            else:
                ax.imshow(img, cmap='gray', norm=Normalize(vmin=self.minInt.value(), vmax=self.maxInt.value()))
            ax.set_facecolor('black')

            self.orientationCmbBx.setCurrentIndex(0 if self.orientationModel is None else self.orientationModel)
            if self.rotation90ChkBx.isEnabled():
                self.rotation90ChkBx.setChecked('90rotation' in self.difCent.info and self.difCent.info['90rotation'])

            if self.areaChkBx.isChecked() and 'int_area' in self.difCent.info:
                # Draw meridian lines
                int_area = self.difCent.info['int_area']
                ax.plot((int_area[0], int_area[0]), (0, img.shape[0]), color='b')
                ax.plot((int_area[1], int_area[1]), (0, img.shape[0]), color='b')
                ax.plot((int_area[0], int_area[1]), (self.difCent.info['center'][1], self.difCent.info['center'][1]), color = 'y')

            if self.offMerChkBx.isChecked():
                # Draw off-meridian lines
                for i in range(1,3):
                    name = "x"+str(i)
                    x = self.difCent.info[name]
                    ax.axvline(x, color = "g")
                for i in range(3,5):
                    name = "x"+str(i)
                    x = self.difCent.info[name]
                    ax.axvline(x, color = "r")

            # Set zoom area
            if self.img_zoom is not None and len(self.img_zoom) == 2:
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
            else:
                ax.set_xlim((0, img.shape[1]))
                ax.set_ylim((0, img.shape[0]))

            self.displayImgFigure.tight_layout()
            self.displayImgCanvas.draw()

            if len(self.groupList[self.currentGroup]) < 5:
                self.imageNames.setText("\n".join(self.groupList[self.currentGroup]))
            else:
                self.imageNames.setText("\n".join(self.groupList[self.currentGroup][0:3])+'\n...\n'+self.groupList[self.currentGroup][-1])

            self.updated = True

        if not self.plots_updated:
            self.topDifTab.drawPlot(self.topDifFigure, self.topDifCanvas, self.topDifAxes, self.difCent.info)
            self.bottomDifTab.drawPlot(self.bottomDifFigure, self.bottomDifCanvas, self.bottomDifAxes, self.difCent.info)
            self.plots_updated = True

    def redrawPlots(self):
        # set updated status to false to trigger program to refresh plots
        self.plots_updated = False

    def closeEvent(self, ev):
        # Trigger when window is closed.
        self.mainWindow.childWindowClosed(self)

class DiffractionCentroidStartWindow(QMainWindow):
    """
    A Class for startup window
    """
    def __init__(self):
        QWidget.__init__(self)
        self.windowList = []
        self.groupList = []
        self.imgList = []
        self.selectedImages = []
        self.dir_path = ""
        self.setWindowTitle("Muscle X Diffraction Centroids v."+musclex.__version__)
        self.fixRanges = []
        self.fixRangeNames = []
        self.initUI()
        self.setConnections()

    def initUI(self):
        """
        Initial all UI in window
        """
        self.centralWidget = QWidget(self)
        self.mainLayout = QGridLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        ### Folder selection tab (Auto-grouping)
        self.folderTab = QWidget()
        self.folderTab.setContentsMargins(0, 0, 0, 0)
        self.folderTabLayout = QGridLayout(self.folderTab)
        self.selectedFolderTextField = QLineEdit("") # Jiranun :: test only
        self.selectedFolderTextField.setEnabled(False)
        # self.selectedFolderTextField.setDragEnabled(True)
        self.browseFolderButton = QPushButton("Browse")
        self.folderDetails = QLabel("")
        self.groupSpnBx = QSpinBox()
        self.groupSpnBx.setMinimum(1)
        self.groupSpnBx.setMaximum(500)
        self.groupSpnBx.setValue(1)
        self.startGroup = QComboBox()
        self.startGroup.setFixedWidth(500)

        # self.maxPeaksChkBx = QCheckBox("Maximum number of peaks (Auto-detection) : ")
        # self.maxPeaksSpnBx = QSpinBox()
        # self.maxPeaksSpnBx.setMinimum(1)
        # self.maxPeaksSpnBx.setValue(2)
        # self.maxPeaksSpnBx.setEnabled(False)
        # self.maxPeaksSpnBx.setSuffix(" peak(s)")
        self.startButton = QPushButton("START")
        self.startButton.setFixedSize(100, 40)

        self.folderTabLayout.addWidget(self.selectedFolderTextField, 0, 0, 1, 3)
        self.folderTabLayout.addWidget(self.browseFolderButton, 0, 3, 1, 1)
        self.folderTabLayout.addWidget(self.folderDetails, 1, 0, 1, 4)
        self.folderTabLayout.addWidget(QLabel("Number of frames to average : "), 2, 0, 1, 2)
        self.folderTabLayout.addWidget(self.groupSpnBx, 2, 2, 1, 1)
        self.folderTabLayout.addWidget(QLabel("image(s)"), 2, 3, 1, 1)
        self.folderTabLayout.addWidget(QLabel("Start Group : "), 3, 0, 1, 2)
        self.folderTabLayout.addWidget(self.startGroup, 3, 2, 1, 2)
        self.folderTabLayout.setAlignment(self.startButton, Qt.AlignCenter)


        #### File selection tab ####
        self.filesTab = QWidget()
        self.filesTab.setContentsMargins(0, 0, 0, 0)
        self.filesTabLayout = QVBoxLayout(self.filesTab)
        self.selectFilesButton = QPushButton("Browse File(s)")
        self.selectFilesButton.setFixedSize(300, 50)
        self.numberOfImages = QLabel("")
        self.imageNames = QTextEdit()
        self.imageNames.setReadOnly(True)
        self.filesTabLayout.addWidget(self.selectFilesButton)
        self.filesTabLayout.addWidget(self.numberOfImages)
        self.filesTabLayout.addWidget(QLabel("Selected file(s) : "))
        self.filesTabLayout.addWidget(self.imageNames)
        self.filesTabLayout.setAlignment(self.selectFilesButton, Qt.AlignCenter)

        self.tabWidget = QTabWidget()
        self.tabWidget.setTabPosition(QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(False)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 40px; width: 300px; }")

        ### Fixed peak ranges option ###
        self.fixRangesGroup = QGroupBox("Meridian Peak Ranges")
        self.fixRangesGroup.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Maximum)
        # self.fixRangesGroup.setCheckable(True)
        # self.fixRangesGroup.setChecked(False)
        self.fixRangesLayout = QGridLayout()
        self.fixRangesGroup.setLayout(self.fixRangesLayout)
        self.addRangeButton = QPushButton("Add")
        self.removeRangeButton = QPushButton("Remove")
        self.peakName = QLineEdit("")
        self.peakName.setFixedWidth(100)
        self.startRange = QSpinBox()
        self.startRange.setMinimum(0)
        self.startRange.setMaximum(2000)
        self.startRange.setKeyboardTracking(False)
        self.endRange = QSpinBox()
        self.endRange.setMinimum(0)
        self.endRange.setMaximum(2000)
        self.endRange.setKeyboardTracking(True)
        self.fixRangesTable = QTableWidget()
        self.fixRangesTable.setColumnCount(3)
        self.fixRangesTable.setHorizontalHeaderLabels(['Name','Start', 'End'])
        self.fixRangesTable.setEditTriggers(QAbstractItemView.NoEditTriggers)
        self.fixRangesLayout.addWidget(QLabel("Name : "), 0, 0, 1, 1)
        self.fixRangesLayout.addWidget(QLabel("Start : "), 0, 1, 1, 1)
        self.fixRangesLayout.addWidget(QLabel("End : "), 0, 2, 1, 1)
        self.fixRangesLayout.addWidget(self.peakName, 1, 0, 1, 1)
        self.fixRangesLayout.addWidget(self.startRange, 1, 1, 1, 1)
        self.fixRangesLayout.addWidget(self.endRange, 1, 2, 1, 1)
        self.fixRangesLayout.addWidget(self.addRangeButton, 2, 0, 1, 3)
        self.fixRangesLayout.addWidget(self.removeRangeButton, 3, 0, 1, 3)
        self.fixRangesLayout.addWidget(self.fixRangesTable, 0, 3, 4, 3)

        self.startButton = QPushButton("START")
        self.startButton.setFixedSize(100, 40)

        ### Off-Meridian option ###
        self.offMeridianGrp = QGroupBox("Off Meridian")
        self.offMeridianGrp.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Maximum)
        self.offMeridianGrp.setCheckable(True)
        self.offMeridianGrp.setChecked(False)
        self.offMerLayout = QGridLayout(self.offMeridianGrp)
        self.x1SpnBx = QSpinBox()
        self.x1SpnBx.setRange(0, 2000)
        self.x2SpnBx = QSpinBox()
        self.x2SpnBx.setRange(0, 2000)
        self.x3SpnBx = QSpinBox()
        self.x3SpnBx.setRange(0, 2000)
        self.x4SpnBx = QSpinBox()
        self.x4SpnBx.setRange(0, 2000)
        self.start51SpnBx = QSpinBox()
        self.start51SpnBx.setRange(0, 2000)
        self.end51SpnBx = QSpinBox()
        self.end51SpnBx.setRange(0, 2000)
        self.start59SpnBx = QSpinBox()
        self.start59SpnBx.setRange(0, 2000)
        self.end59SpnBx = QSpinBox()
        self.end59SpnBx.setRange(0, 2000)
        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Expanding)
        separator.setLineWidth(1)

        self.offMerLayout.addWidget(QLabel("Left"), 0, 0, 1, 2, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("Right"), 0, 2, 1, 2, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("x1"), 1, 0, 1, 1, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("x2"), 1, 1, 1, 1, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("x3"), 1, 2, 1, 1, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("x4"), 1, 3, 1, 1, Qt.AlignCenter)
        self.offMerLayout.addWidget(self.x1SpnBx, 2, 0, 1, 1)
        self.offMerLayout.addWidget(self.x2SpnBx, 2, 1, 1, 1)
        self.offMerLayout.addWidget(self.x3SpnBx, 2, 2, 1, 1)
        self.offMerLayout.addWidget(self.x4SpnBx, 2, 3, 1, 1)
        self.offMerLayout.addWidget(separator, 3, 0, 1, 4)
        self.offMerLayout.addWidget(QLabel("59 Range"), 4, 0, 1, 2, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("51 Range"), 4, 2, 1, 2, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("start"), 5, 0, 1, 1, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("end"), 5, 1, 1, 1, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("start"), 5, 2, 1, 1, Qt.AlignCenter)
        self.offMerLayout.addWidget(QLabel("end"), 5, 3, 1, 1, Qt.AlignCenter)
        self.offMerLayout.addWidget(self.start59SpnBx, 6, 0, 1, 1)
        self.offMerLayout.addWidget(self.end59SpnBx, 6, 1, 1, 1)
        self.offMerLayout.addWidget(self.start51SpnBx, 6, 2, 1, 1)
        self.offMerLayout.addWidget(self.end51SpnBx, 6, 3, 1, 1)

        self.mainLayout.addWidget(self.tabWidget, 0, 0, 1, 2)
        self.mainLayout.addWidget(self.fixRangesGroup, 1, 0, 1, 1)
        self.mainLayout.addWidget(self.offMeridianGrp, 1, 1, 1, 1)
        self.mainLayout.addWidget(self.startButton, 2, 0, 1, 2)
        self.mainLayout.setColumnStretch(0, 2)
        self.mainLayout.setColumnStretch(1, 1)

        self.mainLayout.setAlignment(self.startButton, Qt.AlignCenter)
        self.tabWidget.addTab(self.filesTab, "Select File(s)")
        self.tabWidget.addTab(self.folderTab, "Select a Folder (auto-grouping)")

        self.resize(1100,100)
        self.show()

    def setConnections(self):
        # Set all connections for interactive widget
        self.browseFolderButton.clicked.connect(self.browseFolder)
        self.groupSpnBx.valueChanged.connect(self.groupImages)
        # self.fixRangesGroup.clicked.connect(self.setNameFocus)
        self.addRangeButton.clicked.connect(self.addRangeClicked)
        self.startRange.valueChanged.connect(self.startRangeSet)
        # self.endRange.valueChanged.connect(self.addRangeClicked)
        self.removeRangeButton.clicked.connect(self.removeRangeClicked)
        self.startButton.clicked.connect(self.startProcess)
        self.selectFilesButton.clicked.connect(self.selectFiles)

    def removeRangeClicked(self):
        """
        Trigger when "Remove" is clicked
        """
        if len(self.fixRanges) > 0:
            ind = self.fixRangesTable.currentRow()
            if ind == -1:
                ind = len(self.fixRanges) - 1

            r = self.fixRanges[ind]
            self.fixRanges.remove(r)
            r = self.fixRangeNames[ind]
            self.fixRangeNames.remove(r)
            self.fixRangesTable.removeRow(ind)

    def setNameFocus(self):
        # Set window focus to peak name
        self.peakName.setFocus()
        self.peakName.selectAll()

    def startRangeSet(self):
        # move focus to end point when start point is set
        self.endRange.setFocus()
        self.endRange.selectAll()

    def addRangeClicked(self):
        """
        Trigger when "Add" clicked
        """
        name = self.peakName.text()
        start = self.startRange.value()
        end = self.endRange.value()
        self.addFixedRange(name, start, end)

    def addFixedRange(self, name, start, end):
        """
        Add fixed ranges
        :param name: fixed range name
        :param start: start point of range
        :param end: end point of range
        :return:
        """
        if name == "":
            errMsg = QMessageBox()
            errMsg.setText('Range name is missing')
            errMsg.setInformativeText('Please specify range name')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
        elif start >= end:
            errMsg = QMessageBox()
            errMsg.setText('Invalid Range')
            errMsg.setInformativeText('Start position must be less than End position.')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
        elif (start, end) in self.fixRanges:
            errMsg = QMessageBox()
            errMsg.setText('('+str(start)+', '+str(end)+') has been added already')
            errMsg.setInformativeText('Please select another range')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
        elif name in self.fixRangeNames:
            errMsg = QMessageBox()
            errMsg.setText( str(name)+' has been added already')
            errMsg.setInformativeText('Please select another name')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
        else:
            self.fixRanges.append((start, end))
            self.fixRangeNames.append(name)

            # Add range to table
            self.fixRangesTable.setRowCount(len(self.fixRanges))
            self.fixRangesTable.setItem(len(self.fixRanges) - 1, 0, QTableWidgetItem(name))
            self.fixRangesTable.setItem(len(self.fixRanges) - 1, 1, QTableWidgetItem(str(start)))
            self.fixRangesTable.setItem(len(self.fixRanges) - 1, 2, QTableWidgetItem(str(end)))

        self.setNameFocus()
        col_width = self.fixRangesTable.width()/3
        self.fixRangesTable.setColumnWidth(0, col_width)
        self.fixRangesTable.setColumnWidth(1, col_width)

    def removeAllFixedRange(self):
        # Remove all fixed ranges
        self.fixRanges = []
        self.fixRangeNames = []
        self.fixRangesTable.setRowCount(0)

    def selectFiles(self):
        """
        Popup multiple files selection dialog
        """
        fileList = getFiles()
        if len(fileList) < 1:
            return
        self.numberOfImages.setText("Number of file(s) : "+str(len(fileList)))
        self.selectedImages = []
        nImg = 1

        # Encapsulate and display selected files
        self.imageNames.setText("")
        self.selectedImages = []
        self.dir_path = ""
        for f in sorted(fileList):
            self.dir_path, filename = split(str(f))
            if isImg(filename):
                self.imageNames.append(str(nImg)+". "+filename)
                self.selectedImages.append(filename)
                nImg += 1
        self.initSettings(self.dir_path)

    def browseFolder(self):
        """
        Popup folder selection dialog
        """
        dir_path = QFileDialog.getExistingDirectory(self, "Select a Folder") #jiranun test
        if dir_path != "":
            dir_path = str(dir_path)
            self.selectedFolderTextField.setText(dir_path)
            self.preprocessFolder(dir_path)
            self.initSettings(dir_path)

    def loadSettings(self, dir_path):
        file = fullPath(fullPath(dir_path, "dc_cache"), "settings.cache")
        if exists(file):
            settings = pickle.load(open(file, "rb"))
            return settings
        return None

    def initSettings(self, dir_path):
        """
        load settings from cache if it exists
        :param dir_path: selected directory
        """
        self.removeAllFixedRange()
        self.offMeridianGrp.setChecked(False)
        settings = self.loadSettings(dir_path)
        if settings is not None:
            self.offMeridianGrp.setChecked("off_meridian" in settings)
            if "fix_ranges" in settings:
                fix_ranges = settings["fix_ranges"]
                for (name, pos) in fix_ranges:
                    self.addFixedRange(name, pos[0], pos[1])
            if "off_meridian" in settings:
                off_mer = settings["off_meridian"]
                self.x1SpnBx.setValue(off_mer["x1"])
                self.x2SpnBx.setValue(off_mer["x2"])
                self.x3SpnBx.setValue(off_mer["x3"])
                self.x4SpnBx.setValue(off_mer["x4"])
                self.start51SpnBx.setValue(off_mer["s51"])
                self.end51SpnBx.setValue(off_mer["e51"])
                self.start59SpnBx.setValue(off_mer["s59"])
                self.end59SpnBx.setValue(off_mer["e59"])

    def groupImages(self):
        """
        Do auto-grouping all images in the selected folder
        """
        self.groupList = []
        self.startGroup.clear()
        groupNum = 1
        for i in range(0, len(self.imgList), self.groupSpnBx.value()):
            self.groupList.append(self.imgList[i:i + self.groupSpnBx.value()])
            if self.groupSpnBx.value() > 1:
                if i + self.groupSpnBx.value() - 1 < len(self.imgList):
                    self.startGroup.addItem(str(groupNum) + ' - ' + self.imgList[i] + ', ..., '+
                                            self.imgList[i + self.groupSpnBx.value()-1])
                else:
                    self.startGroup.addItem(str(groupNum) + ' - ' + self.imgList[i] + ', ..., '+
                                            self.imgList[-1])
            else:
                self.startGroup.addItem(str(groupNum) + ' - ' + self.imgList[i])
            groupNum += 1
        # self.startGroup.setCurrentIndex(0) #jiranun test

    def preprocessFolder(self, dir_path):
        """
        Pre process selected folder by get all images in the folder and group them
        :param dir_path: selected folder path
        :return: -
        """
        fileList = os.listdir(dir_path)
        self.imgList = []

        for f in fileList:
            full_file_name = fullPath(dir_path, f)
            if isImg(full_file_name):
                self.imgList.append(f)
        self.imgList.sort()
        self.groupImages()
        self.folderDetails.setText("This folder contains "+str(len(self.imgList))+" image(s)")

    def startProcess(self):
        """
        Trigger when START button is pressed
        """
        if self.tabWidget.currentIndex() == 1:
            if len(self.imgList) == 0:
                errMsg = QMessageBox()
                if self.selectedFolderTextField.text() == "":
                    errMsg.setText('No folder selected')
                    errMsg.setInformativeText('Please select a folder to process')
                else:
                    errMsg.setText('There are no image in this folder')
                    errMsg.setInformativeText('Please select another folder to process')

                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                self.browseFolder()
                return
            else:
                dir_path = str(self.selectedFolderTextField.text())
                settings = {}
                settings['group'] = self.groupSpnBx.value()
                settings['start_group'] = self.startGroup.currentIndex()
                grpList = copy.copy(self.groupList)
        else:
            if len(self.selectedImages) == 0:
                errMsg = QMessageBox()
                errMsg.setText('No image selected')
                errMsg.setInformativeText('Please select files to process')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                return
            else:
                dir_path = self.dir_path
                settings = {}
                settings['group'] = 0
                grpList = [self.selectedImages]

        # if self.fixRangesGroup.isChecked():
        fix_ranges = sorted([(str(self.fixRangeNames[i]),self.fixRanges[i]) for i in range(len(self.fixRanges))], key = lambda nr : nr[1][0])

        if len(fix_ranges) == 0:
            errMsg = QMessageBox()
            errMsg.setText('Meridian peak ranges is empty')
            errMsg.setInformativeText('Please specify at least 1 fixed range.')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
            return
        settings['fix_ranges'] = fix_ranges

        if self.offMeridianGrp.isChecked():
            x1 = self.x1SpnBx.value()
            x2 = self.x2SpnBx.value()
            x3 = self.x3SpnBx.value()
            x4 = self.x4SpnBx.value()
            s51 = self.start51SpnBx.value()
            e51 = self.end51SpnBx.value()
            s59 = self.start59SpnBx.value()
            e59 = self.end59SpnBx.value()
            if x1 == 0 or x2==0 or x3==0 or x4==0 or x1>=x2 or x3>=x4:
                errMsg = QMessageBox()
                errMsg.setText('Off Meridian information is incorrect')
                errMsg.setInformativeText('Please specify all x1,x2,x3, and x4 with x1 < x2 and x3 < x4')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                return
            if s51 == 0 or e51==0 or s59==0 or e59==0:
                errMsg = QMessageBox()
                errMsg.setText('Off Meridian information is missing')
                errMsg.setInformativeText('Please specify both 51 and 59 ranges')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                return
            if s51 > e51 or s59 > e59 or s59 > s51:
                errMsg = QMessageBox()
                errMsg.setText('Off Meridian information is incorrect')
                errMsg.setInformativeText('Please specify correct 51 and 59 ranges. Start point should be smaller than end point.')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                return

            off_mer = {
                "x1" : x1,
                "x2" : x2,
                "x3": x3,
                "x4": x4,
                "s51" : s51,
                "e51" : e51,
                "s59" : s59,
                "e59" : e59
            }
            settings["off_meridian"] = off_mer

        cache_path = fullPath(dir_path, 'dc_cache')
        self.delete_old_results(dir_path, cache_path, settings)
        createFolder(cache_path)
        pickle.dump(settings, open(fullPath(cache_path,"settings.cache"), "wb"))


        # Create DiffractionCentroidProcessWindow object, execute it with all settings
        newWindow = DiffractionCentroidProcessWindow(self, dir_path, grpList, settings)

        # Put DiffractionCentroidProcessWindow object in the list to prevent python to destroy
        self.windowList.append(newWindow)

    def delete_old_results(self, dir_path, cache_path, new_settings):
        old_settings = self.loadSettings(dir_path)
        if old_settings != new_settings and exists(cache_path):
            shutil.rmtree(cache_path)

    def childWindowClosed(self, childwin):
        # Remove DiffractionCentroidProcessWindow
        self.windowList.remove(childwin)

if __name__ == "__main__":
    app = QApplication(sys.argv)
    myapp = DiffractionCentroidStartWindow()
    sys.exit(app.exec_())
