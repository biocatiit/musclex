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

from PyQt4 import QtCore, QtGui
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.patches as patches
from ..utils.file_manager import fullPath, getImgFiles, getStyleSheet, createFolder
from ..modules.ProjectionProcessor import ProjectionProcessor
from ..ui.ProjectionBoxTab import ProjectionBoxTab
from ..utils.image_processor import getBGR, get8bitImage, getNewZoom
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from ..CalibrationSettings import CalibrationSettings
from ..csv_manager import PT_CVSManager
import sys
import traceback
import musclex
import copy
from os.path import exists
import pickle

class ProjectionTracesGUI(QtGui.QMainWindow):
    """
    This class is for Projection Traces GUI Object
    """
    def __init__(self):
        QtGui.QWidget.__init__(self)
        self.setWindowTitle("Projection Traces v." + musclex.__version__)
        self.current_file = 0
        self.dir_path = ""
        self.calSettings = None
        self.update_plot = {'img':True}
        self.imgList = []
        self.projProc = None
        self.syncUI = False
        self.csvManager = None
        self.img_zoom = None
        self.function = None
        self.allboxes = {}
        self.boxtypes = {}
        self.peaks = {}
        self.setStyleSheet(getStyleSheet())
        self.checkableButtons = []
        self.initUI()
        self.setConnections()

    def initUI(self):
        """
        Initial all GUI
        """
        #### Image Tab ####
        self.centralWidget = QtGui.QWidget(self)
        self.mainLayout = QtGui.QVBoxLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        self.tabWidget = QtGui.QTabWidget()
        self.tabWidget.setTabPosition(QtGui.QTabWidget.North)
        self.tabWidget.setDocumentMode(False)
        self.tabWidget.setTabsClosable(True)
        self.tabWidget.setStyleSheet("QTabBar::tab { height: 20px; width: 200px; }")

        self.imageTab = QtGui.QWidget()
        self.imageTabLayer = QtGui.QHBoxLayout(self.imageTab)
        self.displayImgFigure = plt.figure(facecolor='#606060')
        self.imageVLayout = QtGui.QVBoxLayout()
        self.displayImgCanvas = FigureCanvas(self.displayImgFigure)
        self.imageVLayout.addWidget(self.displayImgCanvas)

        self.imageLeftFrame = QtGui.QFrame()
        self.imageLeftFrame.setFixedWidth(250)
        self.leftFrameLayout = QtGui.QVBoxLayout(self.imageLeftFrame)

        # Image selection
        self.selectImageGrp = QtGui.QGroupBox("1. Select an image")
        self.selectImageLayout = QtGui.QVBoxLayout(self.selectImageGrp)
        self.browseImageButton = QtGui.QPushButton("Browse")
        self.selectImageLayout.addWidget(self.browseImageButton)

        # Pattern Properties
        self.propGrp = QtGui.QGroupBox("2. Pattern Settings (Optional)")
        self.propGrp.setEnabled(False)
        self.propLayout = QtGui.QGridLayout(self.propGrp)
        self.calibrateButton = QtGui.QPushButton("Calibration Settings")
        # self.quadFoldButton = QtGui.QPushButton("Quadrant Folding")
        # self.setCenterButton = QtGui.QPushButton("Set Rotation and Center")
        # self.setCenterButton.setCheckable(True)
        # self.checkableButtons.append(self.setCenterButton)
        # self.setRotationButton = QtGui.QPushButton("Set Rotation Angle")
        # self.setRotationButton.setCheckable(True)
        # self.checkableButtons.append(self.setRotationButton)
        # self.lockAngleChkBx = QtGui.QCheckBox("Lock Angle")
        # self.lockAngleSpnBx = QtGui.QSpinBox()
        # self.lockAngleSpnBx.setEnabled(False)
        # self.lockAngleSpnBx.setRange(-180, 180)
        self.propLayout.addWidget(self.calibrateButton, 0, 0, 1, 1)
        # self.propLayout.addWidget(self.quadFoldButton, 1, 0, 1, 2)
        # self.propLayout.addWidget(self.setCenterButton, 2, 0, 1, 2)
        # self.propLayout.addWidget(self.setRotationButton, 3, 0, 1, 2)
        # self.propLayout.addWidget(self.lockAngleChkBx, 4, 0, 1, 1)
        # self.propLayout.addWidget(self.lockAngleSpnBx, 4, 1, 1, 1)

        # Box selection
        self.boxGrp = QtGui.QGroupBox("3. Add boxes")
        self.boxGrp.setEnabled(False)
        self.boxesLayout = QtGui.QVBoxLayout(self.boxGrp)
        self.addBoxButton = QtGui.QPushButton("Add Boxes")
        self.addBoxButton.setCheckable(True)
        self.clearBoxButton = QtGui.QPushButton('Clear All Boxes')
        self.checkableButtons.append(self.addBoxButton)
        self.boxesLayout.addWidget(self.addBoxButton)
        self.boxesLayout.addWidget(self.clearBoxButton)

        # Peaks Selection
        self.selectPeaksGrp = QtGui.QGroupBox("4. Peaks")
        self.selectPeaksGrp.setEnabled(False)
        self.selectPeaksLayout = QtGui.QVBoxLayout(self.selectPeaksGrp)
        self.selectPeaksButton = QtGui.QPushButton("Select Approximate Peak Locations")
        self.selectPeaksButton.setCheckable(True)
        self.checkableButtons.append(self.selectPeaksButton)
        self.selectPeaksLayout.addWidget(self.selectPeaksButton)

        self.leftFrameLayout.addWidget(self.selectImageGrp)
        self.leftFrameLayout.addSpacing(10)
        self.leftFrameLayout.addWidget(self.propGrp)
        self.leftFrameLayout.addSpacing(10)
        self.leftFrameLayout.addWidget(self.boxGrp)
        self.leftFrameLayout.addSpacing(10)
        self.leftFrameLayout.addWidget(self.selectPeaksGrp)
        self.leftFrameLayout.addStretch()

        self.imageRightFrame = QtGui.QFrame()
        self.imageRightFrame.setFixedWidth(250)
        self.rightFrameLayout = QtGui.QVBoxLayout(self.imageRightFrame)

        # Display Options
        self.dispOptGrp = QtGui.QGroupBox("Display Options")
        self.dispOptLayout = QtGui.QGridLayout(self.dispOptGrp)

        self.boxesChkBx = QtGui.QCheckBox("Boxes")
        self.boxesChkBx.setChecked(True)
        self.peaksChkBx = QtGui.QCheckBox("Peaks")
        self.peaksChkBx.setChecked(True)
        self.imgZoomInB = QtGui.QPushButton("Zoom In")
        self.imgZoomInB.setCheckable(True)
        self.imgZoomOutB = QtGui.QPushButton("Full")
        self.checkableButtons.append(self.imgZoomInB)

        self.minIntLabel = QtGui.QLabel("Min Intensity")
        self.minIntSpnBx = QtGui.QDoubleSpinBox()
        self.minIntSpnBx.setKeyboardTracking(False)
        self.minIntSpnBx.setDecimals(2)

        self.maxIntLabel = QtGui.QLabel("Max Intensity")
        self.maxIntSpnBx = QtGui.QDoubleSpinBox()
        self.maxIntSpnBx.setValue(0)
        self.maxIntSpnBx.setDecimals(2)
        self.maxIntSpnBx.setKeyboardTracking(False)

        self.dispOptLayout.addWidget(self.boxesChkBx, 0, 0, 1, 2)
        self.dispOptLayout.addWidget(self.peaksChkBx, 1, 0, 1, 2)
        self.dispOptLayout.addWidget(self.imgZoomInB, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.minIntLabel, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.minIntSpnBx, 3, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntLabel, 4, 0, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntSpnBx, 4, 1, 1, 1)

        # Process Folder Button
        pfss = "QPushButton { color: #ededed; background-color: #af6207}"
        self.processFolderButton = QtGui.QPushButton("Process Current Folder")
        self.processFolderButton.setStyleSheet(pfss)

        # Export 1-D Projections
        self.exportChkBx = QtGui.QCheckBox("Export all 1-D Projections")
        self.exportChkBx.setChecked(True)

        # next previos buttons
        self.nextButton = QtGui.QPushButton(">>>")
        self.prevButton = QtGui.QPushButton("<<<")
        self.bottomLayout = QtGui.QGridLayout()
        self.bottomLayout.addWidget(self.exportChkBx, 0, 0, 1, 2)
        self.bottomLayout.addWidget(self.processFolderButton, 1, 0, 1, 2)
        self.bottomLayout.addWidget(self.prevButton, 2, 0, 1, 1)
        self.bottomLayout.addWidget(self.nextButton, 2, 1, 1, 1)

        self.rightFrameLayout.addWidget(self.dispOptGrp)
        self.rightFrameLayout.addStretch()
        self.rightFrameLayout.addLayout(self.bottomLayout)

        self.imageTabLayer.addWidget(self.imageLeftFrame)
        self.imageTabLayer.addWidget(self.displayImgCanvas)
        self.imageTabLayer.addWidget(self.imageRightFrame)

        self.tabWidget.addTab(self.imageTab, "Image")
        self.tabWidget.tabBar().setTabButton(0, QtGui.QTabBar.LeftSide, None)
        self.tabWidget.tabBar().setTabButton(0, QtGui.QTabBar.RightSide, None)

        #
        ### Status Bar ###
        #
        self.statusBar = QtGui.QStatusBar()
        self.left_status = QtGui.QLabel()
        self.right_status = QtGui.QLabel()
        self.pixel_detail = QtGui.QLabel()
        self.progressBar = QtGui.QProgressBar()
        self.progressBar.setFixedWidth(300)
        self.progressBar.setTextVisible(True)
        self.progressBar.setVisible(False)
        self.statusBar.addWidget(self.left_status)
        self.statusBar.addPermanentWidget(self.pixel_detail)
        self.statusBar.addPermanentWidget(self.right_status)
        self.statusBar.addPermanentWidget(self.progressBar)

        self.mainLayout.addWidget(self.tabWidget)
        self.mainLayout.addWidget(self.statusBar)
        self.show()
        self.resize(1300, 700)

    def setConnections(self):
        """
        Set connection for interactive widgets
        """
        self.tabWidget.currentChanged.connect(self.updateUI)
        self.tabWidget.tabCloseRequested.connect(self.removeTab)

        # Image seletion
        self.browseImageButton.clicked.connect(self.browseFile)

        # # Pattern Properties
        # self.lockAngleChkBx.stateChanged.connect(self.lockAngle)
        # self.quadFoldButton.clicked.connect(self.launchQF)
        # self.setCenterButton.clicked.connect(self.setAngleAndCenterClicked)
        # self.setRotationButton.clicked.connect(self.setAngleClicked)
        self.calibrateButton.clicked.connect(self.calibrationClicked)

        # Display options
        self.maxIntSpnBx.valueChanged.connect(self.updateImage)
        self.minIntSpnBx.valueChanged.connect(self.updateImage)
        self.boxesChkBx.stateChanged.connect(self.updateImage)
        self.peaksChkBx.stateChanged.connect(self.updateImage)
        self.imgZoomInB.clicked.connect(self.imgZoomIn)
        self.imgZoomOutB.clicked.connect(self.imgZoomOut)

        # select boxes
        self.addBoxButton.clicked.connect(self.addABox)
        self.clearBoxButton.clicked.connect(self.clearBoxes)

        # select peaks
        self.selectPeaksButton.clicked.connect(self.addPeaks)

        # Export 1-D Projections checkbox
        self.exportChkBx.stateChanged.connect(self.exportHistograms)

        # Process Folder button
        self.processFolderButton.clicked.connect(self.processFolder)

        self.prevButton.clicked.connect(self.prevClicked)
        self.nextButton.clicked.connect(self.nextClicked)

        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)
        self.displayImgFigure.canvas.mpl_connect('button_release_event', self.imgReleased)
        self.displayImgFigure.canvas.mpl_connect('figure_leave_event', self.leaveImage)
        self.displayImgFigure.canvas.mpl_connect('scroll_event', self.imgScrolled)

    def calibrationClicked(self):
        """
        Triggered when calibration settings button pressed
        """
        success = self.launchCalibrationSettings(force=True)
        if self.projProc is not None and success:
            self.processImage()


    def launchCalibrationSettings(self, force=False):
        """
        Popup Calibration Settings window, if there's calibration settings in cache or calibration.tif in the folder
        :param force: force to popup the window
        :return: True if calibration set, False otherwise
        """
        settingDialog = CalibrationSettings(self.dir_path)
        self.calSettings = None
        cal_setting = settingDialog.calSettings
        if cal_setting is not None or force:
            result = settingDialog.exec_()
            if result == 1:
                self.calSettings = settingDialog.getValues()
                return True
        return False

    def clearImage(self):
        ax = self.displayImgFigure.add_subplot(111)
        del ax.lines
        ax.lines = []
        del ax.patches
        ax.patches = []
        del ax.texts
        ax.texts = []
        self.displayImgCanvas.draw_idle()

    def imgZoomIn(self):
        """
        Triggered when Zoom in image is pressed
        """
        if self.imgZoomInB.isChecked():
            self.setLeftStatus("Please select zoom-in area by clicking 2 points to make a rectangle (ESC to cancel)")
            self.clearImage()
            self.function = ["im_zoomin"]
        else:
            self.function = None

    def imgZoomOut(self):
        """
        Triggered when Zoom out image is pressed
        """
        self.imgZoomInB.setChecked(False)
        self.img_zoom = None
        self.updateImage()

    def updateImage(self):
        """
        Refresh image tab
        """
        self.update_plot['img'] = True
        self.updateUI()

    def addPeakstoBox(self, num, peaks):
        self.peaks[num] = peaks
        self.processImage()

    def addPeaks(self):
        """
        Triggered when Add a Box pressed
        :return:
        """
        if self.projProc is None:
            self.selectPeaksButton.setChecked(False)
            return

        if self.selectPeaksButton.isChecked():
            # Start function
            if self.function is None:
                self.selectPeaksButton.setText("Done")
                self.setLeftStatus(
                    "Add left and right peaks simultaneously to boxes by clicking inside a box (ESC to cancel)")
                peaks = {}
                for num in self.allboxes.keys():
                    peaks[num] = []
                self.function = ['peaks', peaks]
                ax = self.displayImgFigure.add_subplot(111)
                del ax.lines
                ax.lines = []
                self.displayImgCanvas.draw_idle()
            else:
                # ignore if there're other function being active
                self.selectPeaksButton.setChecked(False)
                return
        else:
            if self.function is not None and len(self.function) == 2:
                # When Done clicked
                self.peaks = self.function[1]
            self.processImage()

    def processFolder(self):
        self.numberOfFiles = len(self.imgList)

        errMsg = QtGui.QMessageBox()
        errMsg.setText('Process Current Folder')
        text = 'The current folder will be processed using current settings. Make sure to adjust them before processing the folder. \n\n'
        settings = self.getSettings()

        text += "\nCurrent Settings"
        for bn in self.allboxes.keys():
            text += "\n\n  - Box "+str(bn)+" : " + str(self.allboxes[bn])
            text += "\n     - Peaks : "
            if self.peaks.has_key(bn):
                text += str(self.peaks[bn])
            else:
                text += "-"
        if 'lambda_sdd' in settings.keys():
            text += "\n  - Lambda Sdd : " + str(settings["lambda_sdd"])

        text += '\n\nAre you sure you want to process ' + str(
            self.numberOfFiles) + ' image(s) in this Folder? \nThis might take a long time.'
        errMsg.setInformativeText(text)
        errMsg.setStandardButtons(QtGui.QMessageBox.Yes | QtGui.QMessageBox.Cancel)
        errMsg.setIcon(QtGui.QMessageBox.Warning)
        ret = errMsg.exec_()

        # If "yes" is pressed
        if ret == QtGui.QMessageBox.Yes:
            self.progressBar.setVisible(True)
            for i in range(self.numberOfFiles):
                self.progressBar.setValue(100. / self.numberOfFiles * i)
                QtGui.QApplication.processEvents()
                self.nextClicked()
            self.progressBar.setVisible(False)

    def clearBoxes(self):
        self.allboxes = {}
        self.boxtypes = {}
        self.peaks = {}
        self.removeAllTabs()
        self.processImage()

    def addABox(self):
        """
        Triggered when Add a Box pressed
        :return:
        """
        if self.projProc is None:
            self.addBoxButton.setChecked(False)
            return

        if self.addBoxButton.isChecked():
            if self.function is None:
                # Start function
                self.addBoxButton.setText("Done")
                self.setLeftStatus("Add a box to the image by drawing a rectangles (ESC to cancel)")
                self.function = ['box']
                ax = self.displayImgFigure.add_subplot(111)
                ax.lines = []
                self.displayImgCanvas.draw_idle()
            else:
                self.addBoxButton.setChecked(False)
                self.function = None
                return

    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard
        """
        key = event.key()

        if key == QtCore.Qt.Key_Right:
            self.nextClicked()
        elif key == QtCore.Qt.Key_Left:
            self.prevClicked()
        elif key == QtCore.Qt.Key_Escape:
            self.resetUI()
        elif key == QtCore.Qt.Key_D:
            self.tabWidget.setCurrentIndex((self.tabWidget.currentIndex() + 1) % self.tabWidget.count())
        elif key == QtCore.Qt.Key_A:
            self.tabWidget.setCurrentIndex((self.tabWidget.currentIndex() - 1) % self.tabWidget.count())
        elif key == QtCore.Qt.Key_S:
            self.maxIntSpnBx.stepDown()
        elif key == QtCore.Qt.Key_W:
            self.maxIntSpnBx.stepUp()
        elif key == QtCore.Qt.Key_Q:
            self.close()

    def mousePressEvent(self, event):
        # Clear focus when mouse pressed
        focused_widget = QtGui.QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

    def prevClicked(self):
        """
        Going to the previous image
        """
        self.current_file = (self.current_file - 1) % len(self.imgList)
        self.onImageChanged()

    def nextClicked(self):
        """
        Going to the next image
        """
        self.current_file = (self.current_file + 1) % len(self.imgList)
        self.onImageChanged()

    def removeTab(self, index):
        if index != 0:
            widget = self.tabWidget.widget(index)
            if widget is not None:
                num = widget.num
                del self.allboxes[num]
                del self.boxtypes[num]
                if self.peaks.has_key(num):
                    del self.peaks[num]
                widget.deleteLater()
            self.processImage()
            self.tabWidget.removeTab(index)

    def removeAllTabs(self):
        """
        Remove old box tabs
        :return:
        """
        while self.tabWidget.count() > 1:
            self.tabWidget.removeTab(1)

    def addBoxTabs(self):
        """
        Add old box tabs
        :return:
        """
        self.removeAllTabs()

        for i in self.allboxes.keys():
            proj_tab = ProjectionBoxTab(self, i)
            self.tabWidget.addTab(proj_tab, "Box "+str(i))

    def imgClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if self.projProc is None:
            return

        x = event.xdata
        y = event.ydata

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.pixel_detail.setText("")
            ax = self.displayImgFigure.add_subplot(111)
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

        func = self.function

        # Provide different behavior depending on current active function
        if func is None:
            self.function = ["im_move", (x, y)]
        elif func[0] == "box":
            func.append((x, y))
            if len(func) == 3:
                # A box added
                points = self.function[1:]
                x1 = int(round(min(points[0][0], points[1][0])))
                y1 = int(round(min(points[0][1], points[1][1])))
                x2 = int(round(max(points[0][0], points[1][0])))
                y2 = int(round(max(points[0][1], points[1][1])))
                if len(self.allboxes) > 0:
                    num = max(self.allboxes.keys()) + 1
                else:
                    num = 1
                self.allboxes[num] = ((x1, x2), (y1, y2))
                w = abs(x1-x2)
                h = abs(y1-y2)
                self.boxtypes[num] = 'h' if w >= h else 'v'
                self.function = None
                self.addBoxTabs()
                self.processImage()
        elif func[0] == "peaks":
            peaks = func[1]
            if len(self.allboxes.keys()) > 0:
                ax = self.displayImgFigure.add_subplot(111)
                centerx = self.projProc.orig_img.shape[1] / 2. - 0.5
                centery = self.projProc.orig_img.shape[0] / 2. - 0.5
                for num in self.allboxes.keys():
                    box = self.allboxes[num]
                    boxx = box[0]
                    boxy = box[1]
                    if boxx[0] <= x <= boxx[1] and boxy[0] <= y <= boxy[1]:
                        type = self.boxtypes[num]
                        if type == 'h':
                            distance = int(round(abs(centerx-x)))
                            peaks[num].append(distance)
                            ax.plot((centerx-distance, centerx-distance), boxy, color='r')
                            ax.plot((centerx+distance, centerx+distance), boxy, color='r')
                        else:
                            distance = int(round(abs(centery - y)))
                            peaks[num].append(distance)
                            ax.plot(boxx, (centery - distance, centery - distance), color='r')
                            ax.plot(boxx, (centery + distance, centery + distance), color='r')
                        break
                self.displayImgCanvas.draw_idle()
        else:
            if func[0] == "im_zoomin":
                func.append((x, y))
                if len(func) == 3:
                    p1 = func[1]
                    p2 = func[2]
                    # Set zoom-in location ( x,y, ranges) and update image tab
                    self.img_zoom = [(min(p1[0], p2[0]), max(p1[0], p2[0])), (min(p1[1], p2[1]), max(p1[1], p2[1]))]
                    self.function = None
                    self.imgZoomInB.setChecked(False)
                    self.updateImage()

    def imgOnMotion(self, event):
        """
        Triggered when mouse hovers on image in image tab
        """
        if self.projProc is None:
            return

        x = event.xdata
        y = event.ydata

        img = self.projProc.orig_img

        # Display pixel information if the cursor is on image
        if x is not None and y is not None:
            x = int(round(x))
            y = int(round(y))
            if x < img.shape[1] and y < img.shape[0]:
                self.pixel_detail.setText("x=" + str(x) + ', y=' + str(y) + ", value=" + str(img[y][x]))

        # Calculate new x,y if cursor is outside figure
        if x is None or y is None:
            self.pixel_detail.setText("")
            ax = self.displayImgFigure.add_subplot(111)
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
        elif func[0] == "im_zoomin":
            # draw rectangle
            if len(func) < 2:
                return
            ax = self.displayImgFigure.add_subplot(111)
            if len(ax.patches) > 0:
                ax.patches.pop()
            start_pt = func[1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.displayImgCanvas.draw_idle()

        elif func[0] == "box":
            if len(func) == 1:
                # cross lines
                ax = self.displayImgFigure.add_subplot(111)
                ax.lines = []
                ax.axhline(y, color='y', linestyle='dotted')
                ax.axvline(x, color='y', linestyle='dotted')
                self.displayImgCanvas.draw_idle()
            elif len(func) == 2:
                # draw rectangle
                ax = self.displayImgFigure.add_subplot(111)
                if len(ax.patches) > 0:
                    ax.patches = ax.patches[:len(self.allboxes.keys())]
                ax.lines = []
                start_pt = func[-1]
                w = abs(start_pt[0] - x)
                h = abs(start_pt[1] - y)
                x = min(start_pt[0], x)
                y = min(start_pt[1], y)
                ax.add_patch(patches.Rectangle((x, y), w, h,
                                               linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
                self.displayImgCanvas.draw_idle()
        elif func[0] == "im_move":
            # change zoom-in location (x,y ranges) to move around image
            if self.img_zoom is not None:
                ax = self.displayImgFigure.add_subplot(111)
                move = (func[1][0] - x, func[1][1] - y)
                self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[1], img.shape[0])
                ax.set_xlim(self.img_zoom[0])
                ax.set_ylim(self.img_zoom[1])
                ax.invert_yaxis()
                self.displayImgCanvas.draw_idle()

    def imgReleased(self, event):
        """
        Triggered when mouse released from image
        """
        if self.function is not None and self.function[0] == "im_move":
            self.function = None

    def leaveImage(self, event):
        """
        Set pixel information is empty if mouse leaves figure
        """
        self.pixel_detail.setText("")
        
    def imgScrolled(self, event):
        """
        This function is called when a mouse scrolled on the image in image tab. This will affect zoom-in and zoom-out
        """
        if self.projProc is None or event.xdata is None or event.ydata is None:
            return

        direction = event.button
        x = event.xdata
        y = event.ydata
        img_size = self.projProc.orig_img.shape

        if self.img_zoom is None:
            # init values if it's None
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

        # Set new x, y ranges
        self.img_zoom = [(x1, x2), (y1, y2)]

        # To zoom-in or zoom-out is setting x and y limit of figure
        ax = self.displayImgFigure.add_subplot(111)
        ax.set_xlim(self.img_zoom[0])
        ax.set_ylim(self.img_zoom[1])
        ax.invert_yaxis()
        self.displayImgCanvas.draw_idle()

    # def setAngleAndCenterClicked(self):
    #     """
    #     Prepare for manual rotation angle and center setting
    #     """
    #     if self.projProc is None:
    #         return
    #
    #     if self.setRotAndCentB.isChecked():
    #         self.setLeftStatus("Click on 2 corresponding reflection peaks along the equator (ESC to cancel)")
    #         ax = self.displayImgFigure.add_subplot(111)
    #         del ax.lines
    #         ax.lines = []
    #         del ax.patches
    #         ax.patches = []
    #         self.displayImgCanvas.draw_idle()
    #         self.function = ["angle_center"]  # set current active function
    #     else:
    #         self.resetUI()
    #
    # def setAngleClicked(self):
    #     """
    #     Prepare for manual rotation angle setting
    #     """
    #     if self.projProc is None:
    #         return
    #
    #     if self.setAngleB.isChecked():
    #         self.setLeftStatus("Click on image to select the angle of the equator (ESC to cancel)")
    #         ax = self.displayImgFigure.add_subplot(111)
    #         del ax.lines
    #         ax.lines = []
    #         del ax.patches
    #         ax.patches = []
    #         self.displayImgCanvas.draw_idle()
    #         self.function = ["angle"]  # set current active function
    #     else:
    #         self.resetUI()

    def browseFile(self):
        file_name = str(QtGui.QFileDialog.getOpenFileName(self, 'Open File', '', 'Images (*.tif)', None))
        if file_name != "":
            self.onImageSelect(file_name)

    def onImageSelect(self, fullfilename):
        self.dir_path, self.imgList, self.current_file = getImgFiles(fullfilename)
        self.propGrp.setEnabled(True)
        self.boxGrp.setEnabled(True)
        cache = self.loadBoxesAndPeaks()
        if cache is not None:
            self.allboxes = cache['boxes']
            self.peaks = cache['peaks']
            self.boxtypes = cache['boxtypes']
        else:
            self.allboxes = {}
            self.peaks = {}
        self.csvManager = PT_CVSManager(self.dir_path, self.allboxes, self.peaks)
        self.addBoxTabs()
        self.selectPeaksGrp.setEnabled(False)
        self.onImageChanged()

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new BioImage object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        self.projProc = ProjectionProcessor(self.dir_path, self.imgList[self.current_file])
        # self.initSpinBoxes(self.projProc.info)
        self.initMinMaxIntensities(self.projProc)
        self.img_zoom = None
        self.refreshStatusbar()

        # Process new image
        self.processImage()

    def initMinMaxIntensities(self, projProc):
        """
        Set preference for image min & max intesity spinboxes, and initial their value
        :param projProc: current Projection Processor object
        :return:
        """
        img = projProc.orig_img
        self.syncUI = True
        self.minIntSpnBx.setMinimum(img.min())
        self.minIntSpnBx.setMaximum(img.max())
        self.maxIntSpnBx.setMinimum(img.min())
        self.maxIntSpnBx.setMaximum(img.max())
        self.minIntLabel.setText("Min Intensity <br/>("+str(img.min())+")")
        self.maxIntLabel.setText("Max Intensity <br/>("+str(img.max())+")")
        step = (img.max() - img.min()) * 0.07  # set spinboxes step as 7% of image range
        self.minIntSpnBx.setSingleStep(step)
        self.maxIntSpnBx.setSingleStep(step)

        # use cached values if they're available
        if "minInt" in self.projProc.info and "maxInt" in self.projProc.info:
            self.minIntSpnBx.setValue(self.projProc.info["minInt"])
            self.maxIntSpnBx.setValue(self.projProc.info["maxInt"])
        else:
            if self.maxIntSpnBx.value() == 0:
                self.minIntSpnBx.setValue(0)  # init min intensity as min value
                self.maxIntSpnBx.setValue(img.max() * 0.1)  # init max intensity as 20% of max value
        self.syncUI = False

    def processImage(self):
        """
        Process Image by getting all settings and call process() of BioImage object
        Then, write data and update UI
        """
        if self.projProc is None:
            return
        QtGui.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
        QtGui.QApplication.processEvents()
        settings = self.getSettings()
        try:
            self.projProc.process(settings)
        except Exception, e:
            QtGui.QApplication.restoreOverrideCursor()
            errMsg = QtGui.QMessageBox()
            errMsg.setText('Unexpected error')
            msg = 'Please report the problem with error message below\n\n'
            msg += "Error : " + str(sys.exc_info()[0]) + '\n\n' + str(traceback.format_exc())
            errMsg.setInformativeText(msg)
            errMsg.setStandardButtons(QtGui.QMessageBox.Ok)
            errMsg.setIcon(QtGui.QMessageBox.Warning)
            errMsg.setFixedWidth(300)
            errMsg.exec_()
            raise

        # self.csvManager.writeNewData(self.projProc)
        self.resetUI()
        self.refreshStatusbar()
        self.cacheBoxesAndPeaks()
        self.csvManager.setColumnNames(self.allboxes, self.peaks)
        self.csvManager.writeNewData(self.projProc)
        self.exportHistograms()
        QtGui.QApplication.restoreOverrideCursor()

    def exportHistograms(self):
        """
        Export both original histograms and background subtracted histograms if Export All Projections is checked
        :return:
        """
        if self.exportChkBx.isChecked() and self.projProc:
            path = fullPath(self.dir_path,'/pt_results/1d_projections')
            createFolder(path)
            orig_hists = self.projProc.info['hists']
            subtr_hists = self.projProc.info['subtracted_hists']

            for k in orig_hists.keys():
                hist = orig_hists[k]
                xs = np.arange(len(hist))
                f = open(fullPath(path, 'box_'+str(k)+'_original.txt'), 'w')
                coords = zip(xs, hist)
                f.write("\n".join(map(lambda c : str(c[0])+"\t"+str(c[1]), coords)))
                if subtr_hists.has_key(k):
                    sub_hist = subtr_hists[k]
                    f = open(fullPath(path, 'box_' + str(k) + '_subtracted.txt'), 'w')
                    coords = zip(xs, sub_hist)
                    f.write("\n".join(map(lambda c: str(c[0]) + "\t" + str(c[1]), coords)))


    def cacheBoxesAndPeaks(self):
        cache = {
            'boxes' : self.allboxes,
            'peaks' : self.peaks,
            'boxtypes' : self.boxtypes
        }

        cache_dir = fullPath(self.dir_path, 'pt_cache')
        createFolder(cache_dir)
        cache_file = fullPath(cache_dir, 'boxes_peaks.info')
        pickle.dump(cache, open(cache_file, "wb"))

    def loadBoxesAndPeaks(self):
        cache_file = fullPath(fullPath(self.dir_path, 'pt_cache'), 'boxes_peaks.info')
        if exists(cache_file):
            cache = pickle.load(open(cache_file, "rb"))
            if cache is not None:
                return cache
        return None

    def getSettings(self):
        settings = {}

        # add boxes
        settings['boxes'] = self.allboxes

        # add box types
        settings['types'] = self.boxtypes

        # add peaks location
        settings['peaks'] = self.peaks

        if self.calSettings is not None:
            if self.calSettings["type"] == "img":
                settings["lambda_sdd"] = self.calSettings["silverB"] * self.calSettings["radius"]
            elif self.calSettings["type"] == "cont":
                settings["lambda_sdd"] = 1. * self.calSettings["lambda"] * self.calSettings["sdd"] / self.calSettings["pixel_size"]

        return settings

    def refreshStatusbar(self):
        """
        Set Left status bar to be image detail
        Set Right status bar to by image shape and type
        Clear pixel detail
        """
        if self.projProc is None:
            return
        self.setLeftStatus( "(" + str(self.current_file + 1) + "/" + str(len(self.imgList)) + ") " + fullPath(self.dir_path,
                                                                                            self.projProc.filename))
        img = self.projProc.orig_img
        self.right_status.setText(str(img.shape[0]) + "x" + str(img.shape[1]) + " " + str(img.dtype))
        self.pixel_detail.setText("")
        QtGui.QApplication.processEvents()

    def setLeftStatus(self, s):
        """
        Set text on status bar on the left
        :param s: input text (str)
        """
        self.left_status.setText(s)
        QtGui.QApplication.processEvents()
    #
    # def lockAngle(self):
    #     self.lockAngleSpnBx.setEnabled(self.lockAngleChkBx.isChecked())

    def resetUI(self):
        """
        Refresh all tabs
        """
        self.function = None
        # self.graph_zoom = None
        QtGui.QApplication.restoreOverrideCursor()
        self.selectPeaksButton.setText("Select Approximate Peak Locations")
        self.addBoxButton.setText("Add A Box")

        for b in self.checkableButtons:
            b.setChecked(False)
        for k in self.update_plot.keys():
            self.update_plot[k] = True
        for i in range(1, self.tabWidget.count()):
            tab = self.tabWidget.widget(i)
            tab.clearFlags()

        self.updateUI()

    def updateUI(self):
        if self.projProc is not None and not self.syncUI:
            ind = self.tabWidget.currentIndex()
            if ind == 0:
                # if image tab is selected
                self.updateImageTab()
            else:
                tab = self.tabWidget.widget(ind)
                tab.updateUI()

    def updateImageTab(self):
        """
        Draw all UI in image tab
        """
        if self.projProc is None or self.syncUI or not self.update_plot['img']:
            return

        img = self.projProc.orig_img
        img = getBGR(get8bitImage(copy.copy(img), min=self.minIntSpnBx.value(), max=self.maxIntSpnBx.value()))
        ax = self.displayImgFigure.add_subplot(111)
        ax.cla()
        ax.imshow(img)

        if len(self.allboxes.keys()) > 0:
            self.selectPeaksGrp.setEnabled(True)
            if self.boxesChkBx.isChecked():
                for num in self.allboxes.keys():
                    b = self.allboxes[num]
                    x = b[0][0]
                    y = b[1][0]
                    w = b[0][1] - b[0][0]
                    h = b[1][1] - b[1][0]

                    if self.boxtypes[num] == 'h':
                        ax.add_patch(patches.Rectangle((x, y), w, h,
                                                       linewidth=1, edgecolor='#95f70c', facecolor='none'))
                        ax.text(x + w + 10, y + h / 2., str(num), color='#95f70c', fontsize=10, horizontalalignment='left', verticalalignment='center')
                    else:
                        ax.add_patch(patches.Rectangle((x, y), w, h,
                                                       linewidth=1, edgecolor='y', facecolor='none'))
                        ax.text(x + w /2. , y - 10, str(num), color='y', fontsize=10, horizontalalignment='center',
                                verticalalignment='center')

            if self.peaksChkBx.isChecked():
                for num in self.peaks.keys():
                    centerx = self.projProc.orig_img.shape[1] / 2. - 0.5
                    centery = self.projProc.orig_img.shape[0] / 2. - 0.5
                    for p in self.peaks[num]:
                        if self.boxtypes[num] == 'h':
                            ax.plot((centerx - p, centerx - p), self.allboxes[num][1], color='m')
                            ax.plot((centerx + p, centerx + p), self.allboxes[num][1], color='m')
                        else:
                            ax.plot(self.allboxes[num][0], (centery - p, centery - p), color='r')
                            ax.plot(self.allboxes[num][0], (centery + p, centery + p), color='r')

        # Zoom
        if self.img_zoom is not None and len(self.img_zoom) == 2:
            ax.set_xlim(self.img_zoom[0])
            ax.set_ylim(self.img_zoom[1])
        else:
            ax.set_xlim((0, img.shape[1]))
            ax.set_ylim((0, img.shape[0]))

        self.img_zoom = [ax.get_xlim(), ax.get_ylim()]
        ax.invert_yaxis()
        self.displayImgFigure.tight_layout()
        self.displayImgCanvas.draw()

#     def launchQF(self):
#         self.qf = QFExtend(self)
#         self.qf.onNewFileSelected(fullPath(self.dir_path, self.imgList[self.current_file]))
#
#     def childWindowClosed(self, quadFold):
#         fold = quadFold.info['avg_fold']
#         self.projProc.orig_img = quadFold.imgCache['resultImg']
#         self.projProc.info['center'] = (fold.shape[1], fold.shape[0])
#         self.projProc.info['rotationAngle'] = 0
#         self.qf = None
#         self.img_zoom = None
#         self.resetUI()
#
# class QFExtend(QuadrantFoldingGUI):
#     def __init__(self, mainwin):
#         QuadrantFoldingGUI.__init__(self)
#         self.mainwin = mainwin
#         self.nextButton.setEnabled(False)
#         self.prevButton.setEnabled(False)
#         self.nextButton2.setEnabled(False)
#         self.prevButton2.setEnabled(False)
#
#     def closeEvent(self, ev):
#         """
#         Trigger when window is closed
#         """
#         # delete window object from main window
#         self.mainwin.childWindowClosed(self.quadFold)