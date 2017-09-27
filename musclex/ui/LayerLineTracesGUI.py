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
import cv2
import numpy as np
import matplotlib.patches as patches
from musclex.ui.QuadrantFoldingGUI import QuadrantFoldingGUI
from musclex.bio_utils.file_manager import fullPath, getImgFiles, getStyleSheet
from musclex.biocat_modules.LayerLineProcessor import LayerLineProcessor
from musclex.bio_utils.image_processor import getBGR, get8bitImage, getNewZoom
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
import sys
import traceback
import musclex
import copy

class LayerLineTracesGUI(QtGui.QMainWindow):
    """
    This class is for Layer Line Processor GUI Object
    """
    def __init__(self):
        QtGui.QWidget.__init__(self)
        self.setWindowTitle("Layer Line Traces v." + musclex.__version__)
        self.current_file = 0
        self.dir_path = ""
        self.calSettings = None
        self.update_plot = {'img':True}
        self.imgList = []
        self.layerProc = None
        self.syncUI = False
        self.img_zoom = None
        self.function = None
        self.layerlineboxes = []
        self.qf = None
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
        self.tabWidget.setTabsClosable(False)
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
        self.propGrp = QtGui.QGroupBox("2. Pattern Properties")
        self.propGrp.setEnabled(False)
        self.propLayout = QtGui.QGridLayout(self.propGrp)
        self.calibrateButton = QtGui.QPushButton("Calibration Settings")
        self.quadFoldButton = QtGui.QPushButton("Quadrant Folding")
        self.setCenterButton = QtGui.QPushButton("Set Rotation and Center")
        self.setCenterButton.setCheckable(True)
        self.checkableButtons.append(self.setCenterButton)
        self.setRotationButton = QtGui.QPushButton("Set Rotation Angle")
        self.setRotationButton.setCheckable(True)
        self.checkableButtons.append(self.setRotationButton)
        self.lockAngleChkBx = QtGui.QCheckBox("Lock Angle")
        self.lockAngleSpnBx = QtGui.QSpinBox()
        self.lockAngleSpnBx.setEnabled(False)
        self.lockAngleSpnBx.setRange(-180, 180)
        self.propLayout.addWidget(self.calibrateButton, 0, 0, 1, 2)
        self.propLayout.addWidget(self.quadFoldButton, 1, 0, 1, 2)
        self.propLayout.addWidget(self.setCenterButton, 2, 0, 1, 2)
        self.propLayout.addWidget(self.setRotationButton, 3, 0, 1, 2)
        self.propLayout.addWidget(self.lockAngleChkBx, 4, 0, 1, 1)
        self.propLayout.addWidget(self.lockAngleSpnBx, 4, 1, 1, 1)

        # Layer Line selection
        self.layerlineBoxGrp = QtGui.QGroupBox("3. Layer line box")
        self.layerlineBoxGrp.setEnabled(False)
        self.layerlineBoxLayout = QtGui.QVBoxLayout(self.layerlineBoxGrp)
        self.selectBoxButton = QtGui.QPushButton("Select Layer Line Boxes")
        self.selectBoxButton.setCheckable(True)
        self.checkableButtons.append(self.selectBoxButton)
        self.layerlineBoxLayout.addWidget(self.selectBoxButton)

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
        self.leftFrameLayout.addWidget(self.layerlineBoxGrp)
        self.leftFrameLayout.addSpacing(10)
        self.leftFrameLayout.addWidget(self.selectPeaksGrp)
        self.leftFrameLayout.addStretch()

        self.imageRightFrame = QtGui.QFrame()
        self.imageRightFrame.setFixedWidth(250)
        self.rightFrameLayout = QtGui.QVBoxLayout(self.imageRightFrame)

        # Display Options
        self.dispOptGrp = QtGui.QGroupBox("Display Options")
        self.dispOptLayout = QtGui.QGridLayout(self.dispOptGrp)

        self.boxesChkBx = QtGui.QCheckBox("Layer Line Boxes")
        self.boxesChkBx.setChecked(True)
        self.imgZoomInB = QtGui.QPushButton("Zoom In")
        self.imgZoomInB.setCheckable(True)
        self.imgZoomOutB = QtGui.QPushButton("Full")
        self.checkableButtons.append(self.imgZoomInB)

        self.minIntLabel = QtGui.QLabel("Min Intensity")
        self.minIntSpnBx = QtGui.QDoubleSpinBox()
        self.minIntSpnBx.setKeyboardTracking(False)
        self.maxIntLabel = QtGui.QLabel("Max Intensity")
        self.maxIntSpnBx = QtGui.QDoubleSpinBox()
        self.maxIntSpnBx.setKeyboardTracking(False)

        self.dispOptLayout.addWidget(self.boxesChkBx, 0, 0, 1, 2)
        self.dispOptLayout.addWidget(self.imgZoomInB, 1, 0, 1, 1)
        self.dispOptLayout.addWidget(self.imgZoomOutB, 1, 1, 1, 1)
        self.dispOptLayout.addWidget(self.minIntLabel, 2, 0, 1, 1)
        self.dispOptLayout.addWidget(self.minIntSpnBx, 2, 1, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntLabel, 3, 0, 1, 1)
        self.dispOptLayout.addWidget(self.maxIntSpnBx, 3, 1, 1, 1)

        self.rightFrameLayout.addWidget(self.dispOptGrp)
        self.rightFrameLayout.addStretch()

        self.imageTabLayer.addWidget(self.imageLeftFrame)
        self.imageTabLayer.addWidget(self.displayImgCanvas)
        self.imageTabLayer.addWidget(self.imageRightFrame)

        self.tabWidget.addTab(self.imageTab, "Image")

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
        self.browseImageButton.clicked.connect(self.browseFile)
        self.lockAngleChkBx.stateChanged.connect(self.lockAngle)
        self.quadFoldButton.clicked.connect(self.launchQF)
        self.maxIntSpnBx.valueChanged.connect(self.updateImage)
        self.minIntSpnBx.valueChanged.connect(self.updateImage)
        self.boxesChkBx.stateChanged.connect(self.updateImage)
        self.imgZoomInB.clicked.connect(self.imgZoomIn)
        self.imgZoomOutB.clicked.connect(self.imgZoomOut)

        self.selectBoxButton.clicked.connect(self.addBoxes)
        self.selectPeaksButton.clicked.connect(self.addPeaks)

        self.displayImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.displayImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)
        self.displayImgFigure.canvas.mpl_connect('button_release_event', self.imgReleased)

    def imgZoomIn(self):
        """
        Triggered when Zoom in image is pressed
        """
        if self.imgZoomInB.isChecked():
            self.setLeftStatus("Please select zoom-in area by clicking 2 points to make a rectangle (ESC to cancel)")
            ax = self.displayImgFigure.add_subplot(111)
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            self.displayImgCanvas.draw_idle()
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

    def addPeaks(self):
        """
        Triggered when Select Layer Line Boxes pressed
        :return:
        """
        if self.layerProc is None:
            self.selectPeaksButton.setChecked(False)
            return

        if self.selectPeaksButton.isChecked():
            # Start function
            self.selectPeaksButton.setText("Done")
            self.setLeftStatus(
                "Add Boxes to the image by drawing rectangles, press Done when all boxes added (ESC to cancel)")
            self.function = ['peaks']
            for i in range(len(self.layerlineboxes)):
                self.function.append([])
            ax = self.displayImgFigure.add_subplot(111)
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            self.displayImgCanvas.draw_idle()
        else:
            if self.function is not None and len(self.function) > 2:
                # When Done clicked
                all_points = self.function[1:]
                if len(all_points) > 1:
                    if len(all_points) % 2 != 0:
                        all_points = all_points[:-1]
                    self.layerlineboxes = []
                    for i in np.arange(0, len(all_points), 2):
                        w = int(round(abs(all_points[i][0] - all_points[i + 1][0])))
                        h = int(round(abs(all_points[i][1] - all_points[i + 1][1])))
                        x = int(round(min(all_points[i][0], all_points[i + 1][0])))
                        y = int(round(min(all_points[i][1], all_points[i + 1][1])))
                        self.layerlineboxes.append(((x, y), (w, h)))
                    self.addLayerLineTabs()
            self.selectPeaksButton.setText("Select Approximate Peak Locations")
            self.processImage()

    def addBoxes(self):
        """
        Triggered when Select Layer Line Boxes pressed
        :return:
        """
        if self.layerProc is None:
            self.selectBoxButton.setChecked(False)
            return

        if self.selectBoxButton.isChecked():
            # Start function
            self.selectBoxButton.setText("Done")
            self.setLeftStatus("Add Boxes to the image by drawing rectangles, press Done when all boxes added (ESC to cancel)")
            self.function = ['box']
            ax = self.displayImgFigure.add_subplot(111)
            del ax.lines
            ax.lines = []
            del ax.patches
            ax.patches = []
            self.displayImgCanvas.draw_idle()
        else:
            if self.function is not None and len(self.function) > 2:
                # When Done clicked
                all_points = self.function[1:]
                if len(all_points) > 1:
                    if len(all_points)%2 != 0:
                        all_points = all_points[:-1]
                    self.layerlineboxes = []
                    for i in np.arange(0, len(all_points), 2):
                        w = int(round(abs(all_points[i][0] - all_points[i+1][0])))
                        h = int(round(abs(all_points[i][1] - all_points[i+1][1])))
                        x = int(round(min(all_points[i][0], all_points[i+1][0])))
                        y = int(round(min(all_points[i][1], all_points[i+1][1])))
                        self.layerlineboxes.append(((x,y),(w,h)))
                    self.removeLayerLineTabs()
            self.selectBoxButton.setText("Select Layer Line Boxes")
            self.processImage()

    def removeLayerLineTabs(self):
        """
        Remove old layer line tabs and add new layer line tabs
        :return:
        """
        while self.tabWidget.count() > 1:
            self.tabWidget.removeTab(1)
        # self.tabWidget.removeTab()

    def imgClicked(self, event):
        """
        Triggered when mouse presses on image in image tab
        """
        if self.layerProc is None:
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
        elif func[0] == "angle_center":
            # draw X at points and a line between points
            ax = self.displayImgFigure.add_subplot(111)
            axis_size = 5
            ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
            self.displayImgCanvas.draw_idle()
            func.append((x, y))
            if len(func) == 3:
                if func[1][0] < func[2][0]:
                    x1, y1 = func[1]
                    x2, y2 = func[2]
                else:
                    x1, y1 = func[2]
                    x2, y2 = func[1]

                if abs(x2 - x1) == 0:
                    new_angle = -90
                else:
                    new_angle = -180. * np.arctan((y1 - y2) / abs(x1 - x2)) / np.pi
                # new_angle += 90.

                cx = int(round((x1 + x2) / 2.))
                cy = int(round((y1 + y2) / 2.))
                M = cv2.getRotationMatrix2D(tuple(self.layerProc.info['center']), self.layerProc.info['rotationAngle'], 1)
                invM = cv2.invertAffineTransform(M)
                homo_coords = [cx, cy, 1.]
                new_center = np.dot(invM, homo_coords)
                # Set new center and rotaion angle , re-calculate R-min
                self.layerProc.info['center'] = (int(round(new_center[0])), int(round(new_center[1])))
                self.layerProc.info['rotationAngle'] = self.layerProc.info['rotationAngle'] + new_angle
                self.layerProc.removeInfo('hists')
                self.setCenterButton.setChecked(False)
                self.processImage()
        elif func[0] == "angle":
            center = self.layerProc.info['center']
            x1 = center[0]
            y1 = center[1]
            if abs(x - x1) == 0:
                new_angle = -90
            else:
                new_angle = -180. * np.arctan((y1 - y) / (x1 - x)) / np.pi

            # Set new rotaion angle , re-calculate from R-min calculation process
            self.layerProc.info['rotationAngle'] = self.layerProc.info['rotationAngle'] - new_angle
            self.layerProc.removeInfo('hists')
            self.setRotationButton.setChecked(False)
            self.processImage()
        elif func[0] == "box":
            func.append((x, y))
            if len(func) < 3 or (len(func)-1)%2 != 0:
                return
            ax = self.displayImgFigure.add_subplot(111)
            if len(ax.patches) > 0:
                ax.patches.pop()
            start_pt = func[-2]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='y', facecolor='none', linestyle='dotted'))
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
        if self.layerProc is None or 'rotationAngle' not in self.layerProc.info.keys():
            return

        x = event.xdata
        y = event.ydata

        img = self.layerProc.getRotatedImage()

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
            # draw rectangle
            if len(func) < 2 or (len(func)-1)%2 == 0:
                return
            ax = self.displayImgFigure.add_subplot(111)
            if len(ax.patches) > 0:
                ax.patches = ax.patches[:(len(func)-1)/2]
            start_pt = func[-1]
            w = abs(start_pt[0] - x)
            h = abs(start_pt[1] - y)
            x = min(start_pt[0], x)
            y = min(start_pt[1], y)
            ax.add_patch(patches.Rectangle((x, y), w, h,
                                           linewidth=1, edgecolor='r', facecolor='none', linestyle='dotted'))
            self.displayImgCanvas.draw_idle()

        elif func[0] == "angle":
            # draw line as angle
            center = self.layerProc.info["center"]
            deltax = x - center[0]
            deltay = y - center[1]
            x2 = center[0] - deltax
            y2 = center[1] - deltay
            ax = self.displayImgFigure.add_subplot(111)
            del ax.lines
            ax.lines = []
            ax.plot([x, x2], [y, y2], color="g")
            self.displayImgCanvas.draw_idle()

        elif func[0] == "angle_center":
            # draw X on points and a line between points
            ax = self.displayImgFigure.add_subplot(111)
            # ax2 = self.displayImgFigure.add_subplot(4,4,13)
            axis_size = 5

            if len(func) == 1:
                if len(ax.lines) > 0:
                    del ax.lines
                    ax.lines = []
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            elif len(func) == 2:
                start_pt = func[1]
                if len(ax.lines) > 2:
                    first_cross = ax.lines[:2]
                    del ax.lines
                    ax.lines = first_cross
                ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
                ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
                ax.plot((start_pt[0], x), (start_pt[1], y), color='r')

            # bound = 10
            # ax2.set_xlim((x-bound, x+bound))
            # ax2.set_ylim((y-bound, y+bound))
            # del ax2.lines
            # ax2.lines = []
            # ax2.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            # ax2.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')

            self.displayImgCanvas.draw_idle()
            # self.displayImgCanvas.flush_events()

        elif func[0] == "im_move":
            # change zoom-in location (x,y ranges) to move around image
            if self.img_zoom is not None:
                ax = self.displayImgFigure.add_subplot(111)
                move = (func[1][0] - x, func[1][1] - y)
                self.img_zoom = getNewZoom(self.img_zoom, move, img.shape[0], img.shape[1])
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

    def browseFile(self):
        file_name = str(QtGui.QFileDialog.getOpenFileName(self, 'Open File', '', 'Images (*.tif)', None))
        if file_name != "":
            self.onImageSelect(file_name)

    def onImageSelect(self, fullfilename):
        self.dir_path, self.imgList, self.current_file = getImgFiles(fullfilename)
        self.propGrp.setEnabled(True)
        self.layerlineBoxGrp.setEnabled(True)
        self.layerlineboxes = []
        self.selectPeaksGrp.setEnabled(False)
        self.onImageChanged()

    def onImageChanged(self):
        """
        Need to be called when image is change i.e. to the next image.
        This will create a new BioImage object for the new image and syncUI if cache is available
        Process the new image if there's no cache.
        """
        self.layerProc = LayerLineProcessor(self.dir_path, self.imgList[self.current_file])
        # self.initSpinBoxes(self.layerProc.info)
        self.initMinMaxIntensities(self.layerProc)
        self.img_zoom = None
        self.refreshStatusbar()

        # Process new image
        self.processImage()

    def initMinMaxIntensities(self, layerProc):
        """
        Set preference for image min & max intesity spinboxes, and initial their value
        :param layerProc: current layer line Processor object
        :return:
        """
        img = layerProc.orig_img
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

        if img.shape == (1043, 981):
            self.minIntSpnBx.setDecimals(2)
            self.maxIntSpnBx.setDecimals(2)
        else:
            self.minIntSpnBx.setDecimals(0)
            self.maxIntSpnBx.setDecimals(0)

        # use cached values if they're available
        if "minInt" in self.layerProc.info and "maxInt" in self.layerProc.info:
            self.minIntSpnBx.setValue(self.layerProc.info["minInt"])
            self.maxIntSpnBx.setValue(self.layerProc.info["maxInt"])
        else:
            self.minIntSpnBx.setValue(img.min())  # init min intensity as min value
            self.maxIntSpnBx.setValue(img.max() * 0.1)  # init max intensity as 20% of max value
        self.syncUI = False

    def processImage(self):
        """
        Process Image by getting all settings and call process() of BioImage object
        Then, write data and update UI
        """
        if self.layerProc is None:
            return
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
        QtGui.QApplication.processEvents()
        settings = self.getSettings()
        try:
            self.layerProc.process(settings)
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

        # self.csvManager.writeNewData(self.bioImg)
        self.resetUI()
        self.refreshStatusbar()
        QtGui.QApplication.restoreOverrideCursor()

    def getSettings(self):
        settings = {}
        if len(self.layerlineboxes) > 0:
            settings['boxes'] = self.layerlineboxes
        return settings

    def refreshStatusbar(self):
        """
        Set Left status bar to be image detail
        Set Right status bar to by image shape and type
        Clear pixel detail
        """
        if self.layerProc is None:
            return
        self.setLeftStatus( "(" + str(self.current_file + 1) + "/" + str(len(self.imgList)) + ") " + fullPath(self.dir_path,
                                                                                            self.layerProc.filename))
        img = self.layerProc.orig_img
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

    def lockAngle(self):
        self.lockAngleSpnBx.setEnabled(self.lockAngleChkBx.isChecked())

    def resetUI(self):
        """
        Refresh all tabs
        """
        self.function = None
        # self.graph_zoom = None
        QtGui.QApplication.restoreOverrideCursor()
        for b in self.checkableButtons:
            b.setChecked(False)
        for k in self.update_plot.keys():
            self.update_plot[k] = True

        self.updateUI()

    def updateUI(self):
        if self.layerProc is not None and not self.syncUI:
            ind = self.tabWidget.currentIndex()
            if ind == 0:
                # if image tab is selected
                self.updateImageTab()

    def updateImageTab(self):
        """
        Draw all UI in image tab
        """
        if self.layerProc is None or self.syncUI or not self.update_plot['img']:
            return

        img = self.layerProc.getRotatedImage()
        img = getBGR(get8bitImage(copy.copy(img), min=self.minIntSpnBx.value(), max=self.maxIntSpnBx.value()))
        ax = self.displayImgFigure.add_subplot(111)
        ax.cla()
        ax.imshow(img)

        if len(self.layerlineboxes) > 0:
            self.selectPeaksGrp.setEnabled(True)
            if self.boxesChkBx.isChecked():
                for b in self.layerlineboxes:
                    ax.add_patch(patches.Rectangle(b[0], b[1][0], b[1][1],
                                                   linewidth=1, edgecolor='y', facecolor='none'))

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

    def launchQF(self):
        self.qf = QFExtend(self)
        self.qf.onNewFileSelected(fullPath(self.dir_path, self.imgList[self.current_file]))

    def childWindowClosed(self, quadFold):
        fold = quadFold.info['avg_fold']
        self.layerProc.orig_img = quadFold.imgCache['resultImg']
        self.layerProc.info['center'] = (fold.shape[1], fold.shape[0])
        self.layerProc.info['rotationAngle'] = 0
        self.qf = None
        self.img_zoom = None
        self.resetUI()

class QFExtend(QuadrantFoldingGUI):
    def __init__(self, mainwin):
        QuadrantFoldingGUI.__init__(self)
        self.mainwin = mainwin
        self.nextButton.setEnabled(False)
        self.prevButton.setEnabled(False)
        self.nextButton2.setEnabled(False)
        self.prevButton2.setEnabled(False)

    def closeEvent(self, ev):
        """
        Trigger when window is closed
        """
        # delete window object from main window
        self.mainwin.childWindowClosed(self.quadFold)