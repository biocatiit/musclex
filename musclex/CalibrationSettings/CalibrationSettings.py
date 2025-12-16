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

import pickle
from os.path import isfile, exists
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import fabio
import musclex
import numpy as np
from pyFAI.detectors import Detector
from ..ui.pyqt_utils import *
from ..utils.file_manager import fullPath, createFolder, ifHdfReadConvertless
from ..utils.image_processor import *

class CalibrationSettings(QDialog):
    """
    The CalibrationSettings object is a window and functions helping the software to calibrate the images processed and improve the results found.
    """
    def __init__(self, dir_path, center=None, quadrant_folded=False, initial_settings=None):
        """
        Initialize CalibrationSettings dialog.
        
        Args:
            dir_path: Directory path for calibration files
            center: Optional center coordinates [x, y]
            quadrant_folded: Whether the image is quadrant folded
            initial_settings: Calibration cache dict with 'path' and 'settings' keys.
                            If None, initializes with empty settings.
        
        Note:
            This is a pure UI class. Cache loading should be done externally
            (e.g., by ProcessingWorkspace._load_calibration_cache()).
        """
        super().__init__(None)
        self.setWindowTitle("Calibration Settings")
        self.editableVars = {}
        self.logMsgs = []
        self.dir_path = str(dir_path)
        self.manualCalPoints = None
        self.doubleZoomMode = False
        self.dontShowAgainDoubleZoomMessageResult = False
        self.doubleZoomPt = (0, 0)
        self.doubleZoomAxes = None
        self.cal_img = None
        self.disp_img = None
        self.ax = None
        self.ax2 = None
        self.version = musclex.__version__
        self.uiUpdating = False
        self.quadrant_folded = quadrant_folded
        
        # Initialize with provided settings or defaults
        if initial_settings is not None:
            self.calFile = initial_settings.get("path", fullPath(dir_path, "calibration.tif"))
            self.calSettings = initial_settings.get("settings", {})
        else:
            self.calFile = fullPath(dir_path, "calibration.tif")
            self.calSettings = {}

        # Flag to indicate if calibration needs recalculation
        # Used internally when manual calibration points are changed
        self.recalculate = False

        # Override center if explicitly provided
        if center is not None:
            self.calSettings["center"] = center

        # self.setStyleSheet(getStyleSheet())
        self.initUI()
        self.setConnection()
        self.setAllToolTips()

        if exists(self.calFile) and self.calImageGrpChkBox.isChecked():
            if self.calSettings is None or not self.calSettings:
                self.calibrate()
            else:
                self.updateImage()

    def initUI(self):
        """
        Initialize the CalibrationSettings UI window.
        """
        silverb = 5.83803
        init_lambda = .1033
        init_sdd = 2500
        init_pix_size = 0.172
        typ = None
        center=None
        detector = None
        if self.calSettings is not None:
            typ = self.calSettings["type"] if "type" in self.calSettings else None
            if typ == "cont":
                init_lambda = self.calSettings["lambda"]
                init_sdd = self.calSettings["sdd"]
                init_pix_size = self.calSettings["pixel_size"]
            elif typ == "img":
                silverb = self.calSettings["silverB"]

            if 'center' in self.calSettings:
                center = self.calSettings["center"]
            if "detector" in self.calSettings:
                detector = self.calSettings["detector"]

        self.mainLayout = QVBoxLayout(self)

        self.pathText = QLineEdit()
        if exists(self.calFile):
            self.pathText.setText(self.calFile)
        self.pathText.setEnabled(False)
        self.browseButton = QPushButton("Browse")
        self.unsetButton = QPushButton("Unset")
        self.calImgFigure = plt.figure()
        self.calImgCanvas = FigureCanvas(self.calImgFigure)
        self.calImgCanvas.setHidden(True)

        self.minIntLabel = QLabel("Min intensity : ")
        self.minInt = QDoubleSpinBox()
        #self.minInt.setKeyboardTracking(False)
        self.minInt.setValue(0)
        self.minInt.setDecimals(2)
        self.maxIntLabel = QLabel("Max intensity : ")
        self.maxInt = QDoubleSpinBox()
        #self.maxInt.setKeyboardTracking(False)
        self.maxInt.setValue(0)
        self.maxInt.setDecimals(2)
        self.minIntLabel.setHidden(True)
        self.minInt.setHidden(True)
        self.maxIntLabel.setHidden(True)
        self.maxInt.setHidden(True)

        self.manualCal = QPushButton("Set calibration by points selections")
        self.manualCal.setCheckable(True)
        self.manualCal.setFixedHeight(30)
        self.manualCal.clicked.connect(self.manualCalClicked)
        self.manualCal.setEnabled(exists(self.calFile))
        self.silverBehenate = QDoubleSpinBox()
        self.silverBehenate.setKeyboardTracking(False)
        self.silverBehenate.setDecimals(5)
        self.silverBehenate.setValue(silverb)
        self.silverBehenate.setMinimum(0)
        self.silverBehenate.setMaximum(100)
        self.silverBehenate.setSingleStep(5.83803)
        self.silverBehenate.setObjectName('silverBehenate')
        self.editableVars[self.silverBehenate.objectName()] = None
        self.buttons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        self.buttons.accepted.connect(self.okClicked)
        self.buttons.rejected.connect(self.reject)
        self.buttons.setFixedWidth(100)
        # grpbox_ss = "QGroupBox::title { background-color: #323232 ; subcontrol-origin: margin; subcontrol-position: top left; padding: 0 3px; }"
        self.calImageGrp = QGroupBox("Setting by Calibration Image")
        self.calImageGrp.setCheckable(False)
        self.calImageGrpChkBox = QCheckBox("Enable Setting by Calibration Image") #Checkbox for the calImageGrp to avoid weird behavior
        # self.calImageGrp.setStyleSheet(grpbox_ss)
        self.calImageGrp.setEnabled(typ == "img")
        self.calImageGrpChkBox.setChecked(typ == "img")
        self.calImageLayout = QGridLayout(self.calImageGrp)
        self.calImageLayout.addWidget(QLabel("Calibration File :") , 0, 0, 1, 1)
        self.calImageLayout.addWidget(self.pathText, 0, 1, 1, 1)
        self.calImageLayout.addWidget(self.browseButton, 0, 2, 1, 1)
        self.calImageLayout.addWidget(self.unsetButton, 0, 3, 1, 1)
        self.calImageLayout.addWidget(self.calImgCanvas, 1, 0, 1, 4)
        self.calImageLayout.addWidget(self.minIntLabel, 2, 0, 1, 1)
        self.calImageLayout.addWidget(self.minInt, 2, 1, 1, 1)
        self.calImageLayout.addWidget(self.maxIntLabel, 2, 2, 1, 1)
        self.calImageLayout.addWidget(self.maxInt, 2, 3, 1, 1)
        self.calImageLayout.addWidget(self.manualCal, 3, 0, 1, 2)
        self.calImageLayout.addWidget(QLabel("Calibrant ring d-spacing :"), 3, 2, 1, 1, Qt.AlignRight)
        self.calImageLayout.addWidget(self.silverBehenate, 3, 3, 1, 1)
        # self.calImageLayout.setColumnStretch(1,2)
        self.calImageLayout.setRowStretch(1, 2)

        self.paramGrp = QGroupBox("Setting by Parameters")
        self.paramGrp.setCheckable(False)
        self.paramGrpChkBx = QCheckBox("Enable Setting by Parameters") #Checkbox for the paramgrp to avoid weird behavior
        # self.paramGrp.setStyleSheet(grpbox_ss)
        self.paramGrpChkBx.setChecked(typ == "cont")
        self.paramGrp.setEnabled(typ == "cont")
        self.paramLayout = QGridLayout(self.paramGrp)

        self.lambdaSpnBx = QDoubleSpinBox()
        self.lambdaSpnBx.setDecimals(8)
        self.lambdaSpnBx.setRange(0.00001, 5.)
        self.lambdaSpnBx.setValue(init_lambda)
        self.lambdaSpnBx.setObjectName('lambdaSpnBx')
        self.editableVars[self.lambdaSpnBx.objectName()] = None

        self.sddSpnBx = QDoubleSpinBox()
        self.sddSpnBx.setDecimals(8)
        self.sddSpnBx.setRange(0., 100000000000000000000.)
        self.sddSpnBx.setValue(init_sdd)
        self.sddSpnBx.setObjectName('sddSpnBx')
        self.editableVars[self.sddSpnBx.objectName()] = None

        self.pixsSpnBx = QDoubleSpinBox()
        self.pixsSpnBx.setDecimals(8)
        self.pixsSpnBx.setRange(0.00001, 5.)
        self.pixsSpnBx.setValue(init_pix_size)
        self.pixsSpnBx.setObjectName('pixsSpnBx')
        self.editableVars[self.pixsSpnBx.objectName()] = None


        # self.fixedCenter = QCheckBox("Fixed Center")
        self.centerX = QDoubleSpinBox()
        self.centerX.setDecimals(2)
        self.centerX.setPrefix("X:")
        self.centerX.setRange(0, 100000000000000000000)
        self.centerY = QDoubleSpinBox()
        self.centerY.setDecimals(2)
        self.centerY.setPrefix("Y:")
        self.centerY.setRange(0, 100000000000000000000)

        self.misSettingChkBx = QCheckBox("Correct Mis-Setting Angles")
        self.misSettingChkBx.setEnabled(False)
        self.misSettingChkBx.setToolTip("Not yet implemented")


        if center is not None:
            self.centerX.setValue(center[0])
            self.centerY.setValue(center[1])
        else:
            self.centerX.setValue(1000)
            self.centerY.setValue(1000)

        # self.fixedCenter.setChecked(False)
        #self.centerX.setEnabled(False)
        #self.centerY.setEnabled(False)
        self.centerX.setObjectName('centerX')
        self.editableVars[self.centerX.objectName()] = None
        self.centerY.setObjectName('centerY')
        self.editableVars[self.centerY.objectName()] = None

        # Disable center editing for quadrant folded images
        if self.quadrant_folded:
            self.centerX.setEnabled(False)
            self.centerY.setEnabled(False)
            self.centerX.setToolTip("Center cannot be modified for quadrant folded images (always uses geometric center)")
            self.centerY.setToolTip("Center cannot be modified for quadrant folded images (always uses geometric center)")

        self.manDetector = QCheckBox("Manually Select Detector")
        self.detectorChoice = QComboBox()
        self.alldetectorChoices = list(Detector.registry.keys())
        for c in self.alldetectorChoices:
            self.detectorChoice.addItem(c)
        if detector is not None:
            self.detectorChoice.setCurrentIndex(self.alldetectorChoices.index(detector))
            self.manDetector.setChecked(True)
            self.detectorChoice.setEnabled(True)
        else:
            self.detectorChoice.setCurrentIndex(0)
            self.manDetector.setChecked(False)
            self.detectorChoice.setEnabled(False)
        self.editableVars[self.detectorChoice.objectName()] = None

        self.paramLayout.addWidget(QLabel("Lambda : "), 0, 0, 1, 1)
        self.paramLayout.addWidget(self.lambdaSpnBx, 0, 1, 1, 1)
        self.paramLayout.addWidget(QLabel("nm"), 0, 2, 1, 1)
        self.paramLayout.addWidget(QLabel("S<sub>dd</sub> : "), 1, 0, 1, 1)
        self.paramLayout.addWidget(self.sddSpnBx, 1, 1, 1, 1)
        self.paramLayout.addWidget(QLabel("mm"), 1, 2, 1, 1)
        self.paramLayout.addWidget(QLabel("Pixel Size : "), 2, 0, 1, 1)
        self.paramLayout.addWidget(self.pixsSpnBx, 2, 1, 1, 1)
        self.paramLayout.addWidget(QLabel("mm"), 2, 2, 1, 1)

        self.mainLayout.addWidget(self.calImageGrpChkBox)
        self.mainLayout.addWidget(self.calImageGrp)
        self.mainLayout.addWidget(self.paramGrpChkBx)
        self.mainLayout.addWidget(self.paramGrp)
        # self.mainLayout.addWidget(self.fixedCenter)
        
        # Add label for center section
        centerLabel = QLabel("Calibrated Center (Original Coords):")
        self.mainLayout.addWidget(centerLabel)
        
        # Add warning for quadrant folded images
        if self.quadrant_folded:
            qfWarningLabel = QLabel("Note: Center is fixed at geometric center for quadrant folded images")
            qfWarningLabel.setStyleSheet("QLabel { color: orange; font-style: italic; }")
            self.mainLayout.addWidget(qfWarningLabel)
        
        self.mainLayout.addWidget(self.centerX)
        self.mainLayout.addWidget(self.centerY)
        self.mainLayout.addWidget(self.manDetector)
        self.mainLayout.addWidget(self.detectorChoice)
        self.mainLayout.addWidget(self.misSettingChkBx)
        self.mainLayout.addWidget(self.buttons)
        self.mainLayout.setAlignment(Qt.AlignCenter)
        self.mainLayout.setAlignment(self.buttons, Qt.AlignCenter)

        self.calImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.calImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)

        #self.resize(5, 1)

    def setConnection(self):
        """
        Set the connections on the buttons.
        """
        self.browseButton.clicked.connect(self.browseClicked)
        self.unsetButton.clicked.connect(self.unsetCalImg)
        self.paramGrpChkBx.clicked.connect(self.paramChecked)
        #self.paramGrp.clicked.connect(self.paramChecked)
        self.calImageGrpChkBox.clicked.connect(self.calImageChecked)
        #self.calImageGrp.clicked.connect(self.calImageChecked)
        self.minInt.valueChanged.connect(self.updateImage)
        self.maxInt.valueChanged.connect(self.updateImage)
        # self.fixedCenter.stateChanged.connect(self.centerFixed)
        self.manDetector.stateChanged.connect(self.detectorClicked)
        self.misSettingChkBx.stateChanged.connect(self.correctMisSetting)

        self.silverBehenate.editingFinished.connect(lambda: self.settingChanged('silverB', self.silverBehenate))
        self.lambdaSpnBx.editingFinished.connect(lambda: self.settingChanged('lambda', self.lambdaSpnBx))
        self.sddSpnBx.editingFinished.connect(lambda: self.settingChanged('sdd', self.sddSpnBx))
        self.pixsSpnBx.editingFinished.connect(lambda: self.settingChanged('pixS', self.pixsSpnBx))
        self.centerX.editingFinished.connect(lambda: self.settingChanged('centerX', self.centerX))
        self.centerY.editingFinished.connect(lambda: self.settingChanged('centerY', self.centerY))
        self.detectorChoice.currentIndexChanged.connect(lambda: self.settingChanged('detector', self.detectorChoice))

    def paramChecked(self):
        """
        Uncheck the other button and decalibrate when param is checked.
        """
        self.paramGrp.setEnabled(self.paramGrpChkBx.isChecked())
        self.calImageGrpChkBox.setChecked(False)
        self.calImageGrp.setEnabled(False)

        self.decalibrate()

    def calImageChecked(self):
        """
        Uncheck the other button and decalibrate when calImage is checked.
        """
        self.calImageGrp.setEnabled(self.calImageGrpChkBox.isChecked())
        self.paramGrpChkBx.setChecked(False)
        self.paramGrp.setEnabled(False)
        self.decalibrate()

    def settingChanged(self, name, obj):
        """
        Change the log when the settings are changed.
        """
        self.log_changes(name, obj)

    def decalibrate(self):
        """
        Remove the calibration displayed in the window.
        """
        self.calSettings = None
        QApplication.processEvents()
        if self.calImageGrpChkBox.isChecked() and exists(self.calFile):
            self.calibrate()

    def setAllToolTips(self):
        """
        Set the tool tips.
        """
        self.manualCal.setToolTip(
            "Calibrate the image manually by clicking at least 5 points for circle fitting, and setting appropriate Silver Behenate."
            "\n The accuracy of fitting function will depend on number of points.")

    def manualCalClicked(self):
        """
        Run the manual calibration option, allowing the user to drop points on the image to calibrate.
        """
        if self.manualCal.isChecked():
            self.manualCal.setText("Done")
            self.maxInt.setEnabled(False) #Stop the user from breaking the application
            self.minInt.setEnabled(False) #Stop the user from breaking the application
            self.manualCalPoints = []
            self.doubleZoomMode = True
            self.ax = self.calImgFigure.add_subplot(111)
            self.ax.cla()
            _, img = self.getImage()
            self.ax.imshow(img)
            self.ax.set_xlim((0, img.shape[1]))
            self.ax.set_ylim((0, img.shape[0]))
            # self.ax.invert_yaxis()
            self.ax2 = self.calImgFigure.add_subplot(337)
            self.ax2.cla()
            self.ax2.imshow(img)
            self.ax2.set_yticklabels([])
            self.ax2.set_xticklabels([])
            self.ax2.invert_yaxis()
            self.calImgCanvas.draw_idle()
        else:
            if len(self.manualCalPoints) < 5:
                errMsg = QMessageBox()
                errMsg.setText('Manual Calibration Error')
                errMsg.setInformativeText('Please select at least 5 points')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
                self.manualCal.setChecked(True)
            else:
                self.recalculate = True
                self.manualCal.setText("Set calibration by points selections")
                
                # Store original user points before calibration
                self._last_user_points = list(self.manualCalPoints)
                
                self.calibrate()
                
                # Store refined points for visualization
                if hasattr(self, 'calSettings') and self.calSettings is not None:
                    self.refined_points = getattr(self, '_last_refined_points', None)
                
                self.manualCalPoints = None
                # self.fixedCenter.setChecked(False)

                #Let them change this again:
                self.maxInt.setEnabled(True)
                self.minInt.setEnabled(True)

    def imgClicked(self, event):
        """
        Process the event of the image clicked.
        """
        if event.xdata is None or event.ydata is None or self.manualCalPoints is None:
            return
        x = int(round(event.xdata))
        y = int(round(event.ydata))
        if self.doubleZoomMode:
            # If x, y is inside figure and image is clicked for first time in double zoom mode
            self.doubleZoomPt = (x, y)
            if not self.dontShowAgainDoubleZoomMessageResult:
                msg = QMessageBox()
                msg.setInformativeText(
                    "Please click on zoomed window on the bottom left")
                dontShowAgainDoubleZoomMessage = QCheckBox("Do not show this message again")
                msg.setStandardButtons(QMessageBox.Ok)
                msg.setWindowTitle("Double Zoom Guide")
                msg.setStyleSheet("QLabel{min-width: 500px;}")
                msg.setCheckBox(dontShowAgainDoubleZoomMessage)
                msg.exec_()
                self.dontShowAgainDoubleZoomMessageResult = dontShowAgainDoubleZoomMessage.isChecked()
            self.doubleZoomMode = False
        else:
            self.doubleZoomMode = True
            self.manualCalPoints.append((x, y))
            self.ax.plot(x, y,'ro')
            _, disp_img = self.getImage()
            self.ax.set_xlim((0, disp_img.shape[1]))
            self.ax.set_ylim((0, disp_img.shape[0]))
            #self.ax.invert_yaxis()
            self.calImgCanvas.draw_idle()

    def imgOnMotion(self, event):
        """
        Process the event of the mouse moving over the image.
        """
        if event.xdata is None or event.ydata is None or self.manualCalPoints is None:
            return

        x = event.xdata
        y = event.ydata

        axis_size = 2
        zoom_size = 40
        if not self.doubleZoomMode:
            if len(self.ax2.lines) > 0:
                for i in range(len(self.ax2.lines)-1,-1,-1):
                    self.ax2.lines[i].remove()
            self.ax2.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
            self.ax2.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
        else:
            self.ax2.set_xlim((x - zoom_size, x + zoom_size))
            self.ax2.set_ylim((y - zoom_size, y + zoom_size))
            #self.ax2.invert_yaxis()
        self.calImgCanvas.draw_idle()

    def unsetCalImg(self):
        """
        Unset the calibration displayed in the window.
        """
        self.pathText.setText("")
        self.calFile = ""
        self.manualCal.setEnabled(False)
        self.calSettings = None
        self.updateImage()

    def browseClicked(self):
        """
        Opens a finder window to choose a file and process calibration on it once selected.
        """
        file_name = getAFile()
        if file_name != "" and exists(str(file_name)):
            self.cal_img = None
            self.calFile = str(file_name)
            self.pathText.setText(str(file_name))
            self.manualCal.setEnabled(True)
            self.calibrate()

    def loadSettings(self):
        """
        Load the 'settings' folder containing the previous calibration made.
        """
        cache_path = fullPath(self.dir_path, "settings")
        cache_file = fullPath(cache_path, "calibration.info")
        if exists(cache_path) and isfile(cache_file):
            cache = pickle.load(open(cache_file, "rb"))
            if cache is not None and "version" in cache :
                return cache
        return None

    def keyPressEvent(self, event):
        """
        Manage key press event on keyboard.
        """
        key = event.key()

        if key == Qt.Key_Enter:
            return

    def saveSettings(self):
        """
        Save the calibration settings made in the window in the 'settings' folder.
        """
        cache_path = fullPath(self.dir_path, "settings")
        createFolder(cache_path)
        cache_file = fullPath(cache_path, "calibration.info")
        if self.calSettings is None:
            self.calSettings = {}

        if self.paramGrpChkBx.isChecked():
            self.calSettings = {
                "lambda": self.lambdaSpnBx.value(),
                "pixel_size": self.pixsSpnBx.value(),
                "sdd": self.sddSpnBx.value(),
                "scale": (self.lambdaSpnBx.value() * self.sddSpnBx.value()) / self.pixsSpnBx.value() ,
                "type": "cont"
            }
        elif self.calImageGrpChkBox.isChecked():
            self.calSettings["silverB"] = self.silverBehenate.value()
            self.calSettings["type"] = "img"

        # For quadrant folded images, don't save center (always use geometric center)
        if not self.quadrant_folded:
            self.calSettings["center"] = [self.centerX.value(), self.centerY.value()]

        if self.manDetector.isChecked():
            self.calSettings["detector"] = self.detectorChoice.currentText()

        cache = {
            "path": self.pathText.text(),
            "settings": self.calSettings,
            "version": self.version
        }
        pickle.dump(cache, open(cache_file, "wb"))

    def refine_point_on_ring(self, img, center, user_point, search_radius=20):
        """
        Refine a user-clicked point by finding the local intensity maximum along the radial direction.
        
        Args:
            img: calibration image (numpy array)
            center: fitted center coordinates [x, y]
            user_point: user-clicked point [x, y]
            search_radius: search range around the user point (pixels)
        
        Returns:
            refined_point: [x, y] coordinates of the intensity maximum
        """
        # Calculate radial direction from center to user point
        dx = user_point[0] - center[0]
        dy = user_point[1] - center[1]
        distance = np.sqrt(dx**2 + dy**2)
        
        if distance == 0:
            return user_point
        
        # Normalize direction vector
        dir_x = dx / distance
        dir_y = dy / distance
        
        # Sample along the radial direction
        start_dist = distance - search_radius
        end_dist = distance + search_radius
        num_samples = 2 * search_radius + 1
        
        radial_distances = np.linspace(start_dist, end_dist, num_samples)
        intensities = []
        sample_points = []
        
        for dist in radial_distances:
            # Calculate point coordinates along the radial direction
            x = center[0] + dir_x * dist
            y = center[1] + dir_y * dist
            
            # Check bounds
            if 0 <= x < img.shape[1] and 0 <= y < img.shape[0]:
                # Bilinear interpolation for sub-pixel accuracy
                x0, y0 = int(np.floor(x)), int(np.floor(y))
                x1, y1 = min(x0 + 1, img.shape[1] - 1), min(y0 + 1, img.shape[0] - 1)
                
                fx, fy = x - x0, y - y0
                
                intensity = (img[y0, x0] * (1 - fx) * (1 - fy) +
                            img[y0, x1] * fx * (1 - fy) +
                            img[y1, x0] * (1 - fx) * fy +
                            img[y1, x1] * fx * fy)
                
                intensities.append(intensity)
                sample_points.append([x, y])
            else:
                intensities.append(0)
                sample_points.append([x, y])
        
        # Find the maximum intensity point
        if len(intensities) > 0:
            max_idx = np.argmax(intensities)
            refined_point = sample_points[max_idx]
            return refined_point
        else:
            return user_point

    def refine_calibration_points(self, img, initial_center, user_points, search_radius=20):
        """
        Refine all user-clicked points by finding intensity maxima along radial directions.
        
        Args:
            img: calibration image
            initial_center: initial fitted center
            user_points: list of user-clicked points
            search_radius: search range for each point
        
        Returns:
            refined_points: list of refined points with maximum intensity
        """
        refined_points = []
        
        for pt in user_points:
            refined_pt = self.refine_point_on_ring(img, initial_center, pt, search_radius)
            refined_points.append(refined_pt)
        
        # Store for visualization
        self._last_refined_points = refined_points
        
        return refined_points

    def getImage(self):
        """
        Get the image and returns a copy of the calibration image and the image displayed.
        """
        if self.cal_img is None:
            self.cal_img = fabio.open(str(self.calFile)).data
            self.cal_img = ifHdfReadConvertless(str(self.calFile), self.cal_img)

        if self.minInt.value() == 0 and self.maxInt.value() == 0:
            self.uiUpdating = True
            min_val = self.cal_img.min()
            max_val = self.cal_img.max()
            self.minIntLabel.setText("Min intensity (" + str(np.round(min_val, 2)) + ") : ")
            self.maxIntLabel.setText("Max intensity (" + str(np.round(max_val, 2)) + ") : ")
            self.minInt.setRange(min_val, max_val)
            self.maxInt.setRange(min_val, max_val)
            self.minInt.setValue(min_val)
            self.maxInt.setValue(max_val*0.2)
            self.uiUpdating = False
        self.disp_img = getBGR(get8bitImage(self.cal_img, max=self.maxInt.value(), min=self.minInt.value()))

        return copy.copy(self.cal_img), copy.copy(self.disp_img)

    def calibrate(self):
        """
        Calibrate on the image selected.
        Note: For quadrant_folded images, center will be calculated (for display purposes)
        but will not be saved or used (geometric center is always used instead).
        """
        if self.manualCalPoints is not None:
            # Step 1: Initial fit using user-clicked points
            user_points_array = np.array(self.manualCalPoints, dtype=np.float32)
            initial_center, initial_radius, _ = cv2.fitEllipse(user_points_array)
            initial_center = [initial_center[0], initial_center[1]]
            
            print(f"Initial fit - Center: {initial_center}, Radius: {initial_radius}")
            
            # Step 2: Refine each point by finding intensity maximum along radial direction
            imgcopy, _ = self.getImage()
            refined_points = self.refine_calibration_points(
                imgcopy, 
                initial_center, 
                self.manualCalPoints, 
                search_radius=30  # Search ±30 pixels along radial direction
            )
            
            print(f"Refined {len(refined_points)} points")
            
            # Step 3: Refit circle using refined points
            refined_points_array = np.array(refined_points, dtype=np.float32)
            final_center, final_radius, _ = cv2.fitEllipse(refined_points_array)
            
            print(f"Final fit - Center: {final_center}, Radius: {final_radius}")
            
            self.calSettings = {
                "center": [round(final_center[0], 4), round(final_center[1], 4)],
                "radius": round((final_radius[0] + final_radius[1]) / 4.),
                "scale": round((final_radius[0] + final_radius[1]) / 4.) * self.silverBehenate.value()
            }
            
            # Optional: Visualize refined points on the image
            if hasattr(self, 'ax') and self.ax is not None:
                # Plot refined points in blue
                for rpt in refined_points:
                    self.ax.plot(rpt[0], rpt[1], 'bo', markersize=8)
        else:
            imgcopy, _ = self.getImage()
            imgcopy[imgcopy <= 0.0] = 0
            imgcopy = get16bitImage(imgcopy)
            th = getThreshold(imgcopy, percent=0.15)
            thresh = np.zeros((imgcopy.shape[0], imgcopy.shape[1]), np.uint8)
            thresh[imgcopy >= th] = 255
            thresh = cv2.medianBlur(thresh, 7)

            morph_size = int(max(imgcopy.shape[0], imgcopy.shape[1]) / 400)
            kernel = cv2.getStructuringElement(2, (morph_size * 20, morph_size * 20)) # 2 = cv2.MORPH_ELLIPSE
            img = cv2.morphologyEx(thresh, 1, kernel) # 1 = cv2.MORPH_DILATE
            kernel = cv2.getStructuringElement(2, (morph_size * 15, morph_size * 15)) # 2 = cv2.MORPH_ELLIPSE
            img = cv2.morphologyEx(img, 0, kernel) # 0 = cv2.MORPH_ERODE
            contours = getContours(img, 3, 2) # 3 = cv2.RETR_TREE, 2 = cv2.CHAIN_APPROX_SIMPLE
            cali_radius = 0.0

            if len(contours) > 1:
                sorted_contours = sorted(contours, key=cv2.contourArea, reverse=True)
                size_treshold = morph_size * 100
                for i in range(len(sorted_contours) - 1):
                    cnt1 = sorted_contours[i]
                    cnt2 = sorted_contours[i + 1]
                    center1, radius1, _ = cv2.fitEllipse(cnt1)
                    center2, radius2, _ = cv2.fitEllipse(cnt2)

                    if abs(radius1[0] - radius2[0]) + abs(radius1[1] - radius2[1]) > size_treshold:
                        continue

                    fixcenter = (
                        round((center1[0] + center2[0]) / 2., 4), round((center1[1] + center2[1]) / 2., 4))
                    cali_radius = np.mean([radius1[0], radius1[1], radius2[0], radius2[1]])
                    break

            if cali_radius == 0.0:
                if len(contours) > 0:
                    largest_contour = max(contours, key=cv2.contourArea)
                    center, radius, _ = cv2.fitEllipse(largest_contour)
                    fixcenter = (round(center[0], 4), round(center[1], 4))
                    cali_radius = np.mean([int(np.round(radius[0])), int(np.round(radius[1]))])
                else:
                    errMsg = QMessageBox()
                    errMsg.setText('Calibration Error')
                    errMsg.setInformativeText(
                        'Circle could not be found. Please select 4 points on the circle.')
                    errMsg.setStandardButtons(QMessageBox.Ok)
                    errMsg.setIcon(QMessageBox.Warning)
                    errMsg.exec_()
                    return

            # cali_radius = int(np.round(cali_radius / 2.))
            cali_radius = np.round(cali_radius / 2.)

            self.calSettings = {
                "center": [fixcenter[0], fixcenter[1]],
                "radius": cali_radius,
                "scale": cali_radius * self.silverBehenate.value()
            }


        self.updateImage()

    def updateImage(self):
        """
        Update the image displayed with the calibration circle and center displayed on it.
        """
        if self.uiUpdating:
            return

        #self.calImgFigure.clf()
        if self.calSettings is not None:
            if "center" in self.calSettings:
                center = self.calSettings["center"]
                # self.fixedCenter.setChecked(True)
                self.centerX.setValue(center[0])
                self.centerY.setValue(center[1])
            if self.calImageGrpChkBox.isChecked():
                self.resize(500, 800)
                self.calImgCanvas.setHidden(False)
                self.minIntLabel.setHidden(False)
                self.minInt.setHidden(False)
                self.maxIntLabel.setHidden(False)
                self.maxInt.setHidden(False)
                center = self.calSettings["center"]
                radius = self.calSettings["radius"]
                _, disp_img = self.getImage()
                self.calImgFigure.clf()
                ax = self.calImgFigure.add_subplot(111)
                ax.imshow(disp_img)
                ax.plot([center[0]], [center[1]], 'ro')
                ax.add_patch(
                    patches.Circle(center, radius, linewidth=2, edgecolor='r', facecolor='none', linestyle='dotted'))
                ax.set_xlim((0, disp_img.shape[1]))
                ax.set_ylim((0, disp_img.shape[0]))
                
                # If we have refined calibration points from manual calibration, show them
                if hasattr(self, 'refined_points') and self.refined_points is not None:
                    # Get the original user points if we still have them stored
                    if hasattr(self, '_last_user_points') and self._last_user_points is not None:
                        # Show user-clicked points in red with 'x' markers
                        user_pts_x = [pt[0] for pt in self._last_user_points]
                        user_pts_y = [pt[1] for pt in self._last_user_points]
                        ax.plot(user_pts_x, user_pts_y, 'rx', markersize=8, markeredgewidth=2, label='User points')
                    
                    # Show refined points in blue
                    refined_pts_x = [pt[0] for pt in self.refined_points]
                    refined_pts_y = [pt[1] for pt in self.refined_points]
                    ax.plot(refined_pts_x, refined_pts_y, 'bo', markersize=8, label='Refined points')
                    
                    # Draw lines from center to refined points
                    for pt in self.refined_points:
                        ax.plot([center[0], pt[0]], [center[1], pt[1]], 'g--', linewidth=0.5, alpha=0.5)
                    
                    # Add legend
                    ax.legend(loc='upper right', fontsize=8)

                ax.set_title("center:" + str(center) + " radius:" + str(radius))
                #ax.invert_yaxis()
                self.calImgFigure.tight_layout()
                self.calImgCanvas.draw()
        else:
            self.resize(500, 1)
            self.calImgCanvas.setHidden(True)


    def centerFixed(self):
        """
        React to the fixed center button.
        """

        """
        if self.fixedCenter.isChecked():
            self.centerX.setEnabled(True)
            self.centerY.setEnabled(True)
        else:
            self.centerX.setEnabled(False)
            self.centerY.setEnabled(False)
        """
        pass

    def detectorClicked(self):
        """
        React to the Manual Select Detector checkbox.
        """
        if not self.manDetector.isChecked() and 'detector' in self.calSettings:
            self.calSettings.pop('detector')
        self.detectorChoice.setEnabled(self.manDetector.isChecked())

    def correctMisSetting(self):
        """
        TODO.
        """
        if self.manDetector.isChecked():
            angles = getMisSettingAngles(self.cal_img, find_detector(self.cal_img), [self.centerX, self.centerY])
            print(angles)
        else:
            pass

    def okClicked(self):
        """
        React to the OK button.
        """
        self.saveSettings()
        print(self.calSettings)
        self.accept()

    def getValues(self):
        """
        Return the calibration values.
        """
        return self.calSettings

    def init_logging(self):
        """
        Initialize the logging variables.
        """
        for objName in self.editableVars:
            self.editableVars[objName] = self.findChild(QAbstractSpinBox, objName).value()

    def log_changes(self, name, obj):
        """
        Change the logging variables.
        """
        if name == 'detector':
            newValue = obj.currentText()
        else:
            newValue = obj.value()
        varName = obj.objectName()
        if self.editableVars[varName] == newValue:
            return
        self.logMsgs.append(f'{name}Changed: {self.editableVars[varName]} -> {newValue}')
        self.editableVars[varName] = newValue
