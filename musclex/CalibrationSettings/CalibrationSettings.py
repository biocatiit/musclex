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
from pyFAI.detectors import Detector
from ..ui.pyqt_utils import *
from ..utils.file_manager import fullPath, createFolder, ifHdfReadConvertless
from ..utils.image_processor import *

class CalibrationSettings(QDialog):
    """
    The CalibrationSettings object is a window and functions helping the software to calibrate the images processed and improve the results found.
    """
    def __init__(self, dir_path,center=None):
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
        cache = self.loadSettings()

        self.recalculate = False #This is so that if only the center coords are changed, QF
        #Does not recalculate the image.

        if cache is not None:
            self.calFile = cache["path"]
            self.calSettings = cache["settings"]

            print('cache',cache)
            print('calsettings:',self.calSettings)
        else:
            self.calFile = fullPath(dir_path, "calibration.tif")
            self.calSettings = {}

        if center is not None:
            self.calSettings["center"]=center

        # self.setStyleSheet(getStyleSheet())
        self.initUI()
        self.setConnection()
        self.setAllToolTips()

        if exists(self.calFile) and self.calImageGrpChkBox.isChecked():
            if self.calSettings is None:
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


        self.fixedCenter = QCheckBox("Fixed Center")
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

        self.fixedCenter.setChecked(False)
        #self.centerX.setEnabled(False)
        #self.centerY.setEnabled(False)
        self.centerX.setObjectName('centerX')
        self.editableVars[self.centerX.objectName()] = None
        self.centerY.setObjectName('centerY')
        self.editableVars[self.centerY.objectName()] = None

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
        self.mainLayout.addWidget(self.fixedCenter)
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
        self.fixedCenter.stateChanged.connect(self.centerFixed)
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
            self.ax.invert_yaxis()
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
                self.calibrate()
                self.manualCalPoints = None
                self.fixedCenter.setChecked(False)

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
        self.calSettings = None
        self.updateImage()

    def browseClicked(self):
        """
        Opens a finder window to choose a file and process calibration on it once selected.
        """
        file_name = getAFile()
        if file_name != "":
            self.cal_img = None
            self.calFile = str(file_name)
            self.pathText.setText(str(file_name))
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

        if self.fixedCenter.isChecked():
            self.calSettings["center"] = [self.centerX.value(), self.centerY.value()]
        
        if self.manDetector.isChecked():
            self.calSettings["detector"] = self.detectorChoice.currentText()

        cache = {
            "path": self.pathText.text(),
            "settings": self.calSettings,
            "version": self.version
        }
        pickle.dump(cache, open(cache_file, "wb"))

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
        """
        if self.manualCalPoints is not None:
            center, radius, _ = cv2.fitEllipse(np.array(self.manualCalPoints))
            self.calSettings = {
                "center": [round(center[0], 4), round(center[1],4)],
                "radius": round((radius[0] + radius[1]) / 4.),
                "scale": round((radius[0] + radius[1]) / 4.) * self.silverBehenate.value()
            }
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
                self.fixedCenter.setChecked(True)
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

                ax.set_title("center:" + str(center) + " radius:" + str(radius))
                #ax.invert_yaxis()
                self.calImgFigure.tight_layout()
        else:
            self.resize(500, 1)
            self.calImgCanvas.setHidden(True)

        self.calImgCanvas.draw()

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
