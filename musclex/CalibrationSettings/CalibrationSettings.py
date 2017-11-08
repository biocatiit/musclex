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
import pickle
import fabio
from ..ui.pyqt_utils import *
import matplotlib.patches as patches
from os.path import isfile, exists
from os import makedirs
from ..utils.file_manager import fullPath, getStyleSheet, createFolder
from ..utils.image_processor import *
import musclex

class CalibrationSettings(QDialog):
    def __init__(self, dir_path):
        super(CalibrationSettings, self).__init__(None)
        self.setWindowTitle("Calibration Settings")
        self.dir_path = str(dir_path)
        self.manualCalPoints = None
        self.cal_img = None
        self.disp_img = None
        self.version = musclex.__version__
        cache = self.loadSettings()

        if cache is not None:
            self.calFile = cache["path"]
            self.calSettings = cache["settings"]
        else:
            self.calFile = fullPath(dir_path, "calibration.tif")
            self.calSettings = None

        self.setStyleSheet(getStyleSheet())
        self.initUI()
        self.setConnection()
        self.setAllToolTips()

        if exists(self.calFile) and self.calImageGrp.isChecked():
            if self.calSettings is None:
                self.calibrate()
            else:
                self.updateImage()

    def initUI(self):
        silverb = 5.83803
        init_lambda = .1033
        init_sdd = 2500
        init_pix_size = 0.172
        type = "img"
        center = None
        if self.calSettings is not None:
            type = self.calSettings["type"]
            if type == "cont":
                init_lambda = self.calSettings["lambda"]
                init_sdd = self.calSettings["sdd"]
                init_pix_size = self.calSettings["pixel_size"]
            else:
                silverb = self.calSettings["silverB"]

            if self.calSettings.has_key("center"):
                center = self.calSettings["center"]
        self.mainLayout = QVBoxLayout(self)

        self.pathText = QLineEdit()
        if exists(self.calFile):
            self.pathText.setText(self.calFile)
        self.pathText.setEnabled(False)
        self.browseButton = QPushButton("Browse")
        self.browseButton.clicked.connect(self.browseClicked)
        self.unsetButton = QPushButton("Unset")
        self.unsetButton.clicked.connect(self.unsetCalImg)
        self.calImgFigure = plt.figure(facecolor='#606060')
        self.calImgCanvas = FigureCanvas(self.calImgFigure)
        self.calImgCanvas.setHidden(True)
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
        self.bottons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)
        self.bottons.setFixedWidth(100)
        grpbox_ss = "QGroupBox::title { background-color: #323232 ; subcontrol-origin: margin; subcontrol-position: top left; padding: 0 3px; }"
        self.calImageGrp = QGroupBox("Setting by Calibration Image")
        self.calImageGrp.setCheckable(True)
        self.calImageGrp.setStyleSheet(grpbox_ss)
        self.calImageGrp.setChecked(type == "img")
        self.calImageLayout = QGridLayout(self.calImageGrp)
        l1 = QLabel("Calibration File :")
        l2 = QLabel("Silver Behenate :")
        self.calImageLayout.addWidget(l1, 0, 0, 1, 1)
        self.calImageLayout.addWidget(self.pathText, 0, 1, 1, 1)
        self.calImageLayout.addWidget(self.browseButton, 0, 2, 1, 1)
        self.calImageLayout.addWidget(self.unsetButton, 0, 3, 1, 1)
        self.calImageLayout.addWidget(self.calImgCanvas, 1, 0, 1, 4)
        self.calImageLayout.addWidget(self.manualCal, 2, 0, 1, 2)
        self.calImageLayout.addWidget(l2, 2, 2, 1, 1)
        self.calImageLayout.addWidget(self.silverBehenate, 2, 3, 1, 1)
        self.calImageLayout.setAlignment(l2, Qt.AlignRight)
        # self.calImageLayout.setColumnStretch(1,2)
        self.calImageLayout.setRowStretch(1, 2)

        self.paramGrp = QGroupBox("Setting by Parameters")
        self.paramGrp.setCheckable(True)
        self.paramGrp.setStyleSheet(grpbox_ss)
        self.paramGrp.setChecked(type == "cont")
        self.paramLayout = QGridLayout(self.paramGrp)

        self.lambdaSpnBx = QDoubleSpinBox()
        self.lambdaSpnBx.setDecimals(5)
        self.lambdaSpnBx.setRange(0.00001, 5.)
        self.lambdaSpnBx.setValue(init_lambda)

        self.sddSpnBx = QDoubleSpinBox()
        self.sddSpnBx.setDecimals(2)
        self.sddSpnBx.setRange(0., 5000.)
        self.sddSpnBx.setValue(init_sdd)

        self.pixsSpnBx = QDoubleSpinBox()
        self.pixsSpnBx.setDecimals(5)
        self.pixsSpnBx.setRange(0.00001, 5.)
        self.pixsSpnBx.setValue(init_pix_size)

        self.fixedCenter = QCheckBox("Fixed Center")
        self.centerX = QSpinBox()
        self.centerX.setPrefix("X:")
        self.centerX.setRange(0, 2047)
        self.centerY = QSpinBox()
        self.centerY.setPrefix("Y:")
        self.centerY.setRange(0, 2047)

        if center is not None:
            self.fixedCenter.setChecked(True)
            self.centerX.setValue(center[0])
            self.centerY.setValue(center[1])
        else:
            self.fixedCenter.setChecked(False)
            self.centerX.setValue(1000)
            self.centerY.setValue(1000)

        self.paramLayout.addWidget(QLabel("Lambda : "), 0, 0, 1, 1)
        self.paramLayout.addWidget(self.lambdaSpnBx, 0, 1, 1, 1)
        self.paramLayout.addWidget(QLabel("nm"), 0, 2, 1, 1)
        self.paramLayout.addWidget(QLabel("S<sub>dd</sub> : "), 1, 0, 1, 1)
        self.paramLayout.addWidget(self.sddSpnBx, 1, 1, 1, 1)
        self.paramLayout.addWidget(QLabel("mm"), 1, 2, 1, 1)
        self.paramLayout.addWidget(QLabel("Pixel Size : "), 2, 0, 1, 1)
        self.paramLayout.addWidget(self.pixsSpnBx, 2, 1, 1, 1)
        self.paramLayout.addWidget(QLabel("mm"), 2, 2, 1, 1)
        self.paramLayout.addWidget(self.fixedCenter, 3, 0, 1, 1)
        self.paramLayout.addWidget(self.centerX, 3, 1, 1, 1)
        self.paramLayout.addWidget(self.centerY, 3, 2, 1, 1)

        self.mainLayout.addWidget(self.calImageGrp)
        self.mainLayout.addWidget(self.paramGrp)
        self.mainLayout.addWidget(self.bottons)
        self.mainLayout.setAlignment(Qt.AlignCenter)
        self.mainLayout.setAlignment(self.bottons, Qt.AlignCenter)

        self.calImgFigure.canvas.mpl_connect('button_press_event', self.imgClicked)
        self.calImgFigure.canvas.mpl_connect('motion_notify_event', self.imgOnMotion)

        self.resize(5, 1)

    def setConnection(self):
        self.paramGrp.clicked.connect(self.paramChecked)
        self.calImageGrp.clicked.connect(self.calImageChecked)

    def paramChecked(self):
        self.calImageGrp.setChecked(not self.paramGrp.isChecked())
        self.decalibrate()

    def calImageChecked(self):
        self.paramGrp.setChecked(not self.calImageGrp.isChecked())
        self.decalibrate()

    def decalibrate(self):
        self.calSettings = None
        QApplication.processEvents()
        if self.calImageGrp.isChecked() and exists(self.calFile):
            self.calibrate()

    def setAllToolTips(self):
        self.manualCal.setToolTip(
            "Calibrate the image manually by clicking at least 5 points for circle fitting, and setting appropriate Silver Behenate."
            "\n The accuracy of fitting function will depend on number of points.")

    def manualCalClicked(self):
        if self.manualCal.isChecked():
            self.manualCal.setText("Done")
            self.manualCalPoints = []
            ax = self.calImgFigure.add_subplot(111)
            ax.cla()
            _, img = self.getImage()
            ax.imshow(img)
            ax.invert_yaxis()
            ax2 = self.calImgFigure.add_subplot(337)
            ax2.cla()
            ax2.imshow(img)
            ax2.set_yticklabels([])
            ax2.set_xticklabels([])
            ax.invert_yaxis()
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
                self.manualCal.setText("Set calibration by points selections")
                self.calibrate()
                self.manualCalPoints = None

    def imgClicked(self, event):
        if event.xdata is None or event.ydata is None or self.manualCalPoints is None:
            return
        x = int(round(event.xdata))
        y = int(round(event.ydata))
        self.manualCalPoints.append((x, y))
        ax = self.calImgFigure.add_subplot(111)
        ax.plot(x, y, 'ro')
        _, disp_img = self.getImage()
        ax.set_xlim((0, disp_img.shape[1]))
        ax.set_ylim((0, disp_img.shape[0]))
        ax.invert_yaxis()
        self.calImgCanvas.draw_idle()

    def imgOnMotion(self, event):
        if event.xdata is None or event.ydata is None or self.manualCalPoints is None:
            return

        x = event.xdata
        y = event.ydata
        ax = self.calImgFigure.add_subplot(337)
        del ax.lines
        ax.lines = []
        axis_size = 2
        ax.plot((x - axis_size, x + axis_size), (y - axis_size, y + axis_size), color='r')
        ax.plot((x - axis_size, x + axis_size), (y + axis_size, y - axis_size), color='r')
        zoom_size = 30
        ax.set_xlim((x - zoom_size, x + zoom_size))
        ax.set_ylim((y - zoom_size, y + zoom_size))
        ax.invert_yaxis()
        self.calImgCanvas.draw_idle()

    def unsetCalImg(self):
        self.pathText.setText("")
        self.calFile = ""
        self.calSettings = None
        self.updateImage()

    def browseClicked(self):
        file_name = getAFile('Images (*.tif)')
        if file_name != "":
            self.cal_img = None
            self.calFile = str(file_name)
            self.pathText.setText(str(file_name))
            self.calibrate()

    def loadSettings(self):
        cache_path = fullPath(self.dir_path, "settings")
        cache_file = fullPath(cache_path, "calibration.info")
        if exists(cache_path) and isfile(cache_file):
            cache = pickle.load(open(cache_file, "rb"))
            if cache is not None and cache.has_key("version") and cache["version"] == self.version:
                return cache
        return None

    def saveSettings(self):
        cache_path = fullPath(self.dir_path, "settings")
        createFolder(cache_path)
        cache_file = fullPath(cache_path, "calibration.info")

        if self.paramGrp.isChecked():
            self.calSettings = {
                "lambda": self.lambdaSpnBx.value(),
                "pixel_size": self.pixsSpnBx.value(),
                "sdd": self.sddSpnBx.value(),
                "type": "cont"
            }
            if self.fixedCenter.isChecked():
                self.calSettings["center"] = [self.centerX.value(), self.centerY.value()]
        else:
            if self.calSettings is not None:
                self.calSettings["silverB"] = self.silverBehenate.value()
                self.calSettings["type"] = "img"

        cache = {
            "path": self.pathText.text(),
            "settings": self.calSettings,
            "version": self.version
        }

        pickle.dump(cache, open(cache_file, "wb"))

    def getImage(self):
        if self.cal_img is None:
            self.cal_img = fabio.open(str(self.calFile)).data
            th = getThreshold(self.cal_img, 0.05)
            self.disp_img = getBGR(get8bitImage(self.cal_img, max=th))
        return copy.copy(self.cal_img), copy.copy(self.disp_img)

    # def findEllipseCenter(l):
    #     a, b, c, d, f, g = l[0], l[1] / 2, l[2], l[3] / 2, l[4] / 2, l[5]
    #     num = b * b - a * c
    #     x0 = (c * d - b * f) / num
    #     y0 = (a * f - b * d) / num
    #     return x0, y0
    #
    # def findEllipseradius(l):
    #     a, b, c, d, f, g = l[0], l[1] / 2, l[2], l[3] / 2, l[4] / 2, l[5]
    #     up = 2 * (a * f * f + c * d * d + g * b * b - 2 * b * d * f - a * c * g)
    #     down1 = (b * b - a * c) * ((c - a) * np.sqrt(1 + 4 * b * b / ((a - c) * (a - c))) - (c + a))
    #     down2 = (b * b - a * c) * ((a - c) * np.sqrt(1 + 4 * b * b / ((a - c) * (a - c))) - (c + a))
    #
    #     res1 = np.sqrt(up / down1)
    #     res2 = np.sqrt(up / down2)
    #     return res1, res2

    def calibrate(self):
        if self.manualCalPoints is not None:
            (center, radius, angle) = cv2.fitEllipse(np.array(self.manualCalPoints))
            self.calSettings = {
                "center": [int(round(center[0])), int(round(center[1]))],
                "radius": int(round((radius[0] + radius[1]) / 4.))
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
                sorted_contours = sorted(contours, key=lambda c: cv2.contourArea(c), reverse=True)
                size_treshold = morph_size * 100
                for i in range(len(sorted_contours) - 1):
                    cnt1 = sorted_contours[i]
                    cnt2 = sorted_contours[i + 1]
                    (center1, radius1, angle1) = cv2.fitEllipse(cnt1)
                    (center2, radius2, angle2) = cv2.fitEllipse(cnt2)

                    if abs(radius1[0] - radius2[0]) + abs(radius1[1] - radius2[1]) > size_treshold:
                        continue

                    fixcenter = (
                        int(np.round((center1[0] + center2[0]) / 2.)), int(np.round((center1[1] + center2[1]) / 2.)))
                    cali_radius = np.mean([radius1[0], radius1[1], radius2[0], radius2[1]])
                    break

            if cali_radius == 0.0:
                if len(contours) > 0:
                    largest_contour = max(contours, key=lambda c: cv2.contourArea(c))
                    (center, radius, angle) = cv2.fitEllipse(largest_contour)
                    fixcenter = (int(np.round(center[0])), int(np.round(center[1])))
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

            cali_radius = int(np.round(cali_radius / 2.))
            self.calSettings = {
                "center": [fixcenter[0], fixcenter[1]],
                "radius": cali_radius
            }

        self.updateImage()

    def updateImage(self):
        self.calImgFigure.clf()
        if self.calSettings is not None:
            if self.calSettings.has_key("center"):
                center = self.calSettings["center"]
                self.fixedCenter.setChecked(True)
                self.centerX.setValue(center[0])
                self.centerY.setValue(center[1])
            if self.calImageGrp.isChecked():
                self.resize(500, 800)
                self.calImgCanvas.setHidden(False)
                center = self.calSettings["center"]
                radius = self.calSettings["radius"]
                _, disp_img = self.getImage()
                ax = self.calImgFigure.add_subplot(111)
                ax.cla()
                ax.imshow(disp_img)
                ax.plot([center[0]], [center[1]], 'ro')
                ax.add_patch(
                    patches.Circle(center, radius, linewidth=2, edgecolor='r', facecolor='none', linestyle='dotted'))
                ax.set_xlim((0, disp_img.shape[1]))
                ax.set_ylim((0, disp_img.shape[0]))
                ax.set_title("center:" + str(center) + " radius:" + str(radius))
                ax.invert_yaxis()
                self.calImgFigure.tight_layout()
        else:
            self.resize(500, 1)
            self.calImgCanvas.setHidden(True)

        self.calImgCanvas.draw()

    def okClicked(self):
        self.saveSettings()
        self.accept()

    def getValues(self):
        return self.calSettings
