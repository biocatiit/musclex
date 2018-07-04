from .pyqt_utils import *
from matplotlib.patches import Ellipse
from matplotlib.collections import PatchCollection
from matplotlib.colors import LogNorm, Normalize
import h5py
from ..utils.file_manager import *
from ..modules.ScanningDiffraction import *
from ..csv_manager import CP_CSVManager
import musclex
import pandas as pd
from .CPImageWindow import CPImageWindow

class HDFBrowser(QDialog):
    """
    Provide options for HDF Browser - select or create one
    """
    def __init__(self, msg, setting_path):
        """
        initial dialog
        :param msg: message which appear in dialog
        :param setting_path: path that HDF file will be saved
        """
        super(HDFBrowser, self).__init__(None)
        self.msg = msg
        self.path = setting_path
        self.status = 0
        self.hdf_file = ""
        self.initUI()
        self.setConnection()

    def initUI(self):
        self.mainLayout = QGridLayout(self)
        self.setWindowTitle("HDF Browser")
        self.label = QLabel(self.msg)

        self.browseButton = QPushButton("Browse a HDF File")
        self.createButton = QPushButton("Create a HDF File")

        self.browseGrp = QGroupBox("Browsed File")
        self.browseLayout = QVBoxLayout(self.browseGrp)
        self.browsedFile = QLabel()
        self.browseLayout.addWidget(self.browsedFile)
        self.browseGrp.setHidden(True)

        self.createGroup = QGroupBox("Create a File")
        self.createLayout = QGridLayout(self.createGroup)
        self.createGroup.setHidden(True)

        self.x_start = QDoubleSpinBox()
        self.x_start.setRange(-100000, 100000)
        self.x_start.setDecimals(5)
        self.y_start = QDoubleSpinBox()
        self.y_start.setRange(-100000, 100000)
        self.y_start.setDecimals(5)
        self.x_end = QDoubleSpinBox()
        self.x_end.setRange(-100000, 100000)
        self.x_end.setDecimals(5)
        self.y_end = QDoubleSpinBox()
        self.y_end.setRange(-100000, 100000)
        self.y_end.setDecimals(5)
        self.x_step = QDoubleSpinBox()
        self.x_step.setRange(-100000, 100000)
        self.x_step.setDecimals(5)
        self.y_step = QDoubleSpinBox()
        self.y_step.setRange(-100000, 100000)
        self.y_step.setDecimals(5)

        self.createLayout.addWidget(QLabel("Start"), 0, 1, 1, 1)
        self.createLayout.addWidget(QLabel("End"), 0, 2, 1, 1)
        self.createLayout.addWidget(QLabel("Step size"), 0, 3, 1, 1)

        self.createLayout.addWidget(QLabel("X"), 1, 0, 1, 1)
        self.createLayout.addWidget(self.x_start, 1, 1, 1, 1)
        self.createLayout.addWidget(self.x_end, 1, 2, 1, 1)
        self.createLayout.addWidget(self.x_step, 1, 3, 1, 1)
        self.createLayout.addWidget(QLabel("Y"), 2, 0, 1, 1)
        self.createLayout.addWidget(self.y_start, 2, 1, 1, 1)
        self.createLayout.addWidget(self.y_end, 2, 2, 1, 1)
        self.createLayout.addWidget(self.y_step, 2, 3, 1, 1)

        self.bottons = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel, Qt.Horizontal, self)

        self.mainLayout.addWidget(self.label, 0, 0, 1, 2)
        self.mainLayout.addWidget(self.browseButton, 1, 0, 1, 1)
        self.mainLayout.addWidget(self.createButton, 1, 1, 1, 1)
        self.mainLayout.addWidget(self.browseGrp, 2, 0, 1, 2)
        self.mainLayout.addWidget(self.createGroup, 3, 0, 1, 2)
        self.mainLayout.addWidget(self.bottons,4, 0, 1, 2)

    def setConnection(self):
        """
        Set handler for widgets
        """
        self.browseButton.clicked.connect(self.browseClicked)
        self.createButton.clicked.connect(self.createClicked)
        self.bottons.accepted.connect(self.okClicked)
        self.bottons.rejected.connect(self.reject)

    def browseClicked(self):
        """
        Handle when Browse is clicked
        """
        self.hdf_file = getAFile('HDF (*.hdf)')
        if len(self.hdf_file) > 0:
            self.status = 1
            self.browsedFile.setText(self.hdf_file)
            self.browseGrp.setHidden(False)
            self.createGroup.setHidden(True)

    def createClicked(self):
        """
        Handle when Create is clicked
        """
        self.browseGrp.setHidden(True)
        self.createGroup.setHidden(False)
        self.status = 2

    def okClicked(self):
        """
        Handle when OK is clicked
        """
        if self.status == 0:
            errMsg = QMessageBox()
            errMsg.setText('HDF File is not set')
            errMsg.setInformativeText('Please select a HDF file or create a new one')
            errMsg.setStandardButtons(QMessageBox.Ok)
            errMsg.setIcon(QMessageBox.Warning)
            errMsg.exec_()
        elif self.status == 1:
            self.accept()
        elif self.status == 2:
            # Generate HDF file
            self.hdf_file = join(self.path, 'file.hdf')
            x_start = self.x_start.value()
            y_start = self.y_start.value()
            x_end = self.x_end.value()
            y_end = self.y_end.value()
            x_step = self.x_step.value()
            y_step = self.y_step.value()

            x_nStep = int(np.round((x_end - x_start) / x_step) + 1)
            y_nStep = int(np.round((y_end - y_start) / y_step) + 1)

            data = []
            for j in range(0, y_nStep):
                y = y_start + j*y_step
                for i in range(0, x_nStep):
                    x = x_start + i*x_step
                    data.append((x, y))

            hf = h5py.File(self.hdf_file, 'w')
            data_grp = hf.create_group("data")
            data_grp.create_dataset("BL", data=data)

            hf.close()
            self.accept()

class CPBatchWindow(QMainWindow):
    def __init__(self, mainWin=None, dir_path=""):
        # QDialog.__init__(self, parent)
        QWidget.__init__(self)
        self.filePath = dir_path
        self.widgetList = []
        self.intesityRange = [0, 1, 1, 2]
        self.mainWin = mainWin
        self.colormap = 'jet'
        self.isFlipX = False
        self.isFlipY = False
        self.usingLogScale = False
        self.rotating90 = False

        ##Batch Mode Params
        self.name_dict = {}
        self.intensity_dict = {}
        self.distance_dict = {}
        self.angrange_dict = {}
        self.orientation_dict = {}
        self.fit_dict = {}
        self.fitcd_dict = {}
        self.coord_dict = {}
        self.hdf_filename = ""
        self.hdf_data = np.array([])
        self.xyIntensity = []
        self.xylim = []
        self.batchmodeImg = None
        self.batchmodeImgDetails = None
        self.batchmodeImgFilename = None
        self.updatingUI = False
        self.plots = {}

        self.vec_UV = []
        self.vec_quiver = None

        self.csvManager = CP_CSVManager(self.filePath)
        self.initUI()
        self.setConnections()
        self.processFolder(self.filePath)

    def initUI(self):
        self.setWindowTitle("Muscle X Scanning Diffraction v." + musclex.__version__)
        # self.setStyleSheet(getStyleSheet())
        self.centralWidget = QWidget(self)
        self.mainLayout = QGridLayout(self.centralWidget)
        self.setCentralWidget(self.centralWidget)

        #### IMAGE ####
        self.imgFigure = plt.figure()
        self.imgAxes = self.imgFigure.add_subplot(111)
        self.imgCanvas = FigureCanvas(self.imgFigure)
        self.img_maxInt = QDoubleSpinBox()
        self.img_maxInt.setValue(1)
        self.img_maxInt.setKeyboardTracking(False)
        self.img_minInt = QDoubleSpinBox()
        self.img_minInt.setValue(0)
        self.img_minInt.setKeyboardTracking(False)
        self.img_minIntLabel = QLabel("Min intensity")
        self.img_maxIntLabel = QLabel("Max intensity")

        self.imgFrame = QFrame()
        self.imgLayout = QGridLayout(self.imgFrame)
        self.imgLayout.addWidget(self.imgCanvas, 0, 0, 1, 4)
        self.imgLayout.addWidget(self.img_minIntLabel, 1, 0, 1, 1)
        self.imgLayout.addWidget(self.img_minInt, 1, 1, 1, 1)
        self.imgLayout.addWidget(self.img_maxIntLabel, 1, 2, 1, 1)
        self.imgLayout.addWidget(self.img_maxInt, 1, 3, 1, 1)

        #### Map ####
        self.ringSettingsGrp = QGroupBox("Selected Rings")
        self.ringSettingsLayout = QGridLayout(self.ringSettingsGrp)
        self.bestRadio = QRadioButton("Best Rings")
        self.bestRadio.setChecked(True)
        self.distanceRadio = QRadioButton("D-spacing")
        self.distanceLabel = QLabel("Distance : ")
        self.distanceSpnBx = QDoubleSpinBox()
        self.distanceSpnBx.setRange(0, 2000)
        self.distanceSpnBx.setValue(8)
        self.distanceSpnBx.setDecimals(8)
        self.bandwidthLabel = QLabel("Bandwidth : ")
        self.bandwidthSpnBx = QDoubleSpinBox()
        self.bandwidthSpnBx.setRange(0, 1000)
        self.bandwidthSpnBx.setValue(1)
        self.bandwidthSpnBx.setDecimals(8)
        self.unitChoice = QComboBox()
        self.unitChoice.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self.unitChoice.addItem("nm")
        self.unitChoice.addItem("pixel")
        self.distanceSpnBx.setEnabled(False)
        self.unitChoice.setEnabled(False)
        self.refreshButton = QPushButton("Reload")
        self.refreshButton.setSizePolicy(QSizePolicy.Preferred, QSizePolicy.Preferred)
        self.ringSettingsLayout.addWidget(self.bestRadio, 0, 0, 1, 4)
        self.ringSettingsLayout.addWidget(self.distanceRadio, 1, 0, 1, 1)
        self.ringSettingsLayout.addWidget(self.distanceLabel, 1, 1, 1, 1)
        self.ringSettingsLayout.addWidget(self.distanceSpnBx, 1, 2, 1, 1)
        self.ringSettingsLayout.addWidget(self.bandwidthLabel, 2, 1, 1, 1)
        self.ringSettingsLayout.addWidget(self.bandwidthSpnBx, 2, 2, 1, 1)
        self.ringSettingsLayout.addWidget(self.unitChoice, 1, 3, 2, 1)
        self.ringSettingsLayout.addWidget(self.refreshButton, 0, 4, 3, 1)

        self.flipX = QPushButton("Flip X")
        self.flipX.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Preferred)
        self.flipY = QPushButton("Flip Y")
        self.flipY.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Preferred)

        self.repChoice = QComboBox()
        self.repChoice.addItem("Total Intensity Map")
        self.repChoice.addItem("Ring Intensity Map")
        self.repChoice.addItem("Vector Field")
        self.repChoice.addItem("Elliptical Map")
        self.repChoice.addItem("Intensity and Rotation Map")
        self.repChoice.addItem("Intensity Map with Arrows")
        self.repChoice.addItem("Beam Map")

        self.colorChoice = QComboBox()
        colormaps = ['jet', 'inferno', 'gray', 'gnuplot', 'gnuplot2', 'hsv', 'magma', 'ocean',
                     'rainbow', 'seismic', 'summer', 'spring', 'terrain', 'winter', 'autumn',
                     'Blues', 'Greens', 'Oranges', 'Reds', 'pink']
        for c in colormaps:
            self.colorChoice.addItem(c)

        self.logScale = QCheckBox('Logarithmic Scale')
        self.rotRAngle = QCheckBox('Rotate 90 degrees')

        self.minMap = QDoubleSpinBox()
        self.minMap.setRange(0, 100)
        self.minMap.setKeyboardTracking(False)
        self.minMap.setSuffix('%')
        self.minMap.setValue(0)
        self.maxMap = QDoubleSpinBox()
        self.maxMap.setRange(0, 100)
        self.maxMap.setKeyboardTracking(False)
        self.maxMap.setValue(100)
        self.maxMap.setSuffix('%')
        self.intensityLayout = QGridLayout()
        self.intensityLayout.addWidget(QLabel("Min : "), 0, 0, 1, 1, Qt.AlignCenter)
        self.intensityLayout.addWidget(self.minMap, 0, 1, 1, 1)
        self.intensityLayout.addWidget(QLabel("Max : "), 0, 2, 1, 1, Qt.AlignCenter)
        self.intensityLayout.addWidget(self.maxMap, 0, 3, 1, 1)

        self.mapFigure = plt.figure()
        self.mapAxes = self.mapFigure.add_subplot(111)
        self.mapColorbar = None
        self.mapCanvas = FigureCanvas(self.mapFigure)

        self.saveButton = QPushButton("Save")

        self.mapFrame = QFrame()
        self.mapLayout = QGridLayout(self.mapFrame)
        self.mapLayout.addWidget(self.ringSettingsGrp, 0, 0, 2, 3)
        self.mapLayout.addWidget(self.flipX, 0, 3, 1, 1, Qt.AlignJustify)
        self.mapLayout.addWidget(self.flipY, 1, 3, 1, 1, Qt.AlignJustify)
        self.mapLayout.addWidget(QLabel("Representation : "), 2, 0, 1, 1)
        self.mapLayout.addWidget(self.repChoice, 2, 1, 1, 2)
        self.mapLayout.addWidget(self.logScale, 2, 3, 1, 1)
        self.mapLayout.addWidget(QLabel("Color map : "), 3, 0, 1, 1)
        self.mapLayout.addWidget(self.colorChoice, 3, 1, 1, 2)
        self.mapLayout.addWidget(self.rotRAngle, 3, 3, 1, 1)
        self.mapLayout.addWidget(self.mapCanvas, 4, 0, 1, 4)
        self.mapLayout.addLayout(self.intensityLayout, 5, 0, 1, 4)
        self.mapLayout.addWidget(self.saveButton, 6, 0, 1, 4)
        self.mapLayout.setRowStretch(0, 1)
        self.mapLayout.setRowStretch(1, 1)
        self.mapLayout.setRowStretch(2, 1)
        self.mapLayout.setRowStretch(3, 1)
        self.mapLayout.setRowStretch(4, 20)
        self.mapLayout.setRowStretch(5, 1)
        self.mapLayout.setRowStretch(6, 1)

        # status bar
        self.statusBar = QStatusBar()
        self.imagePathLabel = QLabel('')
        self.statusLabel = QLabel('')
        self.statusBar.addWidget(QLabel('   '))
        self.statusBar.addWidget(self.imagePathLabel)
        self.moreDetailsButton = QPushButton('More Details')
        self.moreDetailsButton.setHidden(True)
        self.rightBarLayout = QVBoxLayout()
        self.rightBarLayout.addWidget(self.statusLabel)
        self.rightBarLayout.addWidget(self.moreDetailsButton)
        self.rightBarLayout.setAlignment(Qt.AlignRight)
        self.rightBarFrame = QFrame()
        self.rightBarFrame.setLayout(self.rightBarLayout)
        self.statusBar.addPermanentWidget(self.rightBarFrame)

        self.mainLayout.addWidget(self.imgFrame, 0, 0, 1, 1)
        self.mainLayout.addWidget(self.mapFrame, 0, 1, 1, 1)
        self.mainLayout.addWidget(self.statusBar, 1, 0, 1, 2)

        self.mainLayout.setColumnStretch(0, 1)
        self.mainLayout.setColumnStretch(1, 2)
        self.mainLayout.setRowStretch(0, 20)
        self.mainLayout.setRowStretch(1, 1)

        self.show()
        self.resize(1200, 800)

    def setConnections(self):
        self.refreshButton.clicked.connect(self.processBatchmodeResults)
        self.bestRadio.toggled.connect(self.ringChoiceChanged)
        self.flipX.clicked.connect(self.flipMapX)
        self.flipY.clicked.connect(self.flipMapY)
        self.logScale.stateChanged.connect(self.updateUI)
        self.rotRAngle.stateChanged.connect(self.updateUI)
        self.img_maxInt.valueChanged.connect(self.maxIntChanged)
        self.img_minInt.valueChanged.connect(self.minIntChanged)

        self.maxMap.valueChanged.connect(self.updateUI)
        self.minMap.valueChanged.connect(self.updateUI)

        self.colorChoice.currentIndexChanged.connect(self.updateUI)
        self.repChoice.currentIndexChanged.connect(self.updateUI)
        self.mapFigure.canvas.mpl_connect('button_press_event', self.plotClicked)
        self.saveButton.clicked.connect(self.saveClicked)
        self.moreDetailsButton.clicked.connect(self.popupImageDetails)

    def ringChoiceChanged(self):
        self.distanceSpnBx.setEnabled(self.distanceRadio.isChecked())
        self.unitChoice.setEnabled(self.distanceRadio.isChecked())
        self.distanceLabel.setEnabled(self.distanceRadio.isChecked())
        self.bandwidthLabel.setEnabled(self.distanceRadio.isChecked())
        self.bandwidthSpnBx.setEnabled(self.distanceRadio.isChecked())

    def closeEvent(self, ev):
        if self.mainWin is not None:
            self.mainWin.removeWidget(self)

    def removeWidget(self, win):
        if win in self.widgetList:
            idx = self.widgetList.index(win)
            del self.widgetList[idx]

    def flipMapX(self):
        """
        Flip map in X direction
        :return:
        """
        self.isFlipX = not self.isFlipX
        if self.repChoice.currentText() == "Elliptical Map" or self.repChoice.currentText() == "Intensity and Rotation Map":
            self.updateUI()
        else:
            self.mapAxes.invert_xaxis()
            self.mapCanvas.draw()

    def flipMapY(self):
        self.isFlipY = not self.isFlipY
        if self.repChoice.currentText() == "Elliptical Map" or self.repChoice.currentText() == "Intensity and Rotation Map":
            self.updateUI()
        else:
            self.mapAxes.invert_yaxis()
            self.mapCanvas.draw()

    def saveClicked(self):
        filename = getSaveFile(join(self.filePath, 'cp_results'))
        print(str(filename)+" has been saved.")
        self.mapFigure.savefig(filename)

    def plotClicked(self, event):
        self.moreDetailsButton.setHidden(False)

        if len(self.xyIntensity) < 3 or event.xdata is None or event.ydata is None:
            return

        x = self.xyIntensity[0]
        y = self.xyIntensity[1]
        x_max = len(x)

        if x.min() <= event.xdata <= x.max()+self.xylim[0] and y.min() <= event.ydata <= y.max()+self.xylim[1]:
            indexs = list(range(0, len(x)))
            col = min(indexs, key=lambda i: abs(x[i] - event.xdata))

            if (self.repChoice.currentText() in ["Total Intensity Map","Intensity and Rotation Map","Ring Intensity Map"]) and event.xdata < x[col]:
                col -= 1

            indexs = list(range(0, len(y)))
            row = min(indexs, key=lambda i: abs(y[i] - event.ydata))
            if (self.repChoice.currentText() in ["Total Intensity Map","Intensity and Rotation Map","Ring Intensity Map"]) and event.ydata < y[row]:
                row = max(row - 1, 0)

            ind = row * x_max + col + self.init_number

            if ind in self.name_dict:
                img_detail = "Total Intensity: " + str(self.intensity_dict[ind])
                img_detail += "\nRing Intensity: " + str(self.peak_intensity_dict[ind])
                img_detail += "\nD-spacing: " + str(self.distance_dict[ind])
                img_detail += "\nOrientation angle: " + str(self.orientation_dict[ind])
                img_detail += "\nAngular range: " + str(self.angrange_dict[ind])
                filename = self.name_dict[ind]
                full_filename = fullPath(self.filePath, filename)
                self.batchmodeImgDetails = img_detail
                self.batchmodeImgFilename = str(filename)
                self.updateRightStatusBar(filename)

                if exists(full_filename):

                    img = fabio.open(full_filename).data
                    if img is not None:
                        self.batchmodeImg = img
                        self.setMinMaxIntensity(img, self.img_minInt, self.img_maxInt, self.img_minIntLabel,
                                                self.img_maxIntLabel)
                        QApplication.processEvents()
                else:
                    self.batchmodeImg = None

                self.updateImage()
            else:
                self.batchmodeImgDetails = None
                self.batchmodeImgFilename = None
                self.batchmodeImg = None


    def popupImageDetails(self):
        if self.batchmodeImg is not None:
            new_image_window = CPImageWindow(self, str(self.batchmodeImgFilename), str(self.filePath))
            self.widgetList.append(new_image_window)
        else:
            if self.batchmodeImgFilename is None:
                errMsg = QMessageBox()
                errMsg.setText('Image has not been selected')
                errMsg.setInformativeText('Please select an image from maps')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
            else:
                errMsg = QMessageBox()
                errMsg.setText('Image not found')
                errMsg.setInformativeText(str(self.batchmodeImgFilename) + ' not found.\nPlease select another image')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()

    def maxIntChanged(self):
        if self.updatingUI:
            return

        if self.img_maxInt.value() < self.img_minInt.value():
            self.img_maxInt.setValue(self.img_minInt.value() + 1)
        else:
            self.updateImage()

    def minIntChanged(self):
        if self.updatingUI:
            return

        if self.img_maxInt.value() < self.img_minInt.value():
            self.img_minInt.setValue(self.img_maxInt.value() - 1)
        else:
            self.updateImage()

    def updateImage(self):
        ax = self.imgAxes
        ax.cla()
        if self.batchmodeImgDetails is not None:
            ax.set_title(self.batchmodeImgFilename)
            ax.set_xlabel(self.batchmodeImgDetails)

            if self.batchmodeImg is not None:
                img = getBGR(get8bitImage(self.batchmodeImg, min=self.img_minInt.value(), max=self.img_maxInt.value()))
                ax.imshow(img)
            else:
                xlim = ax.get_xlim()
                ylim = ax.get_ylim()
                cx = (xlim[0] + xlim[1]) / 2.
                cy = (ylim[0] + ylim[1]) / 2.
                ax.text(cx, cy, "IMAGE NOT FOUND", fontsize=15, horizontalalignment='center')
        else:
            xlim = ax.get_xlim()
            ylim = ax.get_ylim()
            cx = (xlim[0] + xlim[1]) / 2.
            cy = (ylim[0] + ylim[1]) / 2.
            ax.text(cx, cy, "Please click on map\nto see the image\n and details", fontsize=15, horizontalalignment='center')

        self.imgFigure.subplots_adjust(left=0.15, bottom=0.25, right=0.85, top=0.90, wspace=0, hspace=0)
        self.imgCanvas.draw()

    def mousePressEvent(self, event):
        focused_widget = QApplication.focusWidget()
        if focused_widget != None:
            focused_widget.clearFocus()

    def updateUI(self):
        self.colormap = str(self.colorChoice.currentText())
        self.usingLogScale = self.logScale.isChecked()
        self.rotating90 = self.rotRAngle.isChecked()
        representation = self.repChoice.currentText()
        if representation == 'Total Intensity Map':
            self.updateIntensityMap()
        elif representation == 'Ring Intensity Map':
            self.updateIntensityMap(total=False)
        elif representation == 'Vector Field':
            self.updateVectorFieldMap()
        elif representation == 'Elliptical Map':
            self.updateEllipticalMap()
        elif representation == 'Intensity and Rotation Map':
            self.updateIntensityMap(angle = True)
        elif representation == 'Intensity Map with Arrows':
            self.updateVectorFieldMap(fixedsz=True)
        elif representation == 'Beam Map':
            self.updateBeamMap()
        # elif selected_tab == 1:
        #     self.updateAngularRangeTab()
        QApplication.processEvents()


    def updateIntensityMap(self, total = True, angle = False):

        if len(self.xyIntensity) < 3:
            return

        x = copy.copy(self.xyIntensity[0])
        y = copy.copy(self.xyIntensity[1])
        x2 = np.append(x, max(x) + self.xylim[0])
        y2 = np.append(y, max(y) + self.xylim[1])
        if total:
            intensity = copy.copy(self.xyIntensity[2])
        else:
            intensity = copy.copy(self.xyIntensity[3])

        x_coor, y_coor = np.meshgrid(x2, y2)

        max_val = intensity.max()
        if self.maxMap.value() < 100:
            intensity[
                intensity > self.maxMap.value() * max_val / 100.] = self.maxMap.value() * max_val / 100.
        if self.minMap.value() > 0:
            intensity[
                intensity < self.minMap.value() * max_val / 100.] = self.minMap.value() * max_val / 100.
        max_val = intensity.max()
        min_val = intensity[intensity > 0].min()

        ax = self.mapAxes
        ax.cla()
        norm = LogNorm(vmin=min_val, vmax=max_val) if self.usingLogScale else None
        im = ax.pcolormesh(x_coor, y_coor, intensity, cmap=self.colormap, norm=norm)
        # self.intensityMapFigure.colorbar(im)
        if angle:
            centers = [(x[i] + self.xylim[0]/2, y[j]+self.xylim[1]/2.) for j in range(len(y)) for i in range(len(x))]
            ranges = [toFloat(self.angrange_dict[i]) if i in self.angrange_dict.keys() else 0. for i in
                      range(self.init_number, len(self.hdf_data) + self.init_number)]
            patches = []
            colors = []

            # angle factor is taking care of flipping map (keep angle stay the same)
            angle_factor = -1
            if self.isFlipX:
                angle_factor *= -1
            if self.isFlipY:
                angle_factor *= -1

            for i in range(len(self.hdf_data)):
                if ranges[i] == 0:
                    e = Ellipse(xy=centers[i], width=(self.xylim[0]+self.xylim[1]) / 2. /15., height=(self.xylim[0]+self.xylim[1]) / 2./15.)
                else:
                    e_angle = convertRadtoDegreesEllipse((0 if self.rotating90 else np.pi/2.) + 
                        angle_factor * self.orientation_dict[i + self.init_number])
                    e = Ellipse(xy=centers[i], width=(self.xylim[0]+self.xylim[1]) / 2. /10., height=(self.xylim[0]+self.xylim[1]) / 2., angle=e_angle)
                patches.append(e)
                c = max_val - self.intensity_dict[i + self.init_number] if i + self.init_number in self.intensity_dict else -1
                colors.append(c)
                # colors.append(0)

            colors = np.ma.array(colors, mask=np.array(colors) < 0)
            min_val = colors[colors > 0].min()
            norm = LogNorm(vmin=min_val, vmax=max_val) if self.usingLogScale else None
            p = PatchCollection(patches, cmap=self.colormap, norm=norm)
            # p = PatchCollection(patches, cmap='gray')
            p.set_array(colors)
            ax.add_collection(p)

        ax.set_xlim(x.min(), x.max() + self.xylim[0])
        ax.set_ylim(y.min(), y.max() + self.xylim[1])
        ax.set_facecolor('white')

        if self.isFlipX:
            ax.invert_xaxis()
        if self.isFlipY:
            ax.invert_yaxis()

        if self.mapColorbar is None:
            self.mapColorbar = self.mapFigure.colorbar(im, ax=ax, fraction=0.08, pad=0.01)
        else:
            self.mapFigure.colorbar(im, cax=self.mapColorbar.ax)
        self.mapFigure.subplots_adjust(left=0.04, bottom=0.08, right=1, top=1,wspace=0, hspace=0)
        self.mapCanvas.draw()

    # def updateAngularRangeTab(self):
    #
    #     if self.update_plot['arange_maps']:
    #         if len(self.xyIntensity) < 3:
    #             return
    #
    #         x = self.xyIntensity[0]
    #         y = self.xyIntensity[1]
    #         x_max = len(x)
    #
    #         if 'ar_' not in self.plots.keys():
    #             ang_range = [
    #                 convertRadtoDegrees(float(self.angrange_dict[i])) if i in self.angrange_dict and self.angrange_dict[
    #                                                                                                      i] != '' else 0
    #                 for
    #                 i in range(self.init_number, len(self.hdf_data) + self.init_number)]
    #             ang_range = [r if 0 < r <= 180 else 0 for r in ang_range]
    #             ang_range = np.array([ang_range[i:i + x_max] for i in range(0, len(self.hdf_data), x_max)])
    #             self.plots['ar_'] = copy.copy(ang_range)
    #         else:
    #             ang_range = copy.copy(self.plots['ar_'])
    #
    #         # z = cv2.blur(z, (4,4))
    #         x_coor, y_coor = np.meshgrid(x, y)
    #
    #         max_val = ang_range.max()
    #         min_val = ang_range.min()
    #         if self.ar_maxIntMap.value() < 100:
    #             ang_range[ang_range > self.ar_maxIntMap.value() * max_val / 100.] = max_val
    #         if self.ar_minIntMap.value() > 0:
    #             ang_range[ang_range < self.ar_minIntMap.value() * max_val / 100.] = min_val
    #
    #         ax = self.angularMapAxes
    #         ax.cla()
    #         ax.set_title("Angular Range Map (Degrees)\n")
    #         im = ax.pcolormesh(x_coor, y_coor, ang_range, cmap=self.color_maps)
    #         ax.set_xlim(x.min() - self.xylim[0], x.max() + self.xylim[0])
    #         ax.set_ylim(y.min() - self.xylim[1], y.max() + self.xylim[1])
    #         # self.angularMapFigure.colorbar(im)
    #         self.angularMapFigure.tight_layout()
    #         self.angularMapFigure.savefig(fullPath(self.filePath, 'cp_results/angular_range_map.png'))
    #         self.angularMapCanvas.draw()
    #         self.update_plot['arange_maps'] = False

    def updateVectorFieldMap(self, fixedsz=False):
        if len(self.xyIntensity) < 3:
            return

        x = self.xyIntensity[0]
        y = self.xyIntensity[1]
        x_max = len(x)
        intensity = self.xyIntensity[2]

        orientation = np.array(
            [float(self.orientation_dict[i]) if i in self.orientation_dict and self.orientation_dict[i] != '' else 0
             for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)])
        orientation = [np.pi - ang for ang in orientation]
        # orientation = [np.pi - ang if ang < np.pi else ang - np.pi for ang in orientation]
        orientation = np.array([orientation[i:i + x_max] for i in range(0, len(self.hdf_data), x_max)])
        if self.rotating90:
            orientation -= np.pi/2

        U = np.cos(orientation)
        V = np.sin(orientation)
        int_display = copy.copy(intensity)

        max_val = int_display.max()
        min_val = int_display[int_display > 0].min()

        if self.maxMap.value() < 100:
            int_display[
                int_display > self.maxMap.value() * max_val / 100.] = self.maxMap.value() * max_val / 100.
        if self.minMap.value() > 0:
            int_display[
                int_display < self.minMap.value() * max_val / 100.] = self.minMap.value() * max_val / 100.

        speed = 1 if fixedsz else (int_display / int_display.max())
        UN = U * speed
        VN = V * speed
        self.vec_UV = [U, V]

        ax = self.mapAxes
        ax.cla()
        norm = LogNorm(vmin=min_val, vmax=max_val) if self.usingLogScale else None
        scale = None if fixedsz else 0.7
        self.vec_quiver = ax.quiver(x, y, UN, VN,  # data
                                    int_display,  # colour the arrows based on this array
                                    cmap=self.colormap, norm=norm, # colour map
                                    headlength=7, headwidth=4, scale_units='xy', scale=scale, pivot='middle')

        ax.set_xlim(x.min() - self.xylim[0]/2, x.max() + self.xylim[0]/2)
        ax.set_ylim(y.min() - self.xylim[1]/2, y.max() + self.xylim[1]/2)
        # ax.set_aspect('auto')
        ax.set_facecolor('black')

        if self.isFlipX:
            ax.invert_xaxis()
        if self.isFlipY:
            ax.invert_yaxis()

        # self.vectorFieldMapFigure.colorbar(self.vec_quiver)
        if self.mapColorbar is None:
            self.mapColorbar = self.mapFigure.colorbar(self.vec_quiver, ax=ax, fraction=0.08, pad=0.01)
        else:
            self.mapFigure.colorbar(self.vec_quiver, cax=self.mapColorbar.ax)
        self.mapFigure.subplots_adjust(left=0.04, bottom=0.08, right=1, top=1, wspace=0, hspace=0)
        self.mapCanvas.draw()

        # if self.arrowLengthSlider.value() > 5:
        #     self.updateVectorFieldArrow()

    def updateVectorFieldArrow(self):
        if len(self.xyIntensity) < 3 or len(self.vec_UV) < 2 or self.vec_quiver is None:
            return

        intensity = self.xyIntensity[2]
        U = self.vec_UV[0]
        V = self.vec_UV[1]
        int_display = copy.copy(intensity)

        max_val = int_display.max()
        if self.maxMap.value() < 100:
            int_display[
                int_display > self.maxMap.value() * max_val / 100.] = self.maxMap.value() * max_val / 100.
        if self.minMap.value() > 0:
            int_display[
                int_display < self.minMap.value() * max_val / 100.] = self.minMap.value() * max_val / 100.

        speed = int_display / intensity.max() * (self.arrowLengthSlider.value() / 5.)
        UN = U * speed
        VN = V * speed
        self.vec_quiver.set_UVC(UN, VN)
        self.vectorFieldMapCanvas.draw_idle()
        self.vectorFieldMapFigure.savefig(fullPath(self.filePath, 'cp_results/vector_field.png'))

    def updateEllipticalMap(self):

        if len(self.xyIntensity) < 3:
            return

        x = self.xyIntensity[0]
        y = self.xyIntensity[1]

        centers = [(x[i], y[j]) for j in range(len(y)) for i in range(len(x))]
        ranges = [toFloat(self.angrange_dict[i]) if i in self.angrange_dict.keys() else 0. for i in
                  range(self.init_number, len(self.hdf_data) + self.init_number)]
        max_width = max(ranges)
        widths = [toFloat(self.angrange_dict[i]) / max_width if i in self.angrange_dict.keys() and 0 < toFloat(
            self.angrange_dict[i]) / max_width else max_width / 2. for i in
                  range(self.init_number, len(self.hdf_data) + self.init_number)]

        int_display = np.array(list(self.intensity_dict.values()))
        max_val = int_display.max()
        if self.maxMap.value() < 100:
            int_display[
                int_display > self.maxMap.value() * max_val / 100.] = self.maxMap.value() * max_val / 100.
        if self.minMap.value() > 0:
            int_display[
                int_display < self.minMap.value() * max_val / 100.] = self.minMap.value() * max_val / 100.

        # angle factor is taking care of flipping map (keep angle stay the same)
        angle_factor = -1
        if self.isFlipX:
            angle_factor *= -1
        if self.isFlipY:
            angle_factor *= -1

        ax = self.mapAxes
        ax.cla()
        patches = []
        colors = []
        for i in range(len(self.hdf_data)):

            if ranges[i] == 0:
                e = Ellipse(xy=centers[i], width=self.xylim[0]/5., height=self.xylim[0]/5.)
            else:
                angle = convertRadtoDegreesEllipse((0 if self.rotating90 else np.pi/2.) + 
                    angle_factor * self.orientation_dict[i + self.init_number])
                e = Ellipse(xy=centers[i], width= self.xylim[0] * widths[i], height=self.xylim[0],
                            angle=angle)
            patches.append(e)
            # colors.append(self.intensity_dict[i + self.init_number])

            if i < len(int_display):
                colors.append(int_display[i])
            else:
                colors.append(-1)

        colors = np.ma.array(colors, mask=np.array(colors) < 0)
        min_val = colors[colors > 0].min()
        norm = LogNorm(vmin=min_val, vmax=max_val) if self.usingLogScale else None
        p = PatchCollection(patches, cmap=self.colormap, norm=norm)
        p.set_array(colors)
        ax.add_collection(p)
        ax.set_facecolor('black')
        ax.set_xlim(x.min() - self.xylim[0]/2, x.max() + self.xylim[0]/2)
        ax.set_ylim(y.min() - self.xylim[1]/2, y.max() + self.xylim[1]/2)
        ax.set_aspect('auto')

        if self.isFlipX:
            ax.invert_xaxis()
        if self.isFlipY:
            ax.invert_yaxis()

        if self.mapColorbar is None:
            self.mapColorbar = self.mapFigure.colorbar(p, ax=ax, fraction=0.08, pad=0.01)
        else:
            self.mapFigure.colorbar(p, cax=self.mapColorbar.ax)
        self.mapFigure.subplots_adjust(left=0.04, bottom=0.08, right=1, top=1, wspace=0, hspace=0)
        # self.mapFigure.savefig(fullPath(self.filePath, 'cp_results/vector_field.png'))
        self.mapCanvas.draw()

    def updateBeamMap(self):
        if len(self.xyIntensity) < 3:
            return

        x, y = self.xyIntensity[0], self.xyIntensity[1]
        centers = [(x[i], y[j]) for j in range(len(y)) for i in range(len(x))]

        int_display = np.array(list(self.intensity_dict.values()))
        max_val = int_display.max()
        if self.maxMap.value() < 100:
            int_display[int_display > self.maxMap.value() * max_val / 100.] = \
                self.maxMap.value() * max_val / 100.
        if self.minMap.value() > 0:
            int_display[int_display < self.minMap.value() * max_val / 100.] = \
                self.minMap.value() * max_val / 100.

        # generate ellipses
        ax = self.mapAxes
        ax.cla()
        patches = []
        colors = []
        for i in range(len(self.hdf_data)):
            e = Ellipse(xy=centers[i], width=self.xylim[0] * 1.4, height=self.xylim[1] * 1.4)
            patches.append(e)
            colors.append(int_display[i] if i < len(int_display) else -1)

        # render the figure
        colors = np.ma.array(colors, mask=np.array(colors) < 0)
        min_val = colors[colors > 0].min()
        norm = LogNorm(vmin=min_val, vmax=max_val) if self.usingLogScale else \
               Normalize(vmin=0, vmax=max_val)
        xlim = (x.min() - self.xylim[0], x.max() + self.xylim[0])
        ylim = (y.min() - self.xylim[1], y.max() + self.xylim[1])
        p = PatchCollection(patches, cmap=self.colormap, norm=norm)
        p.set_array(colors)
        if False: # no overlapping
            ax.add_collection(p)
            ax.set_xlim(*xlim)
            ax.set_ylim(*ylim)
            ax.set_facecolor('white')
        elif True:
            mask = np.array([(i + j) % 2 == 0 for j in range(len(y)) for i in range(len(x))])
            patches1, patches2 = [], []
            for patch, flag in zip(patches, mask):
                (patches1 if flag else patches2).append(patch)
            tmpfig, tmpax = plt.subplots(figsize=(12.0, 12.0 / (len(x) + 1) * (len(y) + 1)))
            tmpfig.subplots_adjust(left=0, bottom=0, right=1, top=1, wspace=0, hspace=0)

            p1 = PatchCollection(patches1, cmap=self.colormap, norm=norm, antialiaseds=False)
            p1.set_array(colors[mask])
            tmpax.add_collection(p1)
            tmpax.set_xlim(*xlim)
            tmpax.set_ylim(*ylim)
            tmpax.set_axis_off()
            #tmpfig.savefig('fig01.png', transparent=True)
            tmpfig.canvas.draw()
            w, h = tmpfig.canvas.get_width_height()
            img1 = np.frombuffer(tmpfig.canvas.tostring_rgb(), np.uint8).reshape(h, w, -1).copy()

            p2 = PatchCollection(patches2, cmap=self.colormap, norm=norm, antialiaseds=False)
            p2.set_array(colors[mask == False])
            tmpax.cla()
            tmpax.add_collection(p2)
            tmpax.set_xlim(*xlim)
            tmpax.set_ylim(*ylim)
            tmpax.set_axis_off()
            #tmpfig.savefig('fig02.png', transparent=True)
            tmpfig.canvas.draw()
            img2 = np.frombuffer(tmpfig.canvas.tostring_rgb(), np.uint8).reshape(h, w, -1)

            sx, sy = w / (len(x) + 1), h / (len(y) + 1) # step size in pixel
            bx, by = sx * 1.4, sy * 1.4 # beam size in pixel
            ks, kb = sy / sx, by / bx
            dist = lambda u, v: (u**2+v**2)**0.5
            scal = lambda u, v: by / 2 / dist(kb * u, v)
            for idx in np.ndindex(h, w):
                if img1[idx].sum() >= 765:
                    img1[idx] = img2[idx]
                elif img2[idx].sum() < 765: # overlap
                    rx = idx[1] % sx
                    ry = idx[0] % sy
                    x0, y0 = (rx, ry) if ks * rx + ry < sy else (sx - rx, sy - ry)
                    x1, y1 = (rx, sy - ry) if ks * rx - ry < 0 else (sx - rx, ry)
                    d0, d1 = dist(x0, y0), dist(x1, y1)
                    d0, d1 = d0 * (scal(x0, y0) - 1), d1 * (scal(x1, y1) - 1) #
                    w1, w2 = (d0, d1) if (idx[1] // sx + idx[0] // sy) % 2 > 0 else (d1, d0)
                    if w1 < 0:
                        w1, w2 = 0, 1
                    elif w2 < 0:
                        w1, w2 = 1, 0
                    else:
                        w1, w2 = w1 / (d0 + d1), w2 / (d0 + d1)
                    img1[idx] = np.array(img1[idx] * w1 + img2[idx] * w2, np.uint8)
            ax.imshow(img1, origin='upper', extent=[*xlim, *ylim], aspect=4)
        ax.set_aspect('auto')

        if self.isFlipX:
            ax.invert_xaxis()
        if self.isFlipY:
            ax.invert_yaxis()

        if self.mapColorbar is None:
            self.mapColorbar = self.mapFigure.colorbar(p, ax=ax, fraction=0.08, pad=0.01)
        else:
            self.mapFigure.colorbar(p, cax=self.mapColorbar.ax)
        self.mapFigure.subplots_adjust(left=0.04, bottom=0.08, right=1, top=1, wspace=0, hspace=0)
        self.mapCanvas.draw()


    def browseHDF(self, dir_path, hdfList=[]):
        hdf_filename = ""
        path = join(dir_path, 'settings')
        createFolder(path)
        hdf_cache = join(path, 'hdf.info')

        if exists(hdf_cache):
            hdf_filename = pickle.load(open(hdf_cache, "rb"))

        if len(hdf_filename) == 0 or not exists(hdf_filename):
            if len(hdfList) == 1:
                hdf_filename = join(dir_path, hdfList[0])
            else:
                if len(hdfList) == 0:
                    dlg = HDFBrowser('No HDF file detected.\nPlease select an HDF file to process or create a new one.', path)
                else:
                    dlg = HDFBrowser('There are more than one HDF file detected. \nPlease select an HDF file to process or create a new one.', path)

                ret = dlg.exec_()
                if ret == 0:
                    hdf_filename = ""
                else:
                    hdf_filename = dlg.hdf_file

        if hdf_filename != "":
            pickle.dump(hdf_filename, open(hdf_cache, "wb"))
            self.hdf_filename = str(hdf_filename)
            self.processBatchmodeResults()
        else:
            self.close()

    def processBatchmodeResults(self):
        QApplication.setOverrideCursor(Qt.WaitCursor)
        dir_path = self.filePath
        self.csvManager.load_all()
        hdf_filename = self.hdf_filename
        csv_filename = self.csvManager.sum_file
        self.updateStatusBar(text='Dir : ' + dir_path + '\nHDF : ' + hdf_filename + '\nCSV : ' + csv_filename)
        df_sum = self.csvManager.df_sum.copy()
        df_sum = df_sum.sort_values(['filename'], ascending=True)
        df_rings = self.csvManager.df_rings

        # Read intensity from csv to organize the info given
        self.name_dict = {}
        self.intensity_dict = {}
        self.peak_intensity_dict = {}
        self.distance_dict = {}
        self.angrange_dict = {}
        self.orientation_dict = {}
        self.fit_dict = {}
        self.fitcd_dict = {}

        for i, row in df_sum.iterrows():
            filename = str(row['filename'])
            start_ind = filename.rfind('_')
            end_ind = filename.rfind('.')
            index = int(row['filename'][start_ind + 1:end_ind])
            self.name_dict[index] = row['filename']
            self.intensity_dict[index] = row['total intensity']

            # Add ring model if its error < 1. and sigma < 1. (prevent uniform ring)
            all_rings = df_rings[df_rings['filename'] == filename]
            if len(all_rings) > 0:
                distance_ok = True
                if self.bestRadio.isChecked():
                    all_rings = all_rings.sort_values(['angle fitting error'], ascending=True)
                    best_ring = all_rings.iloc[0]
                else:
                    dist = self.distanceSpnBx.value()
                    unit = str(self.unitChoice.currentText())
                    if unit == 'pixel':
                        col = 'S'
                    else:
                        col = 'd'
                    try:
                        min_ind = min(np.arange(len(all_rings)), key=lambda ind: abs(float(all_rings.iloc[ind][col])-dist)) # Find closest ring to distance
                        max_dif = self.bandwidthSpnBx.value()
                        if abs(float(all_rings.iloc[min_ind][col])-dist) > max_dif:
                            distance_ok = False
                    except:
                        print("WARNING : Unable to find the closest ring to the specified d-spacing for image %s" % (row['filename']))
                        min_ind = 0
                        distance_ok = False
                    best_ring = all_rings.iloc[min_ind]

                good_model = float(best_ring['angle fitting error']) < 1. and best_ring['angle sigma'] < 1. and distance_ok
                peak_inten = -1
                d_spacing = 0
                angle = 0
                angle_sigma = 0

                if good_model:
                    peak_inten = float(best_ring['peak intensity']) if pd.notnull(best_ring['peak intensity']) else 0
                    angle = best_ring['angle'] if pd.notnull(best_ring['angle']) else 0
                    angle_sigma = float(best_ring['angle sigma']) if pd.notnull(best_ring['angle sigma']) else 0
                    if pd.notnull(best_ring['d']) and best_ring['d'] != '-':
                        d_spacing = float(best_ring['d'])
                    elif pd.notnull(best_ring['S']):
                        d_spacing = float(best_ring['S'])


                self.peak_intensity_dict[index] = peak_inten
                self.orientation_dict[index] = angle
                self.angrange_dict[index] = angle_sigma
                self.distance_dict[index] = d_spacing
            else:
                self.peak_intensity_dict[index] = -1
                self.orientation_dict[index] = 0
                self.angrange_dict[index] = 0
                self.distance_dict[index] = 0

        self.init_number = min(self.name_dict.keys())

        # Read hdf5 file to get the coordinates and image shape
        hf = h5py.File(hdf_filename, 'r')
        data = hf.get('data').get('BL')
        self.hdf_data = np.array(data)
        self.coord_dict = {}

        for i in range(self.init_number, len(self.hdf_data) + self.init_number):
            self.coord_dict[i] = (self.hdf_data[i - self.init_number][0], self.hdf_data[i - self.init_number][1])

        nCols = 0
        for i in range(self.init_number + 1, len(self.hdf_data) + self.init_number):
            if abs(self.coord_dict[i][1] - self.coord_dict[i - 1][1]) != 0:
                nCols = i - self.init_number
                break

        nRows = int(len(self.hdf_data) / nCols)
        all_xs = np.reshape(np.array([v[0] for k, v in self.coord_dict.items()]), (nRows, nCols))
        all_ys = np.reshape(np.array([v[1] for k, v in self.coord_dict.items()]), (nRows, nCols))

        x = np.mean(all_xs, axis=0)
        y = np.mean(all_ys, axis=1)

        # Check if any error on coordinates
        x_grad = abs(x[1] - x[0])
        y_grad = abs(y[1] - y[0])

        # Plot heatmap for intensity
        z = [float(self.intensity_dict[i]) if i in self.intensity_dict else -1 for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)]
        # z = np.array([z[i:i + x_max] for i in range(0, , x_max)])
        # z = cv2.blur(z, (4,4))
        # intensity = np.array(z)
        ring_z = [float(self.peak_intensity_dict[i]) if i in self.peak_intensity_dict else -1 for i in
             range(self.init_number, len(self.hdf_data) + self.init_number)]
        intensity = np.reshape(z, (len(y), len(x)))
        ring_intensity = np.reshape(ring_z, (len(y), len(x)))
        intensity = np.ma.array(intensity, mask=intensity < 0)
        ring_intensity = np.ma.array(ring_intensity, mask=ring_intensity < 0)

        self.xyIntensity = [x, y, intensity, ring_intensity]
        self.xylim = [x_grad, y_grad]
        self.refreshAllTabs()
        self.updateImage()
        self.updateUI()
        QApplication.restoreOverrideCursor()

    def refreshAllTabs(self):
        # Set all update flags to True
        self.update_plot = {'intensity_maps': True,
                            'ds_maps': True,
                            'arange_maps': True,
                            'vector_maps': True,
                            'ellipse_maps': True
                            }

    def processFolder(self, dir_path):
        imgList, hdfList = getFilesAndHdf(dir_path)
        createFolder(fullPath(dir_path, 'cp_results'))

        if len(imgList) == 0:
            if exists(fullPath(dir_path, 'cp_results/summary.csv')):
                self.browseHDF(dir_path, hdfList)
            else:
                errMsg = QMessageBox()
                errMsg.setText('No image and summary.csv detected')
                errMsg.setInformativeText('Please select an image or another folder to process.')
                errMsg.setStandardButtons(QMessageBox.Ok)
                errMsg.setIcon(QMessageBox.Warning)
                errMsg.exec_()
        else:
            df_sum = self.csvManager.df_sum
            df_rings = self.csvManager.df_rings
            imgs1 = set(df_sum['filename'])
            imgs2 = set(df_rings['filename'])
            # all_imgs = imgs1 & imgs2
            all_imgs = imgs1
            tmp_imlist = set(imgList[:])
            imgList = list(tmp_imlist - all_imgs)
            imgList.sort()
            if len(imgList) > 0:
                cp = CPImageWindow(self, "", dir_path, process_folder=True, imgList=imgList)
            self.browseHDF(dir_path, hdfList)

    def setMinMaxIntensity(self, img, minInt, maxInt, minIntLabel, maxIntLabel):
        min_val = img.min()
        max_val = img.max()
        self.intensityRange = [min_val, max_val - 1, min_val + 1, max_val]
        minInt.setMinimum(self.intensityRange[0])
        minInt.setMaximum(self.intensityRange[1])
        maxInt.setMinimum(self.intensityRange[2])
        maxInt.setMaximum(self.intensityRange[3])
        step = max(1., (max_val - min_val) / 100)
        minInt.setSingleStep(step)
        maxInt.setSingleStep(step)
        minIntLabel.setText("Min intensity (" + str(min_val) + ")")
        maxIntLabel.setText("Max intensity (" + str(max_val) + ")")

        if img.dtype == 'float32':
            decimal = 2
        else:
            decimal = 0

        maxInt.setDecimals(decimal)
        minInt.setDecimals(decimal)

        if maxInt.value() == 1. and minInt.value() == 0.:
            self.updatingUI = True
            minInt.setValue(min_val)
            maxInt.setValue(max_val * 0.6)
            self.updatingUI = False

    def updateRightStatusBar(self, text):
        QApplication.processEvents()
        self.statusLabel.setHidden(False)
        self.statusLabel.setText(text)
        QApplication.processEvents()

    def updateStatusBar(self, text, bar=None):
        QApplication.processEvents()
        self.imagePathLabel.setText(text)
        if bar is not None:
            self.progressBar.setValue(bar)
        QApplication.processEvents()

def convertRadtoDegreesEllipse(rad):
    return rad * 180. / np.pi
